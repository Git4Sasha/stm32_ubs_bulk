#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "stm32f10x.h"
#include "system_stm32f10x.h"
#include "usart1.h"
#include "ringbufhelper.h"

// здесь USART1 настраивается так, чтобы отправка выполнялись через DMA
// приём выполняется в кольцевой буфер

static unsigned char BufForInData[USART1_INBUFLEN];     // Буфер для данных поступающих из вне
static unsigned char InBufHead = 0;                     // Номер ячейки в которую будут сохраняться, поступающие данные (голова буфера)
static unsigned char InBufTail = 0;                     // Номер ячейки из которой можно считывать данные (хвост буфера)
static unsigned int InLostDataCount = 0;                // Счётчик потерянных при приёме байт

static unsigned char BufForOutData[USART1_OUTBUFLEN*2]; // Буфер для передачи данных во вне  (*2 это для двойной буферизации)
static unsigned char *CurBufForOutData=BufForOutData;   // Это адрес того буфера из которого можно считывать данные
static unsigned int IndexForOutData=0;                  // Этот индек изпользуется в таких функция как USART2_AddUIntxx

#define StrBufLen 200                                   // Длина строкового кольцевого буфера
static char StrBufs[StrBufLen][60];                     // Буфер на StrBufLen строк, по 50 символов каждая
static RingBufHelpet_t StrRBuf;                         // Объект для работы с кольцевым буфером, в котором будут храниться строки



static void InitGPIOA() // Настройка ножек порта A для работы в качестве Rx и Tx
{
  // Конфигурация ножек в режиме альтернативной функции, а так же устанавливаются в режим тяни/толкай
  // PA9 - Tx;  PA10 - Rx

  // Режим в котором необходимо конфигурировать ноги для работю какой-либо перефирии, написанно в документации в разделе GPOI

  RCC->APB2ENR |= RCC_APB2ENR_IOPAEN;  // Включение порта A

  //usart1-PA9-TX
  GPIOA->CRH &= ~(GPIO_CRH_MODE9 | GPIO_CRH_CNF9); // Обнуляем все биты для PA9
  GPIOA->CRH |= GPIO_CRH_MODE9_1;
  GPIOA->CRH |= GPIO_CRH_CNF9_1;

  //usart1-PA10-Rx
  GPIOA->CRH &= ~(GPIO_CRH_MODE10 | GPIO_CRH_CNF10); // Обнуляем все биты для PA10
  GPIOA->CRH |= GPIO_CRH_CNF10_0; // Плавающий вход
}

static void InitGPIOB() // Настройка ножек порта B для работы в качестве Rx и Tx (Эти ножки включаются при инициализации функции переназначения функций ножет у перефирии см. AFIO_REMAP)
{
  // Конфигурация ножек в режиме альтернативной функции, а так же устанавливаются в режим тяни/толкай
  // PB6 - Tx;  PB7 - Rx

  // Режим в котором необходимо конфигурировать ноги для работы какой-либо перефирии, написанно в документации в разделе GPOI

  RCC->APB2ENR |= RCC_APB2ENR_AFIOEN;    // Разрешение работы модуля переназначения функций ножек
  AFIO->MAPR |= AFIO_MAPR_USART1_REMAP;  // Включаем переназначение для USART1

  RCC->APB2ENR |= RCC_APB2ENR_IOPBEN;  // Включение порта B

  //usart1-PB6-TX
  GPIOB->CRL &= ~(GPIO_CRL_MODE6 | GPIO_CRL_CNF6); // Обнуляем все биты для PB6
  GPIOB->CRL |= (GPIO_CRL_MODE6_1 | GPIO_CRL_CNF6_1); // Альтернативная функция тяни-толкай

  //usart1-PB7-Rx
  GPIOB->CRL &= ~(GPIO_CRL_MODE7 | GPIO_CRL_CNF7); // Обнуляем все биты для PA10
  GPIOB->CRL |= GPIO_CRL_CNF7_0; // Плавающий вход
}

void USART1_Init(uint32_t br, uint32_t remap)
{
  // br - Скорость работы порта 4300, 9600, 115200 ...
  // remap - признак переназначения функций ножет (по умолчанию изпользуются ножки PA9, PA10, но изпользуя функцию переназначения можно изпользовать ноги PB6, PB7)

  int divider, fraction;


  RCC->AHBENR |= RCC_AHBENR_DMA1EN; // Включение тактирования контролёра DMA1
  RCC->APB2ENR |= RCC_APB2ENR_USART1EN; // Разрешение тактирования последовательного порта 1 ( USART1 )

  if(remap)
    InitGPIOB(); // Настройка ножек порта B для работы в качестве Rx и Tx
  else
    InitGPIOA(); // Настройка ножек порта A для работы в качестве Rx и Tx

  RCC->APB2RSTR |= RCC_APB2RSTR_USART1RST; // Сброс последовательного порта 1
  RCC->APB2RSTR &= ~RCC_APB2RSTR_USART1RST; // Сброс бита сброса (ну и каламбур) последовательного порта 1

  USART1->CR1 = 0; // Сброс контрольного регистра
  USART1->CR1 |= USART_CR1_TE; // Разрешение работы передатчика
  USART1->CR1 |= USART_CR1_RE; // Разрешение работы приёмнка
  USART1->CR1 |= USART_CR1_RXNEIE; // Разрешаем прерывания при приходе данных в полседовательный порт

  USART1->CR2 = 0; // Нулевое значение регистра соответствует всем нужным установкам
  
  USART1->CR3 = 0; // Сброс регистра
  USART1->CR3 |= USART_CR3_DMAT; // Включаем отправку данных от USART`а с помощью DMA

  // для USART1 необходимо брать частоту APB2 (т.к. это устройство подключено к этой шине)

  // Получение делителя частоты для заданной скорости передачи данных
  divider = APB2Freq >> 4; // Делим частоту на 16
  // Дробная часть делителя
  fraction = (divider%br)<<7; // Дробную часть умножаем на 128 для "вытягивания" дробной части
  fraction /= br; // Дробная часть умноженная на 128
  fraction >>= 3; // Одновременное умножение на 16 и деление на 128 ( это равносильно просто делению на 8 )

  // Окончательное значение целой части делителя
  divider /= br; // Делим оставшуюся частоту на скорость передачи

  USART1->BRR = (uint16_t)(divider<<4 | fraction); // Формирования регистра скорости работы USART2

  // Так как УСАП будет передавать данные через КПД, то нужно настроить соответствующий канал КПД ( для УСАП1 это 4-й канал ) (Table 65. Summary of DMA1 requests for each channel в документации)
  DMA1_Channel4->CPAR = (uint32_t)&USART1->DR; // Адрес в который будут передаваться данные ( адрес регистра переферии )
  DMA1_Channel4->CMAR = (uint32_t)CurBufForOutData; // Адрес массива из которого будут поступать данные 
  DMA1_Channel4->CNDTR = 0; // Количество байт, которое необходимо передать

  DMA1_Channel4->CCR |= DMA_CCR1_TCIE; // Будет возникать прерывание по окончанию передачи
  DMA1_Channel4->CCR |= DMA_CCR1_DIR;  // Направление передачи из памяти в перефирию
  DMA1_Channel4->CCR |= DMA_CCR1_MINC; // Увеличение адреса памяти

  NVIC_EnableIRQ(DMA1_Channel4_IRQn);  // Разрешаем прерывания от DMA1 канал 4
  NVIC_EnableIRQ(USART1_IRQn);         // Глобальное разрешение прерывания для USART1

  InBufHead = 0;                       // Номер ячейки в которую будут сохраняться, поступающие данные (голова буфера)
  InBufTail = 0;                       // Номер ячейки из которой можно считывать данные (хвост буфера)

  USART1->CR1 |= USART_CR1_UE;         // Включение последовательного порта

  InitRingBuf(&StrRBuf, StrBufLen);    // Инициализация объекта для кольцевого буфера на 20-ть строк
}

void ResetUSART1(void) // Сброс порта, остановка КПДП, сброс счётчиков ПДП, сброс текущих буферов
{
  while(DMA1_Channel4->CNDTR); // Ждём окончания передачи (есть нежелательная вероятность остановить передачу в то время когда она ещё выполняется, это приведёт к тому, что приёмная сторона получит не полный буфер и будет ожидать остальную часть)

  DMA1_Channel4->CCR &= ~DMA_CCR1_EN; // Выключаем DMA канал 4
  DMA1_Channel4->CNDTR = 0; // Количество байт, которое необходимо передать
  
  RCC->APB2RSTR |= RCC_APB2RSTR_USART1RST; // Сброс последовательного порта 1
  RCC->APB2RSTR &= ~RCC_APB2RSTR_USART1RST; // Сброс бита сброса (ну и каламбур) последовательного порта 1

  InBufHead = 0; // Номер ячейки в которую будут сохраняться, поступающие данные (голова буфера)
  InBufTail = 0; // Номер ячейки из которой можно считывать данные (хвост буфера)
  InLostDataCount = 0; // Обнуляем счётчик потерянных байт
  
  CurBufForOutData=BufForOutData; // Это адрес того буфера из которого можно считывать данные
  IndexForOutData=0; // текущий индекс записи равен нулю
}

void USART1_IRQHandler(void) // Прерывание для USART1
{
  uint8_t data,hd;
  
  if(USART1->SR & USART_SR_RXNE) // Если прерывание возникло из-за того, что пришли данные, которые можно считать, то
  {
    data = USART1->DR; // Необходимо обязательно прочитать байт из регистра данных, чтобы сбросился флаг USART_SR_RXNE в регистре состояния
    hd = InBufHead + 1; // Это будет будущая позиция головы, если она не наедет на хвост
    if(hd==InBufTail) // Если будущее положение головы налезает на хвост, то выходим из прерывания с увеличением кол-ва потерянных байт
    {
      InLostDataCount++; // Увеличиваем счётчик потерянных байт
      return; // Если данные сохранять некуда, то выходим т.е. будет потеря данных
    }
    BufForInData[InBufHead] = data; // Пришедший байт сопируем в буфер
    InBufHead = hd;
  }
}

void DMA1_Channel4_IRQHandler(void) // Обработка прерывания от DMA1 канал 4 (передающий канал)
{
  // когда в УСАРТ будут переданы все USART1_OUTBUFLEN байт, то возникнет это прерывание
  if(DMA1->ISR & DMA_ISR_TCIF4) // Если прерывание произошло по причине окончания передачи в 4-м канале, то
  {
    DMA1->IFCR |= DMA_IFCR_CTCIF4;  // это действие сбросит этот флаг окончания передачи в регистре DMA1->ISR, чтобы прерывание не возникало вновь
    DMA1_Channel4->CCR &= ~DMA_CCR1_EN; // Выключаем DMA канал 4
  }
}

void USART1_SendData(void) // Отправка BufForOutData во вне
{
  while(DMA1_Channel4->CCR & DMA_CCR1_EN); // Пока выполняется передача данных во вне придётся подождать
  // Формирование адреса для передачи с помощью DMA и адреса для буфера, в который можно будет записывать данные
  DMA1_Channel4->CMAR = (uint32_t)CurBufForOutData; // Адрес массива из которого будут поступать данные
  DMA1_Channel4->CNDTR = IndexForOutData; // Указываем количество слов для передачи
  DMA1_Channel4->CCR |= DMA_CCR1_EN; // Включаем DMA канал 4
  if(CurBufForOutData==BufForOutData) // Если указатель текущего буфера равен началу буфера, то
    CurBufForOutData=&BufForOutData[USART1_OUTBUFLEN]; // задаём этот адрес на середину буфера (куда указывает CurBufForOutData, туда и будет выполняться запись)
  else
    CurBufForOutData=BufForOutData; // иначе текущий буфер для записи это начало буфера BufForInData
  IndexForOutData=0; // Сброс текущего индекса в буфере
}

uint8_t USART1_GetInDataLen(void) // Получение кол-во байт во входном буфере
{
  if(InBufTail==InBufHead) return 0;
  if(InBufTail<InBufHead) return (InBufHead-InBufTail);
  // остался вариант когда хвост больше головы (это получается, когда голова достигла конца и переместилась в начало входного буфера)
  return (USART1_INBUFLEN-InBufTail+InBufHead);
}

uint8_t USART1_ReadInBuf(uint8_t *buf, uint8_t len) // Чтение данных из входного буфера (функция вернёт кол-во байт которое было прочитано)
{
  uint8_t cnt=0;
  while((InBufTail!=InBufHead)&&len)
  {
    buf[cnt++] = BufForInData[InBufTail++];
    len--;
  }
  return cnt;
}

void *USART1_GetCurentOutBuf(void){ return CurBufForOutData; } // Возвращаем указатель на буфер в который можно записывать данные для их передачи по Com-порту

void USART1_printf(char *format, ...) // Процедура заменяет процедуру printf, это сделанно для того, чтобы вывод оператора printf направить в последовательный порт
{
  va_list aptr;

  va_start(aptr, format);
  vsprintf((char *)CurBufForOutData, format, aptr);
  va_end(aptr);
  IndexForOutData = strlen((const char *)CurBufForOutData);
  USART1_SendData(); // Отправляем текст
}

void USART1_printfBuf(char *format, ...) // Процедура заменяет процедуру printf, это сделанно для того, чтобы вывод оператора printf направить в последовательный порт
{
  va_list aptr;
  int i;

  if(CanWrite(&StrRBuf))
  {
    i = StrRBuf.Index4Write;                        // номер буфера в массиве StrRBuf, в который будет записываться строка
    va_start(aptr, format);
    vsprintf(&StrBufs[i][0], format, aptr);
    va_end(aptr);
    EndWrite(&StrRBuf);
  }
}

void USART1_SendRBuf(void)                          // Отправка содержимого кольцевого буфера
{
  if(CanRead(&StrRBuf))                             // Если есть возможность чтения из кольцевого буфера, то
  {
    int i = StrRBuf.Index4Read;
    int len = strlen((const char *)&StrBufs[i][0]);
    memcpy((char *)CurBufForOutData, (const char *)&StrBufs[i][0], len);
    IndexForOutData = len;
    USART1_SendData(); // Отправляем текст

    EndRead(&StrRBuf);
  }
}

