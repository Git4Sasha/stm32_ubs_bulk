#include "stm32f10x.h"
#include "usbcore.h"
#include "usbhw.h"



// EndPoint Registers
#define EP_COUNT_MASK   0x03FF                  // Count Mask

// Структура, которая описывает параметры для приёмного и передающего буфера конечных точек
typedef struct
{
  uint32_t addr_tx;       // Адрес в области USB_PMAADDR куда нужно копировать данные для отправки
  uint32_t count_tx;      // Кол-во передаваемых данных (не обязательно должно быть равно максимальному размеру пакета конечной точки, может быть и меньше, если хочется передать меньше данных
  uint32_t addr_rx;       // Адрес в области USB_PMAADDR откуда считываются приходящие данные
  union
  {
    uint32_t u;
    struct
    {
      uint32_t count_rx    :10;  // реальное кол-во принятых байт
      // Следующие 2 поля отвечают за максимальный размер пакета, который указывается в описателе конечной точки
      // block_size - это поле указывает на то, в каких ед. задаётся размер буфера
      // Если block_size = 0, тогда размер буфера равен num_blocks*2 байт т.е. размер буфера под приём может быть в диапазоне от 0 до 62 байт, с шагом 2 байта
      // Если block_size = 1, тогда размер буфера равен num_blocks*32 байт т.е. размер буфера под приём может быть в диапазоне от 0 до 992 байт, с шагом 32 байта
      // ВНИМАНИЕ!!! размер буфера в stm32f103 под USB всего 512 байт и не надо забывать о том, что в этой же области памяти находятся и описатели буферов конечных точек, т.е. структуры EPBufDscr_t
      uint32_t num_blocks :5;
      uint32_t block_size :1;
    };
  };
}EPBufDscr_t;

// Структура, которая хранит адреса буферов, которые изпользуются в процедурах USB_ReadEP и USB_WriteEP
typedef struct
{
  uint32_t *addr_rx;
  uint32_t *addr_tx;
}EPAddrs_t;

static EPAddrs_t EPAddrs[8] = {0};    // Массив, который хранит адреса, которые изпользуются в процедурах USB_ReadEP и USB_WriteEP


// Это указатель на начало описателей приёмного и передающего буферов конечных точек
static EPBufDscr_t *pBUF_DSCR = (EPBufDscr_t *)USB_PMAADDR;

// от адреса USB_PMAADDR распологаются структуры типа EPBufDscr_t, количество этих записей,
// соответствует количеству конечных точек в устройстве.
// Пояснение по нумерации конечных точек:
// 0-я точна работает как на приём, так и на передачу (это по стандарту USB)
// это сделанно для того, чтобы выполнять обмены с USB устройством на этапе конфигурации или для
// управляющих передач.
// Все остальные точки работают только на приём или только на передачу.
// Если наше устройство будет обмениваться данными в обе стороны не только через 0-ю точку, то
// необходимо сделать 2 описателя конечных точек (но сама конечная точка при этом будет одна).
// В каждом описателе конечной точки есть номер конечной точки, конечные точки с номерами 1,2,3...15 это номера для конечных точек, которые принимают данные от хоста (Out точки по стандарту USB, где всё идёт относительно хоста),
// а конечные точки с номерами 128 + 1,2,3...15 (т.е. с установленым 7-м битом) это номера для конечных точек, которые отправляют данные хосту (In точки)

// Эта переменная указывает на свободную область с которой можно располагать очередные данные
// каждый раз когда инициализируется конечная точка, настраиваются поля структуры EPBufDscr_t, чтобы понять
// откуда можно располагать данные служит эта паременная
static uint32_t FreeBufAddr;

// Массив, который хранит имена (адреса) процедур, которые (должен реализовать разработчик USB устройства) должны обрабатывать различные события связанные с конечными точками

void (* const OnEndPointEvent[]) (uint32_t event) = // Массив указателей на функций-обработчики для конечных точек
{
  EndPoint0Event,  // Функция обработчик для конечной точки 0
  EndPoint1Event,  // Функция обработчик для конечной точки 1

  // По аналогии нужно добавлять сюда функции-обработчики для других конечных точек
};

void EP_Status(uint32_t EPNum, uint32_t stat)
{
  uint32_t num, val;

  num = EPNum & 0x0F;
  val = USB->EPxR[num];
  if (EPNum & 0x80)                       /* IN Endpoint */
    USB->EPxR[num] = (val ^ (stat & USB_EPTX_STAT)) & (USB_EPREG_MASK | USB_EPTX_STAT);
  else                                  /* OUT Endpoint */
    USB->EPxR[num] = (val ^ (stat & USB_EPRX_STAT)) & (USB_EPREG_MASK | USB_EPRX_STAT);
}

void EP_Reset(uint32_t EPNum)
{
  uint32_t num, val;

  num = EPNum & 0x0F;
  val = USB->EPxR[num];
  if (EPNum & 0x80)                      /* IN Endpoint */
    USB->EPxR[num] = val & (USB_EPREG_MASK | USB_EP_DTOG_TX);
  else                                  /* OUT Endpoint */
    USB->EPxR[num] = val & (USB_EPREG_MASK | USB_EP_DTOG_RX);
}

void USB_SetValidTX(uint32_t EPNum) // Перевод точки в состояние, когда разрешено отправлять данные (номер точки должен быть с установленым 7-м битом, т.е. 0x8x т.к. это точка типа IN и она передаёт данные хосту)
{
  uint32_t num, val;

  num = EPNum & 0x0F;
  val = USB->EPxR[num];
  USB->EPxR[num] = (val ^ USB_EP_TX_VALID) & (USB_EPREG_MASK | USB_EPTX_STAT);
}

void USB_SetValidRX(uint32_t EPNum)	// Перевод точки в состояние, когда разрешён приём данных от хоста (номер точки должен быть со сброшеными 7-м битом, т.е. 0x0x т.к. это точка типа OUT и она принимает данные от хоста)
{
  uint32_t num, val;

  num = EPNum & 0x0F;
  val = USB->EPxR[num];

  USB->EPxR[num] = (val ^ USB_EP_RX_VALID) & (USB_EPREG_MASK | USB_EPRX_STAT);
}

void USB_DisableEP(uint32_t EPNum) { EP_Status(EPNum, USB_EP_TX_DIS | USB_EP_RX_DIS); }
void USB_ResetEP(uint32_t EPNum) { EP_Reset(EPNum); }
void USB_SetStallEP(uint32_t EPNum) { EP_Status(EPNum, USB_EP_TX_STALL | USB_EP_RX_STALL); } // Перевод точки в состояние STALL
void USB_SetNAK(uint32_t EPNum) { EP_Status(EPNum, USB_EP_TX_NAK); }


void USB_Connect(uint32_t con) // Функция инициализации подключения
{
  uint32_t winterruptmask = 0;

  if (con)
  {
    /* Set winterruptmask variable */
    winterruptmask = USB_CNTR_CTRM  | USB_CNTR_WKUPM | USB_CNTR_SUSPM | USB_CNTR_ERRM  | USB_CNTR_ESOFM | USB_CNTR_RESETM;
    USB->CNTR &= ~winterruptmask;
    USB->CNTR = USB_CNTR_FRES;
    USB->CNTR = 0;
    USB->ISTR = 0;
    USB->CNTR |= winterruptmask;                     /* USB Reset Interrupt Mask */
  }
  else
    USB->CNTR = USB_CNTR_FRES | USB_CNTR_PDWN;           /* Switch Off USB Device */
}

void USB_Reset(void) // Сброс
{
  USB->ISTR = 0;                                 /* Clear Interrupt Status */

  // Полный набор флагов, показан тут, чтобы иметь представление о том какие бывают ( хотя можно и в *.h файле посмотреть)
  //CNTR = CNTR_CTRM | CNTR_RESETM | CNTR_SUSPM | CNTR_WKUPM | CNTR_ERRM | CNTR_PMAOVRM | CNTR_SOFM | CNTR_ESOFM;

  // Минимальный набор ( CNTR_CTRM - Прерывание возникает при корректной передаче, CNTR_RESETM - USB сброс)
  USB->CNTR = USB_CNTR_CTRM | USB_CNTR_RESETM;

  // После всех описателей буферов точек должны находиться данные, поэтому свободная область для буферов конечных точек
  // начинается со смещениея относительно USB_PMAADDR на то кол-во байт, которое занимают описатели буферов конечных точек
  // USB_EP_NUM+1 - +1 т.к. USB_EP_NUM это кол-во конечных точек без учёта нулевой управляющей конечной точки, под которую так же выделяются буферы и описатель для буферов
  FreeBufAddr = sizeof(EPBufDscr_t)*(USB_EP_NUM + 1);  // Формирование смещения от USB_PMAADDR, на свободную область в буфере выделенном под USB

  USB->BTABLE = 0x00;                            /* set BTABLE Address */

  // Тут формируются адреса для приёмного и передающего буферов конечной точки 0
  // только 0-я конечная точка может работать на приём данных и на передачу (это по стандарту USB)
  // это сделанно для того, чтобы выполнять обмены с USB устройством на этапе конфигурации или для
  // управляющих передач.
  // Все остальные точки работают только на приём или только на передачу.
  
  // Настройка конечной точки 0 (это единственная конечная точка, которая может работать на передачу и на приём)

  USBEndPointDescriptor_t epd;    // декскриптор конечной точки (это чтобы использовать процедуру USB_ConfigEP)

  epd.bEndpointAddress = 0;                       // Настройка 0-й конечной точки в режиме OUT
  epd.wMaxPacketSize = USB_MAX_PACKET0;
  epd.bmAttributes = USB_ENDPOINT_TYPE_CONTROL;

  USB_ConfigEP(&epd);

  epd.bEndpointAddress = 0x80;                    // Настройка 0-й конечной точки в режиме IN
  epd.wMaxPacketSize = USB_MAX_PACKET0;
  epd.bmAttributes = USB_ENDPOINT_TYPE_CONTROL;

  USB_ConfigEP(&epd);

  USB->EPxR[0] = USB_EP_CONTROL | USB_EP_RX_VALID;   // В регистре 0-й конечной точки указывается, что это точка типа "Control" и разрешён приём для этой точки

  USB->DADDR = USB_DADDR_EF | 0;                     // Изначально адрес устройства на шине равен 0, после того как хост выдаст адрес устройству значение в этом регистре будет изменено
}

void USB_Suspend(void) // Приостановка работы USB
{
  USB->CNTR |= USB_CNTR_FSUSP;                       /* Force Suspend */
  USB->CNTR |= USB_CNTR_LP_MODE;                      /* Low Power Mode */
}

void USB_WakeUp (void) 
{
  USB->CNTR &= ~USB_CNTR_FSUSP;                      /* Clear Suspend */
}

void USB_SetAddress(uint32_t adr)                   // Задание адреса для устройства
{
  USB->DADDR = USB_DADDR_EF | adr;
}

void USB_ConfigEP(USBEndPointDescriptor_t *pEPD) // Конфигурация конечной точки
{
  uint32_t num, val;
  EPBufDscr_t *plbufdscr;

  num = pEPD->bEndpointAddress & 0x0F;        // Номер конфигурируемой точки
  val = pEPD->wMaxPacketSize;                 // Максимальный размер пакета для конечной точки
  plbufdscr = pBUF_DSCR; plbufdscr += num;    // Получили адрес по которому находится описатель буферов для конечной точки

  if(pEPD->bEndpointAddress & 0x80)  // Если точка передаёт данные хосту (IN endpoint) (её номер 128+ 1,2,3...15), то
  {
    plbufdscr->addr_tx = FreeBufAddr;         // Адрес буфера для передачи данных хосту (этот адрес изпользуется модулем USB, но в таком виде его нельзя изпользовать в процедуре USB_WriteEP)
    plbufdscr->count_tx = 0;
    EPAddrs[num].addr_tx = (uint32_t *)(USB_PMAADDR + (plbufdscr->addr_tx<<1));  // Это значение адреса, которое будет изпользоваться в процедуре USB_WriteEP
  }
  else
  { // Если эта конечная точа принимает данные от хоста (OUT endpoint) (её номер 1,2,3...15), то
    plbufdscr->addr_rx = FreeBufAddr;		  // Адрес буфера для чтения данных от хоста (этот адрес изпользуется модулем USB, но в таком виде его нельзя изпользовать в процедуре USB_ReadEP)
    EPAddrs[num].addr_rx = (uint32_t *)(USB_PMAADDR + (plbufdscr->addr_rx<<1)); // Это значение адреса, которое будет изпользоваться в процедуре USB_ReadEP

    if(val<62)                          // Если буфер для конечной точки меньше 62-х байт, то
    {
      plbufdscr->block_size = 0;        // блоки будут по 2 байта
      plbufdscr->num_blocks = val >> 1; // максимальный размер пакета делится на 2
    }
    else                                  // Если буфер для конечной точки меньше или равен 62 байта, то
    {                                     // ВНИМАНИЕ!!! в случае максимального размера пакета на приём больше 62-х байт, значение максимального размера пакета нужно задавать с шагом 32
      plbufdscr->block_size = 1;          // блоки будут по 32 байта
      plbufdscr->num_blocks = val >> 5;   // максимальный размер пакета делится на 32
    }
  }
  FreeBufAddr += val;                     // Начало свободной области теперь будет в другом месте

  if(num==0) return;                      // Дальнейшие действия не нужны нулевой конечной точки т.к. всё, что нужно будет выполняться в процедуре USB_Reset после настройки 0-й конечной точик

  switch (pEPD->bmAttributes & USB_ENDPOINT_TYPE_MASK)
  {
    case USB_ENDPOINT_TYPE_ISOCHRONOUS:
                                  val = USB_EP_ISOCHRONOUS;
                                  break;
    case USB_ENDPOINT_TYPE_BULK:
                                  val = USB_EP_BULK;
                                  if (USB_DBL_BUF_EP & (1 << num)) val |= USB_EP_KIND;
                                  break;
    case USB_ENDPOINT_TYPE_INTERRUPT:
                                  val = USB_EP_INTERRUPT;
                                  break;
  }
  val |= num;

  if(pEPD->bEndpointAddress & 0x80)  // Если точка передаёт данные хосту (IN endpoint) (её номер 128+ 1,2,3...15), то
    USB->EPxR[num] = val | USB_EP_TX_VALID;
  else
    USB->EPxR[num] = val | USB_EP_RX_VALID;
}


uint32_t USB_ReadEP(uint32_t EPNum, uint8_t *pData) // Чтение данных из конечной токи с заданным номером
{
  uint32_t num, cnt, *pv, n,res;
  EPBufDscr_t *plbufdscr;

  num = EPNum & 0x0F;

  plbufdscr = pBUF_DSCR;
  plbufdscr += num;  // Получили адрес по которому находится описатель буферов для конечной точки

  pv  = EPAddrs[num].addr_rx;

  res = plbufdscr->count_rx & EP_COUNT_MASK;
  cnt = (res+1)>>1;
  for (n = 0; n < cnt; n++)
  {
    *((uint16_t *)pData) = *pv++;
    pData += 2;
  }
  USB_SetValidRX(EPNum);

  return res; // Возвращается количество записанных данных
}


void USB_WriteEP(uint32_t EPNum, uint8_t *pData, uint32_t cnt) // Запись данных с памощью заданной конечной точки
{
  uint32_t num, *pv, n;
  EPBufDscr_t *plbufdscr;

  num = EPNum & 0x0F;

  plbufdscr = pBUF_DSCR;
  plbufdscr += num;  // Получили адрес по которому находится описатель буферов для конечной точки

  pv  = EPAddrs[num].addr_tx;

  plbufdscr->count_tx = cnt;
  cnt = (cnt+1)>>1;
  for (n = 0; n < cnt; n++)
  {
    *pv++ = *((uint16_t *)pData);
    pData += 2;
  }
  USB_SetValidTX(EPNum);

  return;
}

uint32_t USB_GetFrame(void) // Крайний номер фрейма которым передавались данные
{
  return (USB->FNR & USB_FNR_FN);
}

void USB_LP_CAN1_RX0_IRQHandler(void)
{
  uint32_t istr, num, val;

  istr = USB->ISTR;  // Interrupt Status Register

  if(istr & USB_ISTR_RESET) // Сброс устройства
  {
    USB_Reset();
    USB->ISTR = ~USB_ISTR_RESET;
  }

  if(istr & USB_ISTR_SUSP) // Запрос на приостановку
  {
    USB_Suspend();
    USB->ISTR = ~USB_ISTR_SUSP;
  }

  if (istr & USB_ISTR_WKUP) // Устройство будет хост
  {
    USB_WakeUp();
    USB->ISTR = ~USB_ISTR_WKUP;
  }

  if (istr & USB_ISTR_SOF) // Начало пакета
  {
    USB->ISTR = ~USB_ISTR_SOF;
  }

  if((istr = USB->ISTR) & USB_ISTR_CTR) // Если "Correct Transfer", выполняем следующие действия
  {
    USB->ISTR &= ~USB_ISTR_CTR;         // Снимается бит "Correct Transfer" в регистре ISTR ( Interrupt Status Register )

    num = istr & USB_ISTR_EP_ID;        // Определяем номер конечной точки
    val = USB->EPxR[num];               // Получение регистра связанного с заданным номером конечной точки

    if(val & USB_EP_CTR_RX)             // Если возникло прерывание "Correct RX Transfer" (корректный приём данных)
    {
      USB->EPxR[num] = val & ~USB_EP_CTR_RX & USB_EPREG_MASK; // Сброс бита "Correct RX Transfer" и
      if(OnEndPointEvent[num])          // если существует обработчик для заданной точки, то идём дальше
      {
        if (val & USB_EP_SETUP) // Если был получен EP_SETUP, пакет, то запуск обработчика установочного пакета
        {
          OnEndPointEvent[num](USB_EVT_SETUP);   // Запуск обработчика установочного пакета
        }
        else
        {
          OnEndPointEvent[num](USB_EVT_OUT); // Вызов обработчика для приёма данных от хоста
        }
      }
      return;
    }

    if (val & USB_EP_CTR_TX)                // Если была выполнена успешная передача данных, то
    {
      USB->EPxR[num] = val & ~USB_EP_CTR_TX & USB_EPREG_MASK;
      if (OnEndPointEvent[num])             // Если есть обработчик для конечной точки num, то запускаем его
        OnEndPointEvent[num](USB_EVT_IN);   // Вызов обработчика для передачи данных хосту
    }
  }
}
