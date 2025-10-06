#include "stm32f10x.h"
#include "systimer.h"
#include "system_stm32f10x.h"

static volatile uint32_t sysTicks=0; // Переменная увеличивается на 1 в прерывании от системного таймера

void SysTick_Handler(void) // Обработчик прерывания от системного таймера
{
  sysTicks++;
}

uint32_t GetTickCount(void){ return sysTicks;} // Возвращает текущее значение счётчика
uint32_t GetTickFreq(void){ return SystemCoreClock / SysTick->LOAD;} // Возвращает частоту в Гц на которую настроен системный таимер

void Delay(uint32_t dlyTicks) // Задержка на заданное число миллисекунд
{                                              
  uint32_t countTicks;

  countTicks = sysTicks + dlyTicks;
  while(sysTicks < countTicks);
}

void SysTimerInit(unsigned int Freq) // Инициализация системного таймера ( в функцию передаётся частота срабатывания системного таймера в Герцах )
{
  SysTick_Config(SystemCoreClock / Freq); // В функцию передаётся количество тиков до формирования прерывания
}

