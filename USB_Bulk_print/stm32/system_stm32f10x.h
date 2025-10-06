#ifndef __SYSTEM_STM32F10X_H
#define __SYSTEM_STM32F10X_H

#include "stdint.h"

extern uint32_t SystemCoreClock;   // Частота тактирования ядра
extern uint32_t APB1Freq;          // Частота тактирования шины APB1 (PCLK1)
extern uint32_t APB2Freq;          // Частота тактирования шины APB2 (PCLK2)

void SystemCoreClockUpdate(void);


#endif /*__SYSTEM_STM32F10X_H */

