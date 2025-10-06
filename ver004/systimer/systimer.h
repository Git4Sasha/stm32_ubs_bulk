#ifndef SYSTIMER_H
#define SYSTIMER_H

#include "stdint.h"

uint32_t GetTickCount(void); // Возвращает текущее значение счётчика
void Delay(uint32_t dlyTicks); // Задержка на заданное число тиков системного таймера
void SysTimerInit(unsigned int Freq); // Инициализация системного таймера ( в функцию передаётся частота срабатывания системного таймера в Герцах )
uint32_t GetTickFreq(void); // Возвращает частоту в Гц на которую настроен системный таимер

#endif
