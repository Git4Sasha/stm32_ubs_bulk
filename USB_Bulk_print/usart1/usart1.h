#ifndef USART1_USER_H
#define USART1_USER_H

#include "stdint.h"

#define USART1_INBUFLEN 256 // Длина буфера для ввода информации через com-порт
#define USART1_OUTBUFLEN 256 // Длина буфера для вывода информации через com-порт

void USART1_Init(uint32_t br, uint32_t remap); // Инициализация последовательного порта 1 ( br - скорость соединения )
void USART1_SendData(void); // Отправка буфера во вне

uint8_t USART1_GetInDataLen(void); // Получение кол-во байт во входном буфере
uint8_t USART1_ReadInBuf(uint8_t *buf, uint8_t len); // Чтение данных из входного буфера (функция вернёт кол-во байт которое было прочитано)

void *USART1_GetCurentOutBuf(void); // Возвращаем указатель на буфер в который можно записывать данные для их передачи по Com-порту
void USART1_printf(char *format, ...); // Процедура заменяет процедуру printf, это сделанно для того, чтобы вывод оператора printf направить в последовательный порт

void USART1_printfBuf(char *format, ...); // Процедура заменяет процедуру printf, это сделанно для того, чтобы вывод оператора printf направить в последовательный порт
void USART1_SendRBuf(void);                          // Отправка содержимого кольцевого буфера


#endif

