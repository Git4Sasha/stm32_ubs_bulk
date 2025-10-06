/*
 * ringbufhelper.h
 *
 *  Created on: 18 апр. 2020 г.
 *      Author: user
 */

#ifndef RINGBUFHELPER_RINGBUFHELPER_H_
#define RINGBUFHELPER_RINGBUFHELPER_H_

#include "stdint.h"

typedef struct
{
  // publick fields
  int32_t Index4Write; // номер ячейки в кольцевом буфере, в которую возможна запись
  int32_t Index4Read;  // номер ячейки из которой возможно чтение

  // private fields
  int32_t Length;   // максимальное кол-во ячеек в кольцевом буфере
  int32_t Head;     // указатель головы кольцевого буфера
  int32_t NextHead; // указатель на следующее положение головы кольцевого буфера
  int32_t Tail;     // Указатель хвоста кольцевого буфера
}RingBufHelpet_t;

void InitRingBuf(RingBufHelpet_t* obj, uint32_t len); // Инициализация помошника для работы с кольцевым буфером (на вход передаётся длина кольцевого буфера)
void Clean(RingBufHelpet_t* obj);  // Сброс кольцевого буфера

int32_t CanWrite(RingBufHelpet_t* obj); // Функция возвращает 1 если есть возможность запись в кольцевой буфер
void EndWrite(RingBufHelpet_t* obj); // Процедура которую необходимо запускать по окончанию записи в кольцевой буфер

int32_t CanRead(RingBufHelpet_t* obj); // Функция возвращает 1 если есть возможность чтения данных из кольцевого буфера
void EndRead(RingBufHelpet_t* obj); // По окончанию чтения необходимо запускать эту процедуру



#endif /* RINGBUFHELPER_RINGBUFHELPER_H_ */
