#ifndef __USBUSER_H__
#define __USBUSER_H__

#include <stdint.h>

void USB_Init(void); // Инициализация USB HID устройства


extern uint8_t *ToHostBufEP1;               // Указатель на буфер, который будет передан хосту через конечную точку 1
extern uint8_t ToHostBufSize;               // размер буфера для передачи данных хосту (один размер для всех конечных точек)
extern uint8_t ToHostBufFillEP1;            // признак того, что буфер для хоста заполнен (для конечной точки 1)

extern uint8_t *FromHostBuf;                // Указатель на буфер в который приходят данные от хоста
extern uint8_t FromHostBufSize;             // размер буфера для приёма данных от хоста
extern uint8_t FromHostBufFill;             // Признак того, что буфер от хоста заполнен данными


// Функцию необходимо запускать после того как, полученные от Хоста данные были обработанны, 
// если этого не сделать, то новые данные от Хоста не придут.
void ClrFromHostBufFill(void);


#endif
