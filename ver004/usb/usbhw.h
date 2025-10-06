#ifndef __USBHW_H__
#define __USBHW_H__

#include <stdint.h>
#include "usbdesc.h"

/* bmAttributes in Endpoint Descriptor */
#define USB_ENDPOINT_TYPE_MASK                 0x03
#define USB_ENDPOINT_TYPE_CONTROL              0x00
#define USB_ENDPOINT_TYPE_ISOCHRONOUS          0x01
#define USB_ENDPOINT_TYPE_BULK                 0x02
#define USB_ENDPOINT_TYPE_INTERRUPT            0x03


/* USB Hardware Functions */
void USB_Connect      (uint32_t con);
void USB_Reset        (void);
void USB_Suspend      (void);
void USB_Resume       (void);
void USB_WakeUp       (void);
void USB_WakeUpCfg    (uint32_t  cfg);
void USB_SetAddress   (uint32_t adr);
void USB_Configure    (uint32_t cfg);
void USB_ConfigEP     (USBEndPointDescriptor_t *pEPD);
void USB_DirCtrlEP    (uint32_t dir);
void USB_DisableEP    (uint32_t EPNum);
void USB_ResetEP      (uint32_t EPNum);
void USB_SetStallEP   (uint32_t EPNum);
void USB_SetNAK       (uint32_t EPNum);   // Перевод точки в состояние, когда она не готова к приёму данных от хоста
void USB_SetValidTX   (uint32_t EPNum);   // Перевод точки в состояние, когда разрешено отправлять данные (для точек 0x8n)
void USB_SetValidRX   (uint32_t EPNum);   // Перевод точки в состояние, когда разрешено принимать данные (для точек 0x0n)


uint32_t USB_ReadEP(uint32_t EPNum, uint8_t *pData);
void USB_WriteEP(uint32_t EPNum, uint8_t *pData, uint32_t cnt);
uint32_t USB_GetFrame(void);


// Функции, которые обрабатывают события для конечных точек (количество функций зависит от количества конечных точек
// Функции описаны через extern, т.к. реализация этих функций где usbhw.c, но вызываются в usbhw.c внутри обработчика прерываний USB
// через переменную OnEndPointEvent
extern void EndPoint0Event (uint32_t event);
extern void EndPoint1Event (uint32_t event);

#endif  /* __USBHW_H__ */
