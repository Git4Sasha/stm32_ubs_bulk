#ifndef __USBCORE_H__
#define __USBCORE_H__

#include <stdint.h>

#define USB_EP_NUM          2  // Количество конечных точек (0-я + остальные (точки 0x01 и 0x81, считаются за одну точку)
#define USB_MAX_PACKET0     8  // Максимальный размер пакета для 0-й точки для FullSpeed устройств (это по стандарту, а на самом деле можно и 16, 32, 64 задать)
#define USB_DBL_BUF_EP      0  // Будет ли изпользоваться двойная буферизация для конечных точек типа bulk


// Коды событий, которые передаются в функции обработчики событий конечных точек
#define USB_EVT_SETUP       1   // Setup Packet
#define USB_EVT_OUT         2   // OUT Packet
#define USB_EVT_IN          3   // IN Packet
#define USB_EVT_OUT_STALL   4   // OUT Packet - Stalled
#define USB_EVT_IN_STALL    5   // IN Packet - Stalled

typedef union __attribute__((packed))
{
  uint16_t W;
  struct __attribute__((packed))
  {
    uint8_t L;
    uint8_t H;
  }WB;
}Value2b_t;

/* bmRequestType Definition */
typedef union __attribute__((packed))
{
  struct _BM
  {
    uint8_t Recipient : 5;  // Код получателя (0 - USB-устройство, 1 - интерфейс, 2 - другой получатель)
    uint8_t Type      : 2;  // Код типа запроса (0 - стандартный запрос, 1 - специфический запрос для данного класса, 2 - специфический запрос изготовителя, 3 - зарезервированно)
    uint8_t Dir       : 1;  // Направление (0 - от Хоста к USB-устройству, 1 - от USB-устройства к Хосту)
  }BM;
  uint8_t B;
}RequestType_t;

typedef struct __attribute__((packed))  // Конфигурационный пакет (книга "Прак. программ. USB, стр 42"
{
  RequestType_t bmRequestType;  // Тип запроса
  uint8_t       bRequest;         // Код запроса (определяет операцию выполняемую запросом)
  Value2b_t     wValue;           // Параметр запроса - Зависит от типа запроса
  Value2b_t     wIndex;           // Индекс или смещение - Зависит от типа запроса
  uint16_t      wLength;          // Число байт для передачи
}USBSetupPacket_t;


/* USB Endpoint Data Structure */
typedef struct _USB_EP_DATA
{
  uint8_t  *pData;
  uint16_t Count;
}USBEpData_t;

typedef struct
{
  uint8_t  Address;           // Адрес устройства выданный хостом
  uint8_t  ConfigOK;          // Поле хранит номер конфигурации, который задал хост (номер конфигурации нумеруется с 1, если это поле равно 0, значит USB устройство не сконфигурированно)

  uint8_t  EP0Buf[USB_MAX_PACKET0]; // Массив для приёма входных данных для нулевой конечной точки (это единственная точка, которая может работать как на вход так и на выход)
  USBEpData_t EP0Data;              // указатель на текущий буфер для передачи данных хосту
  USBSetupPacket_t sp;              // это поле хранит конфигурационный пакет, который приходит от хоста (конфигурационный пакет приходит всегда для 0-й точки)
}USBDevice_t;                       // Структура, которая хранит параметры USB устройства

uint8_t GetConfigNum(void); // Возвращаем номер конфигурации, которую выбрал хост

#endif  /* __USBCORE_H__ */
