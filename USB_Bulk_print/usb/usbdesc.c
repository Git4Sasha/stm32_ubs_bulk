#include "usbcore.h"
#include "usbdesc.h"


// Описатель устройства USB
const uint8_t USB_DeviceDescriptor[USB_DEVICE_DESC_SIZE] =
{
  USB_DEVICE_DESC_SIZE,       /*bLength */ // // общая длина дескриптора устройства в байтах
  USB_DEVICE_DESCRIPTOR_TYPE, /*bDescriptorType*/ // bDescriptorType - показывает, что это за дескриптор. В данном случае - Device descriptor (описатель устройства)
  WBVAL(0x0110),              /*bcdUSB */  // // bcdUSB - какую версию стандарта USB поддерживает устройство. 1.10

  // класс, подкласс устройства и протокол, по стандарту USB. У нас нули, означает каждый интерфейс сам за себя
  0x00,                       /*bDeviceClass*/
  0x00,                       //bDeviceSubClass - это значение должно быть таким в дескрипторе устройства
  0x00,                       //bDeviceProtocol - это значение должно быть таким в дескрипторе устройства

  USB_MAX_PACKET0,           /*bMaxPacketSize*/ //bMaxPacketSize - максимальный размер пакетов для Endpoint 0 (64 байта для HS устройств и 8 для других)
  // VID и PID,  по которым и определяется, что же это за устройство.
  WBVAL(USBD_VID),   /*idVendor*/
  WBVAL(USBD_PID),   /*idProduct*/

  WBVAL(0x0100),     // bcdDevice версия устройства

  // дальше идут индексы строк, описывающих производителя, устройства и серийный номер.
  // Отображаются в свойствах устройства в диспетчере устройств
  // А по серийному номеру подключенные устройства с одинаковым VID/PID различаются системой.   
  
  4,          // Index of manufacturer  string  - это смещение относительно начала строкового дескриптора, чтобы получить строку производителя
  32,         // Index of product string        - это смещение относительно начала строкового дескриптора, чтобы получить строку с описанием продукта
  68,         // Index of serial number string  - это смещение относительно начала строкового дескриптора, чтобы получить строку серийным номером устройства

  1           // bNumConfigurations  // bNumConfigurations - количество возможных конфигураций
}; // DeviceDescriptor



// USB Configuration Descriptor All Descriptors (Configuration, Interface, Endpoint, Class, Vendor
// ВНИМАНИЕ!!!!! Меняя дескриптор конфигурации не забывай изменять значение USB_CONFIG_DESC_SIZE, если это значение будет больше, чем сумма
// всех значений в полях bLength, то это не так страшно, но меньшее значение приведёт к печальным результатам
const uint8_t USB_ConfigDescriptor[USB_CONFIG_DESC_SIZE] =
{
  0x09,                               // bLength: Configuration Descriptor size  // bLength: длина дескриптора конфигурации
  USB_CONFIGURATION_DESCRIPTOR_TYPE,  // bDescriptorType: Configuration  // bDescriptorType: тип дескриптора - конфигурация
  WBVAL(USB_CONFIG_DESC_SIZE),         // wTotalLength: общий размер всего дерева под данной конфигурацией в байтах

  0x01,         /*bNumInterfaces: 1 interface*/ // bNumInterfaces: в конфигурации всего один интерфейс
  0x01,         /*bConfigurationValue: Configuration value*/ // bConfigurationValue: индекс данной конфигурации
  0x00,         /*iConfiguration: Index of string descriptor describing the configuration*/ // iConfiguration: индекс строки, которая описывает эту конфигурацию
  0x80,         /*bmAttributes: bus powered and Support Remote Wake-up */ // bmAttributes: признак того, что устройство имеет свой источник питания и имеет возможность пробуждаться
  0x32,         /*MaxPower 100 mA: this current is used for detecting Vbus*/ // MaxPower 100 mA: устройству хватит 100 мА
  
    /************** Дескриптор интерфейса ****************/
    0x09,                               // bLength: размер дескриптора интерфейса
    USB_INTERFACE_DESCRIPTOR_TYPE,      // bDescriptorType: тип дескриптора - интерфейс
    0x00,                               // bInterfaceNumber: порядковый номер интерфейса - 0
    0x00,                               // bAlternateSetting: признак альтернативного интерфейса, у нас не используется
    USB_EP_NUM,                         // bNumEndpoints - количество конечных точек
    0xFF,                               // bInterfaceClass: класс определяется производителем

    // если бы мы косили под стандартное устройство, например клавиатуру или мышь, то надо было бы указать правильно класс и подкласс, а так у нас общее HID-устройство
    0x00,         // bInterfaceSubClass : подкласс интерфейса.
    0x00,         // nInterfaceProtocol : протокол интерфейса
    0x00,         // iInterface: индекс строки, описывающей интерфейс (скорее всего можно сделать строку и указать смещение на неё, тогда где-то будет отображаться эта сторока)

    /******************** дескрипторы конечных точек (endpoints) ********************/
    // конечная точка может иметь номер от 0 до 15
    // 0 - конечная точка должна быть всегда, она зарезервированна для конфигурации USB устройства (это Control endpoint)
    // через неё можно принимать и передавать данные не относящиеся к конфигурации USB устройства, но это не по стандарту (для обмена данными через неё подробнее см. библиотеку libusb)
    // конечные точки кроме 0-й работают только в одном направлении
    // конечные точки с 1 по 15 могут работать только на выход (относительно хоста, как принято при "разговорах о USB") (т.е. хост выдаёт данные устройству)
    // конечные точки с 1 по 15 с установленым 7-м битом в номере точки (т.е. реально номер конечной точки получается 129-143) работают только вход (т.е. хост получает данные от этих точек)
    // т.е. на самом деле конечных точек 30 штук + 1 (15 - вход, 15 - выход, 1 - двунаправленная для управления)
    // ВНИМАНИЕ !!! при описании конечных точек не забудь указать их кол-во в макросе USB_EP_NUM


    // конечная точка для приёма данных от хоста
    0x07,                           // bLength: длина дескриптора
    USB_ENDPOINT_DESCRIPTOR_TYPE,   // тип дескриптора - endpoints
    0x01,                           // bEndpointAddress: адрес конечной точки и направление (OUT)
    0x02,                           // bmAttributes: тип конечной точки - Bulk endpoint
    WBVAL(BULK_OUT_PACKET_SIZE),    // wMaxPacketSize:  Bytes max // Максимальный размер пакета, для обмена с управляющей машиной
    0,                              // bInterval: Polling Interval [ms] // период опроса конечной точки

    // Дескриптор bulk конечной точки IN (передача данных хосту)
    0x07,                                 // bLength
    USB_ENDPOINT_DESCRIPTOR_TYPE,         // bDescriptorType
    0x01 | 0x80,                          // bEndpointAddress 2 in endpoint - через эту точку данные будут передаваться от устройства хосту
    0x02,                                 // bmAttributes   - тип конечной точки - Bulk endpoint
    WBVAL(BULK_IN_PACKET_SIZE),           // wMaxPacketSize - Максимальный размер пакета в байтах
    0                                     // bInterval - Это поле сообщает хосту, что изохронная конечная точка требует один пакет на 1 миллисекунду USB-кадра
};


/* USB String Descriptor (optional) */
const uint8_t USB_StringDescriptor[] = 
{
  /* Index 0x00: LANGID Codes */
  0x04,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  WBVAL(0x0409), /* US English */    /* wLANGID */

  /* Index 0x04: Manufacturer */
  0x1C,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'S',0,
  'W',0,
  'B',0,
  'W',0,
  'S',0,
  'S',0,
  'o',0,
  'f',0,
  't',0,
  'w',0,
  'a',0,
  'r',0,
  'e',0,

  /* Index 0x20: Product */
  0x24,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'A',0,
  'D',0,
  'C',0,
  '-',0,
  'U',0,
  'S',0,
  'B',0,
  '-',0,
  'B',0,
  'u',0,
  'l',0,
  'k',0,
  ' ',0,
  ' ',0,
  ' ',0,
  ' ',0,
  ' ',0,

  /* Index 0x44: Serial Number */
  0x1A,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'D',0,
  'E',0,
  'M',0,
  'O',0,
  ' ',0,
  '0',0,
  '1',0,
  '.',0,
  '0',0,
  '0',0,
  ' ',0,
  ' ',0,

  /* Index 0x5E: Interface 0, Alternate Setting 0 */
  0x09,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'B',0,
  'u',0,
  'l',0,
  'k',0,
};


