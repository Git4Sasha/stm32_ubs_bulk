#ifndef __USBDESC_H__
#define __USBDESC_H__

#include <stdint.h>


#define WBVAL(x) (x & 0xFF),((x >> 8) & 0xFF)

// Бывает так, что Виндовс запоминает PID и VID устройства, и если у Виндовс с устройством возникают проблемы
// то одной из причин может быть, что это у Винды проблемы для данных PID и VID. 
#define USBD_VID 0x0483 // (1155)   idVendor
#define USBD_PID 0x5704 // (22276)  idProduct


#define USB_DEVICE_DESC_SIZE        18    // Размер массива с описателем устройства
#define USB_CONFIG_DESC_SIZE        32    // Размер массива с описателем конфигурации устройства

#define BULK_IN_PACKET_SIZE         64    // Размер пакета при bulk передаче данных хосту
#define BULK_OUT_PACKET_SIZE        8     // Размер пакета при bulk передаче данных от хоста


/* USB Descriptor Types */
#define USB_DEVICE_DESCRIPTOR_TYPE             1
#define USB_CONFIGURATION_DESCRIPTOR_TYPE      2
#define USB_STRING_DESCRIPTOR_TYPE             3
#define USB_INTERFACE_DESCRIPTOR_TYPE          4
#define USB_ENDPOINT_DESCRIPTOR_TYPE           5
#define USB_DEVICE_QUALIFIER_DESCRIPTOR_TYPE   6
#define USB_OTHER_SPEED_CONFIG_DESCRIPTOR_TYPE 7
#define USB_INTERFACE_POWER_DESCRIPTOR_TYPE    8

#define HID_HID_DESCRIPTOR_TYPE         0x21    // Код HID дескриптора
#define HID_REPORT_DESCRIPTOR_TYPE      0x22    // Код REPOTR дескриптора

// Коды стандартных запросов к USB
#define USB_REQUEST_GET_STATUS                 0
#define USB_REQUEST_CLEAR_FEATURE              1
#define USB_REQUEST_SET_FEATURE                3
#define USB_REQUEST_SET_ADDRESS                5
#define USB_REQUEST_GET_DESCRIPTOR             6
#define USB_REQUEST_SET_DESCRIPTOR             7
#define USB_REQUEST_GET_CONFIGURATION          8
#define USB_REQUEST_SET_CONFIGURATION          9
#define USB_REQUEST_GET_INTERFACE              10
#define USB_REQUEST_SET_INTERFACE              11
#define USB_REQUEST_SYNC_FRAME                 12

/* bmRequestType.Recipient */
#define REQUEST_TO_DEVICE          0
#define REQUEST_TO_INTERFACE       1

/* bmRequestType.Type */
#define REQUEST_STANDARD           0
#define REQUEST_CLASS              1


/* USB Standard Configuration Descriptor */
typedef struct __attribute__((packed))
{
  uint8_t  bLength;
  uint8_t  bDescriptorType;
  uint16_t  wTotalLength;
  uint8_t  bNumInterfaces;
  uint8_t  bConfigurationValue;
  uint8_t  iConfiguration;
  uint8_t  bmAttributes;
  uint8_t  bMaxPower;
}USBConfiguration_Descriptor_t;

/* USB Standard Interface Descriptor */
typedef struct __attribute__((packed))
{
  uint8_t  bLength;
  uint8_t  bDescriptorType;
  uint8_t  bInterfaceNumber;
  uint8_t  bAlternateSetting;
  uint8_t  bNumEndpoints;
  uint8_t  bInterfaceClass;
  uint8_t  bInterfaceSubClass;
  uint8_t  bInterfaceProtocol;
  uint8_t  iInterface;
}USBInterfaceDescriptor_t;

/* USB Standard Endpoint Descriptor */
typedef struct __attribute__((packed))
{
  uint8_t  bLength;
  uint8_t  bDescriptorType;
  uint8_t  bEndpointAddress;
  uint8_t  bmAttributes;
  uint16_t  wMaxPacketSize;
  uint8_t  bInterval;
}USBEndPointDescriptor_t;

/* USB String Descriptor */
typedef struct __attribute__((packed))
{
  uint8_t  bLength;
  uint8_t  bDescriptorType;
  uint16_t  bString;
}USBStringDescriptor_t;

/* USB Common Descriptor */
typedef struct __attribute__((packed))
{
  uint8_t  bLength;
  uint8_t  bDescriptorType;
}USBCommonDescriptor_t;


extern const uint8_t USB_DeviceDescriptor[];
extern const uint8_t USB_ConfigDescriptor[];
extern const uint8_t USB_StringDescriptor[];


#endif  /* __USBDESC_H__ */
