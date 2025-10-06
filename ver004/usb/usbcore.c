#include "usbhw.h"
#include "usbcore.h"
#include "usbdesc.h"

USBDevice_t USBDev={0}; // Переменная, которая хранит параметры USB устройства

void USB_EP0DataInStage(USBDevice_t *ud) // Передача данных Хосту
{ // Передача данных хосту 
  // т.к. количество байт, которое конечная точка может передать за одну передачу ограниченно
  // весь буфер для выдачи (данных хосту) выполняется за несколько запусков данной процедуры
  // количесво данных которое уже отправлено и полное количество данных хранится в структуре USB устройства

  uint32_t cnt;

  if (ud->EP0Data.Count > USB_MAX_PACKET0)
    cnt = USB_MAX_PACKET0;
  else 
    cnt = ud->EP0Data.Count;

  USB_WriteEP(0x80, ud->EP0Data.pData, cnt);
  ud->EP0Data.pData += cnt;
  ud->EP0Data.Count -= cnt;
}

void USB_EP0DataOutStage(USBDevice_t *ud) // приём данных через нулевую точку
{ // Приём организован так, что недополученные данные (конечная точка имеет ограниченный буфер приёма) с прошлого приёма добавляются к уже полученным в текущем приёме
  uint32_t cnt;

  cnt = USB_ReadEP(0x00, ud->EP0Data.pData);
  if(!cnt)
  {
    ud->EP0Data.pData = 0;
    ud->EP0Data.Count = 0;
  }
  else
  {
    ud->EP0Data.pData += cnt;
    ud->EP0Data.Count -= cnt;
  }
}

uint32_t USB_GetDescriptor(USBDevice_t *uhd) // Обработчик запроса GET_DESCRIPTOR 
{
  uint8_t  *pD;
  uint32_t len, n;

  switch (uhd->sp.bmRequestType.BM.Recipient)
  {
    case REQUEST_TO_DEVICE:  // получатель запроса - устройство USB
                            switch (uhd->sp.wValue.WB.H) 
                            {
                              case USB_DEVICE_DESCRIPTOR_TYPE:
                                                              uhd->EP0Data.pData = (uint8_t *)USB_DeviceDescriptor; // Адрес дескриптора устройства
                                                              len = USB_DEVICE_DESC_SIZE; // Указываем размер дескриптора устройства

                                                              break;
                              case USB_CONFIGURATION_DESCRIPTOR_TYPE:
                                                                    // При запросе дескриптора конфигурации в младшем байте SetupPacket.wValue (SetupPacket.wValue.WB.L) 
                                                                    // хранится индекс дескриптора (т.к. дескрипторов конфигурации может быть несколько)
                                                                    pD = (uint8_t *)USB_ConfigDescriptor;

                                                                    // В этом цикле выполняется переход на указанный номер дескриптора конфигурации
                                                                    // этот цикл не нужен, если у стройства есть только одна конфигурация
                                                                    for (n=0; n != uhd->sp.wValue.WB.L; n++) 
                                                                    {
                                                                      if (((USBConfiguration_Descriptor_t *)pD)->bLength != 0)
                                                                        pD += ((USBConfiguration_Descriptor_t *)pD)->wTotalLength;
                                                                    }
                                                                    // Если после перехода к указанному дескриптору конфигурации его длина оказалась 0-й, то выходим из функции
                                                                    if (((USBConfiguration_Descriptor_t *)pD)->bLength == 0)
                                                                      return 0; // Если возвращается 0, то в ответ будет передан пакет STALL

                                                                    uhd->EP0Data.pData = pD;  // Адрес с которого начинается передача
                                                                    len = ((USBConfiguration_Descriptor_t *)pD)->wTotalLength;  // Количество данных которое необходимо отправить

                                                                    break;
                              case USB_STRING_DESCRIPTOR_TYPE:
                                                              uhd->EP0Data.pData = (uint8_t *)USB_StringDescriptor + uhd->sp.wValue.WB.L;
                                                              len = ((USBStringDescriptor_t *)uhd->EP0Data.pData)->bLength;

                                                              break;
                              default: return 0;
                            }
                            break;
    case REQUEST_TO_INTERFACE:  // Получатель запроса - интерфейс
                              switch (uhd->sp.wValue.WB.H) // какой дескриптор должно устройство передать хосту
                              {
                                default:
                                        return 0;
                              }
                              break;
    default: return 0;
  }

  // Когда программа доходит до сюда в EP0Data.Count лежит то кол-во байт, которое хост хочет получить от устройства
  // это значение не всегда совпадает с тем, что можно выдать USB устройство, оно может быть как больше, так и меньше.
  // Примеры:
  //        1 - В самом начале хост не значет размер дескриптора устройства, и запрашивает у USB устройства 64 байт, что больше размера дескриптора устройства, поэтому значение EP0Data.Count ограничивается длиной дескриптора устройства
  //        2 - Когда хост запрашивате дескриптор конфигурации, то устройство готово передать хосту весь декскриптор со всеми вложениями (с дескриптором интерфейса, с дескрипторами конечных точек и другими), но
  //            хосту нужена только заголовочная чать всего дерева дескриптора конфигурации из неё хост получает информацию о полном размере дерева дескриптора конфигурации и тогда уже запросит все данные.

  if(uhd->EP0Data.Count>len) uhd->EP0Data.Count=len;      // Формируем количество байт для передачи

  USB_EP0DataInStage(uhd);                                // Передача данных хосту через 0-ю конечную точку, ту, которая используется для управляющих обменов
 
  return 1;
}

uint32_t USB_SetConfiguration(USBDevice_t *uhd) //
{
  uint8_t *pD;
  uint32_t n;

  if(uhd->sp.wValue.WB.L) // Если есть номер конфигурации (в setup пакете) (номера конфигурации нумеруются с 1) , то
  {
    pD = (uint8_t *)USB_ConfigDescriptor;
    while (((USBCommonDescriptor_t *)pD)->bLength) 
    {
      switch (((USBCommonDescriptor_t*)pD)->bDescriptorType) 
      {
        case USB_CONFIGURATION_DESCRIPTOR_TYPE: // Установка конфигурации в соответствии с тем номером, который получен в Setup пакете

                                              if(((USBConfiguration_Descriptor_t *)pD)->bConfigurationValue == uhd->sp.wValue.WB.L) 
                                              {
                                                uhd->ConfigOK = uhd->sp.wValue.WB.L; // Формирование номера текущей, выбранной хостом конфигурации
                                                // Если конфигугация хостом выбрана, то потом будут конфигурироваться конечные точки (поэтому сначала они все выключаются)
                                                for (n = 1; n < 16; n++) { USB_DisableEP(n); USB_DisableEP(n | 0x80); }
                                              } 
                                              break;
        case USB_INTERFACE_DESCRIPTOR_TYPE:
                                          break;
        case USB_ENDPOINT_DESCRIPTOR_TYPE:				// Дошли до дескриптора конечной точки
                                          USB_ConfigEP((USBEndPointDescriptor_t *)pD);  // Конфигурация конечной точки на основе описателя конечной точки
                                          break;
      }
      pD += ((USBCommonDescriptor_t *)pD)->bLength;  // Смещение указателя на длину декскриптора (т.е. переход к следующему дескриптору)
    }
  }
  else 
  { // Если в конфигурационном пакете не задан номер конфигурации (нумеруется с 1), то
    uhd->ConfigOK = 0;          // текущая конфигурация 0-я (т.е. конфигурации нет)
    // Отключение всех конечных точек (нет конфигурации, нет конечных точек)
    for (n = 1; n < 16; n++) { USB_DisableEP(n); USB_DisableEP(n | 0x80); }
    return 0;
  }
  if(uhd->ConfigOK == uhd->sp.wValue.WB.L) // Если номер текущей конфигурации совпадает с номером в конфигурационным пакетом, то
    return 1; // возвращаем положительный результат
  return 0;
}

void USB_EP0_Setup_package(USBDevice_t *uhd) // Процедура обрабатывает приход к USB устройству конфигурационного пакета
{
  int validep=0;

  USB_ReadEP(0x00, (uint8_t *)&uhd->sp);      // Выполняем чтение конфигурационного покета
  uhd->EP0Data.Count = uhd->sp.wLength;       // Запоминаем кол-во байт, которое потом нужно будет принять

  switch(uhd->sp.bmRequestType.BM.Type) 
  {
    case REQUEST_STANDARD: // Если это стандартный запрос, то
                          switch (uhd->sp.bRequest) // Проверка стандартных запросов к устройству
                          {
                            case USB_REQUEST_GET_DESCRIPTOR: // запрос к устройству для получения дескриптора (дескриптора устройства, кофигурации, конечной точки ...)
                                                            if(!USB_GetDescriptor(uhd)) validep = 1;  // USB_GetDescriptor() - передача хосту запрошенного дескриптора
                                                            break;

                            case USB_REQUEST_SET_ADDRESS:  // Запрос для задания адреса USB устройства на шине
                                                           // адрес переданый устройству пока только запоминается
                                                           // адрес будет установлен в регистр адреса DADDR, в функции
                                                           // USB_SetAddress
                                                           // Адрес, который назначил хост USB устройство должно установить по приходу пакета IN

                                                            switch (uhd->sp.bmRequestType.BM.Recipient)
                                                            {
                                                              case REQUEST_TO_DEVICE:
                                                                                      uhd->Address = 0x80 | uhd->sp.wValue.WB.L;  // Запоминаем адрес устройства на шине (с установкой 7-го, что будет означать, что хост назначил адрес USB устройству)
                                                                                      USB_WriteEP(0x80, ((void *)0), 0);          // Эта запись означает, что передача данных не производится
                                                                                      break;
                                                              default: validep = 1;
                                                            }

                                                            break;

                            case USB_REQUEST_SET_CONFIGURATION:  // Хост указывает устройству какую конфигурацию необходимо установить

                                                              switch (uhd->sp.bmRequestType.BM.Recipient) 
                                                              {
                                                                case REQUEST_TO_DEVICE:
                                                                                        if(USB_SetConfiguration(uhd))
                                                                                        {
                                                                                          USB_WriteEP(0x80, ((void *)0), 0); // Эта запись означает, что передача данных не производится
                                                                                        }
                                                                                        else
                                                                                        {
                                                                                          validep = 1; break;
                                                                                        }
                                                                                        break;
                                                                default:  validep = 1;
                                                              }
                                                              break;
                            default: validep = 1;
                          }
                          break;

    case REQUEST_CLASS:  // Запрос специфический для данного класса (для работы HID устройства все эти запросы можно проигнорировать)

    default: validep = 1;
  }

  if(validep)
  {
    uhd->EP0Data.Count = 0;
    USB_SetValidTX(0x80);
  }
}

void EndPoint0Event(uint32_t event) // Процедура обрабатывает пакет приходящий от Хоста (конфигурационный пакет, передача данных, приём данных, перевод точки в состояние STALL)
{
  USBDevice_t *ud = &USBDev; // Формируем указатель на USB устройства

  switch (event)
  {
    case USB_EVT_SETUP:  // пакет Setup от хоста (конфигурационный пакет)

                      USB_EP0_Setup_package(ud);
                      break;

    case USB_EVT_OUT: // Если хост передаёт данные устройству
                      USB_EP0DataOutStage(ud);                  // получаем очередную порцию данных (при этом может прийти пакет с нулевым кол-вом данных)
                      break;

    case USB_EVT_IN:  // Хост запрашивает данные у USB устройства
                    if(ud->Address & 0x80)  // Если хост назначил адрес USB устройству, то этот адрес нужно задать в регистре USB
                    {
                      ud->Address &= 0x7F;          // Сброс 7-го бита, который служил признаком того, что хост назначил адрес USB устройству
                      USB_SetAddress(ud->Address);  // Формирование адреса в регистре адреса USB устройства
                      break;
                    }

                    USB_EP0DataInStage(ud);
                    break;
  }
}
