#include "system_stm32f10x.h"
#include "systimer.h"
#include "stm32f10x.h"
#include "usbuser.h"
#include "usart1.h"
#include "gpio.h"


// В этой версии реализована bulk передача данных от конечной точки
// для приёма bulk передач, программой на ПК, изпользуется функция libusb_bulk_transfer из библиотеки libusb
// На МК реализованны 2 конечные точки типа bulk одна на передачу, другая на приём

// Если программа не видит устройство её можно запустить от суперпользователя или прописать правило для PID и VID
// Пример прописывания правила:
// создать файл в /etc/udev/rules.d/nn-xxxxx.rules  (nn - произвольный номер, он означает приоритет в том случае, если попадутся одинаковые PID и VID, то будут применены правила с меньшим номером, xxxxx - любая отсебятина латиницей)
// SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="5704", MODE="0666"

int main(void)
{
  int i,j,pause;
  uint32_t tm, tm1;

  SystemCoreClockUpdate();
  SysTimerInit(1000);

  USART1_Init(1000000, 0);

  USART1_printf("Поехали!!\n");


  GPIOConfig(GPIOA, 0, OUT_DIGIT_PUSH_PULL_10MHz);
  GPIOConfig(GPIOA, 1, OUT_DIGIT_PUSH_PULL_10MHz);

  USB_Init();

  USART1_printf("USB_HID_Init()\n");

  pause = 0;
  i=j=0;
  tm = GetTickCount();
  tm1 = tm + 100;
  while(1)
  {
    USART1_SendRBuf();

    if(!pause)
    {
      if(!ToHostBufFillEP1)       // Запись данных в конечную точку 1 (если буфер пуст)
      {
        for(i=0;i<ToHostBufSize;i++)
          ToHostBufEP1[i] = (i+j);
        ToHostBufFillEP1 = 1;   // Установка признака того, что есть данные для конечной точки 1 (те данные, которые можно отправить хосту)
        j++;
      }
    }else
    {
      tm = GetTickCount();
      if(tm>tm1)
      {
        tm1 = tm + pause;
        if(!ToHostBufFillEP1)       // Запись данных в конечную точку 1 (если буфер пуст)
        {
          for(i=0;i<ToHostBufSize;i++)
            ToHostBufEP1[i] = (i+j);
          ToHostBufFillEP1 = 1;   // Установка признака того, что есть данные для конечной точки 1 (те данные, которые можно отправить хосту)
          j++;
        }
      }
    }


    if(FromHostBufFill)       // Если от хоста пришли данные, то выводим то, что пришло
    {
      USART1_printf("buf[0]=%d buf[1]=%d buf[2]=%d buf[3]=%d buf[4]=%d\n", FromHostBuf[0], FromHostBuf[1], FromHostBuf[2], FromHostBuf[3],  FromHostBuf[4]);

      int *pint = (int*)(FromHostBuf);

      pause = pint[0];
      USART1_printf("pause=%d\n", pause);

      ClrFromHostBufFill();
    }

  }

}


