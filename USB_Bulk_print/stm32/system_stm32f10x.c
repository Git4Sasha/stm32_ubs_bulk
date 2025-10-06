#include "stm32f10x.h"


#if defined (STM32F10X_HD) || (defined STM32F10X_XL) || (defined STM32F10X_HD_VL)
/* #define DATA_IN_ExtSRAM */
#endif

/*!< Uncomment the following line if you need to relocate your vector Table in
     Internal SRAM. */ 
/* #define VECT_TAB_SRAM */
#define VECT_TAB_OFFSET  0x0 /*!< Vector Table base offset field. 
                                  This value must be a multiple of 0x100. */

/*******************************************************************************
*  Clock Definitions
*******************************************************************************/
#ifdef SYSCLK_FREQ_HSE
  uint32_t SystemCoreClock         = SYSCLK_FREQ_MHz;        /*!< System Clock Frequency (Core Clock) */
#else /*!< HSI Selected as System Clock source */
  uint32_t SystemCoreClock         = HSI_VALUE;        /*!< System Clock Frequency (Core Clock) */
#endif

__I uint8_t AHBPrescTable[16] = {0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 6, 7, 8, 9};

uint32_t APB1Freq;  // Частота тактирования шины APB1
uint32_t APB2Freq;  // Частота тактирования шины APB2


static void SetSysClock(void);

#ifdef DATA_IN_ExtSRAM
  static void SystemInit_ExtMemCtl(void); 
#endif /* DATA_IN_ExtSRAM */

// Настройка системы микроконтроллера
// инициализирование работы с флеш, настройка множителя частоты
// функция должна запускать только после сброса МК

void SystemInit(void)
{
  /* Reset the RCC clock configuration to the default reset state(for debug purpose) */
  /* Set HSION bit */
  RCC->CR |= (uint32_t)0x00000001;

  /* Reset SW, HPRE, PPRE1, PPRE2, ADCPRE and MCO bits */
  RCC->CFGR &= (uint32_t)0xF0FF0000;
  
  /* Reset HSEON, CSSON and PLLON bits */
  RCC->CR &= (uint32_t)0xFEF6FFFF;

  /* Reset HSEBYP bit */
  RCC->CR &= (uint32_t)0xFFFBFFFF;

  /* Reset PLLSRC, PLLXTPRE, PLLMUL and USBPRE/OTGFSPRE bits */
  RCC->CFGR &= (uint32_t)0xFF80FFFF;

  /* Disable all interrupts and clear pending bits  */
  RCC->CIR = 0x009F0000;

  //FLASH->ACR = 18;  // set access control register 

  /* Configure the System clock frequency, HCLK, PCLK2 and PCLK1 prescalers */
  /* Configure the Flash Latency cycles and enable prefetch buffer */
  SetSysClock(); // Установка частоты процессора и шин

  SCB->VTOR = FLASH_BASE | VECT_TAB_OFFSET; /* Vector Table Relocation in Internal FLASH. */
}

static void SetSysClock(void) // Configures the System clock frequency, HCLK, PCLK2 and PCLK1 prescalers.
{
  // В данной процедуре выставляется тактирование 128 МГц от внешнего генератора на 8 МГц

  __IO uint32_t StartUpCounter = 0, HSEStatus = 0;
  
  /* SYSCLK, HCLK, PCLK2 and PCLK1 configuration ---------------------------*/    
  /* Enable HSE */    
  RCC->CR |= ((uint32_t)RCC_CR_HSEON);
 
  /* Wait till HSE is ready and if Time out is reached exit */
  do
  {
    HSEStatus = RCC->CR & RCC_CR_HSERDY;
    StartUpCounter++;  
  }while((HSEStatus == 0) && (StartUpCounter != HSE_STARTUP_TIMEOUT));

  if((RCC->CR & RCC_CR_HSERDY) != RESET)
    HSEStatus = (uint32_t)0x01;
  else
    HSEStatus = (uint32_t)0x00;

  if(HSEStatus == (uint32_t)0x01)
  {
    /* Enable Prefetch Buffer */
    FLASH->ACR |= FLASH_ACR_PRFTBE;

    /* Flash 2 wait state */
    FLASH->ACR &= (uint32_t)((uint32_t)~FLASH_ACR_LATENCY);
    FLASH->ACR |= (uint32_t)FLASH_ACR_LATENCY_2;    

    /* HCLK = SYSCLK */
    RCC->CFGR |= (uint32_t)RCC_CFGR_HPRE_DIV1;
      
    /* PCLK2 = HCLK */
    RCC->CFGR |= (uint32_t)RCC_CFGR_PPRE2_DIV1;
    
    // PCLK1 = HCLK / 2 
    RCC->CFGR |= (uint32_t)RCC_CFGR_PPRE1_DIV2;

    /*  PLL configuration: PLLCLK = HSE * 9 = 72 MHz */

    RCC->CFGR &= (uint32_t)((uint32_t)~(RCC_CFGR_PLLSRC | RCC_CFGR_PLLXTPRE | RCC_CFGR_PLLMULL | RCC_CFGR_USBPRE));

    RCC->CFGR |= (uint32_t)(RCC_CFGR_PLLSRC_HSE | RCC_CFGR_PLLMULL9); // Множитель внешней частоты 9, т.е. частота на которой работает процессор 8МГц*9=72МГц

    /* Enable PLL */
    RCC->CR |= RCC_CR_PLLON;

    /* Wait till PLL is ready */
    while((RCC->CR & RCC_CR_PLLRDY) == 0);
    
    /* Select PLL as system clock source */
    RCC->CFGR &= (uint32_t)((uint32_t)~(RCC_CFGR_SW));
    RCC->CFGR |= (uint32_t)RCC_CFGR_SW_PLL;    

    /* Wait till PLL is used as system clock source */
    while ((RCC->CFGR & (uint32_t)RCC_CFGR_SWS) != (uint32_t)0x08);
  }
  else
  { /* If HSE fails to start-up, the application will have wrong clock 
         configuration. User can add here some code to deal with this error */

  }
}

void SystemCoreClockUpdate(void) // Расчёт текущей частоты квантования и запись этого значения в глобальную переменную SystemCoreClock
{
  // Расчёт текущей частоты квантования (на основе значение хранящихся в регистрах RCC) и запись этого значения в глобальную переменную SystemCoreClock
  // так же в этой процедуре расчитываются частоты квантования для шин APB1 и APB2

  uint32_t tmp = 0, pllmull = 0, pllsource = 0;

  /* Get SYSCLK source -------------------------------------------------------*/
  tmp = RCC->CFGR & RCC_CFGR_SWS;
  
  switch(tmp)
  {
    case 0x00:  /* HSI used as system clock */
                SystemCoreClock = HSI_VALUE;
                break;
    case 0x04:  /* HSE used as system clock */
                SystemCoreClock = HSE_VALUE;
                break;
    case 0x08:  /* PLL used as system clock */

                /* Get PLL clock source and multiplication factor ----------------------*/
                pllmull = RCC->CFGR & RCC_CFGR_PLLMULL;
                pllsource = RCC->CFGR & RCC_CFGR_PLLSRC;
                pllmull = ( pllmull >> 18) + 2;
                
                if (pllsource == 0x00)
                {
                  /* HSI oscillator clock divided by 2 selected as PLL clock entry */
                  SystemCoreClock = (HSI_VALUE >> 1) * pllmull;
                }
                else
                {
                  /* HSE selected as PLL clock entry */
                  if ((RCC->CFGR & RCC_CFGR_PLLXTPRE) != (uint32_t)RESET)
                  {/* HSE oscillator clock divided by 2 */
                    SystemCoreClock = (HSE_VALUE >> 1) * pllmull;
                  }
                  else
                  {
                    SystemCoreClock = HSE_VALUE * pllmull;
                  }
                }
                break;
    default:
                SystemCoreClock = HSI_VALUE;
                break;
  }
  
  /* Compute HCLK clock frequency ----------------*/
  /* Get HCLK prescaler */
  tmp = AHBPrescTable[((RCC->CFGR & RCC_CFGR_HPRE) >> 4)];
  /* HCLK clock frequency */
  SystemCoreClock >>= tmp;  

  // Расчёт частоты тактирования APB1 (PCLK1)
  tmp = (RCC->CFGR & RCC_CFGR_PPRE1) >> 8; // Получаем делитель частоты ядра для шины APB1
  switch(tmp)
  {
    case 4: APB1Freq = SystemCoreClock >> 1; break;  // Делим на 2
    case 5: APB1Freq = SystemCoreClock >> 2; break;  // Делим на 4
    case 6: APB1Freq = SystemCoreClock >> 3; break;  // Делим на 8
    case 7: APB1Freq = SystemCoreClock >> 4; break;  // Делим на 16
    default: APB1Freq = SystemCoreClock; break; 
  }

  // Расчёт частоты тактирования APB2 (PCLK2)
  tmp = (RCC->CFGR & RCC_CFGR_PPRE2) >> 11; // Получаем делитель частоты ядра для шины APB1
  switch(tmp)
  {
    case 4: APB2Freq = SystemCoreClock >> 1; break;  // Делим на 2
    case 5: APB2Freq = SystemCoreClock >> 2; break;  // Делим на 4
    case 6: APB2Freq = SystemCoreClock >> 3; break;  // Делим на 8
    case 7: APB2Freq = SystemCoreClock >> 4; break;  // Делим на 16
    default: APB2Freq = SystemCoreClock; break; 
  }
}


/**
  * @brief  Setup the external memory controller. Called in startup_stm32f10x.s 
  *          before jump to __main
  * @param  None
  * @retval None
  */ 
#ifdef DATA_IN_ExtSRAM
/**
  * @brief  Setup the external memory controller. 
  *         Called in startup_stm32f10x_xx.s/.c before jump to main.
  * 	      This function configures the external SRAM mounted on STM3210E-EVAL
  *         board (STM32 High density devices). This SRAM will be used as program
  *         data memory (including heap and stack).
  * @param  None
  * @retval None
  */ 
void SystemInit_ExtMemCtl(void) 
{
/*!< FSMC Bank1 NOR/SRAM3 is used for the STM3210E-EVAL, if another Bank is 
  required, then adjust the Register Addresses */

  /* Enable FSMC clock */
  RCC->AHBENR = 0x00000114;
  
  /* Enable GPIOD, GPIOE, GPIOF and GPIOG clocks */  
  RCC->APB2ENR = 0x000001E0;
  
/* ---------------  SRAM Data lines, NOE and NWE configuration ---------------*/
/*----------------  SRAM Address lines configuration -------------------------*/
/*----------------  NOE and NWE configuration --------------------------------*/  
/*----------------  NE3 configuration ----------------------------------------*/
/*----------------  NBL0, NBL1 configuration ---------------------------------*/
  
  GPIOD->CRL = 0x44BB44BB;  
  GPIOD->CRH = 0xBBBBBBBB;

  GPIOE->CRL = 0xB44444BB;  
  GPIOE->CRH = 0xBBBBBBBB;

  GPIOF->CRL = 0x44BBBBBB;  
  GPIOF->CRH = 0xBBBB4444;

  GPIOG->CRL = 0x44BBBBBB;  
  GPIOG->CRH = 0x44444B44;
   
/*----------------  FSMC Configuration ---------------------------------------*/  
/*----------------  Enable FSMC Bank1_SRAM Bank ------------------------------*/
  
  FSMC_Bank1->BTCR[4] = 0x00001011;
  FSMC_Bank1->BTCR[5] = 0x00000200;
}
#endif /* DATA_IN_ExtSRAM */


