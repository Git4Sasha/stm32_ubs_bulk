unit USB_ReadWrite;

interface

uses
  Classes
  , SysUtils
  , LCLIntf
  , SimpleThread
  , LibUSB
  , ArrayOfTypes
  ;

type
  TDataIn=procedure(buf:PByte; cnt, epnum:Integer) of object;

  TEndPointInfo=record    // Информация о конечных точках
    attr:Integer;         // Атрибуты конечной точки (из них можно узнать, тип конечной точки: Прерывания, Изохронная, Массивная, Контрольная)
    sizein:Integer;       // Максимальный размер данных, которые могут быть приняты хостом через конечную точку
    sizeout:Integer;      // Максимальный размер данных, которые могут быть отправлены хостом через конечную точку
  end;
  TArrayOfEndPointInfo=array of TEndPointInfo;

  { TUSBReadWrite }

  TUSBReadWrite=class
  private
    FLibUSBCtx:Pointer;                   // Контекст для работы библиотеки lIbUSB
    FDevHandle:Pointer;                   // Идентификатор устройства для чтения
    FManufactureString:string;            // Имя производителя
    FProductString:string;                // Имя продукта
    FSerialNumberString:string;           // Строка с серийным номером
    FReadThreads:array of TSimpleThread;  // Массив для хранения потоков чтения данных (на каждую конечную точку, которая может передавать данные хосту создаётся свой поток чтения)
    FReadThreadOn:Boolean;                // Признак того, что поток для чтения данных запущен (можно и без него обойтись (особенно если USB устройство ничего не возвращает))
    FOnDataIn:TDataIn;                    // Событие, которое возникает при приходе данных от USB устройства
    FOnNoConnect:TNotifyEvent;            // Событие, которое возникает при разрыве соединения с USB устройством
    FMessageDataIn:Integer;               // Сообщение, которое может быть отправленно, если пришли данные
    FHandleWin:THandle;                   // Идентификатор окна, которому будет отправляться сообщение
    FUSBDeviceOpened:Boolean;             // Признак того, что HID устройство открыто
    FEndPointINCount:Integer;             // Кол-во конечных точек типа "IN"
    FEndPointOUTCount:Integer;            // Кол-во конечных точек типа "OUT"
    FEndPointInfo:TArrayOfEndPointInfo;   // Массив, который хранит размеры каждой точки типа "IN" и типа "OUT"

    function GetStringDesc(pdh:Pointer; StrIndex:Word):string;                // Получение строки по индексу из описателя устройства
    procedure FReadFuncEP(Sender:TObject);                                    // Функция реализует поток чтения данных с USB устройства для конечной точки
    function OpenUSBDByHandle(RunReadThread:Boolean=True):Boolean;            // Открытие устройства по его адресу
    procedure StopReadThreads;                                                // Процедура для остановки потоков чтения данных
  public
    Log:TStrings; // журнал

    constructor Create;
    destructor Destroy; override;
    function GetUSBDeviceNames(pid:Word=0; vid:Word=0):TStrings; // Процедура возвращает список имён устройств
    function OpenUSBDByPID_VID(PID,VID:Word; RunReadThread:Boolean=True):Boolean; // Открывает устройство по идентификаторам устройства VID и PID
    function OpenUSBDByName(name:string; RunReadThread:Boolean=True):Boolean;     // Функция открывает устройство по его имения, которое можно получить изпользуя функцию GetUSBDeviceNames
    procedure CloseHidDevice;                                                     // Закрытие USB устройства
    function RecvData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;     // приём данных от USB устройства
    function SendData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;     // передача данных USB устройству
    procedure SetMessageDataIn(HandleWin:THandle; msgnum:Integer);                // Процедура задаёт номер сообщения, которое будет отправляться окну при приходе данных

    property ManufactureString:string read FManufactureString;   // Имя производителя
    property ProductString:string read FProductString;           // Имя продукта
    property SerialNumberString:string read FSerialNumberString; // Строка с серийным номером
    property OnDataIn:TDataIn read FOnDataIn write FOnDataIn; // Событие, которое возникает при приходе данных от USB устройства
    property OnNoConnect:TNotifyEvent read FOnNoConnect write FOnNoConnect;
    property DeviceOpened:Boolean read FUSBDeviceOpened; // Признак того что устройство открыто
    property EndPointINCount:Integer read FEndPointINCount;
    property EndPointOUTCount:Integer read FEndPointOUTCount;
    property EndPointInfo:TArrayOfEndPointInfo read FEndPointInfo;
  end;

implementation

{ TUSBReadWrite }

constructor TUSBReadWrite.Create;
begin
  Log:=TStringList.Create;            // Для журнала
  FUSBDeviceOpened:=False;            // HID устройство не открыто
  libusb_init(FLibUSBCtx);            // Инициализация библиотеки libUSB
//  libusb_set_debug(FLibUSBCtx, 3);    // Можно установить уровень вывода отладочных сообщений
//  libusb_set_log_cb                   // Можно установить функцию обратного вызова для отлавливания отладочных сообщений
  FUSBDeviceOpened:=False;
  FHandleWin:=0;

  SetLength(FEndPointInfo, 15);      // Выделение памяти на максимально возможное кол-во конечных точек (потом каждая ячейка будет заполнена при открытии устройства)
end;

destructor TUSBReadWrite.Destroy;
begin
  CloseHidDevice; // Закрываем HID устройство
  Log.Free;
  libusb_exit(FLibUSBCtx); // Прекращаем работу с библиотекой
  FEndPointInfo:=nil;
  inherited;
end;

function TUSBReadWrite.GetStringDesc(pdh: Pointer; StrIndex: Word): string;
var
  buf:array [0..255] of Byte;
  buflen,v:Integer;
begin
  Result:='';

  buflen:=SizeOf(buf);
  FillByte(buf[0], buflen, 0);
  if StrIndex<>0 then begin
    v:=libusb_get_string_descriptor_ascii(pdh, StrIndex, @buf[0], buflen);
    if v<=0 then Exit;
  end;

  Result:=PAnsiChar(@buf[0]);
end;

procedure TUSBReadWrite.FReadFuncEP(Sender: TObject);  // Функция потока для чтения, приходящих данных от USB устройства
var
  tob:TSimpleThread;
  uhrw:TUSBReadWrite;
  res, i,epnum:Integer;
  buf:TArrayOfByte;
  pbuf:PByte;
begin
  tob:=Sender as TSimpleThread;
  uhrw:=TUSBReadWrite(tob.UserPar);

  epnum:=tob.UserId;                                      // Номер конечной точки
  SetLength(buf, FEndPointInfo[epnum].sizein);            // Выделение памяти для приёма данных
  pbuf:=@buf[0];                                          // Адрес на начало приёмного буфера

  repeat
    res:=RecvData(epnum, pbuf, 300);                       // Запуск функции приёма данных от HID USB (с ограничением по времени)
    if res>0 then begin                                    // Если данные приняты, то
      if Assigned(FOnDataIn) then                         // Если назначен обработчик этого события, то
        FOnDataIn(pbuf, res, epnum);                       // запускаем этот обработчик
    end;

    if res=LIBUSB_ERROR_NO_DEVICE then Break;              // Если устройство было отключено, то прерываем поток
  until tob.CheckTerminated;                              // Проверка того, что поток необходимо остановить

  buf:=nil;

  i:=Length(FReadThreads)-1;
  if tob=FReadThreads[i] then begin                       // Если завершается последний поток из тех, которые считывают данные, то выполняются действия для оповещения основного потока об этом событии
    if Assigned(FOnNoConnect) then FOnNoConnect(uhrw);

    if FHandleWin<>0 then                                 // Если задано окно, которому можно отправить сообщение о каком-либо событии, то
      PostMessage(FHandleWin, FMessageDataIn, res, 0);    // отправляем сообщение о том что закрылся поток чтения данных с USB устройства (это может означать, что устройство было отсоединено)

    FReadThreadOn:=False;                                 // Потоки чтения не работают (благодаря этому присвоению процедура закрытия потоков (StopReadThreads) ничего делать не будет и это правильно т.к. поток закроется сам)
    if not tob.CheckTerminated then                       // Если выход из потока произошёл не по причине его закрытия пользователем, а из-за ошибки работы с USB устройством, то надо закрывать устройство
                                                          // Если же выход из потока происходит т.к. вызвали процедуру Terminate, то это означает, что как раз и запущена процедура для закрытия устройства в рамках которой останавливаются потоки чтения
      CloseHidDevice;                                     // Ножно закрыть устройство т.к. выход из потока чтения данных по факту означает, что с устройством мы больше работать не можем
  end;
end;

function TUSBReadWrite.OpenUSBDByHandle(RunReadThread: Boolean): Boolean;
var
  i,res,epnum:Integer;
  dev:Pointer;
  desc:libusb_device_descriptor;
  conf:plibusb_config_descriptor;
  pepd:plibusb_endpoint_descriptor;
  str:string;
  inepnums:array [0..15] of Byte;
begin
  Result:=False;

  res:=libusb_kernel_driver_active(FDevHandle, 0); // Проверка захвачено ли ядром устройство
  Log.Append(Format('libusb_kernel_driver_active = %d', [res]));
  if res<0 then begin
    libusb_close(FDevHandle);
    Exit;
  end;

  if res=1 then begin // Если USB устройство изпользуется ядром, то его нужно "забрать" у него
    res:=libusb_detach_kernel_driver(FDevHandle, 0); // пытаемся отключить устройство от ядра
    Log.Append(Format('libusb_detach_kernel_driver = %d',[res]));
    if res<0 then begin
      libusb_close(FDevHandle);
      Exit;
    end;
  end;

  // Теперь захватываем интерфейс
  res:=libusb_claim_interface(FDevHandle, 0);
  Log.Append(Format('libusb_claim_interface %d',[res]));
  if res<0 then begin
    libusb_close(FDevHandle);
    Exit;
  end;

  Log.Append('Устройство открыто');

  dev:=libusb_get_device(FDevHandle);
  res:=libusb_get_device_descriptor(dev, desc);
  if res<0 then
    Log.Append('Не удалось получить информацию об устройстве')
  else begin
    FManufactureString:=GetStringDesc(FDevHandle, desc.iManufacturer);
    FProductString:=GetStringDesc(FDevHandle, desc.iProduct);
    FSerialNumberString:=GetStringDesc(FDevHandle, desc.iSerialNumber);
  end;

  FillByte(inepnums[0], SizeOf(inepnums), 0);   // Обнуление массива, который хранит номера точек типа IN

  // Нужно определить кол-во конечных точек
  // Кол-во конечных точек будет определяться только для 1-й конфигурации (для начала будет так)
  conf:=nil;
  res:=libusb_get_config_descriptor_by_value(dev, 1, conf);
  if res<>0 then begin
    Log.Append('libusb_get_config_descriptor_by_value <> 0');
    libusb_close(FDevHandle);
    Exit;
  end;

  FEndPointINCount:=0;        // Кол-во конечных точек типа "IN"
  FEndPointOUTCount:=0;       // Кол-во конечных точек типа "OUT"
  FillByte(FEndPointInfo[0], SizeOf(TValue4b)*Length(FEndPointInfo), 0);          // Обнуление массива, который хранит параметры конечных точек

  Log.Append(Format('End point count = %d',[conf^.interface_^.altsetting^.bNumEndpoints]));

  pepd:=conf^.interface_^.altsetting^.endpoint;                       // Получен адрес на массив конечных точек
  for i:=0 to conf^.interface_^.altsetting^.bNumEndpoints-1 do begin  // Цикл по конечным точкам, чтобы определить их параметры
    epnum:=pepd[i].bEndpointAddress and $0F;

    FEndPointInfo[epnum].attr:=pepd[i].bmAttributes and %11;              // Сохраняем тип конечной точки

    str:=Format('%d  EndpointAddress=%d', [i, epnum]);

    if pepd[i].bEndpointAddress and $80<>0 then begin
      FEndPointInfo[epnum].sizein:=pepd[i].wMaxPacketSize;            // Максимальный размер пакета для конечной точки epnum если она работает как IN
      inepnums[FEndPointINCount]:=epnum;                              // Сохраняются номера конечных точек типа IN, чуть ниже этот массив понадобится при запуске потоков, которые будут ожидать данные от устройства
      Inc(FEndPointINCount);
      str:=str + ' IN ';
    end else begin
      FEndPointInfo[epnum].sizeout:=pepd[i].wMaxPacketSize;           // Максимальный размер пакета для конечной точки epnum если она работает как OUT
      Inc(FEndPointOUTCount);
      str:=str + ' OUT ';
    end;

    case pepd[i].bmAttributes and %11 of
      LIBUSB_TRANSFER_TYPE_CONTROL      :   str:=str + ' Ctrl ';
      LIBUSB_TRANSFER_TYPE_ISOCHRONOUS  :   str:=str + ' Iso ';
      LIBUSB_TRANSFER_TYPE_BULK         :   str:=str + ' bulk ';
      LIBUSB_TRANSFER_TYPE_INTERRUPT    :   str:=str + ' Int ';
    end;

    str:=str + Format('Pack size = %d', [pepd[i].wMaxPacketSize]);
    Log.Append(str);
  end;
  Log.Append(Format('End point IN count = %d',[FEndPointINCount]));
  Log.Append(Format('End point OUT count = %d',[FEndPointOUTCount]));

  libusb_free_config_descriptor(conf);

  if RunReadThread then begin
    // Запуск потока, который будет принимать данные от USB-HID устройства
    // !!! ВНИМАНИЕ !!! не забудь, что в линуксе при работе с потоками необходимо подключить модуль cthreads самым первым в проекте (см. файл проекта *.lpr)
    // На каждую конечную точку типа IN создаётся свой поток чтения данных
    SetLength(FReadThreads, FEndPointINCount);      // Выделение памяти для массива, который будет хранить объекты для работы с потоками, которые ожидают данные от USB устройства
    for i:=0 to FEndPointINCount-1 do begin
      FReadThreads[i]:=TSimpleThread.CreateSimple;
      // В качестве пользовательского параметра изпользуется сам объект
      // В качестве параметра uid, передаваемого в поточную функцию будет передаваться номер конечной точки
      FReadThreads[i].StartThread(@FReadFuncEP, inepnums[i], Self);
    end;

    FReadThreadOn:=True;  // По этому признаку будет определяться нужно ли останавливать поток когда устройство закрывается
  end else
    FReadThreadOn:=False;  // Поток для чтения данных запущен не был, значит и останавливать его нет необходимости

  FUSBDeviceOpened:=True;

  Result:=True;
end;

procedure TUSBReadWrite.StopReadThreads;   // Процедура для остановки потоков чтения данных
var
  i:Integer;
begin
  if FReadThreadOn then begin                     // Если поток для чтения запускался, то его нужно остановить
    for i:=0 to Length(FReadThreads)-1 do begin   // Остановка всех потоков, которые ожидают данные от USB устройства
      FReadThreads[i].Terminate;
      FReadThreads[i].WaitFor;
    end;
    FReadThreads:=nil;
    FReadThreadOn:=False;
  end;
end;

procedure TUSBReadWrite.CloseHidDevice;
begin
  if FUSBDeviceOpened then begin                // Если устройство открыто, то закрываем его
    StopReadThreads;                            // Остановка потоков чтения данных

    libusb_release_interface(FDevHandle, 0);    // Эта функция парная для libusb_claim_interface (т.е. вызвав libusb_claim_interface нужно вызывать libusb_release_interface)
    libusb_attach_kernel_driver(FDevHandle, 0); // отдаём системе, захваченное ранее USB устройство (эта функция парная для libusb_detach_kernel_driver)
    libusb_close(FDevHandle);

    // Очистка всех остальных параметров
    FManufactureString:='';
    FProductString:='';
    FSerialNumberString:='';

    FEndPointINCount:=0;
    FEndPointOUTCount:=0;
    FillByte(FEndPointInfo[0], SizeOf(TArrayOfEndPointInfo)*Length(FEndPointInfo), 0);          // Обнуление массива, который хранит размеры для конечных точек

    FUSBDeviceOpened:=False; // Выставляем признак того, что устройство не открыто
  end;
end;

function TUSBReadWrite.GetUSBDeviceNames(pid: Word; vid: Word): TStrings; // Получаем список имён USB устройств
var
  devs:pplibusb_device; // Это будет массив идентификаторов устройств USB (каждое устройство это указатель)
  i:Integer;
  v:Integer;
  dev_handle:plibusb_device_handle;
  desc:libusb_device_descriptor;
  str:string;
begin
  Result:=TStringList.Create;

  devs:=nil;
  i:=libusb_get_device_list(FLibUSBCtx, @devs);
  if i<0 then Exit; // USB устройства не обнаружены

  for i:=0 to i-1 do begin // Цикл по списку устройств
    str:='';
    v:=libusb_get_device_descriptor(devs[i], desc);
    if (pid<>0)and(vid<>0) then begin // Если входные параметры НЕ равны нулю, значит задан фильтр по идентификаторам
      if (pid<>desc.idProduct)or(vid<>desc.idVendor) then Continue;// Если номера идентификаторов не соответствуют тем, что заданы, то переходим к следующему устройству
    end;

    if v<0 then Continue; // Если не удалось получить описатель устройства, то переключаемся на следующее устройство

    dev_handle:=nil;
    v:=libusb_open(devs[i], dev_handle); // Получение значение через которое можно определять различные параметры устройства
    if v<>0 then Continue;  // Если произошла ошибка, то переключаемя на следующее устройство

    str:=Format('VID=%.4x  PID=%.4x',[desc.idVendor, desc.idProduct]);
    str:=str+' '+GetStringDesc(dev_handle, desc.iManufacturer);
    str:=str+' '+GetStringDesc(dev_handle, desc.iProduct);
    str:=str+' '+GetStringDesc(dev_handle, desc.iSerialNumber);
    str:=str+Format(' %.2d',[i]);

    libusb_close(dev_handle); // Закрываем устройство

    Result.Append(str);
  end;

  libusb_free_device_list(devs, 1); // Освобождение памяти от списка идентификаторов устройств (кол-во ссылок на устройства уменьшается на 1)
end;

function TUSBReadWrite.OpenUSBDByPID_VID(PID, VID: Word; RunReadThread: Boolean): Boolean;
begin
  Log.Clear;
  Result:=False;

  CloseHidDevice; // Если устройство было открыто, то оно будет закрыто

  FDevHandle:=libusb_open_device_with_vid_pid(FLibUSBCtx, vid, pid);
  if FDevHandle=nil then Exit;

  Result:=OpenUSBDByHandle(RunReadThread);
end;

function TUSBReadWrite.OpenUSBDByName(name: string; RunReadThread: Boolean): Boolean;
var
  devs:pplibusb_device; // Это будет массив идентификаторов устройств USB (каждое устройство это указатель)
  i:Integer;
  v:Integer;
  dev_handle:Pointer;
  desc:libusb_device_descriptor;
  str:string;
begin
  Result:=False;
  Log.Clear;

  devs:=nil;
  i:=libusb_get_device_list(FLibUSBCtx, @devs);
  if i<0 then Exit; // USB устройства не обнаружены

  for i:=0 to i-1 do begin // Цикл по списку устройств
    str:='';
    v:=libusb_get_device_descriptor(@devs[i], desc);

    if v<0 then Continue; // Если не удалось получить описатель устройства, то переключаемся на следующее устройство

    dev_handle:=nil;
    v:=libusb_open(@devs[i], dev_handle); // Получение значение через которое можно определять различные параметры устройства
    if v<>0 then Continue;  // Если произошла ошибка, то переключаемя на следующее устройство

    str:=Format('VID=%.4x  PID=%.4x',[desc.idVendor, desc.idProduct]);
    str:=str+' '+GetStringDesc(dev_handle, desc.iManufacturer);
    str:=str+' '+GetStringDesc(dev_handle, desc.iProduct);
    str:=str+' '+GetStringDesc(dev_handle, desc.iSerialNumber);
    str:=str+Format(' %.2d',[i]);

    if str=name then begin
      FDevHandle:=dev_handle;
      Result:=OpenUSBDByHandle(RunReadThread);
      Break;
    end;
    libusb_close(dev_handle); // Закрываем устройство
  end;

  libusb_free_device_list(devs, 1); // Освобождение памяти от списка идентификаторов устройств (кол-во ссылок на устройства уменьшается на 1)
end;

function TUSBReadWrite.RecvData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bread:LongInt;
  res:Integer;
begin
  res:=0; bread:=0;
  case FEndPointInfo[epnum].attr of
    LIBUSB_TRANSFER_TYPE_ISOCHRONOUS  :   begin end;
    LIBUSB_TRANSFER_TYPE_BULK         :   res:=libusb_bulk_transfer(FDevHandle, epnum or $80, buf, FEndPointInfo[epnum].sizein, bread, tout);
    LIBUSB_TRANSFER_TYPE_INTERRUPT    :   res:=libusb_interrupt_transfer(FDevHandle, epnum or $80, buf, FEndPointInfo[epnum].sizein, bread, tout);
  end;

  if res=0 then Result:=bread else Result:=res;
end;

function TUSBReadWrite.SendData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bwrite:Integer;
  res:Integer;
begin
  res:=0; bwrite:=0;
  case FEndPointInfo[epnum].attr of
    LIBUSB_TRANSFER_TYPE_ISOCHRONOUS  :   begin end;
    LIBUSB_TRANSFER_TYPE_BULK         :   res:=libusb_bulk_transfer(FDevHandle, epnum, buf, FEndPointInfo[epnum].sizeout, bwrite, tout);
    LIBUSB_TRANSFER_TYPE_INTERRUPT    :   res:=libusb_interrupt_transfer(FDevHandle, epnum, buf, FEndPointInfo[epnum].sizeout, bwrite, tout);
  end;

  if res=0 then Result:=bwrite else Result:=res;
end;

procedure TUSBReadWrite.SetMessageDataIn(HandleWin: THandle; msgnum: Integer); // Установка номера сообщения, которое будет отправляться, указанному окну (HandleWin)
begin
  if HandleWin<=0 then // Такое значение идентификатора окна будет разсматриваться как желание прекратить получать сообщения о каких-либо событиях
    HandleWin:=0
  else begin // иначе, задаём параметры для отправки сообщения окну о том, что пришли данные
    FHandleWin:=HandleWin;
    FMessageDataIn:=msgnum;
  end;
end;

end.
