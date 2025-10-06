unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , FileUtil
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , XMLPropStorage
  , StdCtrls
  , ExtCtrls
  , ValueListEditorNum
  , Messages
  , LMessages
  , USB_ReadWrite
  , ArrayOfTypes
  , LocalTimeWork
  , MassLikeWin
  , GLChartF
  , GLGrafXY
  , EditNum

  , LibLogger
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ednPause: TEditNum;
    glsch1: TGLSimpleChartF;
    lblInByteCount: TLabel;
    Memo1: TMemo;
    tmCheckUpdate: TTimer;
    ValueListEditorNum1: TValueListEditorNum;
    XMLPropStorage1: TXMLPropStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure glsch1AfterGLInit(Sender: TObject);
    procedure glsch1BeforeGLDeinit(Sender: TObject);
    procedure tmCheckUpdateTimer(Sender: TObject);
  private
    FBulkUSBDev:TUSBReadWrite;
    FInByteCount:Integer;
    FWinMas1f:TWinMass1f;
    FGrfData:TGLGrafXY;


    procedure MSGFromUSB(var msg: TMessage); message LM_USER;
    procedure InDataFromUSB(buf:PByte; cnt, epnum:Integer);
    procedure DrawScene;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  POINT_COUNT = 1000;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBulkUSBDev:=TUSBReadWrite.Create;                  // Создание объекта для работы с USB устройством
  FBulkUSBDev.SetMessageDataIn(Self.Handle, LM_USER);    // Формирование параметров, чтобы окно могло получать сообщения в различных ситуавцях
  FBulkUSBDev.OnDataIn:=@InDataFromUSB;                  // Назначение функции, которая будет обрабатывать входные данные
  FWinMas1f:=TWinMass1f.Create(POINT_COUNT);
end;

procedure TForm1.glsch1AfterGLInit(Sender: TObject);
var
  i:Integer;
begin
  FGrfData:=TGLGrafXY.Create(255,0,0);
  FGrfData.FillXYDefault(POINT_COUNT);
end;

procedure TForm1.glsch1BeforeGLDeinit(Sender: TObject);
begin
  FGrfData.Free;
end;

procedure TForm1.tmCheckUpdateTimer(Sender: TObject);
begin
  lblInByteCount.Caption:=Format('Пришло байт: %d',[FInByteCount]);

  FGrfData.FillYPoints(FWinMas1f.GetBufPointer);

  DrawScene;
end;

procedure TForm1.MSGFromUSB(var msg: TMessage);
begin
  if msg.wParam<0 then begin
    Memo1.Lines.Add(Format('Ошибка чтения данных %d',[msg.wParam]));
    if msg.wParam=-4 then
      Memo1.Lines.Add('Устройство отключено');
    Exit;
  end;
  Memo1.Lines.Add('Устройство закрыто пользователем');
end;

procedure TForm1.InDataFromUSB(buf: PByte; cnt, epnum: Integer);
var
  i:Integer;
begin
  SendDigitLog(epnum, LL_BLACK_COL);

  for i:=0 to cnt-1 do begin
    FWinMas1f.AddVal(buf[i]);
  end;

  SendDigitLog(epnum, LL_RED_COL);

  FInByteCount:=FInByteCount + cnt;
end;

procedure TForm1.DrawScene;
begin
  glsch1.ClearScreen;

  FGrfData.DrawGraf(glsch1.Transform);

  glsch1.DrawAxis;
  glsch1.DrawScaleFrame;
  glsch1.SwapBuffers;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FBulkUSBDev.Free;
  FWinMas1f.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pid,vid:Integer;
begin
  vid:=ValueListEditorNum1.IntValue['VID'];
  pid:=ValueListEditorNum1.IntValue['PID'];

  if not FBulkUSBDev.OpenUSBDByPID_VID(pid, vid) then begin // Если устройство не удалось открыть, то
    Memo1.Lines.AddStrings(FBulkUSBDev.Log); // отображаем журнал
    Exit;
  end;

  FInByteCount:=0;

  Memo1.Lines.AddStrings(FBulkUSBDev.Log); // отображаем журнал

  Memo1.Append('Устройство открыто');
  Memo1.Append(FBulkUSBDev.ManufactureString);
  Memo1.Append(FBulkUSBDev.ProductString);
  Memo1.Append(FBulkUSBDev.SerialNumberString);
  Memo1.Lines.AddStrings(FBulkUSBDev.Log); // отображаем журнал

  tmCheckUpdate.Enabled:=True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FBulkUSBDev.CloseHidDevice;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i,res:Integer;
  buf:TArrayOfByte;
  pause:TValue4b;
  tm:Double;

begin
  if not FBulkUSBDev.DeviceOpened then begin
    Memo1.Append('Устройство не открыто');
    Exit;
  end;

  SetLength(buf, FBulkUSBDev.EndPointInfo[1].sizeout);

  pause.UInt:=ednPause.TxtToInt;
  buf[0]:=pause.b0;
  buf[1]:=pause.b1;
  buf[2]:=pause.b2;
  buf[3]:=pause.b3;

  tm:=GetClockTimeValue;
  res:=FBulkUSBDev.SendData(1, @buf[0], 1000);
  tm:=GetClockTimeValue-tm;
  Memo1.Append(Format('1 Время записи %.5f мс',[tm*1000]));

  buf:=nil;

  if res<0 then Memo1.Append(Format('1 Ошибка отправки %d',[res]));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  strs:TStrings;
begin
  strs:=FBulkUSBDev.GetUSBDeviceNames;
  Memo1.Lines.AddStrings(strs);
  strs.Free;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Memo1.Clear;
end;

end.

