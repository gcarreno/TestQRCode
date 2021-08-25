unit TQRC.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, StdCtrls,
  ExtCtrls, ubarcodes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bqrLazBarcode: TBarcodeQR;
    btnQRCodeGenSavePNG: TButton;
    btnQRCodeGenSaveJPG: TButton;
    btnQRCodeGenSaveBMP: TButton;
    btnQRCodeGenClear: TButton;
    btnLazBarcodeGenerate: TButton;
    btnQRCodeGenGenerate: TButton;
    btnLazBarcodeClear: TButton;
    btnLazBarcodeSaveBMP: TButton;
    btnLazBarcodeSaveJPG: TButton;
    btnLazBarcodeSavePNG: TButton;
    btnQRCodeGenSaveSVG: TButton;
    imgQRCodeGen: TImage;
    lblQRCodeGen: TLabel;
    lblLazBarcode: TLabel;
    memLazBarcode: TMemo;
    memQRCodeGen: TMemo;
    panQRCodeGen: TPanel;
    panLazBarcode: TPanel;
    psMain: TPairSplitter;
    pssLazBarcode: TPairSplitterSide;
    pssQRCodeGen: TPairSplitterSide;
    procedure btnLazBarcodeClearClick(Sender: TObject);
    procedure btnLazBarcodeGenerateClick(Sender: TObject);
    procedure btnLazBarcodeSaveBMPClick(Sender: TObject);
    procedure btnLazBarcodeSaveJPGClick(Sender: TObject);
    procedure btnLazBarcodeSavePNGClick(Sender: TObject);
    procedure btnQRCodeGenClearClick(Sender: TObject);
    procedure btnQRCodeGenGenerateClick(Sender: TObject);
    procedure btnQRCodeGenSaveBMPClick(Sender: TObject);
    procedure btnQRCodeGenSaveJPGClick(Sender: TObject);
    procedure btnQRCodeGenSavePNGClick(Sender: TObject);
    procedure btnQRCodeGenSaveSVGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  QlpIQrCode,
  QlpQrCode,
  QlpQRCodeGenLibTypes,
  FPWriteBMP,
  FPWriteJPEG,
  FPWritePNG;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormResize(Sender: TObject);
begin
  psMain.Position:= ClientWidth div 2;
end;

procedure TfrmMain.btnLazBarcodeGenerateClick(Sender: TObject);
begin
  bqrLazBarcode.Text:= memLazBarcode.Text;
end;

procedure TfrmMain.btnLazBarcodeSaveBMPClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.bmp');
end;

procedure TfrmMain.btnLazBarcodeSaveJPGClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.jpg', TJPEGImage);
end;

procedure TfrmMain.btnLazBarcodeSavePNGClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.png', TPortableNetworkGraphic);
end;

procedure TfrmMain.btnQRCodeGenClearClick(Sender: TObject);
begin
  imgQRCodeGen.Picture.Clear;
end;

procedure TfrmMain.btnQRCodeGenGenerateClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
  iwQRCode: TFPWriterBMP;
  msQRCode: TMemoryStream;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );

  // The Scale argument is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  msQRCode:= TMemoryStream.Create;
  try
    iwQRCode:= TFPWriterBMP.Create;
    try
      QRCodeBMP.SaveToStream(msQRCode, iwQRCode);
      QRCodeBMP.Free;
      msQRCode.Position:= 0;
      imgQRCodeGen.Picture.Bitmap.LoadFromStream(msQRCode);
    finally
      iwQRCode.Free;
    end;
  finally
    msQRCode.Free;
  end;
end;

procedure TfrmMain.btnQRCodeGenSaveBMPClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  // The Scale argument is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  QRCodeBMP.SaveToFile('QRCodeGen.bmp');
  QRCodeBMP.Free;
end;

procedure TfrmMain.btnQRCodeGenSaveJPGClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
  iwQRCode: TFPWriterJPEG;
  fsQRCode: TFileStream;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  // The Scale argument is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  fsQRCode:= TFileStream.Create('QRCodeGen.jpg', fmCreate);
  try
    iwQRCode:= TFPWriterJPEG.Create;
    try
      iwQRCode.ProgressiveEncoding:= False;
      iwQRCode.CompressionQuality:= 90;
      QRCodeBMP.SaveToStream(fsQRCode, iwQRCode);
      QRCodeBMP.Free;
    finally
      iwQRCode.Free;
    end;
  finally
    fsQRCode.Free;
  end;
end;

procedure TfrmMain.btnQRCodeGenSavePNGClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
  iwQRCode: TFPWriterPNG;
  fsQRCode: TFileStream;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  // The Scale argument is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  fsQRCode:= TFileStream.Create('QRCodeGen.png', fmCreate);
  try
    iwQRCode:= TFPWriterPNG.Create;
    try
      QRCodeBMP.SaveToStream(fsQRCode, iwQRCode);
      QRCodeBMP.Free;
    finally
      iwQRCode.Free;
    end;
  finally
    fsQRCode.Free;
  end;
end;

procedure TfrmMain.btnQRCodeGenSaveSVGClick(Sender: TObject);
var
  QRCode: IQrCode;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  QRCode.ToSvgFile(2, 'QRCodeGen.svg');
end;

procedure TfrmMain.btnLazBarcodeClearClick(Sender: TObject);
begin
  bqrLazBarcode.Text:= EmptyStr;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  bqrLazBarcode.Text:= EmptyStr;
end;

end.

