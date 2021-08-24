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
    btnQRCodeGenSave: TButton;
    btnQRCodeGenClear: TButton;
    btnLazBarcodeGenerate: TButton;
    btnQRCodeGenGenerate: TButton;
    btnLazBarcodeClear: TButton;
    btnLazBarcodeSave: TButton;
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
    procedure btnLazBarcodeSaveClick(Sender: TObject);
    procedure btnQRCodeGenClearClick(Sender: TObject);
    procedure btnQRCodeGenGenerateClick(Sender: TObject);
    procedure btnQRCodeGenSaveClick(Sender: TObject);
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
  QlpIQrSegment,
  QlpQrSegment,
  QlpQrSegmentMode,
  QlpBitBuffer,
  QlpConverters,
  QlpQRCodeGenLibTypes;

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

procedure TfrmMain.btnLazBarcodeSaveClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.bmp');
end;

procedure TfrmMain.btnQRCodeGenClearClick(Sender: TObject);
begin
  imgQRCodeGen.Picture.Clear;
end;

procedure TfrmMain.btnQRCodeGenGenerateClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );

  // The Scale argiment is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);

  //  I'm being lazy here and using an intermidiate file.
  //  A SaveToStream and LoadFrom Stream using a TMemoryStream would be better
  //  but the QRCode part needs a ImageWriter type that I don't know about
  QRCodeBMP.SaveToFile('temp_file.bmp');
  imgQRCodeGen.Picture.LoadFromFile('temp_file.bmp');
  DeleteFile('temp_file.bmp');
  QRCodeBMP.Free;
end;

procedure TfrmMain.btnQRCodeGenSaveClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  // The Scale argiment is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  QRCodeBMP.SaveToFile('QRCodeGen.bmp');
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

