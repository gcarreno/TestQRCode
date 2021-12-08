{ Implements Forms.Main

  MIT License

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit TQRC.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, PairSplitter
, StdCtrls
, ExtCtrls
, FPImage
, ubarcodes
;

type
{ TMyGifImage }
  TMyGifImage = class(TGifImage)
  protected
    class function GetWriterClass: TFPCustomImageWriterClass; override;
  end;

{ TfrmMain }
  TfrmMain = class(TForm)
    bqrLazBarcode: TBarcodeQR;
    btnLazBarcodeCopyToClipboard: TButton;
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
    btnLazBarcodeSaveGIF: TButton;
    btnQRCodeGenSaveGIF: TButton;
    btnQRCodeGenCopyToClipboard: TButton;
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
    procedure btnLazBarcodeCopyToClipboardClick(Sender: TObject);
    procedure btnLazBarcodeGenerateClick(Sender: TObject);
    procedure btnLazBarcodeSaveBMPClick(Sender: TObject);
    procedure btnLazBarcodeSaveGIFClick(Sender: TObject);
    procedure btnLazBarcodeSaveJPGClick(Sender: TObject);
    procedure btnLazBarcodeSavePNGClick(Sender: TObject);

    procedure btnQRCodeGenClearClick(Sender: TObject);
    procedure btnQRCodeGenCopyToClipboardClick(Sender: TObject);
    procedure btnQRCodeGenGenerateClick(Sender: TObject);
    procedure btnQRCodeGenSaveBMPClick(Sender: TObject);
    procedure btnQRCodeGenSaveGIFClick(Sender: TObject);
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

const
  cVersion = '0.6';

implementation

uses
  QlpIQrCode
, QlpQrCode
, QlpQRCodeGenLibTypes
, FPWriteBMP
, FPWriteJPEG
, FPWritePNG
, FPWriteGIF
, Clipbrd
;

{$R *.lfm}

{ TMyGifImage }

class function TMyGifImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result:= TFPWriterGIF;
end;

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
  ShowMessage('Bitmap file "LazBarcode.bmp" saved');
end;

procedure TfrmMain.btnLazBarcodeSaveGIFClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.gif', TMyGifImage);
  ShowMessage('Bitmap file "LazBarcode.gif" saved');
end;

procedure TfrmMain.btnLazBarcodeSaveJPGClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.jpg', TJPEGImage);
  ShowMessage('Bitmap file "LazBarcode.jpg" saved');
end;

procedure TfrmMain.btnLazBarcodeSavePNGClick(Sender: TObject);
begin
  bqrLazBarcode.SaveToFile('LazBarcode.png', TPortableNetworkGraphic);
  ShowMessage('Bitmap file "LazBarcode.png" saved');
end;

procedure TfrmMain.btnLazBarcodeClearClick(Sender: TObject);
begin
  bqrLazBarcode.Text:= EmptyStr;
end;

procedure TfrmMain.btnLazBarcodeCopyToClipboardClick(Sender: TObject);
begin
  bqrLazBarcode.CopyToClipboard;
  ShowMessage('Image copyed to clipboard');
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

procedure TfrmMain.btnQRCodeGenSaveGIFClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
  iwQRCode: TFPWriterGIF;
  fsQRCode: TFileStream;
begin
  QRCode:= TQrCode.EncodeText(
    memQRCodeGen.Text,
    TQrCode.TEcc.eccLow,
    TEncoding.UTF8
  );
  // The Scale argument is still a bit magical for me so please experiment
  QRCodeBMP:= QRCode.ToBitmapImage(7,2);
  fsQRCode:= TFileStream.Create('QRCodeGen.gif', fmCreate);
  try
    iwQRCode:= TFPWriterGIF.Create;
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

procedure TfrmMain.btnQRCodeGenClearClick(Sender: TObject);
begin
  imgQRCodeGen.Picture.Clear;
end;

procedure TfrmMain.btnQRCodeGenCopyToClipboardClick(Sender: TObject);
var
  QRCode: IQrCode;
  QRCodeBMP: TQRCodeGenLibBitmap;
  iwQRCode: TFPWriterBMP;
  msQRCode: TMemoryStream;
  bmp: TBitmap;
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
      bmp:= TBitmap.Create;
      try
        //imgQRCodeGen.Picture.Bitmap.LoadFromStream(msQRCode);
        bmp.LoadFromStream(msQRCode);
        Clipboard.Assign(bmp);
        ShowMessage('Image copyed to clipboard');
      finally
        bmp.Free;
      end;
    finally
      iwQRCode.Free;
    end;
  finally
    msQRCode.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  bqrLazBarcode.Text:= EmptyStr;
  Caption:= Caption + ' v' + cVersion;
end;

initialization
  TPicture.RegisterFileFormat('.gif', 'gif', TMyGifImage);
end.

