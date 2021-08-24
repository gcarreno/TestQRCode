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
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

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
  bqrLazBarcode.SaveToFile('test.bmp');
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

