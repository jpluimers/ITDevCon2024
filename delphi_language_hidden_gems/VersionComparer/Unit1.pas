{ This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.

  Written by Codehunter / 2019 / codehunter [at] gmx . net  }

unit Unit1;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type

  TForm1 = class(TForm)
    btnCompare: TButton;
    cbxOperator: TComboBox;
    cbxVersionA: TComboBox;
    cbxVersionB: TComboBox;
    grpResult: TGroupBox;
    lblAuthorInfo: TLabel;
    lblOperator: TLabel;
    lblResult: TPanel;
    lblVersionA: TLabel;
    lblVersionB: TLabel;
    procedure btnCompareClick(Sender: TObject);
    procedure lblResultClick(Sender: TObject);
  private type
    TOperator = (Equal, Greater, GreaterOrEqual, Less, LessOrEqual, NotEqual);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VersionComparer, Dialogs;

procedure TForm1.btnCompareClick(Sender: TObject);
var
  LVersionA, LVersionB: TVersion;
  LResult: Boolean;
begin
  LResult := False;

  // You can assign Strings directly to TVersion
  LVersionA := cbxVersionA.Text;
  LVersionB := cbxVersionB.Text;

  // You can compare two TVersions directly with Pascal Operators
  case TOperator(cbxOperator.ItemIndex) of
    Equal          : LResult := (LVersionA =  LVersionB);
    Greater        : LResult := (LVersionA >  LVersionB);
    GreaterOrEqual : LResult := (LVersionA >= LVersionB);
    Less           : LResult := (LVersionA <  LVersionB);
    LessOrEqual    : LResult := (LVersionA <= LVersionB);
    NotEqual       : LResult := (LVersionA <> LVersionB);
  end;

  lblResult.Caption := BoolToStr(LResult, True);
end;

procedure TForm1.lblResultClick(Sender: TObject);
begin
  try
    if TVersion('1.0') > '1.0 RC1' then begin

    end;
  except
    on EConvertError do begin
      ShowMessage('Da ging etwas schief');
    end;
  end;
end;

end.
