program OperatorInHelper;

uses
  System.SysUtils,
  System.Types;

type
  TStringRecord = record
  private
    FValue: string;
  public
    class operator In(A: TStringRecord; B: TStringDynArray): Boolean;
    class operator Implicit(aValue: string): TStringRecord; overload;
  end;

  TStringHelper = record helper for string
  public
    class operator In(A: string; B: TStringDynArray): Boolean;
  end;

function IsStringIn(A: string; B: TStringDynArray): Boolean;
var
  S: string;
begin
  Result := False;
  for S in B do begin
    if SameStr(A, S) then begin
      Result := True;
      Break;
    end;
  end;
end;

{ TStringRecord }

class operator TStringRecord.Implicit(aValue: string): TStringRecord;
begin
  inherited;
  Result.FValue := aValue;
end;

class operator TStringRecord.In(A: TStringRecord; B: TStringDynArray): Boolean;
begin
  Result := IsStringIn(A.FValue, B);
end;

{ TStringHelper }

class operator TStringHelper.In(A: string; B: TStringDynArray): Boolean;
begin
  Result := IsStringIn(A, B);
end;

procedure Beispiel;
var
  LStringRecord: TStringRecord;
  LString: string;
begin
  LStringRecord := 'Foo';
  if LStringRecord in ['Foo', 'Bar'] then begin
    // Machwas
  end;
  LString := 'Foo';
  if LString in ['Foo', 'Bar'] then begin
    // Machwas
  end;
end;

begin
  Beispiel();
end.