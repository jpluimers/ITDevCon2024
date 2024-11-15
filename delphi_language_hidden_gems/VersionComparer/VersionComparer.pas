{ This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.

  Written by Codehunter / 2019 / codehunter [at] gmx . net  }

unit VersionComparer;

interface

uses
  Winapi.Windows;

type
  TVersionElements = Byte;

  TVersion = record
  strict private type
    TIndex = (idxMajorMS, idxMajorLS, idxMinorMS, idxMinorLS);
  strict private
    FMajorMS: DWord;
    FMajorLS: DWord;
    FMinorMS: DWord;
    FMinorLS: DWord;
    FElements: TVersionElements;

    function GetElements: TVersionElements;
    procedure SetElements(const AValue: TVersionElements);

    procedure SetElement(const AIndex: TIndex; const AValue: DWord);
  public
    property Elements: TVersionElements read GetElements write SetElements;
    property MajorMS: DWord index idxMajorMS read FMajorMS write SetElement;
    property MajorLS: DWord index idxMajorLS read FMajorLS write SetElement;
    property MinorMS: DWord index idxMinorMS read FMinorMS write SetElement;
    property MinorLS: DWord index idxMinorLS read FMinorLS write SetElement;

    class operator Equal(A: TVersion; B: TVersion): Boolean;
    class operator Equal(A: TVersion; B: string): Boolean;
    class operator Equal(A: string; B: TVersion): Boolean;

    class operator Explicit(AString: string): TVersion;
    class operator Explicit(AVersion: TVersion): string;

    class operator GreaterThan(A: TVersion; B: TVersion): Boolean;
    class operator GreaterThan(A: TVersion; B: string): Boolean;
    class operator GreaterThan(A: string; B: TVersion): Boolean;

    class operator GreaterThanOrEqual(A: TVersion; B: TVersion): Boolean;
    class operator GreaterThanOrEqual(A: TVersion; B: string): Boolean;
    class operator GreaterThanOrEqual(A: string; B: TVersion): Boolean;

    class operator Implicit(AString: string): TVersion;
    class operator Implicit(AVersion: TVersion): string;

    class operator LessThan(A: TVersion; B: TVersion): Boolean;
    class operator LessThan(A: TVersion; B: string): Boolean;
    class operator LessThan(A: string; B: TVersion): Boolean;

    class operator LessThanOrEqual(A: TVersion; B: TVersion): Boolean;
    class operator LessThanOrEqual(A: TVersion; B: string): Boolean;
    class operator LessThanOrEqual(A: string; B: TVersion): Boolean;

    class operator NotEqual(A: TVersion; B: TVersion): Boolean;
    class operator NotEqual(A: TVersion; B: string): Boolean;
    class operator NotEqual(A: string; B: TVersion): Boolean;

    function ToString(const AElements: TVersionElements = 0): string;

    procedure Clear;
  end;

implementation

uses
  System.SysUtils;

resourcestring
  R_INVALID_VERSION_STR = '"%s" is not a valid Version';

procedure TVersion.Clear;
begin
  FElements := 0;
  FMajorMS  := 0;
  FMajorLS  := 0;
  FMinorMS  := 0;
  FMinorLS  := 0;
end;

class operator TVersion.Equal(A, B: TVersion): Boolean;
begin
  Result := (A.MajorMS = B.MajorMS) and
            (A.MajorLS = B.MajorLS) and
            (A.MinorMS = B.MinorMS) and
            (A.MinorLS = B.MinorLS);
end;

class operator TVersion.Equal(A: TVersion; B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := (A = LB);
end;

class operator TVersion.Equal(A: string; B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := (LA = B);
end;

class operator TVersion.Explicit(AVersion: TVersion): string;
begin
  Result := AVersion;
end;

class operator TVersion.Explicit(AString: string): TVersion;
begin
  Result := AString;
end;

class operator TVersion.GreaterThan(A, B: TVersion): Boolean;
begin
  Result := (A.MajorMS > B.MajorMS) or
            (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS > B.MajorLS)
            ) or (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS = B.MajorLS) and
              (A.MinorMS > B.MinorMS)
            ) or (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS = B.MajorLS) and
              (A.MinorMS = B.MinorMS) and
              (A.MinorLS > B.MinorLS)
            );
end;

class operator TVersion.GreaterThan(A: string;
  B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := (LA > B);
end;

class operator TVersion.GreaterThan(A: TVersion;
  B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := (A > LB);
end;

class operator TVersion.GreaterThanOrEqual(A,
  B: TVersion): Boolean;
begin
  Result := (A = B) or
            (A > B);
end;

class operator TVersion.GreaterThanOrEqual(A: TVersion;
  B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := (A >= B);
end;

class operator TVersion.GreaterThanOrEqual(A: string;
  B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := (LA >= B);
end;

class operator TVersion.Implicit(AVersion: TVersion): string;
begin
  Result := AVersion.ToString(AVersion.Elements);
end;

class operator TVersion.Implicit(AString: string): TVersion;
var
  I: TVersionElements;
  LElement: string;
  LElementI: Integer;
  LElements: TArray<string>;
begin
  Result.Clear;
  LElements := AString.Split(['.']);
  I := 1;
  for LElement in LElements do begin
    LElementI := StrToIntDef(LElement, -1);
    if LElementI > -1 then begin
      case I of
        1: Result.MajorMS := LElementI;
        2: Result.MajorLS := LElementI;
        3: Result.MinorMS := LElementI;
        4: Result.MinorLS := LElementI;
      end;
      Inc(I);
    end else begin
      Raise EConvertError.CreateFmt(R_INVALID_VERSION_STR, [AString]);
      Break;
    end;
  end;
//  Result.Elements := I - 1;
end;

class operator TVersion.LessThan(A, B: TVersion): Boolean;
begin
  Result := (A.MajorMS < B.MajorMS) or
            (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS < B.MajorLS)
            ) or (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS = B.MajorLS) and
              (A.MinorMS < B.MinorMS)
            ) or (
              (A.MajorMS = B.MajorMS) and
              (A.MajorLS = B.MajorLS) and
              (A.MinorMS = B.MinorMS) and
              (A.MinorLS < B.MinorLS)
            );
end;

class operator TVersion.LessThan(A: TVersion; B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := (A < LB);
end;

class operator TVersion.LessThan(A: string; B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := (LA < B);
end;

class operator TVersion.LessThanOrEqual(A, B: TVersion): Boolean;
begin
  Result := (A = B) or
            (A < B);
end;

class operator TVersion.LessThanOrEqual(A: TVersion;
  B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := (A = LB) or
            (A < LB);
end;

class operator TVersion.LessThanOrEqual(A: string;
  B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := (LA = B) or
            (LA < B);
end;

class operator TVersion.NotEqual(A, B: TVersion): Boolean;
begin
  Result := not (A = B);
end;

class operator TVersion.NotEqual(A: TVersion; B: string): Boolean;
var
  LB: TVersion;
begin
  LB := B;
  Result := not (A = LB);
end;

class operator TVersion.NotEqual(A: string; B: TVersion): Boolean;
var
  LA: TVersion;
begin
  LA := A;
  Result := not (LA = B);
end;

function TVersion.GetElements: TVersionElements;
begin
  if FElements < 1 then begin
    FElements := 1;
  end;
  Result := FElements;
end;

procedure TVersion.SetElements(const AValue: TVersionElements);
begin
  FElements := AValue;
  case FElements of
    1:
    begin
      FMajorLS := 0;
      FMinorMS := 0;
      FMinorLS := 0;
    end;

    2:
    begin
      FMinorMS := 0;
      FMinorLS := 0;
    end;

    3:
    begin
      FMinorLS := 0;
    end;
  end;
end;

procedure TVersion.SetElement(const AIndex: TIndex; const AValue: DWord);
var
  LElements: TVersionElements;
begin
  case AIndex of
    idxMajorMS: FMajorMS := AValue;
    idxMajorLS: FMajorLS := AValue;
    idxMinorMS: FMinorMS := AValue;
    idxMinorLS: FMinorLS := AValue;
  end;
  LElements := TVersionElements(Byte(AIndex) + 1);
  if (Elements = 0) or (Elements > 4) or (LElements > Elements) then begin
    Elements := LElements;
  end;
end;

function TVersion.ToString(
  const AElements: TVersionElements = 0): string;
var
  LElements: TVersionElements;
begin
  Result := '';
  if (AElements = 0) or (AElements > 4) then begin
    LElements := FElements;
  end else begin
    LElements := AElements;
  end;
  case LElements of
    1: Result := MajorMS.ToString;
    2: Result := Format('%d.%d', [MajorMS, MajorLS]);
    3: Result := Format('%d.%d.%d', [MajorMS, MajorLS, MinorMs]);
    4: Result := Format('%d.%d.%d.%d', [MajorMS, MajorLS, MinorMS, MinorLS]);
  end;
end;

end.
