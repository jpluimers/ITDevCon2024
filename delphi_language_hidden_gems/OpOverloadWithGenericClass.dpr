// https://gist.github.com/jpluimers/f9db5497259e3f0ea6bcec9bf654603f
// which was forked of the now now non-existing https://gist.github.com/lynatan/673e574faa8343fa01d7a91e75065c54
//
// see https://api.github.com/gists/673e574faa8343fa01d7a91e75065c54/forks
// Luckily that revealed the username changed, so the code was not deleted:
// https://gist.github.com/superswanman/673e574faa8343fa01d7a91e75065c54
// (via

program OpOverloadWithGenericClass;

uses
  Classes;

type
  TObjectHelper = class helper for TObject
  public
    class function &&op_LogicalOr<T: class>(A, B: T): T; static;
  end;

class function TObjectHelper.&&op_LogicalOr<T>(A, B: T): T;
begin
  if A <> nil then
    Result := A
  else
    Result := B;
end;

procedure Test;
var
  sl1, sl2, sl3: TStringList;
begin
  sl1 := nil;
  sl2 := TStringList.Create;
  sl3 := sl1 or sl2; // -> sl3 = sl2
end;

begin
  Test();
  ReadLn;
end.