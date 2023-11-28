unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, RTTI,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json,
  AocLetterReader;

type
  SetOfByte = Set of Byte;

  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    CaloriesPerElf: TList<integer>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;


  TAdventOfCodeDay = class(TAdventOfCode)
  private

  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;


  const
    DeltaX: Array[0..3] of integer = (1, -1, 0, 0);
    DeltaY: Array[0..3] of integer = (0, 0, -1, 1);

implementation

{$REGION 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;
var
  i, TotalCalories, Calories: integer;
begin
  inherited;

  CaloriesPerElf := TList<integer>.Create;

  TotalCalories := 0;
  for i := 0 to FInput.Count - 1 do
  Begin
    if TryStrToInt(FInput[i], Calories) then
      inc(TotalCalories, Calories)
    else
    begin
      CaloriesPerElf.Add(TotalCalories);
      TotalCalories := 0;
    end;
  End;

  CaloriesPerElf.Add(TotalCalories);
  CaloriesPerElf.Sort;
  CaloriesPerElf.Reverse;
end;

procedure TAdventOfCodeDay1.AfterSolve;
begin
  inherited;
  CaloriesPerElf.Free;
end;

function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := CaloriesPerElf.First;
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := CaloriesPerElf[0] + CaloriesPerElf[1] + CaloriesPerElf[2];
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay'}
procedure TAdventOfCodeDay.BeforeSolve;
begin
  inherited;

end;

procedure TAdventOfCodeDay.AfterSolve;
begin
  inherited;

end;

function TAdventOfCodeDay.SolveA: Variant;
var
  s: String;
begin
  for s in FInput do
    Writeln(s);
end;

function TAdventOfCodeDay.SolveB: Variant;
begin
  Result := null;
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1]);

end.
