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
    function CalculateCalibrationSum(CheckLetters: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    ResultA, ResultB: Integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    type
      TSymbolGrid = array of array of string;
    var
      Symbols: TList<TPoint>;
      Grid: TSymbolGrid;
      MaxX, MaxY: Integer;

    procedure GetNumbersForPoint(var aGrid: TSymbolGrid; ResultList: TList<Integer>; aPoint: TPoint; ClearNumbers: Boolean);
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    WinningCount: Array of integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  SeedRange = Record
  public
    RangeStart, RangeStop: Int64;
    class function Create(const aRangeStart, aRangeStop: Int64): SeedRange; static;
  End;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    function PlantSeeds(Seeds: TList<SeedRange>): Int64;
  protected
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
function TAdventOfCodeDay1.CalculateCalibrationSum(CheckLetters: Boolean): integer;
const
  Numbers: array[1..9] of string = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');
var
  s: string;
  i, j: integer;
  num1, num2, num: Integer;
begin
  Result := 0;

  for s in FInput do
  begin
    Num1 := -1;
    num2 := -1;

    for j := 1 to Length(s) do
    begin
      if not TryStrToInt(s[j], num) and CheckLetters then
      begin
        for i := 1 to 9 do
        begin
          if pos(numbers[i], s, j) = j then
          begin
            num := i;
            Break;
          end;
        end;
      end;

      if num > 0 then
      begin
        if num1 <= 0 then
          num1 := num;
        num2 := num;
      end;
    end;

    Result := Result + Num1*10 + num2;
  end;
end;

function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := CalculateCalibrationSum(False);
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := CalculateCalibrationSum(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay2'}
procedure TAdventOfCodeDay2.BeforeSolve;
  var
  MaxCubes: Array[0..2] of integer;
  s: String;
  Split: TStringDynArray;
  GameId, Cubes, Idx, ColorIdx: integer;
begin
  ResultA := 0;
  ResultB := 0;

  for s in FInput do
  begin
    Split := SplitString(s.Replace(',', '').Replace(':','').Replace(';',''), ' ');

    GameId := StrToInt(Split[1]);
    idx := 2;
    MaxCubes[0] := 0;
    MaxCubes[1] := 0;
    MaxCubes[2] := 0;

    while idx < Length(Split) do
    begin
      Cubes := StrToInt(Split[idx]);
      ColorIdx := IndexStr(Split[idx+1], ['red', 'green', 'blue']);
      MaxCubes[ColorIdx] := Max(MaxCubes[ColorIdx], Cubes);
      Inc(Idx, 2);
    end;

    if (MaxCubes[0] <= 12) and (MaxCubes[1] <= 13) and (MaxCubes[2] <= 14) then
      Inc(ResultA, GameId);

    Inc(ResultB, MaxCubes[0] * MaxCubes[1] * MaxCubes[2]);
  end;
end;

function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay3'}
procedure TAdventOfCodeDay3.BeforeSolve;
var
  x, y, i: integer;
  c: char;
begin
  Symbols := TList<TPoint>.Create;

  MaxX := Length(FInput[0]);
  MaxY := FInput.Count;

  SetLength(Grid, MaxX);
  for x := 0 to MaxX-1 do
  begin
    SetLength(Grid[x], MaxY);
    for y := 0 to MaxY-1 do
    begin
      c := FInput[y][x+1];

      if c = '.' then
        Continue;

      Grid[x][y] := c;

      if not TryStrToInt(c, i) then
        Symbols.Add(TPoint.Create(x, y))
    end;
  end;
end;

procedure TAdventOfCodeDay3.AfterSolve;
begin
  Symbols.Free;
end;

procedure TAdventOfCodeDay3.GetNumbersForPoint(var aGrid: TSymbolGrid; ResultList: TList<Integer>; aPoint: TPoint; ClearNumbers: Boolean);

  function _InternalGetNumber(aX, aY: integer): Boolean;
  var
    TotalNum, CurrX: Integer;
  begin
    Result := False;

    if not InRange(aX, 0, MaxX) or not InRange(aY, 0, MaxY) then
      Exit(False); // Out of range

    if not IsNumber(aGrid[aX][aY]) then
      Exit; // Empty or a symbol;

    CurrX := aX;
    while True do // Move to start;
    begin
      if CurrX - 1 < 0 then
        Break; // At the edge

      if not IsNumber(aGrid[CurrX-1][aY]) then
        Break; // Char to te left is nan;

      Dec(CurrX);
    end;

    TotalNum := 0;
    while True do // Read number
    begin
      if CurrX >= MaxX then
        Break; // At the edge

      if not IsNumber(aGrid[CurrX][aY]) then
        Break; // Done reading

      TotalNum := 10 * TotalNum + StrToInt(aGrid[CurrX][aY]);
      if ClearNumbers then
        aGrid[CurrX][aY] := '';
      inc(CurrX);
    end;

    Result := True;
    ResultList.Add(TotalNum);
  end;

begin
  ResultList.Clear;

  // Left of Point;
  _InternalGetNumber(aPoint.X-1, aPoint.Y);

  // Right of Point;
  _InternalGetNumber(aPoint.X+1, aPoint.Y);

  // Above Point;
  if not _InternalGetNumber(aPoint.X, aPoint.Y+1) then
  begin
    _InternalGetNumber(aPoint.X-1, aPoint.Y+1); // Left top
    _InternalGetNumber(aPoint.X+1, aPoint.Y+1); // Right top
  end;

  // Below Point;
  if not _InternalGetNumber(aPoint.X, aPoint.Y-1) then
  begin
    _InternalGetNumber(aPoint.X-1, aPoint.Y-1); // Left bottom
    _InternalGetNumber(aPoint.X+1, aPoint.Y-1); // Right bottom
  end;
end;

function TAdventOfCodeDay3.SolveA: Variant;
var
  LocalGrid: TSymbolGrid;
  CurrentPoint: TPoint;
  X, Y, I: Integer;
  ResultList: TList<Integer>;
begin
  Result := 0;

  SetLength(LocalGrid, MaxX);
  for x := 0 to MaxX - 1 do
  begin
    SetLength(LocalGrid[X], MaxY);
    for y := 0 to MaxY - 1 do
      LocalGrid[X][Y] := Grid[X][Y]
  end;

  ResultList := TList<Integer>.Create;

  for CurrentPoint in Symbols do
  begin
    GetNumbersForPoint(LocalGrid, ResultList, CurrentPoint, True);
    for I in ResultList do
      Inc(Result, I);
  end;

  ResultList.Free;
end;

function TAdventOfCodeDay3.SolveB: Variant;
var
  CurrentPoint: TPoint;
  ResultList: TList<Integer>;
begin
  Result := 0;
  ResultList := TList<Integer>.Create;

  for CurrentPoint in Symbols do
  begin
    if Grid[CurrentPoint.X][CurrentPoint.Y] <> '*' then
      Continue;

    GetNumbersForPoint(Grid, ResultList, CurrentPoint, False);
    if ResultList.Count = 2 then
      Inc(Result, ResultList[0] * ResultList[1]);
  end;

  ResultList.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay4'}
procedure TAdventOfCodeDay4.BeforeSolve;

  function GetCards(aInput: string): SetOfByte;
  var
    s: String;
    i: Integer;
  begin
    Result := [];

    for s in SplitString(aInput, ' ') do
      if TryStrToInt(s, i) then
        Include(Result, i);
  end;

var
  split: TStringDynArray;
  match: Byte;
  CardIndex: integer;
begin
  SetLength(WinningCount, FInput.Count);

  for CardIndex := 0 to FInput.Count -1 do
  begin
    split := SplitString(FInput[CardIndex], ':|');

    for match in GetCards(Split[1]) * GetCards(Split[2]) do
      WinningCount[CardIndex] := WinningCount[CardIndex] + 1;
  end;
end;

function TAdventOfCodeDay4.SolveA: Variant;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(WinningCount) -1 do
    if WinningCount[i] > 0 then
      Inc(Result, 1 shl (WinningCount[i]-1));
end;

function TAdventOfCodeDay4.SolveB: Variant;
var
  WinningIndex, idx: integer;
  Cards: Array of integer;
begin
  SetLength(Cards, Length(WinningCount));
  for idx := 0 to Length(WinningCount)-1 do
    Cards[idx] := 1;

  Result := 0;
  for WinningIndex := 0 to Length(WinningCount)-1 do
  begin
    for Idx := 1 to WinningCount[WinningIndex] do
      Cards[WinningIndex + Idx] := Cards[WinningIndex + Idx] + Cards[WinningIndex];
        Inc(Result, Cards[WinningIndex]);
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay5'}
class function SeedRange.Create(const aRangeStart, aRangeStop: Int64): SeedRange;
begin
  Result.RangeStart := aRangeStart;
  Result.RangeStop := aRangeStop;
end;

function TAdventOfCodeDay5.PlantSeeds(Seeds: TList<SeedRange>): Int64;
var
  split: TStringDynArray;
  i, idx, SeedIdx: Integer;
  NewSeeds: TList<SeedRange>;
  Seed: SeedRange;
  Dest_Start, Dest_Stop, Source_Start, RangeLength, Source_Stop, LocalOffSet, Seed_Start, Seed_Stop: Int64;
  StartInRange, StopInRange, FullOverlap: Boolean;
begin
  NewSeeds := TList<SeedRange>.Create;

  idx := 3;
  while idx < FInput.Count -1 do
  begin
    if FInput[idx] = '' then
    begin
      inc(idx, 2);

      for Seed in Seeds do
        NewSeeds.Add(Seed);

      Seeds.Free;
      Seeds := NewSeeds;
      NewSeeds := TList<SeedRange>.Create;
    end;

    split := SplitString(FInput[Idx], ' ');
    Dest_Start := Split[0].ToInt64;
    Source_Start := Split[1].ToInt64;
    RangeLength := Split[2].ToInt64;

    Source_Stop := Source_Start + RangeLength;
    Dest_Stop := Dest_Start + RangeLength;

    for SeedIdx := Seeds.Count-1 downto 0 do
    begin
      Seed := Seeds.ExtractAt(SeedIdx);

      Seed_Start := Seed.RangeStart;
      Seed_Stop := Seed.RangeStop;

      StartInRange := InRange(Seed_Start, Source_Start, Source_Stop);
      StopInRange  := InRange(Seed_Stop, Source_Start, Source_Stop);

      if StartInRange and StopInRange then
      begin
        LocalOffSet := Seed_Start - Source_Start;
        NewSeeds.Add(SeedRange.Create(Dest_Start + LocalOffSet, Dest_Start + LocalOffSet + Seed_Stop - Seed_Start));
      end
      else if StartInRange then
      begin
        NewSeeds.Add(SeedRange.Create(Dest_Start + Seed_Start - Source_Start, Dest_Stop ));
        Seeds.Add(SeedRange.Create(Source_Stop + 1, Seed_Stop ))
      end
      else if StopInRange  then
      begin
        NewSeeds.Add(SeedRange.Create(Dest_Start, Dest_Start + Seed_Stop - Source_Start));
        Seeds.Add(SeedRange.Create(Seed_Start, Source_Start-1));
      end
      else if (Seed_Start <= Source_Start) and (Seed_Stop >= Source_Stop) then
      begin
        NewSeeds.Add(SeedRange.Create(Dest_Start, Dest_Stop));

        Seeds.Add(SeedRange.Create(Seed_Start, Source_Start-1));
        Seeds.Add(SeedRange.Create(Source_Stop + 1, Seed_Stop));
      end
      else
        Seeds.Add(Seed);
    end;

    Inc(idx);
  end;

  Result := MaxInt64;
  for Seed in nEWSeeds do
    Result := Min(Result, Seed.RangeStart);
  for Seed in Seeds do
    Result := Min(Result, Seed.RangeStart);
  Seeds.Free;
  NewSeeds.Free;
end;

//function TAdventOfCodeDay5.PlantSeeds(Seeds: TList<SeedRange>): Int64;
//var
//  s: String;
//  split: TStringDynArray;
//  i, idx, SeedIdx: int64;
//  NewSeeds: TList<SeedRange>;
//  Seed: SeedRange;
//  Dest_Start, Dest_Stop, Source_Start, RangeLength, Source_Stop, LocalOffSet: int64;
//  StartInRange, StopInRange, FullOverlap: Boolean;
//
//  Seed_Start, Seed_Stop: int64 ;
//begin
//  NewSeeds := TList<SeedRange>.Create;
//
//  idx := 3;
//  while idx < FInput.Count -1 do
//  begin
//    if FInput[idx] = '' then
//    begin
//      inc(idx, 2);
//
//      for Seed in Seeds do
//        NewSeeds.Add(Seed);
//
//      Seeds.Free;
//      Seeds := NewSeeds;
//      NewSeeds := TList<SeedRange>.Create;
//    end;
//
//    split := SplitString(FInput[Idx], ' ');
//    Dest_Start := Split[0].ToInt64;
//    Source_Start := Split[1].ToInt64;
//    RangeLength := Split[2].ToInt64;
//
//    Source_Stop := Source_Start + RangeLength;
//    Dest_Stop := Dest_Start + RangeLength;
//
//    for SeedIdx := Seeds.Count-1 downto 0 do
//    begin
//      Seed := Seeds.ExtractAt(SeedIdx);
//
//      Seed_Start := Seed.RangeStart;
//      Seed_Stop := Seed.RangeStop;
//
//      StartInRange := InRange(Seed_Start, Source_Start, Source_Stop);
//      StopInRange  := InRange(Seed_Stop, Source_Start, Source_Stop);
//
//      if StartInRange and StopInRange then
//      begin
//        LocalOffSet := Seed_Start - Source_Start;
//        NewSeeds.Add(SeedRange.Create(Dest_Start + LocalOffSet, Dest_Start + LocalOffSet + Seed_Stop - Seed_Start));
//        Continue;
//      end;
//
//      if StartInRange then
//      begin
//        NewSeeds.Add(SeedRange.Create(Dest_Start + Seed_Start - Source_Start, Dest_Stop ));
//        Seeds.Add(SeedRange.Create(Source_Stop + 1, Seed_Stop ))
//      end
//      else if StopInRange  then
//      begin
//        NewSeeds.Add(SeedRange.Create(Dest_Start, Dest_Start + Seed_Stop - Source_Start));
//        Seeds.Add(SeedRange.Create(Seed_Start, Source_Start-1));
//      end
//      else if (Seed_Start <= Source_Start) and (Seed_Stop >= Source_Stop) then
//      begin
//        NewSeeds.Add(SeedRange.Create(Dest_Start, Dest_Stop));
//
//        Seeds.Add(SeedRange.Create(Seed_Start, Source_Start-1));
//        Seeds.Add(SeedRange.Create(Source_Stop + 1, Seed_Stop));
//      end
//      else
//        Seeds.Add(Seed);
//    end;
//
//    Inc(idx);
//  end;
//
//  Result := MaxInt64;
//  for Seed in nEWSeeds do
//    Result := Min(Result, Seed.RangeStart);
//  for Seed in Seeds do
//    Result := Min(Result, Seed.RangeStart);
//end;

function TAdventOfCodeDay5.SolveA: Variant;
var
  s: String;
  Seeds: TList<SeedRange>;
begin
  Seeds := TList<SeedRange>.Create;

  for s in SplitString(FInput[0].Replace('seeds: ', ''), ' ') do
    Seeds.Add(SeedRange.Create(s.ToInt64, s.ToInt64));

  Result := PlantSeeds(Seeds);
end;

function TAdventOfCodeDay5.SolveB: Variant;
var
  split: TStringDynArray;
  i: integer;
  Seeds: TList<SeedRange>;
begin
  Seeds := TList<SeedRange>.Create;

  split := SplitString(FInput[0].Replace('seeds: ', ''), ' ');
  i := 0;
  while i < Length(split) do
  begin
    Seeds.Add(SeedRange.Create(Split[i].ToInt64, Split[i].ToInt64 + Split[i+1].ToInt64));
    Inc(i, 2);;
  end;

  Result := PlantSeeds(Seeds);
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
  split: TStringDynArray;
begin
  for s in FInput do
  begin
    split := SplitString(s, ',');
    Writeln(s);
  end;
end;

function TAdventOfCodeDay.SolveB: Variant;
begin
  Result := null;
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5]);

end.
