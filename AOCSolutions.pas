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

  FertilizerRule = record
    Source_Start, Source_Stop, Destination_Start, Destination_Stop: int64;
    class function Create(aSource_Start, aDestination_Start, aRange: int64): FertilizerRule; Static;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    Rules: TDictionary<String,TList<FertilizerRule>>;
    Map: TDictionary<string, string>;

    function PlantSeeds(Seeds: TList<SeedRange>): Int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    function RaceBoats(aTime, aDistance: Int64): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function PlayCamelCards(UseJoker: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  LeftRight = (Left, Right);
  TAdventOfCodeDay8 = class(TAdventOfCode)
  private
    NodeNameLookup: TDictionary<string, Integer>;
    NodeNames: array of string;
    Nodes: Array[LeftRight] of Array of Integer;
    Instructions: array of LeftRight;
    StartNodesPartB: TList<integer>;

    function NodeNameToIndex(Const aNodeName: string): integer;
    function NavigateWasteLand(StartNodeIndex: Integer; UseZZZ: Boolean): Int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    PrevState, NextState: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  type
    PipeSegment = (None, Vertical, Horizontal, L, J, Seven, F);
    PipeSegments = set of PipeSegment;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    PipeLoop: TDictionary<TPoint,PipeSegment>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    ResultA, ResultB: Int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
    function AnalyzeHotSprings(UnfoldRecords: Boolean): int64;

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

class function FertilizerRule.Create(aSource_Start, aDestination_Start, aRange: int64): FertilizerRule;
begin
  Result.Source_Start := aSource_Start;
  Result.Source_Stop := aSource_Start + aRange;
  Result.Destination_Start := aDestination_Start;
  Result.Destination_Stop := aDestination_Start + aRange;
end;


procedure TAdventOfCodeDay5.BeforeSolve;
var
  Idx: Integer;
  CurrRule: TList<FertilizerRule>;
  CurrState: string;
  Split: TStringDynArray;
begin
  inherited;

  Rules := TObjectDictionary<String, TList<FertilizerRule>>.Create([doOwnsValues]);
  Map := TDictionary<string,string>.Create;

  CurrRule := nil;
  CurrState := '';
  for idx := 1 to FInput.Count -1 do
  begin
    if FInput[idx] = '' then
      Continue;

    Split := SplitString(FInput[idx], ' ');
    if not IsNumber(Split[0]) then
    begin
      Split := SplitString(FInput[Idx], ' -');

      CurrState := Split[0];
      Map.Add(CurrState, Split[2]);
      CurrRule := TList<FertilizerRule>.Create;
      Rules.Add(CurrState, CurrRule);
    end
    else
      CurrRule.Add(FertilizerRule.Create(Split[1].ToInt64, Split[0].ToInt64, Split[2].ToInt64))
  end;
end;

procedure TAdventOfCodeDay5.AfterSolve;
begin
  inherited;

  Map.Free;
  Rules.Free;
end;

function TAdventOfCodeDay5.PlantSeeds(Seeds: TList<SeedRange>): Int64;
var
  i, idx, SeedIdx: Integer;
  NewSeeds: TList<SeedRange>;
  Rule: FertilizerRule;
  Seed: SeedRange;
  LocalOffSet: Int64;
  StartInRange, StopInRange, FullOverlap: Boolean;
  CurrentState: string;
begin
  CurrentState := 'seed';
  NewSeeds := TList<SeedRange>.Create;

  while currentState <> 'location' do
  begin
    for Rule in Rules[CurrentState] do
    begin
      for SeedIdx := Seeds.Count-1 downto 0 do
      begin
        Seed := Seeds[SeedIdx];

        StartInRange := InRange(Seed.RangeStart, Rule.Source_Start, Rule.Source_Stop);
        StopInRange  := InRange(Seed.RangeStop, Rule.Source_Start, Rule.Source_Stop);
        FullOverlap := (Seed.RangeStart <= Rule.Source_Start) and (Seed.RangeStop >= Rule.Source_Stop);

        if not (StartInRange or StopInRange or FullOverlap) then
          Continue;

        Seeds.Delete(SeedIdx);

        if StartInRange and StopInRange then
        begin
          LocalOffSet := Seed.RangeStart - Rule.Source_Start;
          NewSeeds.Add(SeedRange.Create(Rule.Destination_Start + LocalOffSet, Rule.Destination_Start + LocalOffSet + Seed.RangeStop - Seed.RangeStart));
        end
        else if StartInRange then
        begin
          NewSeeds.Add(SeedRange.Create(Rule.Destination_Start + Seed.RangeStart - Rule.Source_Start, Rule.Destination_Stop ));
          Seeds.Add(SeedRange.Create(Rule.Source_Stop + 1, Seed.RangeStop ))
        end
        else if StopInRange  then
        begin
          NewSeeds.Add(SeedRange.Create(Rule.Destination_Start, Rule.Destination_Start + Seed.RangeStop - Rule.Source_Start));
          Seeds.Add(SeedRange.Create(Seed.RangeStart, Rule.Source_Start-1));
        end
        else if FullOverlap then
        begin
          NewSeeds.Add(SeedRange.Create(Rule.Destination_Start, Rule.Destination_Stop));

          Seeds.Add(SeedRange.Create(Seed.RangeStart, Rule.Source_Start-1));
          Seeds.Add(SeedRange.Create(Rule.Source_Stop + 1, Seed.RangeStop));
        end
      end;
    end;

    for Seed in Seeds do
      NewSeeds.Add(Seed);

    Seeds.Free;
    Seeds := NewSeeds;
    NewSeeds := TList<SeedRange>.Create;

    CurrentState := Map[CurrentState];
  end;

  Result := MaxInt64;
  for Seed in Seeds do
    Result := Min(Result, Seed.RangeStart);
 
  Seeds.Free;
  NewSeeds.Free;
end;

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
{$REGION 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.RaceBoats(aTime, aDistance: Int64): Int64;
var
  a, b, c, d, r1, r2: Int64;
begin
  // https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap03/quad-2.html
  a := -1;
  b := aTime;
  c := -aDistance;
  d := b * b - 4 * a * c;

  r1 := Ceil((-b + sqrt(d))/(2*a));
  r2 := Trunc((-b - sqrt(d))/(2*a));

  result := r2-r1 + 1;
end;

function TAdventOfCodeDay6.SolveA: Variant;
Var
  SplitTime, SplitDistance: TStringDynArray;
  i: integer;
begin
  SplitTime := SplitString(DeleteRepeatedSpaces(FInput[0]), ' ');
  SplitDistance := SplitString(DeleteRepeatedSpaces(FInput[1]), ' ');

  Result := 1;
  for i := 1 to Length(SplitTime)-1 do
    Result := Result * RaceBoats(SplitTime[i].ToInt64, SplitDistance[i].ToInt64);
end;

function TAdventOfCodeDay6.SolveB: Variant;
Var
  SplitTime, SplitDistance: TStringDynArray;
begin
  SplitTime := SplitString((StringReplace(FInput[0], ' ', '', [rfReplaceAll])), ':');
  SplitDistance := SplitString((StringReplace(FInput[1], ' ', '', [rfReplaceAll])), ':');

  Result := RaceBoats(SplitTime[1].ToInt64, SplitDistance[1].ToInt64);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.PlayCamelCards(UseJoker: Boolean): integer;

  function ReadHand(const aHand: string): int64;
  const
    CardStrengths: array[boolean] of string = ('AKQJT98765432', 'AKQT98765432J');
    JokerStrength: integer = 0;
  var
    Split: TStringDynArray;
    i, ActiveCards, CardIndex, CardStrength, TypeStrength, LetterStrength, MostLetters, JokerCount: int64;
    CardTotals: Array[0..12] of integer;
  begin
    for i := 0 to 12 do
      CardTotals[i] := 0;

    Split := SplitString(aHand, ' ');

    LetterStrength := 0;
    ActiveCards := 0;
    for CardIndex := 1 to 5  do
    begin
      CardStrength := 13-Pos(Split[0][CardIndex], CardStrengths[UseJoker]);

      ActiveCards := ActiveCards or 1 shl CardStrength;
      CardTotals[CardStrength] := CardTotals[CardStrength] + 1;
      LetterStrength := (LetterStrength shl 4) + CardStrength;
    end;

    MostLetters := MaxIntValue(CardTotals);
    if UseJoker and (CardTotals[JokerStrength] > 0) and (CountTrueBits(ActiveCards) > 1) then
    begin
      JokerCount := CardTotals[JokerStrength];
      CardTotals[JokerStrength] := 0;
      ActiveCards := ActiveCards and not 1;
      MostLetters := MaxIntValue(CardTotals) + JokerCount;
    end;

    TypeStrength := 0;
    case CountTrueBits(ActiveCards) of
      1: TypeStrength := 6;// Five of a kind
      2: TypeStrength := MostLetters + 1; // Four of a kind / Full house
      3: TypeStrength := MostLetters; // Three of a kind / two pair
      4: TypeStrength := 1; // One pair
      5: TypeStrength := 0; // High Card
    end;

    Result := (TypeStrength shl 52) +
              (LetterStrength shl 32) +
              Split[1].ToInt64
  end;

var
  Queue: PriorityQueue<Int64>;
  s: String;
  idx: integer;
begin
  Queue := PriorityQueue<Int64>.Create();
  for s in FInput do
    Queue.Enqueue(ReadHand(s));

  Result := 0;
  Idx := 1;
  while Queue.Count > 0 do
  begin
    Result := Result + Idx * (Queue.Dequeue and maxInt);
    Inc(Idx)
  end;
end;

function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := PlayCamelCards(False);
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := PlayCamelCards(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
procedure TAdventOfCodeDay8.BeforeSolve;
var
  CurrentNodeIndex, i: Integer;
  CurrentNode: string;
begin
  SetLength(Instructions, Length(FInput[0]));
  SetLength(Nodes[Left], FInput.Count - 2);
  SetLength(Nodes[Right], FInput.Count - 2);
  SetLength(NodeNames, FInput.Count - 2);

  NodeNameLookup := TDictionary<string, Integer>.Create;
  StartNodesPartB := TList<integer>.Create;

  for i := 1 to Length(FInput[0]) do
    Instructions[i-1] := LeftRight(IfThen(FInput[0][i] = 'L', Ord(Left), Ord(Right)));

  for i := 2 to FInput.Count -1 do
  begin
    CurrentNode := Copy(FInput[i], 1, 3);
    CurrentNodeIndex := NodeNameToIndex(CurrentNode);

    if CurrentNode[3] = 'A' then
      StartNodesPartB.Add(CurrentNodeIndex);
    
    Nodes[Left][CurrentNodeIndex] := NodeNameToIndex(Copy(FInput[i], 8, 3));
    Nodes[Right][CurrentNodeIndex] := NodeNameToIndex(Copy(FInput[i], 13, 3));
    NodeNames[CurrentNodeIndex] := CurrentNode;
  end;
end;

procedure TAdventOfCodeDay8.AfterSolve;
begin
  StartNodesPartB.Free;
  NodeNameLookup.Free;
end;

function TAdventOfCodeDay8.NodeNameToIndex(Const aNodeName: string): integer;
begin
  if NodeNameLookup.TryGetValue(aNodeName, Result) then
    Exit;

  Result := NodeNameLookup.Count;
  NodeNameLookup.Add(aNodeName, Result);
end;

function TAdventOfCodeDay8.NavigateWasteLand(StartNodeIndex: integer; UseZZZ: Boolean): Int64;
var
  CurrentNodeIndex, InstructionIndex: integer;
  CurrentNode: string;
begin
  Result := 0;

  CurrentNodeIndex := StartNodeIndex;
  InstructionIndex := 0;

  while True do
  begin
    Inc(Result);

    CurrentNodeIndex := Nodes[Instructions[InstructionIndex]][CurrentNodeIndex];
    CurrentNode := NodeNames[CurrentNodeIndex];

    if ((CurrentNode[3] = 'Z') and not UseZZZ) or (CurrentNode = 'ZZZ') then
      Exit;

    InstructionIndex := (InstructionIndex + 1) mod Length(Instructions);
  end;
end;

function TAdventOfCodeDay8.SolveA: Variant;
begin
  Result := NavigateWasteLand(NodeNameToIndex('AAA'), True);
end;

function TAdventOfCodeDay8.SolveB: Variant;
var
  StepsTaken: Int64;
  CurrentNodeIndex: integer;
begin
  Result := 0;
  for CurrentNodeIndex in StartNodesPartB do
  begin
    StepsTaken := NavigateWasteLand(CurrentNodeIndex, False);

    if Result = 0 then
      Result := StepsTaken
    else
      Result := LCM(Result, StepsTaken);
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay9'}
procedure TAdventOfCodeDay9.BeforeSolve;
var
  s: string;
  Nums: TList<Int64>;
  FirstNums: TStack<Int64>;
  split: TStringDynArray;
  TempPrev, i, Diff: Int64;
  AllZeros: Boolean;
begin
  PrevState := 0;
  NextState := 0;

  Nums := TList<Int64>.Create;
  FirstNums := TStack<Int64>.Create;

  for s in FInput do
  begin
    Nums.Clear;
    split := SplitString(s, ' ');
    for i := 0 to Length(split)-1 do
      Nums.Add(Split[i].ToInt64);

    NextState := NextState + Nums.Last;
    FirstNums.Push(Nums.First);

    AllZeros := False;
    while not AllZeros do
    begin
      AllZeros := True;

      for i := 0 to Nums.Count-2 do
      begin
        Diff := Nums[i+1] - Nums[i];
        Nums[i] := Diff;
        AllZeros := AllZeros and (Diff = 0)
      end;

      Nums.Delete(Nums.Count-1);

      NextState := NextState + Nums.Last;
      FirstNums.Push(Nums.First);
    end;

    TempPrev := 0;
    while FirstNums.Count > 0 do
      TempPrev := FirstNums.Pop - TempPrev;

    Inc(PrevState, TempPrev);
  end;

  Nums.Free;
  FirstNums.Free;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := NextState;
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := PrevState;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay10'}

procedure TAdventOfCodeDay10.BeforeSolve;
Const
  TestSegment: PipeSegment = F;
  RealSegment: PipeSegment = J;

  function _PickNext(aPrev, aNext1, aNext2: TPoint): TPoint;
  begin
    Result := aNext1;
    if aPrev = aNext1 then
      Result := aNext2
  end;

var
  x, y: Integer;
  Map: TDictionary<TPoint, PipeSegment>;
  Segment: PipeSegment;
  Start, Next, NewNext, Prev: TPoint;
begin
  PipeLoop := TDictionary<TPoint,PipeSegment>.Create;

  Map := TDictionary<TPoint,PipeSegment>.Create;

  for y := 0 to FInput.Count-1 do
    for x := 1 to Length(FInput[0]) do
    begin
      Segment := None;
      case IndexStr(FInput[Y][X], ['|', '-', 'L', 'J', '7', 'F', '.', 'S']) of
        0: Segment := Vertical;
        1: Segment := Horizontal;
        2: Segment := L;
        3: Segment := J;
        4: Segment := Seven;
        5: Segment := F;
        6: Segment := None;
        7: begin
            Segment := RealSegment; // TODO
            Start := TPoint.Create(X-1, Y);
           end;
        else
          Assert(False);
      end;
      Map.Add(TPoint.Create(X-1, Y), Segment);
    end;

  Next := TPoint.Create(Start);
  Prev := TPoint.Create(Next);

  while (Next <> Start) or (PipeLoop.Count = 0) do
  begin
    Segment := Map[Next];

    case Map[Next] of
      Vertical   : NewNext := _PickNext(Prev, TPoint.Create(Next.X, Next.Y-1), TPoint.Create(Next.X, Next.Y+1));
      Horizontal : NewNext := _PickNext(Prev, TPoint.Create(Next.X-1, Next.Y), TPoint.Create(Next.X+1, Next.Y));
      L          : NewNext := _PickNext(Prev, TPoint.Create(Next.X, Next.Y-1), TPoint.Create(Next.X+1, Next.Y));
      J          : NewNext := _PickNext(Prev, TPoint.Create(Next.X-1, Next.Y), TPoint.Create(Next.X, Next.Y-1));
      Seven      : NewNext := _PickNext(Prev, TPoint.Create(Next.X-1, Next.Y), TPoint.Create(Next.X, Next.Y+1));
      F          : NewNext := _PickNext(Prev, TPoint.Create(Next.X+1, Next.Y), TPoint.Create(Next.X, Next.Y+1));
    else
      Assert(False)
    end;

    PipeLoop.Add(Next, Segment);

    Prev := TPoint.Create(Next);
    Next := TPoint.Create(NewNext);
  end;
end;

procedure TAdventOfCodeDay10.AfterSolve;
begin
  PipeLoop.Free;
end;

function TAdventOfCodeDay10.SolveA: Variant;
begin
  Result := PipeLoop.Count shr 1;
end;

function TAdventOfCodeDay10.SolveB: Variant;
var
  x, y, SegmentCrossings: Integer;
  Segment: PipeSegment;
begin
  // https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
  Result := 0;
  for y := 0 to FInput.Count -1 do
  begin
    SegmentCrossings := 0;
    for x := 0 to Length(FInput[0]) -1 do
    begin
      if PipeLoop.TryGetValue(TPoint.Create(x, y), Segment) then
        if Segment in [Vertical, F, Seven] then
          Inc(SegmentCrossings);

      if (SegmentCrossings and 1 = 1) and not PipeLoop.ContainsKey(TPoint.Create(x, y)) then
      Inc(Result);
    end;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
procedure TAdventOfCodeDay11.BeforeSolve;
var
  x, y, i, j, GalaxyCount: Integer;
  Found: Boolean;
  Current, Target: TPoint;
  BaseDist: Int64;
  EmptyRows, EmptyCols: TList<Integer>;
  Galaxies: array of TPoint;
begin
  ResultA := 0;
  ResultB := 0;

  EmptyRows := TList<Integer>.Create;
  EmptyCols := TList<Integer>.Create;
  GalaxyCount := 0;

  for y := 0 to FInput.Count-1 do
  begin
    Found := False;
    for x := 1 to Length(FInput[0]) do
      if FInput[y][x] = '#' then
      begin
        Found := True;
        inc(GalaxyCount);
      end;

    if not found then
      EmptyRows.Add(y);
  end;

  SetLength(Galaxies, GalaxyCount);
  GalaxyCount := 0;

  for x := 1 to Length(FInput[0]) do
  begin
    Found := False;
    for y := 0 to FInput.Count-1 do
    if FInput[y][x] = '#' then
      begin
        Found := True;
        Galaxies[GalaxyCount] := TPoint.Create(x, y);
        Inc(GalaxyCount);
      end;

    if not found then
      EmptyCols.Add(x);
  end;

  for i := 0 to GalaxyCount-1 do
  begin
    Current := Galaxies[i];
    for j := i+1 to GalaxyCount-1 do
    begin
      Target := Galaxies[J];

      BaseDist := Abs(Target.x - Current.x) + Abs(Target.y - Current.y);
      Inc(ResultA, BaseDist);
      Inc(ResultB, BaseDist);

      for x in EmptyCols do
        if InRange(x, Min(Target.x, Current.x), Max(Target.x, Current.x)) then
        begin
          Inc(ResultA, 1);
          Inc(ResultB, 1000000 - 1);
        end;

      for y in EmptyRows do
        if InRange(y, Min(Target.y, Current.y), Max(Target.y, Current.y)) then
        begin
          Inc(ResultA, 1);
          Inc(ResultB, 1000000 - 1);
        end;
    end;
  end;

  EmptyRows.Free;
  EmptyCols.Free;
end;

function TAdventOfCodeDay11.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay12'}
type
  SpringState = (Unkown = 1, Operational = 2, Damaged = 3);
  TSprings = Array of SpringState;
  TRecords = Array of Integer;

function TAdventOfCodeDay12.AnalyzeHotSprings(UnfoldRecords: Boolean): int64;

  function InternalAnalyze(RecordIdx, springIdx, SpringCount, RecordCount: int64; Springs: TSprings; Records: TRecords; Cache: TDictionary<Int64, Int64>): Int64;
  var
    LocalRecordIndex, LocalSpringIndex, expectedLength, i, Key: int64;
  begin
    key := 0;
    if UnfoldRecords and InRange(springIdx, 0, SpringCount-1)  then
    begin
      Key := (springIdx shl 32) + (RecordIdx shl 2) + Ord(Springs[springIdx+1]) ;

      if Cache.TrygetValue(key, Result) then
        Exit;
    end;

    try
      Result := 1;

      LocalSpringIndex := springIdx;
      LocalRecordIndex := RecordIdx;

      while True do
      begin
        Inc(LocalSpringIndex);

        if LocalSpringIndex = SpringCount then // Out of springs, Valid if all records are used
          Exit(ifThen(LocalRecordIndex = RecordCount, 1, 0));

        if Springs[LocalSpringIndex] = Damaged then
        begin
          if LocalRecordIndex = RecordCount then
            Exit(0); // Found a damaged spring, but out of records => Invald

          expectedLength := Records[LocalRecordIndex];
          inc(LocalRecordIndex);

          for i := 0 to expectedLength-1 do
          begin
            if LocalSpringIndex = SpringCount then
              Exit(0); // We need more damaged springs, but where out of springs -> Invalid

            if Springs[LocalSpringIndex] = Operational then
              Exit(0); // We need a damaged spring, but this one is explicit operational

            Inc(LocalSpringIndex);
          end;

          if LocalSpringIndex = SpringCount then // Out of springs, Valid if all records are used
            Exit(ifThen(LocalRecordIndex = RecordCount, 1, 0));

          if Springs[LocalSpringIndex] = Damaged then // We need an operational spring after the damaged spring(s), but this one is operational => Invalid
            Exit(0);
        end
        else if Springs[LocalSpringIndex] = Unkown then
        begin
          Springs[LocalSpringIndex] := Operational;
          Result := InternalAnalyze(LocalRecordIndex, LocalSpringIndex-1, SpringCount, RecordCount, Springs, Records, Cache);

          Springs[LocalSpringIndex] := Damaged;
          Result := Result + InternalAnalyze(LocalRecordIndex, LocalSpringIndex-1, SpringCount, RecordCount, Springs, Records, Cache);

          Springs[LocalSpringIndex] := Unkown;

          Exit;
        end;
      end;

    finally
      if key <> 0 then
        Cache.AddOrSetValue(Key, Result);
    end;
  end;

var
  s: string;
  split: TStringDynArray;
  i, j, SpringCount, RecordCount, FoldedSpringCount, FoldedRecordCount, LocalIndex: integer;
  LineResult: Int64;
  Springs: TSprings;
  Records: TRecords;
  Cache: TDictionary<Int64,Int64>;
begin
  Result := 0;
  Cache := TDictionary<Int64, Int64>.Create;

  for s in FInput do
  begin
    split := SplitString(s, ', ');

    Cache.Clear;
    FoldedSpringCount := Length(split[0]);
    FoldedRecordCount := Length(split)-1;

    SpringCount := FoldedSpringCount;
    RecordCount := FoldedRecordCount;
    if UnfoldRecords then
    begin
      SpringCount := FoldedSpringCount*5 + 4;
      RecordCount := FoldedRecordCount*5
    end;

    SetLength(Springs, SpringCount);
    SetLength(Records, RecordCount);

    for i := 1 to FoldedSpringCount do
      Springs[i-1] := SpringState(Pos(split[0][i], '?.#')) ;

    for i := 1 to FoldedRecordCount do
      Records[i-1] := Split[i].ToInteger;

    if UnfoldRecords then
    begin
      LocalIndex := FoldedSpringCount;

      for i := 1 to 4 do
      begin
        Springs[LocalIndex] := Unkown;
        Inc(LocalIndex);
        for j := 0 to FoldedSpringCount -1 do
          Springs[LocalIndex + j] := Springs[j];
        inc(LocalIndex, FoldedSpringCount);
      end;

      LocalIndex := FoldedRecordCount;

      for i := 1 to 4 do
      begin
        for j := 0 to FoldedRecordCount-1 do
          Records[LocalIndex + j] := Records[j];
        Inc(LocalIndex, FoldedRecordCount);
      end;
    end;

    LineResult := InternalAnalyze(0, -1, SpringCount, RecordCount, Springs, Records, Cache);

    Inc(Result, LineResult);
  end;

  Cache.Free;
end;

function TAdventOfCodeDay12.SolveA: Variant;
begin
  Result := AnalyzeHotSprings(False);
end;

function TAdventOfCodeDay12.SolveB: Variant;
begin
  Result := AnalyzeHotSprings(True);
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
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
  TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
  TAdventOfCodeDay11,TAdventOfCodeDay12]);

end.
