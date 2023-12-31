unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, RTTI,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json,
  AocLetterReader, uAOCTimer,
  System.Threading, system.Hash;

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

  PipeSegment = (None =1, Start =2, Vertical =3, Horizontal =4, NE =5, NW =6, SW =7 , SE =8);
    
  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    PipeLoop: TDictionary<TPosition,PipeSegment>;
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

  TAdventOfCodeDay13 = class(TAdventOfCode)
  private
    function CountReflections(Const aMaxReflectionDiffs: integer): integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay14 = class(TAdventOfCode)
  private
    function CalculateLoad(PartB: Boolean): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay15 = class(TAdventOfCode)
  private
    function Hash(aValue: string): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TReflector = (refNone=1, RevPos, Revneg, RevHorz, RevVert);
  TAdventOfCodeDay16 = class(TAdventOfCode)
  private
    Map: array of array of TReflector;
    MaxX, MaxY: Integer;

    function FireBeam(aFrom: TPosition; InitialDirection: TAOCDirection): integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay17 = class(TAdventOfCode)
  private
    Map: array of array of integer;
    MaxX, MaxY: Integer;

    function MoveCrucible(UseUltaCrucible: boolean): Integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay18 = class(TAdventOfCode)
  private
    function FindLavaLagoon(const UseHexValue: Boolean): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TCategorieValues = array[0..3] of Int64;
  TRule = Record
    CategorieIdxToCheck: Integer;
    CheckBigger: Boolean;
    CheckValue: Int64;
    NextWorkFlow: String;
    class function CreateFromString(const aValue: string): TRule; static;
  End;

  TAdventOfCodeDay19 = class(TAdventOfCode)
  private
    Workflows: TDictionary<String,TList<TRule>>;

    function AnalyzeWorkFlow(WorkFlowName: string; MinValues, MaxValues: TCategorieValues): Int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TSignal = record
    RecieverId: Integer;
    SenderId: Integer;
    Value: Boolean;
    class function Create(aReciever, aSender: Integer; aValue: boolean): TSignal; static;
  end;

  TSignalBus = TQueue<TSignal>;
  TModule = class
  private
    FModuleName: string;
    FModuleId: Integer;
    class function CreateModuleId(const aModuleName: string): integer;
  protected
    procedure SendSignals(aValue: Boolean; aBus: TSignalBus);
  public
    ConnectedModules: TList<Integer>;
    DependsOn: TList<Integer>;

    constructor Create(aValue: string);
    destructor Destroy; override;
    
    procedure RecieveHello(aFrom: integer); virtual;
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); virtual; abstract;

    property ModuleName: string read FModuleName;
    property ModuleId: Integer read FModuleId;
  end;
  
  TAdventOfCodeDay20 = class(TAdventOfCode)
  private
    function CreateModules: TDictionary<integer,TModule>;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay21 = class(TAdventOfCode)
  private
    Map: TDictionary<Int64, Boolean>;
    MaxX, MaxY: Integer;
    Start: TPosition;

    function CountSteps(aX, aY, aStepsLeft: Integer): Int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  type TBrick = class
    FPosition1,
    FPosition2: TPosition3;
    FId: integer;
    FOnGroundLevel: Boolean;

    FIsSupporting,
    FIsSupportedBy: TDictionary<Integer, TBrick>;
  public
    Constructor Create(aPosition1, aPosition2: TPosition3; aId: Integer);
    destructor Destroy; override;

    function MinPos: TPosition3;
    function MaxPos: TPosition3;

    procedure FallDown;
    procedure AddSupportingBrick(aBrick: TBrick);
    procedure AddSupportedByBrick(aBrick: TBrick);
    function WouldFallDown(aFallenBrickIds: TDictionary<Integer,TBrick>): Boolean;

    property Id: Integer read FId;
    property IsSupporting: TDictionary<Integer, TBrick> read FIsSupporting;
    property IsSupportedBy: TDictionary<Integer, TBrick> read FIsSupportedBy;
    property OnGroundLevel: Boolean read FOnGroundLevel write FOnGroundLevel;
  end;

  TAdventOfCodeDay22 = class(TAdventOfCode)
  private
    Bricks: TDictionary<Integer,TBrick>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TPathTile = (SlopeNorth, SLopeEast, SlopeSouth, SlopeWest, Wall, Path);
  TAdventOfCodeDay23 = class(TAdventOfCode)
  private
    Map: TDictionary<TPosition,TPathTile>;
    StartPosition, StopPosition: TPosition;

    function FindPath(Const CanClimbSlopes: Boolean): Integer;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  THailStone = record
    Position, Velocity: TPosition3;
    class function Create(aPosition, aVelocity: TPosition3): THailStone; static;
  end;

  TAdventOfCodeDay24 = class(TAdventOfCode)
  private
    HailStones: TList<THailStone>;
    function GetIntersection(PositionX1, PositionY1, VelocityX1, VelocityY1, PositionX2, PositionY2, VelocityX2, VelocityY2: Int64; out Time1, Time2: Float64): Boolean;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay25 = class(TAdventOfCode)
  private
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

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
Const
  AllowedDirections: array[PipeSegment] of TAOCDirections = (
  {None       } [],
  {Start      } [North, South, East, West],
  {Vertical   } [North, South],
  {Horizontal } [East, West],
  {NE         } [North, East],
  {NW         } [North, West],
  {SW         } [South, West],
  {SE         } [South, East]
  );

procedure TAdventOfCodeDay10.BeforeSolve;
Var
  Map: TDictionary<TPosition, PipeSegment>;

  function FindLoop(aStart: TPosition; aStartSegment: PipeSegment): Boolean;
  var
    Next, NewNext, Prev: TPosition;
    CurrentSegment, NextSegment: PipeSegment;
    Direction: TAOCDirection;
    Found: Boolean;
  begin
    PipeLoop.Clear;

    CurrentSegment := aStartSegment;
    Next := aStart;
    Prev := Next.Clone;

    while True do
    begin
      Found := False;
      for Direction in AllowedDirections[CurrentSegment] do
      begin
        NewNext := Next.Clone.ApplyDirection(Direction);
        if NewNext.Equals(Prev) then
          Continue;

        NextSegment := Map[NewNext];
        if ([RotateDirection(Direction, 2)] * AllowedDirections[NextSegment]) = [] then
          Continue;

        Found := True;
        CurrentSegment := NextSegment;
        prev := Next.Clone;
        Next.ApplyDirection(Direction);
        PipeLoop.Add(Next.Clone, CurrentSegment);

        if NextSegment = PipeSegment.Start then
          Exit(True);

        break;
      end;

      if not found then
        Exit(False);
    end;
  end;

var
  x, y: Integer;
  Start: TPosition;
  Segment: PipeSegment;
begin
  PipeLoop := TDictionary<TPosition,PipeSegment>.Create;
  try
    Map := TDictionary<TPosition,PipeSegment>.Create;

    for y := 0 to FInput.Count-1 do
      for x := 1 to Length(FInput[0]) do
      begin
      Segment := PipeSegment(Pos(FInput[Y][X], '.S|-LJ7F'));
        if Segment = PipeSegment.Start then
          Start := TPosition.Create(X-1, Y);

        Map.Add(TPosition.Create(X-1, Y), Segment);
      end;

    for Segment in [Vertical, Horizontal, NE, NW, SW, SE] do
      if FindLoop(Start.Clone, Segment) then
        Exit;
  finally
    Map.Free
  end
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
      if PipeLoop.TryGetValue(TPosition.Create(x, y), Segment) then
        if Segment in [Vertical, SE, SW] then
          Inc(SegmentCrossings);

      if (SegmentCrossings and 1 = 1) and not PipeLoop.ContainsKey(TPosition.Create(x, y)) then
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

  function InternalAnalyze(aSpring: SpringState; RecordIdx, springIdx, SpringCount, MaxSpringCount, RecordCount, MaxRecordCount: int64; Springs: TSprings; Records: TRecords; Cache: TDictionary<Int64, Int64>): Int64;
  var
    LocalRecordIndex, LocalSpringIndex, expectedLength, i, Key: int64;
    LocalSpring: SpringState;
  begin
    key := 0;
    try
      Result := 1;

      LocalSpringIndex := springIdx;
      LocalRecordIndex := RecordIdx;
      LocalSpring := aSpring;

      while True do
      begin
        if UnfoldRecords then
        begin
          Key := (LocalSpringIndex shl 16) + (LocalRecordIndex shl 2) + Ord(LocalSpring) ;

          if Cache.TrygetValue(key, Result) then
          begin
            Key := 0;
            Exit;
          end;
        end;

        if LocalSpring = Damaged then
        begin
          if LocalRecordIndex = MaxRecordCount then
            Exit(0); // Found a damaged spring, but out of records => Invald

          expectedLength := Records[LocalRecordIndex mod RecordCount];
          inc(LocalRecordIndex);

          for i := 0 to expectedLength-1 do
          begin
            if LocalSpringIndex = MaxSpringCount then
              Exit(0); // We need more damaged springs, but where out of springs -> Invalid

            if Springs[LocalSpringIndex mod SpringCount] = Operational then
              Exit(0); // We need a damaged spring, but this one is explicit operational

            Inc(LocalSpringIndex);
          end;

          if LocalSpringIndex = MaxSpringCount then // Out of springs, Valid if all records are used
            Exit(ifThen(LocalRecordIndex = MaxRecordCount, 1, 0));

          if Springs[LocalSpringIndex mod SpringCount] = Damaged then // We need an operational spring after the damaged spring(s), but this one is Damaged => Invalid
            Exit(0);
        end
        else if LocalSpring = Unkown then
        begin
          Result := InternalAnalyze(Damaged, LocalRecordIndex, LocalSpringIndex, SpringCount, MaxSpringCount, RecordCount, MaxRecordCount, Springs, Records, Cache);
          Result := Result + InternalAnalyze(Operational, LocalRecordIndex, LocalSpringIndex, SpringCount, MaxSpringCount, RecordCount, MaxRecordCount, Springs, Records, Cache);
          Exit;
        end;

        Inc(LocalSpringIndex);

        if LocalSpringIndex = MaxSpringCount then // Out of springs, Valid if all records are used
          Exit(ifThen(LocalRecordIndex = MaxRecordCount, 1, 0));

        LocalSpring := Springs[LocalSpringIndex mod SpringCount];
      end;
    finally
      if key <> 0 then
        Cache.Add(Key, Result);
    end;
  end;

var
  s: string;
  split: TStringDynArray;
  i, MaxSpringCount, MaxRecordCount, FoldedSpringCount, FoldedRecordCount: integer;
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

    SetLength(Springs, FoldedSpringCount + ifthen(UnfoldRecords, 1, 0));
    SetLength(Records, FoldedRecordCount);

    for i := 1 to FoldedSpringCount do
      Springs[i-1] := SpringState(Pos(split[0][i], '?.#')) ;

    for i := 1 to FoldedRecordCount do
      Records[i-1] := Split[i].ToInteger;

    MaxSpringCount := FoldedSpringCount;
    MaxRecordCount := FoldedRecordCount;
    if UnfoldRecords then
    begin
      MaxSpringCount := FoldedSpringCount*5 + 4;
      MaxRecordCount := FoldedRecordCount*5;
      Springs[FoldedSpringCount] := Unkown;
      Inc(FoldedSpringCount);
    end;

    LineResult := InternalAnalyze(Springs[0], 0, 0, FoldedSpringCount, MaxSpringCount, FoldedRecordCount, MaxRecordCount, Springs, Records, Cache);

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
{$REGION 'TAdventOfCodeDay13'}
procedure TAdventOfCodeDay13.BeforeSolve;
begin
  FInput.Add(''); // To include the last image
end;

function TAdventOfCodeDay13.CountReflections(const aMaxReflectionDiffs: integer): integer;

  function IsValidReflectionLine(aLines: TStringList; aLineIndex: integer): Boolean;
  var
    Left, Right, DiffCount, j: Integer;
  begin
    Result := False;
    Left := aLineIndex-1;
    Right:= aLineIndex;
    DiffCount := 0;

    while (Left >= 0) and (Right < aLines.Count) do
    begin
      for j := 1 to Length(aLines[0]) do
        if aLines[Left][j] <> aLines[Right][j] then
          Inc(DiffCount);

      if DiffCount > aMaxReflectionDiffs then
        Exit;

      Inc(Right);
      Dec(Left);
    end;

    if DiffCount = aMaxReflectionDiffs then
      Result := True
  end;

  function FindReflectionLine(aLines: TStringList): Int64;
  var
    i: Integer;
  begin
    Result := 0;

    for i := 1 to aLines.Count-1 do
      if IsValidReflectionLine(aLines, i) then
        Exit(i);
  end;

var
  s, s2: String;
  i, j: integer;
  Horz, Vert: TStringList;

begin
  Horz := TStringList.Create;
  Vert := TStringList.Create;

  Result := 0;

  for s in FInput do
  begin
    if s <> '' then
      Horz.Add(s)
    else
    begin
      for j := 1 to Length(Horz[0]) do
      begin
        s2 := '';
        for i := 0 to Horz.Count-1 do
          s2 := s2 + Horz[i][j];
        Vert.Add(s2)
      end;

      Inc(Result, FindReflectionLine(Vert));
      Inc(Result, 100*FindReflectionLine(Horz));

      Horz.Clear;
      Vert.Clear;
    end;
  end;

  Vert.Free;
  Horz.Free;
end;

function TAdventOfCodeDay13.SolveA: Variant;
begin
  Result := CountReflections(0);
end;

function TAdventOfCodeDay13.SolveB: Variant;
begin
  Result := CountReflections(1);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay14'}
type TRockKind = (RockNone=1, RockRound=2, RockSquare=3);
function TAdventOfCodeDay14.CalculateLoad(PartB: Boolean): Int64;
var
  Platform: array of array of TRockKind;
  MaxX, MaxY: Integer;

  procedure TiltPlatform(Const Loop1StartX, Loop1StartY, Loop1DeltaX, Loop1DeltaY, Loop2DeltaX, Loop2DeltaY: integer);
  var
    Loop1X, Loop1Y, Loop2X, Loop2Y, NextX, NextY: integer;
  begin
    Loop1X := Loop1StartX;
    Loop1Y := Loop1StartY;

    while InRange(Loop1X, 0, MaxX) and InRange(Loop1Y, 0, MaxY) do
    begin
      Loop2X := Loop1X;
      Loop2Y := Loop1Y;
      NextX := Loop2X;
      NextY := Loop2Y;

      while InRange(Loop2X, 0, MaxX) and InRange(Loop2Y, 0, MaxY) do
      begin
        case Platform[Loop2X][Loop2Y] of
          RockSquare:
            begin
              NextX := Loop2x + Loop2DeltaX;
              NextY := Loop2Y + Loop2DeltaY;
            end;
          RockRound:
            begin
              Platform[Loop2X][Loop2Y] := RockNone;
              Platform[NextX][NextY] := RockRound;

              Inc(NextX, Loop2DeltaX);
              Inc(NextY, Loop2DeltaY);
            end;
        end;

        Inc(Loop2X, Loop2DeltaX);
        Inc(Loop2Y, Loop2DeltaY);
      end;

      Inc(Loop1X, Loop1DeltaX);
      Inc(Loop1Y, Loop1DeltaY);
    end;
  end;

const TiltingRounds: Array[Boolean] of int64 = (1, 1000000000);
var
  MapString: String;
  x, y: Integer;
  CurrentRound, StepSize, StepsToTake, CacheIdx, Weigth: Int64;
  FillChache: Boolean;
  Cache: TStringList;
  MapStringBuilder: TStringBuilder;
begin
  Cache := TStringList.Create;
  MapStringBuilder := TStringBuilder.Create;
  try
    FillChache := not PartB;
    MaxX := Length(FInput[0])-1;
    MaxY := FInput.Count-1;

    SetLength(Platform, MaxX);
    for x := 0 to MaxX do
      SetLength(Platform[X], MaxY);

    for y := 0 to FInput.Count-1 do
      for x := 1 to Length(FInput[0]) do
        Platform[x-1][y] := TRockKind(Pos(FInput[Y][X], '.O#'));

    CurrentRound := 1;
    while CurrentRound<= TiltingRounds[PartB] do
    begin
      TiltPlatform(0, 0, 1, 0, 0, 1);

      if PartB then
      begin
        TiltPlatform(0, 0, 0, 1, 1, 0);     // West
        TiltPlatform(0, MaxY, 1, 0, 0, -1); // South
        TiltPlatform(MaxX, 0, 0, 1, -1, 0); // West
      end;

      if CurrentRound = 75 then
        FillChache := True;

      if FillChache then
      begin
        Weigth := 0;
        MapString := '';
        MapStringBuilder.Clear;

        for x := 0 to MaxX do
          for y := 0 to MaxY do
            if Platform[x][y] = RockRound then
            begin
              Inc(Weigth, MaxY + 1 - y);
              MapStringBuilder.Append(x*maxX + y);
            end;

        MapString := MapStringBuilder.ToString;

        CacheIdx := Cache.IndexOf(MapString);
        if CacheIdx >= 0 then
        begin
          StepSize := Cache.Count - CacheIdx;
          StepsToTake := 1000000000 - CurrentRound;
          StepsToTake := StepsToTake mod StepSize;
          CacheIdx := CacheIdx + StepsToTake;
          Result := Int64(Cache.Objects[CacheIdx]);
          Exit;
        end;

        if FillChache then
          Cache.AddObject(MapString, TObject(Weigth));
      end;

      Inc(CurrentRound);
    end;

    Result := Int64(Cache.Objects[0]);
  finally
    Cache.Free;
    MapStringBuilder.Free;
  end;
end;

function TAdventOfCodeDay14.SolveA: Variant;
begin
  Result := CalculateLoad(False) ;
end;

function TAdventOfCodeDay14.SolveB: Variant;
begin
  Result := CalculateLoad(True) ;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay15'}
function TAdventOfCodeDay15.Hash(aValue: string): Integer;
var i: Integer;
begin
  Result := 0;

  for i := 1 to Length(aValue) do
  begin
    Result := Result + Ord(aValue[i]);
    Result := Result * 17;
    Result := Result mod 256
  end;
end;

function TAdventOfCodeDay15.SolveA: Variant;
var
  s: String;
begin
  Result := 0;
  for s in SplitString(FInput[0], ',') do
    Inc(Result, Hash(s));
end;

function TAdventOfCodeDay15.SolveB: Variant;
var
  Boxes: TDictionary<Integer,TStringList>;
  s, LensLabel: String;
  i, FocalLength, BoxId, LensIndex: Integer;
  split, Split2: TStringDynArray;
  BoxContent: TStringList;
begin
  Boxes := TObjectDictionary<Integer,TStringList>.Create([doOwnsValues]);

  Split := SplitString(FInput[0], ',');
  Result := 0;

  for i := 0 to Length(split)-1 do
  begin
    s := split[i];

    split2 := SplitString(s, '-=');
    LensLabel := Split2[0];

    BoxId := Hash(LensLabel);
    if not Boxes.TryGetValue(BoxId, BoxContent) then
    begin
      BoxContent := TStringList.Create;
      Boxes.Add(BoxId, BoxContent);
    end;

    LensIndex := BoxContent.IndexOf(LensLabel);
    if s.Contains('-') then
    begin
      if LensIndex >= 0 then
        BoxContent.Delete(LensIndex);
    end
    else
    begin
      FocalLength := Split2[1].ToInteger;

      if LensIndex >= 0 then
        BoxContent.Objects[LensIndex] := TObject(FocalLength)
      else
        BoxContent.AddObject(LensLabel, TObject(FocalLength));
    end;
  end;

  for BoxId := 0 to 255 do
    if Boxes.TryGetValue(BoxId, BoxContent) then
      for LensIndex := 0 to BoxContent.Count -1 do
        Inc(Result, (BoxId+1) * (LensIndex+1) * Integer(BoxContent.Objects[LensIndex]));
    
  Boxes.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay16'}
procedure TAdventOfCodeDay16.BeforeSolve;
var
  x,y: integer;
begin
  MaxX := Length(FInput[0])-1;
  MaxY := FInput.Count -1;

  SetLength(Map, MaxX+1);
  for x := 0 to MaxX do
    SetLength(Map[x], MaxY+1);

  for y := 0 to MaxY do
    for x := 0 to MaxX do
      Map[x][y] := TReflector(Pos(FInput[Y][X+1], './\-|'));
end;

const
  ReflectorDate: array[TReflector] of array[TAOCDirection] of TAOCDirections = (
  {refNone} ([North],[East],[South],[West]),
  {RefPos}  ([East],[North],[West],[South]),
  {Refneg}  ([West],[South],[East],[North]),
  {RevHorz} ([East, West],[East],[East, West],[West]),
  {RevVert} ([North],[North, South],[South],[North, South]));

function TAdventOfCodeDay16.FireBeam(aFrom: TPosition; InitialDirection: TAOCDirection): integer;
var
  Energyzed: array of array of TAOCDirections;

  procedure InternalFireBeam(aCurrent: TPosition; aDirection: TAOCDirection);
  var
    Reflector: TReflector;
    newDirection: TAOCDirection;
  begin
    if not InRange(aCurrent.x, 0, maxX) or not InRange(aCurrent.y, 0, maxY) then
      Exit;

    Reflector := Map[aCurrent.x, aCurrent.y];

    if Energyzed[aCurrent.x, aCurrent.y] = [] then
      Inc(Result);
    if aDirection in Energyzed[aCurrent.x, aCurrent.y] then
      Exit;
    Energyzed[aCurrent.x, aCurrent.y] := Energyzed[aCurrent.x, aCurrent.y] + [aDirection];

    for newDirection in ReflectorDate[Reflector][aDirection] do
      InternalFireBeam(aCurrent.ApplyDirection(newDirection), newDirection)
  end;

var
  x: integer;
begin
  Result := 0;

  SetLength(Energyzed, MaxX+1);
  for x := 0 to MaxX do
    SetLength(Energyzed[x], MaxY+1);

  InternalFireBeam(aFrom, InitialDirection);
end;

function TAdventOfCodeDay16.SolveA: Variant;
begin
  Result := FireBeam(TPosition.Create(0,0), East);
end;

function TAdventOfCodeDay16.SolveB: Variant;
var
  x,y: integer;
  res: Integer;
begin
  Result := 0;

  for y := 0 to FInput.Count-1 do
  begin
    res := FireBeam(TPosition.Create(0,y), TAOCDirection.East);
    Result := Max(Result, res);

    res := FireBeam(TPosition.Create(MaxX,y), TAOCDirection.West);
    Result := Max(Result, res);
  end;

  for x := 0 to Length(FInput[0])-1 do
  begin
    res := FireBeam(TPosition.Create(x,0), TAOCDirection.South);
    Result := Max(Result, res);

    res := FireBeam(TPosition.Create(x, MaxY), TAOCDirection.North);
    Result := Max(Result, res);
  end
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay17'}
type
  TCrucibleNode = record
    Position: TPosition;
    CurrentDir: TAOCDirection;
    HeatLoss: Integer;
    Class function Create(aPosition: TPosition; aCurrentDir: TAOCDirection; aHeatLoss: Integer): TCrucibleNode; static;
  end;

class function TCrucibleNode.Create(aPosition: TPosition; aCurrentDir: TAOCDirection; aHeatLoss: Integer): TCrucibleNode;
begin
  Result.Position := aPosition;
  Result.CurrentDir := aCurrentDir;
  Result.HeatLoss := aHeatLoss;
end;

procedure TAdventOfCodeDay17.BeforeSolve;
var
  x,y: Integer;
begin
  MaxX := Length(FInput[0]);
  MaxY := FInput.Count;

  SetLength(Map, MaxX);
  for x := 0 to MaxX do
    SetLength(Map[x], MaxY);

  for y := 0 to MaxY-1 do
    for x := 0 to MaxX-1 do
      Map[x][y] := StrToInt(FInput[Y][X+1]);
end;

function TAdventOfCodeDay17.MoveCrucible(UseUltaCrucible: boolean): Integer;
var
  HeatLoss, i, key, GridSize: integer;
  Work: PriorityQueue<Integer, TCrucibleNode>;
  CurrentWork, NewWork: TCrucibleNode;
  NextDir: TAOCDirection;
  Init: Boolean;
  NextPosition: TPosition;
  Seen: array of integer;
begin
  GridSize := MaxX * MaxY;

  Work := PriorityQueue<Integer, TCrucibleNode>.Create();

  SetLength(Seen, GridSize * 2);

  CurrentWork := TCrucibleNode.Create(TPosition.Create(0,0), East, 0);
  Work.Enqueue(0, CurrentWork);

  Result := 0;
  Init := True;

  while Work.Count > 0 do
  begin
    CurrentWork := Work.Dequeue;

    if (CurrentWork.Position.y = MaxY-1) and (CurrentWork.Position.x = MaxX-1) then
      Exit(CurrentWork.HeatLoss);

    key := (CurrentWork.Position.x * MaxX + CurrentWork.Position.y) + (Ord(CurrentWork.CurrentDir) and 1) * GridSize;
    if Seen[key] < CurrentWork.HeatLoss then
      Continue;

    for NextDir in [North, East, South, West] do
    begin
      if (not Init) and ((Ord(NextDir) and 1) = (Ord(CurrentWork.CurrentDir) and 1)) then
        Continue; // Dont move back or in the same direction

      HeatLoss := 0;
      for i := 1 to ifthen(UseUltaCrucible, 10, 3) do
      begin
        NextPosition := CurrentWork.Position.Clone.ApplyDirection(NextDir, i);

        if not InRange(NextPosition.x, 0, MaxX-1) then
          Break;

        if not InRange(NextPosition.y, 0, MaxY-1) then
          Break;

        HeatLoss := HeatLoss + Map[NextPosition.x][NextPosition.y];

        if UseUltaCrucible and (i < 4) then
          Continue;

        NewWork := TCrucibleNode.Create(NextPosition, NextDir, CurrentWork.HeatLoss + HeatLoss);

        key := (NewWork.Position.x * MaxX + NewWork.Position.y) + (Ord(NewWork.CurrentDir) and 1) * GridSize;

        if (Seen[Key] <> 0) and (Seen[Key] <= NewWork.HeatLoss) then
          continue;

        Seen[Key] := NewWork.HeatLoss;
        Work.Enqueue(NewWork.HeatLoss, NewWork);
      end;
    end;

    Init := False;
  end;
end;

function TAdventOfCodeDay17.SolveA: Variant;
begin
  Result := MoveCrucible(False);
end;

function TAdventOfCodeDay17.SolveB: Variant;
begin
  Result := MoveCrucible(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay18'}
function TAdventOfCodeDay18.FindLavaLagoon(const UseHexValue: Boolean): Int64;
var
  s: string;
  CurrentX, CurrentY, PrevX, PrevY, Distance, Area, Border: Int64;
  PosAt, Rotation: Integer;
begin
//https://arachnoid.com/area_irregular_polygon/index.html
//https://en.wikipedia.org/wiki/Pick%27s_theorem

  CurrentX := 0;
  CurrentY := 0;
  PrevX := 0;
  PrevY := 0;
  Area := 0;
  Border := 0;

  for s in FInput do
  begin
    PosAt := Pos('#', s);

    if UseHexValue then
    begin
      Distance := StrToInt64('$'+Copy(s, PosAt +1, Length(s) -PosAt -2));
      Rotation := StrToInt('$'+Copy(s, PosAt +6, 1));
    end
    else
    begin
      Distance := StrToInt(Copy(s, 3, PosAt -5));
      Rotation := Pos(s[1], 'RDLU') - 1;
    end;

    case Rotation of
      0: Inc(CurrentX, Distance);
      1: Inc(CurrentY, Distance);
      2: Dec(CurrentX, Distance);
      3: Dec(CurrentY, Distance);
    end;

    Inc(Area, CurrentX * PrevY);
    Dec(Area, CurrentY * PrevX);

    PrevX := CurrentX;
    PrevY := CurrentY;

    Inc(Border, Distance);
  end;

  Result := (abs(Area) shr 1) + (Border shr 1) + 1;
end;

function TAdventOfCodeDay18.SolveA: Variant;
begin
  Result := FindLavaLagoon(False);
end;

function TAdventOfCodeDay18.SolveB: Variant;
begin
  Result := FindLavaLagoon(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay19'}

class function TRule.CreateFromString(const aValue: string): TRule;
var
  Split: TStringDynArray;
begin
  Split := SplitString(aValue, '<>:');

  if Length(Split) = 1 then
  begin
    Result.CategorieIdxToCheck := -1;
    Result.CheckBigger := True;
    Result.CheckValue := -1;
    Result.NextWorkFlow := aValue;
  end
  else
  begin
    Result.CategorieIdxToCheck := Pos(aValue[1], 'xmas')-1;
    Result.CheckBigger := aValue[2] = '>';
    Result.CheckValue := Split[1].ToInt64;
    Result.NextWorkFlow := Split[2];
  end;
end;

procedure TAdventOfCodeDay19.BeforeSolve;
var
  s: String;
  split: TStringDynArray;
  i, j: Integer;
  Rules: TList<TRule>;
begin
  Workflows := TObjectDictionary<String,TList<TRule>>.Create([doOwnsValues]);

  i := 0;
  while i <= FInput.Count-1 do
  begin
    s := FInput[i];
    if s = '' then
      Break;

    split := SplitString(s, '{},');

    Rules := TList<TRule>.Create;
    Workflows.Add(Split[0], Rules);

    for j := 1 to Length(Split)-1 do
      if Split[j] <> '' then
        Rules.Add(TRule.CreateFromString(Split[j]));

    Inc(i);
  end;
end;

procedure TAdventOfCodeDay19.AfterSolve;
begin
  Workflows.Free;
end;

function TAdventOfCodeDay19.AnalyzeWorkFlow(WorkFlowName: string; MinValues, MaxValues: TCategorieValues): Int64;

  function CopyValues(aSoure: TCategorieValues): TCategorieValues;
  var
    i: Integer;
  begin
    for i := 0 to 3 do
      Result[i] := aSoure[i];
  end;

var
  Rules: TList<TRule>;
  Rule: TRule;
  NewValues: TCategorieValues;
  i: Integer;
begin
  Result := 0;

  for i := 0 to 3 do
    if MaxValues[i] < MinValues[i] then
      Exit; // Invalid state;

  if WorkFlowName = 'R' then
    Exit; // Rejected

  if WorkFlowName = 'A' then
  begin
    Result := 1;
    for i := 0 to 3 do
      Result := Result * (MaxValues[i] - MinValues[i] + 1);
    exit; // Accepted, Return count
  end;

  Rules := Workflows[WorkFlowName];
  for i := 0 to Rules.Count -1 do
  begin
    Rule := Rules[i];

    if Rule.CategorieIdxToCheck < 0 then
    begin
      Result := Result + AnalyzeWorkFlow(Rule.NextWorkFlow, MinValues, MaxValues);
      Exit; // Last rule
    end;

    if Rule.CheckBigger then
    begin
      NewValues := CopyValues(MinValues);
      NewValues[Rule.CategorieIdxToCheck] := Max(MinValues[Rule.CategorieIdxToCheck], Rule.CheckValue + 1);
      Result := Result + AnalyzeWorkFlow(Rule.NextWorkFlow, NewValues, MaxValues);
      MaxValues[Rule.CategorieIdxToCheck] := Min(MaxValues[Rule.CategorieIdxToCheck], Rule.CheckValue);
    end
    else
    begin
      NewValues := CopyValues(MaxValues);
      NewValues[Rule.CategorieIdxToCheck] := Min(MaxValues[Rule.CategorieIdxToCheck], Rule.CheckValue - 1);
      Result := Result + AnalyzeWorkFlow(Rule.NextWorkFlow, MinValues, NewValues);
      MinValues[Rule.CategorieIdxToCheck] := Max(MinValues[Rule.CategorieIdxToCheck], Rule.CheckValue);
    end
  end;
end;

function TAdventOfCodeDay19.SolveA: Variant;
var
  split: TStringDynArray;
  i, j, Idx: Integer;
  Val, Res: Int64;
  MinValues, MaxValues: TCategorieValues;
begin
  for i := WorkFlows.Count + 1 to FInput.Count-1 do
  begin
    split := SplitString(FInput[i], '{=,}');
    j := 1;
    Res := 0;
    while j < 9 do
    begin
      Idx := Pos(split[j], 'xmas')-1;
      Val := Split[j+1].ToInt64;
      MinValues[Idx] := Val;
      MaxValues[Idx] := Val;
      Inc(Res, Val);
      Inc(j, 2);
    end;

    if AnalyzeWorkFlow('in', MinValues, MaxValues) = 1 then
      Inc(Result, Res);
  end;
end;

function TAdventOfCodeDay19.SolveB: Variant;
var
  i: integer;
  MinValues, MaxValues: TCategorieValues;
begin
  for i := 0 to 3 do
  begin
    MinValues[i] := 1;
    MaxValues[i] := 4000;
  end;

  Result := AnalyzeWorkFlow('in', MinValues, MaxValues);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay20'}
type
  TBroadCaster = class(TModule)
  public
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); override;
  end;

  TButton = class(TModule)
  private
    FButtonPushCount: integer;
  public
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); override;
    procedure PushButton(aBus: TSignalBus);

    constructor Create(aValue: string);
    
    property ButtonPushCount: integer read FButtonPushCount;  
  end;

  TReciever = class(TModule)
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); override;
  end;

  TFlipFlop = class(TModule)
  private
    currentState: Boolean;
  public
    constructor Create(aValue: string);
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); override;
  end;

  TConjunction = class(TModule)
  private
    RecievedPulses: TDictionary<integer, boolean>;
  public
    constructor Create(aValue: string);
    destructor Destroy; override;
    
    procedure RecieveHello(aFrom: integer); override;
    procedure RecieveSignal(aSignal: TSignal; aBus: TSignalBus); override;
  end;
  
{ TSignal }
  
class function TSignal.Create(aReciever, aSender: Integer; aValue: boolean): TSignal;
begin
  Result.RecieverId := aReciever;
  Result.SenderId := aSender;
  Result.Value := aValue;
end;

{ TModule }

constructor TModule.Create(aValue: string);
var
  split: TStringDynArray;
  s: string;
begin
  Split := SplitString(aValue, '->');
  Self.FModuleName := Trim(split[0]);
  Self.FModuleId := CreateModuleId(ModuleName);

  ConnectedModules := TList<integer>.Create;
  DependsOn := TList<integer>.Create;
  if Length(split) > 2 then
  begin
    Split := SplitString(Split[2], ',');
    for s in split do
      if s <> '' then
        ConnectedModules.Add(CreateModuleId(Trim(s)));
  end;
end;

class function TModule.CreateModuleId(const aModuleName: string): integer;
begin
  Result := THashBobJenkins.GetHashValue(aModuleName[Low(string)], Length(aModuleName) * SizeOf(Char), 0)
end;

destructor TModule.Destroy;
begin
  ConnectedModules.Free;
  DependsOn.Free;
  inherited;
end;

procedure TModule.RecieveHello(aFrom: integer);
begin
  DependsOn.Add(aFrom);
end;

procedure TModule.SendSignals(aValue: Boolean; aBus: TQueue<TSignal>);
var 
  i: Integer;
begin
  for i in ConnectedModules do
    aBus.Enqueue(TSignal.Create(i, ModuleId, aValue));
end;

{ TFlipFlop }

constructor TFlipFlop.Create(aValue: string);
begin
  currentState := False;
  inherited;
end;

procedure TFlipFlop.RecieveSignal(aSignal: TSignal; aBus: TQueue<TSignal>);
begin
  if aSignal.Value = false then
  begin
    currentState := not currentState;
    SendSignals(CurrentState, aBus);
  end;
end;

{ TConjunction }

constructor TConjunction.Create(aValue: string);
begin
  RecievedPulses := TDictionary<integer,Boolean>.Create();
  inherited
end;

destructor TConjunction.Destroy;
begin
  RecievedPulses.Free;
  inherited;
end;

procedure TConjunction.RecieveHello(aFrom: integer);
begin
  inherited;
  RecievedPulses.Add(aFrom, False);
end;

procedure TConjunction.RecieveSignal(aSignal: TSignal; aBus: TQueue<TSignal>);
var 
  AllHigh: Boolean;
begin
  RecievedPulses[aSignal.SenderId] := aSignal.Value;

  AllHigh := (aSignal.Value);
  if AllHigh then
    AllHigh := not RecievedPulses.ContainsValue(False);

  SendSignals(not AllHigh, aBus)
end;

{ TBroadCaster }

procedure TBroadCaster.RecieveSignal(aSignal: TSignal; aBus: TQueue<TSignal>);
begin
  SendSignals(aSignal.Value, aBus);
end;

{ TButton }

constructor TButton.Create(aValue: string);
begin
  inherited; 
  FButtonPushCount := 0; 
end;

procedure TButton.PushButton(aBus: TSignalBus);
begin
  Inc(FButtonPushCount);
  SendSignals(False, aBus);
end;

procedure TButton.RecieveSignal(aSignal: TSignal; aBus: TQueue<TSignal>);
begin
  // Nothing
end;

{ TReciever }

procedure TReciever.RecieveSignal(aSignal: TSignal; aBus: TSignalBus);
begin
  // Nothing
end;

function TAdventOfCodeDay20.CreateModules: TDictionary<integer,TModule>;
var
  i: Integer;
  s: string;
  Module: TModule;
  Button: TButton;
  Reciever: TReciever;
  Modules: TDictionary<integer, TModule>;
begin
  Modules := TObjectDictionary<Integer, TModule>.Create([doOwnsValues]);

  for s in FInput do
  begin
    Module := nil;
    if s[1] = 'b' then
      Module := TBroadCaster.Create(s)
    else if s[1] = '%' then
      Module := TFlipFlop.Create(RightStr(s, Length(s)-1))
    else if s[1] = '&' then
      Module := TConjunction.Create(RightStr(s, Length(s)-1));
      
    Modules.Add(Module.ModuleId, Module);
  end;

  for Module in Modules.Values do
    for i in Module.ConnectedModules do
    begin  
      if Modules.ContainsKey(i) then
        Modules[i].RecieveHello(Module.ModuleId)
      else
      begin
        Reciever := TReciever.Create('Dummy');
        Reciever.FModuleId := i; // Todo fix hack
        Modules.Add(Reciever.ModuleId, Reciever);
        Reciever.RecieveHello(Module.ModuleId);
      end;
    end;
    
  Button := TButton.Create('button -> broadcaster');
  Modules.Add(Button.ModuleId, Button);
  Result := Modules;
end;

function TAdventOfCodeDay20.SolveA: Variant;
var
  Modules: TDictionary<Integer,TModule>;
  Module: TModule;
  Bus: TSignalBus;
  Signal: TSignal;
  PulsCountLow, PulsCountHigh: Integer;
  Button: TButton;
begin
  Bus := TSignalBus.Create;
  Modules := CreateModules;
  Button := Modules[Module.CreateModuleId('button')] as TButton;

  PulsCountLow := 0;
  PulsCountHigh := 0;
        
  Button.PushButton(Bus);
  while Bus.Count > 0 do
  begin
    Signal := Bus.Dequeue;

    if Signal.Value then
      Inc(PulsCountHigh)
    else
      Inc(PulsCountLow);

    if Modules.TryGetValue(signal.RecieverId, Module) then
      Module.RecieveSignal(Signal, bus);

    if (Bus.Count = 0) and (Button.ButtonPushCount < 1000) then
      Button.PushButton(Bus);
  end;

  Result := PulsCountHigh * PulsCountLow;

  Bus.Free;
  Modules.Free;
end;

function TAdventOfCodeDay20.SolveB: Variant;
var
  i: Integer;
  Reciever, FinalModule: TModule;
  Modules: TDictionary<integer,TModule>;
  Bus: TQueue<TSignal>;
  Signal: TSignal;
  Button: TButton;
  SignalsToCheck: TAOCDictionary<integer,integer>;
begin
  Modules := CreateModules;
  Bus := TSignalBus.Create;
  Button := Modules[TModule.CreateModuleId('button')] as TButton;
  Reciever := Modules[TModule.CreateModuleId('rx')];
  FinalModule := Modules[Reciever.DependsOn.First];
  SignalsToCheck := TAOCDictionary<integer,integer>.Create;

  Button.PushButton(Bus);
  while true do
  begin
    Signal := Bus.Dequeue;

    if (Signal.Value) and (Signal.RecieverId = FinalModule.ModuleId) then
    begin
      SignalsToCheck.AddOrIgnoreValue(Signal.SenderId, Button.ButtonPushCount);
      if SignalsToCheck.Count = FinalModule.DependsOn.Count then
        Break;
    end;

    Modules[Signal.RecieverId].RecieveSignal(signal, bus);

    if Bus.count = 0 then
      Button.PushButton(bus);
  end;

  Result := 1;
  for i  in SignalsToCheck.Values do
    Result := LCM(Result, i);

  Bus.Free;
  Modules.Free;
  SignalsToCheck.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay21'}
type GardenPosition = record
  Position: TPosition;
  StepsLeft: Int64;
  class function Create(aPosition: TPosition; StepsLeft: Int64): GardenPosition; Static;
end;

class function GardenPosition.Create(aPosition: TPosition; StepsLeft: Int64): GardenPosition;
begin
  Result.Position := aPosition;
  Result.StepsLeft := StepsLeft;
end;

procedure TAdventOfCodeDay21.BeforeSolve;
var
  x,y: Integer;
  Position: TPosition;
begin
  MaxX := Length(FInput[0])-1;
  MaxY := FInput.Count -1;

  Map := TDictionary<Int64,Boolean>.Create;

  for y := 0 to MaxY do
    for x := 0 to MaxX do
    begin
      Position := TPosition.Create(x, y);
      Map.Add(Position.CacheKey, FInput[Y][X+1] = '#');
      if FInput[Y][X+1] = 'S' then
        Start := Position.Clone;
    end;
end;

procedure TAdventOfCodeDay21.AfterSolve;
begin
  Map.Free;
end;

function TAdventOfCodeDay21.CountSteps(aX, aY, aStepsLeft: Integer): Int64;
var
  Work: TQueue<GardenPosition>;
  Seen: TDictionary<Int64,Boolean>;
  Current: GardenPosition;
  NextDirection: TAOCDirection;
begin
  Result := 0;
  Work := TQueue<GardenPosition>.Create;
  Work.Enqueue(GardenPosition.Create(TPosition.Create(aX, aY), aStepsLeft));

  Seen := TDictionary<Int64,Boolean>.Create;

  while Work.Count > 0 do
  begin
    Current := Work.Dequeue;

    if Current.StepsLeft < 0 then
      Continue;

    if (not InRange(Current.Position.x, 0, MaxX)) or (not InRange(Current.Position.Y, 0, MaxY)) then
      Continue; // Out of bounds

    if Map[Current.Position.CacheKey] then
      Continue; // aRock;

    if Seen.ContainsKey(Current.Position.CacheKey) then
      Continue; // Already processed

    Seen.Add(Current.Position.CacheKey, True);

    if not Odd(Current.StepsLeft) then
      Inc(Result);

    for NextDirection in [North, East, South, West] do
      Work.Enqueue(GardenPosition.Create(Current.Position.Clone.ApplyDirection(NextDirection), Current.StepsLeft - 1));
  end;

  Work.Free;
  Seen.Free;
end;

function TAdventOfCodeDay21.SolveA: Variant;
begin
  Result := CountSteps(Start.X, Start.Y, 64);
end;

function TAdventOfCodeDay21.SolveB: Variant;
Const
  TotalSteps: Int64 = 26501365;
var
  GardenBlocks, GardenLength, OddGardenSteps: Int64;
begin
  GardenLength := MaxX + 1;
  GardenBlocks := Trunc((TotalSteps - Start.X) / GardenLength);
  OddGardenSteps := GardenLength + Start.X -1; // -1 because it took one step to get in this block

  Result :=
  // Complete odd blocks
    (GardenBlocks-1) * (GardenBlocks-1) * CountSteps(Start.X, Start.Y, 999) +
  // Complete even blocks
    GardenBlocks * GardenBlocks * CountSteps(Start.X, Start.Y, 1000) +

  // Cuttoff Odd blocks allong the edge
    (GardenBlocks-1) * CountSteps(0, 0, OddGardenSteps) +
    (GardenBlocks-1) * CountSteps(MaxX, 0, OddGardenSteps) +
    (GardenBlocks-1) * CountSteps(0, MaxY, OddGardenSteps) +
    (GardenBlocks-1) * CountSteps(MaxX, MaxY, OddGardenSteps) +

  // Cuttoff evens block allong the edge (only 64 steps, because it took one edge to get here)
    GardenBlocks * CountSteps(0, 0, Start.X-1) +
    GardenBlocks * CountSteps(MaxX ,0, Start.X-1) +
    GardenBlocks * CountSteps(0, MaxY, Start.X-1) +
    GardenBlocks * CountSteps(MaxX, MaxY, Start.X-1) +

  // Four end-blocks extending from the center
    CountSteps(start.x, MaxY, GardenLength-1) +
    CountSteps(start.x, 0, GardenLength-1) +
    CountSteps(MaxX, start.Y, GardenLength-1) +
    CountSteps(0, start.y, GardenLength-1);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay22'}
procedure TBrick.AddSupportedByBrick(aBrick: TBrick);
begin
  if not FIsSupportedBy.ContainsKey(aBrick.Id) then
    FIsSupportedBy.AddOrSetValue(aBrick.Id, aBrick);
end;

procedure TBrick.AddSupportingBrick(aBrick: TBrick);
begin
  if not FIsSupporting.ContainsKey(aBrick.Id) then
    FIsSupporting.AddOrSetValue(aBrick.Id, aBrick);
end;

constructor TBrick.Create(aPosition1, aPosition2: TPosition3; aId: Integer);
begin
  FPosition1 := aPosition1;
  FPosition2 := aPosition2;
  FId := aId;

  FIsSupporting := TDictionary<Integer, TBrick>.Create;
  FIsSupportedBy  := TDictionary<Integer, TBrick>.Create;
end;

destructor TBrick.Destroy;
begin
  FIsSupporting.Free;
  FIsSupportedBy.Free;
end;

procedure TBrick.FallDown;
begin
  FPosition1.Z := FPosition1.Z -1;
  FPosition2.Z := FPosition2.Z -1;
end;

function TBrick.MaxPos: TPosition3;
begin
  Result := TPosition3.Max(FPosition1, FPosition2)
end;

function TBrick.MinPos: TPosition3;
begin
  Result := TPosition3.Min(FPosition1, FPosition2)
end;

function TBrick.WouldFallDown(aFallenBrickIds: TDictionary<Integer, TBrick>): Boolean;
var
  SupportingBrickId: Integer;
begin
  if MinPos.Z = 0 then
    Exit(False); // On ground level

  Result := True;
  for SupportingBrickId in IsSupportedBy.Keys do
    if not aFallenBrickIds.ContainsKey(SupportingBrickId) then
      Exit(False);
end;

procedure TAdventOfCodeDay22.BeforeSolve;
var
  settled: TDictionary<TPosition3,TBrick>;
  s: String;
  split: TStringDynArray;
  i, x, y: Integer;
  Work: PriorityQueue<Integer, TBrick>;
  Brick, SettledBrick: TBrick;
  CanFall: Boolean;
  minPos, maxPos: TPosition3;
begin
  Bricks := TObjectDictionary<Integer,TBrick>.Create([doOwnsValues]);
  Work := PriorityQueue<Integer, TBrick>.Create;;
  settled := TDictionary<TPosition3,TBrick>.Create;

  i := 0;
  for s in FInput do
  begin
    Inc(i);
    split := SplitString(s, ',~');
    Brick := TBrick.Create(
      TPosition3.Create(split[0].ToInt64,split[1].ToInt64,split[2].ToInt64),
      TPosition3.Create(split[3].ToInt64,split[4].ToInt64,split[5].ToInt64),
      i);

    Work.Enqueue(Brick.MinPos.Z, Brick);
    Bricks.Add(i, Brick);
  end;

  while Work.Count > 0 do
  begin
    Brick := Work.Dequeue;
    minPos := Brick.MinPos;
    maxPos := Brick.MaxPos;

    CanFall := minPos.z > 0;
    while CanFall do
    begin
      CanFall := minPos.z > 0;
      if CanFall then
        for x := minPos.x to maxPos.x do
          for y := minPos.y to maxPos.y do
            if settled.ContainsKey(TPosition3.Create(X, Y, minPos.Z-1)) then
            begin
              CanFall := False;
              break
            end;

      if CanFall then
      begin
        Brick.FallDown;
        minPos := Brick.MinPos;
        maxPos := Brick.MaxPos;
      end
    end;

    for x := minPos.x to maxPos.x do
      for y := minPos.y to maxPos.y do
      begin
        if settled.TryGetValue(TPosition3.Create(x,y,minPos.Z-1), SettledBrick) then
        begin
          SettledBrick.AddSupportingBrick(Brick);
          Brick.AddSupportedByBrick(SettledBrick);
        end;

        settled.Add(TPosition3.Create(x,y,maxPos.Z), Brick);
      end;
  end;

  settled.Free;
end;

procedure TAdventOfCodeDay22.AfterSolve;
begin
  inherited;
  Bricks.Free;
end;

function TAdventOfCodeDay22.SolveA: Variant;
var
  Brick, SupportingBrick: TBrick;
  CanRemove: Boolean;
  FallenBricks: TDictionary<Integer,TBrick>;
begin
  FallenBricks := TDictionary<Integer,TBrick>.Create(1);
  for Brick in Bricks.Values do
  begin
    FallenBricks.Clear;
    FallenBricks.Add(Brick.Id, Brick);

    CanRemove := True;
    for SupportingBrick in Brick.IsSupporting.Values do
      if SupportingBrick.WouldFallDown(FallenBricks) then
      begin
        CanRemove := False;
        Break;
      end;

    if CanRemove then
      inc(Result);
  end;
end;

function TAdventOfCodeDay22.SolveB: Variant;
var
  InitialBrick, CurrentBrick, SupportingBrick: TBrick;
  BricksToCalc: TQueue<TBrick>;
  FallenBricks: TDictionary<Integer, TBrick>;
begin
  Result := 0;

  BricksToCalc := TQueue<TBrick>.Create;
  FallenBricks := TDictionary<Integer, TBrick>.Create;

  for InitialBrick in Bricks.Values do
  begin
    FallenBricks.Clear;
    FallenBricks.Add(InitialBrick.Id, InitialBrick);

    for SupportingBrick in InitialBrick.IsSupporting.Values do
      BricksToCalc.Enqueue(SupportingBrick);

    while BricksToCalc.Count > 0 do
    begin
      CurrentBrick := BricksToCalc.Dequeue;

      if FallenBricks.ContainsKey(CurrentBrick.Id) then
        Continue;

      if not CurrentBrick.WouldFallDown(FallenBricks) then
        Continue;

      FallenBricks.Add(CurrentBrick.Id, CurrentBrick);

      for SupportingBrick in CurrentBrick.IsSupporting.Values do
        BricksToCalc.Enqueue(SupportingBrick);
    end;

    Inc(Result, FallenBricks.Count - 1);
  end;

  FallenBricks.Free;
  BricksToCalc.Free;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay23'}
procedure TAdventOfCodeDay23.BeforeSolve;
var
  x, y: Integer;
  Position: TPosition;
  PathTile: TPathTile;
begin
  Map := TDictionary<TPosition,TPathTile>.Create;

  for y := 0 to FInput.Count-1 do
    for x := 1 to Length(FInput[0]) do
    begin
      PathTile := TPathTile(Pos(FInput[Y][X], '^>v<#.')-1);
      Position := TPosition.Create(X-1, Y);

      if (PathTile = TPathTile.Path) and (y = 0) then
        StartPosition := Position.Clone;

      if (PathTile = TPathTile.Path) and (y = FInput.Count-1) then
        StopPosition := Position.Clone;

      Map.Add(TPosition.Create(X-1, Y), PathTile);
    end;
end;

procedure TAdventOfCodeDay23.AfterSolve;
begin
  Map.Free;
end;

function TAdventOfCodeDay23.FindPath(const CanClimbSlopes: Boolean): Integer;
var
  PositionKeys: TDictionary<TPosition, Byte>;
  SimpleMap: TDictionary<Byte,TDictionary<Byte, Integer>>;
  StopId: Byte;

  function PositionKey(aPosition: TPosition): Byte;
  begin
    if PositionKeys.TryGetValue(aPosition, Result) then
      Exit;
    Result := PositionKeys.Count;
    PositionKeys.Add(aPosition, Result);
  end;

  function CanEnterPath(aPathTile: TPathTile; NextDirecetion: TAOCDirection): Boolean;
  begin
    if aPathTile = wall then
      Exit(False);

    if aPathTile = Path then
      Exit(True);

    if CanClimbSlopes then
      Exit(True);

    Result :=
      ((aPathTile = SlopeNorth) and (NextDirecetion = North)) or
      ((aPathTile = SlopeEast) and (NextDirecetion = East)) or
      ((aPathTile = SlopeSouth) and (NextDirecetion = South)) or
      ((aPathTile = SlopeWest) and (NextDirecetion = West));
  end;

  procedure BuildSimplifiedMap(From: Byte; CurrentPos: TPosition; aPrevDirection: TAOCDirection; StepsTaken: Integer);
  var
    NextDirections: TAocDirections;
    NextDirecetion, PrevDirection, NextPrevDirection: TAOCDirection;
    NextDirectionCount, PrevSteps: Integer;
    NextPosition: TPosition;
    NextPathTile: TPathTile;
  begin
    PrevDirection := aPrevDirection;

    repeat
      NextDirectionCount := 0;
      NextDirections := [];
      NextPrevDirection := North;

      for NextDirecetion in [North, East, South, West] do
      begin
        if RotateDirection(NextDirecetion, 2) = PrevDirection then
          Continue;

        NextPosition := CurrentPos.Clone.ApplyDirection(NextDirecetion);
        if not Map.TryGetValue(NextPosition, NextPathTile) then
          Continue;

        if CanEnterPath(NextPathTile, NextDirecetion) then
        begin
          Inc(NextDirectionCount);
          Include(NextDirections, NextDirecetion);
          NextPrevDirection := NextDirecetion;;
        end;
      end;

      StepsTaken := StepsTaken + 1;
      if NextDirectionCount = 1 then
      begin
        PrevDirection := NextPrevDirection;
        CurrentPos := CurrentPos.ApplyDirection(PrevDirection);
      end
      else if NextDirectionCount > 1 then
      begin
        NextDirecetion := RotateDirection(PrevDirection, 2);
        if CanEnterPath(Map[CurrentPos.Clone.ApplyDirection(NextDirecetion)], NextDirecetion) then
           Include(NextDirections, NextDirecetion);
      end;

    until (NextDirectionCount <> 1);

    SimpleMap[From].TryGetValue(PositionKey(CurrentPos), PrevSteps);
    SimpleMap[From].AddOrSetValue(PositionKey(CurrentPos), Max(StepsTaken, PrevSteps));

    if not SimpleMap.ContainsKey(PositionKey(CurrentPos)) then
    begin
      SimpleMap.Add(PositionKey(CurrentPos), TDictionary<Byte,Integer>.Create);
      for NextDirecetion in NextDirections do
        BuildSimplifiedMap(PositionKey(CurrentPos), CurrentPos.Clone.ApplyDirection(NextDirecetion), NextDirecetion, 0);
    end;
  end;

  function _FindPath(CurrentPos: Byte; Seen: SetOfByte; StepsTaken: Integer): integer;
  var
    NextPosition: Byte;
  begin
    if CurrentPos = StopId then
      Exit(StepsTaken);

    Result := 0;
    for NextPosition in SimpleMap[CurrentPos].Keys do
    begin
      if NextPosition in Seen then
        Continue;

      Include(Seen, NextPosition);
      Result := Max(Result, _FindPath(NextPosition, Seen, StepsTaken + SimpleMap[CurrentPos][NextPosition]));
      ExClude(Seen, NextPosition);
    end;
  end;

begin
  PositionKeys := TDictionary<TPosition, Byte>.Create;

  SimpleMap := TDictionary<Byte,TDictionary<Byte, Integer>>.Create;
  SimpleMap.Add(PositionKey(StartPosition), TDictionary<Byte, Integer>.Create);
  StopId := PositionKey(StopPosition);
  BuildSimplifiedMap(PositionKey(StartPosition), StartPosition, East, -1);

  Result := _FindPath(PositionKey(StartPosition), [PositionKey(StartPosition)], 0);
  SimpleMap.Free;
  PositionKeys.Free;
end;

function TAdventOfCodeDay23.SolveA: Variant;
begin
  Result := FindPath(False);
end;

function TAdventOfCodeDay23.SolveB: Variant;
begin
  Result := FindPath(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay24'}
class function THailStone.Create(aPosition, aVelocity: TPosition3): THailStone;
begin
  Result.Position := aPosition;
  Result.Velocity := aVelocity;
end;

procedure TAdventOfCodeDay24.BeforeSolve;
var
  s: String;
  Split: TStringDynArray;
begin
  HailStones := TList<THailStone>.Create;

  for s in FInput do
  begin
    split := SplitString(s.Replace(' ', '', [rfReplaceAll]), '@,');
    HailStones.Add(THailStone.Create(
      TPosition3.Create(Split[0].ToInt64,Split[1].ToInt64, Split[2].ToInt64),
      TPosition3.Create(Split[3].ToInt64,Split[4].ToInt64, Split[5].ToInt64)));
  end;
end;

procedure TAdventOfCodeDay24.AfterSolve;
begin
  HailStones.Free;
end;

function TAdventOfCodeDay24.GetIntersection(PositionX1, PositionY1, VelocityX1,
  VelocityY1, PositionX2, PositionY2, VelocityX2, VelocityY2: Int64; out Time1,
  Time2: Float64): Boolean;
var
  a, b, c, d, e, f: Int64;
begin
  // Modified from  https://codereview.stackexchange.com/questions/238711/solving-a-linear-system-with-two-variables

  a := -VelocityX1;
  b := VelocityX2;
  c := PositionX1 - PositionX2;

  d := -VelocityY1;
  e := VelocityY2;
  f := PositionY1 - PositionY2;

  if ((a * e - b *d) = 0)  then
    Exit(False);

  Result := True;
  Time2 := (a * f - c * d) / (a * e - b  *d);
  Time1 := (c - (b * Time2)) / a;
end;

function TAdventOfCodeDay24.SolveA: Variant;
Const
  LowerBound: Int64 = 200000000000000;
  HigherBound: Int64 = 400000000000000;
var
  i, j: Integer;
  HailStoneOne, HailStoneTwo: THailStone;
  TimeOne, TimeTwo, IntersectX, InterSectY: Float64;
begin
  result := 0;
  for i := 0 to FInput.Count-1 do
  begin
    HailStoneOne := HailStones[i];

    for j := i + 1 to FInput.Count-1 do
    begin
      HailStoneTwo := HailStones[j];

      if not GetIntersection(HailStoneOne.Position.X, HailStoneOne.Position.Y, HailStoneOne.Velocity.X, HailStoneOne.Velocity.Y,
                             HailStoneTwo.Position.X, HailStoneTwo.Position.Y, HailStoneTwo.Velocity.X, HailStoneTwo.Velocity.Y, TimeOne,TimeTwo ) then
        Continue;

      if (TimeOne < 0) then
        Continue;

      if (TimeTwo < 0) then
        Continue;

      IntersectX := HailStoneOne.Position.X + TimeOne * HailStoneOne.Velocity.X;
      InterSectY := HailStoneOne.Position.Y + TimeOne * HailStoneOne.Velocity.Y;

      if (IntersectX < LowerBound) or (IntersectX > HigherBound) then
        Continue;

      if (InterSectY < LowerBound) or (InterSectY > HigherBound) then
        Continue;

      Inc(Result);
    end;
  end;
end;

function TAdventOfCodeDay24.SolveB: Variant;

  procedure _CheckVelocity(Difference, Velocity: Int64; PossibleVelocitys: TList<Int64>);
  var
    Factors: TList<Int64>;
    i: Int64;
  begin
    Factors := TList<Int64>.Create;

    for i := -1000 to 1000 do
      if (i + Velocity <> 0) and (Difference mod (i + Velocity) = 0) then
        Factors.Add(-i);

    if PossibleVelocitys.Count = 0 then
      PossibleVelocitys.AddRange(Factors)
    else
      for i := PossibleVelocitys.Count -1 downto 0 do
        if not Factors.Contains(PossibleVelocitys[i]) then
          PossibleVelocitys.Delete(i);

    Factors.Free;
  end;

  procedure _AddVelocitysIfEmpty(PossibleVelocitys: TList<Int64>);
  var
    I: Int64;
  begin
    if PossibleVelocitys.Count > 0 then
      Exit;

    for i := -500 to 500 do
      PossibleVelocitys.Add(i);
  end;

var
  VelocityX, VelocityY, VelocityZ: TList<Int64>;
  i, j: Integer;
  HailStoneOne, HailStoneTwo, HailStone: THailStone;
  VelX, VelY, VelZ: Int64;
  x, y, z, TimeOne, TimeTwo: Float64;
  AllHit: Boolean;
begin
  VelocityX := TList<Int64>.Create;
  VelocityY := TList<Int64>.Create;
  VelocityZ := TList<Int64>.Create;
  try

    for i := 0 to FInput.Count-1 do
    begin
      HailStoneOne := HailStones[i];

      for j := i + 1 to FInput.Count-1 do
      begin
        HailStoneTwo := HailStones[j];

        if HailStoneOne.Velocity.X = HailStoneTwo.Velocity.X then
          _CheckVelocity(HailStoneOne.Position.X - HailStoneTwo.Position.X, HailStoneOne.Velocity.X, VelocityX);
        if HailStoneOne.Velocity.Y = HailStoneTwo.Velocity.Y then
          _CheckVelocity(HailStoneOne.Position.Y - HailStoneTwo.Position.Y, HailStoneOne.Velocity.Y, VelocityY);
        if HailStoneOne.Velocity.Z = HailStoneTwo.Velocity.Z then
          _CheckVelocity(HailStoneOne.Position.Z - HailStoneTwo.Position.Z, HailStoneOne.Velocity.Z, VelocityZ);
      end;
    end;

    _AddVelocitysIfEmpty(VelocityX);
    _AddVelocitysIfEmpty(VelocityY);
    _AddVelocitysIfEmpty(VelocityZ);

    HailStoneOne := HailStones[0];
    HailStoneTwo := HailStones[1];

    for VelX in VelocityX do
      for VelY in VelocityY do
        for VelZ in VelocityZ do
        begin
          if not GetIntersection(
            HailStoneOne.Position.X, HailStoneOne.Position.Y, HailStoneOne.Velocity.X - VelX, HailStoneOne.Velocity.Y - VelY,
            HailStoneTwo.Position.X, HailStoneTwo.Position.Y, HailStoneTwo.Velocity.X - VelX, HailStoneTwo.Velocity.Y - VelY,
            TimeOne, TimeTwo) then
            Continue;

          x := HailStoneOne.Position.X + (HailStoneOne.Velocity.X - VelX) * TimeOne;
          y := HailStoneOne.Position.Y + (HailStoneOne.Velocity.Y - VelY) * TimeOne;
          z := HailStoneOne.Position.Z + (HailStoneOne.Velocity.Z - VelZ) * TimeOne;

          AllHit := (VelocityX.Count = 1) and (VelocityY.Count = 1) and (VelocityZ.Count = 1);
          if not AllHit then // Only needed for the example
          begin
            AllHit := True;
            for HailStone in HailStones do
            begin
              if (HailStone.Velocity.x - velX) = 0 then
                Continue;

              TimeOne := Abs((HailStone.Position.x - x) / (HailStone.Velocity.x - velX));

              AllHit :=
                ((HailStone.Position.X + HailStone.Velocity.x * TimeOne) = (X + VelX * TimeOne)) and
                ((HailStone.Position.Y + HailStone.Velocity.Y * TimeOne) = (Y + VelY * TimeOne)) and
                ((HailStone.Position.Z + HailStone.Velocity.Z * TimeOne) = (Z + VelZ * TimeOne));
              if not AllHit then
                Break;


            end;
          end;

          if AllHit then
          begin
            Result := x + y + z;
            Exit;
          end;
        end;
  finally
    VelocityX.Free;
    VelocityY.Free;
    VelocityZ.Free;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay25'}
Type
  WireInfo = record
    ModuleTo: Int64;
    WiresUsed: String;
    StepsTaken: Integer;
    class function Create(aModuleTo: Int64; aWiresUsed: String; aStepsTaken: Integer): WireInfo; Static;
  end;

class function WireInfo.Create(aModuleTo: Int64; aWiresUsed: String; aStepsTaken: Integer): WireInfo;
begin
  Result.ModuleTo := aModuleTo;
  Result.WiresUsed := aWiresUsed;
  Result.StepsTaken := aStepsTaken;
end;

function TAdventOfCodeDay25.SolveA: Variant;
var
  ModuleIds: TDictionary<string, Int64>;
  Modules: TDictionary<Int64,TList<Int64>>;

  function WireName(aWire1, aWire2: string): string;
  begin
    if aWire1 < aWire2 then
      Result := aWire1 + aWire2
    else
      Result := aWire2 + aWire1;
  end;

  function ModuleId(Const aNodeName: String): Int64;
  begin
    if ModuleIds.TryGetValue(aNodeName, Result) then
      Exit;
    Result := ModuleIds.Count + 1;
    ModuleIds.Add(aNodeName, Result);
  end;

  function CalcWireId(Module1, Module2: Int64): Int64;
  begin
    Result := (Max(Module1, Module2) shl 16) + Min(Module1, Module2);
  end;

  function CheckMap(aFrom: Int64; Disconected1, Disconected2, Disconected3: Int64; BuildMap: Boolean; WireUsage: TList<Int64>): Integer;
  var
    Work: TQueue<WireInfo>;
    Seen: TDictionary<Integer,Boolean>;
    Info: WireInfo;
    s: String;
    Split: TStringDynArray;
    WireCount: TDictionary<Int64,Int64>;
    WireCountPair: TPair<Int64,Int64>;
    PrevCount, WireId, i: Int64;
  begin
    Work := TQueue<WireInfo>.Create;
    Seen := TDictionary<Integer,Boolean>.Create;
    WireCount := TDictionary<Int64,Int64>.Create;


    Work.Enqueue(WireInfo.Create(aFrom, '', 0));

    while Work.Count > 0 do
    begin
      Info := Work.Dequeue;

      if Seen.ContainsKey(Info.ModuleTo) then
        Continue;
      Seen.Add(Info.ModuleTo, True);

      if BuildMap then
      begin
        split := SplitString(Info.WiresUsed, '|');
        for s in Split do
        begin
          if not TryStrToInt64(s, WireId) then
            Continue;

          WireCount.TryGetValue(WireId, PrevCount);
          WireCount.AddOrSetValue(WireId, PrevCount + 1);
        end;
      end;

      for i in Modules[Info.ModuleTo] do
      begin
        WireId := CalcWireId(Info.ModuleTo, i);
        if (WireId <> Disconected1) and (WireId <> Disconected2) and (WireId <> Disconected3)   then
          Work.Enqueue(WireInfo.Create(i, Info.WiresUsed + '|' + WireId.ToString, Info.StepsTaken + 1));
      end
    end;

    if BuildMap then
    begin
      WireUsage.Clear;
      for WireCountPair in WireCount do
        WireUsage.Add((WireCountPair.Value shl 32) + WireCountPair.Key);
      WireUsage.Sort;
      WireUsage.Reverse;
    end;

    Result := 0;
    if Seen.Count <> Modules.Count then
      Result := Seen.Count * (Modules.Count - Seen.Count);

    Seen.Free;
    Work.Free;
    WireCount.Free;
  end;

var
  s: String;
  split: TStringDynArray;
  ModuleId0, ModuleIdI, x, y, z, i: Integer;
  WireUsage1, WireUsage2, WireUsage3, Connected: TList<Int64>;
begin
  Modules := TObjectDictionary<Int64,TList<Int64>>.Create([doOwnsValues]);
  ModuleIds := TDictionary<string, Int64>.Create;
  WireUsage1 := TList<Int64>.Create;
  WireUsage2 := TList<Int64>.Create;

  try
    for s in FInput do
    begin
      split := SplitString(s.Replace(':', '', []), ' ');
      ModuleId0 := ModuleId(Split[0]);

      for i := 1 to Length(split)-1 do
      begin
        ModuleIdI := ModuleId(Split[i]);

        if not Modules.TryGetValue(ModuleId0, Connected) then
        begin
          Connected := TList<Int64>.Create;
          Modules.Add(ModuleId0, Connected);
        end;
        Connected.Add(ModuleIdI);

        if not Modules.TryGetValue(ModuleIdI, Connected) then
        begin
          Connected := TList<Int64>.Create;
          Modules.Add(ModuleIdI, Connected);
        end;
        Connected.Add(ModuleId0);
      end;
    end;

    CheckMap(1, 0, 0, 0, True, WireUsage1);
    for x := 0 to Min(20, WireUsage1.Count-1) do
    begin
      CheckMap((WireUsage1.Last and maxInt) shr 16, WireUsage1[x] and MaxInt, 0, 0, True, WireUsage2);

      for y := x + 1 to Min(20, WireUsage1.Count-1) do
      begin
        for z := y + 1 to Min(20, WireUsage1.Count-1) do
        begin
          Result := CheckMap((WireUsage1.Last and maxInt) shr 16, WireUsage1[x] and MaxInt, WireUsage2[y] and MaxInt, WireUsage2[z] and MaxInt, False, nil);
          if Result > 0 then
          begin
            WriteLn('Done');
            Exit;
          end;
        end;
      end;
    end;
  finally
    Modules.Free;
    ModuleIds.Free;
    WireUsage1.Free;
    WireUsage2.Free;
  end;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin
  Result := null;
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
  TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
  TAdventOfCodeDay11,TAdventOfCodeDay12,TAdventOfCodeDay13,TAdventOfCodeDay14,TAdventOfCodeDay15,
  TAdventOfCodeDay16,TAdventOfCodeDay17,TAdventOfCodeDay18,TAdventOfCodeDay19,TAdventOfCodeDay20,
  TAdventOfCodeDay21,TAdventOfCodeDay22,TAdventOfCodeDay23,TAdventOfCodeDay24,TAdventOfCodeDay25]);

end.
