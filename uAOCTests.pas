unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows, system.Classes,
  uAocUtils, AocSolutions, AOCBase, uAOCConfig, uAocTimer;

type
  AOCTest = record
    AOCClass: TAdventOfCodeRef;
    ExpectedSolutionA, ExpectedSolutionB: String;
    LoadOverridenTestData: TLoadOverridenTestData
end;

type AOCTests = class
public
  Class procedure RunTests(aConfig: TAOCConfig);
end;

Const AOCTestData: array[0..10] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '55029'; ExpectedSolutionB: '55686'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '3099'; ExpectedSolutionB: '72970'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '527144'; ExpectedSolutionB: '81463996'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '32001'; ExpectedSolutionB: '5037841'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '57075758'; ExpectedSolutionB: '31161857'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '1195150'; ExpectedSolutionB: '42550411'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '252295678'; ExpectedSolutionB: '250577259'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '18157'; ExpectedSolutionB: '14299763833181'),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '1696140818'; ExpectedSolutionB: '1152'),
 (AOCClass: TAdventOfCodeDay10; ExpectedSolutionA: '6942'; ExpectedSolutionB: '297'),
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '9274989'; ExpectedSolutionB: '357134560737')
);

implementation
class procedure AOCTests.RunTests(aConfig: TAOCConfig);

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
  end;

Var
  Test: AOCTest;
  AdventOfCode: TAdventOfCode;
  TotalTime, TestTimer: AocTimer;
  SolutionA, SolutionB: string;
  Times: TStringList;
  ElapsedMicroSeconds: Integer;
  s: string;
begin
  Writeln('');

  Times := TStringList.Create;
  try
    TotalTime := AOCTimer.Start;
    for Test in AOCTestData do
    begin
      Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

      AdventOfCode := Test.AOCClass.Create(aConfig);

      TestTimer := AOCTimer.Start;
      AdventOfCode.Test(SolutionA, SolutionB, Test.LoadOverridenTestData);
      ElapsedMicroSeconds := TestTimer.ElapsedTime;
      Times.Add(Format('%s -> Time: %d %s', [Test.AOCClass.Classname, ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      AdventOfCode.Free;

      _Check('Part a', Test.ExpectedSolutionA, SolutionA);
      _Check('Part b', Test.ExpectedSolutionB, SolutionB);
      Writeln(FormAt('Total time %d %s', [ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      Writeln('');
    end;

    Writeln(Format('All tests done in %d %s', [TotalTime.ElapsedTime(MilliSeconds), TimeIndicator[MilliSeconds]]));
    for s in Times do
      WriteLn(s);
  finally
    Times.Free;
  end
end;

end.
