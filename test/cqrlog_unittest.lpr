program cqrlog_unittest;

{$mode objfpc}{$H+}

uses
  consoletestrunner, cabrilloimport_test;

var
  Runner: TTestRunner;
begin
  Runner:=TTestRunner.Create(nil);
  Runner.Initialize;
  Runner.Run;
  Runner.Free;
end.

