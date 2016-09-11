(*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.
*)

unit cabrilloimport_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, fpcunit, testregistry, uCabrilloImport;

  //Cannot use fCabrilloImport since it requires full init sequence for dData (e.g. connection to cqrlog001 DB)

type

{ TCabrilloImportTest }

TCabrilloImportTest=class(TTestCase, ICabrilloGrid)
private
  Fields: array of string;
  MaxLens: array of integer;

protected
  procedure SetUp; override;
  procedure TearDown; override;

public
  procedure ClearColumnsGrid;
  //ICabrilloGrid mock
  procedure AddToColumnsGrid(Field: string; MaxLength: integer);
  procedure ModifyColumnsGridLastRow(Field: string; MaxLength: integer);

published
  procedure FieldDetectionTest;
end;

implementation

{ TCabrilloImportTest }

procedure TCabrilloImportTest.SetUp;
begin
  QSOIter:=TQSOLinesIterator.Create;
  CabrilloGrid:=Self;
end;

procedure TCabrilloImportTest.TearDown;
begin
  FreeAndNil(QSOIter);
end;

procedure TCabrilloImportTest.ClearColumnsGrid;
begin
  SetLength(Fields, 0);
  SetLength(MaxLens, 0);
end;

procedure TCabrilloImportTest.AddToColumnsGrid(Field: string; MaxLength: integer);
var
  H: Integer;
begin
  H:=Length(Fields);
  SetLength(Fields, H+1);
  Fields[H]:=Field;
  SetLength(MaxLens, H+1);
  MaxLens[H]:=MaxLength;
end;

procedure TCabrilloImportTest.ModifyColumnsGridLastRow(Field: string; MaxLength: integer);
var
  H: Integer;
begin
  H:=High(Fields);
  Fields[H]:=Field;
  MaxLens[H]:=MaxLength;
end;

procedure TCabrilloImportTest.FieldDetectionTest;
type
  TFieldArray = array of string;
  TLenArray = array of integer;
const
  N_LINES=6;
  LINES: array [1..N_LINES] of string = (
    ' 3549 CW 2005-01-07 1846 CQRLOGEX      599 AP     LW9HN         599 OB    ', //RDXC
    ' 1825 CW 2006-03-10 2320 CQRLOGEX      599 32     UW4FX         599 14     0', //CQ-CW
    '21026 CW 2007-03-15 1338 CQRLOGEX      599 001    JU3RO         599 VN      ', //HA-DX
    ' 3533 CW 2008-04-18 0004 CQRLOGEX      001   46N23O   IF4L          001   52N55O   ', //RAEM
    '   14 CW 2009-12-23 1531 CQRLOGEX      599 001    XI8R          599 D     ', //SPDXC
    ' 3519 CW 2010-04-10 1939 CQRLOGEX      599 UT10NWR       599' //NA
    );
var
  ExpFields: array [1..N_LINES] of TFieldArray;
  ExpLens: array [1..N_LINES] of TLenArray;
  i, j, nFields: Integer;

begin
  ExpFields[1]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', 'Rst', 'Exch', 'Call', 'Rst', 'Exch');
  ExpLens[1]:=TLenArray.Create(5, 2, 10, 4, 13, 3, 6, 13, 3, 6);
  ExpFields[2]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', 'Rst', 'Exch', 'Call', 'Rst', 'Exch', 't');
  ExpLens[2]:=TLenArray.Create(5, 2, 10, 4, 13, 3, 6, 13, 3, 6, 1);
  ExpFields[3]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', 'Rst', 'Exch', 'Call', 'Rst', 'Exch');
  ExpLens[3]:=TLenArray.Create(5, 2, 10, 4, 13, 3, 6, 13, 3, 8);
  ExpFields[4]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', '', '', 'Call', '', 'Exch');
  ExpLens[4]:=TLenArray.Create(5, 2, 10, 4, 13, 5, 8, 13, 5, 9);
  ExpFields[5]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', 'Rst', 'Exch', 'Call', 'Rst', 'Exch');
  ExpLens[5]:=TLenArray.Create(5, 2, 10, 4, 13, 3, 6, 13, 3, 6);
  ExpFields[6]:=TFieldArray.Create('Freq', 'Mo', 'Date', 'Time', 'Call', 'Rst', 'Call', 'Exch');
  ExpLens[6]:=TLenArray.Create(5, 2, 10, 4, 13, 3, 13, 3);

  for i:=1 to N_LINES do
    AssertEquals('Test data issue: length mismatch between ExpFields and ExpLens for i='+IntToStr(i),
                 Length(ExpFields[i]), Length(ExpLens[i]));

  for i:=1 to N_LINES do begin
    WriteLn('Test line ', i);
    ClearColumnsGrid;
    QSOIter.Lines.Clear;
    QSOIter.Lines.Add(LINES[i]);
    GuessQSORows;

    AssertEquals('Field count i='+IntToStr(i), Length(ExpFields[i]), Length(Fields));
    nFields:=Min(Length(ExpFields[i]), Length(Fields));
    for j:=0 to nFields-1 do begin
      AssertEquals('Max len j='+IntToStr(j), ExpLens[i][j], MaxLens[j]);
      AssertEquals('Field j='+IntToStr(j), ExpFields[i][j], Fields[j]);
    end;

  end;
end;

initialization
  RegisterTest(TCabrilloImportTest);
end.

