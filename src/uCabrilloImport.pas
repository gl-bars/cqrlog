(*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.
*)

unit uCabrilloImport;

//frmCabrilloImport backend

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl;

type
  TStringMap = specialize TFPGMap<string, string>;

  { TQSOLinesIterator }

  TQSOLinesIterator=class
  private
    FCurrentIndex: integer;

    function GetCurrentLine: string;
  public
    Lines: TStringList;
    Fields: TStringMap;
    FieldLengths: array of integer;

    constructor Create;
    destructor Destroy; override;
    procedure ParseCurrent;
    function MoveNext: Boolean;
    procedure Reset;

    property CurrentLine: string read GetCurrentLine;
  end;

  ICabrilloGrid = interface
    procedure AddToColumnsGrid(Field: string; MaxLength: integer);
    procedure ModifyColumnsGridLastRow(Field: string; MaxLength: integer);
  end;


//Extracted from fCabrilloImport

procedure GuessQSORows;

var
  QSOIter: TQSOLinesIterator = nil;
  CabrilloGrid: ICabrilloGrid = nil;

implementation

procedure GuessQSORows;

  function CopyLField(S: String; StartPos: integer): string;
  var
    p: Integer;
  begin
    p:=StartPos;
    Assert(p<=Length(S), Format('String index out of range (%d/%d)', [p, Length(S)]));

    //Left-aligned field data
    while (p<Length(S)) and (S[p]<>' ') do
      Inc(p);
    //Padding space
    while (p<Length(S)) and (S[p]=' ') do
      Inc(p);
    p-=2; //return to the last char of the current field
    Result:=Copy(S, StartPos, p-StartPos+1);
  end;


  function GuessFieldName(Len: integer): string;
  begin
    case Len of
    1: Result:='t';
    2: Result:='Mo';
    3: Result:='Rst';
    4: Result:='Time';
    6: Result:='Exch';
    10: Result:='Date';
    13: Result:='Call';
    else Result:='';
    end;
  end;

const
  FREQ_LEN=5;
  TERMINAL_FIELD=' Z'; //add extra field to simplify the logic
var
  s, Field: String;
  p, Len: integer;
begin
  Assert(QSOIter.Lines.Count>0, 'QSOIter.Lines must be non-empty');

  s:=QSOIter.Lines[0] + TERMINAL_FIELD;
  p:=1;
  Len:=0;

  if s[p+FREQ_LEN]=' ' then begin
    p+=FREQ_LEN;
    CabrilloGrid.AddToColumnsGrid('Freq', FREQ_LEN);
  end;

  while p<=Length(s)-Length(TERMINAL_FIELD) do begin
    while (p <= Length(s)) and (s[p]=' ') do
      Inc(p); //skip space between fields
    Field:=CopyLField(s, p);
    Assert(Length(Field)>0, Format('Field cannot be empty - p=%d ("%s")', [p, s[p]]));
    Len:=Length(Field);
    p+=Len;
    CabrilloGrid.AddToColumnsGrid(GuessFieldName(Len), Len);
  end;
  //Due to stripping of trailing space the length of the last field may vary
  if Len=1 then
    CabrilloGrid.ModifyColumnsGridLastRow('t', 1)
  else
    CabrilloGrid.ModifyColumnsGridLastRow('Exch', Len);
end;

{ TQSOLinesIterator }

function TQSOLinesIterator.GetCurrentLine: string;
begin
  Result:=Lines[FCurrentIndex];
end;

constructor TQSOLinesIterator.Create;
begin
  Lines:=TStringList.Create;
  Fields:=TStringMap.Create;
  Reset;
end;

destructor TQSOLinesIterator.Destroy;
begin
  Lines.Free;
  Fields.Free;
  //inherited Destroy;
end;

procedure TQSOLinesIterator.ParseCurrent;
var
  p, i, N: Integer;
  Line: String;
begin
  Line:=Lines[FCurrentIndex];
  p:=1;
  N:=Length(FieldLengths);
  Assert(Fields.Count=N);
  for i:=0 to N-1 do begin
    Fields.Data[i]:=Trim(Copy(Line, p, FieldLengths[i]));
    p+=FieldLengths[i]+1; //+1: space
  end;
end;

function TQSOLinesIterator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result:=(FCurrentIndex<Lines.Count);
end;

procedure TQSOLinesIterator.Reset;
begin
  FCurrentIndex:=-1;
end;

end.

