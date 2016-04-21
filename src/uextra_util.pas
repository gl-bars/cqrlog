(*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.
*)

unit uextra_util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrtti;

function StreamObjectToJson(const o: TObject): string;


implementation

function StreamObjectToJson(const o: TObject): string;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray]; // Save strings as JSON array
    Result := Streamer.ObjectToJSONString(o);
  finally
    Streamer.Free;
  end;
end;

end.

