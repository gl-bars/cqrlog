(*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.
*)

unit umysql_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, mysql51conn, mysql55conn, mysql56conn, mysql57conn;

type

{ TMysqlHelper }

TMysqlHelper = class helper for TConnectionName
  procedure CreateDBUTF8(fMySQLVersion: currency);
end;

implementation

{ TMysqlHelper }

procedure TMysqlHelper.CreateDBUTF8(fMySQLVersion: currency);
var
  P: procedure (const query : string) of object;
begin
  //For all supported mysql versions
  if fMySQLVersion < 5.5 then
    TMethod(P).Code:=@mysql51conn.TConnectionName.ExecuteDirectMySQL
  else if fMySQLVersion < 5.6 then
    TMethod(P).Code:=@mysql55conn.TConnectionName.ExecuteDirectMySQL
  else if fMySQLVersion < 5.7 then
    TMethod(P).Code:=@mysql56conn.TConnectionName.ExecuteDirectMySQL
  else
    TMethod(P).Code:=@mysql57conn.TConnectionName.ExecuteDirectMySQL;

  TMethod(P).Data:=Self;
  P('CREATE DATABASE IF NOT EXISTS '+DatabaseName+
    ' DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_bin');
end;

end.

