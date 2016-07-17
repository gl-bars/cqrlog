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
  Classes,
  mysql56dyn, mysql56conn; //prefer 5.6

type

{ TMysqlHelper }

TMysqlHelper = class helper for TConnectionName
  procedure CreateDBUTF8;
end;

implementation

{ TMysqlHelper }

procedure TMysqlHelper.CreateDBUTF8;
var
  P: procedure (const query : string) of object;
begin
  TMethod(P).Code:=@TConnectionName.ExecuteDirectMySQL;
  TMethod(P).Data:=Self;
  P('CREATE DATABASE IF NOT EXISTS '+DatabaseName+
    ' DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_bin');
end;

end.

