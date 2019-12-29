(*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.
*)

unit fCabrilloImport;

{
  Note: No information from Cabrillo header is committed to database

  //TODO: compare Call_s and other personal info with selected profile
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, SynEdit, SynHighlighterAny,
  LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Grids, ButtonPanel, strutils, LCLProc, Spin, DBGrids, variants, LCLType,
  dData, dUtils, fAdifImport, uCabrilloImport, types, math;

type
  EUserAborted = class(Exception);

  { TfrmCabrilloImport }

  TfrmCabrilloImport = class(TForm)
    btnClose: TButton;
    btnImport: TButton;
    buttonNext: TButton;
    checkBoxReplacePHWith: TCheckBox;
    cmbProfiles: TComboBox;
    DataSource1: TDataSource;
    editFreqMultiplier: TEdit;
    editReplacePHWith: TEdit;
    edtRemarks: TEdit;
    groupCorrections: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    labelFreqMultiplier: TLabel;
    labelResult: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCount: TLabel;
    lblErrorLog: TLabel;
    lblErrors: TLabel;
    lblFileName: TLabel;
    pageControlSteps: TPageControl;
    Panel1: TPanel;
    panelHeader: TPanel;
    panelQSOs: TPanel;
    Q1: TSQLQuery;
    Q4: TSQLQuery;
    Splitter1: TSplitter;
    gridHeader: TStringGrid;
    gridQSOs: TStringGrid;
    sheetChooseColumns: TTabSheet;
    sheetResult: TTabSheet;
    SynAnySyn1: TSynAnySyn;
    synEditColumns: TSynEdit;
    TimerParseCols: TTimer;
    tr: TSQLTransaction;
    procedure buttonNextClick(Sender: TObject);
    procedure checkBoxReplacePHWithChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure synEditColumnsChange(Sender: TObject);
    procedure TimerParseColsTimer(Sender: TObject);
  private
    { private declarations }
    ColumnsList: TColumnsList;
    OldQSO_Count: integer;
    TimerCount: integer;
    FreqMultiplier: double;
    ReplaceModePH: boolean;
    WrongCabrilloLines: TStringList;
    ERR_FILE : String;
    WrongRecNr : Integer;
    GlobalProfile : Word;

    function GetCabrilloFilename: string;

    procedure LoadCabrilloLines(const Lines: TStringList);
    procedure AddToHeaderGrid(Param, Value: string);
    procedure GridSetSentRcvd;
    procedure ReloadQSOGrid;
    procedure CheckIfCorrectionsNeeded;
    function CheckForDuplicateColumns: boolean;
    function CheckForMandatoryFields: boolean;
    function ParseColumnsString: boolean;
    procedure DoImport;
    procedure InsertHeaderFromFile(var IntoList: TStringList);
    function MakeRecord(out d: TnewQSOEntry; var ErrorMessage: string): boolean;
    procedure AddNewRecord(var d: TnewQSOEntry);
    procedure ApplyLrsGridWorkaround;

    procedure InitQSOLinesIterator;
    function ReadQSOFields: boolean;
  public
    { public declarations }

    procedure ClearGridRows(var Grid: TStringGrid);
    procedure AddRowToGrid(var Grid: TStringGrid);
    function GetQSOCount: integer;

    property CabrilloFilename: string read GetCabrilloFilename;
  end;

const
  //cqrlog_main field names
  F_QSODATE = 'qsodate';
  F_TIME_ON = 'time_on';
  F_TIME_OFF = 'time_off';
  F_CALLSIGN = 'callsign';
  F_FREQ = 'freq';
  F_MODE = 'mode';
  F_RST_S = 'rst_s';
  F_RST_R = 'rst_r';
  F_NAME = 'name';
  F_QTH = 'qth';
  F_QSL_S = 'qsl_s';
  F_QSL_R = 'qsl_r';
  F_QSL_VIA = 'qsl_via';
  F_IOTA = 'iota';
  F_PWR = 'pwr';
  F_ITU = 'itu';
  F_WAZ = 'waz';
  F_LOC = 'loc';
  F_MY_LOC = 'my_loc';
  F_REMARKS = 'remarks';
  F_COUNTY = 'county';
  F_ADIF = 'adif';
  F_IDCALL = 'idcall';
  F_AWARD = 'award';
  F_BAND = 'band';
  F_STATE = 'state';
  F_CONT = 'cont';
  F_PROFILE = 'profile';
  F_LOTW_QSLSDATE = 'lotw_qslsdate';
  F_LOTW_QSLS = 'lotw_qsls';
  F_LOTW_QSLRDATE = 'lotw_qslrdate';
  F_LOTW_QSLR = 'lotw_qslr';
  F_QSLS_DATE = 'qsls_date';
  F_QSLR_DATE = 'qslr_date';
  F_EQSL_QSLSDATE = 'eqsl_qslsdate';
  F_EQSL_QSL_SENT = 'eqsl_qsl_sent';
  F_EQSL_QSLRDATE = 'eqsl_qslrdate';
  F_EQSL_QSL_RCVD = 'eqsl_qsl_rcvd';

  F_EXCH1 = 'exch1';
  F_EXCH2 = 'exch2';

  CBR_QSO='QSO: ';

resourcestring
  BUTTON_READY = 'Ready';
  IMPORT_RESULT_NEW_QSOS = 'Result: %d new QSOs';
  COL_FORMAT_ERRORS_FOUND = 'Format errors found! Please correct Columns string before importing.';
  MANDATORY_FIELDS_MISSING = 'Following mandatory fields are missing: %s'+LineEnding+'Please add them to Columns string and try again';
  DUPLICATE_COLUMN = 'Duplicate column "%s"[%d]';
  FREQ_MULT_NOT_VALID_FLOAT = 'Error: "%s" is not a valid float value. Correct Freq multiplier and try again';
  WRONG_QSO_FREQ = 'Wrong QSO freq: %s';
  ERROR_PARSING_TEXT = 'Error parsing "%s"';

implementation

uses uMyIni, dDXCC, uextra_util;

{ TfrmCabrilloImport }

procedure TfrmCabrilloImport.FormCreate(Sender: TObject);
var
  tmp: Char;
begin
  ColumnsList:=TColumnsList.Create;

  QSOIter:=TQSOLinesIterator.Create;
  CabrilloGrid:=ColumnsList;

  Q1.DataBase:=dmData.MainCon;
  Q4.DataBase := dmData.MainCon;
  tr.DataBase := dmData.MainCon;

  dmData.InsertProfiles(cmbProfiles,False);
  cmbProfiles.Text := dmData.GetDefaultProfileText;
  try
    tmp := FormatSettings.TimeSeparator;
    FormatSettings.TimeSeparator := '_';
    ERR_FILE := 'errors_'+TimeToStr(now)+'.log';
  finally
    FormatSettings.TimeSeparator := tmp;
  end;

  OldQSO_Count:=GetQSOCount;
  WrongCabrilloLines:=TStringList.Create;
end;

procedure TfrmCabrilloImport.buttonNextClick(Sender: TObject);
begin
  case pageControlSteps.ActivePageIndex of
    0: begin
      if not ReadQSOFields then exit;
      DoImport;
      labelResult.Caption:=Format(IMPORT_RESULT_NEW_QSOS, [GetQSOCount - OldQSO_Count]);
      pageControlSteps.ActivePageIndex:=pageControlSteps.ActivePageIndex + 1;
      buttonNext.Caption:=BUTTON_READY;
    end;
    1: begin
      ModalResult:=mrOK;
      Close;
    end;
  end;
end;

procedure TfrmCabrilloImport.checkBoxReplacePHWithChange(Sender: TObject);
begin
  editReplacePHWith.Enabled:=checkBoxReplacePHWith.Checked;
end;

procedure TfrmCabrilloImport.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ColumnsList);
  FreeAndNil(QSOIter);
  WrongCabrilloLines.Free;
end;

procedure TfrmCabrilloImport.FormShow(Sender: TObject);
var
  Lines: TStringList;
begin
  Lines:=TStringList.Create;
  try
    Lines.LoadFromFile(CabrilloFilename);

    ClearGridRows(gridHeader);
    QSOIter.Lines.Clear;
    LoadCabrilloLines(Lines);
    lblCount.Caption:=IntToStr(QSOIter.Lines.Count);
  finally
    Lines.Free;
  end;

  ColumnsList.ClearColumns;
  GuessQSORows;
  GridSetSentRcvd;
  synEditColumns.Text:=ColumnsList.SaveToColumnsString;
  ReloadQSOGrid;
  gridQSOs.AutoSizeColumns;
  CheckIfCorrectionsNeeded;

  pageControlSteps.ShowTabs:=false;
  dmUtils.LoadFontSettings(Self);

  CheckForDuplicateColumns;
  ApplyLrsGridWorkaround;
end;

procedure TfrmCabrilloImport.synEditColumnsChange(Sender: TObject);
begin
  TimerCount:=0;
  TimerParseCols.Enabled:=true;
end;

procedure TfrmCabrilloImport.TimerParseColsTimer(Sender: TObject);
begin
  Inc(TimerCount);
  if TimerCount>=10 then begin
    TimerParseCols.Enabled:=false;
    if ParseColumnsString then
      ReloadQSOGrid;
  end;
end;

function TfrmCabrilloImport.GetCabrilloFilename: string;
begin
  Result:=lblFileName.Caption;
end;

procedure TfrmCabrilloImport.LoadCabrilloLines(const Lines: TStringList);
const
  CABRILLO_PARTS: array [0..2] of string = ('START-OF-LOG', 'QSO', 'END-OF-LOG');
var
  i, p: Integer;
  Param, Value: string;
begin
  for i:=0 to Lines.Count-1 do begin
    p:=Pos(':', Lines[i]);
    if p>0 then begin
      Param:=Copy(Lines[i], 1, p-1); //without ':'
      Value:=Copy(Lines[i], p+2, Length(Lines[i])); //without ': '
      p:=AnsiIndexStr(Param, CABRILLO_PARTS);
      if p=1 then
        QSOIter.Lines.Add(Value)
      else if p<0 then
        AddToHeaderGrid(Param, Value);
    end;
  end;
end;

procedure TfrmCabrilloImport.AddToHeaderGrid(Param, Value: string);
begin
  with gridHeader do begin
    AddRowToGrid(gridHeader);
    Cells[FixedCols    , RowCount-1]:=Param;
    Cells[FixedCols + 1, RowCount-1]:=Value;
  end;
end;

procedure TfrmCabrilloImport.GridSetSentRcvd;
const
  COLUMNS: array [0..2] of string = ('Call', 'Rst', 'Exch');
var
  i, nCols, Index: Integer;
  arColumnsUsed: array [0..2] of byte;
  Title: string;
begin
  for i:=Low(arColumnsUsed) to High(arColumnsUsed) do
    arColumnsUsed[i]:=0;

  nCols:=ColumnsList.ColumnsCount;
  for i:=0 to nCols-1 do begin
    Title:=ColumnsList.Columns[i].Name;
    Index:=AnsiIndexStr(Title, COLUMNS);
    if Index>=0 then begin
      ColumnsList.SetColumnTitle(i, Title + IfThen(arColumnsUsed[Index]=0, '_s', '_r'));
      Inc(arColumnsUsed[Index]);
    end;
  end;
end;

procedure TfrmCabrilloImport.ReloadQSOGrid;
var
  i, nCols: Integer;
  q, p: integer;
  arFieldLengths: array of integer;
begin
  if not Self.Visible then
    exit; //abort if hidden

  with gridQSOs do begin
    BeginUpdate;
    ClearGridRows(gridQSOs);
    nCols:=ColumnsList.ColumnsCount;
    SetLength(arFieldLengths, nCols);
    ColCount:=FixedCols + nCols;
    for i:=0 to nCols-1 do begin
      Cells[FixedCols+i, 0]:=ColumnsList.Columns[i].Name;
      arFieldLengths[i]:=ColumnsList.Columns[i].MaxLength;
    end;

    for q:=0 to QSOIter.Lines.Count-1 do begin
      AddRowToGrid(gridQSOs);
      p:=1;
      for i:=0 to nCols-1 do begin
        Cells[FixedCols+i, RowCount-1]:=Copy(QSOIter.Lines[q], p, arFieldLengths[i]);
        p+=arFieldLengths[i]+1; //+1: space
      end;
    end;
    EndUpdate;
  end;
end;

procedure TfrmCabrilloImport.CheckIfCorrectionsNeeded;
var
  i, nCols: Integer;
  j: integer;
begin
  //Replace mode PH
  nCols:=ColumnsList.ColumnsCount;
  for i:=0 to nCols-1 do begin
    if ColumnsList.Columns[i].Name='Mo' then BEGIN
      with gridQSOs do begin
        for j:=FixedRows to RowCount-1 do begin
          if Cells[i, j]='PH' then BEGIN
            checkBoxReplacePHWith.Checked:=true;
            editReplacePHWith.Text:='FM';
            break;
          END;
        end;
      end;
      break;
    END;
  end;
end;

function TfrmCabrilloImport.CheckForDuplicateColumns: boolean;
var
  i, nCols, DupIndex: Integer;
  DupPos: array [1..2] of integer;
  arUsedColumns: array of string;
  c, ErrorMsg: string;
begin
  Result:=true;
  DupIndex:=-1;
  ErrorMsg:=COL_FORMAT_ERRORS_FOUND + LineEnding;

  nCols:=ColumnsList.ColumnsCount;
  SetLength(arUsedColumns, nCols);
  for i:=0 to nCols-1 do
    arUsedColumns[i]:='';

  for i:=0 to nCols-1 do begin
    c:=ColumnsList.Columns[i].Name;
    if AnsiIndexStr(c, arUsedColumns)>0 then begin
      ErrorMsg += Format(DUPLICATE_COLUMN, [c, ColumnsList.Columns[i].MaxLength]) + LineEnding;
      if DupIndex<0 then DupIndex:=i; //assign only for the first duplicate found
    end;
    arUsedColumns[i]:=c;
  end;

  if DupIndex>=0 then begin
    //Highlight the duplicate in SynEdit
    c:=ColumnsList.Columns[DupIndex].Name + ':' + IntToStr(ColumnsList.Columns[DupIndex].MaxLength);
    DupPos[1]:=Pos(c, synEditColumns.Text);
    DupPos[2]:=PosEx(c, synEditColumns.Text, DupPos[1]+1); //2nd entry
    i:=ifthen(DupPos[2]>0, DupPos[2], DupPos[1]);
    synEditColumns.SelStart:=i;
    synEditColumns.SelEnd:=i+Length(c);
    MessageDlg(Caption, ErrorMsg, mtWarning, [mbOK], 0);
    Result:=false;
  end;
end;

function TfrmCabrilloImport.CheckForMandatoryFields: boolean;
const
  MANDATORY_FIELDS: array [0..4] of string=('Freq', 'Mo', 'Date', 'Time', 'Call_r');
var
  F: string;
  MissingFields: TStringList;
begin
  MissingFields:=TStringList.Create;
  try
    for F in MANDATORY_FIELDS do begin
      if not QSOIter.Fields.IndexOf(F)>=0 then
        MissingFields.Add(F);
    end;

    Result:=(MissingFields.Count=0);
    if not Result then begin
      MissingFields.Delimiter:=',';
      MessageDlg(Caption, Format(MANDATORY_FIELDS_MISSING, [MissingFields.DelimitedText]), mtWarning, [mbOk], 0);
    end;
  finally
    MissingFields.Free;
  end;
end;

function TfrmCabrilloImport.ParseColumnsString: boolean;
var
  Err: string;
begin
  Result:=ColumnsList.LoadFromColumnsString(synEditColumns.Text, Err);
  if not Result then
    MessageDlg(Caption, Format(ERROR_PARSING_TEXT, [Err]), mtWarning, [mbOK], 0);
end;

procedure TfrmCabrilloImport.DoImport;
var
  d: TnewQSOEntry;
  ErrorMessage: string;
begin
  GlobalProfile := dmData.GetNRFromProfile(cmbProfiles.Text);
  WrongRecNr := 0;
  WrongCabrilloLines.Clear;

  if not tr.Active then
     tr.StartTransaction;
  Q1.Active:=true;
  try
    while QSOIter.MoveNext do begin
      QSOIter.ParseCurrent;
      if MakeRecord(d, ErrorMessage) then
        AddNewRecord(d) else
      begin
        WrongCabrilloLines.Add(CBR_QSO + QSOIter.CurrentLine);
        WrongCabrilloLines.Add(ErrorMessage);
        WrongCabrilloLines.Add('');
      end;
    end;

    Q1.ApplyUpdates;
    tr.Commit;
  except
    on EA: EUserAborted do begin
      DebugLn('CabrilloImport: ' + EA.Message);
      tr.Rollback;
    end;
    on E: Exception do
    begin
      MessageDlg(Caption, 'Import failed!' + LineEnding + E.Message, mtError, [mbOK], 0);
      tr.Rollback;
    end;
  end;
  lblErrors.Caption   := IntToStr(WrongRecNr);
  if WrongRecNr>0 then begin
    lblErrors.Font.Color := clRed;
    InsertHeaderFromFile(WrongCabrilloLines);
    WrongCabrilloLines.SaveToFile(dmData.UsrHomeDir + ERR_FILE);
    lblErrorLog.Caption := dmData.UsrHomeDir + ERR_FILE;
    lblErrorLog.Visible := true;
  end;
end;

procedure TfrmCabrilloImport.InsertHeaderFromFile(var IntoList: TStringList);
var
  Lines: TStringList;
  i: Integer;
begin
  Lines:=TStringList.Create;
  try
    Lines.LoadFromFile(CabrilloFilename);
    i:=0;
    while (i<Lines.Count) and (not AnsiStartsStr(CBR_QSO, Lines[i])) do begin
      IntoList.Insert(i, Lines[i]);
      Inc(i);
    end;
    IntoList.Add('END-OF-LOG:');
  finally
    Lines.Free;
  end;
end;

function TfrmCabrilloImport.MakeRecord(out d: TnewQSOEntry; var ErrorMessage: string): boolean;
var
  Freq: double;
  Mode: string;
begin
  Result:=true;
  d:=Default(TnewQSOEntry);
  with QSOIter do begin
    Mode:=Fields['Mo'];
    if ReplaceModePH and (Mode='PH') then
      Mode:=editReplacePHWith.Text;

    //Apply Freq multiplier
    if not TryStrToFloat(NormalizeDecimalSeparator(Fields['Freq']), Freq) then begin
      ErrorMessage:=Format(WRONG_QSO_FREQ, [Fields['Freq']]);
      inc(WrongRecNr);
      exit(false);
    end;
    d.FREQ:=FloatToStr(Freq * FreqMultiplier);
    d.MODE:=Mode;
    d.QSO_DATE:=Fields['Date'];
    d.TIME_ON:=Fields['Time'];
    //'Call_s'
    if Fields.IndexOf('Rst_r')>=0 then
      d.RST_SENT:=Fields['Rst_r'];
    if Fields.IndexOf('Exch_s')>=0 then
      d.EXCH1:=Fields['Exch_s'];
    d.CALL:=Fields['Call_r'];
    if Fields.IndexOf('Rst_r')>=0 then
      d.RST_RCVD:=Fields['Rst_r'];
    if Fields.IndexOf('Exch_r')>=0 then
      d.EXCH2:=Fields['Exch_r'];
  end;

  if not dmUtils.IsAdifOK(d.QSO_DATE,d.TIME_ON,d.TIME_OFF,d.CALL,d.FREQ,d.MODE,d.RST_SENT,
                            d.RST_RCVD,d.IOTA,d.ITUZ,d.CQZ,d.GRIDSQUARE,d.MY_GRIDSQUARE,
                            d.BAND,ErrorMessage) then
  begin
    inc(WrongRecNr);
    Result:=false;
  end;
end;

procedure TfrmCabrilloImport.AddNewRecord(var d: TnewQSOEntry);
const
  F_FIELD = 'field';
var
  freq, band: String;
  profile    : String;
  dxcc_adif  : Integer;

  procedure FormatFields;
  var
    MyPower : String;
    MyLoc   : String;
    pAr : TExplodeArray;
    pProf : String;
    pLoc : String;
    pQTH : String;
    pEq  : String;
    pNote : String;
    dxcc,id_waz,id_itu : String;
    tmp,mycont : String;
    len        : Integer=0;
  begin
    MyPower := cqrini.ReadString('NewQSO','PWR','5 W');
    MyLoc   := cqrini.ReadString('Station','LOC','');

    if not dmUtils.IsLocOK(d.MY_GRIDSQUARE) then
      d.MY_GRIDSQUARE := MyLoc;

    d.TIME_ON := copy(d.TIME_ON,1,2) + ':' + copy(d.TIME_ON,3,2);
    if d.TIME_OFF <> '' then
      d.TIME_OFF := copy(d.TIME_OFF,1,2) + ':' + copy(d.TIME_OFF,3,2)
    else
      d.TIME_OFF := d.TIME_ON;

    if edtRemarks.Text <> '' then
      d.COMMENT := edtRemarks.Text + ' ' + d.COMMENT;
    if d.TX_PWR = '' then
      d.TX_PWR := MyPower;

    if GlobalProfile > 0 then
    begin
       profile := IntToStr(GlobalProfile)
    end
    else begin
      if d.APP_CQRLOG_PROFILE <> '' then
      begin
        pAr := dmUtils.Explode('|',d.APP_CQRLOG_PROFILE);
        len := Length(pAr);
        if pAr[0] <> '0' then
        begin
          pProf := pAr[0];
          profile := pAr[0];
          if len > 2 then
            pLoc  := pAr[1];
          if Len > 3 then
            pQTH  := pAr[2];
          if len > 4 then
            pEq   := pAr[3];
          if len > 5 then
            pNote := pAr[4];

          Q4.Close;
          Q4.SQL.Text := 'SELECT nr FROM profiles WHERE locator='+QuotedStr(pLoc) +
                         ' and qth='+QuotedStr(pQTH)+' and rig='+QuotedStr(pEq) +
                         ' and remarks='+QuotedStr(pNote);
          if dmData.DebugLevel >=1 then Writeln(Q4.SQL.Text);
          Q4.Open;
          if Q4.Fields[0].AsInteger = 0 then
          begin
            Q4.Close();
            Q4.SQL.Text := 'select nr from profiles where nr = '+pProf;
            if dmData.DebugLevel >=1 then Writeln(Q4.SQL.Text);
            Q4.Open();
            if (Q4.Fields[0].AsInteger > 0) then //if profile with this number doesnt exists,
            begin                           //we can save the number
              Q4.Close();
              Q4.SQL.Text := 'select max(nr) from profiles';
              if dmData.DebugLevel >=1 then Writeln(Q4.SQL.Text);
              Q4.Open();
              pProf := IntToStr(Q4.Fields[0].AsInteger+1)
            end;
            Q4.Close;
            Q4.SQL.Text := 'insert into profiles (nr,locator,qth,rig,remarks,visible) values ('+
                           ':nr,:locator,:qth,:rig,:remarks,:visible)';
            Q4.Prepare;
            Q4.Params[0].AsString  := pProf;
            Q4.Params[1].AsString  := pLoc;
            Q4.Params[2].AsString  := pQTH;
            Q4.Params[3].AsString  := pEq;
            Q4.Params[4].AsString  := pNote;
            Q4.Params[5].AsInteger := 1;

            {
            Q4.SQL.Text := 'insert into profiles (nr,locator,qth,rig,remarks,visible) values (' +
                           pProf+','+QuotedStr(pLoc)+','+QuotedStr(pQTH)+','+QuotedStr(pEq)+','+
                           QuotedStr(pNote)+',1)';
            }
            if dmData.DebugLevel >=1 then Writeln(Q4.SQL.Text);
            Q4.ExecSQL;
            Q4.Close();
          end
        end
        else
          profile := '0'
      end
      else
        profile := '0'
    end;

    freq := FormatFloat('0.0000;;',StrToFloat(d.FREQ));
    band := dmUtils.GetBandFromFreq(d.FREQ);

    dxcc_adif := dmDXCC.id_country(d.CALL,dmUtils.StrToDateFormat(d.QSO_DATE),dxcc,mycont,tmp,id_waz,tmp,id_itu,tmp,tmp);
    if d.CQZ = '' then
      d.CQZ := id_waz;
    if d.ITUZ = '' then
      d.ITUZ := id_itu;
    if (d.CONT = '') or (d.CONT<>'EU') or (d.CONT<>'AS') or (d.CONT<>'AF') or (d.CONT<>'NA') or (d.CONT<>'SA') or
       (d.CONT <> 'OC') or (d.CONT<>'AN') then
       d.CONT := mycont;
  end;

begin
  FormatFields;

  if Q1.Locate('qsodate;time_on;callsign', VarArrayOf([d.QSO_DATE, d.TIME_ON, d.CALL]), []) then begin
    if Application.MessageBox('It looks like this QSOs are in the log.'#13'Do you really want to inport it again?',
                              'Question',MB_ICONQUESTION + MB_YESNO) = idNo then
    begin
      raise EUserAborted.Create('Duplicate QSO - import aborted');
    end
  end;

  Q1.Append;

  with Q1 do begin
    FieldValues[F_QSODATE]  := d.QSO_DATE;
    FieldValues[F_TIME_ON]  := d.TIME_ON;
    FieldValues[F_TIME_OFF] := d.TIME_OFF;
    FieldValues[F_CALLSIGN] := d.CALL;
    FieldValues[F_FREQ]     := StrToFloat(freq);
    FieldValues[F_MODE]     := d.MODE;
    FieldValues[F_RST_S]    := d.RST_SENT;
    FieldValues[F_RST_R]    := d.RST_RCVD;
    FieldValues[F_NAME]     := d.NAME;
    FieldValues[F_QTH]      := d.QTH;
    FieldValues[F_QSL_S]    := d.QSL_SENT;
    FieldValues[F_QSL_R]    := d.QSL_RCVD;
    FieldValues[F_QSL_VIA]  := d.QSL_VIA;
    FieldValues[F_IOTA]     := d.IOTA;
    FieldValues[F_PWR]      := d.TX_PWR;
    if Length(d.ITUZ)>0 then
      FieldValues[F_ITU]    := d.ITUZ;
    if Length(d.CQZ)>0 then
      FieldValues[F_WAZ]    := d.CQZ;
    FieldValues[F_LOC]      := d.GRIDSQUARE;
    FieldValues[F_MY_LOC]   := d.MY_GRIDSQUARE;
    FieldValues[F_REMARKS]  := d.COMMENT;
    FieldValues[F_ADIF]     := dxcc_adif;
    FieldValues[F_IDCALL]   := dmUtils.GetIDCall(d.CALL);
    FieldValues[F_AWARD]    := d.AWARD;
    FieldValues[F_BAND]     := band;
    FieldValues[F_STATE]    := d.STATE;
    FieldValues[F_CONT]     := UpperCase(d.CONT);
    FieldValues[F_PROFILE]  := StrToInt(profile);

    {FieldValues[F_LOTW_QSLS] := '';
    FieldValues[F_LOTW_QSLR] := '';
    FieldValues[F_EQSL_QSLSDATE] := '';
    FieldValues[F_EQSL_QSLRDATE] := '';}

    FieldValues[F_EXCH1] := d.EXCH1;
    FieldValues[F_EXCH2] := d.EXCH2;
  end;
  Q1.Post;
end;

procedure TfrmCabrilloImport.ApplyLrsGridWorkaround;
begin
  //TODO: try without workaround after lrs/LResources are removed
  gridHeader.Options:=gridHeader.Options + [goColSizing] - [goHorzLine];
  gridQSOs.Options:=gridQSOs.Options + [goColSizing];
end;

procedure TfrmCabrilloImport.InitQSOLinesIterator;
var
  i, nCols: Integer;
begin
  nCols:=ColumnsList.ColumnsCount;
  SetLength(QSOIter.FieldLengths, nCols);

  QSOIter.Fields.Clear;
  for i:=0 to nCols-1 do begin
    QSOIter.Fields[ColumnsList.Columns[i].Name]:=''; //create keys with empty data
    QSOIter.FieldLengths[i]:=ColumnsList.Columns[i].MaxLength;
  end;
end;

function TfrmCabrilloImport.ReadQSOFields: boolean;
begin
  if not ParseColumnsString then exit(false);
  InitQSOLinesIterator;
  if not (CheckForDuplicateColumns and CheckForMandatoryFields) then
    exit(false);

  if not TryStrToFloat(editFreqMultiplier.Text, FreqMultiplier) then begin
    MessageDlg(Caption, Format(FREQ_MULT_NOT_VALID_FLOAT, [editFreqMultiplier.Text]), mtWarning, [mbOK], 0);
    exit(false);
  end;
  ReplaceModePH:=checkBoxReplacePHWith.Checked;

  Result:=true;
end;

procedure TfrmCabrilloImport.ClearGridRows(var Grid: TStringGrid);
var
  i: Integer;
begin
  with Grid do begin
    RowCount:=FixedRows+1;

    for i:=FixedCols to ColCount-1 do
      Cells[i, RowCount-1]:='';
  end;
end;

procedure TfrmCabrilloImport.AddRowToGrid(var Grid: TStringGrid);
begin
  with Grid do
    if Length(Cells[FixedCols, RowCount-1])>0 then //last row is not empty
      RowCount:=RowCount+1;
end;

function TfrmCabrilloImport.GetQSOCount: integer;
var
  TrActive: Boolean;
begin
  Q4.Close;
  Q4.SQL.Text := 'SELECT COUNT(*) FROM cqrlog_main';
  TrActive:=tr.Active;
  if not TrActive then
    tr.StartTransaction;
  Q4.Open;
  Result:=Q4.Fields[0].AsInteger;
  if not TrActive then
    tr.RollBack;
  Q4.Close
end;

initialization
  {$I fCabrilloImport.lrs}

end.

