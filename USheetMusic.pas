unit USheetMusic;

interface

uses
  SysUtils, System.Zip,
  UMyMemoryStream;

type
  TTieStatus = (tieOff, tieStart, tieMitte, tieStop);

  TSaveRec = record
    iEvent: integer;
    iEnd: integer;
    hasEvenGriff: boolean;
    hasSame: boolean;
    nextSame: boolean;
    t, t1: integer;
    tupletNr: integer;
    InPu: boolean;
    triole: integer;
    Len: integer;
    slen, sDur: string;
    dot: boolean;
    tie:  TTieStatus;
    tieFractions: string;  // z.B. 1/2
    tieMeasures: integer;

    Takt, TaktNr: integer;
    MeasureNr: integer;
    Offset: integer;
    MostRight: integer;
    t32takt: integer;
    appoggiatura: TTieStatus;
    appoggNr: integer;

    Aufrunden: boolean;

    procedure Clear;
    function LimitToTakt: boolean;
    function Rest(Delta: integer; UseIf: boolean): boolean;
    function SaveLen(Delta: integer): boolean;
    function GetLenS(var t32: integer; quarterNote: integer): string;
  end;


function GetLen(var t32: integer; var dot: boolean; t32Takt: integer): string;

implementation

uses
  UXmlNode, UXmlParser, UEventArray, UMyMidiStream;


function GetLen(var t32: integer; var dot: boolean; t32Takt: integer): string;
var
  val: integer;
begin
  dot := false;
  val := GetLen_(t32, Dot, t32takt);
  if val = 0 then
    result := '?'
  else
    result := GetFraction_(32 div val);
end;

function TSaveRec.GetLenS(var t32: integer; quarterNote: integer): string;
var
  t, t1: integer;
begin
  t := 8*takt div quarterNote - t32Takt;
  if (t32 > t) and (t > 0) then
  begin
    t := 8*takt div quarterNote - t32Takt;
    t1 := t;
    result := GetLen(t, dot, t32Takt);
    dec(t32, t1 - t);
  end else
    result := GetLen(t32, dot, t32Takt);
end;

procedure TSaveRec.Clear;
begin
  iEvent := 0;
  iEnd := 0;
  hasEvenGriff := false;
  hasSame := false;
  nextSame := false;
  t := 0;
  t1 := 0;

  tupletNr := 0;
  InPu := false;
  triole := 0;
  Len := 0;
  slen := '';
  sDur := '';
  dot := false;
  tie := tieOff;
  tieFractions := '';
  tieMeasures := 0;

  Takt := 0;
  TaktNr := 0;
  MeasureNr := 1;
  Offset := 0;
  MostRight := 0;
  t32takt := 0;
  appoggiatura := tieOff;
  appoggNr := 0;

  Aufrunden := false;
end;

function TSaveRec.LimitToTakt: boolean;
begin
  if Len < 0 then
    result := false
  else
  result := (offset div Takt) <> ((offset + Len) div Takt);
  if result then
    Len := takt * ((offset div Takt) + 1) - offset;
end;

function TSaveRec.Rest(Delta: integer; UseIf: boolean): boolean;
begin
  result := (Delta > 0) and not UseIf;
  tie := tieOff;
  Len := Delta;
  if LimitToTakt then
    result := true;
end;

function TSaveRec.SaveLen(Delta: integer): boolean;
begin
  result := Delta > 0;
  Len := Delta;
end;


////////////////////////////////////////////////////////////////////////////////


end.
