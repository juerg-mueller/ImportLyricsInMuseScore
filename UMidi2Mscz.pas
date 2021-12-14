unit UMidi2Mscz;
// Test Midi files: http://midkar.com
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ShellApi, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SaveDialog1: TSaveDialog;
    cbxCodePage: TComboBox;
    Label4: TLabel;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function Merge(FileName: string): boolean;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$define _TEST}

uses
  UMyMidiStream, UMidiDataStream, UEventArray, UXmlParser, UXmlNode;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  i: integer;
  ext: string;
begin
  inherited;

  DropH := Msg.Drop;
  try
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    for i:= 0 to DroppedFileCount-1 do
    begin
      FileNameLength := DragQueryFile(DropH, i, nil, 0);
      SetLength(FileName, FileNameLength);
      DragQueryFile(DropH, i, PChar(FileName), FileNameLength + 1);
      ext := LowerCase(ExtractFileExt(Filename));
      if (ext = '.mid') or
         (ext = '.mscz') or
         (ext = '.mscx') then
      begin
        Merge(FileName);
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result := 0;
end;


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

procedure ReduceToLyrics(var Events: TMidiEventArray; Karaoke: boolean; Header: TDetailHeader);
var
  i, k, len: integer;
begin
  i := 1;
  k := 1;
  // remove all but lyrics, texts, and measure changes
  while i < Length(Events) do
  begin
    if (Events[i].command = $ff) and (Events[i].d1 in [1, 5, 88]) then
    begin
      Events[k] := Events[i];
//      Header.SetTimeSignature(Events[k], Events[k].Bytes);
      inc(k);
    end else begin
      inc(Events[k-1].var_len, Events[i].var_len);

    end;
    inc(i);
  end;
  SetLength(Events, k);

  i := 1;
  while i < Length(Events) do
  begin
    if (Events[i].command = $ff) and
       (Events[i].d1 in [1, 5]) then
    begin
      if Karaoke and (Events[i].d1 = 1) then
        Events[i].d1 := 5;

      if (Events[i].d1 <> 5) or
         ((i < 10) and (Events[i][0] = '@')) then
      begin
        inc(Events[i-1].var_len, Events[i].var_len);
        for k := i to Length(Events)-2 do
          Events[k] := Events[k+1];
        SetLength(Events, Length(Events)-1);
      end else
        inc(i);
    end else
      inc(i);
  end;
  i := 2;
  while i < Length(Events) do
  begin
    if (Events[i].d1 <> 5) then
    else begin
      if Events[i][0] in [#10, #13] then
      begin
        if Events[i][0] = #10 then
          Events[i][0] := '\'
        else
          Events[i][0] := '/';
      end;
      if (Events[i][0] = ' ') then
      begin
        Events[i-1].AppendByte(ord(' '));
        with Events[i] do
        begin
          for k := 1 to Length(Bytes)-1 do
            Bytes[k-1] := Bytes[k];
          SetLength(Bytes, Length(Bytes)-1);
        end;
      end else
      if (Events[i][0] in ['/', '\']) and
         (Events[i-1][Length(Events[i-1].Bytes)-1] <> ' ') then
        Events[i-1].AppendByte(ord(' '));
    end;
    inc(i);
  end;

  // letzten Takt füllen
  len := 0;
  for i := 0 to Length(Events)-1 do
    inc(len, Events[i].var_len);
  if Length(Events) > 2 then
  begin
    len := len mod Header.TicksPerMeasure;
    if len = 0 then
    begin
      if Events[Length(Events)-1].var_len = 0 then
        Events[Length(Events)-1].var_len := Header.TicksPerMeasure;
    end else
      inc(Events[Length(Events)-1].var_len, Header.TicksPerMeasure -len);
  end;
end;

function BuildLyricsTree(var LyricsTree: KXmlNode; const Events: TMidiEventArray;
                         Header: TDetailHeader; CodePage: integer): boolean;
var
  iEvent, iEnd: integer;
  offset, midiOffset: integer;
  delta, no: integer;
  t, t1: integer;
  t32takt: double;
  dot: boolean;
  sLen, s: string;
  Voice, Rest, Child: KXmlNode;
  NextLyrics: boolean;
  InTuplet: boolean;
  TaktNr: integer;
  as_: AnsiString;
  l: integer;

  procedure AddRest(var InTuplet: boolean);
  var
    IsTuplet: boolean;
    Tuplet, Child, Child1, BaseNote: KXmlNode;
  begin
    BaseNote := nil;
    IsTuplet := (Header.DeltaTimeTicks mod (3*delta div 2)) < 2;
    if IsTuplet <> InTuplet then
    begin
      if IsTuplet then
      begin
        Tuplet := Voice.AppendChildNode('Tuplet');
        Child := Tuplet.AppendChildNode('normalNotes');
        Child.Value := '2';
        Child := Tuplet.AppendChildNode('actualNotes');
        Child.Value := '3';
        BaseNote := Tuplet.AppendChildNode('baseNote');
        Child := Tuplet.AppendChildNode('Number');
        Child1 := Child.AppendChildNode('style');
        Child1.Value := 'Tuplet';
        Child1 := Child.AppendChildNode('text');
        Child1.Value := '3';
      end else
        Voice.AppendChildNode('endTuplet');
      InTuplet := IsTuplet;
    end;

    if IsTuplet then
      delta := 3*delta div 2;
    t := 8*delta div Header.DeltaTimeTicks;
    t1 := t;
    sLen := GetLen(t, dot, round(t32takt));
    Rest := Voice.AppendChildNode('Rest');
    Rest.AppendChildNode('visible', '0');
    Child := Rest.AppendChildNode('durationType', sLen);
    if BaseNote <> nil then
      BaseNote.Value := sLen;
    if dot then
      Rest.AppendChildNode('dots', '1');
    t := t1-t;
    if IsTuplet then
      t32takt := t32takt - 2.0*t/ 3.0
    else
      t32takt := t32takt - t;
    t := Header.DeltaTimeTicks*t div 8;
    if IsTuplet then
      t := 2*t div 3;
    inc(offset, t);
    if (t1 = t) and result then
    begin
      result := false; // nicht quantisiert
//      Application.MessageBox('Sorry, maybe your midi file is not quantizied!', 'Error');
    end;
  end;

begin
  result := false;
  LyricsTree := KXmlNode.Create;
  LyricsTree.Name := 'LyricsTree';
  if Length(Events) = 0 then
    exit;

  result := true;
  TaktNr := 0;
  iEvent := 1;
  offset := 0;
  t32takt := 0;
  InTuplet := false;
  midiOffset := Events[0].var_len;
  Voice := nil;
  while result and (iEvent < Length(Events)) do
  begin
    if Header.SetTimeSignature(Events[iEvent], Events[iEvent].Bytes) then
    begin
      t32takt := 0;
      inc(iEvent);
      if iEvent = Length(Events) then
        break;
    end;
    if round(t32takt) = 0 then
    begin
      Voice := LyricsTree.AppendChildNode('voice');
      t32takt := 32.0*Header.measureFact / Header.measureDiv;
      inc(TaktNr);
      Voice.Attributes['id'] := IntToStr(TaktNr);
    end;

    delta := midiOffset - offset;
    iEnd := iEvent;
    NextLyrics := delta = 0;
    //  Lyrics an derselben Stelle ermitteln
    while (delta = 0) and (iEnd < Length(Events)) do
    begin
      if Header.GetRaster(Events[iEnd].var_len) = 0 then
        inc(iEnd)
      else
        delta := Header.GetRaster(Events[iEnd].var_len);
    end;
    if iEnd = Length(Events) then
      dec(iEnd);
    // Taktgrenze prüfen
    if Header.MeasureRestTicks(t32takt) < delta then
      delta := Header.MeasureRestTicks(t32takt);

    AddRest(InTuplet);
    if not result then
    begin
      result := true;
      repeat
        // skip measure
        delta := Header.MeasureRestTicks(t32takt);
        AddRest(InTuplet);
      until not Result or (Header.MeasureRestTicks(t32takt) <= 0);
      while (iEvent < Length(Events)) and
            (offset > midiOffset) do
      begin
        inc(midiOffset, Header.GetRaster(Events[iEvent].var_len));
        inc(iEvent);
      end;
    end else
    if NextLyrics then
    begin
      no := 0;
      while iEvent <= iEnd do
      begin
        s := Events[iEvent].code[CodePage];
        if trim(s) <> '' then
        begin
          Child := Rest.AppendChildNode('Lyrics');
          if no > 0 then
            Child.AppendChildNode('no', IntToStr(no));
          if (Length(s) > 0) then
          begin
            if (s[Length(s)] = ' ') then
              SetLength(s, Length(s)-1)
            else
            if not (AnsiChar(s[Length(s)]) in [' ', '/', '\']) then
              Child.AppendChildNode('syllabic', 'begin');
          end;
          Child.AppendChildNode('text', s);
        end;
        inc(midiOffset, Header.GetRaster(Events[iEvent].var_len));
        inc(iEvent);
        inc(no);
      end;
    end;
  end;

  // letzten Takt füllen
  if result and (Voice <> nil) then
  begin
    while result and (Header.MeasureRestTicks(t32takt) > 0) do
    begin
      delta := Header.MeasureRestTicks(t32takt);
      InTuplet := false;
      AddRest(InTuplet);
    end;
    result := true;
  end;
end;

procedure AddVoice(FirstStaff: KXmlNode; const LyricsTree: KXmlNode; Header: TDetailHeader);
var
  iTree, iStaff: integer;
  FirstVoice: integer;
begin
  iStaff := 0;
  iTree := 0;
  while iStaff < FirstStaff.Count do
  begin
    if FirstStaff[iStaff].Name = 'Measure' then
    begin
      FirstVoice := 0;
      while (FirstVoice < FirstStaff[iStaff].Count-1) and (FirstStaff[iStaff][FirstVoice].Name <> 'voice') do
        inc(FirstVoice);
      if iTree < LyricsTree.Count then
      begin
        FirstStaff[iStaff].InsertChildNode(FirstVoice+1, LyricsTree[iTree].CopyTree);
        inc(iTree);
      end;
    end;
    inc(iStaff);
  end;
end;

function ReduceVoice(var FirstStaff: KXmlNode; const Header: TDetailHeader): boolean;
var
  StaffCopy: KXmlNode;
  iStaff: integer;
  i, iVoice: integer;
  iChord1, iChord2: integer;
  First: integer;
  Measure, Voice1, Voice2, Chord1, Chord2, Lyrics: KXmlNode;
  Voice1Copy: KXmlNode;
  Dur1, Dur2: integer;
  offset1, offset2: integer;
  InTuplet1, InTuplet2: boolean;
  lyricsNo, No: integer;

  function SearchNext(var iChord: integer; var InTuplet: boolean;
                      Voice: KXmlNode): boolean;
  begin
    if iChord < 0 then
      iChord := 0;
    while (iChord < Voice.Count) and
          (Voice[iChord].Name <> 'Rest') and (Voice[iChord].Name <> 'Chord') do
    begin
      if Voice[iChord].Name = 'Tuplet' then
        InTuplet := true
      else
      if Voice[iChord].Name = 'endTuplet' then
        InTuplet := false;
      inc(iChord);
    end;
    result := iChord < Voice.Count;
  end;

  function GetDur(Chord: KXmlNode): integer;
  var
    Child: KXmlNode;
    Duration: string;
    Dots: string;
  begin
    Child := Chord.HasChild('durationType');
    if Child <> nil then
      Duration := Child.Value
    else
      Duration := '';
    Child := Chord.HasChild('dots');
    if Child <> nil then
      Dots := Child.Value
    else
      Dots := '';

    result := Header.GetChordTicks(Duration, Dots);
  end;

begin
  result := true;
  StaffCopy := FirstStaff.CopyTree;

  iStaff := 0;
  offset1 := 0;
  offset2 := 0;
  InTuplet1 := false;
  InTuplet2 := false;
  while (iStaff < FirstStaff.Count) and result do
  begin
    Measure := FirstStaff[iStaff];
    if Measure.Name = 'Measure' then
    begin
      Voice1 := nil;
      Voice2 := nil;
      iVoice := 0;
      // beide Stimmen sind im gleichen Takt nacheinander
      while (iVoice < Measure.Count) and (Voice2 = nil) do
      begin
        if Measure[iVoice].Name = 'voice' then
        begin
          if Voice1 = nil then
            Voice1 := Measure[iVoice]   // 1. Stimme
          else
            Voice2 := Measure[iVoice];  // 2. Stimme
        end;
        inc(iVoice);
      end;
      if Voice2 = nil then
        break;
      iChord1 := 0;
      iChord2 := 0;
      Voice1Copy := Voice1.CopyTree;
      while result and
            (iChord1 < Voice1.Count) and (iChord2 < Voice2.Count) do
      begin
        result := SearchNext(iChord1, InTuplet1, Voice1) and
                  SearchNext(iChord2, InTuplet2, Voice2);
        if not result then
          break;

        Chord1 := Voice1[iChord1];
        Chord2 := Voice2[iChord2];
        First := Chord1.GetFirstIndex('Note');
        // Rest hat keine "Note"
        if First < 0 then
          First := Chord1.Count;
        Lyrics := Chord2.HasChild('Lyrics');
        if Lyrics <> nil then
        begin
          No := 0;
          for lyricsNo := 0 to Chord2.Count-1 do
          begin
            Lyrics := Chord2.ChildNodes[lyricsNo];
            if Lyrics.Name = 'Lyrics' then
            begin
              Chord1.InsertChildNode(First+no, Lyrics.CopyTree);
              inc(no);
            end;
          end;
        end;
        dur1 := GetDur(Chord1);
        if InTuplet1 then
          dur1 := 2*dur1 div 3;
        dur2 := GetDur(Chord2);
        if InTuplet2 then
          dur2 := 2*dur2 div 3;
        inc(offset1, dur1);
        inc(offset2, dur2);
        while (offset1 <> offset2) and
              result and
              (iChord1 < Voice1.Count) and (iChord2 < Voice2.Count) do
        begin
          while (iChord1 < Voice1.Count) and (offset1 < offset2) do
          begin
            inc(iChord1);
            if SearchNext(iChord1, InTuplet1, Voice1) then
            begin
              Chord1 := Voice1[iChord1];
              dur1 := GetDur(Chord1);
              if InTuplet1 then
                dur1 := 2*dur1 div 3;
              inc(offset1, dur1);
            end;
          end;
          while result and
                (iChord2 < Voice2.Count) and (offset1 > offset2) do
          begin
            inc(iChord2);
            if SearchNext(iChord2, InTuplet2, Voice2) then
            begin
              Chord2 := Voice2[iChord2];
              dur2 := GetDur(Chord2);
              if InTuplet2 then
                dur2 := 2*dur2 div 3;
              inc(offset2, dur2);
              if Chord2.HasChild('Lyrics') <> nil then
                result := false;
            end;
          end;
        end;
        inc(iChord1);
        inc(iChord2);
      end;
      if result then
      begin
        Measure.RemoveChild(Voice2);
        Voice1Copy.Free;
      end else begin
        Measure.RemoveChild(Voice1);
        Measure.InsertChildNode(0, Voice1Copy);
        result := true;
      end;

    end;
    inc(iStaff);
  end;

  if not result then
  begin
    for i := FirstStaff.Count-1 downto 0 do
      FirstStaff.RemoveChild(FirstStaff[i]);
    for i := 0 to StaffCopy.Count-1 do
    begin
      FirstStaff.AppendChildNode(StaffCopy[i]);
      StaffCopy.ChildNodes[i] := nil;
    end;
  end;
  StaffCopy.Free;
end;

function TForm1.Merge(FileName: string): boolean;
var
  IsKaraoke: boolean;
  KaraokeChannel: integer;
  Staffs: array of KXmlNode;
  events: TEventArray;
  Event: TMidiEvent;
  hasLyrics: array of integer;
  hasText: array of integer;
  hasSound: array of integer;
  Root: KXmlNode;
  Score, Staff, Measure, Chord, Voice, Lyrics: KXmlNode;
  Child, LyricsTree, FirstStaff: KXmlNode;
  Ext: string;
  i, k, iScore: integer;
  iStaff, iVoice, iChord, iMeasure: integer;
  MidiEvents: TMidiEventArray;
  Tracks: TTrackEventArray;
  CodePage: integer;
begin
  result := false;
  case cbxCodePage.ItemIndex of
    0: CodePage := CP_UTF8;
    1: CodePage := 28591; // Ansi-Code iso-8859-1
    else CodePage := CP_UTF8;
  end;
  Ext := LowerCase(ExtractFileExt(FileName));
  SetLength(FileName, Length(FileName) - Length(Ext));

  if not FileExists(FileName + '.mid') then
  begin
    Application.MessageBox(
      PChar(Format('File "%s.mid" does not exist!',
                   [FileName])), 'Error', MB_OK);
    exit;
  end;

  Events := TEventArray.Create;
  if not Events.LoadMidiFromFile(FileName + '.mid', true) then
  begin
    Application.MessageBox(
      PChar(Format('File "%s.mid" not read!', [FileName])), 'Error', MB_OK);
    Events.Free;
    exit;
  end;
{$ifdef TEST}
  Events.SaveSimpleMidiToFile(FileName + '.txt');
{$endif}
  Events.DetailHeader.smallestFraction := 16; // 16th
  SetLength(hasText, Events.TrackCount);
  SetLength(hasSound, Events.TrackCount);
  SetLength(hasLyrics, Events.TrackCount);
  KaraokeChannel := -1;
  IsKaraoke := false;
  for i := 0 to Events.TrackCount-1 do
  begin
    hasLyrics[i] := 0;
    hasSound[i] := 0;
    hasText[i] := 0;
    for k := 0 to Length(Events.Track[i])-1 do
    begin
      Event := Events.Track[i][k];
      if Event.command = $ff then
      begin
        if Event.d1 = 1 then
          inc(hasText[i])
        else
        if Event.d1 = 5 then
          inc(hasLyrics[i]);
      end else
      if Event.Event = 9 then
        inc(hasSound[i]);
    end;
    if (hasSound[i] = 0) and (hasText[i] > 10) and (hasLyrics[i] = 0) then
    begin
      IsKaraoke := true;
      KaraokeChannel := i
    end;
  end;
  Events.InsertMeasureChanges;

  if not FileExists(FileName + '.mscz') and
     not FileExists(FileName + '.mscx') then
  begin
    Application.MessageBox(
      PChar(Format('Neither the file "%s.mscz" nor the file "%s.mscx" exists!',
                   [FileName, FileName])), 'Error', MB_OK);
    exit;
  end;

  if (Ext = '.mscx') or (Ext = '.mscz') then
  begin
    if not KXmlParser.ParseFile(FileName + Ext, Root) then
      exit;
  end else
  if not KXmlParser.ParseFile(FileName + '.mscz', Root) and
     not KXmlParser.ParseFile(FileName + '.mscx', Root) then
    exit;

  Score := Root.ChildNodes[Root.Count-1];
  if (Score.Name <> 'Score') or
     not Score.GetChild('Staff', Staff) then
  begin
    Application.MessageBox('Error in MuseScore file!', 'Error');
    exit;
  end;

  // remove lyrics
  FirstStaff := nil;
  SetLength(Staffs, 0);
  for iScore := 0 to Score.Count-1 do
  begin
    Staff := Score[iScore];
    if Staff.Name = 'Staff' then
    begin
      if FirstStaff = nil then
        FirstStaff := Staff;
      SetLength(Staffs, Length(Staffs)+1);
      Staffs[Length(Staffs)-1] := Staff;
      for iStaff := 0 to Staff.Count-1 do
      begin
        Measure := Staff[iStaff];
        if Measure.Name = 'Measure' then
        begin
          for iMeasure := 0 to Measure.Count-1 do
          begin
            Voice := Measure[iMeasure];
            if Voice.Name = 'voice' then
            begin
              for iVoice := 0 to Voice.Count-1 do
              begin
                Chord := Voice[iVoice];
                if (Chord.Name = 'Chord') or (Chord.Name = 'Rest') then
                begin
                  iChord := 0;
                  while iChord < Chord.Count do
                  begin
                    Lyrics := Chord[iChord];
                    if Lyrics.Name = 'Lyrics' then
                    begin
                      Chord.RemoveChild(Lyrics)
                    end else
                      inc(iChord);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

  end;

  // Zu jedem Staff in MuseScore gibt es ein Staff in Score

  result := Length(Staffs) > 0;
  if result then
  begin
    if not IsKaraoke then
    begin
      for i := 0 to Length(hasLyrics)-1 do
      begin
        if result and (hasLyrics[i] > 10)  and (i < Length(Staffs)) then
        begin
          LyricsTree := nil;
          TEventArray.Quantize(Events.Track[i], Events.DetailHeader);
          ReduceToLyrics(Events.Track[i], true, Events.DetailHeader);
          result := BuildLyricsTree(LyricsTree, Events.Track[i], Events.DetailHeader, CodePage);
          if result then
          begin
            AddVoice(Staffs[i], LyricsTree, Events.DetailHeader);
            ReduceVoice(Staffs[i], Events.DetailHeader);
          end;
          LyricsTree.Free;
        end;
      end;
    end else begin
      LyricsTree := nil;
      TEventArray.Quantize(Events.Track[KaraokeChannel], Events.DetailHeader);
      ReduceToLyrics(Events.Track[KaraokeChannel], true, Events.DetailHeader);
      result := BuildLyricsTree(LyricsTree, Events.Track[KaraokeChannel], Events.DetailHeader, CodePage);
      if result then
      begin
{$ifdef TEST}
        LyricsTree.SaveToXmlFile(FileName + '_tree.xml');
{$endif}
        AddVoice(FirstStaff, LyricsTree, Events.DetailHeader);
        ReduceVoice(FirstStaff, Events.DetailHeader);
      end;
      LyricsTree.Free;
    end;
  end;
  if result then
  begin
    try
      if FileExists(FileName + '_.mscz') then
      begin
        if Application.MessageBox(PChar(Format('File "%s" exists. Overwrite it?', [FileName + '_.mscz'])),
                                  'Overwrite?', MB_YESNO) <> IDYES then
          exit;
      end;
      Root.SaveToMsczFile(FileName + '_.mscz');
{$ifdef TEST}
      Root.SaveToXmlFile(FileName + '_.xml', '<?xml version="1.0" encoding="UTF-8"?>'#13#10);
{$endif}
    finally
      Root.Free;
      Events.Free;
    end;
  end else
    Application.MessageBox('Sorry, error occured!', 'Error');
end;
{
procedure TForm1.Button1Click(Sender: TObject);
var
  FileName: string;
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    Merge(FileName);
  end;
end;
 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, true);
end;

end.


