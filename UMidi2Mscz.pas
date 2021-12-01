unit UMidi2Mscz;

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
  while i < Length(Events) do
  begin
    if (Events[i].command = $ff) then
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
{
  for i := Length(Events)-1 downto 1 do
    if Events[i].var_len < 4 then
    begin
      inc(Events[i-1].var_len, Events[i].var_len);
      Events[i].var_len := 0;
    end;}
end;

function BuildLyricsTree(var LyricsTree: KXmlNode; const Events: TMidiEventArray;
                         Header: TDetailHeader): boolean;
var
  iEvent, i, k, len: integer;
  offset, midiOffset: integer;
  delta, no: integer;
  t, t1, t32takt: integer;
  dot: boolean;
  sLen, s: string;
  Voice, Rest, Child: KXmlNode;
  NextLyrics: boolean;

  procedure AddRest;
  begin
    t := 8*delta div Header.DeltaTimeTicks;
    t1 := t;
    sLen := GetLen(t, dot, t32takt);
    Rest := Voice.AppendChildNode('Rest');
    Rest.AppendChildNode('visible', '0');
    Child := Rest.AppendChildNode('durationType', sLen);
    if dot then
      Rest.AppendChildNode('dots', '1');
    inc(offset, Header.DeltaTimeTicks*(t1-t) div 8);
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
  iEvent := 1;
  offset := 0;
  midiOffset := Events[0].var_len;
  Voice := nil;
  while result and (iEvent < Length(Events)) do
  begin
    if offset mod Header.TicksPerMeasure = 0 then
    begin
      Voice := LyricsTree.AppendChildNode('voice');
      t32takt := 0;
    end;
    delta := midiOffset - offset;
    i := iEvent;
    if i = 15 then
      i := i;
    NextLyrics := delta = 0;
    //  Lyrics an derselben Stelle ermitteln
    while (delta = 0) and (i < Length(Events)) do
    begin
      if Header.GetRaster(Events[i].var_len) = 0 then
        inc(i)
      else
        delta := Header.GetRaster(Events[i].var_len);
    end;
    if i = Length(Events) then
      dec(i);
    // Taktgrenze prüfen
    if Header.TicksPerMeasure < offset mod Header.TicksPerMeasure + delta then
      delta := Header.TicksPerMeasure - offset mod Header.TicksPerMeasure;

    AddRest;
    if not result then
    begin
      result := true;
      repeat
        // skip measure
        delta := Header.TicksPerMeasure - offset mod Header.TicksPerMeasure;
        if delta = 0 then
          delta := Header.TicksPerMeasure;
        AddRest;
      until not Result or ((offset mod Header.TicksPerMeasure) = 0);
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
      while iEvent <= i do
      begin
        s := UTF8ToString(Events[iEvent].ansi);
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
  if (Voice <> nil) then
    while (offset mod Header.TicksPerMeasure) > 0 do
    begin
      delta := Header.TicksPerMeasure - (offset mod Header.TicksPerMeasure);
      AddRest;
    end;
end;

procedure AddVoice(FirstStaff: KXmlNode; const LyricsTree: KXmlNode; Header: TDetailHeader);
var
  iTree, offset, midiOffset, delta, iStaff: integer;
  Measure, Staff, Rest, Voice, Chord, Child: KXmlNode;
  duration, Len, relOffset, no: integer;
  sLen, s: string;
  FirstVoice: integer;
begin
  iStaff := 0;
  iTree := 0;
  while iStaff < FirstStaff.Count do
  begin
    if FirstStaff[iStaff].Name = 'Measure' then
    begin
      relOffset := offset;
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

  function SearchNext(var iChord: integer; Voice: KXmlNode): boolean;
  begin
    if iChord < 0 then
      iChord := 0;
    while (iChord < Voice.Count) and
          (Voice[iChord].Name <> 'Rest') and (Voice[iChord].Name <> 'Chord') and
          (Voice[iChord].Name <> 'Tuplet') do
      inc(iChord);
    result := iChord < Voice.Count;
    if result then
    begin
      result := Voice[iChord].Name <> 'Tuplet';
      if not result then
//        Application.MessageBox('Sorry, tuplets are not implemented yet!', 'Error');
    end;
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
  while (iStaff < FirstStaff.Count) and result do
  begin
    Measure := FirstStaff[iStaff];
    if Measure.Name = 'Measure' then
    begin
      Voice1 := nil;
      Voice2 := nil;
      iVoice := 0;
      if iStaff = 22 then
        i := i;
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
        result := SearchNext(iChord1, Voice1) and
                  SearchNext(iChord2, Voice2);
        if not result then
          break;

        Chord1 := Voice1[iChord1];
        Chord2 := Voice2[iChord2];
        First := Chord1.GetFirstIndex('Note');
        Lyrics := Chord2.HasChild('Lyrics');
        if Lyrics <> nil then
        begin
          Chord1.InsertChildNode(First, Lyrics.CopyTree);
        end;
        dur1 := GetDur(Chord1);
        dur2 := GetDur(Chord2);
        inc(offset1, dur1);
        inc(offset2, dur2);
        while (offset1 <> offset2) and
              result and
              (iChord1 < Voice1.Count) and (iChord2 < Voice2.Count) do
        begin
          while (iChord1 < Voice1.Count) and (offset1 < offset2) do
          begin
            inc(iChord1);
            if SearchNext(iChord1, Voice1) then
            begin
              Chord1 := Voice1[iChord1];
              dur1 := GetDur(Chord1);
              inc(offset1, dur1);
            end;
          end;
          while result and
                (iChord2 < Voice2.Count) and (offset1 > offset2) do
          begin
            inc(iChord2);
            if SearchNext(iChord2, Voice2) then
            begin
              Chord2 := Voice2[iChord2];
              dur2 := GetDur(Chord2);
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
  KaraokeChannel: integer;
  lyricsStaff: integer;
  events: TEventArray;
  Event: TMidiEvent;
  hasLyrics: array of integer;
  hasText: array of integer;
  hasSound: array of integer;
  Root: KXmlNode;
  Score, Staff, Measure, Chord, Voice, Lyrics, Staffs, FirstStaff: KXmlNode;
  Child, LyricsTree: KXmlNode;
  Ext: string;
  i, k, iScore: integer;
  iStaff, iVoice, iChord, iMeasure: integer;
  staffCount: integer;
  MidiEvents: TMidiEventArray;
  Tracks: TTrackEventArray;
  LyricStaffNo: integer;
begin
  result := false;
  Ext := ExtractFileExt(FileName);
  SetLength(FileName, Length(FileName) - Length(Ext));
  Events := nil;

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
  Events.SaveSimpleMidiToFile(FileName + '.txt');
  Events.DetailHeader.smallestFraction := 16; // 32nd
  SetLength(hasText, Events.TrackCount);
  SetLength(hasSound, Events.TrackCount);
  SetLength(hasLyrics, Events.TrackCount);
  KaraokeChannel := -1;
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
      KaraokeChannel := i;
  end;

  i := 1;
  k := 1;
  while (i < Length(Events.Track[KaraokeChannel+1])) do
  begin
    with Events.Track[KaraokeChannel+1][i] do
      if (command <> $ff) or (d1 <> 5) or
         not (Events.Track[KaraokeChannel+1][i][0] in [#0..#31]) then
      begin
        Events.Track[KaraokeChannel+1][k] := Events.Track[KaraokeChannel+1][i];
        inc(k);
      end;
    inc(i);
  end;
  SetLength(Events.Track[KaraokeChannel+1], k);

  if not FileExists(FileName + '.mscz') and
     not FileExists(FileName + '.mscx') then
  begin
    Application.MessageBox(
      PChar(Format('Neither the file "%s.mscz" nor the file "%s.mscx exists!',
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
  staffCount := 0;
  FirstStaff := nil;
  lyricsStaff := -1;
  LyricStaffNo := -1;
  for iScore := 0 to Score.Count-1 do
  begin
    Staff := Score[iScore];
    if Staff.Name = 'Part' then
    begin
      Child := Score[iScore].HasChild('trackName');
      if (Child <> nil) and (LyricStaffNo < 0) and
         (Pos('lyric', LowerCase(Child.Value)) > 0) then
      begin
        Child := Score[iScore].HasChild('Staff');
        if (Child <> nil) then
          LyricStaffNo := StrToIntDef(Child.Attributes['id'], -1);
      end;
    end else
    if Staff.Name = 'Staff' then
    begin
      if (LyricStaffNo >= 0) and
         (LyricStaffNo = StrToIntDef(Staff.Attributes['id'], -1)) then
        FirstStaff := Staff;
      if FirstStaff = nil then
        FirstStaff := Staff;
      inc(staffCount);
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
                      if lyricsStaff = -1 then
                      begin
                        lyricsStaff := iScore;
                      //  FirstStaff := Staff;
                      end;
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

  result := (FirstStaff <> nil) and (KaraokeChannel >= 0);
  if result then
  begin
    LyricsTree := nil;
    TEventArray.Quantize(Events.Track[KaraokeChannel], Events.DetailHeader);
    ReduceToLyrics(Events.Track[KaraokeChannel], true, Events.DetailHeader);
    result := BuildLyricsTree(LyricsTree, Events.Track[KaraokeChannel], Events.DetailHeader);
    if result then
    begin
      AddVoice(FirstStaff, LyricsTree, Events.DetailHeader);
      ReduceVoice(FirstStaff, Events.DetailHeader);
    end;
    LyricsTree.Free;
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
//      Root.SaveToXmlFile(FileName + '_.xml', '<?xml version="1.0" encoding="UTF-8"?>'#13#10);
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
