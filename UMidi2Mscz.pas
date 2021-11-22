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

procedure ReduceToLyrics(var Events: TMidiEventArray);
var
  i, k: integer;
begin
  i := 1;
  while i < Length(Events) do
  begin
    if (Events[i].command <> $ff) or
       (Events[i].d1 <> 5) then
    begin
      inc(Events[i-1].var_len, Events[i].var_len);
      for k := i to Length(Events)-2 do
        Events[k] := Events[k+1];
      SetLength(Events, Length(Events)-1);
    end else
      inc(i);
  end;
end;

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


procedure AddVoice(FirstStaff: KXmlNode; var Events : TMidiEventArray; Header: TDetailHeader);
var
  iEvent, offset, midiOffset, delta, iStaff: integer;
  Measure, Staff, Rest, Voice, Chord, Child: KXmlNode;
  t32takt, t, t1, i: integer;
  dots: boolean;
  duration, Len, relOffset, no: integer;
  sLen, s: string;
begin
  for iEvent := 1 to Length(Events)-2 do
    if (Events[iEvent].var_len < 10) and (Events[iEvent].var_len > 0) then
    begin
      inc(Events[iEvent-1].var_len, Events[iEvent].var_len);
      Events[iEvent].var_len := 0;
    end;

  midiOffset := Events[0].var_len;
  offset := 0;
  iEvent := 1;
  iStaff := 0;
  while iStaff < FirstStaff.Count do
  begin
    if FirstStaff[iStaff].Name = 'Measure' then
    begin
      relOffset := offset;
      t32takt := 0;
      Voice := FirstStaff[iStaff].AppendChildNode('voice');
      while (offset - relOffset < Header.TicksPerMeasure) and
            (iEvent < Length(Events)) do
      begin
        delta := Header.GetRaster(midiOffset - offset);
        if (delta > 0) and (midiOffset - offset > -10) then
        begin
          Len := delta;
          if Len > Header.TicksPerMeasure then
            Len := Header.TicksPerMeasure;
          t := 8*Len div Header.DeltaTimeTicks;
          while t > 0 do
          begin
            t1 := t;
            sLen := GetLen(t, dots, t32takt);
            Rest := Voice.AppendChildNode('Rest');
            Rest.AppendChildNode('visible', '0');
            Child := Rest.AppendChildNode('durationType', sLen);
            if dots then
              Rest.AppendChildNode('dots', '1');
            inc(t32takt, t1 - t);
          end;
          inc(offset, Len);
        end else begin
          i := iEvent;
          while (i < Length(Events)) and (Events[i].var_len < 10) do
          begin
            inc(midiOffset, Events[i].var_len);
            inc(i);
          end;
          if i > Length(Events) then
            i := Length(Events)-1
          else
            inc(midiOffset, Events[i].var_len);
          Len := Header.GetRaster(midiOffset - offset);
          if Len > Header.TicksPerMeasure then
            Len := Header.TicksPerMeasure;
          t := 8*Len div Header.DeltaTimeTicks;
          t1 := t;
          sLen := GetLen(t, dots, t32takt);
          Chord := Voice.AppendChildNode('Rest');
          Chord.AppendChildNode('visible', '0');
          Chord.AppendChildNode('durationType', sLen);
          if dots then
            Chord.AppendChildNode('dots', '1');
          inc(t32takt, t1 - t);
          Len := Header.DeltaTimeTicks*(t1 - t) div 8;

          inc(offset, Len);
          no := 0;
          while iEvent <= i do
          begin
            s := UTF8ToString(Events[iEvent].ansi);
            if trim(s) <> '' then
            begin
              Child := Chord.AppendChildNode('Lyrics');
              if no > 0 then
                Child.AppendChildNode('no', IntToStr(no));
              if (Length(s) > 0) then
              begin
                if (s[Length(s)] = ' ') then
                  SetLength(s, Length(s)-1)
                else
                if not (AnsiChar(s[Length(s)]) in [' '..'@', '/', '\']) then
                  Child.AppendChildNode('syllabic', 'begin');
              end;
              Child.AppendChildNode('text', s);
            end;
            inc(iEvent);
            inc(no);
          end;
        end;
      end;
      inc(iStaff);
    end;
  end;
end;

function ReduceVoice(var FirstStaff: KXmlNode): boolean;
var
  StaffCopy: KXmlNode;
  iStaff: integer;
  i, iVoice: integer;
  iChord1, iChord2: integer;
  First: integer;
  Measure, Voice1, Voice2, Chord1, Chord2: KXmlNode;
  Dur1, Dur2, Dots1, Dots2: string;

  function SearchNext(var iChord: integer; Voice: KXmlNode): boolean;
  begin
    while (iChord < Voice.Count) and
          (Voice[iChord].Name <> 'Rest') and (Voice[iChord].Name <> 'Chord') do
      inc(iChord);
    result := iChord < Voice.Count;
  end;

  procedure GetDur(var Duration, Dots: string; Chord: KXmlNode);
  var
    Child: KXmlNode;
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
  end;

begin
  result := true;
  StaffCopy := FirstStaff.CopyTree;

  iStaff := 0;
  while (iStaff < FirstStaff.Count) and result do
  begin
    Measure := FirstStaff[iStaff];
    if Measure.Name = 'Measure' then
    begin
      Voice1 := nil;
      Voice2 := nil;
      iVoice := 0;
      while (iVoice < Measure.Count) and (Voice2 = nil) do
      begin
        if Measure[iVoice].Name = 'voice' then
        begin
          if Voice1 = nil then
            Voice1 := Measure[iVoice]
          else
            Voice2 := Measure[iVoice];
        end;
        inc(iVoice);
      end;
      result := Voice2 <> nil;
      iChord1 := 0;
      iChord2 := 0;
      while result and
            (iChord1 < Voice1.Count) and (iChord2 < Voice2.Count) do
      begin
        if SearchNext(iChord1, Voice1) and
           SearchNext(iChord2, Voice2) then
        begin
          Chord1 := Voice1[iChord1];
          Chord2 := Voice2[iChord2];
          First := Chord1.GetFirstIndex('Note');
          GetDur(Dur1, Dots1, Chord1);
          GetDur(Dur2, Dots2, Chord2);
          if Chord2.HasChild('Lyrics') <> nil then
          begin
            if (Dur1 = Dur2) and (Dots1 = Dots2) then
            begin
              for i := 0 to Chord2.Count-1 do
                if (Chord2.ChildNodes[i] <> nil) and
                   (Chord2[i].Name = 'Lyrics') then
                begin
                  Chord1.InsertChildNode(First, Chord2[i]);
                  Chord2.ChildNodes[i] := nil;
                  inc(First);
                end;
              for i := Chord2.Count-1 downto 1 do
                if (Chord2[i] = nil) then
                  Chord2.PurgeChild(i);;
            end else
              result := false;
          end;
        end else
          result := false;
        inc(iChord1);
        inc(iChord2);
      end;
      Measure.RemoveChild(Voice2);
    end;
    inc(iStaff);
  end;


  if result then
    StaffCopy.Free
  else begin
    for i := FirstStaff.Count-1 downto 0 do
      FirstStaff.RemoveChild(FirstStaff[i]);
    for i := 0 to StaffCopy.Count-1 do
    begin
      FirstStaff.AppendChildNode(StaffCopy[i]);
      StaffCopy.ChildNodes[i] := nil;
    end;
    StaffCopy.Free;
  end;
end;

function TForm1.Merge(FileName: string): boolean;
var
  events: TEventArray;
  Root: KXmlNode;
  Score, Staff, Measure, Chord, Voice, Lyrics, Staffs, FirstStaff: KXmlNode;
  Ext: string;
  i, k, iScore: integer;
  iStaff, iVoice, iChord, iMeasure: integer;
  staffCount: integer;
  hasLyrics: boolean;
  MidiEvents: TMidiEventArray;
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
  if not Events.LoadMidiFromFile(FileName + '.mid') then
  begin
    Application.MessageBox(
      PChar(Format('File "%s.mid" not read!', [FileName])), 'Error', MB_OK);
    Events.Free;
    exit;
  end;

  Events.DetailHeader.smallestFraction := 64; // 64th

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
  hasLyrics := false;
  FirstStaff := nil;
  for iScore := 0 to Score.Count-1 do
  begin
    if Staff.Name = 'Staff' then
    begin
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
                    Lyrics := Chord.ChildNodes[iChord];
                    if Lyrics.Name = 'Lyrics' then
                    begin
                      hasLyrics := true;
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

  // Score.MakeStaffArray(Staffs);
  // Staffs.SaveToXmlFile('Score.xml');
  // Zu jedem Staff in MuseScore gibt es ein Staffs in Score
  // Zu jedem voice in MuseScore gibt es in Staffs ein Staff

  if FirstStaff <> nil then
  for i := 0 to Events.TrackCount-1 do
  begin
    ReduceToLyrics(Events.Track[i]);
    if Length(Events.Track[i]) > 5 then
    begin
      AddVoice(FirstStaff, Events.Track[i], Events.DetailHeader);
      ReduceVoice(FirstStaff);
    end;
  end;
  try
    if FileExists(FileName + '_.mscz') then
    begin
       if Application.MessageBox(PChar(Format('File "%s" exists. Overwrite it?', [FileName + '_.mscz'])),
                                 'Overwrite?', MB_YESNO) <> IDYES then
         exit;
    end;
    Root.SaveToMsczFile(FileName + '_.mscz');
  finally
    Root.Free;
    Events.Free;
  end;
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
