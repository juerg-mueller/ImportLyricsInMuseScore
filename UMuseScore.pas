unit UMuseScore;

interface

uses
  SysUtils, System.Zip;


implementation

uses
  UXmlNode, UEventArray,
  UMyMidiStream, USheetMusic, UMyMemoryStream;

const
  UseBellows = true;
  UseColors_ = true;
  UseGrandCross = true;
  BellowsWidth = 0.5;


function SaveToMscx(const Events: TEventArray; const FileName: string): boolean;
var
  SaveRec: TSaveRec;

  StaffNode, MeasureNode_, VoiceNode, ChordNode: KXmlNode;
  Volta1StartNode, Volta2StartNode: KXmlNode;

  function MakeVoltaSpanner: KXmlNode;
  begin
    result := NewXmlNode('Spanner');
    result.AppendAttr('type', 'Volta');
  end;
  {
  procedure Verzierung(iEvent: integer; NoteNode: KXmlNode; sDur: string);
  var
    Child, Child1: KXmlNode;
    CrossHead: boolean;
    GriffEvent: TGriffEvent;

    procedure AddMirror(Right: boolean);
    begin
      if Right then
        NoteNode.AppendChildNode('mirror', 'right')
      else
        NoteNode.AppendChildNode('mirror', 'left');
    end;

  begin
    GriffEvent := GriffPartitur.GriffEvents[iEvent];
    if UseGrandCross then
    begin
      if SaveRec.hasEvenGriff then
        AddMirror(odd(GetPitchLine(GriffEvent.GriffPitch)))
      else
      if SaveRec.hasSame then
      begin
        if SaveRec.nextSame then
        begin
          AddMirror(not GriffEvent.Cross);
          SaveRec.nextSame := false;
        end else
        if (iEvent < SaveRec.iEnd) and
           (GriffEvent.GriffPitch = GriffPartitur.GriffEvents[iEvent+1].GriffPitch) then
        begin
          SaveRec.nextSame := true;
          AddMirror(not GriffEvent.Cross);
        end;
      end;
    end;
    CrossHead := GriffEvent.Cross;
    if CrossHead and UseGrandCross then
    begin
      Child := NoteNode.AppendChildNode('Symbol');
      Child.AppendChildNode('name', 'noteheadXBlack');
      Child1 := Child.AppendChildNode('offset');
      Child1.AppendAttr('x', '-1.6');
      Child1.AppendAttr('y', '0');
      if not GriffEvent.InPush then
        AddHeadColor(Child);
    end;

    Child := NoteNode.AppendChildNode('Events');
    Child := Child.AppendChildNode('Event');
    if GriffEvent.GriffPitch <> GriffEvent.SoundPitch then
      Child.AppendChildNode('pitch',
        IntToStr(integer(GriffEvent.SoundPitch) - GriffEvent.GriffPitch));
    NoteNode.AppendChildNode('pitch', IntToStr(GriffEvent.GriffPitch));
    NoteNode.AppendChildNode('tpc', IntToStr(MuseScoreTPC[GriffEvent.GriffPitch mod 12]));
    if CrossHead and not UseGrandCross then
    begin
      if not GriffEvent.InPush then
        AddHeadColor(NoteNode);
      NoteNode.AppendChildNode('head', 'cross');
    end;
  end;

  procedure AddNextPrev(mes: integer; next: KXmlNode);
  var
    Child: KXmlNode;
    m, n: integer;
  begin
    n := 1;
    if mes < 0 then
    begin
      n := -1;
      mes := -mes;
    end;
    m := mes div GriffPartitur.GriffHeader.Details.TicksPerMeasure;
    if (mes mod GriffPartitur.GriffHeader.Details.TicksPerMeasure) > 0 then
      inc(m);
    m := n*m;

    Child := next.AppendChildNode('location');
    Child.AppendChildNode('measures', IntToStr(m));
  end;

  function AddVolta(s: string; visible: boolean): KXmlNode;
  var
    Child: KXmlNode;
  begin
    result := MakeVoltaSpanner;
    Child := result.AppendChildNode('Volta');
    if not visible then
    begin
      Child.AppendChildNode('visible', '0');
    end;
    Child.AppendChildNode('endHookType', '1');
    Child.AppendChildNode('beginText', s + '.');
    Child.AppendChildNode('endings', s);
  end;

  procedure AddVoltaPrev(delta: integer);
  var
    Child1, Child2: KXmlNode;
  begin
    Child1 := MakeVoltaSpanner;
    Child2 := Child1.AppendChildNode('prev');
    AddNextPrev(delta, Child2);
    VoiceNode.InsertChildNode(0, Child1);
  end;

  procedure AddNewMeasure;
  begin
    MeasureNode_ := StaffNode.AppendChildNode('', 'Measure ' + IntToStr(SaveRec.MeasureNr));
    MeasureNode_ := StaffNode.AppendChildNode('Measure');
    VoiceNode := MeasureNode_.AppendChildNode('voice');
    VoiceNode.AppendChildNode('BarLine');
  end;

  procedure NeuerTakt(Visible: boolean);
  var
    i: integer;
    Child1: KXmlNode;
  begin
    with SaveRec do
    begin
      if LastRepeat = rVolta1Start then
      begin
        Volta1Off := offset;
        Volta1StartNode := AddVolta('1', Visible);
        VoiceNode.InsertChildNode(0, Volta1StartNode);
        LastRepeat := rRegular;
        Volta2StartNode := nil;
      end;
      if (LastRepeat = rVolta2Start) and (Volta1Off >= 0) then
      begin
        Volta2Off := offset;
        Child1 := Volta1StartNode.AppendChildNode('next');
        AddNextPrev(Volta2Off - Volta1Off, Child1);

        AddVoltaPrev(-(Volta2Off - Volta1Off));

        Volta2StartNode := AddVolta('2', Visible);
        VoiceNode.InsertChildNode(1, Volta2StartNode);
        LastRepeat := rRegular;
      end;

      if TaktNr < offset div Takt then
      begin
        if (LastRepeat = rStart) then
        begin
          if not Visible then
          begin
            Child1 := nil;
            for i := 0 to Length(VoiceNode.ChildNodes)-1 do
            begin
              if VoiceNode.ChildNodes[i].Name = 'BarLine' then
              begin
                Child1 := VoiceNode.ChildNodes[i];
                break;
              end;
            end;
            if Child1 = nil then
              Child1 := VoiceNode.AppendChildNode('BarLine');
            Child1.AppendChildNode('subtype' , 'start-repeat');
            Child1.AppendChildNode('visible', '0');
          end else begin
            Child1 := NewXmlNode('startRepeat');
            MeasureNode_.InsertChildNode(0, Child1);
          end;
          LastRepeat := rRegular;
        end;

        if LastRepeat in [rVolta1Stop, rStop] then
        begin
          Child1 := NewXmlNode('endRepeat');
          Child1.Value := '2';
          MeasureNode_.InsertChildNode(0, Child1);
          LastRepeat := rRegular;
        end;

        t32takt := 0;
        inc(MeasureNr);
        AddNewMeasure;
        TaktNr := offset div Takt;

        if (LastRepeat = rVolta2Stop) and (Volta2Off >= 0) and
           (Volta2StartNode <> nil) then
        begin
          Child1 := Volta2StartNode.AppendChildNode('next');
          AddNextPrev(offset - Volta2Off, Child1);

          AddVoltaPrev(-(offset - Volta2Off));
          LastRepeat := rRegular;
        end;
      end;
    end;
  end;

  function AddTimeSig(Visible: boolean): KXmlNode;
  begin
    result := VoiceNode.AppendChildNode('TimeSig');
    if not Visible then
    begin
      result.AppendChildNode('visible', '0');
    end;
    result.AppendChildNode('sigN', IntToStr(GriffPartitur.GriffHeader.Details.measureFact));
    result.AppendChildNode('sigD', IntToStr(GriffPartitur.GriffHeader.Details.measureDiv));
  end;

  procedure AddRest(Len: integer; Lyrics, Visible: boolean; Bellows: boolean = false);
  var
    t, t1: integer;
    RestNode, Child, Child1: KXmlNode;
    iLen: integer;
    s: string;
  begin
    t := 8*Len div GriffPartitur.quarterNote;
    while t > 0 do
    begin
      t1 := t;
      if Lyrics then
      begin
        SaveRec.dot := false;
        if ((GriffPartitur.GriffHeader.Details.measureDiv = 8) or not SaveRec.Aufrunden) and
           (t >= 4) then
        begin
          SaveRec.sLen := 'eighth';
          dec(t, 4);
        end else
        if (t >= 8) then
        begin
          SaveRec.sLen := 'quarter';
          dec(t, 8);
        end else
          SaveRec.sLen := GetLen2(t, SaveRec.dot, SaveRec.t32takt);
        iLen := GetLyricLen(SaveRec.sLen);
        s := char(iLen);
        Child := VoiceNode.AppendChildNode('StaffText');
        Child1 := Child.AppendChildNode('offset');
        Child1.AppendAttr('x', '0');
        if UseBellows then
          Child1.AppendAttr('y', Format('%g', [6.6 + BellowsWidth])) //'7.1');
        else
          Child1.AppendAttr('y', '5.9');
        Child := Child.AppendChildNode('text');
        Child1 := Child.AppendChildNode('font');
        Child1.AppendAttr('face', 'ScoreText');
        Child1 := Child.AppendChildNode('font', s);
        // örgeli 22 st 18
        if iLen >= 58612 then
          Child1.AppendAttr('size', '24')
        else
        if GriffPartitur.Instrument.BassDiatonic then
          Child1.AppendAttr('size', '18')
        else
          Child1.AppendAttr('size', '22');
        if SaveRec.dot then
        begin
          Child1 := Child.AppendChildNode('font', '.');
          Child1.AppendAttr('size', '18');
        end;
        Child1 := Child.AppendChildNode('font');
        Child1.AppendAttr('face', 'Edwin');
      end else begin
        SaveRec.sLen := SaveRec.GetLenS(t, GriffPartitur.quarterNote);
      end;
      RestNode := VoiceNode.AppendChildNode('Rest');
      if Lyrics or Bellows then
        RestNode.AppendChildNode('visible', '0');
      if SaveRec.dot then
        RestNode.AppendChildNode('dots', '1');
      RestNode.AppendChildNode('durationType', SaveRec.sLen);
      inc(SaveRec.t32takt, t1 - t);
      inc(SaveRec.offset, GriffPartitur.quarterNote*(t1 - t) div 8 );
      NeuerTakt(Visible);
    end;
  end;
}
  procedure AppendStaff;


    procedure AddTie(NoteNode: KXmlNode; Next: boolean);
    var
      Child, Child1: KXmlNode;
      s: string;
    begin
      Child := NoteNode.AppendChildNode('Spanner');
      Child.AppendAttr('type', 'Tie');
      if Next then
        Child.AppendChildNode('Tie');

      s := 'prev';
      if Next then
        s := 'next';
      Child1 := Child.AppendChildNode(s);
      Child1 := Child1.AppendChildNode('location');
      s := SaveRec.tieFractions;
      if s <> '' then
      begin
        if not Next then
          s := '-' + s;
        Child1.AppendChildNode('fractions', s);
      end else begin
        if Next then
          s := '1'
        else
          s := '-1';
        Child1.AppendChildNode('measures', s);
      end;
    end;

    function GetFraction: string;
    var
      s: string;
      n, d: integer;
    begin
      result := '';
      s := SaveRec.slen;
      d := GetFraction_(s);
      if d = 0 then
        exit;

      n := 1;
      if SaveRec.dot then
      begin
        n := 3;
        d := 2*d;
      end;
      result := IntToStr(n) + '/' + IntToStr(d);
    end;

    procedure AddChord(WithBass: boolean);
    var
      Child1, Child2: KXmlNode;
    begin
      with SaveRec do
      begin
        ChordNode := VoiceNode.AppendChildNode('Chord');
        if Dot then
        begin
          ChordNode.AppendChildNode('dots', '1');
        end;
        ChordNode.AppendChildNode('durationType', sLen);
      end;
    end;

  var
    i: integer;
    NoteNode, Child, Child1, Child2: KXmlNode;
  begin
    with Events.Track[1], SaveRec do
    begin
      Clear;
      Takt := Events.DetailHeader.TicksPerMeasure;

      if Lyrics then
        SaveRec.Aufrunden := AufViertelnotenAufrunden;

      AddNewMeasure;

        AddTimeSig(true);

        Child := VoiceNode.AppendChildNode('Tempo');
        Child.AppendChildNode('tempo',
          Format('%f', [Events.DetailHeader.beatsPerMin / 60.0]));
        Child.AppendChildNode('followText', '1');
        Child1 := Child.AppendChildNode('text', ' = ' + IntToStr(Events.DetailHeader.beatsPerMin));
        Child2 := NewXmlNode('sym', 'metNoteQuarterUp');
        Child1.InsertChildNode(0, Child2);

      iEvent := 0;
      while iEvent < Length(Events.Track[1]) do
      begin
        NeuerTakt(nt = ntDiskant);
        if SaveRec.MostRight < GriffEvents[SaveRec.iEvent].AbsRect.Right then
          SaveRec.MostRight := GriffEvents[SaveRec.iEvent].AbsRect.Right;

        with GriffEvents[SaveRec.iEvent] do
        if NoteType > ntBass then
        begin
          if (GriffEvents[iEvent].Repeat_ <> rRegular){ and (nt = ntDiskant) }then
            LastRepeat := GriffEvents[iEvent].Repeat_;
          NeuerTakt(nt = ntDiskant);
          if NoteType = ntRest then
          begin
            Len := GriffHeader.Details.GetRaster(GriffEvents[SaveRec.iEvent].AbsRect.Right - offset);
            dot := false;
            AddRest(Len, Lyrics, nt = ntDiskant);
            LastRepeat := rRegular;
          end;
        end else begin
          // Pausen einfügen
          while (SaveRec.Rest(GriffHeader.Details.GetRaster(GriffEvents[SaveRec.iEvent].AbsRect.Left - offset),
                   (nt = ntBass) and (NoteType <> ntBass))) do
             AddRest(Len, Lyrics, nt = ntDiskant);

          if (GriffEvents[iEvent].Repeat_ <> rRegular) {and (nt = ntDiskant)} then
            LastRepeat := GriffEvents[iEvent].Repeat_;

          if NoteType = nt then
          begin
            if (nt = ntDiskant) and SetTriolen(SaveRec) then
            begin
              t := offset;
              tupletNr := 1;
              Child1 := VoiceNode.AppendChildNode('Tuplet');
              Child1.AppendChildNode('normalNotes', '2');
              Child1.AppendChildNode('actualNotes', '3');
              Child1.AppendChildNode('baseNote', sLen);
              Child2 := Child1.AppendChildNode('Number');
              Child2.AppendChildNode('style', 'Tuplet');
              Child2.AppendChildNode('text', '3');
              for i := 1 to 3 do
              begin
                ChordNode := VoiceNode.AppendChildNode('Chord');
                ChordNode.AppendChildNode('durationType', sLen);
                iEnd := LastChordEvent(iEvent);
                while iEvent <= iEnd do
                begin
                  if GriffEvents[iEvent].NoteType = ntDiskant then
                  begin
                    NoteNode := ChordNode.AppendChildNode('Note');
                    Verzierung(iEvent, NoteNode, sDur);
                  end;
                  inc(iEvent);
                end;
              end;
              inc(offset, 8*quarterNote div triole);
              inc(t32takt, 64 div triole);
              VoiceNode.AppendChildNode('endTuplet');
              NeuerTakt(nt = ntDiskant);
              continue;
            end;

            SetIEnd(SaveRec);

            tie := tieOff;
            tieFractions := '';
            while SaveRec.SaveLen(GriffHeader.Details.GetRaster(GriffEvents[SaveRec.iEvent].AbsRect.Right - offset)) do
            begin
              if nt = ntBass then
              begin
                if Len < QuarterNote div 2 then
                  Len := QuarterNote div 2;
                if Lyrics and Aufrunden and (Len < QuarterNote) then
                begin
                  // auf Viertelnoten aufrunden
                  Len := QuarterNote;
                end;
              end;
              t1 := Len;
              SaveRec.LimitToTakt;
              if (t1 > Len) { and (nt = ntDiskant)} then
              begin
                case Tie of
                  tieOff: Tie := tieStart;
                  tieStart: Tie := tieMitte;
                end;
              end else
              if (tieFractions = '') and (Tie <> tieOff) then
                Tie := tieStop;

              t := 8*Len div quarterNote;
              while t > 0 do
              begin
                i := iEvent;
                if (nt = ntDiskant) and
                   GriffPartitur.IsAppoggiatura(SaveRec) then
                  appoggiatura := tieStart;
                sLen := USheetMusic.GetLen(t, dot, t32takt);

                if (t > 0) {and (nt = ntDiskant)} then
                begin
                  case Tie of
                    tieOff: Tie := tieStart;
                    tieStop: Tie := tieMitte;
                  end;
                end else
                if (Tie <> tieOff) and (t = 0) and (tieFractions <> '') then
                  Tie := tieStop;

                sDur := IntToStr(3*(t1-t));
                inc(t32takt, t1 - t);

                NeuerTakt(nt = ntDiskant);

                if Lyrics and (nt = ntBass) then
                begin
                  if iEvent = 60 then
                    t := t;
                  if not Lyrics or (Tie <= tieStart) then
                  begin
                    if not Instrument.BassDiatonic then
                    begin
                      Child := VoiceNode.AppendChildNode('StaffText');
                      if not GriffEvents[iEvent].Cross then
                      begin
                        Child.AppendChildNode('size', '14');  // 14 pt
                      end;
                      Child.AppendChildNode('text', IntToStr(GriffEvents[iEvent].GriffPitch));

                      if (iEvent < iEnd) and
                         (GriffEvents[iEnd].NoteType = ntBass) then
                      begin
                        Child := VoiceNode.AppendChildNode('StaffText');
                        Child.AppendChildNode('text', IntToStr(GriffEvents[iEnd].GriffPitch));
                      end;
                    end else begin
                      Child := VoiceNode.AppendChildNode('StaffText');
                      if not GriffEvents[iEvent].InPush then
                        AddHeadColor(Child);
                      Child.AppendChildNode('text', GriffEvents[iEvent].GetSteiBass);
                      if (iEvent < iEnd) and
                         (GriffEvents[iEnd].NoteType = ntBass) then
                      begin
                        Child := VoiceNode.AppendChildNode('StaffText');
                        if not GriffEvents[iEvent].InPush then
                          AddHeadColor(Child);
                        Child.AppendChildNode('text', GriffEvents[iEnd].GetSteiBass);
                      end;
                    end;
                  end;
                  // Die Pause wird beschriftet.
                  Child := VoiceNode.AppendChildNode('Rest');
                  Child.AppendChildNode('visible', '0');
                  if Dot then
                  begin
                    Child.AppendChildNode('dots', '1');
                  end;
                  Child.AppendChildNode('durationType', sLen);
                end else begin
                  AddChord(true);
                  while i <= iEnd do
                    with GriffEvents[i] do
                    begin
                      if GriffEvents[i].NoteType = nt then
                      begin
                        if appoggiatura <> tieOff then
                        begin
                          case appoggiatura of
                            tieStart:
                              begin
                                Child := ChordNode.AppendChildNode('Spanner');
                                Child.AppendAttr('type', 'Slur');
                                Child.AppendChildNode('Slur');
                                Child1 := Child.AppendChildNode('next');
                                Child1.AppendChildNode('location');
                                ChordNode.AppendChildNode('appoggiatura');
                                if (i < iEnd) and GriffEvents[i+1].IsAppoggiatura(GriffHeader) then
                                  appoggiatura := tieMitte
                                else
                                  appoggiatura := tieStop;
                              end;
                            tieMitte:
                              if (i >= iEnd) or
                                 not GriffEvents[i+1].IsAppoggiatura(GriffHeader) then
                                appoggiatura := tieStop;
                            tieStop:
                              begin
                                AddChord(false);
                                Child := ChordNode.AppendChildNode('Spanner');
                                Child.AppendAttr('type', 'Slur');

                                Child1 := Child.AppendChildNode('prev');
                                Child1 := Child1.AppendChildNode('location');
                                Child1.AppendChildNode('grace', '0');
                                appoggiatura := tieOff;
                              end;
                          end;
                        end;

                        NoteNode := ChordNode.AppendChildNode('Note');
                        if (nt = ntDiskant) and not GriffEvents[i].InPush then
                          AddHeadColor(NoteNode);

                        if Tie <> TieOff then
                        begin
                          if Tie in [tieStart, tieMitte] then
                          begin
                            if t > 0 then
                              tieFractions := GetFraction;
                            AddTie(NoteNode, true);   // next
                            tieFractions := '';
                          end;
                          if Tie in [tieMitte, tieStop] then
                          begin
                            AddTie(NoteNode, false);  // prev
                          end;
                        end;

                        if nt = ntDiskant then
                          Verzierung(i, NoteNode, sDur)
                        else
                        if not Lyrics then
                        begin
                          NoteNode.AppendChildNode('pitch',
                            IntToStr(GriffEvents[i].SoundPitch));
                        end;
                      end;
                    inc(i);
                  end;
                  if Tie = tieStart then
                    Tie := tieMitte;
                end;
                if t > 0 then
                  tieFractions := GetFraction;
              end;
              inc(offset, Len);
              if t32takt*quarterNote >= 8*Takt then
                NeuerTakt(nt = ntDiskant);
            end;
            iEvent := iEnd;
          end;
        end;
        inc(iEvent);
      end;

      SaveRec.MostRight := GriffPartitur.GriffHeader.Details.GetRaster(SaveRec.MostRight);
      SaveRec.Len := SaveRec.MostRight mod SaveRec.Takt;
      with SaveRec do
      begin
        if Len > 0 then
          Len := MostRight + Takt - Len - Offset;
        if Len > 0 then
          AddRest(Len, Lyrics, nt = ntDiskant);

        if (Length(VoiceNode.ChildNodes) >= 2) or
           (SaveRec.LastRepeat <> rRegular) then
          AddRest(Takt, Lyrics, nt = ntDiskant);
        StaffNode.RemoveChild(MeasureNode_);
      end;
    end;
  end;


  function AddStaff(Nr: integer; Part: KXmlNode): KXmlNode;
  begin
    result := Part.AppendChildNode('Staff');
    result.AppendAttr('id', IntToStr(Nr));
  end;

  function AddPart(Nr: integer; BassClef, Invisual: boolean; Score:KXmlNode): KXmlNode;
  var
    Child, Child1, Child2: KXmlNode;
  begin
    result := Score.AppendChildNode('Part');
    Child := AddStaff(Nr, result);
    Child1 := Child.AppendChildNode('StaffType');
    Child1.AppendAttr('group', 'pitched');
    Child1.AppendChildNode('name', 'stdNormal');

    if Invisual then
    begin
      Child1.AppendChildNode('clef', '0');
      Child1.AppendChildNode('barlines', '0');
      Child1.AppendChildNode('timesig', '0');
      Child1.AppendChildNode('invisible', '1');

      Child.AppendChildNode('invisible', '1');
      Child.AppendChildNode('hideSystemBarLine', '1');
    end else
    if BassClef then
    begin
      Child.AppendChildNode('defaultClef', 'F');
    end else
      Child1.AppendChildNode('clef', '0');

    if BassClef then
    begin
      result.AppendChildNode('show', '0');
    end;
    result.AppendChildNode('trackName', 'Akkordeon');
    Child := result.AppendChildNode('Instrument');
    Child.AppendAttr('id', 'accordion');
    Child.AppendChildNode('trackName', 'Akkordeon');
    Child.AppendChildNode('instrumentId', 'keyboard.accordion');
    Child1 := Child.AppendChildNode('Articulation');
    Child1.AppendChildNode('velocity', '100');
    Child1.AppendChildNode('gateTime', '100');

    Child1 := Child.AppendChildNode('Channel');
    Child2 := Child1.AppendChildNode('progam');
    Child2.AppendAttr('value', '0');
    Child2 := Child1.AppendChildNode('controller');
    Child2.AppendAttr('ctrl', '10');
    Child2.AppendAttr('value', '63');
    Child1.AppendChildNode('synti', 'Fluid');

    Child1 := Child.AppendChildNode('Channel');
    Child1.AppendAttr('value', '0');
    Child2 := Child1.AppendChildNode('progam');
    Child2.AppendAttr('value', '0');
    Child2 := Child1.AppendChildNode('controller');
    Child2.AppendAttr('ctrl', '10');
    Child2.AppendAttr('value', '63');
    Child1.AppendChildNode('synti', 'Fluid');
  end;

var
  Root, Score, Part, Child, Child1, Child2: KXmlNode;
  Staff1, Staff3: KXmlNode;
  s, t: string;
  p: integer;
  Stream: TMyMemoryStream;
begin


  Root := NewXmlNode('museScore');
  Root.AppendAttr('version', '3.02');
  Root.AppendChildNode('programVersion', '3.6.2');
  Root.AppendChildNode('programRevision');

  Score := Root.AppendChildNode('Score');
  Child := Score.AppendChildNode('LayerTag');
  Child.AppendAttr('id', '0');
  Child.AppendAttr('tag', 'default');

  Score.AppendChildNode('currentLayer', '0');
  Score.AppendChildNode('Division', IntToStr(GriffPartitur.quarterNote));

  Child := Score.AppendChildNode('Style');

  // Lyrics unten
  Child.AppendChildNode('staffPlacement', '1');
  Child1 := Child.AppendChildNode('staffPosBelow');
  Child1.AppendAttr('x', '0');
  if UseBellows then
    Child1.AppendAttr('y', Format('%g', [5.7 + BellowsWidth]))
  else
    Child1.AppendAttr('y', '5.4');
  //'6.1');      // 5.3
  Child.AppendChildNode('Spatium', '1.74978');

  Score.AppendChildNode('showInvisible', '0');
  Score.AppendChildNode('showUnprintable', '1');
  Score.AppendChildNode('showFrames', '1');
  Score.AppendChildNode('showMargins', '1');

  s := ExtractFilename(FileName);
  SetLength(s, Length(s) - Length(ExtractFileExt(s)));
  p := Pos('_', s);
  if p > 0 then
    SetLength(s, p-1);

  Child := Score.AppendChildNode('metaTag');
  Child.AppendAttr('name', 'workTitle');
  Child.Value := s;

  ///////////////////////////////////////////////////////////////// Score Part 1

  AddPart(1, false, false, Score);

  StaffNode := AddStaff(1, Score);

  Stream := KXmlNode.BuildMemoryStream(Root);
  Stream.SaveToFile(FileName);
  Stream.Free;

  result := true;
end;

end.
