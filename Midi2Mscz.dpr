program Midi2Mscz;

uses
  Vcl.Forms,
  UEventArray in 'UEventArray.pas',
  UMidi2Mscz in 'UMidi2Mscz.pas' {Form1},
  UMidiDataStream in 'UMidiDataStream.pas',
  UMyMemoryStream in 'UMyMemoryStream.pas',
  UMyMidiStream in 'UMyMidiStream.pas',
  UXmlNode in 'UXmlNode.pas',
  UXmlParser in 'UXmlParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
