object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Insert Lyrics from Midi in MuseScore File'
  ClientHeight = 295
  ClientWidth = 759
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 184
    Width = 490
    Height = 45
    Caption = 'drop .mscz/x or .mid file here.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -37
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 40
    Top = 43
    Width = 673
    Height = 23
    Caption = 
      'You imported a score from a midi file, but some lyrics are in Mu' +
      'seScore missing.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 40
    Top = 104
    Width = 684
    Height = 23
    Caption = 
      'Save the score either as mscz or as mscx file with the same name' +
      ' as the midi file.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object OpenDialog1: TOpenDialog
    Left = 480
    Top = 176
  end
  object SaveDialog1: TSaveDialog
    Left = 160
    Top = 160
  end
end
