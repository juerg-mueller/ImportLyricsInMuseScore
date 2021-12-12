object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Insert Lyrics from Midi in MuseScore File'
  ClientHeight = 303
  ClientWidth = 658
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
    Left = 80
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
    Width = 572
    Height = 19
    Caption = 
      'You imported a score from a midi file, but some lyrics are in Mu' +
      'seScore missing.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 38
    Top = 104
    Width = 576
    Height = 19
    Caption = 
      'Save the score either as mscz or as mscx file with the same name' +
      ' as the midi file.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 242
    Top = 256
    Width = 54
    Height = 13
    Caption = 'Midi Coding'
  end
  object cbxCodePage: TComboBox
    Left = 304
    Top = 253
    Width = 83
    Height = 21
    ItemIndex = 0
    TabOrder = 0
    Text = 'UTF-8'
    Items.Strings = (
      'UTF-8'
      'ISO 8859-1')
  end
  object OpenDialog1: TOpenDialog
    Left = 576
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    Left = 88
    Top = 128
  end
end
