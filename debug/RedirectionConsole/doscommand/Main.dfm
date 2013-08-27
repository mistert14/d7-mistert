object Form1: TForm1
  Left = 338
  Top = 116
  Width = 558
  Height = 386
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 529
    Height = 281
    OEMConvert = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 24
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 200
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkClose
  end
  object Button1: TButton
    Left = 112
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = Button1Click
  end
  object DosCommand1: TDosCommand
    CommandLine = 'Cmd.exe'
    OnNewLine = DosCommand1NewLine
    OnTerminated = DosCommand1Terminated
    InputToOutput = False
    MaxTimeAfterBeginning = 0
    MaxTimeAfterLastOutput = 0
    Left = 384
    Top = 312
  end
end
