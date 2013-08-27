object Form2: TForm2
  Left = 285
  Top = 148
  Width = 607
  Height = 536
  Caption = 'Associer une animation flash'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ShockwaveFlash1: TShockwaveFlash
    Left = 0
    Top = 48
    Width = 591
    Height = 450
    Align = alBottom
    TabOrder = 0
    ControlData = {
      6755665500090000153D0000822E000008000200000000000800000000000800
      0000000008000E000000570069006E0064006F00770000000800060000002D00
      310000000800060000002D003100000008000A00000048006900670068000000
      08000200000000000800060000002D0031000000080000000000080002000000
      0000080010000000530068006F00770041006C006C0000000800040000003000
      0000080004000000300000000800020000000000080000000000080002000000
      00000D0000000000000000000000000000000000080004000000310000000800
      0400000030000000080000000000080004000000300000000800080000006100
      6C006C00000008000C000000660061006C00730065000000}
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 33
    Caption = 'Ouvrir une animation flash'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 160
    Top = 8
    Width = 145
    Height = 33
    Caption = 'Sauvegarder'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 312
    Top = 8
    Width = 129
    Height = 33
    Caption = 'Quitter'
    TabOrder = 3
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Left = 536
    Top = 8
  end
end
