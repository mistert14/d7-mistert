object Form24: TForm24
  Left = 470
  Top = 319
  Width = 350
  Height = 245
  Caption = 'D'#233'tecter le cable AXE027 PICAXE USB'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 96
    Width = 253
    Height = 13
    Alignment = taCenter
    Caption = 'Double cliquez sur un '#233'lement pour afficher les d'#233'tails'
  end
  object ListBox: TListBox
    Left = 8
    Top = 8
    Width = 321
    Height = 81
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBoxDblClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 120
    Width = 321
    Height = 73
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 296
    Top = 8
  end
end
