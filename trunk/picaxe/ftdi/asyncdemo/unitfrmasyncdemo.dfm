object frmAsyncDemo: TfrmAsyncDemo
  Left = 371
  Top = 259
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ftdiclass async loopback demo'
  ClientHeight = 93
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 91
    Height = 13
    Caption = 'Device description:'
  end
  object editname: TEdit
    Left = 120
    Top = 16
    Width = 153
    Height = 21
    TabOrder = 0
    Text = 'AXE027 PICAXE USB'
  end
  object Button1: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button2Click
  end
end
