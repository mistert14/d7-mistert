object Form1: TForm1
  Left = 198
  Top = 124
  Width = 928
  Height = 480
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
  object SynMemo1: TSynMemo
    Left = 16
    Top = 8
    Width = 433
    Height = 417
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.AutoSize = True
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Gradient = True
    Highlighter = SynGeneralSyn1
    Lines.UnicodeStrings = 
      '#picaxe 16m2'#13#10'#com 3'#13#10#13#10#39#13#10#39'Programme de test'#13#10#39#13#10#13#10'main:'#13#10#13#10'   ' +
      ' high B.0'#13#10'    pause 1000'#13#10'    low B.0'#13#10'    pause 1000'#13#10'    goto' +
      ' main'
    WantTabs = True
    FontSmoothing = fsmNone
  end
  object SynGeneralSyn1: TSynGeneralSyn
    DefaultFilter = '*.bas'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.LineCommentarStart = '#'
    Options.Visible = True
    CommentAttri.Foreground = clGreen
    Comments = [csBasStyle]
    DetectPreprocessor = False
    IdentifierAttri.Foreground = clHotLight
    KeyAttri.Foreground = clFuchsia
    KeyWords.UnicodeStrings = 
      '#com'#13#10'#picaxe'#13#10'B.0'#13#10'B.1'#13#10'B.2'#13#10'B.3'#13#10'B.4'#13#10'B.5'#13#10'B.6'#13#10'B.7'#13#10'C.0'#13#10'C.1'#13 +
      #10'C.2'#13#10'C.6'#13#10'C.7'#13#10'else'#13#10'for'#13#10'gosub'#13#10'goto'#13#10'high'#13#10'if'#13#10'let'#13#10'low'#13#10'next' +
      #13#10'pause'#13#10'serrxd'#13#10'sertxd'#13#10'then'#13#10'to'
    SymbolAttri.Foreground = clMaroon
    Left = 592
    Top = 24
  end
end
