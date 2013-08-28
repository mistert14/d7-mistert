unit prog1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterGeneral, SynEdit, SynMemo;

type
  TForm1 = class(TForm)
    SynMemo1: TSynMemo;
    SynGeneralSyn1: TSynGeneralSyn;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
