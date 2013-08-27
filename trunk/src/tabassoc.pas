unit tabassoc;

interface

uses
  Classes;

type
  TElementTableauAssociatif = record
    Cle: string;
    Valeur: string;
  end;
  PElementTableauAssociatif = ^TElementTableauAssociatif;

  TableauAssociatif = class
  private
    function getCount: integer;
    function rechercheElem(Cle: string): PElementTableauAssociatif;
    procedure AjoutElement(Cle: string; Valeur: string);
    function obtenirValeur(Cle: string): string;
    function listeCles: Tstringlist;
  public
    fElems: TList;
    function EstPresent(Cle: string): boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RetirerElement(Cle: string);
    property Count: integer
      read getCount;
    property Keys: Tstringlist
      read ListeCles;
    property Valeur[cle: string]: string
      read obtenirValeur
      write AjoutElement; default;
  end;

implementation

{ TableauAssociatif }
function TableauAssociatif.ListeCles:Tstringlist;
var liste:TStringList;

i:integer;
begin
  liste:=TStringList.Create;
  for i:=0 to self.fElems.Count-1 do begin
      liste.Add(PElementTableauAssociatif(fElems.Items[i])^.Cle);
  end;
  result:=liste;
end;

procedure TableauAssociatif.AjoutElement(Cle, Valeur: string);
var
  Elem: PElementTableauAssociatif;
begin
  // recherche d'une entrée comportant la clé transmise
  Elem := rechercheElem(Cle);
  // si l'entrée existe
  if Elem <> nil then
    { mise à jour de la valeur (l'entrée sera toujours référencée dans la liste,
      il n'y a donc rien de plus à faire au niveau du pointeur puisque ce n'est
      pas lui qui change mais une des valeurs pointées. }
    Elem^.Valeur := Valeur
  else
    begin
      { Création d'un nouvel élément }
      new(Elem);
      Elem^.Cle := Cle;
      Elem^.Valeur := Valeur;
      { ajout d'une entrée dans la liste des pointeurs. Notez le transtypage implicite
        de Elem en type Pointer. Notez également qu'on ne DOIT PAS appeler Dispose
        sur Elem ici : ce sera fait lorsque l'élément sera plus tard retiré. }
      fElems.Add(Elem);
    end;
end;

constructor TableauAssociatif.Create;
begin
  fElems := TList.Create;
end;

destructor TableauAssociatif.Destroy;
var
  indx: integer;
begin
  for indx := 0 to fElems.Count - 1 do
    Dispose(PElementTableauAssociatif(fElems[indx]));
  fElems.Free;
  inherited;
end;

function TableauAssociatif.getCount: integer;
begin
  Result := fElems.Count;
end;

function TableauAssociatif.obtenirValeur(Cle: string): string;
var
  Elem: PElementTableauAssociatif;
begin
  // recherche d'une entrée comportant la clé transmise
  Elem := rechercheElem(Cle);
  if Elem <> nil then
    Result := Elem^.Valeur
  else
    Result := '';
end;

function TableauAssociatif.rechercheElem(Cle: string): PElementTableauAssociatif;
var
  indx: integer;
begin
  indx := 0;
  Result := nil;
  { recherche jusqu'à ce que Result soit modifié (clé trouvée) ou qu'il n'y ait plus aucun
    élément à trouver. Remarquez le choix du while bien préférable à un for. }
  while (indx < fElems.Count) and (Result = nil) do
    begin
      { remarquez que Items est une propriété tableau par défaut et que l'on
        pourrait écrire fElems[indx] à la place de fElems.Items[indx].
        Remarquez le transtypae et la comparaison dans la foulée... }
      if PElementTableauAssociatif(fElems.Items[indx])^.Cle = Cle then
        // ici, on exploite le coté "par défaut" de Items
        Result := fElems[indx];
      inc(indx);
    end;
end;
function TableauAssociatif.EstPresent(Cle: string): boolean;
begin
  result:=self.rechercheElem(Cle)<> nil;
end;

procedure TableauAssociatif.RetirerElement(Cle: string);
var
  Elem: PElementTableauAssociatif;
begin
  // recherche d'une entrée comportant la clé transmise
  Elem := rechercheElem(Cle);
  if Elem <> nil then
    begin
      fElems.Extract(Elem);
      Dispose(Elem);
    end;
end;

end.
