unit formannotate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  Grids, Menus, SVNTypes, Types;

type

  { TfAnnotate }

  TfAnnotate = class(TForm)
    ButtonPanel1: TButtonPanel;
    FindDialog1: TFindDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    sgAnnotate: TStringGrid;
    procedure FindDialog1Find(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure sgAnnotatePrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    FList: TSVNAnnotateList;
    procedure SetList(AValue: TSVNAnnotateList);
    Procedure UpdateList;
  public
     property List: TSVNAnnotateList read FList write SetList;
  end;

var
  fAnnotate: TfAnnotate;

implementation
uses strutils;

{$R *.lfm}

{ TfAnnotate }

procedure TfAnnotate.MenuItem2Click(Sender: TObject);
begin
  //
  FindDialog1.Execute;
end;

procedure TfAnnotate.FindDialog1Find(Sender: TObject);
var
  i,startindex: integer;
  Options: TStringSearchOptions;

  Function FindInLine(Line:Integer): boolean;
  Var
    FoundPos: integer;
  begin
    if SearchBuf(pchar(sgAnnotate.Cols[4][line]), Length(sgAnnotate.Cols[4][line]), 0, 0, finddialog1.FindText, Options) <> nil then
      begin
        sgAnnotate.Row:=Line;
        sgAnnotate.TopRow:=Line;
        Result:= true;
      end
    else
      Result:= false;
  end;

begin

  Options:=[soDown];
  if frMatchCase in FindDialog1.Options then
    Options:=Options+[soMatchCase];

  if frWholeWord in FindDialog1.Options then
    Options:=Options+[soWholeWord];

  startindex:=sgAnnotate.Row;
  if startindex=-1 then startindex:=0;

  if frFindNext in finddialog1.Options then //start from next index
    inc(startindex);

  if (frDown in FindDialog1.Options)  then
    begin
      for i:=startindex to sgAnnotate.Cols[4].Count-1 do
        if FindInLine(i) then
          exit;
      if (frEntireScope in FindDialog1.Options)  then
        for i:=1 to startindex -1 do
          if FindInLine(i) then
            exit;
    end;

  if not (frDown in FindDialog1.Options) then
    begin
      for i:=startindex -2 downto 1 do
        if FindInLine(i) then
          exit;

      if (frEntireScope in FindDialog1.Options)  then
        for i:=sgAnnotate.Cols[4].Count-1 downto startindex -1 do
          if FindInLine(i) then
            exit;
    end;
end;

procedure TfAnnotate.MenuItem3Click(Sender: TObject);
begin
  if finddialog1.FindText='' then
    finddialog1.Execute
  else
    FindDialog1Find(finddialog1); //next scan
end;

procedure TfAnnotate.sgAnnotatePrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then exit;

  if sgAnnotate.Cells[5,aRow] = '1' then
    sgAnnotate.Canvas.Brush.Color:= clwhite
  else
    sgAnnotate.Canvas.Brush.Color:= clLtGray;

end;

procedure TfAnnotate.SetList(AValue: TSVNAnnotateList);
begin
  if FList = AValue then
    Exit;
  FList := AValue;

  UpdateList;

end;

procedure TfAnnotate.UpdateList;
var
  i: integer;
  Item: TSVNAnnotateItem;
begin
  sgAnnotate.RowCount:= FList.Count +1;
  for i := 0 to FList.Count -1 do
    begin
      item := FList[i];
      sgAnnotate.Cells[0,i+1] := Inttostr(Item.LineNo);
      sgAnnotate.Cells[1,i+1] := ifthen(Item.revision > 0, Inttostr(Item.Revision), ' - ');
      sgAnnotate.Cells[2,i+1] := Item.Author;
      sgAnnotate.Cells[3,i+1] := ifthen(item.DateSVN > 0, datetostr(item.DateSVN), ' - ');
      sgAnnotate.Cells[4,i+1] := Item.Line;
      sgAnnotate.Cells[5,i+1] := IntToStr(Item.Group);
    end;
end;

end.

