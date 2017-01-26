unit formannotate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  Grids, SVNTypes;

type

  { TfAnnotate }

  TfAnnotate = class(TForm)
    ButtonPanel1: TButtonPanel;
    sgAnnotate: TStringGrid;
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
      sgAnnotate.Cells[0,i] := Inttostr(Item.LineNo);
      sgAnnotate.Cells[1,i] := ifthen(Item.revision > 0, Inttostr(Item.Revision), ' - ');
      sgAnnotate.Cells[2,i] := Item.Author;
      sgAnnotate.Cells[3,i] := ifthen(item.DateSVN > 0, datetostr(item.DateSVN), ' - ');
      sgAnnotate.Cells[4,i] := Item.Line;
    end;
end;

end.

