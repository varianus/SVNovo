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
    StringGrid1: TStringGrid;
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
  StringGrid1.RowCount:= FList.Count +1;
  for i := 0 to FList.Count -1 do
    begin
      item := FList[i];
      StringGrid1.Cells[0,i] := Inttostr(Item.LineNo);
      StringGrid1.Cells[1,i] := Inttostr(Item.Revision);
      StringGrid1.Cells[2,i] := Item.Author;
      StringGrid1.Cells[3,i] := datetostr(item.DateSVN);
      StringGrid1.Cells[4,i] := Item.Line;
    end;
end;

end.

