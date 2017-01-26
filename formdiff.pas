unit formdiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, framerevision;

type

  { TfDiff }

  TfDiff = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbDiffKind: TComboBox;
    frmRevision1: TfrmRevision;
    frmRevision2: TfrmRevision;
    procedure cbDiffKindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    function GetselectedRevision(Selector: integer): string;
  end;

var
  fDiff: TfDiff;

implementation

uses SVNTypes;

{$R *.lfm}

{ TfDiff }

procedure TfDiff.cbDiffKindChange(Sender: TObject);
begin
  case cbDiffKind.ItemIndex of
     0:
       begin
          frmRevision1.Enabled := false;
          frmRevision2.Enabled := false;
       end;
     1:
       begin
          frmRevision1.Enabled := false;
          frmRevision2.Enabled := false;
       end;
     2:
       begin
          frmRevision1.Enabled := true;
          frmRevision2.Enabled := false;
       end;
     3:
       begin
          frmRevision1.Enabled := true;
          frmRevision2.Enabled := true;
       end;

  end;

end;

procedure TfDiff.FormCreate(Sender: TObject);
begin
  frmRevision1.gbRevision.Caption:= Format(rsCaptionRevision,[1]);
  frmRevision2.gbRevision.Caption:= Format(rsCaptionRevision,[2]);
  cbDiffKind.ItemIndex:= 2;
  frmRevision1.Enabled := true;
  frmRevision2.Enabled := false;
end;

function TfDiff.GetselectedRevision(Selector: integer): string;
begin
  case Selector of
    1: case cbDiffKind.ItemIndex of
         0,
         1,
         2: Result := '';
         3: Result := frmRevision1.GetSelectedRevision;
      end;
    2: case cbDiffKind.ItemIndex of
         0: Result := REV_BASE;
         1: Result := REV_HEAD;
         2: Result := frmRevision1.GetSelectedRevision;
         3: Result := frmRevision2.GetSelectedRevision;
      end;
  end;
end;

end.

