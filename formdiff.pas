{
  This file is part of SVNovo
  Copyright (C) 2016 Marco Caselli <marcocas@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
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

