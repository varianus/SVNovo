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
unit framerevision;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, ExtCtrls;

type

  { TfrmRevision }

  TfrmRevision = class(TFrame)
    dteRevDate: TDateEdit;
    edtRevision: TEdit;
    gbRevision: TGroupBox;
    rbRevision: TRadioButton;
    rbDate: TRadioButton;
    rbHead: TRadioButton;
  private

  public
    function GetSelectedRevision: string;
  end;

ResourceString
  rsCaptionRevision = 'Revision or date #%d';

implementation

uses SVNTypes;

{$R *.lfm}

{ TfrmRevision }

function TfrmRevision.GetSelectedRevision: string;
begin
  if not Enabled then
    begin
      Result := EmptyStr;
      exit;
    end;

  if rbHead.Checked then
    begin
      Result := REV_HEAD;
      exit;
    end;

  if rbRevision.Checked then
    begin
      Result := edtRevision.Text;
      exit;
    end;

  if rbDate.Checked then
    begin
      Result := FormatDateTime('{yyyy-mm-dd}', dteRevDate.Date + 1);
      exit;
    end;

end;

end.

