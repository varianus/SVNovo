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
unit formconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, ExtCtrls, Buttons, Config;

type

  { TfConfig }

  TfConfig = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    leSVNExe: TLabeledEdit;
    pcPreferences: TPageControl;
    tsGeneral: TTabSheet;
  private
    Procedure ConfigToMap;
    Procedure MapToConfig;
  public

  end;

var
  fConfig: TfConfig;

implementation

{$R *.lfm}

uses SVNClasses;

{ TfConfig }

procedure TfConfig.ConfigToMap;
begin
 leSVNExe.Text:= ConfigObj.ReadString('SVN/Executable',  TSVNClient.FindSvnExecutable);
end;

procedure TfConfig.MapToConfig;
begin

end;

end.

