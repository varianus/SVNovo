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
unit formcommit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Config, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ButtonPanel, CheckLst, ExtCtrls;

type

  { TfCommit }

  TfCommit = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    cbRecents: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    mLogMessage: TMemo;
    Panel1: TPanel;
    procedure cbRecentsSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    History : TSimpleHistory;
  public

  end;

var
  fCommit: TfCommit;

implementation

{$R *.lfm}

{ TfCommit }

procedure TfCommit.OKButtonClick(Sender: TObject);
begin
  History.Add(mLogMessage.Lines.Text);
  History.WriteToConfig(ConfigObj, 'History/CommitMessages');
end;

procedure TfCommit.FormCreate(Sender: TObject);
begin
  History := TSimpleHistory.Create;
  History.LoadFromConfig(ConfigObj, 'History/CommitMessages');
  History.Max:= 50;
  History.GetList(cbRecents.Items);
  cbRecents.Items.Insert(0,'');
end;

procedure TfCommit.cbRecentsSelect(Sender: TObject);
begin
  mLogMessage.Lines.Text := cbRecents.Text;
end;

end.

