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

unit formlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, SVNClasses, FilesSupport, Config;

type

  { TfLog }

  TfLog = class(TForm)
    actClose: TAction;
    actDiff: TAction;
    ActionList1: TActionList;
    actView: TAction;
    bClose: TButton;
    bDiff: TButton;
    lvAffectedFiles: TListView;
    lstREvisions: TListView;
    mMessage: TMemo;
    pcInfo: TPageControl;
    tsMessage: TTabSheet;
    tsFiles: TTabSheet;
    procedure actCloseExecute(Sender: TObject);
    procedure actDiffExecute(Sender: TObject);
    procedure actViewExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstREvisionsSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    FCurrentFile: String;
    FList: TSVNLogList;
    procedure SetCurrentFile(AValue: String);
    procedure SetList(AValue: TSVNLogList);
    procedure UpdateList;
  public
    Property CurrentFile: String read FCurrentFile write SetCurrentFile;
    property List: TSVNLogList read FList write SetList;
  end;

var
  fLog: TfLog;

implementation
uses strutils, uMain;
{$R *.lfm}

{ TfLog }

procedure TfLog.FormDestroy(Sender: TObject);
begin
  if Assigned(FList) then
    FList.Free;
end;

procedure TfLog.lstREvisionsSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
var
  i: integer;
  svnItem: TSVNLogItem;
  l: TlistItem;
begin
  if Selected then
  begin
    lvAffectedFiles.Clear;
    mMessage.Lines.Text := TSVNLogItem(Item.Data).Message;
    svnItem := TSVNLogItem(Item.Data);
    for i := 0 to svnItem.AffectedFiles.Count - 1 do
    begin
      l := lvAffectedFiles.items.Add;
      l.Caption := svnItem.AffectedFiles.Items[i].Action;
      l.SubItems.Add(svnItem.AffectedFiles.Items[i].FileName);

    end;

  end;
end;

procedure TfLog.FormCreate(Sender: TObject);
begin
  FList := nil;
end;

procedure TfLog.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfLog.actDiffExecute(Sender: TObject);
var
 svnItem: TSVNLogItem;
 TempName1, TempName2: string;
begin

  if lstREvisions.SelCount = 1 then
    begin
      SvnItem := TSVNLogItem(lstREvisions.Selected.Data);
      TempName1 := fMain.SVNClient.Export(CurrentFile, svnItem.Revision);
      RunExternal(CFG_Diff,[CurrentFile, TempName1]);
    end;

end;

procedure TfLog.actViewExecute(Sender: TObject);
begin
   //
end;

procedure TfLog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfLog.SetList(AValue: TSVNLogList);
begin
  if FList = AValue then
    Exit;
  FList := AValue;

  UpdateList;

end;

procedure TfLog.SetCurrentFile(AValue: String);
begin
  if FCurrentFile=AValue then Exit;
  FCurrentFile:=AValue;
end;

procedure TfLog.UpdateList;
var
  i: integer;
  item: TListItem;
  svnItem: TSVNLogItem;
begin
  lstREvisions.Clear;
  if not Assigned(FList) then
    exit;
  lstREvisions.BeginUpdate;
  for i := 0 to FList.Count - 1 do
  begin
    item := lstREvisions.Items.Add;
    svnItem := FList.Items[i];
    item.Data := svnItem;
    item.Caption := IntToStr(svnItem.Revision);
    Item.SubItems.Add(svnItem.Author);
    Item.SubItems.Add(DateTimeToStr(svnItem.DateSVN));
    Item.SubItems.Add(ReplaceLineEndings(svnItem.Message, ' '));
  end;
  lstREvisions.EndUpdate;
  lstREvisions.Items[0].Selected := True;

end;

end.


