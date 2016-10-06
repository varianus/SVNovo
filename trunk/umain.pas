{
This file is part of OvoRepo
Copyright (C) 2011 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}

unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls, ActnList, ShellCtrls, SVNClasses;

type

  { TfMain }

  TFileTreeNode = class ( TTreeNode)
    public
      FullPath: string;
    end;

  TfMain = class(TForm)
    actCommit: TAction;
    actRefresh: TAction;
    actShowUnversioned: TAction;
    actFlatMode: TAction;
    actShowModified: TAction;
    actShowUnmodified: TAction;
    actShowConflict: TAction;
    actUpdate: TAction;
    ActionList: TActionList;
    ImageList_22x22: TImageList;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SVNFileListView: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tvBookMark: TTreeView;
    procedure actCommitExecute(Sender: TObject);
    procedure actFlatModeExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actShowConflictExecute(Sender: TObject);
    procedure actShowModifiedExecute(Sender: TObject);
    procedure actShowUnmodifiedExecute(Sender: TObject);
    procedure actShowUnversionedExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure SVNFileListViewData(Sender: TObject; Item: TListItem);
    procedure SVNFileListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tvBookMarkClick(Sender: TObject);
    procedure tvBookMarkCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
  private
    SVNClient : TSVNClient;
    RepositoryPath: string;
    Filter: TSVNItemStatusSet;
    procedure LoadBookmarks;
    procedure LoadTree(Node: TTreeNode; BasePath: string);
    procedure Log(Sender: TObject; const Status: TSVNItemStatus;
      const fileName: TFileName);
    procedure UpdateFilesListView;
  public

  end;

var
  fMain: TfMain;

implementation
uses LazFileUtils, Config, FilesSupport;
{$R *.lfm}

{ TfMain }

procedure TfMain.UpdateFilesListView;
var
  i: integer;
  Item: TListItem;
  SVNItem: TSVNStatusItem;
  Path: string;
  imgIdx: integer;

begin
  SVNFileListView.BeginUpdate;
  try
  SVNFileListView.Clear;
  for i := 0 to SVNClient.List.Count -1 do
    begin
      SVNItem := SVNClient.List[i];
      if not (SVNItem.ItemStatus in Filter) then
        Continue;
      Item := SVNFileListView.Items.Add;
      Item.Data:=SVNItem;

      with item do
         begin
       //checkboxes
         Caption := '';
         Checked := SVNItem.Checked;
         //path
         Path := SVNItem.Path;
         if pos(SVNClient.RepositoryPath, Path) = 1 then
           path := CreateRelativePath(path, SVNClient.RepositoryPath, false);
         SubItems.Add(Path);

         if (SVNItem.ItemStatus <> sisUnversioned) and
            (SVNItem.ItemStatus <> sisAdded) then
           begin
             SubItems.Add(IntToStr(SVNItem.Revision));
             SubItems.Add(IntToStr(SVNItem.CommitRevision));
             SubItems.Add(SVNItem.Author);
             if (SVNItem.DateModified) <> 0 then
               SubItems.Add(DateTimeToStr(SVNItem.DateModified))
             else
               SubItems.Add('');
             SubItems.Add(SVNItem.Extension);
             SubItems.Add(DateTimeToStr(SVNItem.DateSVN));
           end
         else
           begin
             SubItems.Add('');
             SubItems.Add('');
             SubItems.Add('');
             if (SVNItem.DateModified) <> 0 then
               SubItems.Add(DateTimeToStr(SVNItem.DateModified))
             else
               SubItems.Add('');

             SubItems.Add(SVNItem.Extension);
             SubItems.Add('');
           end;

         //file status
         SubItems.Add(TSVNClient.ItemStatusToStatus(SVNItem.ItemStatus));
         //property status
         SubItems.Add(SVNItem.PropStatus);
         //check if file is versioned
         Case SVNItem.ItemStatus of
          sisAdded:       if SVNItem.IsFolder then imgidx :=  2 else imgidx :=  1;
          sisConflicted:  if SVNItem.IsFolder then imgidx := -1 else imgidx :=  9;
          sisDeleted:     if SVNItem.IsFolder then imgidx := 13 else imgidx := 12;
          sisExternal:    if SVNItem.IsFolder then imgidx := 15 else imgidx := -1;
          sisIgnored:     if SVNItem.IsFolder then imgidx := -1 else imgidx := -1;
          sisIncomplete:  if SVNItem.IsFolder then imgidx := -1 else imgidx := -1;
          sisMerged:      if SVNItem.IsFolder then imgidx := -1 else imgidx := 42;
          sisMissing:     if SVNItem.IsFolder then imgidx := 44 else imgidx := 43;
          sisModified:    if SVNItem.IsFolder then imgidx := 46 else imgidx := 45;
          sisNone:        if SVNItem.IsFolder then imgidx := -1 else imgidx := -1;
          sisNormal:      if SVNItem.IsFolder then imgidx := 18 else imgidx := 56;
          sisObstructed:  if SVNItem.IsFolder then imgidx := -1 else imgidx := -1;
          sisReplaced:    if SVNItem.IsFolder then imgidx := 63 else imgidx := 62;
          sisUnversioned: if SVNItem.IsFolder then imgidx := 54 else imgidx := 53;
        else
          imgidx := -1;
        end;
        ImageIndex:= imgidx;
        StateIndex:= imgidx;

       end;

    end;

  finally
    SVNFileListView.EndUpdate;
  end;

  //if Assigned(SVNClient) then
  //   SVNFileListView.Items.Count:= SVNClient.List.Count;
end;

procedure TfMain.LoadBookmarks;
var
  st: TStringList;
  i: integer;
  item: TFileTreeNode;
begin
  st := TStringList.Create;
  tvBookMark.Items[0].DeleteChildren;
  ConfigObj.ReadStrings('Repositories/Path', St);
  for i := 0 to st.Count -1 do
    begin
      item := TFileTreeNode(tvBookMark.Items.AddChild(tvBookMark.Items[0], st[i]));
      item.FullPath:= st[i];
      item.ImageIndex:= 5;
      item.HasChildren:=true;
    end;
  tvBookMark.Items[0].Expand(False);
  st.free;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  st: TStringList;
begin
  LoadBookmarks;

  SVNClient := TSVNClient.Create();
  SVNClient.SVNExecutable:= ConfigObj.ReadString('SVN/Executable', SVNClient.SVNExecutable);

  //ConfigObj.WriteString('SVN/Executable',SVNExecutable);
  //st:= TStringList.Create;
  //st.Add(SVNClient.RepositoryPath);
  //st.Add(SVNClient.RepositoryPath+'!!');
  //ConfigObj.WriteStrings('Repositories/Path', st);
  //st.free;

  SetColumn(SVNFileListView, 0, 25, '', False, taLeftJustify);
  SetColumn(SVNFileListView, 1, 200, rsPath, true, taLeftJustify);
  SetColumn(SVNFileListView, 2, 75, rsRevision, True, taRightJustify);
  SetColumn(SVNFileListView, 3, 75, rsCommitRevision, True, taRightJustify);
  SetColumn(SVNFileListView, 4, 75, rsAuthor, True, taLeftJustify);
  SetColumn(SVNFileListView, 5, 75, rsDateModified, True, taRightJustify);
  SetColumn(SVNFileListView, 6, 75, rsExtension, True, taLeftJustify);
  SetColumn(SVNFileListView, 7, 75, rsDateSVN, True, taRightJustify);
  SetColumn(SVNFileListView, 8, 100, rsFileStatus, True, taLeftJustify);
  SetColumn(SVNFileListView, 9, 125, rsPropertyStatus, True, taLeftJustify);

  Filter:=[sisAdded,
           sisConflicted,
           sisDeleted,
           sisExternal,
           sisIgnored,
           sisIncomplete,
           sisMerged,
           sisMissing,
           sisModified,
           sisNone,
           sisNormal,
           sisObstructed,
           sisReplaced,
           sisUnversioned];
  UpdateFilesListView;

  ConfigObj.Flush;

end;

procedure TfMain.Log (Sender: TObject; const Status: TSVNItemStatus; const fileName: TFileName);
begin
  Memo1.Lines.Add(fileName);
end;

procedure TfMain.actUpdateExecute(Sender: TObject);
begin
  SVNClient.OnUpdate:=@log ;
  SVNClient.Update();
end;

procedure TfMain.actCommitExecute(Sender: TObject);
begin
//
end;

procedure TfMain.actFlatModeExecute(Sender: TObject);
begin
  SVNClient.FlatMode:= actFlatMode.Checked;
  UpdateFilesListView;
end;

procedure TfMain.actRefreshExecute(Sender: TObject);
begin
  SVNClient.LoadStatus;
  UpdateFilesListView;
end;

procedure TfMain.actShowConflictExecute(Sender: TObject);
begin
  actShowConflict.Checked := not actShowConflict.Checked;
  if actShowConflict.Checked then
    Filter:=Filter + [sisConflicted]
  else
    Filter:=Filter - [sisConflicted];

  UpdateFilesListView;
end;

procedure TfMain.actShowModifiedExecute(Sender: TObject);
begin
  actShowModified.Checked := not actShowModified.Checked;
  if actShowModified.Checked then
    Filter:=Filter + [sisAdded, sisDeleted, sisConflicted, sisModified]
  else
    Filter:=Filter - [sisAdded, sisDeleted, sisConflicted, sisModified];
  UpdateFilesListView;
end;

procedure TfMain.actShowUnmodifiedExecute(Sender: TObject);
begin
  actShowUnmodified.Checked := not actShowUnmodified.Checked;
  if actShowUnmodified.Checked then
    Filter:=Filter + [sisNormal]
  else
    Filter:=Filter - [sisNormal];
  UpdateFilesListView;
end;

procedure TfMain.actShowUnversionedExecute(Sender: TObject);
begin
  actShowUnversioned.Checked := not actShowUnversioned.Checked;
  if actShowUnversioned.Checked then
    Filter:=Filter + [sisUnversioned]
  else
    Filter:=Filter - [sisUnversioned];
  UpdateFilesListView;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  SVNClient.Free;
end;

procedure TfMain.SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn
  );
begin
  case Column.Index of
    0: SVNClient.List.ReverseSort(siChecked);
    1: SVNClient.List.ReverseSort(siPath);
    2: SVNClient.List.ReverseSort(siRevision);
    3: SVNClient.List.ReverseSort(siCommitRevision);
    4: SVNClient.List.ReverseSort(siAuthor);
    5: SVNClient.List.ReverseSort(siDateModified);
    6: SVNClient.List.ReverseSort(siExtension);
    7: SVNClient.List.ReverseSort(siDateSVN);
    8: SVNClient.List.ReverseSort(siItemStatus);
    9: SVNClient.List.ReverseSort(siPropStatus);
  end;

  UpdateFilesListView;
end;

procedure TfMain.SVNFileListViewData(Sender: TObject; Item: TListItem);
var
  StatusItem : TSVNStatusItem;
  Path: string;
  imgidx: integer;
begin
  StatusItem := SVNClient.List.Items[item.index];


end;

procedure TfMain.SVNFileListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Item.Data) then
    TSVNStatusItem(Item.Data).Selected:=Selected;

end;

procedure TfMain.LoadTree(Node: TTreeNode; BasePath: string);
var
  St:TstringList;
  i: integer;
  NewNode: TTreeNode;
begin
  St:= TStringList.Create;
  BuildFolderList(IncludeTrailingPathDelimiter(BasePath) , St, false);
  For i := 0 to St.Count-1 do
    begin
      NewNode := Node.Owner.AddChild(Node, CreateRelativePath(St[i], BasePath, false));
      if NewNode.Text = '' then
        NewNode.Text:= '.';
      TFileTreeNode(NewNode).FullPath:= St[i];
      NewNode.ImageIndex:= 18;
      NewNode.StateIndex:= 18;
      NewNode.SelectedIndex:= 57;
      if Assigned(SVNClient.List.LocateByPath(St[i])) then
        LoadTree(NewNode, St[i]);
    end;
  st.free;

end;

procedure TfMain.tvBookMarkClick(Sender: TObject);
Var
  BookMark: TFileTreeNode;
  i: integer;
begin
  if not Assigned(tvBookMark.Selected) then
      exit;

  BookMark := TFileTreeNode(tvBookMark.Selected);
  if BookMark.AbsoluteIndex = 0 then  exit;

  if (BookMark.Level = 1) and (BookMark.Count = 0) then
    begin
      SVNClient.RepositoryPath := BookMark.FullPath;
      BookMark.DeleteChildren;
      LoadTree(BookMark, BookMark.Text);
    end
  else
   SVNClient.RepositoryPath := BookMark.FullPath;

  UpdateFilesListView;

end;

procedure TfMain.tvBookMarkCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TFileTreeNode;
end;

end.

