unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls, ActnList, SVNClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    actCommit: TAction;
    actShowUnversioned: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    actUpdate: TAction;
    ActionList1: TActionList;
    ImageList_22: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    SVNFileListView: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tvBookMark: TTreeView;
    procedure actCommitExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure SVNFileListViewData(Sender: TObject; Item: TListItem);
    procedure tvBookMarkClick(Sender: TObject);
  private
    SVNStatus : TSVNStatus;
    RepositoryPath: string;
    procedure LoadBookmarks;
    procedure UpdateFilesListView;
  public

  end;

var
  Form1: TForm1;

implementation
uses LazFileUtils, Config;
{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateFilesListView;
var
  i: integer;
begin
  SVNFileListView.Clear;
  if Assigned(SVNStatus) then
     SVNFileListView.Items.Count:= SVNStatus.List.Count;
end;

procedure TForm1.LoadBookmarks;
var
  st: TStringList;
  i: integer;
  item: TTreeNode;
begin
  st := TStringList.Create;
  tvBookMark.Items[0].DeleteChildren;
  ConfigObj.ReadStrings('Repositories/Path', St);
  for i := 0 to st.Count -1 do
    begin
      item := tvBookMark.Items.AddChild(tvBookMark.Items[0], st[i]);
      item.ImageIndex:= 5;
      item.HasChildren:=true;
    end;
  tvBookMark.Items[0].Expand(False);
  st.free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  st: TStringList;
begin
  LoadBookmarks;

  SVNStatus := nil;

  //ConfigObj.WriteString('SVN/Executable',SVNExecutable);
  //st:= TStringList.Create;
  //st.Add(SVNStatus.RepositoryPath);
  //st.Add(SVNStatus.RepositoryPath+'!!');
  //ConfigObj.WriteStrings('Repositories/Path', st);
  //st.free;

  SetColumn(SVNFileListView, 0, 25, '', False);
  SetColumn(SVNFileListView, 1, 200, rsPath, true);
  SetColumn(SVNFileListView, 2, 75, rsRevision, True);
  SetColumn(SVNFileListView, 3, 75, rsCommitRevision, True);
  SetColumn(SVNFileListView, 4, 75, rsAuthor, True);
  SetColumn(SVNFileListView, 5, 75, rsDate, True);
  SetColumn(SVNFileListView, 6, 75, rsExtension, True);
  SetColumn(SVNFileListView, 7, 100, rsFileStatus, True);
  SetColumn(SVNFileListView, 8, 125, rsPropertyStatus, True);
  UpdateFilesListView;

  ConfigObj.Flush;

end;

procedure TForm1.actUpdateExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.actCommitExecute(Sender: TObject);
begin
//
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SVNStatus.Free;
end;

procedure TForm1.SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn
  );
begin
  case Column.Index of
    0: SVNStatus.ReverseSort(siChecked);
    1: SVNStatus.ReverseSort(siPath);
    2: SVNStatus.ReverseSort(siExtension);
    3: SVNStatus.ReverseSort(siItemStatus);
    4: SVNStatus.ReverseSort(siPropStatus);
    5: SVNStatus.ReverseSort(siAuthor);
    6: SVNStatus.ReverseSort(siRevision);
    7: SVNStatus.ReverseSort(siCommitRevision);
    8: SVNStatus.ReverseSort(siDate);
  end;

  UpdateFilesListView;
end;

procedure TForm1.SVNFileListViewData(Sender: TObject; Item: TListItem);
var
  StatusItem : TSVNStatusItem;
  Path: string;
  imgidx: integer;
begin
  StatusItem := SVNStatus.List.Items[item.index];
  with item do
    begin
  //checkboxes
    Caption := '';
    Checked := StatusItem.Checked;
    //path
    Path := StatusItem.Path;
    if pos(SVNStatus.RepositoryPath, Path) = 1 then
      path := CreateRelativePath(path, SVNStatus.RepositoryPath, false);
    SubItems.Add(Path);

    if (StatusItem.ItemStatus <> sisUnversioned) and
       (StatusItem.ItemStatus <> sisAdded) then
    begin
      //revision
      SubItems.Add(IntToStr(StatusItem.Revision));
      //commit revision
      SubItems.Add(IntToStr(StatusItem.CommitRevision));
      //author
      SubItems.Add(StatusItem.Author);
      //date
      SubItems.Add(DateTimeToStr(StatusItem.Date));
    end
    else
    begin
      //revision
      SubItems.Add('');
      //commit revision
      SubItems.Add('');
      //author
      SubItems.Add('');
      //date
      SubItems.Add('');
    end;

    //extension
    SubItems.Add(StatusItem.Extension);
    //file status
    SubItems.Add(TSVNStatus.ItemStatusToStatus(StatusItem.ItemStatus));
    //property status
    SubItems.Add(StatusItem.PropStatus);
    //check if file is versioned
    Case StatusItem.ItemStatus of
     sisAdded:       if StatusItem.IsFolder then imgidx :=  2 else imgidx :=  1;
     sisConflicted:  if StatusItem.IsFolder then imgidx := -1 else imgidx :=  9;
     sisDeleted:     if StatusItem.IsFolder then imgidx := 13 else imgidx := 12;
     sisExternal:    if StatusItem.IsFolder then imgidx := 15 else imgidx := -1;
     sisIgnored:     if StatusItem.IsFolder then imgidx := -1 else imgidx := -1;
     sisIncomplete:  if StatusItem.IsFolder then imgidx := -1 else imgidx := -1;
     sisMerged:      if StatusItem.IsFolder then imgidx := -1 else imgidx := 42;
     sisMissing:     if StatusItem.IsFolder then imgidx := 44 else imgidx := 43;
     sisModified:    if StatusItem.IsFolder then imgidx := 46 else imgidx := 45;
     sisNone:        if StatusItem.IsFolder then imgidx := -1 else imgidx := -1;
     sisNormal:      if StatusItem.IsFolder then imgidx := 18 else imgidx := 56;
     sisObstructed:  if StatusItem.IsFolder then imgidx := -1 else imgidx := -1;
     sisReplaced:    if StatusItem.IsFolder then imgidx := 63 else imgidx := 62;
     sisUnversioned: if StatusItem.IsFolder then imgidx := 54 else imgidx := 53;
   else
     imgidx := -1;
   end;
   ImageIndex:= imgidx;
   StateIndex:= imgidx;

  end;
end;

procedure TForm1.tvBookMarkClick(Sender: TObject);
begin
  if Assigned(SVNStatus) then
     begin
      SVNStatus.Free;
     end;
   SVNStatus := TSVNStatus.Create(tvBookMark.Selected.Text, true);

   UpdateFilesListView;
end;

end.

