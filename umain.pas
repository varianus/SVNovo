unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls, ActnList, SVNClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    ImageList_22: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    SVNFileListView: TListView;
    ToolBar1: TToolBar;
    tvBookMark: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure SVNFileListViewData(Sender: TObject; Item: TListItem);
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
  SVNFileListView.Items.Count:= SVNStatus.List.Count;
end;

procedure TForm1.LoadBookmarks;
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  st: TStringList;
begin

  SVNStatus := nil;
  SetColumn(SVNFileListView, 0, 25, '', False);
  SetColumn(SVNFileListView, 1, 300, rsPath, true);
  SetColumn(SVNFileListView, 2, 75, rsExtension, True);
  SetColumn(SVNFileListView, 3, 100, rsFileStatus, True);
  SetColumn(SVNFileListView, 4, 125, rsPropertyStatus, True);
  SetColumn(SVNFileListView, 5, 75, rsRevision, True);
  SetColumn(SVNFileListView, 6, 75, rsCommitRevision, True);
  SetColumn(SVNFileListView, 7, 75, rsAuthor, True);
  SetColumn(SVNFileListView, 8, 75, rsDate, True);

  SVNStatus := TSVNStatus.Create('C:\source\ovoplayer\trunk', true);

ConfigObj.WriteString('SVN/Executable',SVNExecutable);
st:= TStringList.Create;
st.Add(SVNStatus.RepositoryPath);
st.Add(SVNStatus.RepositoryPath+'!!');
ConfigObj.WriteStrings('Repositories/Path', st);

st.free;

  RepositoryPath:= SVNStatus.RepositoryPath;
  UpdateFilesListView;
  ConfigObj.Flush;

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
begin
  StatusItem := SVNStatus.List.Items[item.index];
  with item do
    begin
  //checkboxes
    Caption := '';
    Checked := StatusItem.Checked;
    //path
    Path := StatusItem.Path;
    if pos(RepositoryPath, Path) = 1 then
      path := CreateRelativePath(path, RepositoryPath, false);
    SubItems.Add(Path);
    //extension
    SubItems.Add(StatusItem.Extension);
    //file status
    SubItems.Add(StatusItem.ItemStatus);
    //property status
    SubItems.Add(StatusItem.PropStatus);
    //check if file is versioned
    Case LowerCase(StatusItem.ItemStatus) of
     'added'      : ImageIndex:=  1;
     'conflicted' : ImageIndex:=  9;
     'deleted'    : ImageIndex:= 12;
 //    'external'  :  ImageIndex:= ;
 //    'ignored'   :  ImageIndex:= ;
 //    'incomplete' :  ImageIndex:= ;
 //    'merged'    :  ImageIndex:= ;
     'missing'   :  ImageIndex:= 43;
 //    'modified'  :  ImageIndex:= ;
 //    'none'      :  ImageIndex:= ;
     'normal'    :  ImageIndex:= 56;
 //    'obstructed'
 //    'replaced'
     'unversioned': ImageIndex:= 53;
    end;

    if (LowerCase(StatusItem.ItemStatus) <> 'unversioned') and
       (LowerCase(StatusItem.ItemStatus) <> 'added') then
    begin
      //revision
      SubItems.Add(IntToStr(StatusItem.Revision));
      //commit revision
      SubItems.Add(IntToStr(StatusItem.CommitRevision));
      //author
      SubItems.Add(StatusItem.Author);
      //date
      SubItems.Add(DateTimeToStr(StatusItem.Date));
    end;
  end;
end;

end.

