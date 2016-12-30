unit formlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, SVNClasses;

type

  { TfLog }

  TfLog = class(TForm)
    bClose: TButton;
    lvAffectedFiles: TListView;
    lstREvisions: TListView;
    mMessage: TMemo;
    pcInfo: TPageControl;
    tsMessage: TTabSheet;
    tsFiles: TTabSheet;
    procedure bCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstREvisionsSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    FList: TSVNLogList;
    procedure SetList(AValue: TSVNLogList);
    procedure UpdateList;
  public

    property List: TSVNLogList read FList write SetList;
  end;

var
  fLog: TfLog;

implementation
uses strutils;
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
    mMessage.Lines.Text := Item.SubItems[2];
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

procedure TfLog.bCloseClick(Sender: TObject);
begin
  Close;
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
    Item.SubItems.Add(DelChars(DelChars(svnItem.Message, #10), #13));
  end;
  lstREvisions.EndUpdate;
  lstREvisions.Items[0].Selected := True;

end;

end.


