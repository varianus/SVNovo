{ Copyright (C) 2008 Darius Blaszijk

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

unit SVNClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, fileutil, LazFileUtils, UTF8Process, LCLProc, Controls,
  XMLRead, DOM, Process, StdCtrls, Forms, fgl;

resourcestring
  rsAction = 'Action';
  rsAdded = 'Added';
  rsAdd = 'Add to version control';
  rsAuthor = 'Author';
  rsCommit = 'Commit';
  rsCommitRevision = 'Commit revision';
  rsConflict = 'Conflict';
  rsCopyFromPath = 'Copy from path';
  rsCreatePatchFile = 'Create patch file';
  rsDate = 'Date';
  rsDelete = 'Delete';
  rsDeleted = 'Deleted';
  rsDiffActiveFile = 'Diff active file';
  rsEdit = 'Edit';
  rsExtension = 'Extension';
  rsFileNotInWorkingCopyAnymore = 'File is not part of local working copy (anymore)';
  rsFileStatus = 'File status';
  rsIndexOutOfBoundsD = 'Index out of bounds (%d)';
  rsLazarusSVNCommit = 'LazarusSVN Commit';
  rsLazarusSVNDiff = '%s - LazarusSVN Diff ...';
  rsLazarusSVNLog = '%s - LazarusSVN Log ...';
  rsLazarusSVNUpdate = '%s - LazarusSVN Update ...';
  rsCommitMsgHistory = 'Commit Message History';
  rsCommitMsg = 'Commit Message';
  rsMerged = 'Merged';
  rsMessage = 'Message';
  rsNoAuthor = '(no author)';
  rsOpenFileInEditor = 'Open file in editor';
  rsOpenThisRevisionInEditor = 'Open this revision in editor';
  rsOpenPreviousRevisionInEditor = 'Open previous revision in editor';
  rsOnlyModifiedItemsCanBeDiffed = 'Only modified (M) Items can be diffed';
  rsPath = 'Path';
  rsProjectFilename = 'Project filename';
  rsProjectIsActive = 'Project is active';
  rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst = 'Project is not '
    +'active in SVN settings, please activate first.';
  rsProjectName = 'Project name';
  rsProjectOptions = 'Project options';
  rsPropertyStatus = 'Property status';
  rsRemove = 'Remove from version control (keep local)';
  rsRepositoryPath = 'Repository path';
  rsRevert = 'Revert';
  rsRevision = 'Revision';
  rsSave = 'Save';
  rsSettings = 'Settings';
  rsSVNSettings = 'SVN settings';
  rsShowDiff = 'Show diff';
  rsShowDiffBase = 'Show Diff of Local Changes';
  rsShowDiffPrev = 'Show Diff Against Previous Version';
  rsShowDiffHead = 'Show Diff Against HEAD';
  rsShowDiffCountRev = 'Show Last X Commits';
  rsRefresh = 'Refresh';
  rsShowLog = 'Show log';
  rsSourceFileDoesNotBelongToTheProjectPleaseAddFirst = 'Source file does not '
    +'belong to the project. Please add first.';
  rsSVNTools = 'SVN tools';
  rsUpdate = 'Update';
  rsUpdated = 'Updated';

const
   READ_BYTES = 2048;
   SVN_REPOSITORY = 'SVN repository';
   SVN_ACTIVE = 'SVN active';

type
  TSortDirection  = (sdAscending, sdDescending);

  TStatusItemName = (siChecked, siPath, siExtension, siPropStatus, siItemStatus,
                     siRevision, siCommitRevision, siAuthor, siDate);

  TSVNItemStatus = (
     sisAdded,
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
     sisUnversioned);

//  PSVNStatusItem = ^TSVNStatusItem;

  { TSVNStatusItem }

  TSVNStatusItem = class //record
    Checked: boolean;
    Path: string;
    Extension: string;
    PropStatus: string;
    ItemStatus: TSVNItemStatus;
    Revision: integer;
    CommitRevision: integer;
    Author: string;
    Date: TDate;
    Kind: integer;
    Function IsFolder : boolean;
  end;


  TSVNStatusList = class (specialize TFPGObjectList<TSVNStatusItem>)
  private
    FSortDirection: TSortDirection;
    FSortItem: TStatusItemName;
  public
    procedure Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);  overload;
    procedure Sort;  overload;
    procedure ReverseSort(ASortItem: TStatusItemName);
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property SortItem: TStatusItemName read FSortItem write FSortItem;
  end;

  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FFlatMode: boolean;
    FRepositoryPath: string;
    fSvnExecutable: string;
    Verbose: boolean;
    function ExecuteSvnReturnXml(ACommand: string): TXMLDocument;
    function FindSvnExecutable: string;
    function GetSvnExecutable: string;
    procedure SetFlatMode(AValue: boolean);
    procedure SetRepositoryPath(AValue: string);
  public
    List: TSVNStatusList; // TFPList;
    Class Function StatusToItemStatus(sStatus: string): TSVNItemStatus; inline;
    Class Function ItemStatusToStatus(Status: TSVNItemStatus): string; inline;
    //
    constructor Create(const ARepoPath: string='');
    destructor Destroy; override;
    //
    Property SVNExecutable: string read GetSvnExecutable write fSvnExecutable;
    procedure UpdateStatus;
    property RepositoryPath: string read FRepositoryPath write SetRepositoryPath;
    Property FlatMode: boolean read FFlatMode write SetFlatMode;
  end;

procedure CmdLineToMemo(CmdLine: string; Memo: TMemo);
procedure SetColumn(ListView: TListView; ColNo, DefaultWidth: integer; AName: string; AutoSize: boolean = true);
function ReplaceLineEndings(const s, NewLineEnds: string): string;
function ISO8601ToDateTime(ADateTime: string): TDateTime;

implementation

uses math;


function CompareBoolean (Const A, B: Boolean): Integer;
const
   BoolOrder: Array [False..True] Of Integer = (0,1); // o 1,0 se si desidera ordinare il contrario
Begin
   result := BoolOrder [A] - BoolOrder [B];
End ;

procedure CmdLineToMemo(CmdLine: string; Memo: TMemo);
var
  AProcess: TProcessUTF8;
  BytesRead: LongInt;
  n: LongInt;
  M: TMemoryStream;

  procedure UpdateMemoFromStream;
  var
    s: string;
  begin
    if BytesRead > 0 then begin
      SetLength(s, BytesRead);
      M.Read(s[1], BytesRead);

      // this woks exactly like Append() only without the newline bug
      Memo.SelText := ReplaceLineEndings(s, LineEnding);

      M.SetSize(0);
      BytesRead:=0;
    end;
  end;

begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.CommandLine := CmdLine;
  debugln('CmdLineToMemo commandline=', AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  M := TMemoryStream.Create;
  BytesRead := 0;
  Memo.Lines.Text := '';

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
      UpdateMemoFromStream;
      Application.ProcessMessages;
    end
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
      UpdateMemoFromStream;
      Application.ProcessMessages;
    end;
  until n <= 0;

  AProcess.Free;
  M.Free;

  Memo.Cursor:=crDefault;
end;

procedure SetColumn(ListView: TListView; ColNo, DefaultWidth: integer; AName: string; AutoSize: boolean = true);
begin
  ListView.Column[ColNo].Caption:=AName;
  ListView.Column[ColNo].AutoSize:=AutoSize;
  ListView.Column[ColNo].Width:=DefaultWidth;
end;


function ReplaceLineEndings(const s, NewLineEnds: string): string;
var
  p: Integer;
  StartPos: LongInt;
begin
  Result:=s;
  p:=1;
  while (p<=length(Result)) do begin
    if Result[p] in [#10,#13] then begin
      StartPos:=p;
      if (p<length(Result))
      and (Result[p+1] in [#10,#13]) and (Result[p]<>Result[p+1]) then
        inc(p);
      Result:=copy(Result,1,StartPos-1)+NewLineEnds+copy(Result,p+1,length(Result));
      inc(p,length(NewLineEnds));
    end else begin
      inc(p);
    end;
  end;
end;

function ISO8601ToDateTime(ADateTime: string): TDateTime;
var
  y, m, d, h, n, s: word;
begin
  y := StrToInt(Copy(ADateTime, 1, 4));
  m := StrToInt(Copy(ADateTime, 6, 2));
  d := StrToInt(Copy(ADateTime, 9, 2));
  h := StrToInt(Copy(ADateTime, 12, 2));
  n := StrToInt(Copy(ADateTime, 15, 2));
  s := StrToInt(Copy(ADateTime, 18, 2));

  Result := ComposeDateTime( EncodeDate(y,m,d), EncodeTime(h,n,s,0));
end;

function SortPathAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
   if Result = 0 then
      Result := CompareText(Item1.Path, Item2.Path);
end;

function SortPathDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result = 0 then
     Result := -CompareText(Item1.Path, Item2.Path);

end;

function SortSelectedAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   if Item1.Checked > Item2.Checked then
     Result := 1
   else
     if Item1.Checked = Item2.Checked then
       Result := SortPathDescending(Item1, Item2)
     else
       Result := -1;
end;

function SortSelectedDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortSelectedAscending(Item1, Item2);
end;

function SortExtensionAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   Result := CompareText(Item1.Extension, Item2.Extension);
end;

function SortExtensionDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortExtensionAscending(Item1, Item2);
end;

function SortItemStatusAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result:=0;
// Result := CompareValue(Item1.ItemStatus, Item2.ItemStatus);
end;

function SortItemStatusDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortItemStatusAscending(Item1, Item2);
end;

function SortPropStatusAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   Result := CompareText(Item1.PropStatus, Item2.PropStatus);
end;

function SortPropStatusDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortPropStatusAscending(Item1, Item2);
end;

function SortPropertyAuthorAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   Result := CompareText(Item1.Author, Item2.Author);
end;

function SortPropertyAuthorDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortPropertyAuthorAscending(Item1, Item2);
end;

function SortPropertyRevisionAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := CompareValue(Item1.Revision, Item2.Revision);
end;

function SortPropertyRevisionDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortPropertyRevisionAscending(Item1, Item2);
end;

function SortPropertyCommitRevisionAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
   Result := CompareValue(Item1.CommitRevision, Item2.CommitRevision);
end;

function SortPropertyCommitRevisionDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortPropertyCommitRevisionAscending(Item1, Item2);
end;

function SortPropertyDateAscending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := CompareValue(Item1.Date, Item2.Date);
end;

function SortPropertyDateDescending(const Item1, Item2: TSVNStatusItem): Integer;
begin
  Result := -SortPropertyDateAscending(Item1, Item2);
end;

function TSVNClient.ExecuteSvnReturnXml(ACommand: string): TXMLDocument;
var
  AProcess: TProcessUTF8;
  M: TMemoryStream;
  n, BytesRead: Integer;
begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.CommandLine := SVNExecutable + ' ' + ACommand;
  debugln('TSVNLogFrm.ExecuteSvnReturnXml CommandLine ' + AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  M := TMemoryStream.Create;
  BytesRead := 0;

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end
    else begin
      // no data, wait 100 ms
      Sleep(100);
    end;
  end;

  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until n <= 0;
  M.SetSize(BytesRead);

  ReadXMLFile(Result, M);

  M.Free;
  AProcess.Free;
end;

{ TSVNStatusItem }

function TSVNStatusItem.IsFolder: boolean;
begin
  Result := Kind = 2;
end;

{ TSVNClient }

class function TSVNClient.StatusToItemStatus(sStatus: string): TSVNItemStatus;
begin
  Case sStatus of
    'added'      :  Result :=  sisAdded;
    'conflicted' :  Result :=  sisConflicted;
    'deleted'    :  Result :=  sisDeleted;
    'external'   :  Result :=  sisExternal;
    'ignored'    :  Result :=  sisIgnored;
    'incomplete' :  Result :=  sisIncomplete;
    'merged'     :  Result :=  sisMerged;
    'missing'    :  Result :=  sisMissing;
    'modified'   :  Result :=  sisModified;
    'none'       :  Result :=  sisNone;
    'normal'     :  Result :=  sisNormal;
    'obstructed' :  Result :=  sisObstructed;
    'replaced'   :  Result :=  sisReplaced;
    'unversioned':  Result :=  sisUnversioned;
  else
    Result := sisNone;
  end;
end;

class function TSVNClient.ItemStatusToStatus(Status: TSVNItemStatus): string;
begin
  Case Status of
    sisAdded:       Result := 'added';
    sisConflicted:  Result := 'conflicted';
    sisDeleted:     Result := 'deleted';
    sisExternal:    Result := 'external';
    sisIgnored:     Result := 'ignored';
    sisIncomplete:  Result := 'incomplete';
    sisMerged:      Result := 'merged';
    sisMissing:     Result := 'missing';
    sisModified:    Result := 'modified';
    sisNone:        Result := 'none';
    sisNormal:      Result := 'normal';
    sisObstructed:  Result := 'obstructed';
    sisReplaced:    Result := 'replaced';
    sisUnversioned: Result := 'unversioned';
  else
    Result := '';
  end;

end;

constructor TSVNClient.Create(const ARepoPath: string);
begin
  Verbose := true;
  FFlatMode:= false;
  List := TSVNStatusList.Create;
  if ARepoPath <> EmptyStr then
     begin
       FRepositoryPath:=ARepoPath;
       UpdateStatus;
     end;

end;

procedure TSVNClient.UpdateStatus;
var
  ActNode: TDOMNode;
  Doc: TXMLDocument;
  F: LongInt;
  i: integer;
  ListItem: TSVNStatusItem;
  Node: TDOMNode;
  NodeName: string;
  NodeValue: string;
  Path: string;
  SubNode: TDOMNode;
  Command : string;
begin
  List.Clear;

  if FRepositoryPath = EmptyStr then
    exit;

  if Verbose then
    Command := ('stat --verbose --xml "' + RepositoryPath  + '" --non-interactive')
  else
    Command := ('stat --xml "' + RepositoryPath  + '" --non-interactive');

  if fFlatMode then
    Command := Command + ' --depth=infinity'
  else
    Command := Command + ' --depth=immediates';


  Doc := ExecuteSvnReturnXml(Command);
  Node := Doc.DocumentElement.FirstChild.FirstChild;
  if Node = nil then begin
    // no <entry> node found, list is empty.
    Doc.Free;
    Exit();
  end;

  repeat
    SubNode := Node;
    Path := UTF8Encode(SubNode.Attributes.Item[0].NodeValue);
    debugln('TSVNStatus.Create ' + Path);
    F:=FileGetAttr(Path);
    If (F<>-1) {and ((F and faDirectory)=0)} then
    begin
      ListItem := TSVNStatusItem.Create;
      //initialize author (anonymous repositories)
      ListItem.Author := rsNoAuthor;
      //path
      ListItem.Path := Path;
      //Extension
      ListItem.Extension:=ExtractFileExt(Path);
      //get the wc-status attributes
      ListItem.ItemStatus:=sisNone;
      ListItem.Checked:=False;
      ListItem.PropStatus:='';
      if (F and faDirectory)=faDirectory then
        ListItem.Kind:= 2
      else
        ListItem.Kind:= 1;
      for i := 0 to SubNode.ChildNodes.Item[0].Attributes.Length -1 do
      begin
        NodeName := SubNode.ChildNodes.Item[0].Attributes.Item[i].NodeName;
        NodeValue := SubNode.ChildNodes.Item[0].Attributes.Item[i].NodeValue;
        if NodeName = 'item' then
        begin
          //ItemStatus
          ListItem.ItemStatus := StatusToItemStatus(NodeValue);
          //Checked
          ListItem.Checked:=(NodeValue<>'unversioned') and (NodeValue<>'normal');
        end;
        if NodeName = 'props' then
          //PropStatus
          ListItem.PropStatus := NodeValue;
        if NodeName = 'revision' then
          //Revision
          ListItem.Revision := StrToInt(NodeValue);
      end;
      //get the commit attributes
      SubNode := SubNode.ChildNodes.Item[0].ChildNodes.Item[0];
      if Assigned(SubNode) then
      begin
        //CommitRevision
        ListItem.CommitRevision:=StrToInt(SubNode.Attributes.Item[0].NodeValue);
        for i := 0 to SubNode.ChildNodes.Count - 1 do
        begin
          ActNode := SubNode.ChildNodes.Item[i];
          if Assigned(ActNode) then
          begin
            NodeName := ActNode.NodeName;
            //Author
            if NodeName = 'author' then
              ListItem.Author := ActNode.FirstChild.NodeValue;
            //Date
            if NodeName = 'date' then
              ListItem.Date := ISO8601ToDateTime(ActNode.FirstChild.NodeValue);
          end;
        end;
      end;
      List.Add(ListItem);
    end;
    Node := Node.NextSibling;
  until not Assigned(Node);
  Doc.Free;
  List.Sort;
end;

procedure TSVNClient.SetFlatMode(AValue: boolean);
begin
  if FFlatMode=AValue then Exit;
  FFlatMode:=AValue;
  UpdateStatus;
end;

function TSVNClient.GetSvnExecutable: string;
begin
  if fSvnExecutable = EmptyStr then
    fSvnExecutable := FindSvnExecutable;
  Result :=  fSvnExecutable;

end;

function TSVNClient.FindSvnExecutable: string;
var
  //Output: string;
  rv:integer;
begin
  while True do
  begin
    Result := FindDefaultExecutablePath('svn') ;
    if FileExists(Result)
       then break;

    {$IFDEF MSWINDOWS}
    // Some popular locations for SlikSVN, Subversion, and TortoiseSVN:
    // Covers both 32 bit and 64 bit Windows.
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles\Subversion\bin\svn.exe')
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)\Subversion\bin\svn.exe')
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles\SlikSvn\bin\svn.exe')
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)\SlikSvn\bin\svn.exe')
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles\TorToiseSVN\bin\svn.exe')
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)\TorToiseSVN\bin\svn.exe')
       else break;
    //Directory where current executable is:
   {$ENDIF MSWINDOWS}
    break;
  end;

  if not FileExists(Result) then
  begin
    //current directory. Note: potential for misuse by malicious program.
    {$ifdef mswindows}
    if FileExists(Result + '.exe') then
      Result := Result + '.exe';
    {$endif mswindows}
  end;


end;

procedure TSVNClient.SetRepositoryPath(AValue: string);
begin
  if FRepositoryPath=AValue then Exit;
  FRepositoryPath:=AValue;
  UpdateStatus;
end;

destructor TSVNClient.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

procedure TSVNStatusList.Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);
begin
  SortDirection := ADirection;
  SortItem := ASortItem;

  if ADirection = sdDescending then
    case ASortItem of
      siChecked:        Sort(@SortSelectedAscending);
      siPath:           Sort(@SortPathAscending);
      siExtension:      Sort(@SortExtensionAscending);
      siItemStatus:     Sort(@SortItemStatusAscending);
      siPropStatus:     Sort(@SortPropStatusAscending);
      siAuthor:         Sort(@SortPropertyAuthorAscending);
      siRevision:       Sort(@SortPropertyRevisionAscending);
      siCommitRevision: Sort(@SortPropertyCommitRevisionAscending);
      siDate:           Sort(@SortPropertyDateAscending);
    end
  else
    case ASortItem of
      siChecked:        Sort(@SortSelectedDescending);
      siPath:           Sort(@SortPathDescending);
      siExtension:      Sort(@SortExtensionDescending);
      siItemStatus:     Sort(@SortItemStatusDescending);
      siPropStatus:     Sort(@SortPropStatusDescending);
      siAuthor:         Sort(@SortPropertyAuthorDescending);
      siRevision:       Sort(@SortPropertyRevisionDescending);
      siCommitRevision: Sort(@SortPropertyCommitRevisionDescending);
      siDate:           Sort(@SortPropertyDateDescending);
    end;
end;

procedure TSVNStatusList.Sort;
begin
  Sort(SortItem, SortDirection);
end;

procedure TSVNStatusList.ReverseSort(ASortItem: TStatusItemName);
begin
  if SortItem = ASortItem then
  begin
     if SortDirection = sdDescending then
       Sort(ASortItem, sdAscending)
     else
       Sort(ASortItem, sdDescending)
  end
  else
    Sort(ASortItem, sdAscending);
end;

end.
