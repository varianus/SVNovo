{ Copyright (C) 2008 Darius Blaszijk
  Copyright (C) 2016 Marco Caselli

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
  Classes, SysUtils, ComCtrls, fileutil, LazFileUtils, Controls,
  Laz2_XMLRead, Laz2_DOM,  StdCtrls, Forms, ProcessRunner, SVNTypes;

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
  rsDateSVN = 'Date';
  rsDateModified = 'Last Changed';
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
  rsName= 'Name';
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
  rsNoCommitMessage = '(No commit message)';

const
   SVN_REPOSITORY = 'SVN repository';
   SVN_ACTIVE = 'SVN active';

type

  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FFlatMode: boolean;
    FOnBeginProcess: TNotifyEvent;
    FOnEndProcess: TNotifyEvent;
    FOnSVNMessage: TRunnerMessage;
    FRepositoryPath: string;
    fSvnExecutable: string;
    Verbose: boolean;
    Runner: TProcessRunner;
    function GetSvnExecutable: string;
    procedure SetFlatMode(AValue: boolean);
    procedure SetOnBeginProcess(AValue: TNotifyEvent);
    procedure SetOnEndProcess(AValue: TNotifyEvent);
    procedure SetOnSVNMessage(AValue: TRunnerMessage);
    procedure SetRepositoryPath(AValue: string);
    procedure SetSvnExecutable(AValue: string);
  Protected
    procedure BeginProcess;
    procedure EndProcess;
    procedure RunnerMessage(Sender: TObject; const MessageKind: TRunnerMessageKind; const Message: string);
  public
    List: TSVNStatusList; // TFPList;
    //
    constructor Create(const ARepoPath: string='');
    destructor Destroy; override;
    //
    // SVN Commands
    procedure LoadStatus;
    Procedure Update(Elements: TStrings; Revision:string = '');
    Procedure CheckOut(URL: string; LocalPath:TFileName; Revision:string = '');
    function Export(Element: string; Revision:string = ''):string; overload;
    function Export(Element: string; Revision:Integer):string;  overload;
    procedure Add(Elements: TStrings; Recursive: boolean=false);
    procedure Revert(Elements: TStrings; Recursive: boolean=false);
    procedure Resolve(Elements: TStrings);
    procedure Commit(Elements: TStrings; Message: string; Recursive: boolean=false);
    function Log(FileName: TFileName): TSVNLogList;
    function Annotate(FileName: TFileName): TSVNAnnotateList;
    procedure CleanUp;
    procedure Upgrade;

    //
    // Support funcs
    Class Function StatusToItemStatus(sStatus: string): TSVNItemStatus; inline;
    Class Function ItemStatusToStatus(Status: TSVNItemStatus): string; inline;
    Class function FindSvnExecutable: string;
    Function FullFileName(FileName: TFilename): TFilename; inline;
    //
    // Properties
    Property SVNExecutable: string read GetSvnExecutable write SetSvnExecutable;
    property RepositoryPath: string read FRepositoryPath write SetRepositoryPath;
    Property FlatMode: boolean read FFlatMode write SetFlatMode;
    //
    //Events
    property OnSVNMessage : TRunnerMessage read FOnSVNMessage write SetOnSVNMessage;
    Property OnBeginProcess: TNotifyEvent read FOnBeginProcess write SetOnBeginProcess;
    Property OnEndProcess: TNotifyEvent read FOnEndProcess write SetOnEndProcess;
  end;

function ISO8601ToDateTime(ADateTime: string): TDateTime;

implementation

uses RegExpr, FilesSupport;

function ISO8601ToDateTime(ADateTime: string): TDateTime;
var
  y, m, d, h, n, s: word;
begin
  try
    y := StrToInt(Copy(ADateTime, 1, 4));
    m := StrToInt(Copy(ADateTime, 6, 2));
    d := StrToInt(Copy(ADateTime, 9, 2));
    h := StrToInt(Copy(ADateTime, 12, 2));
    n := StrToInt(Copy(ADateTime, 15, 2));
    s := StrToInt(Copy(ADateTime, 18, 2));
    Result := ComposeDateTime( EncodeDate(y,m,d), EncodeTime(h,n,s,0));
  Except
    Result := 0;
  end;
end;


{ TSVNClient }

procedure TSVNClient.BeginProcess;
begin
  if Assigned(FOnBeginProcess) then
     FOnBeginProcess(Self);
end;

procedure TSVNClient.EndProcess;
begin
  if Assigned(FOnEndProcess) then
     FOnEndProcess(Self);

end;

procedure TSVNClient.RunnerMessage(Sender: TObject;
  const MessageKind: TRunnerMessageKind; const Message: string);
var
  i: integer;
  Str: string;
  sts: TSVNItemStatus;
  FName: string;
begin

  if not Assigned(FOnSVNMessage)   then
    exit;

  Sts:= sisNone;

  if MessageKind = ieInfo then
    begin
      i := pos(' ', Message);
      str := Copy(Message,1, i - 1);
      Case str of
       'A': sts := sisAdded;
       'D': sts := sisDeleted;
       'U': sts := sisUpdated;
       'C': sts := sisConflicted;
       'G': sts := sisMerged;
      end;
    end;

    begin
      if sts <> sisNone then
        begin
           FName:=Copy(Message, 6, Length(message));
           FOnSVNMessage(Self, MessageKind, format('%s %s',[ItemStatusToStatus(sts),fname]))

        end
      else
        FOnSVNMessage(Self, MessageKind, format('%s %s',[ItemStatusToStatus(sts),Message]));
    end;
end;

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
    'updated'    :  Result :=  sisUnversioned;
  else
    Result := sisNone;
  end;
end;

class function TSVNClient.ItemStatusToStatus(Status: TSVNItemStatus): string;
begin
  Case Status of
    sisAdded:       Result := rsAdded;
    sisConflicted:  Result := 'Conflicted';
    sisDeleted:     Result := rsDeleted;
    sisExternal:    Result := 'external';
    sisIgnored:     Result := 'ignored';
    sisIncomplete:  Result := 'incomplete';
    sisMerged:      Result := rsMerged;
    sisMissing:     Result := 'missing';
    sisModified:    Result := 'modified';
    sisNone:        Result := '';
    sisNormal:      Result := 'normal';
    sisObstructed:  Result := 'obstructed';
    sisReplaced:    Result := 'replaced';
    sisUnversioned: Result := 'unversioned';
    sisUpdated:     Result := rsUpdated;
  else
    Result := '';
  end;

end;

constructor TSVNClient.Create(const ARepoPath: string);
begin
  Runner := TProcessRunner.Create;
  Runner.OnRunnerMessage := @RunnerMessage;
  Verbose := true;
  FFlatMode:= false;
  List := TSVNStatusList.Create(True);
  if ARepoPath <> EmptyStr then
     begin
       FRepositoryPath:=ARepoPath;
       LoadStatus;
     end;

end;

function TSVNClient.Log(FileName: TFileName): TSVNLogList;
var
  ActNode: TDOMNode;
  Doc: TXMLDocument;
  i,j, k: integer;
  ListItem: TSVNLogItem;
  PathItem: TAffectedFile;
  Node: TDOMNode;
  NodeName: string;
  NodeValue: string;
  Path: string;
  SubNode: TDOMNode;
  PathNode: TDOMNode;
  Command : TStringList;
begin
  BeginProcess;
  Result := TSVNLogList.Create(true);
  try
    if FileName = EmptyStr then
      exit;

    Runner.Params.Clear;
    Runner.Params.AddStrings(['log','--xml','-v']);
    Runner.Params.Add('--non-interactive');
    Runner.Params.Add(FileName);

    Doc := Runner.ExecuteReturnXml;


    if not Assigned(Doc) then
       begin
       // Doc is nil, a svn error occurred
         Doc.Free;
         Exit();
       end;

    Node := Doc.DocumentElement.FirstChild;
    if not Assigned(Node) then
      begin
      // no <entry> node found, list is empty.
        Doc.Free;
        Exit();
      end;

    repeat
      SubNode := Node;
      begin
        ListItem := TSVNLogItem.Create;
        //initialize author (anonymous repositories)
        ListItem.Author := rsNoAuthor;

        for i := 0 to SubNode.Attributes.Length -1 do
        begin
          NodeName := SubNode.Attributes.Item[i].NodeName;
          NodeValue := SubNode.Attributes.Item[i].NodeValue;
          if NodeName = 'revision' then
            //Revision
            ListItem.Revision := StrToInt(NodeValue);
        end;
        for i := 0 to SubNode.ChildNodes.Count - 1 do
        begin
          ActNode := SubNode.ChildNodes.Item[i];
          if Assigned(ActNode) then
          begin
            NodeName := ActNode.NodeName;
            if NodeName = 'author' then
              ListItem.Author := ActNode.FirstChild.NodeValue;
            if NodeName = 'date' then
              ListItem.DateSVN := ISO8601ToDateTime(ActNode.FirstChild.NodeValue);
            if NodeName = 'msg' then
              ListItem.Message := ActNode.FirstChild.NodeValue;
            if NodeName = 'paths' then
              for j:= 0 to ActNode.ChildNodes.Count -1 do
               begin
                 PathNode := ActNode.ChildNodes.Item[j];
                 PathItem := TAffectedFile.Create;
                 ListItem.AffectedFiles.Add(PathItem);
                 PathItem.FileName:=PathNode.FirstChild.NodeValue;
                 for k := 0 to PathNode.Attributes.Length -1 do
                 begin
                   NodeName := PathNode.Attributes.Item[k].NodeName;
                   NodeValue := PathNode.Attributes.Item[k].NodeValue;
                   if NodeName = 'action' then
                     PathItem.Action := NodeValue;
                 end;
               end;
          end;
        end;
        Result.Add(ListItem);
      end;
      Node := Node.NextSibling;
    until not Assigned(Node);
    Doc.Free;

  finally
    EndProcess;
  end;

end;

function TSVNClient.Annotate(FileName: TFileName): TSVNAnnotateList;
var
  Res: TStringList;
  RegExp: TRegExpr;
  Item: TSVNAnnotateItem;
  i: integer;
  nItem: Integer;
begin
  BeginProcess;
  Result := TSVNAnnotateList.Create(True);
  try
    if FileName = EmptyStr then
      exit;

    Runner.Params.Clear;
    Runner.Params.AddStrings(['annotate','-v']);
    Runner.Params.Add('--non-interactive');
    Runner.Params.Add(FileName);

    RegExp := TRegExpr.Create('\s*(\d+|-)\s+([^\s]+) (.{44}) (.*)');
    Res := Runner.ExecuteReturnTxt;
    try
      for i := 0 to res.Count -1 do
        begin
          RegExp.Exec(Res[i]);
          Item:= TSVNAnnotateItem.Create;
          item.LineNo:= i + 1;
          item.Revision:= StrToInt64Def(RegExp.Match[1], 0);
          item.Author:= RegExp.Match[2];
          item.DateSVN:= ISO8601ToDateTime(RegExp.Match[3]);
          item.Line:= RegExp.Match[4];
          nItem := Result.Add(Item);
          If nItem = 0 then
             Item.Group:= 1
          else
             if Result.Items[nItem-1].Revision = Item.Revision then
               Item.Group:= Result.Items[nItem-1].Group
             else
               Item.Group:= Result.Items[nItem-1].Group xor 1;
        end;
    finally
      Res.Free;
    end;

  finally
    EndProcess;
  end;

end;


procedure TSVNClient.LoadStatus;
var
  ActNode: TDOMNode;
  Doc: TXMLDocument;
  F: LongInt;
  i: integer;
  ListItem: TSVNItem;
  Node: TDOMNode;
  NodeName: string;
  NodeValue: string;
  Path: string;
  SubNode: TDOMNode;
begin
  BeginProcess;
  try
    List.Clear;

    if FRepositoryPath = EmptyStr then
      exit;

    Runner.Params.Clear;
    Runner.Params.AddStrings(['stat','--xml', '--non-interactive']);

    if Verbose then
      Runner.Params.Add('--verbose');

    if fFlatMode then
      Runner.Params.Add('--depth=infinity')
    else
      Runner.Params.Add('--depth=immediates');

    Runner.Params.Add(FRepositoryPath);

    Doc := Runner.ExecuteReturnXml;
    if not assigned(Doc) then
    begin
      Exit();
    end;

    Node := Doc.DocumentElement.FirstChild.FirstChild;
    if not assigned(Node) then
    begin
      // no <entry> node found, list is empty.
      Doc.Free;
      Exit();
    end;

    repeat
      SubNode := Node;
      Path := UTF8Encode(SubNode.Attributes.Item[0].NodeValue);
//      debugln('TSVNStatus.Create ' + Path);
      F:=FileGetAttr(Path);
      If (F<>-1) {and ((F and faDirectory)=0)} then
      begin
        ListItem := TSVNItem.Create;
        //initialize author (anonymous repositories)
        ListItem.Author := rsNoAuthor;
        //path
        ListItem.Path := Path;
        ListItem.Name := ExtractFileName(Path);
        //Extension
        ListItem.Extension:=ExtractFileExt(Path);
        //get the wc-status attributes
        ListItem.ItemStatus:=sisNone;
        FileAge(Path, ListItem.DateModified);
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
                ListItem.DateSVN := ISO8601ToDateTime(ActNode.FirstChild.NodeValue);
            end;
          end;
        end;
        List.Add(ListItem);
      end;
      Node := Node.NextSibling;
    until not Assigned(Node);
    Doc.Free;
    List.Sort;

  finally
    EndProcess;
  end;

end;

procedure TSVNClient.Update(Elements: TStrings; Revision:string = '');
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['update','--non-interactive', '--trust-server-cert']);

    if (Revision <> '') and (trim(Revision) <> REV_HEAD) then
      Runner.Params.Add('-r ' + Revision);

    if not Assigned(Elements) or (Elements.Count = 0) then
      Runner.Params.Add(FRepositoryPath)
    else
      Runner.Params.AddStrings(Elements);

    Runner.Execute;
  finally
    EndProcess;
  end;
end;

procedure TSVNClient.CheckOut(URL: string; LocalPath: TFileName;
  Revision: string);
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['checkout','--non-interactive', '--trust-server-cert']);

    if (Revision = '') then
       Revision := REV_HEAD;

    if (Revision <> '')  then
      begin
        Runner.Params.Add('--revision='+Revision);
      end;

    Runner.Params.Add(URL);
    Runner.Params.Add(LocalPath);
    Runner.Execute;
  finally
    EndProcess;
  end;

end;

function TSVNClient.Export(Element: string; Revision: string): string;
begin
  BeginProcess;
  try
    Runner.params.Clear;
    Runner.Params.AddStrings(['export','--non-interactive', '--force', '--trust-server-cert']);

    if (Revision = '') then
       Revision := REV_HEAD;

    if (Revision <> '')  then
      begin
        Runner.Params.Add('--revision='+Revision);
      end;

    Runner.Params.Add(Element);
    Result := IncludeTrailingPathDelimiter(GetTempDir) +
                                           ChangeFileExt(ExtractFileNameOnly(Element),'')+'_'+
                                           EncodeSafeFileName(Revision)+
                                           ExtractFileExt(Element);

    Runner.Params.Add(Result);
    Runner.Execute;
  finally
    EndProcess;
  end;
end;

function TSVNClient.Export(Element: string; Revision: Integer): string;
begin
  Result := Export(Element, IntToStr(Revision));
end;

procedure TSVNClient.Add(Elements: TStrings; Recursive: boolean= false);
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['add','--non-interactive', '--trust-server-cert']);

    if Recursive  then
      Runner.Params.Add('--depth=infinity');

    if not Assigned(Elements) or (Elements.Count = 0) then
      Runner.Params.Add(FRepositoryPath)
    else
      Runner.Params.AddStrings(Elements);

    Runner.Execute;

  finally
    EndProcess;
  end;
end;

procedure TSVNClient.Revert(Elements: TStrings; Recursive: boolean= false);
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['revert','--non-interactive', '--trust-server-cert']);

    if Recursive  then
      Runner.Params.Add('--depth=infinity');

    if not Assigned(Elements) or (Elements.Count = 0) then
      Runner.Params.Add(FRepositoryPath)
    else
      Runner.Params.AddStrings(Elements);

    Runner.Execute;
  finally
    EndProcess;
  end;
end;

procedure TSVNClient.Resolve(Elements: TStrings);
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['resolve','--accept','working','--non-interactive','--trust-server-cert']);
    Runner.Params.AddStrings(Elements);
    Runner.Execute;

  finally
    EndProcess;
  end;


end;


procedure TSVNClient.CleanUp;
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['cleanup','--non-interactive', '--trust-server-cert']);
    Runner.Execute;

  finally
    EndProcess;
  end;
end;

procedure TSVNClient.Upgrade;
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['upgrade','--non-interactive']);
    Runner.Execute;

  finally
    EndProcess;
  end;
end;

procedure TSVNClient.Commit(Elements: TStrings; Message: string;
  Recursive: boolean);
var
  intMessage : string;
begin
  BeginProcess;
  try
    Runner.Params.Clear;
    Runner.Params.AddStrings(['commit','--non-interactive', '--trust-server-cert']);

    if Recursive  then
      Runner.Params.Add('--depth=infinity');

    if not Assigned(Elements) or (Elements.Count = 0) then
      Runner.Params.Add(FRepositoryPath)
    else
      Runner.Params.AddStrings(Elements);

    {$IFDEF WINDOWS}
    IntMessage := AnsiQuotedStr(Message, '"');
    if pos ('"', message) > 0 then
      IntMessage := StringReplace(IntMessage, '""', '"""', [rfReplaceAll]);
    {$ELSE}
      IntMessage := Message;
    {$ENDIF}

    Runner.Params.Add('--message='+IntMessage);
    Runner.Execute;
  finally
    EndProcess;
  end;
end;



procedure TSVNClient.SetFlatMode(AValue: boolean);
begin
  if FFlatMode=AValue then Exit;
  FFlatMode:=AValue;
  if FRepositoryPath <> EmptyStr then
    LoadStatus;
end;

procedure TSVNClient.SetOnBeginProcess(AValue: TNotifyEvent);
begin
  if FOnBeginProcess=AValue then Exit;
  FOnBeginProcess:=AValue;
end;

procedure TSVNClient.SetOnEndProcess(AValue: TNotifyEvent);
begin
  if FOnEndProcess=AValue then Exit;
  FOnEndProcess:=AValue;
end;

procedure TSVNClient.SetOnSVNMessage(AValue: TRunnerMessage);
begin
  if FOnSVNMessage=AValue then Exit;
  FOnSVNMessage:=AValue;

end;

function TSVNClient.GetSvnExecutable: string;
begin
  if fSvnExecutable = EmptyStr then
    fSvnExecutable := FindSvnExecutable;
  Result :=  fSvnExecutable;

end;

class function TSVNClient.FindSvnExecutable: string;
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
       then Result := GetEnvironmentVariable('ProgramFiles')+'\Subversion\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)')+'\Subversion\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles')+'\SlikSvn\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)')+'\SlikSvn\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles\')+'TorToiseSVN\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)')+'\TorToiseSVN\bin\svn.exe'
       else break;
    if not FileExists(Result)
       then Result := GetEnvironmentVariable('ProgramFiles(x86)')+'\RapidSVN-0.13\bin\svn.exe'
       else break;
    //Directory where current executable is:
       result := ExtractFileDir(ParamStr(0));
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

function TSVNClient.FullFileName(FileName: TFilename): TFilename;
begin
  Result := FRepositoryPath + FileName;
end;

procedure TSVNClient.SetRepositoryPath(AValue: string);
begin
  if FRepositoryPath=AValue then Exit;
  FRepositoryPath:= IncludeTrailingPathDelimiter(AValue);
  Runner.CurrentDirectory:= FRepositoryPath;
  LoadStatus;
end;

procedure TSVNClient.SetSvnExecutable(AValue: string);
begin
  Runner.Executable := AValue;
end;

destructor TSVNClient.Destroy;
begin
  List.Free;
  Runner.free;
  inherited Destroy;
end;


end.

