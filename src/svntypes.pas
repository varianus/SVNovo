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
unit SVNTypes;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Defaults,  Generics.Collections;
Const
   REV_BASE = 'BASE';
   REV_HEAD = 'HEAD';

Type
//  TSortDirection  = (sdAscending=1, sdDescending=-1);
  TSortDirection  = (sdAscending, sdDescending);

  TStatusItemName = (siChecked, siName, siPath, siExtension, siPropStatus, siItemStatus,
                     siRevision, siCommitRevision, siAuthor, siDateSVN, siDateModified);

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
     sisUnversioned,
     sisUpdated);

 TSVNItemStatusSet = set of TSVNItemStatus;

//  PSVNStatusItem = ^TSVNStatusItem;

  { TSVNItem }
  TSVNSimpleItem = class //record
    Revision: integer;
    Author: string;
    DateSVN: TDateTime;
  end;

  TSVNItem = class(TSVNSimpleItem) //record
  public
    Checked: boolean;
    Path: string;
    Name: string;
    Extension: string;
    PropStatus: string;
    ItemStatus: TSVNItemStatus;
    CommitRevision: integer;
    DateModified: TDateTime;
    Kind: integer;
    Selected: boolean;

    Function IsFolder : boolean;
    Constructor Create;
  end;

  TAffectedFile = class
    Action : String;
    FileName: String;
  end;

  TAffectedFiles = class(TObjectList<TAffectedFile>)
  end;

  { TSVNLogItem }

  TSVNLogItem = class (TSVNSimpleItem)
    Message: string;
    AffectedFiles: TAffectedFiles;
    Constructor Create;
    Destructor Destroy; override;
  end;


  TSVNLogList = class (TObjectList<TSVNLogItem>)
  end;


  TSVNAnnotateItem = class (TSVNSimpleItem)
    LineNo: integer;
    Line: string;
    Group: integer;
  end;

  TSVNAnnotateList = class (TObjectList<TSVNAnnotateItem>)
  end;

  { TSVNStatusList }

  TSVNStatusList = class (TObjectList<TSVNItem>)
  private
    FSortDirection: TSortDirection;
    FSortItem: TStatusItemName;
    function SortExtension(constref Item1, Item2: TSVNItem): Integer;
    function SortItemStatus(constref Item1, Item2: TSVNItem): Integer;
    function SortPath(constref Item1, Item2: TSVNItem): Integer;
    function SortName(constref Item1, Item2: TSVNItem): Integer;
    function SortPropertyAuthor(constref Item1, Item2: TSVNItem): Integer;
    function SortPropertyCommitRevision(constref Item1, Item2: TSVNItem): Integer;
    function SortPropertyDateSVN(constref Item1, Item2: TSVNItem): Integer;
    function SortPropertyDateModified(constref Item1, Item2: TSVNItem): Integer;
    function SortPropertyRevision(constref Item1, Item2: TSVNItem): Integer;
    function SortPropStatus(constref Item1, Item2: TSVNItem): Integer;
    function SortSelected(constref Item1, Item2: TSVNItem): Integer;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    function LocateByPath(FullPath: string): TSVNItem;
    procedure Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);  overload;
    procedure Sort;  overload;
    procedure ReverseSort(ASortItem: TStatusItemName);
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property SortItem: TStatusItemName read FSortItem write FSortItem;

  end;


implementation
uses
  Math;

function CompareBoolean (Const A, B: Boolean): Integer;
const
   BoolOrder: Array [False..True] Of Integer = (0,1); // o 1,0 se si desidera ordinare il contrario
Begin
   result := BoolOrder [A] - BoolOrder [B];
End ;

{ TSVNLogItem }

constructor TSVNLogItem.Create;
begin
  AffectedFiles := TAffectedFiles.Create();
end;

destructor TSVNLogItem.Destroy;
begin
  AffectedFiles.free;
  inherited Destroy;
end;

function TSVNStatusList.SortPath(constref Item1, Item2: TSVNItem): Integer;
begin
   Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
   if Result <> 0 then exit;

   Result := CompareText(Item1.Path, Item2.Path);
   if SortDirection = sdDescending then
     Result *= -1;
end;

function TSVNStatusList.SortName(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareText(Item1.Name, Item2.Name);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortSelected(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  if Item1.Checked > Item2.Checked then
    Result := 1
  else
    if Item1.Checked = Item2.Checked then
      Result :=  SortPath(Item1, Item2) * -1
    else
      Result := -1;
end;

constructor TSVNStatusList.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  SortDirection:=sdAscending;
end;

function TSVNStatusList.SortExtension(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareText(Item1.Extension, Item2.Extension);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortItemStatus(constref Item1, Item2: TSVNItem): Integer;
begin

 Result := CompareValue(ord(Item1.ItemStatus), ord(Item2.ItemStatus));
end;

function TSVNStatusList.SortPropStatus(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareText(Item1.PropStatus, Item2.PropStatus);
  if SortDirection = sdDescending then
    Result *= -1;

end;


function TSVNStatusList.SortPropertyAuthor(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareText(Item1.Author, Item2.Author);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortPropertyRevision(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareValue(Item1.Revision, Item2.Revision);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortPropertyCommitRevision(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareValue(Item1.CommitRevision, Item2.CommitRevision);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortPropertyDateSVN(constref Item1, Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareValue(Item1.DateSVN, Item2.DateSVN);
  if SortDirection = sdDescending then
    Result *= -1;

end;

function TSVNStatusList.SortPropertyDateModified(constref Item1,
  Item2: TSVNItem): Integer;
begin
  Result := -CompareBoolean(Item1.IsFolder, Item2.IsFolder);
  if Result <> 0 then exit;

  Result := CompareValue(Item1.DateModified, Item2.DateModified);
  if SortDirection = sdDescending then
    Result *= -1;


end;

function TSVNStatusList.LocateByPath(FullPath: string): TSVNItem;
begin
  for  result in self do
    begin
       if SameText(Result.Path,FullPath) then
         exit;
    end;
  Result:= nil;
end;

procedure TSVNStatusList.Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);
begin
  SortDirection := ADirection;
  SortItem := ASortItem;

  case ASortItem of
      siChecked:        Sort(TComparer<TSVNItem>.Construct(SortSelected));
      siName:           Sort(TComparer<TSVNItem>.Construct(SortName));
      siPath:           Sort(TComparer<TSVNItem>.Construct(SortPath));
      siExtension:      Sort(TComparer<TSVNItem>.Construct(SortExtension));
      siItemStatus:     Sort(TComparer<TSVNItem>.Construct(SortItemStatus));
      siPropStatus:     Sort(TComparer<TSVNItem>.Construct(SortPropStatus));
      siAuthor:         Sort(TComparer<TSVNItem>.Construct(SortPropertyAuthor));
      siRevision:       Sort(TComparer<TSVNItem>.Construct(SortPropertyRevision));
      siCommitRevision: Sort(TComparer<TSVNItem>.Construct(SortPropertyCommitRevision));
      siDateSVN:        Sort(TComparer<TSVNItem>.Construct(SortPropertyDateSVN));
      siDateModified:   Sort(TComparer<TSVNItem>.Construct(SortPropertyDateModified));
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

{ TSVNItem }

function TSVNItem.IsFolder: boolean;
begin
  Result := Kind = 2;
end;

constructor TSVNItem.Create;
begin
  Selected:= false;
end;


end.

