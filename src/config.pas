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
unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, jsonConf, typinfo;

type
  { TEnum }

 generic TEnum<T> = class(TObject)
  public
    class function ToString(const aEnumValue: T): string; reintroduce;
    class function FromString(const aEnumString: string; const aDefault: T): T;
  end;

{ TConfig }

  TConfig = class
  private
    FConfigFile: string;
    fConfigDir: string;
    ResourcesPath: string;
    fConfigHolder: TJSONConfig;
  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(APath: string; Values: TStrings);
    function ReadStrings(APath: string; Values: TStrings): integer;
    procedure WriteString(APath: string; Value: String);
    function ReadString(APath: string; ADefault: String): string;
    function GetResourcesPath: string;
    procedure WriteBoolean(APath: string; Value: Boolean);
    function ReadBoolean(APath: string; ADefault: Boolean): Boolean;
    procedure WriteInteger(APath: string; Value: Integer);
    function ReadInteger(APath: string; ADefault: Integer): Integer;

    procedure Flush;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
  end;

  { TSimpleHistory }

  TSimpleHistory = class
  private
    FMax: Integer;
    IntList: TStringList;
    function GetCount: integer;
    procedure SetMax(AValue: Integer);
  public
    function Add(const S: string): Integer;
    Constructor Create;
    Destructor Destroy; override;
    Procedure GetList(List: TStrings);
    Procedure LoadFromConfig(Config: TConfig; APath: string);
    Procedure WriteToConfig(Config: TConfig; APath: string);
    Property Max: Integer read FMax write SetMax;
    Property Count: integer read GetCount;
  end;

const
   CFG_BookMark = 'Repositories/Path';

   CFG_Editor = 'Editor';
   CFG_Diff = 'Diff';

function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, lclproc
  // only for default font !
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

var
  FConfigObj: TConfig;

const
  SectionUnix = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';
  ResourceSubDirectory = 'Resources';

const
 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/SVNovo/';
  {$DEFINE NEEDCFGSUBDIR}
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

  SectionGeneral = 'General';


function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelim: Char): string;
var
  TokStart: Integer;
begin
  repeat
    if SeekPos > Length(s) then begin Result := ''; Exit end;
    if S[SeekPos] = TokenDelim then Inc(SeekPos) else Break;
  until false;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not(S[SeekPos] = TokenDelim) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  result := Copy(s, TokStart, SeekPos-TokStart);

  { We don't have to do Inc(seekPos) below. But it's obvious that searching
    for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDir(False);
  ForceDirectories(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function ConfigObj: TConfig;
begin
  if not Assigned(FConfigObj) then
    FConfigObj := TConfig.Create;
  Result := FConfigObj;
end;

{ TEnum }

class function TEnum.ToString(const aEnumValue: T): string;
begin
  WriteStr(Result, aEnumValue);
end;

class function TEnum.FromString(const aEnumString: string; const aDefault: T): T;
var
  OrdValue: Integer;
begin
  OrdValue := GetEnumValue(TypeInfo(T), aEnumString);
  if OrdValue < 0 then
    Result := aDefault
  else
    Result := T(OrdValue);
end;

{ TSimpleHistory }

procedure TSimpleHistory.SetMax(AValue: Integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;

  while IntList.Count > FMax do
    IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed

end;

function TSimpleHistory.GetCount: integer;
begin
  Result := IntList.count;
end;

function TSimpleHistory.Add(const S: string): Integer;
var
   i : integer;
begin
   i := IntList.IndexOf(S);
   if i<>-1 then
     IntList.Delete(i);

   IntList.Insert(0, S);

   // Trim the oldest files if more than NumFiles
   while IntList.Count > FMax do
     IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed

end;

constructor TSimpleHistory.Create;
begin
  IntList := TStringList.Create;
end;

destructor TSimpleHistory.Destroy;
begin
  FreeAndNil(IntList);
  inherited Destroy;
end;

procedure TSimpleHistory.GetList(List: TStrings);
begin
  List.Assign(IntList);
end;

procedure TSimpleHistory.LoadFromConfig(Config: TConfig; APath: string);
begin
  Config.ReadStrings(APath, IntList);
end;

procedure TSimpleHistory.WriteToConfig(Config: TConfig; APath: string);
begin
  Config.WriteStrings(APath, IntList);
end;

constructor TConfig.Create;
begin
  FConfigFile := GetAppConfigFile(False
{$ifdef NEEDCFGSUBDIR}
    , True
{$ENDIF}
    );
  fConfigDir := GetConfigDir;
  fConfigHolder := TJSONConfig.Create(nil);
  fConfigHolder.Formatted:=true;
  fConfigHolder.Filename:= FConfigFile;
  ReadConfig;

end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigHolder.Flush;
  fConfigHolder.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
end;

procedure TConfig.ReadConfig;
begin

{$ifdef WINDOWS}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix + '/' + IdentResourcesPath,
    ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := fConfigHolder.GetValue(SectionUnix + '/' + IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}

end;

procedure TConfig.WriteStrings(APath: string; Values: TStrings);
begin
  fConfigHolder.SetValue(APath, Values);
end;

function TConfig.ReadStrings(APath: string; Values: TStrings): integer;
begin
  fConfigHolder.GetValue(APath, Values, '');
  Result := Values.Count;
end;

procedure TConfig.WriteString(APath: string; Value: String);
begin
  fConfigHolder.SetValue(APath, Value);
end;

function TConfig.ReadString(APath: string; ADefault: String): string;
begin
  Result := fConfigHolder.GetValue(APath, ADefault);
end;

procedure TConfig.WriteBoolean(APath: string; Value: Boolean);
begin
  fConfigHolder.SetValue(APath, Value);
end;

function TConfig.ReadBoolean(APath: string; ADefault: Boolean): Boolean;
begin
  Result := fConfigHolder.GetValue(APath, ADefault);
end;

procedure TConfig.WriteInteger(APath: string; Value: Integer);
begin
  fConfigHolder.SetValue(APath, Value);
end;

function TConfig.ReadInteger(APath: string; ADefault: Integer): Integer;
begin
  Result := fConfigHolder.GetValue(APath, ADefault);
end;

procedure TConfig.Flush;
begin
  fConfigHolder.Flush;
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := ResourcesPath;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

initialization
  FConfigObj := nil;

finalization
  if Assigned(FConfigObj) then
  begin
    FConfigObj.SaveConfig;
    FConfigObj.Free;
  end;


end.
