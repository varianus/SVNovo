unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, jsonConf, DOM;

type
  { TXMLConfigExtended }

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
    procedure Flush;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
  end;


function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, lclproc, typinfo
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
  DefaultDirectory = '/usr/share/ovorepo/';
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

function TConfig.ReadString(APath: string; ADefault: String
  ): string;
begin
  fConfigHolder.GetValue(APath, ADefault);
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
