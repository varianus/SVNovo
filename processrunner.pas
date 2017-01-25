unit ProcessRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLRead, Laz2_DOM, Process;

type
  { TProcessRunner }
  TRunnerMessageKind= (ieInfo, ieCommand, ieError);

  TRunnerMessage = procedure (Sender: TObject; const MessageKind: TRunnerMessageKind; const Message: string) of object;

  TProcessRunner = class
  Private
    FCurrentDirectory: TFileName;
    FExecutable: TFileName;
    FOnRunnerMessage: TRunnerMessage;
    FParams: TStringList;
    function getParams: TStrings;
    procedure HandleOutput(var MemStream: TMemoryStream; var BytesRead: LongInt);
    procedure SetCurrentDirectory(AValue: TFileName);
    procedure SetExecutable(AValue: TFileName);
    procedure SetOnRunnerMessage(AValue: TRunnerMessage);
  Public
    Function Execute:Integer;
    Function ExecuteReturnTxt: TStringList;
    function ExecuteReturnXml: TXMLDocument;
  public
    Constructor Create;
    Destructor Destroy; override;
    Property Params: TStrings read getParams;
    Property Executable: TFileName read FExecutable write SetExecutable;
    Property CurrentDirectory : TFileName read FCurrentDirectory write SetCurrentDirectory;
    property OnRunnerMessage : TRunnerMessage read FOnRunnerMessage write SetOnRunnerMessage;
  end;


function ReplaceLineEndings(const s, NewLineEnds: string): string;
function ReplaceLineEndings(const s:string; NewLineEnds: AnsiChar): string;


implementation

uses
  UTF8Process;

Const
  READ_BYTES = 2048;

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

function ReplaceLineEndings(const s:string; NewLineEnds: AnsiChar): string;
var
  p: Integer;
  r: integer;
  Max:integer;
begin
  Max:= Length(S);
  SetLength(Result, Max);

  p:=1;
  r:=1;
  while (p<=Max) do
    begin
      if S[p] in [#10,#13] then
        begin
          Result[r]:=NewLineEnds;
          inc(r);
          inc(p);
          While (p<=Max) and (s[P] in [#10,#13])  do
            inc(p);
        end
    else
      begin
        Result[r]:=S[p];
        Inc(r);
        Inc(p);
      end;
    end;
  SetLength(Result, R-1);
end;


{ TProcessRunner }

procedure TProcessRunner.SetExecutable(AValue: TFileName);
begin
  if FExecutable=AValue then Exit;
  FExecutable:=AValue;
end;

function TProcessRunner.getParams: TStrings;
begin
  Result:= FParams;
end;

procedure TProcessRunner.SetCurrentDirectory(AValue: TFileName);
begin
  if FCurrentDirectory=AValue then Exit;
  FCurrentDirectory:=AValue;
end;

procedure TProcessRunner.SetOnRunnerMessage(AValue: TRunnerMessage);
begin
  if FOnRunnerMessage=AValue then Exit;
  FOnRunnerMessage:=AValue;
end;

procedure TProcessRunner.HandleOutput(var MemStream: TMemoryStream;
  var BytesRead: LongInt);
var
  S: TStringList;
  n: LongInt;
  str: string;
begin
  Memstream.SetSize(BytesRead);
  S := TStringList.Create;
  S.LoadFromStream(MemStream);

  for n := 0 to S.Count - 1 do
    begin
      if Assigned(FOnRunnerMessage)   then
         FOnRunnerMessage(Self, ieInfo, S[n]);
    end;

  S.Free;
  BytesRead := 0;
  MemStream.Clear;

end;


function TProcessRunner.Execute: Integer;
var
  AProcess: TProcessUTF8;
  MemStream: TMemoryStream;
  n, BytesRead: Integer;
begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.Executable := FExecutable;
  AProcess.Parameters.Assign(FParams);
  AProcess.CurrentDirectory:= FCurrentDirectory;
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  if Assigned(FOnRunnerMessage) then
     FOnRunnerMessage(Self, ieCommand, Aprocess.Executable + ' '+ ReplaceLineEndings(AProcess.Parameters.text, ' '));
  AProcess.Execute;

  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  while AProcess.Running do
  begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
      HandleOutput(MemStream, BytesRead);
    end
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
      HandleOutput(MemStream, BytesRead);
    end;
  until n <= 0;
  Result := AProcess.ExitCode;
  AProcess.Free;
  MemStream.Free;


end;

function TProcessRunner.ExecuteReturnTxt: TStringList;
var
  AProcess: TProcessUTF8;
  M: TMemoryStream;
  n, BytesRead: Integer;
begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.Executable := FExecutable;
  AProcess.Parameters.Assign(FParams);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.ShowWindow := swoHIDE;
  if Assigned(FOnRunnerMessage) then
     FOnRunnerMessage(Self, ieCommand, Aprocess.Executable + ' '+ ReplaceLineEndings(AProcess.Parameters.text, ' '));

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

  try
    Result := TStringList.Create;
    Result.LoadFromStream(M);
  Except
    Result := nil;
  end;

  M.Free;
  AProcess.Free;

end;

function TProcessRunner.ExecuteReturnXml: TXMLDocument;
var
  AProcess: TProcessUTF8;
  M: TMemoryStream;
  n, BytesRead: Integer;
begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.Executable := FExecutable;
  AProcess.Parameters.Assign(FParams);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.ShowWindow := swoHIDE;
  if Assigned(FOnRunnerMessage) then
     FOnRunnerMessage(Self, ieCommand, Aprocess.Executable + ' '+ ReplaceLineEndings(AProcess.Parameters.text, ' '));

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
  try
    ReadXMLFile(Result, M);
  Except
    Result.free;
    Result := nil;
  end;

  M.Free;
  AProcess.Free;


end;

constructor TProcessRunner.Create;
begin
  FParams := TStringList.Create;
end;

destructor TProcessRunner.Destroy;
begin

  FParams.Free;
  Inherited Destroy;
end;



end.

