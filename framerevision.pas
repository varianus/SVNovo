unit framerevision;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, ExtCtrls;

type

  { TfrmRevision }

  TfrmRevision = class(TFrame)
    dteRevDate: TDateEdit;
    edtRevision: TEdit;
    gbRevision: TGroupBox;
    rbRevision: TRadioButton;
    rbDate: TRadioButton;
    rbHead: TRadioButton;
  private

  public
    function GetSelectedRevision: string;
  end;

ResourceString
  rsCaptionRevision = 'Revision or date #%d';

implementation

uses SVNTypes;

{$R *.lfm}

{ TfrmRevision }

function TfrmRevision.GetSelectedRevision: string;
begin
  if not Enabled then
    begin
      Result := EmptyStr;
      exit;
    end;

  if rbHead.Checked then
    begin
      Result := REV_HEAD;
      exit;
    end;

  if rbRevision.Checked then
    begin
      Result := edtRevision.Text;
      exit;
    end;

  if rbDate.Checked then
    begin
      Result := FormatDateTime('{yyyy-mm-dd}', dteRevDate.Date + 1);
      exit;
    end;

end;

end.

