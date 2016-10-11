unit formupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TfUpdate }

  TfUpdate = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbLatest: TCheckBox;
    cbRecursive: TCheckBox;
    eRevision: TEdit;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
  private

  public

  end;

var
  fUpdate: TfUpdate;

implementation

{$R *.lfm}

end.

