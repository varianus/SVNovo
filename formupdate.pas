unit formupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.lfm}

end.

