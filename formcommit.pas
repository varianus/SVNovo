unit formcommit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, CheckLst;

type

  { TfCommit }

  TfCommit = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    mLogMessage: TMemo;
  private

  public

  end;

var
  fCommit: TfCommit;

implementation

{$R *.lfm}

end.

