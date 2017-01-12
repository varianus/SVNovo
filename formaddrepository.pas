unit formaddrepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel;

type

  { TfAddRepository }

  TfAddRepository = class(TForm)
    ButtonPanel1: TButtonPanel;
    DirectoryEdit: TDirectoryEdit;
    Label1: TLabel;
    leRepositoryURL: TLabeledEdit;
    Panel1: TPanel;
  private

  public

  end;

var
  fAddRepository: TfAddRepository;

implementation

{$R *.lfm}

end.


