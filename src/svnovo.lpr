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
program SVNovo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  clocale,
  {$ENDIF}
  Interfaces, sysutils, // this includes the LCL widgetset
  Forms, uMain, Config, SVNClasses, FilesSupport, formupdate, ulicense,
  formcommit, formlog, formconfig, AppConsts, formaddrepository, ProcessRunner,
  SVNTypes, formannotate, framerevision, formdiff;

{$R *.res}


begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

