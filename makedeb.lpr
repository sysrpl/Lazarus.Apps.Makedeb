(********************************************************)
(*                                                      *)
(*  Debian Packager                                     *)
(*  http://www.getlazarus.org/apps/makedeb              *)
(*  Anthony Walter <sysrpl@gmail.com>                   *)
(*                                                      *)
(*  Released under the copyleft license                 *)
(*                                                      *)
(*  Last Modified November 2015                         *)
(*                                                      *)
(********************************************************)
program makedeb;

{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, Composer, DebianPack;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TComposeForm, ComposeForm);
  Application.Run;
end.

