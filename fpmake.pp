{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for Pas6502 0.1

   This file was generated on 21/04/19
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_Pas6502(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('pas6502');
    P.Version:='0.1';

    P.Directory:=ADirectory;

    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('pas6502.pas');
    t.Dependencies.AddUnit('cpu6502');

    T:=P.Targets.AddUnit('cpu6502.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('Pas6502.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_Pas6502('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
