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
unit DebianPack;

{$mode delphi}

interface

uses
  Process,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TDebianPackage }

type
  TDebianPackage = class(TObject)
  private
    FBasePath: string;
    FThread: TSimpleThread;
    FArchitecture: string;
    procedure BuildDepends;
    procedure Desktop;
    procedure Generate;
    procedure Pixmap;
    procedure Prepare;
  public
    Icon: IBitmap;
    FileName: string;
    Caption: string;
    Name: string;
    Version: string;
    Section: string;
    Author: string;
    Website: string;
    ShortInfo: string;
    LongInfo: string;
    Depends: StringArray;
    FailReason: string;
  public
    constructor Create;
    procedure Build(Thread: TSimpleThread);
  end;

function FileArchitecture(FileName: string): string;

implementation

function FileArchitecture(FileName: string): string;
var
  S: string;
begin
  RunCommand('objdump', ['-a', FileName], S);
  if S.Contains('elf64') then
    Result := 'amd64'
  else if S.Contains('elf32') then
    Result := 'i386'
  else
    Result := '';
end;

constructor TDebianPackage.Create;
begin
  inherited Create;
  Icon := NewBitmap;
end;

function DependsRun(const ExeName: string; const Commands: array of string): string;
begin
  RunCommand(ExeName, Commands, Result);
end;

type
  TStringTransformFunc = function(const Value: string): string;

function StrTransform(Strings: StringArray; Transform: TStringTransformFunc): StringArray;
var
  I: Integer;
begin
  Result.Length := Strings.Length;
  for I := 0 to Strings.Length - 1 do
    Result[I] := Transform(Strings[I]);
end;

function StrUnique(Strings: StringArray): StringArray;
var
  S: string;
  I: Integer;
begin
  Result.Length := 0;
  for I := 0 to Strings.Length - 1 do
  begin
    S := Strings[I];
    if Result.IndexOf(S) < 0 then
      Result.Push(S);
  end;
  Result.Sort(soAscend);
end;

function RestrictLibs(constref Value: string): Boolean;
const
  IgnoreList: array[0..10] of string = (
    'libatk',
    'libX',
    'libxc',
    'libglib',
    'libfreetype',
    'libharfbuzz',
    'libffi',
    'libthai',
    'libgraphite',
    'libdatrie',
    'libfontconfig'
  );
var
  S, I: string;
begin
  Result := True;
  S := Value.Trim;
  for I in IgnoreList do
    if S.BeginsWith(I) then
      Exit(False);
end;

function UserLibs(constref Value: string): Boolean;
begin
  Result := Value.Contains('/usr/lib/');
  if Result then
    Result := RestrictLibs(Value.Trim)
end;

function UserFile(const Value: string): string;
begin
  Result := Value.SecondOf(' => ');
  Result := Result.FirstOf(' (').Trim;
end;

function FilePackage(const Value: string): string;
begin
  Result := DependsRun('dpkg', ['-S', Value]);
  Result := Result.FirstOf(':').trim;
end;

function PackageVersion(const Value: string): string;
var
  S: string;
begin
  S := DependsRun('dpkg', ['-s', Value]);
  S := S.LineWith('Version: ');
  S := S.SecondOf(': ');
  S := S.FirstOf('-');
  S := S.FirstOf('~');
  S := S.FirstOf('ubuntu');
  Result := Value + ' (>= ' + S +  ')';
end;

procedure TDebianPackage.BuildDepends;
var
  Items: StringArray;
begin
  if FArchitecture <> FileArchitecture(ParamStr(0)) then
    Exit;
  Depends.Clear;
  FThread.Status := 'Finding dependencies';
  Items := DependsRun('ldd', FileName).Split(#10);
  Items := Items.Filter(UserLibs);
  FThread.Status := 'Looking up packages';
  Items := StrTransform(Items, UserFile);
  Items := StrTransform(Items, FilePackage);
  Items := StrUnique(Items);
  Items := Items.Filter(RestrictLibs);
  FThread.Status := 'Resolving package versions';
  Depends := StrTransform(Items, PackageVersion);
end;

procedure TDebianPackage.Prepare;
var
  S: string;
begin
  FThread.Status := 'Preparing items';
  RunCommand('rm', [FBasePath + '-' + FArchitecture + '.deb'], S);
  RunCommand('strip', ['-s', FileName], S);
end;

procedure TDebianPackage.Desktop;
var
  AppDesktop: StringArray;
  S, C: string;
begin
  S := PathCombine(FBasePath, 'usr/share/applications');
  DirForce(S);
  S := PathCombine(S, Name + '.desktop');
  AppDesktop.Push('[Desktop Entry]');
  AppDesktop.Push('Name=' + Caption);
  AppDesktop.Push('Comment=' + ShortInfo);
  AppDesktop.Push('Icon=' + FileExtractName(FileName));
  C := Section;
  if C = 'admin' then
    C := 'Admininstration'
  else if C = 'comm' then
    C := 'Communication'
  else if C = 'database' then
    C := 'Database'
  else if C = 'devel' then
    C := 'Development'
  else if C = 'editors' then
    C := 'Editors'
  else if C = 'electronics' then
    C := 'Electronics'
  else if C = 'fonts' then
    C := 'Fonts'
  else if C = 'games' then
    C := 'Games'
  else if C = 'graphics' then
    C := 'Graphics'
  else if C = 'math' then
    C := 'Math'
  else if C = 'web' then
    C := 'Internet'
  else if C = 'net' then
    C := 'Networking'
  else if C = 'news' then
    C := 'News'
  else if C = 'science' then
    C := 'Science'
  else if C = 'sound' then
    C := 'Sound'
  else if C = 'utils' then
    C := 'Utility'
  else if C = 'video' then
    C := 'Video'
  else
    C := 'Miscellaneous';
  AppDesktop.Push('Categories=' + C);
  AppDesktop.Push('Terminal=false');
  AppDesktop.Push('Type=Application');
  AppDesktop.Push('Exec=' + FileExtractName(FileName));
  FileWriteStr(S, AppDesktop.Join(LineBreak));
  RunCommand('chmod', ['+x', S], S);
end;

procedure TDebianPackage.Pixmap;
var
  S: string;
begin
  S := PathCombine(FBasePath, 'usr/share/pixmaps');
  DirForce(S);
  S := PathCombine(S, Name + '.png');
  Icon.SaveToFile(S);
end;

procedure TDebianPackage.Generate;

  function SpacedLongInfo: string;
  var
    Lines: StringArray;
    I: Integer;
  begin
    Lines := LongInfo.Lines;
    for I := 0 to Lines.Length - 1 do
      Lines[I] := ' ' + Lines[I].Trim;
    Result := Lines.Join(LineBreak) + LineBreak;
  end;

var
  Control: StringArray;
  S: string;
  I: LargeWord;
begin
  S := PathCombine(FBasePath, 'usr/local/bin/');
  DirForce(PathCombine(FBasePath, 'usr/local/bin/'));
  S := PathCombine(FBasePath, 'usr/local/bin/' +
    FileExtractName(FileName));
  FileDelete(S);
  RunCommand('cp', [FileName, S], S);
  WriteLn(S);
  DirForce(PathCombine(FBasePath, 'DEBIAN'));
  Control.Push('Package: ' + Name);
  Control.Push('Version: ' + Version);
  Control.Push('Section: ' + Section);
  Control.Push('Priority: optional');
  Control.Push('Architecture: ' + FArchitecture);
  I := FileSize(FileName) div 1024;
  Control.Push('Installed-Size: ' + IntToStr(I));
  Control.Push('Depends: ' + Depends.Join(', '));
  Control.Push('Maintainer: ' + Author);
  Control.Push('Homepage: ' + Website);
  Control.Push('Description: ' + ShortInfo);
  Control.Push(SpacedLongInfo);
  S := Control.Join(#10);
  FileWriteStr(PathCombine(FBasePath, 'DEBIAN/control'), S);
  S := FileExtractName(FileName) + '_' + Version + '-' + FArchitecture + '.deb';
  FThread.Status := 'Generating ' + S;
  RunCommand('fakeroot', ['dpkg-deb', '--build', FBasePath], S);
  RunCommand('mv', [FBasePath + '.deb', FBasePath + '-' + FArchitecture + '.deb'], S);
end;

procedure TDebianPackage.Build(Thread: TSimpleThread);
begin
  FailReason := '';
  FThread := nil;
  FArchitecture := FileArchitecture(FileName);
  if FArchitecture = '' then
  begin
    FailReason := 'Not a valid application file';
    Exit;
  end;
  FThread := Thread;
  FBasePath := FileExtractPath(FileName);
  FBasePath := PathCombine(FBasePath, Name + '_' + Version);
  Prepare;
  if not FThread.Terminated then
    BuildDepends;
  if (FailReason = '') and (Depends.Length = 0) then
  begin
    FailReason := 'Dependencies could not be located';
    Exit;
  end;
  if not FThread.Terminated then
    Pixmap;
  if not FThread.Terminated then
    Desktop;
  if not FThread.Terminated then
    Generate;
  FThread := nil;
end;

end.

