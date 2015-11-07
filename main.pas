unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Process,
  Codebot.System,
  Codebot.Controls.Extras;

{ TMainForm }

type
  TMainForm = class(TForm)
    BrowseButton: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Progress: TIndeterminateProgress;
    StopButton: TButton;
    DebugMemo: TMemo;
    FileDialog: TOpenDialog;
    procedure BrowseButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
  private
    FThread: TSimpleThread;
    FProcessing: Boolean;
    FFilename: string;
    FStatus: string;
    procedure ProcessDone;
    procedure ProcessFile(Thread: TSimpleThread);
    procedure ProcessProgress;
    procedure ProcessStatus;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ProcessStatus;
begin
  DebugMemo.Lines.Add(FStatus);
end;

procedure TMainForm.ProcessProgress;
begin
  Progress.Caption := FStatus;
end;

procedure TMainForm.ProcessDone;
begin
  FProcessing := False;
  FThread := nil;
  Caption := 'Make Debian: Completed';
  Progress.Caption := 'Ready';
  Progress.Status := psReady;
end;

function StrConvert(constref Item: string): string;
begin
  Result := Item;
end;

procedure StrRemove(var A: StringArray; S: string);
var
  I: Integer;
begin
  for I := A.Length - 1 downto 0 do
    if A[I].contains(S) then
      A.Delete(I);
end;

function CacheRead(const FileName: string): string;
var
  A, B: string;
begin
  A := ConfigAppDir(False, True);
  B := FileName.Replace('/', '.');
  B := B + '.txt';
  A := PathCombine(A, B);
  if FileExists(A) then
    Result := FileReadStr(A)
  else
    Result := '';
end;

function CacheWrite(const FileName, Value: string): string;
var
  A, B: string;
begin
  A := ConfigAppDir(False, True);
  B := FileName.Replace('/', '.');
  B := B + '.txt';
  A := PathCombine(A, B);
  FileWriteStr(A, Value);
end;

procedure TMainForm.ProcessFile(Thread: TSimpleThread);
var
  Lines, Items, Packages: StringArray;
  Command, Line, S: string;
  I, J: Integer;
begin
  FStatus := 'Listing dynamic dependencies';
  Thread.Synch(ProcessProgress);
  RunCommand('ldd', [FFilename], Command);
  Lines := Command.Split(#10);
  for Line in Lines do
    if Line.Contains('/usr/lib') then
      Items.Push(Line.Trim.Split(' ')[2]);
  Lines := Items;
  Lines.Sort;
  StrRemove(Lines, '/libX');
  I := Lines.Length;
  J := 0;
  FStatus := 'Looking up packages for shared object files';
  Thread.Synch(ProcessStatus);
  for Line in Lines do
  begin
    if Thread.Terminated then Break;
    Inc(J);
    FStatus := 'Looking up package %d of %d'.Format([J, I]);
    Thread.Synch(ProcessProgress);
    FStatus := Line;
    Thread.Synch(ProcessStatus);
    S := CacheRead(Line);
    if S = '' then
    begin
      RunCommand('apt-file', ['search', Line], Command);
      Items := Command.Trim.Split(#10);
      StrRemove(Items, '-dbg:');
      S := Items[0];
      S := S.Split(': ')[0];
      CacheWrite(Line, S);
    end;
    if Packages.IndexOf(S) < 0 then
    begin
      Packages.Push(S);
      FStatus := S;
    end
    else
      FStatus := 'duplicate ' + S;
    Thread.Synch(ProcessStatus);
  end;
  Lines := Packages;
  Lines.Sort;
  I := Lines.Length;
  J := 0;
  FStatus := 'Looking up package versions';
  Thread.Synch(ProcessStatus);
  for Line in Lines do
  begin
    if Thread.Terminated then Break;
    Inc(J);
    FStatus := 'Looking up versions %d of %d'.Format([J, I]);
    RunCommand('dpkg', ['-s', Line], Command);
    S := Command.LineWith('Version: ');
    S := S.Split(': ')[1];
    S := S.Split('-')[0];
    FStatus := Line + ' ( >= ' + S + ')';
    Thread.Synch(ProcessStatus);
  end;
  Thread.Synch(ProcessDone);
end;

procedure TMainForm.BrowseButtonClick(Sender: TObject);
var
  S: string;
begin
  if FProcessing then
    Exit;
  if FileDialog.Execute then
  begin
    DebugMemo.Lines.Clear;
    FProcessing := True;
    FFilename := FileDialog.FileName;
    Caption := 'Make Debian: Processing';
    Progress.Caption := 'Processing';
    Progress.Status := psBusy;
    FThread := TSimpleThread.Create(ProcessFile);
  end;
end;

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

end.

