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
unit Composer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, Dialogs, Buttons,
  ExtDlgs, ExtCtrls, FileUtil, Process, FormState, DebianPack,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls,
  Codebot.Controls.Tooltips,
  Codebot.Controls.Banner,
  Codebot.Controls.Extras,
  Codebot.Controls.Buttons;

{ TComposeForm }

type
  TComposeForm = class(TBannerForm)
    ApplicationProperties: TApplicationProperties;
    IconImage: TImageStrip;
    ButtonImages: TImageStrip;
    IconDialog: TOpenPictureDialog;
    OpenDialog: TOpenDialog;
    AppEdit: TEdit;
    AppButton: TThinButton;
    SectionBox: TComboBox;
    PackageEdit: TEdit;
    AuthorEdit: TEdit;
    IconButton: TThinButton;
    CaptionEdit: TEdit;
    WebsiteEdit: TEdit;
    VerisonEdit: TEdit;
    ShortEdit: TEdit;
    LongEdit: TMemo;
    Progress: TIndeterminateProgress;
    BuildButton: TButton;
    CloseButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure AppButtonClick(Sender: TObject);
    procedure ApplicationPropertiesShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure BuildButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IconButtonClick(Sender: TObject);
  private
    FThread: TSimpleThread;
    FPackage: TDebianPackage;
    FOriginal: IBitmap;
    FCustom: IBitmap;
    FCancelled: Boolean;
    FPriorStatus: TProgressStatus;
    FPriorCaption: string;
    procedure DisableEdits;
    procedure DoThreadDone(Sender: TObject);
    procedure DoThreadStatus(Sender: TObject);
    procedure EnableEdits;
    procedure LoadPackage(FileName: string);
    procedure SavePackage(FileName: string);
  end;

var
  ComposeForm: TComposeForm;

implementation

{$R *.lfm}

procedure TComposeForm.SavePackage(FileName: string);
var
  Control: TControl;
  Buffer: TBuffer;
  D: IDocument;
  N: INode;
  F: IFiler;
  S: string;
  I: Integer;
begin
  D := DocumentCreate;
  N := D.Force('data');
  F := N.Filer;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control is TEdit then
      F.WriteStr(Control.Name, TEdit(Control).Text)
    else if Control is TMemo then
      F.WriteStr(Control.Name, TMemo(Control).Text)
    else if Control is TComboBox then
      F.WriteInt(Control.Name, TComboBox(Control).ItemIndex)
    else if Control is TCheckBox then
      F.WriteBool(Control.Name, TCheckBox(Control).Checked);
  end;
  S := GetTempDir(True);
  S := PathCombine(S, ExtractFileNameOnly(FileName) + '.png');
  FCustom.SaveToFile(S);
  Buffer.LoadFromFile(S);
  F.WriteStr('icon', HexEncode(Buffer));
  FileDelete(S);
  FileName := FileName + '.mkdeb';
  D.Save(FileName);
end;

procedure TComposeForm.LoadPackage(FileName: string);
var
  Control: TControl;
  Buffer: TBuffer;
  D: IDocument;
  N: INode;
  F: IFiler;
  S: string;
  I: Integer;
begin
  S := FileName + '.mkdeb';
  if FileExists(S) then
  begin
    D := DocumentCreate;
    D.Load(S);
    N := D.Force('data');
    F := N.Filer;
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      if Control is TEdit then
        TEdit(Control).Text := F.ReadStr(Control.Name)
      else if Control is TMemo then
        TMemo(Control).Text := F.ReadStr(Control.Name)
      else if Control is TComboBox then
        TComboBox(Control).ItemIndex := F.ReadInt(Control.Name, -1)
      else if Control is TCheckBox then
        TCheckBox(Control).Checked := F.ReadBool(Control.Name);
    end;
    FCustom.Clear;
    S := F.ReadStr('icon');
    if S.Length > 1000 then
    begin
      Buffer := HexDecode(S);
      S := GetTempDir(True);
      S := PathCombine(S, ExtractFileNameOnly(FileName) + '.png');
      Buffer.SaveToFile(S);
      FCustom.LoadFromFile(S);
      FileDelete(S);
    end;
    if FCustom.Empty then
      FCustom := FOriginal.Clone;
    IconImage.Clear;
    IconImage.Add(FCustom);
  end
  else
  begin
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      if Control is TEdit then
        TEdit(Control).Text := ''
      else if Control is TMemo then
        TMemo(Control).Text := ''
      else if Control is TComboBox then
        TComboBox(Control).ItemIndex := -1
      else if Control is TCheckBox then
        TCheckBox(Control).Checked := False;
    end;
    AppEdit.Text := FileName;
    CaptionEdit.Text := ExtractFileNameOnly(FileName);
    PackageEdit.Text := CaptionEdit.Text;
    VerisonEdit.Text := '1.0-1';
    FCustom := FOriginal.Clone;
    IconImage.Clear;
    IconImage.Add(FCustom);
  end;
end;

procedure TComposeForm.ApplicationPropertiesShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if FThread <> nil then
    Exit;
  CanShow := False;
  if HintStr <> '' then
  begin
    if UseTipify then
      Tipify(HintInfo.HintControl)
    else
    begin
      if Progress.Status <> psHelp then
      begin
        FPriorStatus := Progress.Status;
        FPriorCaption := Progress.Caption;
      end;
      Progress.Status := psHelp;
      Progress.Caption := HintStr;
    end;
  end
  else
  begin
    if UseTipify then
      Tipify(nil)
    else
    begin
      Progress.Status := FPriorStatus;
      Progress.Caption := FPriorCaption;
    end;
  end;
end;

procedure AlignVert(Controls: array of TControl);
var
  Y, I: Integer;
begin
  if Length(Controls) < 2 then
    Exit;
  Y := Controls[0].Top + Controls[0].Height div 2;
  for I := 1 to Length(Controls) - 1 do
    Controls[I].Top := Y - Controls[I].Height div 2;
end;

{ TComposeForm }

procedure TComposeForm.FormCreate(Sender: TObject);
begin
  FOriginal := NewBitmap;
  IconImage.CopyTo(FOriginal);
  FCustom := FOriginal.Clone;
  FPackage := TDebianPackage.Create;
  AlignVert([AppEdit, AppButton]);
  AlignVert([BuildButton, Progress]);
end;

procedure TComposeForm.FormDestroy(Sender: TObject);
begin
  FPackage.Free;
end;

procedure TComposeForm.FormShow(Sender: TObject);
begin
  if Tag = 1 then Exit;
  Tag := 1;
  Progress.Top := BuildButton.Top - (Progress.Height - BuildButton.Height) div 2;
end;

procedure TComposeForm.IconButtonClick(Sender: TObject);
begin
  if IconDialog.Execute and FileExists(IconDialog.FileName) then
  begin
    FCustom.LoadFromFile(IconDialog.FileName);
    if FCustom.Height = FCustom.Width then
    begin
      if FCustom.Width > 64 then
        FCustom := FCustom.Resample(64, 64);
      IconImage.Clear;
      IconImage.Add(FCustom);
    end;
  end;
end;

procedure TComposeForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FThread = nil;
end;

procedure TComposeForm.DoThreadStatus(Sender: TObject);
begin
  Progress.Caption := FThread.Status;
end;

procedure TComposeForm.DoThreadDone(Sender: TObject);
begin
  FThread := nil;
  EnableEdits;
  CloseButton.Caption := 'Close';
  if FCancelled then
  begin
    Progress.Status := psError;
    Progress.Caption := 'Deb file creation cancelled';
  end
  else if FPackage.FailReason <> '' then
  begin
    Progress.Status := psError;
    Progress.Caption := FPackage.FailReason;
  end
  else
  begin
    Progress.Status := psReady;
    Progress.Caption := 'Deb file creation complete';
    SavePackage(FPackage.FileName);
  end;
  FPriorStatus := Progress.Status;
  FPriorCaption := Progress.Caption;
end;

procedure TComposeForm.DisableEdits;
var
  C: TControl;
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    if C is TLabel then
      Continue;
    if C = CloseButton then
      Continue;
    if C is TIndeterminateProgress then
      Continue;
    C.Enabled := False;
  end;
end;

procedure TComposeForm.EnableEdits;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := True;
end;

procedure TComposeForm.CloseButtonClick(Sender: TObject);
begin
  if FThread = nil then
    Close
  else
  begin
    Progress.Caption := 'Stopping';
    FCancelled := True;
    FThread.Terminate;
  end;
  CloseButton.Enabled := False;
end;

procedure TComposeForm.BuildButtonClick(Sender: TObject);

  procedure Validate(State: Boolean; const Msg: string; W: TWinControl);
  begin
    if State then
    begin
      W.Color := clDefault;
      Exit;
    end;
    Progress.Status := psError;
    Progress.Caption := Msg;
    FPriorStatus := Progress.Status;
    FPriorCaption := Progress.Caption;
    W.Color := W.CurrentColor.Blend(clRed, 0.2).Color;
    W.SetFocus;
    Abort;
  end;

var
  S: string;
begin
  if FThread = nil then
  begin
    FPackage.FileName := Trim(AppEdit.Text);
    Validate(FileExists(FPackage.FileName), 'Application file not found', AppEdit);
    S := FileArchitecture(FPackage.FileName);
    Validate(S <> '', 'Application file is invalid', AppEdit);
    IconImage.CopyTo(FPackage.Icon);
    S := FileExtractPath(FPackage.FileName);
    ChDir(S);
    FPackage.Caption := Trim(CaptionEdit.Text);
    Validate(FPackage.Caption.Length > 1, 'Invalid caption', CaptionEdit);
    FPackage.Name := Trim(PackageEdit.Text);
    Validate(FPackage.Name.IsIdentifier, 'Invalid package identifier', PackageEdit);
    FPackage.Version := Trim(VerisonEdit.Text);
    Validate(SectionBox.ItemIndex > -1, 'Invalid category', SectionBox);
    case SectionBox.ItemIndex of
      0: S := 'admin';
      1: S := 'comm';
      2: S := 'database';
      3: S := 'devel';
      4: S := 'editors';
      5: S := 'electronics';
      6: S := 'fonts';
      7: S := 'games';
      8: S := 'graphics';
      9: S := 'math';
      10: S := 'web';
      11: S := 'net';
      12: S := 'news';
      13: S := 'science';
      14: S := 'sound';
      15: S := 'utils';
      16: S := 'video';
    else
      S := 'misc';
    end;
    FPackage.Section := S;
    FPackage.Author := Trim(AuthorEdit.Text);
    FPackage.Website := Trim(WebsiteEdit.Text);
    FPackage.ShortInfo := Trim(ShortEdit.Text);
    FPackage.LongInfo := Trim(LongEdit.Text);
    Progress.Caption := 'Creating deb package';
    Progress.Status := psBusy;
    Progress.Visible := True;
    CloseButton.SetFocus;
    CloseButton.Caption := 'Stop';
    BuildButton.Enabled := False;
    FCancelled := False;
    DisableEdits;
    FThread := TSimpleThread.Create(FPackage.Build, DoThreadStatus, DoThreadDone);
  end;
end;

procedure TComposeForm.AppButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    AppEdit.Text := OpenDialog.FileName;
    LoadPackage(AppEdit.Text);
    AppEdit.Text := OpenDialog.FileName;
  end;
end;

end.

