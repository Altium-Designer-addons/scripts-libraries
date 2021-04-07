{..............................................................................}
{ Summary   This scripts can be used to Zoom/Select/Mask component by it's     }
{           Designator. It is similar to "Jump component" command.             }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

Uses
    IniFiles;
Var
    IniFileName : String;

{..............................................................................}

{..............................................................................}
procedure ReadFromIniFile(AFileName : String);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    if (IniFile.ReadBool('CheckBoxes','Zoom',True)) then
        CheckBoxZoom.Checked := 1
    else
        CheckBoxZoom.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','Select',True)) then
        CheckBoxSelect.Checked := 1
    else
        CheckBoxSelect.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','Mask',True)) then
        CheckBoxMask.Checked := 1
    else
        CheckBoxMask.Checked := 0;

    IniFile.Free;
end;
{..............................................................................}

{..............................................................................}
Procedure WriteToIniFile(AFileName : String);
Var
    IniFile : TIniFile;
Begin
    IniFile := TIniFile.Create(AFileName);

    if (CheckBoxZoom.Checked) then
       IniFile.WriteBool('CheckBoxes','Zoom',True)
    else
       IniFile.WriteBool('CheckBoxes','Zoom',False);

    if (CheckBoxSelect.Checked) then
       IniFile.WriteBool('CheckBoxes','Select',True)
    else
       IniFile.WriteBool('CheckBoxes','Select',False);

    if (CheckBoxMask.Checked) then
       IniFile.WriteBool('CheckBoxes','Mask',True)
    else
       IniFile.WriteBool('CheckBoxes','Mask',False);

    IniFile.Free;
End;
{..............................................................................}

{..............................................................................}
procedure TZoomToComponent.OKButtonClick(Sender: TObject);
var
    ProcessLauncher : IProcessLauncher;
    Parameters : String;
begin
    Parameters := 'Apply=True|Expr=(Objectkind=''Component'') and (Name = ' + '''' + AnsiUpperCase(TextComponentDesignator.Text) + '''' + ')|Index=1';
    if (CheckBoxZoom.Checked) then
       Parameters := Parameters + '|Zoom=True'
    else
       Parameters := Parameters + '|Zoom=False';

    if (CheckBoxSelect.Checked) then
       Parameters := Parameters + '|Select=True'
    else
       Parameters := Parameters + '|Select=False';

    if (CheckBoxMask.Checked) then
       Parameters := Parameters + '|Mask=True'
    else
       Parameters := Parameters + '|Mask=False';

    ProcessLauncher := Client;
    ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
    ProcessLauncher.PostMessage('PCB:RunQuery', Parameters, Length(Parameters), Client.CurrentView);
    WriteToIniFile(IniFileName);     // Saving condition of checkboxes
    Close;
end;
{..............................................................................}

{..............................................................................}
procedure TZoomToComponent.CancelButtonClick(Sender: TObject);
begin
   Close;
end;
{..............................................................................}

{..............................................................................}
procedure TZoomToComponent.ZoomToComponentCreate(Sender: TObject);
begin
    IniFileName := ClientAPI_SpecialFolder_AltiumApplicationData + '\ZoomComponentTemp.ini';
    ReadFromIniFile(IniFileName);   // Reading condition of checkboxes
end;
{..............................................................................}
procedure TZoomToComponent.Button1Click(Sender: TObject);
{var
   ProcessLauncher : IProcessLauncher;
   Tekst : String;
begin
   ProcessLauncher := Client;
   Tekst := ProcessLauncher.PostMessage('WorkspaceManager:SaveObject','ObjectKind=Document|SaveMode=SaveAs|Type=P-CAD',256,Nil);
   ShowMessage(Tekst);  }


   var
   Board      : IPCB_Board;
   Begin
       Board := PCBServer.GetCurrentPCBBoard;
       If Board = Nil Then Exit;
   ResetParameters;
   AddStringParameter('SaveMode', 'SaveAs');
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileFormat', 'Export P-CAD ASCII');
   AddStringParameter('File', 'Nesto.pcb');
   ShowMessage(RunProcess('WorkspaceManager:SaveObject'));




end;


