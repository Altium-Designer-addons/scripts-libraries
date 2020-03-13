{..............................................................................}
{ Summary   This Script generates single pin nets report.                      }
{           It generates report on project-level.                              } 
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure SinglePinNets;
var

    Workspace      : IWorkspace;
    PCBProject     : IProject;
    FlatHierarchy  : IDocument;
    Document       : IDocument;
    NetNum         : Integer;
    Net            : INet;
    NetName        : String;
    SinglePinsFile : TStringList;
    SaveDialog     : TSaveDialog;
    Flag           : Integer;
    FileName       : String;

begin

   Workspace   := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   If (PcbProject = nil) then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   ResetParameters;
   AddStringParameter( 'Action', 'Compile' );
   AddStringParameter( 'ObjectKind', 'Project' );
   RunProcess( 'WorkspaceManager:Compile' );

   // Compile project
   FlatHierarchy := PCBProject.DM_DocumentFlattened;

   // If we couldn't get the flattened sheet, then most likely the project has
   // not been compiled recently
   if (FlatHierarchy = nil) then
   begin
       // First try compiling the project
       ResetParameters;
       AddStringParameter( 'Action', 'Compile' );
       AddStringParameter( 'ObjectKind', 'Project' );
       RunProcess( 'WorkspaceManager:Compile' );

       // Try Again to open the flattened document
       FlatHierarchy := PCBProject.DM_DocumentFlattened;
       if (FlatHierarchy = nil) then
       begin
           ShowMessage('NOTICE: Compile the Project before Running this script.');
           Exit;
       end;
   end;

   // Here I need to cycle through all Parts and see what component parameters
   // are selected.

   SaveDialog        := TSaveDialog.Create(Application);
   SaveDialog.Title  := 'Save Report as ...';
   SaveDialog.Filter := 'txt file (*.txt)|*.txt';

   Flag := SaveDialog.Execute;
   if (not Flag) then exit;

   FileName := SaveDialog.FileName;

   // Set file extension
   FileName := ChangeFileExt(FileName, '.txt');

   SinglePinsFile := TStringList.Create;

   SinglePinsFile.Add('------------------------------------------------------------');
   SinglePinsFile.Add('Single Pin Net Report for ' + PCBProject.DM_ProjectFileName);
   SinglePinsFile.Add('-- ' + DateToStr(Date));
   SinglePinsFile.Add('-- ' + TimeToStr(Time));
   SinglePinsFile.Add('------------------------------------------------------------');
   SinglePinsFile.Add(' ');


   For NetNum := 0 to FlatHierarchy.DM_NetCount - 1 do
   begin
      Net  := FlatHierarchy.DM_Nets(NetNum);

      if ((Net.DM_PinCount = 1) and ((Net.DM_NetLabelCount <> 0) or (Net.DM_PortCount <> 0) or (Net.DM_PowerObjectCount <> 0) )) then
         SinglePinsFile.Add('Net ' + Net.DM_NetName + ' has only one pin (Pin ' + Net.DM_Pins(0).DM_Part.DM_LogicalDesignator + '-' + Net.DM_Pins(0).DM_PinNumber + ')');    
   end;

   SinglePinsFile.SaveToFile(FileName);
   SinglePinsFile.Free;

   Document  := Client.OpenDocument('Text', FileName);
   If Document <> Nil Then
       Client.ShowDocument(Document);
end;
