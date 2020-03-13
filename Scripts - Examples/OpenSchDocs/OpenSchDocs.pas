{..............................................................................}
{                                                                              }
{ Summary   This scripts can be used to open all schematic documents in a      }
{           project.                                                           }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
procedure OpenSchDocs;
var
   PcbProject : IProject;
   DocNum     : Integer;
begin
   PcbProject := GetWorkspace.DM_FocusedProject;
   if PcbProject = nil then exit;

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

   For DocNum := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
      if (PcbProject.DM_LogicalDocuments(DocNum).DM_DocumentKind = 'SCH') and not (Client.IsDocumentOpen(PcbProject.DM_LogicalDocuments(DocNum).DM_FullPath)) then
         Client.ShowDocument(Client.OpenDocument('Sch', PcbProject.DM_LogicalDocuments(DocNum).DM_FullPath));
end;
