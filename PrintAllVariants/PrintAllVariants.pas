{..............................................................................}
{ Summary   This Script saves variant information to CSV file, which can then  }
{           be easily opened in excel.                                         }
{                                                                              }
{           File contains component physical designators in rows and variant   }
{           names in columns. cells in between contain "Fitted" or "Not        }
{           Fitted" value, depending on weather component is in variant or     }
{           not.                                                               }
{                                                                              }
{           After you open this file in excel, you can then merge it with BOM, }
{           and BOM will then contain info about all variants.                 }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure SaveVariantsToCSV;
var

    Workspace     : IWorkspace;
    PCBProject    : IProject;
    FlatHierarchy : IDocument;
    ComponentNum  : Integer;
    Component     : IComponent;
    VariantNum    : Integer;
    Variant       : IProjectVariant;
    CompVariation : IComponentVariation;
    Line          : String;
    VariationFile : TStringList;
    SaveDialog    : TSaveDialog;
    Flag          : Integer;
    FileName      : String;

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
   SaveDialog.Title  := 'Save Variants to CSV';
   SaveDialog.Filter := 'CSV file (*.csv)|*.csv';

   Flag := SaveDialog.Execute;
   if (not Flag) then exit;

   FileName := SaveDialog.FileName;

   // Set file extension
   FileName := ChangeFileExt(FileName, '.csv');

   VariationFile := TStringList.Create;

   Line := 'Component,[No Variations]';

   for VariantNum := 0 to PCBProject.DM_ProjectVariantCount - 1 do
   begin;
      Variant := PCBProject.DM_ProjectVariants(VariantNum);
      Line := Line + ',' + Variant.DM_Description;
   end;

   // Add header line
   VariationFile.Add(Line);

   For ComponentNum := 0 to FlatHierarchy.DM_ComponentCount - 1 do
   begin
      Component := FlatHierarchy.DM_Components(ComponentNum);

      Line := Component.DM_PhysicalDesignator + ',Fitted';

      for VariantNum := 0 to PCBProject.DM_ProjectVariantCount - 1 do
      begin;
         Variant := PCBProject.DM_ProjectVariants(VariantNum);

         CompVariation := Variant.DM_FindComponentVariationByDesignator(Component.DM_PhysicalDesignator);

         If (CompVariation = Nil) Then Line := Line + ',Fitted'
         Else if CompVariation.DM_VariationKind = eVariation_NotFitted then
              Line := Line + ',Not Fitted'
         else Line := Line + ',Fitted';
      end;

      VariationFile.Add(Line);
   end;

   VariationFile.SaveToFile(FileName);
   VariationFile.Free;
end;
