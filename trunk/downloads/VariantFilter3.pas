//
// VariantFilter3
//
// Select a variant from the form and click the Apply button to highlight fitted variables.
//
// This script creates a filter expression to select components that are fitted in a variant.
// The filter expression is placed in the PCB Filter panel and applied.
// This can then be used to create variant component classes in the PCB panel.
//
//
// Created by Eric Albach    10/17/2014
//
// Based on PrintAllVariants script by Petar Perisin
//

Var
    variantsel : integer;


procedure Start;
  var
    Workspace     : IWorkspace;
    PCBProject    : IProject;
    VariantNum    : Integer;
    Variant       : IProjectVariant;
    VarArray      : Array[0..20] of String;
    FlatHierarchy : IDocument;

begin
   variantsel := 1;
   Workspace   := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   If (PcbProject = nil) then
   begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   begin
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

   // Get variant names and show them on the form
   for VariantNum := 0 to PCBProject.DM_ProjectVariantCount - 1 do
   begin;
      Variant := PCBProject.DM_ProjectVariants(VariantNum);
      VarArray[VariantNum] := Variant.DM_Description;
   end;

   if VarArray[0] <> '' then RadioButton1.Caption := VarArray[0] else RadioButton1.Hide;
   if VarArray[1] <> '' then RadioButton2.Caption := VarArray[1] else RadioButton2.Hide;
   if VarArray[2] <> '' then RadioButton3.Caption := VarArray[2] else RadioButton3.Hide;
   if VarArray[3] <> '' then RadioButton4.Caption := VarArray[3] else RadioButton4.Hide;
   if VarArray[4] <> '' then RadioButton5.Caption := VarArray[4] else RadioButton5.Hide;
   if VarArray[5] <> '' then RadioButton6.Caption := VarArray[5] else RadioButton6.Hide;
   if VarArray[6] <> '' then RadioButton7.Caption := VarArray[6] else RadioButton7.Hide;
   if VarArray[7] <> '' then RadioButton8.Caption := VarArray[7] else RadioButton8.Hide;
   if VarArray[8] <> '' then RadioButton9.Caption := VarArray[8] else RadioButton9.Hide;
   if VarArray[9] <> '' then RadioButton10.Caption := VarArray[9] else RadioButton10.Hide;

   Form1.Show;
end;


procedure GetVariants;
 var
    Workspace       : IWorkspace;
    PCBProject      : IProject;
    FlatHierarchy   : IDocument;
    ComponentNum    : Integer;
    Component       : IComponent;
    VariantNum      : Integer;
    Variant         : IProjectVariant;
    CompVariation   : IComponentVariation;
    Line            : String;
    filter          : string;
    ProcessLauncher : IProcessLauncher;

 begin
   Workspace   := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   // Cycle through all Parts and see what component parameters are selected.

   filter := 'Apply=True|Select=True|Expr=(ObjectKind = ''Component'') AND ';
   Line := '';

   If (variantsel > PCBProject.DM_ProjectVariantCount) then
   begin
      ProcessLauncher := Client;
      ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
      ProcessLauncher.PostMessage('PCB:RunQuery', 'Apply=True|Select=True|Expr=(Name = ''Variant Does Not Exist'')', Length(filter), Client.CurrentView);
      exit;
   end;

   FlatHierarchy := PCBProject.DM_DocumentFlattened;

   for VariantNum := 0 to PCBProject.DM_ProjectVariantCount - 1 do
   begin;
      Variant := PCBProject.DM_ProjectVariants(VariantNum);
      Line := Line + ',' + Variant.DM_Description;
   end;

   for ComponentNum := 0 to FlatHierarchy.DM_ComponentCount - 1 do
   begin
      Component := FlatHierarchy.DM_Components(ComponentNum);
      Line := '';
      VariantNum := variantsel -1;
      Variant := PCBProject.DM_ProjectVariants(VariantNum);
      CompVariation := Variant.DM_FindComponentVariationByUniqueId(Component.DM_UniqueId);

      If (CompVariation = Nil) Then Line := Line + '(Name = ''' + Component.DM_PhysicalDesignator + ''') OR '
      else if CompVariation.DM_VariationKind = eVariation_NotFitted then
         Line := Line
      else Line := Line + '(Name = ''' + Component.DM_PhysicalDesignator + ''') OR ';
      filter := filter + Line;
   end;

      if copy(filter,Length(filter) - 3,4) = ' OR ' then filter := copy(filter,1,Length(filter) - 3);
      if copy(filter,Length(filter) - 4,5) = ' AND ' then filter := 'Apply=True|Select=True|Expr=(Name = ''Variant Is Empty'')';

      ProcessLauncher := Client;
      ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
      ProcessLauncher.PostMessage('PCB:RunQuery', filter, Length(filter), Client.CurrentView);

 end;



procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  variantsel := 1;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  variantsel := 2;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  variantsel := 3;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton4Click(Sender: TObject);
begin
  variantsel := 4;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton5Click(Sender: TObject);
begin
  variantsel := 5;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton6Click(Sender: TObject);
begin
  variantsel := 6;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton7Click(Sender: TObject);
begin
  variantsel := 7;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton8Click(Sender: TObject);
begin
  variantsel := 8;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton9Click(Sender: TObject);
begin
  variantsel := 9;
  Form1.Close;
  GetVariants;
end;

procedure TForm1.RadioButton10Click(Sender: TObject);
begin
  variantsel := 10;
  Form1.Close;
  GetVariants;
end;


