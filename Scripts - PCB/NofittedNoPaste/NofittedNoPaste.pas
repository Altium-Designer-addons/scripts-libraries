
Procedure StartScript ();
var
  Board       : IPCB_Board;
  ComponentPCB: IPCB_Component;
  boolAss     : boolean;
  boolused    : boolean;
  NoFitye     : boolean;
  RulePaste   : IPCB_PasteMaskExpansionRule;

  filter      : string;

  Workspace     : IWorkspace;
  PCBProject    : IProject;
  VariantNum    : Integer;
  Variant       : IProjectVariant;
  FlatHierarchy   : IDocument;
  Component       : IComponent;
  ComponentNum    : Integer;
  CompVariation   : IComponentVariation;

  fitted : boolean;

Begin
  NoFitye := true;
  boolAss := False;
  boolused := False;
  fitted := True;
  Workspace   := GetWorkspace;
  PCBProject  := Workspace.DM_FocusedProject;

  filter := '(ObjectKind = ''Pad'') AND (';

  Board := PCBServer.GetCurrentPCBBoard;
  If Board = nil then Begin ShowError('Open board!'); Exit; End;



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

   Variant := PCBProject.DM_CurrentProjectVariant;

   FlatHierarchy := PCBProject.DM_DocumentFlattened;

   if Variant = NIL then
   begin
      ShowMessage('Select a board Variant.');
      exit;
   end;

   for ComponentNum := 0 to FlatHierarchy.DM_ComponentCount - 1 do
   begin
      Component := FlatHierarchy.DM_Components(ComponentNum);
      CompVariation := Variant.DM_FindComponentVariationByUniqueId(Component.DM_UniqueId);
      if (CompVariation <> NIL) then
      if (CompVariation.DM_VariationKind = eVariation_NotFitted) then
      begin
           filter := filter + '(Component = ''' + Component.DM_PhysicalDesignator + ''') OR ';
           NoFitye := false;
      end;
   end;
  filter := copy(filter,1,Length(filter) - 4) + ')';

  RulePaste := PCBServer.PCBRuleFactory(eRule_PasteMaskExpansion);
  RulePaste.Expansion := MMsToCoord(-2539);
  RulePaste.DRCEnabled := false;
  RulePaste.Name := 'Disable paste :' +Variant.DM_Description;
  RulePaste.Comment := 'Disable paste';
  RulePaste.Scope1Expression := filter;

  if NoFitye then
  begin
      ShowMessage('No NotFitted components.');
      exit;
  end;

  Board.AddPCBObject(RulePaste);

end;
