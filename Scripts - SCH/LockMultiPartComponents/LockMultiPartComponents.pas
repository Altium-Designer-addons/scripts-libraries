procedure LockMultiPartComponents;
var

    Workspace         : IWorkspace;
    PCBProject        : IProject;
    ProjectName       : String;
    Document          : IDocument;
    DocNum            : Integer;
    Rectangle         : TCoordRect;
    FlatHierarchy     : IDocument;
    SCHDoc            : ISCH_document;
    MaxNumber         : Integer;
    Iterator          : ISCH_Iterator;
    AComponent        : ISCH_Component;
    CompIterator      : ISCH_Iterator;
    Parameter         : ISCH_Parameter;
    Designator        : String;
    AsciiCode         : Integer;

begin

   Workspace := GetWorkspace;
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
   If (FlatHierarchy = Nil) Then
   Begin
       // First try compiling the project
       ResetParameters;
       AddStringParameter( 'Action', 'Compile' );
       AddStringParameter( 'ObjectKind', 'Project' );
       RunProcess( 'WorkspaceManager:Compile' );

       // Try Again to open the flattened document
       FlatHierarchy := PCBProject.DM_DocumentFlattened;
       If (FlatHierarchy = Nil) Then
       Begin
           ShowMessage('NOTICE: Compile the Project before Running this script.');
           Exit;
       End; // If (FlattenedDoc = Nil) Then
   End;

   Client.StartServer('SCH');
   MaxNumber := 0;                                          

   // First we eill check existing parameters, to get max number of this parameter
   For DocNum := 0 to PCBProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PCBProject.DM_LogicalDocuments(DocNum);
      if Document.DM_DocumentKind = 'SCH' then
      begin
         SCHDoc := SCHServer.GetSchDocumentByPath(Document.DM_FullPath);
         if SCHDoc <> nil then
         begin
            Iterator := SCHDoc.SchIterator_Create;
            If Iterator = Nil Then Exit;
            Try
               Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
               AComponent := Iterator.FirstSchObject;
               While AComponent <> Nil Do
               Begin
                  Try
                     CompIterator := AComponent.SchIterator_Create;
                     CompIterator.AddFilter_ObjectSet(MkSet(eParameter));

                     Parameter := CompIterator.FirstSchObject;
                     While Parameter <> Nil Do
                     Begin
                        if ((AnsiUpperCase(Parameter.Name) = 'GROUP') and (Parameter.Text <> '') and (Parameter.Text <> '*')) then
                           if StrToInt(Parameter.Text) > MaxNumber then
                              MaxNumber := StrToInt(Parameter.Text);
                        Parameter := CompIterator.NextSchObject;
                     End;
                  Finally
                     AComponent.SchIterator_Destroy(CompIterator);
                  End;
                  AComponent := Iterator.NextSchObject;
               End;
            Finally
               SchDoc.SchIterator_Destroy(Iterator);
            end;
         end;
      end;
   end;

   // Now we do same thing once again to set parts that have no value in this field
   For DocNum := 0 to PCBProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PCBProject.DM_LogicalDocuments(DocNum);
      if Document.DM_DocumentKind = 'SCH' then
      begin
         SCHDoc := SCHServer.GetSchDocumentByPath(Document.DM_FullPath);
         if SCHDoc <> nil then
         begin
            Iterator := SCHDoc.SchIterator_Create;
            If Iterator = Nil Then Exit;
            Try
               Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
               AComponent := Iterator.FirstSchObject;
               While AComponent <> Nil Do
               Begin
                  Try
                     CompIterator := AComponent.SchIterator_Create;
                     CompIterator.AddFilter_ObjectSet(MkSet(eParameter));

                     Parameter := CompIterator.FirstSchObject;
                     While Parameter <> Nil Do
                     Begin
                        if ((AnsiUpperCase(Parameter.Name) = 'GROUP') and ((Parameter.Text = '') or (Parameter.Text = ' ') or (Parameter.Text = '*'))) then
                        begin
                           Designator := AComponent.Designator.Text;
                           AsciiCode := ord(Designator[1]);
                           while ((AsciiCode > 57) or (AsciiCode < 48)) do
                           begin
                              Delete(Designator,1,1);
                              if Length(Designator) = 0 then break;
                              AsciiCode := ord(Designator[1]);
                           end;
                           if Designator <> '' then
                              Parameter.Text := IntToStr(StrToInt(Designator) + MaxNumber);
                        end;
                        Parameter := CompIterator.NextSchObject;
                     End;
                  Finally
                     AComponent.SchIterator_Destroy(CompIterator);
                  End;
                  AComponent := Iterator.NextSchObject;
               End;
            Finally
               SchDoc.SchIterator_Destroy(Iterator);
            end;
         end;
      end;
   end;
end;
