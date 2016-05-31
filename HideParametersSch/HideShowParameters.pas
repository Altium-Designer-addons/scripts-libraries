Procedure HideShowParameters(Show : Boolean);
var
   Workspace     : IWorkspace;
   Project       : IProject;
   Document      : IDocument;
   SchDocument   : ISch_Sheet;
   Iterator      : ISch_Iterator;
   ParamIterator : ISch_Iterator;
   Component     : ISch_Component;
   Parameter     : ISch_Parameter;
   i             : Integer;

begin
   Workspace   := GetWorkspace;
   Project  := Workspace.DM_FocusedProject;

   if (Project = nil) then
   Begin
      ShowMessage('No Project opened.');
      exit;
   end;
   if SchServer = Nil Then Exit;

   for i := 0 to Project.DM_LogicalDocumentCount - 1 do
   begin
        if (Project.DM_LogicalDocuments(i).DM_DocumentKind <> 'SCH' ) then Continue;
        Document := Client.OpenDocument('Sch', Project.DM_LogicalDocuments(i).DM_FullPath);
        Client.ShowDocument(Document);
        SchDocument := SchServer.GetCurrentSchDocument;
        Iterator := SchDocument.SchIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

        try
            Component := Iterator.FirstSchObject;
            while Component <> nil do
            begin
                try
                    ParamIterator := Component.SchIterator_Create;
                    ParamIterator.AddFilter_ObjectSet(MkSet(eParameter));

                    Parameter := ParamIterator.FirstSchObject;
                    while Parameter <> nil do
                    begin
                        if (UpperCase(Parameter.Text) = '=CURRENTFOOTPRINT') then
                        begin
                             Parameter.IsHidden := not Show;
                        end;
                        Parameter := ParamIterator.NextSchObject;
                    end;
                finally
                    Component.SchIterator_Destroy(ParamIterator);
                end;

                Component := Iterator.NextSchObject;
            end;

        finally
            SchDocument.SchIterator_Destroy(Iterator);
            SchDocument.GraphicallyInvalidate;
        end;
    end;

end;

Procedure HideParameters;
begin
    HideShowParameters(false);
end;

Procedure ShowParameters;
begin
    HideShowParameters(true);
end;
