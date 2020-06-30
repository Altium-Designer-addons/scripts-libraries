
procedure TForm1.bGoClick(Sender: TObject);
var
doc            : IDocument;
WS             : IWorkspace;
CurrentLib     :ISch_Lib;
LibIterator             : ISch_Iterator;
ImplIterator            : ISch_Iterator;
LibComponent            : ISch_Component;
SchModel                : ISch_Implementation;
txt                     : string;
SchModelDatafileLink : ISch_ModelDatafileLink;

i                    : int;

begin
     WS  := GetWorkspace;
     Doc := WS.DM_FocusedDocument;
     CurrentLib := SchServer.GetCurrentSchDocument;

     If CurrentLib = Nil Then
     Begin
        ShowWarning('This is not a schematic library document. Aborting');
        Exit;
     End;
     If Doc.DM_DocumentKind <> 'SCHLIB' Then
     Begin
        ShowWarning('This is not a schematic library document. Aborting');
        Exit;
     End;     

     LibIterator := CurrentLib.SchLibIterator_Create;
     LibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
     LibComponent := LibIterator.FirstSchObject;

     while (LibComponent <> Nil) do
     begin
        { Exclude bogus components, such as title blocks. }
        if (LibComponent.Designator.Text <> '*') then
        begin
          //txt :=  LibComponent.Designator.Text;
          //txt :=  LibComponent.DesignItemId;

          ImplIterator := LibComponent.SchIterator_Create;
          ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
          SchModel := ImplIterator.FirstSchObject;
          while (SchModel <> Nil) do
          begin
               if (AnsiUpperCase(SchModel.ModelType) = 'PCBLIB') then
               begin
                   //txt :=  SchModel.ModelName;
                   SchModel.ClearAllDatafileLinks;
                   SchModel.AddDataFileLink('1',tLink.Text,'PCBLIB');
               end;
               SchModel := ImplIterator.NextSchObject;
          end;
        end;
        LibComponent.SchIterator_Destroy(ImplIterator);

        { Move on to next schematic component. }
        LibComponent := LibIterator.NextSchObject;
     end;
        CurrentLib.SchIterator_Destroy(LibIterator);
end;

