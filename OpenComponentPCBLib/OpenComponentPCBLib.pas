//    OpenComponentPCBLib
//	    by John Michael Go-Soco
//      This procedure opens the component footprint library of a selected component
//		Usage: Select component(s) on PCB, then run script. This should then open that component's associated PCBLib, 
//		then move the current library selection to that component's footprint.
Procedure OpenComponentPCBLib;
    var
        Board           : IPCB_Board;
        Iterator        : IPCB_BoardIterator;
        ThisObject      : IPCB_Component;
        MsgReply        : integer;
        LibProject      : IPCB_Library;
        ThisLibrary     : IDocument;
    Begin
        Board := PCBServer.GetCurrentPCBBoard;
        If Board = Nil then Exit;
        try
			// find the object(s) of interest
            Iterator := Board.BoardIterator_Create;
            Iterator.SetState_FilterAll;
            Iterator.Addfilter_ObjectSet(mkSet(eComponentObject));
            ThisObject := Iterator.NextPCBObject;
            while (ThisObject <> Nil) do
            begin
                If ThisObject.Selected = True then
                begin
				    // showmessage('Footprint Name: ' + ThisObject.Pattern); // Library Component Ref
					// open the library holding this component's footprint
					LibProject := GetWorkspace.DM_OpenProject(ThisObject.SourceFootprintLibrary, true);
                    // navigate to the footprint using the GotoLibraryComponent process
					ResetParameters;
					AddStringParameter('FileName',ThisObject.SourceFootprintLibrary);
					AddStringParameter('Footprint', ThisObject.Pattern);
                    RunProcess('PCB:GotoLibraryComponent');
                end;
                ThisObject := Iterator.NextPCBObject;
            end;
        finally
            Board.BoardIterator_Destroy(Iterator);
        end;
    End;