uses libMessage, libUtils;

function OpenLib_PCBComponent(WS : IWorkspace; MM : IMessagesManager) : boolean;
var
   PCB                      : IPCB_Board;
   SelectedComponent        : IPCB_Component;
                            
   ResolvedLibPath          : WideString;
                            
                            
   SchLibLibraryPath        : WideString;
   SchLibFilePath           : WideString;
   PCBLibLibraryPath        : WideString;
   PCBLibFilePath           : WideString;
                            
   LibPrj                   : IProject;
   
   LogicalPCBLibDoc         : IDocument; 
   PCBLibDoc                : IPCB_Library;
   
   SelectedComponentInLib   : IPCB_Component;

begin

    result := false;

    PCB :=  PCBServer.GetCurrentPCBBoard;
    If PCB = Nil then
    begin
       ShowWarning('This is not a PCB document');
       AddError(MM, 'Script Exception','This is not a PCB document');
       Exit;
    end;

    // get selected Component
    SelectedComponent := GetSelectedPCBObject(PCB, mkSet(eComponentObject));

    if (SelectedComponent = Nil) then
    begin
        AddError(MM, 'Script Exception', 'No component selected.');
        ShowWarning('No component selected.');
        Exit;
    end;

    // get library path
    GetPCBComponentLibraryPath(SelectedComponent, {var} SchLibLibraryPath, {var} SchLibFilePath,
                                                  {var} PCBLibLibraryPath, {var} PCBLibFilePath);

    // prepare to open library path
    if (not FileExists(PCBLibFilePath)) then
    begin
        AddMessage(MM, 'Script Status','Object Library "'+SelectedComponent.SourceFootprintLibrary+'" found in Library "'+PCBLibFilePath+'" with path "'+PCBLibLibraryPath+'" but Library file is not there. Aborting. '+#13+#10+'Check whether the library "'+SelectedComponent.SourceFootprintLibrary+'" is properly installed (globally or in project).');
        ShowWarning('File not found.'+#13+#10+ #13+#10 + 'Object Library "'+SelectedComponent.SourceFootprintLibrary+'" found in Library "'+PCBLibFilePath+'" with path "'+PCBLibLibraryPath+'" but Library file is not there. Aborting. '+#13+#10+'Check whether the library "'+SelectedComponent.SourceFootprintLibrary+'" is properly installed (globally or in project).');
        exit;
    end;

    // look, whether this library is already open in the document manager
    if (not FindOpenPCBLibraryProject(SelectedComponent.SourceFootprintLibrary, PCBLibFilePath, {var} LibPrj)) then
    begin
       // open the lib.
       
       OpenPCBLibraryOrProject(PCBLibFilePath, PCBLibLibraryPath);
       
    end;

    if (LibIsIntegrated(PCBLibFilePath)) then
    begin
        // For an integrated library, we need to open the correct sub library (pcblib), according to the PCBLibLibraryPath

        // assure the project is really open
        if (not FindOpenPCBLibraryProject(SelectedComponent.SourceFootprintLibrary, PCBLibFilePath, {var} LibPrj)) then
        begin
             AddError(MM, 'Script Status','Unable to find the opened Library "'+PCBLibFilePath+'" with path "'+PCBLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
             ShowWarning('Unable to find the opened Library "'+PCBLibFilePath+'" with path "'+PCBLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
             Exit;
        end;
        
        // open the corresponding subfile
        LogicalPCBLibDoc := Nil;
        PCBLibDoc := Nil;
        if (not OpenPCBLibrarySubfile(LibPrj, PCBLibLibraryPath,  {var} LogicalPCBLibDoc, {var} PCBLibDoc)) then
        begin
            ShowWarning('Unable to find the opened Library subfile (attempt 2) "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
            AddError(MM, 'Script Status','Unable to find the opened Library subfile (attempt 2) "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
            Exit;
        end;
    end
    else
    begin
        // get PCBLibDoc of currently open PCBLib
        LogicalPCBLibDoc := Nil;
        PCBLibDoc := PcbServer.GetCurrentPCBLibrary;
    end;
    
    // try to pre-select the component in the now open PCBLib
    
    if (not FindComponentInPCBLib(LogicalPCBLibDoc, PCBLibDoc, SelectedComponent, {var} SelectedComponentInLib)) then
    begin
        if (LogicalPCBLibDoc = Nil) then
        begin
            AddError(MM, 'Script Status','Unable to find compenent footprint "'+SelectedComponent.Pattern+'" in Library (nominal) "'+PCBLibFilePath+'".');
            ShowWarning('Unable to find compenent footprint "'+SelectedComponent.Pattern+'" in Library (nominal) "'+PCBLibFilePath+'".');
        end
        else
        begin
            AddError(MM, 'Script Status','Unable to find compenent footprint "'+SelectedComponent.Pattern+'" in Library "'+LogicalPCBLibDoc.DM_FullPath+'".');
            ShowWarning('Unable to find compenent footprint "'+SelectedComponent.Pattern+'" in Library "'+LogicalPCBLibDoc.DM_FullPath+'".');
        end;
    end;
    
    
    FocusComponentInPCBLib(LogicalPCBLibDoc, PCBLibDoc, SelectedComponent);

    result := true;
    
end;




function OpenLib_SchComponent(WS : IWorkspace; MM : IMessagesManager) : boolean;
var
   Doc                      : IDocument;
   Sch                      : ISch_Document;
   SelectedComponent        : ISch_Component;
                            
   ResolvedLibPath          : WideString;
                            
                            
   SchLibLibraryPath        : WideString;
   SchLibFilePath           : WideString;
   PCBLibLibraryPath        : WideString;
   PCBLibFilePath           : WideString;
                            
   LibPrj                   : IProject;
   
   LogicalSchLibDoc         : IDocument; 
   SchLibDoc                : ISch_Lib;
                            
   CompCnt                  : Integer;
   CompIndexInLib           : Integer;
   SelectedComponentInLib   : ISch_Component;

begin
    
    result := false;
    
    Sch :=  SchServer.GetCurrentSchDocument;
    If Sch = Nil then
    begin
       AddError(MM, 'Script Exception','This is not a schematic document');
       ShowWarning('This is not a schematic document');
       Exit;
    end;
    
    SelectedComponent := GetSelectedSchObject(Sch, mkSet(eSchComponent));
    
    if (SelectedComponent = Nil) then
    begin
        AddError(MM, 'Script Exception', 'No component selected.');
        ShowWarning('No component selected.');
        Exit;
    end;
    
    // get library path
    
    Doc := WS.DM_FocusedDocument;
    If Doc.DM_DocumentKind <> 'SCH' Then
    Begin
        ShowWarning('This is not a schematic document');
        Exit;
    End;
    
    GetSchComponentLibraryPath(SelectedComponent, {var} SchLibLibraryPath, {var} SchLibFilePath,
                                                  {var} PCBLibLibraryPath, {var} PCBLibFilePath);
    
    // prepare to open library path
    if (not FileExists(SchLibFilePath)) then
    begin
        AddMessage(MM, 'Script Status','Object Library "'+SelectedComponent.SourceLibraryName+'" found in Library "'+SchLibFilePath+'" with path "'+SchLibLibraryPath+'" but Library file is not there. Aborting. '+#13+#10+'Check whether the library "'+SelectedComponent.SourceLibraryName+'" is properly installed (globally or in project).');
        ShowWarning('File not found.'+#13+#10+ #13+#10 + 'Object Library "'+SelectedComponent.SourceLibraryName+'" found in Library "'+SchLibFilePath+'" with path "'+SchLibLibraryPath+'" but Library file is not there. Aborting. '+#13+#10+'Check whether the library "'+SelectedComponent.SourceLibraryName+'" is properly installed (globally or in project).');

        exit;
    end;
    
    // look, whether this library is already open in the document manager
    LibPrj := Nil;
    if (not FindOpenSchLibraryProject(SelectedComponent.SourceLibraryName, SchLibFilePath, {var} LibPrj)) then
    begin
       // open the lib.
       
       OpenSchLibraryOrProject(SchLibFilePath, SchLibLibraryPath);
       
    end;
    
    if (LibIsIntegrated(SchLibFilePath)) then
    begin
        // For an integrated library, we need to open the correct sub library (pcblib), according to the PCBLibLibraryPath

        // assure the project is really open
        LibPrj := Nil;
        if (not FindOpenSchLibraryProject(SelectedComponent.SourceLibraryName, SchLibFilePath, {var} LibPrj)) then
        begin
             AddError(MM, 'Script Status','Unable to find the opened Library "'+SchLibFilePath+'" with path "'+SchLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
             ShowWarning('Unable to find the opened Library "'+SchLibFilePath+'" with path "'+SchLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
             Exit;
        end;
        
        // open the corresponding subfile
        LogicalSchLibDoc := Nil;
        SchLibDoc := Nil;
        if (not OpenSchLibrarySubfile(LibPrj, SchLibLibraryPath,  {var} LogicalSchLibDoc, {var} SchLibDoc)) then
        begin
            ShowWarning('Unable to find the opened Library subfile (attempt 2) "'+SchLibFilePath+'" at path "'+SchLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
            AddError(MM, 'Script Status','Unable to find the opened Library subfile (attempt 2) "'+SchLibFilePath+'" at path "'+SchLibLibraryPath+'" in document tree. Is this really an IntLib? Execution aborted.');
            Exit;
        end;
    end
    else
    begin
        // get SchLibDoc of currently open SchLib
        LogicalSchLibDoc := Nil;
        SchLibDoc := SchServer.GetCurrentSchDocument;
    end;
    
    // try to pre-select the component in the now open PCBLib
    
    CompCnt := 0;
    CompIndexInLib := 0;
    SelectedComponentInLib := Nil;
    if (not FindComponentInSchLib(LogicalSchLibDoc, SchLibDoc, SelectedComponent, {var} CompCnt, {var} CompIndexInLib, {var} SelectedComponentInLib)) then
    begin
        if (LogicalSchLibDoc = Nil) then
        begin
            AddError(MM, 'Script Status','Unable to find compenent symbol "'+SelectedComponent.LibReference+'" in Library (nominal)"'+SchLibFilePath+'".');
            ShowWarning('Unable to find compenent symbol "'+SelectedComponent.LibReference+'" in Library (nominal) "'+SchLibFilePath+'".');
        end
        else
        begin
            AddError(MM, 'Script Status','Unable to find compenent symbol "'+SelectedComponent.LibReference+'" in Library "'+LogicalSchLibDoc.DM_FullPath+'".');
            ShowWarning('Unable to find compenent symbol "'+SelectedComponent.LibReference+'" in Library "'+LogicalSchLibDoc.DM_FullPath+'".');
        end;
    end;
    
    
    FocusComponentInSchLib(LogicalSchLibDoc, SchLibDoc, SelectedComponent, CompCnt, CompIndexInLib, SelectedComponentInLib);

    result := true;
    
end;
