function StripTrailingBackslash(filePath : TDynamicString;
                                )        : TDynamicString; forward;


function SplitStringIntoLeftAndRight(    splitMe   : TDynamicString;
                                         delimiter : TString;
                                     var leftStr   : TDynamicString;
                                     var rightStr  : TDynamicString;
                                         )         : Integer; forward;


function GetFilenameFromPath(filePath : TDynamicString) : TDynamicString; forward;

function GetDirFromPath(filePath : TDynamicString) : TDynamicString; forward;

function MoveDirUpInPath(filePath : TDynamicString) : TDynamicString; forward;


function ProjectForIntLib(LibPrjName : WideString; LibPrjPath : WideString; LibraryName : WideString, LibraryPath : WideString) : boolean; forward;

//procedure AddMessage(MM : IMessagesManager; MClass : WideString; MText : WideString); forward;

//procedure AddWarning(MM : IMessagesManager; MClass : WideString; MText : WideString); forward;

//procedure AddError(MM : IMessagesManager; MClass : WideString; MText : WideString); forward;

function OpenIntegratedLibrary(IntLibFilePath : WideString) : boolean; forward;

function GetLibNumFromLibLibraryPath(LibLibraryPath : WideString) : Integer; forward;



function GetSelectedPCBObject(PCB : IPCB_Board; AObjectSet : TObjectSet) : IPCB_Component;
// AObjectSet := mkSet(eComponentObject)
var

    Iterator        : IPCB_BoardIterator;
    ThisObject      : IPCB_Component;
    SelectedComponent  : IPCB_Component;
begin
  try
    // find the object(s) of interest
    Iterator := PCB.BoardIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.Addfilter_ObjectSet(AObjectSet);    // eComponentBodyObject
    ThisObject := Iterator.FirstPCBObject;
    SelectedComponent := Nil;
    while (ThisObject <> Nil) do
    begin
        If (ThisObject.Selected = True) then
        begin
             SelectedComponent := ThisObject;

             break;
        end;
        ThisObject := Iterator.NextPCBObject;
    end;
    finally
        PCB.BoardIterator_Destroy(Iterator);
    end;

    result := SelectedComponent;
end;


function GetSelectedSchObject(Sch : ISch_Document; AObjectSet : TObjectSet) : ISch_Component;
// AObjectSet := mkSet(eSchComponent)
var
        MM              : IMessagesManager;
        WS              : IWorkspace;
        F               : Boolean;
        //Sch             : ISch_Document;
        Iterator        : ISch_Iterator;
        ThisObject      : ISch_Component;
        SelectedObject  : ISch_Component;
        MsgReply        : integer;
        LibProject      : IPCB_Library;
        ThisLibrary     : IDocument;
        LibPath         : WideString;
        LibRef          : WideString;
        SourceLib       : WideString;
        Designator      : WideString;
        // --
        
begin
    try
        // find the object(s) of interest
        Iterator := Sch.SchIterator_Create;
        Iterator.SetState_FilterAll;
        Iterator.Addfilter_ObjectSet(AObjectSet);
        ThisObject := Iterator.FirstSchObject;
        SelectedObject := Nil;
        while (ThisObject <> Nil) do
        begin
            If (ThisObject.Selection = True) then
            begin
                
                 SelectedObject := ThisObject;

                 break;

            end;
            ThisObject := Iterator.NextSchObject;
        end;
    finally
        Sch.SchIterator_Destroy(Iterator);
    end;
    
    result := SelectedObject;
end;


function GetCurrentSchLibComponent() : ISch_Component;
var
    CurrentSchLib     : ISch_Lib;
    CurrentSch        : ISch_Sheet;

    SchLibIterator    : ISch_Iterator;

    Prj            : IProject;
    doc            : IDocument;
    SchDoc         : ISch_Document;
    Part           : IPart;
    Imp            : IComponentImplementation;
    SchLibComponent : ISch_Component;
    CurrentSchObject  : ISch_Component;
    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    WS  := GetWorkspace;
    MM := WS.DM_MessagesManager;
    
    CurrentSchObject := Nil;
    
    AddMessage(MM, 'Function Start','GetCurrentSchLibComponent');
    
    // Get current schematic document.
    Doc := WS.DM_FocusedDocument;
    If Doc.DM_DocumentKind <> 'SCHLIB' Then
    Begin
        ShowWarning('This is not a schematic library document. Aborting');
        AddWarning(MM, 'Script Failure', 'This is not a schematic library document. Aborting');
        Exit;
    End;

    
    
    //CurrentSch :=  SchServer.GetCurrentSchDocument;
    CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
    if CurrentSch = Nil then
    begin
       ShowWarning('CurrentSch was Nil!!');
       Exit;
    end;
    
    AddMessage(MM, 'Script Info','Looking for current Schematic component "'+CurrentSch.CurrentSchComponent.LibReference+'" with UID "'+CurrentSch.CurrentSchComponent.UniqueId+'".');
    
    SchServer.ProcessControl.PreProcess(CurrentSch, '');

    try // begin
        { Sanity check. }
        

        // find selected component
        SchLibIterator := CurrentSch.SchLibIterator_Create;
        SchLibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
        

       { Loop over all components in this schematic page. }
       SchLibComponent := SchLibIterator.FirstSchObject;

       while (SchLibComponent <> Nil) do
       begin
          { Exclude bogus components, such as title blocks. }
          If ((CurrentSch.CurrentSchComponent.LibReference = SchLibComponent.LibReference) and (CurrentSch.CurrentSchComponent.UniqueId = SchLibComponent.UniqueId)) then
          begin
               CurrentSchObject := SchLibComponent;
               AddMessage(MM, 'Script Info','Current Schematic component "'+CurrentSch.CurrentSchComponent.LibReference+'" with UID "'+CurrentSch.CurrentSchComponent.UniqueId+'" found as "'+SchLibComponent.UniqueId+'".');
               break;
          end;

          { Move on to next schematic component. }
          SchLibComponent := SchLibIterator.NextSchObject;
       end;
    finally
       CurrentSch.SchIterator_Destroy(SchLibIterator);
       SchServer.ProcessControl.PostProcess(CurrentSch, '');
    end; {try}
    
    result := CurrentSchObject;
end;


function GetSchLibFootprint(SchLibComponent : ISch_Component; ModelName : WideString; Description : WideString) : ISch_Implementation;
var
    ImplIterator   : ISch_Iterator;
    SchModel       : ISch_Implementation;
    SelectedSchModel : ISch_Implementation;
    footprintIndex : Integer;
    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    WS  := GetWorkspace;
    MM := WS.DM_MessagesManager;
    
    AddMessage(MM, 'Function Start','GetCurrentSchLibFootprint');
    
    result := Nil;
    SelectedSchModel := Nil;
    
    ImplIterator := SchLibComponent.SchIterator_Create;

    try
        ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

        SchModel := ImplIterator.FirstSchObject;
        { Loop over all models. }
        footprintIndex := 0;

        while (SchModel <> Nil) do
        begin

           { FIXME:  Currently we ignore all models that are of type other than PCBLIB. }

           { Make sure this model is a footprint. }
           if (AnsiUpperCase(SchModel.ModelType) = 'PCBLIB') then
           begin
              footprintIndex := footprintIndex+1;

              AddMessage(MM, 'Script Status','Found component model "'+SchModel.ModelName+'" (at index '+IntToStr(footprintIndex)+' mapping: "'+SchModel.MapAsString+'" descr: "'+SchModel.Description+'")');

              if ((SchModel.ModelName = ModelName) and (SchModel.Description = Description)) then
              begin
                   AddMessage(MM, 'Script Status','  This is the previously selected model.');
                   SelectedSchModel := SchModel;
                   //break;
              end;

           end; { endif is footprint }

           { Advance to next model within this schematic component. }
           SchModel := ImplIterator.NextSchObject;

          end; { endwhile SchModel <> Nil }

    finally
       { Free iterator. }
       SchLibComponent.SchIterator_Destroy(ImplIterator);
    end; {endtry}
    
    result := SelectedSchModel;
end;



function GetPCBComponentLibraryPath(Component : IPCB_Component;
                                    var SchLibLibraryPath : WideString; var SchLibFilePath : WideString;
                                    var PCBLibLibraryPath : WideString; var PCBLibFilePath : WideString) : WideString;
var
    IntMan              : IIntegratedLibraryManager;

    ALibraryName        : WideString;
    AComponentName      : WideString;

    DatafileIndex       : Integer;
    AModelName          : WideString;
    AModelType          : WideString;

    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    WS  := GetWorkspace;
    MM := WS.DM_MessagesManager;
                            
    SchLibLibraryPath := '';
    SchLibFilePath := '';
    PCBLibLibraryPath := '';
    PCBLibFilePath := '';
    result := 'Undefined';

    AddMessage(MM, 'Function Start','GetPCBCompenentLibraryPath');

    IntMan := IntegratedLibraryManager;
    If IntMan = Nil Then Exit;
    
    if (IntMan.InstalledLibraryCount < 1) then
    begin
         AddWarning(MM, 'Script Exception','No Libraries in current project. Aborting.');
         ShowWarning('No Libraries in current project. Aborting.');
         Exit;
    end;
    
    ALibraryName := Component.SourceLibReference;
    AComponentName := Component.SourceLibReference;
    SchLibLibraryPath := IntMan.GetComponentLocation (ALibraryName, AComponentName, (* var *) SchLibFilePath);

    ALibraryName := Component.SourceFootprintLibrary;
    AModelName := Component.Pattern;
    AModelType := 'PCBLib';

    DatafileIndex := 0;
    
    PCBLibLibraryPath := IntMan.GetComponentDatafileLocation(DatafileIndex, AModelName, AModelType, AComponentName, ALibraryName, {var} PCBLibFilePath);

    result := PCBLibFilePath;
end;


function GetSchComponentCurrentFootprint(SchComponent : ISch_Component; var SelectedSchModel : ISch_Implementation, var SelectedSchModelDatafileLink : ISch_ModelDatafileLink) : WideString;
var
    ImplIterator            : ISch_Iterator;
    SchModel                : ISch_Implementation;
    //SelectedSchModel      : ISch_Implementation;
    footprintIndex          : Integer;
    k                       : Integer;
    SchModelDatafileLink    : ISch_ModelDatafileLink;
    //SelectedSchModelDatafileLink : ISch_ModelDatafileLink;
    FootprintName           : WideString; 
    SelectedFootprintName   : WideString; 
    WS                      : IWorkspace;
    MM                      : IMessagesManager;
begin
    WS  := GetWorkspace;
    MM := WS.DM_MessagesManager;
    result := '';
    
    AddMessage(MM, 'Function Start','GetSchComponentCurrentFootprint');
    
    ImplIterator := SchComponent.SchIterator_Create;
    ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

    SelectedSchModel := Nil;

    try

      SchModel := ImplIterator.FirstSchObject;
      { Loop over all models. }
          footprintIndex := 0;



          while (SchModel <> Nil) do
          begin

             { FIXME:  Currently we ignore all models that are of type other than PCBLIB. }

             { Make sure this model is a footprint. }
             if (AnsiUpperCase(SchModel.ModelType) = 'PCBLIB') then
             begin


                AddMessage(MM, 'Script Status','Found component model "'+SchModel.ModelName+'" (at index '+IntToStr(footprintIndex)+' mapping: "'+SchModel.MapAsString+'" descr: "'+SchModel.Description+'")');


                for k:= 0 to SchModel.DatafileLinkCount-1 do
                begin
                     SchModelDataFileLink := SchModel.GetState_SchDatafileLink(k);

                     //SchModelDataFileLink.EntityName;
                     //SchModelDataFileLink.FileKind;
                     //SchModelDataFileLink.Location;

                     if (AnsiUpperCase(SchModelDataFileLink.FileKind) = AnsiUpperCase('PCBLIB')) then // take the first PCBLib link. Multiple PCBLib links per model are unlikely, but should be checked in the future. ToDo.
                     begin
                        FootprintName := SchModelDataFileLink.EntityName;

                        if (SchModel.IsCurrent) then
                        begin
                             AddMessage(MM, 'Script Status','  This is the current model: "'+FootprintName+'"');
                             SelectedSchModel := SchModel;
                             SelectedFootprintName := FootprintName;
                             SelectedSchModelDataFileLink := SchModelDataFileLink;
                             break;
                        end
                        
                     end;


                end; { end for datalinks }

                footprintIndex := footprintIndex+1;

             end; { endif is footprint }

             { Advance to next model within this schematic component. }
             SchModel := ImplIterator.NextSchObject;

          end; { endwhile SchModel <> Nil }

       { Finally clause... }
    finally
       { Free iterator. }
       SchComponent.SchIterator_Destroy(ImplIterator);
    end; {endtry}
    
    result := SelectedFootprintName;
end;


function GetSchComponentLibraryPath(Component : ISch_Component;
                                    var SchLibLibraryPath : WideString; var SchLibFilePath : WideString;
                                    var PCBLibLibraryPath : WideString; var PCBLibFilePath : WideString) : WideString;
var
    IntMan              : IIntegratedLibraryManager;
    FileName            : TDynamicString;
    ALibraryName        : WideString;
    AComponentName      : WideString;
    
    SelectedSchModel : ISch_Implementation;
    SelectedSchModelDatafileLink : ISch_ModelDatafileLink;

    DatafileIndex       : Integer;
    AModelName          : WideString;
    AModelType          : WideString;
    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    result := 'undefined';
    WS  := GetWorkspace;
    MM := WS.DM_MessagesManager;
    
    IntMan := IntegratedLibraryManager;
    If IntMan = Nil Then Exit;

    if (IntMan.InstalledLibraryCount < 1) then
    begin
         AddWarning(MM, 'Script Exception','No Libraries in current project. Aborting.');
         ShowWarning('No Libraries in current project. Aborting.');
         Exit;
    end;

    ALibraryName := Component.SourceLibraryName;
    AComponentName := Component.LibReference;
    SchLibLibraryPath := IntMan.GetComponentLocation (ALibraryName, AComponentName, (* var *) SchLibFilePath);

    ALibraryName := Component.SourceLibraryName;
    
    AModelName := GetSchComponentCurrentFootprint(Component, {var} SelectedSchModel, {var} SelectedSchModelDatafileLink);
    //AModelName := Component.LibReference; // this is just a bad guess, as there is no other information available ToDo: find the currently selected footprint.
    
    AModelType := 'PCBLib';

    DatafileIndex := 0;

    PCBLibLibraryPath := IntMan.GetComponentDatafileLocation(DatafileIndex, AModelName, AModelType, AComponentName, ALibraryName, {var} PCBLibFilePath);
    result := SchLibFilePath;
end;



function LibIsIntegrated(LibFilePath : WideString) : boolean;
var
    isintegrated : boolean;
begin
    isintegrated := SameString(Copy(LibFilePath, Length(LibFilePath)-7+1,7),'.IntLib',false);
    result := isintegrated;
end;



function FindOpenPCBLibraryProject(LibName : WideString; LibFilePath : WideString; var LibPrj : IProject) : boolean;
var

    WSMServer : IWSM_ServerInterface;
    LibPrjName      : WideString;
    LibPrjPath      : WideString;
    LibOpen         : bool;
    k               : Integer;
    isintegrated    : boolean;
    WS : IWorkspace;
    MM              : IMessagesManager;
begin

    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    MM := WS.DM_MessagesManager;

    AddMessage(MM, 'Function Start','FindOpenPCBLibraryProject');

    LibOpen := false;
    
    for k := 0 to WS.DM_ProjectCount-1 do
    begin
         LibPrj := WS.DM_Projects(k);
         LibPrjName := LibPrj.DM_ProjectFileName;
         LibPrjPath := LibPrj.DM_ProjectFullPath;

         if (ProjectForIntLib(LibPrjName, LibPrjPath,  LibName, LibFilePath)) then
         begin
            //ShowWarning('Library is assumed to be open in project "'+LibPrjName+'" at path "'+LibPrjPath+'".');
            AddWarning(MM, 'Script Status','Library is assumed to be (already) open in project "'+LibPrjName+'" at path "'+LibPrjPath+'".');
            LibOpen := true;
            break;
         end;
    end;

    if (not LibOpen) then
    begin
        LibPrj := Nil; // prevent unwanted access.
    end;

    result := LibOpen;
end;

function FindOpenSchLibraryProject(LibName : WideString; LibFilePath : WideString; var LibPrj : IProject) : boolean;
begin
    result := FindOpenPCBLibraryProject(LibName, LibFilePath, {var} LibPrj);
end;


function OpenPCBLibraryOrProject(PCBLibFilePath : WideString; PCBLibLibraryPath : WideString) : WideString;
var
    isIntegrated    : boolean;
    WS : IWorkspace;
    MM              : IMessagesManager;
begin

    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    MM := WS.DM_MessagesManager;

    AddMessage(MM, 'Function Start','OpenPCBLibraryOrProject');

    // library is not open, therefore we open it now.

    isintegrated := SameString(Copy(PCBLibFilePath, Length(PCBLibFilePath)-7+1,7),'.IntLib',false);
    if (isIntegrated = True) then // intlib
    begin
        // try to find a .LibPkg project at the same location and open this instead
        //  to prevent extraction of libraries which are already available in extracted form.

        OpenIntegratedLibrary(PCBLibFilePath);
        AddMessage(MM, 'Script Status','Object found in integrated Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library should now be open.');
        result := 'IntLib';
        Exit;
    end
    else // not integrated
    begin
        // standalone pcblib
        ResetParameters;
        AddStringParameter('Kind','PCBLIB');
        AddStringParameter('FileName', PCBLibFilePath);
        RunProcess('WorkspaceManager:OpenObject');
        //ShowWarning('Object found in Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library now open. This is a standalone library. Done.');
        AddMessage(MM, 'Script Status','Object found in standalone Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library should now be open.');
        result := 'PCBLib';
        Exit;
    end;

    //ShowWarning('Object found in Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library now open.');
    
end;


function OpenSchLibraryOrProject(SchLibFilePath : WideString; SchLibLibraryPath : WideString) : WideString;
var
    isIntegrated    : boolean;
    WS : IWorkspace;
    MM              : IMessagesManager;
begin

    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    MM := WS.DM_MessagesManager;

    AddMessage(MM, 'Function Start','OpenSchLibraryOrProject');

    // library is not open, therefore we open it now.

    isintegrated := SameString(Copy(SchLibFilePath, Length(SchLibFilePath)-7+1,7),'.IntLib',false);
    if (isIntegrated = True) then // intlib
    begin
        // try to find a .LibPkg project at the same location and open this instead
        //  to prevent extraction of libraries which are already available in extracted form.

        OpenIntegratedLibrary(SchLibFilePath);
        AddMessage(MM, 'Script Status','Object found in integrated Library "'+SchLibFilePath+'" at path "'+SchLibLibraryPath+'", library should now be open.');
        result := 'IntLib';
        Exit;
    end
    else // not integrated
    begin
        // standalone schlib
        ResetParameters;
        AddStringParameter('Kind','SCHLIB');
        AddStringParameter('FileName', SchLibFilePath);
        RunProcess('WorkspaceManager:OpenObject');
        //ShowWarning('Object found in Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library now open. This is a standalone library. Done.');
        AddMessage(MM, 'Script Status','Object found in standalone Library "'+SchLibFilePath+'" at path "'+SchLibLibraryPath+'", library should now be open.');
        result := 'PCBLib';
        Exit;
    end;

    //ShowWarning('Object found in Library "'+PCBLibFilePath+'" at path "'+PCBLibLibraryPath+'", library now open.');
    
end;


function OpenIntegratedLibrary(IntLibFilePath : WideString) : boolean;
var
    IntLibProjectGuess : WideString;
    IntLibProjectGuessExists : boolean;
    WS              : IWorkspace;
    MM              : IMessagesManager;
    DlgRes          : TModalResult;
begin

     WS  := GetWorkspace;
     If WS = Nil Then Exit;

     MM := WS.DM_MessagesManager;

     AddMessage(MM, 'Function Start','OpenIntegratedLibrary');

     // guess the lib project
     IntLibProjectGuess := StringReplace(IntLibFilePath, '.IntLib', '.LibPkg', MkSet(rfIgnoreCase));

     // assume it is one dir up (to exit Output files directory)
    IntLibProjectGuess := MoveDirUpInPath(IntLibProjectGuess);
    if (not (FileExists(IntLibProjectGuess))) then // if its not one dir up, check again in current dir.
    begin
        // go for original location
       IntLibProjectGuess := StringReplace(IntLibFilePath, '.IntLib', '.LibPkg', MkSet(rfIgnoreCase));
    end;

    //if (FileExists(IntLibProjectGuess)) then
    IntLibProjectGuessExists := (FileExists(IntLibProjectGuess));

    DlgRes := mrCancel;
    if (IntLibProjectGuessExists) then
    begin
        DlgRes := ConfirmNoYesCancelWithCaption('Open .LibPkg instead of .IntLib?','Found a .LibPkg Project for the IntLib "'+IntLibFilePath+'" at path "'+IntLibProjectGuess+'". Should this be opened instead? '+(#13+#10)+'Compiled Library might differ from project files. Yes will open the library project, No will extract files from the IntLib (be careful of overwrites), Cancel will abort the operation.');
        if (DlgRes = mrCancel) then
        begin
            AddWarning(MM, 'Script Status','User has cancelled the operation facing LibPkg vs. IntLib choice.');
            Exit;
        end;
    end;

    if ( (IntLibProjectGuessExists) and (DlgRes = mrYes)) then
    begin
        //ShowWarning('Found a .LibPkg Project for the IntLib "'+DatafileFoundInLibraryPath+'" at path "'+IntLibProjectGuess+'". This will be opened instead of the extracted IntLib. Depending on the Built Version, the source might differ.');
        AddWarning(MM, 'Script Status','Found a .LibPkg Project for the IntLib "'+IntLibFilePath+'" at path "'+IntLibProjectGuess+'". User has chosen that this will be opened instead of the extracted IntLib. Depending on the Built Version, the source might differ.');
        ResetParameters;
        AddStringParameter('Kind','IntegratedLibrary');
        AddStringParameter('FileName', IntLibProjectGuess);
        RunProcess('WorkspaceManager:OpenObject');

    end
    else
    begin
        if (IntLibProjectGuessExists) then
        begin
            AddMessage(MM, 'Script Status','Found a .LibPkg Project for the IntLib "'+IntLibFilePath+'" at path "'+IntLibProjectGuess+'". User has chosen to extract the int lib though. This will be consistent with design data. Be careful not to overwrite data.');
        end;

        ResetParameters;
        // IntLib Project (.LibPkg) not found, extract.
        AddStringParameter('Kind','IntegratedLibrary');
        AddStringParameter('FileName', IntLibFilePath);
        RunProcess('WorkspaceManager:OpenObject');
    end;
end;

function GetLibNumFromLibLibraryPath(LibLibraryPath : WideString) : Integer;
var
   Pos1, Pos2, Len, Res : Integer;
   tmp : TDynamicString;
begin
     Res := 0;
     Len := Length(LibLibraryPath);
     Pos1 := LastDelimiter('\', LibLibraryPath);
     Pos2 := LastDelimiter('.', LibLibraryPath);
     if ((Pos1 < Pos2) and (Pos1 > 0) and (Pos2 > 0) and (Pos1 <= Len) and (Pos2 <= Len)) then
     begin
          tmp := Copy(LibLibraryPath, Pos1+1,Pos2-Pos1-1);
          Res := StrToInt(tmp);
     end;

     Result := Res;
end;


function OpenPCBLibrarySubfile(LibPrj : IProject; PCBLibLibraryPath : WideString; var LogicalPCBLibDoc : IDocument; var PCBLibDoc : IPCB_Library) : boolean;
var
    LibPrjPath          : WideString;
    OpenedLibraryProj   : IProject;
    LDoc                : IDocument;
    //--
    tmpstr              : WideString;
    OpenedLDoc          : IPCB_Library;
    LibOpen             : Boolean;
    k                   : Integer;
    PCBLibNum           : Integer;
    CurrentPCBLibNum    : Integer;

    PCB                 : IPCB_Board;
    CurrentPCB          : IPCB_Board;
    CurrentPCBLib       : IPCB_Library;
    LibIterator         : IPCB_BoardIterator;
    LibComponent        : IPCB_Component;
    SelectedComponentInLib : IPCB_Component;
    CompCnt             : Integer;
    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;

    LogicalPCBLibDoc := nil;
    PCBLibDoc := nil;
    result := false;

    AddMessage(MM, 'Function Start','OpenPCBLibrarySubfile');

    LibPrjPath := LibPrj.DM_ProjectFullPath;
    
    OpenedLibraryProj := WS.DM_OpenProject(LibPrjPath, true);  // TIntegratedLibraryProjectAdapter

    // open the corresponding PCB-library in the project
    WS := GetWorkspace;

    PCBLibNum :=  GetLibNumFromLibLibraryPath(PCBLibLibraryPath);

    CurrentPCBLibNum := 0;

    //OpenedLibraryProj := WS.DM_FocusedProject;
    LibOpen := false;
    for k:= 0 to (OpenedLibraryProj.DM_LogicalDocumentCount-1) do
    begin
         LDoc := OpenedLibraryProj.DM_LogicalDocuments(k);
         // DM_DocumentIsLoaded
         // DM_DocumentKind
       //LDoc.DM_FileName;
       //LDoc.DM_FullPath;
         if (LDoc.DM_DocumentKind = 'PCBLIB') then
         begin
              if (CurrentPCBLibNum = PCBLibNum) then
              begin
                    // open this document.
                    ResetParameters;
                    AddStringParameter('ObjectKind','PCBLIB');
                    AddStringParameter('FileName', LDoc.DM_FullPath);
                    RunProcess('WorkspaceManager:OpenObject');

                    //ShowWarning('Opened Intlib contained Schematic Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" in document tree. Done.');
                    AddMessage(MM, 'Script Status','Opened Intlib/LibPkg contained PCB Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" Index='+IntToStr(CurrentPCBLibNum)+' matching location "'+PCBLibLibraryPath+'" => '+IntToStr(GetLibNumFromLibLibraryPath(PCBLibLibraryPath))+' in document tree. Done.');

                    tmpstr := LDoc.DM_FullPath;
                    OpenedLDoc := WS.DM_GetDocumentFromPath(tmpstr);
                    
                    CurrentPCBLib := PcbServer.GetCurrentPCBLibrary;
                  
                    LibOpen := true;
                    
                    LogicalPCBLibDoc := LDoc;
                    PCBLibDoc := CurrentPCBLib;
                    
                    break;
              end
              else
              begin
                    AddMessage(MM, 'Script Status','Skipping Intlib contained PCB Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" because its Index '+IntToStr(CurrentPCBLibNum)+' does not match the path "'+PCBLibLibraryPath+'" => '+IntToStr(GetLibNumFromLibLibraryPath(PCBLibLibraryPath))+' ');
              end;

              CurrentPCBLibNum := CurrentPCBLibNum + 1;
         end;
    end;
    result := LibOpen;
end;


function OpenSchLibrarySubfile(LibPrj : IProject; SchLibLibraryPath : WideString; var LogicalSchLibDoc : IDocument; var SchLibDoc : ISch_Lib) : boolean;
var
    Sch                 : ISch_Document;
    LibPrjPath          : WideString;
    OpenedLibraryProj   : IProject;
    LDoc                : IDocument;
    //--
    tmpstr              : WideString;
    OpenedLDoc          : ISch_Lib;
    LibOpen             : Boolean;
    k                   : Integer;
    SchLibNum           : Integer;
    CurrentSchLibNum    : Integer;

    CurrentSch          : ISch_Sheet;
    LibIterator         : ISch_Iterator;
    LibComponent        : ISch_Component;
        
    CompCnt             : Integer;
    WS                  : IWorkspace;
    MM                  : IMessagesManager;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;
    
    LogicalSchLibDoc := nil;
    SchLibDoc := nil;
    result := false;

    AddMessage(MM, 'Function Start','OpenPCBLibrarySubfile');

    LibPrjPath := LibPrj.DM_ProjectFullPath;
    
    OpenedLibraryProj := WS.DM_OpenProject(LibPrjPath, true);  // TIntegratedLibraryProjectAdapter

    // open the corresponding PCB-library in the project
    WS := GetWorkspace;

    SchLibNum := GetLibNumFromLibLibraryPath(SchLibLibraryPath);

    CurrentSchLibNum := 0;

    LibOpen := false;
    for k:= 0 to OpenedLibraryProj.DM_LogicalDocumentCount-1 do
    begin
         LDoc := OpenedLibraryProj.DM_LogicalDocuments(k);
         // DM_DocumentIsLoaded
         // DM_DocumentKind
       //LDoc.DM_FileName;
       //LDoc.DM_FullPath;
         if (LDoc.DM_DocumentKind = 'SCHLIB') then
         begin
              if (SchLibNum = CurrentSchLibNum) then
              begin // correct index within lib
                    // open this document.
                    ResetParameters;
                    AddStringParameter('ObjectKind','SCHLIB');
                    AddStringParameter('FileName', LDoc.DM_FullPath);
                    RunProcess('WorkspaceManager:OpenObject');

                    //ShowWarning('Opened Intlib contained Schematic Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" in document tree. Done.');
                    AddMessage(MM, 'Script Status','Opened Intlib contained Schematic Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" in document tree. Done.');

                    tmpstr := LDoc.DM_FullPath;
                    OpenedLDoc := WS.DM_GetDocumentFromPath(tmpstr);
                                        
                    SchLibDoc :=  SchServer.GetCurrentSchDocument;
                    
                    LibOpen := true;

                    LogicalSchLibDoc := LDoc;
                    //SchLibDoc := Sch;
                    
                    break;
              end
              else
              begin
                   AddMessage(MM, 'Script Status','Skipped Intlib contained Schematic Library"'+LDoc.DM_FileName+'" at index '+IntToStr(CurrentSchLibNum)+' because of path "'+LDoc.DM_FullPath+'" => index '+IntToStr(GetLibNumFromLibLibraryPath(SchLibLibraryPath))+' in document tree. Done.');
              end;

              CurrentSchLibNum := CurrentSchLibNum + 1;

         end;
    end;
        
    result := LibOpen;
end;



function FindComponentInPCBLib( LogicalPCBLibDoc : IDocument; PCBLibDoc : IPCB_Library; SelectedComponent : IPCB_Component; var SelectedComponentInLib : IPCB_Component) : boolean;
var
    CurrentPCB              : IPCB_Board;
    LibIterator             : IPCB_BoardIterator;
    LibComponent            : IPCB_Component;
    //SelectedComponentInLib  : IPCB_Component;
    CompCnt                 : Integer;
    WS                      : IWorkspace;
    MM                      : IMessagesManager;
    found                   : boolean;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;

    AddMessage(MM, 'Function Start','FindComponentInPCBLib');
    
    found := false;
    result := false;
    
    PCBServer.ProcessControl.PreProcess(PCBLibDoc, '');

    try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }
        //CurrentSch :=  SchServer.GetCurrentSchDocument;
        
        if (LogicalPCBLibDoc = Nil) then 
        begin // resort to unsafer method of current document
            CurrentPCB := PCBServer.GetCurrentPCBBoard();
        end
        else
        begin
            CurrentPCB := PCBServer.GetPCBBoardByPath(LogicalPCBLibDoc.DM_FullPath);
        end;
        
          { Sanity check. }
        if CurrentPCB = Nil then begin
            AddError(MM, 'Script Status','PCBLib should be open but PCB server returned nil for current file. Critical failure.');
            ShowWarning('CurrentPCB was Nil!!');
            Exit;
        end;

        { Look for components only }
          //Iterator := CurrentSch.SchIterator_Create;
        LibIterator := PCBLibDoc.LibraryIterator_Create;
        LibIterator.AddFilter_ObjectSet(MkSet(eSchComponent, eComponentObject));

        SelectedComponentInLib := Nil;
        CompCnt := 0;


        { Loop over all components in this schematic page. }
        LibComponent := LibIterator.FirstPCBObject;

        while (LibComponent <> Nil) do
        begin
            { Exclude bogus components, such as title blocks. (untested) }
            if (LibComponent.Description <> '*') then
            begin
                CompCnt := CompCnt + 1;
                //WriteToDebugFile('  Found component "'+LibComponent.Designator.Text+'" (symbol reference "'+component.SymbolReference+'")');

                if (SelectedComponent.Pattern = LibComponent.Name) then
                begin // found the requested (previously in schematic selected object) in lib.
                      SelectedComponentInLib := LibComponent;
                      AddMessage(MM, 'Script Status','Found Component Footprint "'+SelectedComponent.Pattern+'" in Library iteration as "'+LibComponent.Name+'".');
                      found := true;
                     break;  // this break is ok here, because we do not need CompCnt later on...
                end
            end;

            { Move on to next schematic component. }
            LibComponent := LibIterator.NextPCBObject;
        end;
    finally
        PCBLibDoc.LibraryIterator_Destroy(LibIterator);
        PCBServer.ProcessControl.PostProcess(PCBLibDoc, ''); 
    end; {try}
    result := found;
end;

function FindComponentInPCBLibByName(PCBLib : IPCB_Library; PCBComponentName : WideString; var MatchingLibComponent : IPCB_Component) : boolean;
var
    LibIterator             : IPCB_BoardIterator;
    LibComponent            : IPCB_Component;
    CompCnt                 : Integer;
    WS                      : IWorkspace;
    MM                      : IMessagesManager;
    found                   : boolean;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;
    
    MatchingLibComponent := Nil;
    result := false;
    found := false;
    
    AddMessage(MM, 'Function Start','FindComponentInPCBLibByName');
    
    LibIterator := PCBLib.LibraryIterator_Create;
    LibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    //SelectedObjectInPCBLib := Nil;
    CompCnt := 0;

    try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

       { Loop over all components in this schematic page. }
       LibComponent := LibIterator.FirstPCBObject;

       while (LibComponent <> Nil) do
       begin
          { Exclude bogus components, such as title blocks. (untested) }
          if (LibComponent.Description <> '*') then
          begin
              if (PCBComponentName = LibComponent.Name) then
              begin // found the requested (previously in schematic selected object) in lib.
                    MatchingLibComponent := LibComponent;
                    found := true;

                    AddMessage(MM, 'Script Status','Found Component Footprint "'+PCBComponentName+'" in Library iteration.');
                   break;  // this break is ok here, because we do not need CompCnt later on...
              end;
              
              CompCnt := CompCnt + 1;
          end;

          { Move on to next schematic component. }
          LibComponent := LibIterator.NextPCBObject;
       end;
    finally
       PCBLib.LibraryIterator_Destroy(LibIterator);
    end; {try}
    result := found;
end;



function FindComponentInSchLib(LogicalSchLibDoc : IDocument; SchLibDoc : ISch_Lib; SelectedComponent : ISch_Component; var CompCnt : Integer; var CompIndexInLib : Integer; var SelectedComponentInLib : ISch_Component) : boolean;
var
    CurrentSch              : ISch_Sheet;
    LibIterator             : ISch_Iterator;
    LibComponent            : ISch_Component;
    //SelectedComponentInLib  : ISch_Component;
    //CompIndexInLib          : Integer;
    //CompCnt                 : Integer;
    WS                      : IWorkspace;
    MM                      : IMessagesManager;
    found                   : boolean;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;

    AddMessage(MM, 'Function Start','FindComponentInSchLib');
    
    result := false;
    found := false;
    
    SchServer.ProcessControl.PreProcess(LogicalSchLibDoc, '');

    try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }
        
        //CurrentSch :=  SchServer.GetCurrentSchDocument;
        if (LogicalSchLibDoc = Nil) then
        begin // resort to unsafer method of current document
            CurrentSch := SchServer.GetCurrentSchDocument();
        end
        else
        begin
            CurrentSch := SchServer.GetSchDocumentByPath(LogicalSchLibDoc.DM_FullPath);
        end;


      { Sanity check. }
        if CurrentSch = Nil then begin
           ShowWarning('CurrentSch was Nil!!');
           Exit;
        end;

      { Look for components only }
      //Iterator := CurrentSch.SchIterator_Create;
      LibIterator := CurrentSch.SchLibIterator_Create;
      LibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

      SelectedComponentInLib := Nil;
      CompCnt := 0;

     { Loop over all components in this schematic page. }
     LibComponent := LibIterator.FirstSchObject;

     while (LibComponent <> Nil) do
     begin
        { Exclude bogus components, such as title blocks. }
        if (LibComponent.Designator.Text <> '*') then
        begin

            //WriteToDebugFile('  Found component "'+LibComponent.Designator.Text+'" (symbol reference "'+component.SymbolReference+'")');
            if (SelectedComponent.LibReference = LibComponent.SymbolReference) then
            begin // found the requested (previously in schematic selected object) in lib.
                  SelectedComponentInLib := LibComponent;
                  AddMessage(MM, 'Script Status','Found Component "'+SelectedComponent.LibReference+'" in Library iteration at position index '+IntToStr(CompCnt)+'.' );
                  CompIndexInLib := CompCnt;
                  found := true;
                 //break;
            end;

            CompCnt := CompCnt + 1;

        end;

        { Move on to next schematic component. }
        LibComponent := LibIterator.NextSchObject;
     end;
    finally
        CurrentSch.SchIterator_Destroy(LibIterator);
        SchServer.ProcessControl.PostProcess(LogicalSchLibDoc, '');
    end; {try}
    
    result := found;
end;



function FocusComponentInPCBLibByName(PCBLibFullFilePath : WideString; PCBComponentName : WideString) : boolean;
var
    res : boolean;
begin
    res := false;
    
    // prevent multi selection
    ResetParameters;
    RunProcess('PCB:LastComponent');

    // focus
    ResetParameters;
    AddStringParameter('FileName', PCBLibFullFilePath);
    AddStringParameter('Footprint', PCBComponentName);
    RunProcess('PCB:GotoLibraryComponent');

    RunProcess('PCB:SwitchTo2D');
    
    // prevent multi-selection
    RunProcess('PCB:PreviousComponent');
    RunProcess('PCB:NextComponent');

    // repeat focus selection
    ResetParameters;
    AddStringParameter('FileName', PCBLibFullFilePath);
    AddStringParameter('Footprint', PCBComponentName);
    RunProcess('PCB:GotoLibraryComponent');

    RunProcess('PCB:SwitchTo2D');
    res := true;
    
    
    result := res;
end;

function FocusComponentInPCBLib(LogicalPCBLibDoc : IDocument; PCBLibDoc : IPCB_Lib; SelectedComponent : IPCB_Component) : boolean;
begin
    result := FocusComponentInPCBLibByName(LogicalPCBLibDoc.DM_FullPath, SelectedComponent.Pattern);
end;

function FocusComponentInSchLib(LogicalSchLibDoc : IDocument; SchLibDoc : IPCB_Library; SelectedComponent : ISch_Component; CompCnt : Integer; CompIndexInLib : Integer; SelectedComponentInLib : ISch_Component) : boolean;
var
    res : boolean;
    k, m : Integer;
    WS                      : IWorkspace;
    MM                      : IMessagesManager;
    found                   : boolean;
begin
    WS := GetWorkSpace;
    MM := WS.DM_MessagesManager;
    
    result := false;
    
    AddMessage(MM, 'Function Start','FocusComponentInSchLib');
    
    if (CompIndexInLib <= CompCnt/2) then
    begin
       ResetParameters;
       RunProcess('SCH:FirstComponentLibraryEditor');
    end
    else
    begin
       ResetParameters;
       RunProcess('SCH:LastComponentLibraryEditor');
    end;



    If ((SelectedComponentInLib <> Nil) and (ConfirmNoYes('Look for component '+SelectedComponent.LibReference+' Part '+Chr(Ord('A') + SelectedComponent.CurrentPartID -1)+' through iteration now?') ) ) then
    begin
        for k:= 0 to CompCnt-1 do
        begin
             if (SchLibDoc.GetState_Current_SchComponent.LibReference = SelectedComponentInLib.SymbolReference) then
             begin // component found.
                 if (CompIndexInLib <= CompCnt/2) then
                 begin
                     AddMessage(MM, 'Script Status','Found Component "'+SelectedComponentInLib.SymbolReference+'" in Library iteration at position '+IntToStr(k)+' from start.');
                 end
                 else
                 begin
                      AddMessage(MM, 'Script Status','Found Component "'+SelectedComponentInLib.SymbolReference+'" in Library iteration at position '+IntToStr(k)+' from the end.');
                 end;

                 if (SelectedComponent.IsMultiPartComponent = true) then
                 begin
                      AddMessage(MM, 'Script Status','  Component "'+SelectedComponentInLib.SymbolReference+'" is a multipart component, trying to select the correct part "'+Chr(Ord('A') + SelectedComponent.CurrentPartID -1)+'" ('+IntToStr(SelectedComponent.CurrentPartID)+').');

                      for m := 0 to SelectedComponentInLib.PartCount-1 do  // select the parts
                      begin
                           if (SchLibDoc.GetState_CurrentSchComponentPartId = SelectedComponent.CurrentPartID) then
                           begin
                                AddMessage(MM, 'Script Status','  Found Component "'+SelectedComponentInLib.SymbolReference+'" part "'+Chr(Ord('A') + SelectedComponent.CurrentPartID -1)+'" ID '+IntToStr(SelectedComponent.CurrentPartID)+' as '+IntToStr(SchLibDoc.GetState_CurrentSchComponentPartId)+' on forward iteration.');
                                result := true;
                              break;
                           end;
                           ResetParameters;
                           RunProcess('SCH:NextComponentPart');
                      end;

                      for m := 0 to SelectedComponentInLib.PartCount-1 do  // select the parts
                      begin
                           if (SchLibDoc.GetState_CurrentSchComponentPartId = SelectedComponent.CurrentPartID) then
                           begin
                                AddMessage(MM, 'Script Status','  Found Component "'+SelectedComponentInLib.SymbolReference+'" part "'+Chr(Ord('A') + SelectedComponent.CurrentPartID -1)+'" ID '+IntToStr(SelectedComponent.CurrentPartID)+' as '+IntToStr(SchLibDoc.GetState_CurrentSchComponentPartId)+' on reverse iteration.');
                                result := true;
                              break;
                           end;
                           ResetParameters;
                           RunProcess('SCH:PreviousComponentPart');
                      end;

                 end;

                 break;
             end
             else
             begin // component not yet found
             //Process: SCH:NextComponentLibraryEditor
                 if (CompIndexInLib <= CompCnt/2) then
                 begin // started at first, going to end
                      ResetParameters;
                      RunProcess('SCH:NextComponentLibraryEditor');
                 end
                 else
                 begin // started at last, going to front
                      ResetParameters;
                      RunProcess('SCH:PreviousComponentLibraryEditor');
                 end;

             end;

        end; // for all components
    end; // do library iteration
end;

function StripTrailingBackslash(filePath : TDynamicString;
                                )        : TDynamicString;
var
   len      : Integer;
   position : Integer;
   temp     : TDynamicString;

begin

   { Copy input to temp. }
   temp := filePath;

   //   ShowMessage('String is "' + temp + '".');

   { Repeat until we no longer have a '\' char as the last char in this string. }
   repeat
   begin

      { Determine string length and position of last '\' char in string. }
      len := Length(temp);
      position := LastDelimiter('\', temp);

      { If the position of the last '\' char is at the end of the string, then strip it off. }
      if ( (position = len) and (len > 0) ) then
      begin
         SetLength(temp, (position-1));
      end;

   end;
   until ( (Length(temp) = 0) or (LastDelimiter('\', temp) <> (Length(temp))) );

   //   ShowMessage('String is now "' + temp + '".');

   //   ShowMessage('String is "' + temp + '".');
   //   ShowMessage('Strlen is ' + IntToStr(Length(temp)));
   //   ShowMessage('LastDelimiter returns ' + IntToStr(LastDelimiter('\', temp)));

   { Return temp as function return value. }
   result := temp;

end; { end StripTrailingBackslash() }

function GetFilenameFromPath(filePath : TDynamicString) : TDynamicString;
var
   len      : Integer;
   position : Integer;
   temp     : TDynamicString;
begin
   //temp := StripTrailingBackslash(filePath);
   temp := filePath;
   len := Length(temp);
   position := LastDelimiter('\', temp);

   if ((position > 1) and (position < len)) then
   begin
        temp := Copy(temp,position+1,len-position);
   end
   else
   begin
        temp := '';
   end;

   result:= temp;

end;

function GetDirFromPath(filePath : TDynamicString) : TDynamicString;
var
   len      : Integer;
   position : Integer;
   temp     : TDynamicString;
begin
   temp := filePath;
   position := LastDelimiter('\', temp);
   len := Length(temp);
   if (position = len) then
   begin     // only path.
        result := StripTrailingBackslash(filePath);
        Exit;
   end;

   //temp := StripTrailingBackslash(filePath);
   len := Length(temp);
   position := LastDelimiter('\', temp);

   if ((position > 1) and (position < len)) then
   begin
        temp := Copy(temp,1,position);
   end;

   result:= temp;

end;

function MoveDirUpInPath(filePath : TDynamicString) : TDynamicString;
var
   len      : Integer;
   position : Integer;
   temp     : TDynamicString;
   sdir      :  TDynamicString;
   sfile     :  TDynamicString;
begin
   sdir :=  GetDirFromPath(filePath);
   sfile :=  GetFilenameFromPath(filePath);

   temp := StripTrailingBackslash(sdir);
   len := Length(temp);
   position := LastDelimiter('\', temp);

   if ((position > 1) and (position < len)) then
   begin
        temp := Copy(temp,1,position-1);
   end;

   result:= temp+'\'+sfile;

end;


{***************************************************************************
 * function SplitStringIntoLeftAndRight()
 *  Split a string with a single delimiter character into "left" and "right"
 *  halves.
 *
 *  Returns left half in var parm leftStr.
 *  Returns right half in var parm rightStr.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitStringIntoLeftAndRight(    splitMe   : TDynamicString;
                                         delimiter : TString;
                                     var leftStr   : TDynamicString;
                                     var rightStr  : TDynamicString;
                                         )         : Integer;
var
   position : Integer;

begin

   { Assume success. }
   result := 0;
   leftStr := '';
   rightStr := '';

   { This entry will look something like "foo=bar".
    So split it into a left string (before '=' char) and a right string (after '=' char). }

   { Find the position of the next delimiter character. }
   position := AnsiPos(delimiter, splitMe);

   { Sanity check. }
   if (position <= 0) then
   begin
      result := 1;  { Flag that we had an error. }
      WriteToDebugFile('Unable to find delimiter "' + delimiter + '" in string "' + splitMe + '"!');

      { Return the original string as the left string. }
      leftStr := splitMe;

   end

   { Else we passed the sanity check.  Proceed. }
   else
   begin

      { The left string is everything up until the char before the delimiter. }
      leftStr := Copy(splitMe, 0, (position-1));
      //            WriteToDebugFile('db leftStr is "' + leftStr + '".');

      { The right string is everything after the delimiter char. }
      rightStr := Copy(splitMe, (position+1), MaxInt);
      //            WriteToDebugFile('db rightStr is "' + rightStr + '".');

   end; { endelse }

end; { end SplitStringIntoLeftAndRight() }

function ProjectForIntLib(LibPrjName : WideString; LibPrjPath : WideString; LibraryName : WideString, LibraryPath : WideString) : boolean;
var
   LibraryNameWithoutExt   : TDynamicString;
   ProjectNameWithoutExt   : TDynamicString;

begin
     LibraryNameWithoutExt := StringReplace(LibraryName, '.IntLib', '', MkSet(rfIgnoreCase));
     ProjectNameWithoutExt := StringReplace(LibPrjName, '.LibPkg', '', MkSet(rfIgnoreCase));
     Result := SameString(LibraryNameWithoutExt, ProjectNameWithoutExt, false);           // todo: check path plausibility
end;




