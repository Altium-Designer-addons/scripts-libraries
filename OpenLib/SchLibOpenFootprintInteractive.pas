
procedure TFrmModelList.FrmModelListShow(Sender: TObject);
var

    k, l         : Integer;
    ImageIndex   : Integer;
    MM          : IMessagesManager;
    F           : Boolean;

    CurrentSchLib     : ISch_Lib;
    CurrentSch        : ISch_Sheet;

    SchLibIterator    : ISch_Iterator;

    Prj            : IProject;
    doc            : IDocument;
    SchDoc         : ISch_Document;
    Part           : IPart;
    Imp            : IComponentImplementation;
    SchLibComponent : ISch_Component;
    SelectedSchObject  : ISch_Component;


    SchModelDatafileLink : ISch_ModelDatafileLink;

    LogicalDoc : IDocument;
    OpenedLogicalDoc      : IPCB_Library;

    RetrievedPCBLibLDoc : IDocument;

    FootprintName : WideString;


    PCB             : IPCB_Board;
    Iterator        : IPCB_BoardIterator;
    ThisObject      : IPCB_Component;

    CurrentPCB          : IPCB_Board;
    CurrentPCBLib       : IPCB_Library;
    PCBLibIterator         : IPCB_BoardIterator;
    PCBLibComponent           : IPCB_Component;
    SelectedObjectInPCBLib    : IPCB_Component;
    CompCnt                 : Integer;
    ImplIterator            : ISch_Iterator;
    SchModel                : ISch_Implementation;
    SelectedSchModel : ISch_Implementation;
    footprintIndex : Integer;

    TheRow : TStringList;

    MsgReply        : integer;
    LibProject      : IPCB_Library;
    ThisLibrary     : IDocument;
    LibPath         : WideString;
    LibRef          : WideString;
    LibFootprint    : WideString;
    SourceLib       : WideString;
    Designator      : WideString;
    // --
    WS             : IWorkspace;

    IntLibReport   : TStringList;
    FilePath       : WideString;

    SelRect : TGridRect;
    SelRow : Integer;

begin
     // form stays on top:
     //SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
     FrmModelList.FormStyle := fsStayOnTop;

     // begin

    WS := GetWorkSpace;

    If WS = Nil Then
    begin
         ShowMessage('Workspace Nil.');
         Exit;
    end;

    MM := WS.DM_MessagesManager;

    //MM.ClearMessages;
    AddMessage(MM, 'Script Start','PCBOpenPartLibrary');


    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then Exit;

    // Get current schematic document.
    Doc := WS.DM_FocusedDocument;
    If Doc.DM_DocumentKind <> 'SCHLIB' Then
    Begin
        ShowWarning('This is not a schematic library document. Aborting');
        AddWarning(MM, 'Script Failure', 'This is not a schematic library document. Aborting');
        Exit;
    End;

    SchServer.ProcessControl.PreProcess(Doc, '');

        //CurrentSch :=  SchServer.GetCurrentSchDocument;
    CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

    { Sanity check. }
    if CurrentSch = Nil then
    begin
       ShowWarning('CurrentSch was Nil!!');
       Exit;
    end;

    // find selected component
    SelectedSchObject := GetCurrentSchLibComponent();

    if (SelectedSchObject = Nil) then
    begin
       ShowWarning('No Component selected. Aborting');
       AddWarning(MM, 'Script Failure','No Component selected. Aborting');
       Exit;
    end;


    // find selected footprint

    ImplIterator := SelectedSchObject.SchIterator_Create;
    ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

    SelectedSchModel := Nil;

    try


       StrGrdModels.RowCount := 2;
       StrGrdModels.ColCount := 7;

       TheRow := CreateObject(TStringList);
       TheRow.Clear();
       TheRow.Add('No.');
       TheRow.Add('ModelName');
       TheRow.Add('Description');
       TheRow.Add('Link No.');
       TheRow.Add('EntityName');
       TheRow.Add('FileKind');
       TheRow.Add('Location');

       StrGrdModels.FixedRows := 1;
       SelRow := 0;

       StrGrdModels.Rows(0) := TheRow;


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

                     SchModelDataFileLink.EntityName;
                     SchModelDataFileLink.FileKind;
                     SchModelDataFileLink.Location;

                     if (AnsiUpperCase(SchModelDataFileLink.FileKind) = AnsiUpperCase('PCBLIB')) then
                     begin
                        FootprintName := SchModelDataFileLink.EntityName;

                        TheRow.Clear();

                        if (SchModel.IsCurrent) then
                        begin
                             AddMessage(MM, 'Script Status','  This is the current model.');
                             SelectedSchModel := SchModel;
                             TheRow.Add(IntToStr(footprintIndex)+' Curr');
                        end
                        else
                        begin
                             TheRow.Add(IntToStr(footprintIndex));
                        end;

                        TheRow.Add(SchModel.ModelName);
                        TheRow.Add(SchModel.Description);
                        TheRow.Add(IntToStr(k));
                        TheRow.Add(SchModelDataFileLink.EntityName);
                        TheRow.Add(SchModelDataFileLink.FileKind);
                        TheRow.Add(SchModelDataFileLink.Location);

                        if ((footprintIndex > 0) or (k > 0)) then
                        begin
                             StrGrdModels.RowCount := StrGrdModels.RowCount+1; // add a row
                        end;
                        StrGrdModels.Rows(StrGrdModels.RowCount-StrGrdModels.FixedRows) := (TheRow);         // fill the row.

                        if (SchModel.IsCurrent) then
                        begin
                             SelRow := StrGrdModels.RowCount-StrGrdModels.FixedRows;

                        end;

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
       SelectedSchObject.SchIterator_Destroy(ImplIterator);
    end; {endtry}

    SelRect := StrGrdModels.Selection;
    SelRect.Left := StrGrdModels.FixedCols;
    SelRect.Top := SelRow;
    SelRect.Bottom := SelRow;
    SelRect.Right := StrGrdModels.ColCount-1;
    StrGrdModels.SetFocus;
    StrGrdModels.Selection := SelRect;


end;


procedure GoToFootprint(); forward;

procedure TFrmModelList.btnOkClick(Sender: TObject);
begin
     GoToFootprint();
end;


procedure GoToFootprint();
var
    selectedRowIdx : Integer;
    grdSelection : TGridRect;

    ModelNo, ModelName, Description, LinkNo, LnkEntityName, LnkFileKind, LnkLocation : TDynamicString;

    k, l         : Integer;
    ImageIndex   : Integer;
    MM          : IMessagesManager;
    F           : Boolean;

    Prj            : IProject;
    SelectedSchObject  : ISch_Component;
    SelectedSchModel : ISch_Implementation;
    CurrentPCBLib       : IPCB_Library;
    PCBLibComponent : IPCB_Component;
    RetrievedPCBLibLDoc : IDocument;
    RetrievedPCBLib : IPCB_Library;
    
    FootprintName : WideString;

    SchModelDatafileLink : ISch_ModelDatafileLink;

    LogicalDoc : IDocument;
    OpenedLogicalDoc      : IPCB_Library;

    TheRow : TStringList;

    MsgReply        : integer;
    LibProject      : IPCB_Library;
    ThisLibrary     : IDocument;
    LibPath         : WideString;
    LibRef          : WideString;
    LibFootprint    : WideString;
    SourceLib       : WideString;
    Designator      : WideString;
    // --
    WS             : IWorkspace;

    IntLibReport   : TStringList;
    FilePath       : WideString;

begin


    WS := GetWorkSpace;

    If WS = Nil Then
    begin
         ShowMessage('Workspace Nil.');
         Exit;
    end;

    MM := WS.DM_MessagesManager;

    //MM.ClearMessages;
    AddMessage(MM, 'Script Continued','PCBOpenPartLibrary');


    // import selection
    grdSelection := StrGrdModels.Selection;
    grdSelection.Top;
    ModelNo := StrGrdModels.Cells(0, grdSelection.Top );   // col, row
    ModelName := StrGrdModels.Cells(1, grdSelection.Top);
    Description := StrGrdModels.Cells(2, grdSelection.Top);
    LinkNo := StrGrdModels.Cells(3, grdSelection.Top);
    LnkEntityName := StrGrdModels.Cells(4, grdSelection.Top);
    LnkFileKind := StrGrdModels.Cells(5, grdSelection.Top);
    LnkLocation := StrGrdModels.Cells(6, grdSelection.Top);

    {TheRow.Add('No.');
    TheRow.Add('ModelName');
    TheRow.Add('Description');
    TheRow.Add('Link No.');
    TheRow.Add('EntityName');
    TheRow.Add('FileKind');
    TheRow.Add('Location');}

    AddWarning(MM, 'Script Info','Imported selection: '+ModelNo+' "'+ModelName+'" '+Description+ ' LNo '+LinkNo+'.');


    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then 
    begin
        ShowError('No Poject focused. Aborting.');
        Exit;
    end;

    SelectedSchObject := GetCurrentSchLibComponent();

    if (SelectedSchObject = Nil) then
    begin
        AddWarning(MM, 'Script Info','CurrentSchObject was nil. Aborting.');
        Exit;
    end;


    // find selected footprint

    SelectedSchModel := GetSchLibFootprint(SelectedSchObject, ModelName, Description);

    if (SelectedSchModel = Nil) then
    begin
       ShowWarning('No Component footprint implementation selected. Aborting');
       AddWarning(MM, 'Script Failure','No Component footprint implementation selected. Aborting');
    end;



    // find library of this footprint
    SelectedSchModel.DatafileLinkCount;
    //SelectedSchModel.DatafileLink;
    SelectedSchModel.ModelName;
    SelectedSchModel.DatabaseModel;
    SelectedSchModel.ModelName;


    if (SelectedSchModel.DatafileLinkCount < 1) then
    begin
         ShowWarning('Found no ('+IntToStr(SelectedSchModel.DatafileLinkCount)+') datafile links for file. Aborting.');
         AddWarning(MM, 'Script Failure','Found no ('+IntToStr(SelectedSchModel.DatafileLinkCount)+') datafile links for file. Aborting.');
         Exit;
    end;

    FootprintName := '';
    if (SelectedSchModel.DatafileLinkCount > 1) then
    begin
         //ShowWarning('Found multiple ('+IntToStr(SelectedSchModel.DatafileLinkCount)+') datafile links for file.');
         AddWarning(MM, 'Script Status','Found multiple ('+IntToStr(SelectedSchModel.DatafileLinkCount)+') datafile links for file.');
    end;

    for k:= 0 to SelectedSchModel.DatafileLinkCount-1 do
    begin
         SchModelDataFileLink := SelectedSchModel.GetState_SchDatafileLink(k);

         SchModelDataFileLink.EntityName;
         SchModelDataFileLink.FileKind;
         SchModelDataFileLink.Location;



         if ((AnsiUpperCase(SchModelDataFileLink.FileKind) = AnsiUpperCase(LnkFileKind)) and (SchModelDataFileLink.EntityName = LnkEntityName) and (SchModelDataFileLink.Location = LnkLocation)) then
         begin
            FootprintName := SchModelDataFileLink.EntityName;
            AddMessage(MM, 'Script Status','Identified the footprint "'+SchModelDataFileLink.EntityName+'" ('+SelectedSchModel.Description+') as '+SchModelDataFileLink.FileKind+' "'+SchModelDataFileLink.EntityName+'" ('+SchModelDataFileLink.Location+').');
         end;
    end;



    PCBLibComponent := Nil;

    for k := 0 to Prj.DM_LogicalDocumentCount -1 do
    begin
         LogicalDoc := Prj.DM_LogicalDocuments(k);

         if (LogicalDoc.DM_DocumentKind = 'PCBLIB') then
         begin

            if ( (Length(LnkLocation) = 0) or (AnsiCompareStr(AnsiLowerCase(LogicalDoc.DM_FileName), AnsiLowerCase(LnkLocation)) = 0 ) ) then
            begin
                // open this document.
                ResetParameters;
                AddStringParameter('ObjectKind','PCBLIB');
                AddStringParameter('FileName', LogicalDoc.DM_FullPath);
                RunProcess('WorkspaceManager:OpenObject');

                //ShowWarning('Opened Intlib contained Schematic Library"'+LDoc.DM_FileName+'" at path "'+LDoc.DM_FullPath+'" in document tree. Done.');
                AddMessage(MM, 'Script Status','Opened Project contained PCB Library"'+LogicalDoc.DM_FileName+'" at path "'+LogicalDoc.DM_FullPath+'" in document tree. Done.');


                OpenedLogicalDoc := WS.DM_GetDocumentFromPath(LogicalDoc.DM_FullPath);
                // select component
                // ISch_Lib.GetState_Current_SchComponent
                //LDoc.Current_SchComponent;
                //PCB :=  PCBServer.GetCurrentPCBBoard;
                CurrentPCBLib := PcbServer.GetCurrentPCBLibrary;
                //CurrentPCBLib.GetState_CurrentComponent;
                //Sch.SetState_Current_SchComponent(SelectedObject);


                // iterate through library to find the footprint...
                if ( FindComponentInPCBLibByName(CurrentPCBLib, FootprintName, {var} PCBLibComponent) ) then
                begin
                    RetrievedPCBLibLDoc := LogicalDoc;
                    RetrievedPCBLib := CurrentPCBLib;
                    AddMessage(MM, 'Script Status','Found Component Footprint "'+FootprintName+'" in Library iteration within Library "'+LogicalDoc.DM_FullPath+'" as '+PCBLibComponent.Name+'.');
                    
                    break;
                end
                
            end
            else
            begin
                AddMessage(MM, 'Script Status','Skipping Library Library "'+LogicalDoc.DM_FullPath+'" because of location "'+LnkLocation+'"');
            end; {if location = '' or matching}
         end; {if pcblib}
    end;

    If (PCBLibComponent <> Nil) then
    begin
        FocusComponentInPCBLibByName(RetrievedPCBLibLDoc.DM_FullPath, FootprintName);
    end;

    FrmModelList.Close;

end;


procedure TFrmModelList.btnCancelClick(Sender: TObject);
begin
     FrmModelList.Close;
end;

procedure TFrmModelList.Label1Click(Sender: TObject);
begin
     StrGrdModels.SetFocus;
end;

procedure TFrmModelList.FrmModelListMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  SelRect : TGridRect;
  SelRow  : Integer;
begin

    SelRect := StrGrdModels.Selection;
    SelRect.Left := StrGrdModels.FixedCols;
    SelRect.Right := StrGrdModels.ColCount-1;
    SelRow := SelRect.Top;

    if (SelRow < (StrGrdModels.RowCount-1)) then
    begin
        SelRow := SelRow +1;
    end;

    SelRect.Top := SelRow;
    SelRect.Bottom := SelRow;

    StrGrdModels.SetFocus;
    StrGrdModels.Selection := SelRect;

end;


procedure TFrmModelList.FrmModelListMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  SelRect : TGridRect;
  SelRow  : Integer;
begin

    SelRect := StrGrdModels.Selection;
    SelRect.Left := StrGrdModels.FixedCols;
    SelRect.Right := StrGrdModels.ColCount-1;
    SelRow := SelRect.Top;

    if (SelRow > (StrGrdModels.FixedRows)) then
    begin
        SelRow := SelRow -1;
    end;

    SelRect.Top := SelRow;
    SelRect.Bottom := SelRow;

    StrGrdModels.SetFocus;
    StrGrdModels.Selection := SelRect;

end;

procedure TFrmModelList.FrmModelListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     SBStatusBar.Panels.Items(0).Text := IntToStr(Key);

     if (Key = 27) then // esc
     begin
          FrmModelList.Close;
     end;
     if ((key = 70) and InSet(ssAlt,Shift)) then
     begin
        GoToFootprint();
     end;
     //ShowMessage(Key);
end;

procedure TFrmModelList.StrGrdModelsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     SBStatusBar.Panels.Items(0).Text := IntToStr(Key);

     if (Key = 27) then // esc
     begin
          FrmModelList.Close;
     end;

     //if (key = ord('f')) then
     if (key = 70) then
     begin
          if (InSet(ssAlt,Shift) or InSet(ssCtrl,Shift)) then
          begin
             GoToFootprint();
          end;
     end;
end;



procedure TFrmModelList.StrGrdModelsDblClick(Sender: TObject);
begin
    GoToFootprint();
end;


