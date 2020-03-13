// CreateTableOfContents  Version 5
//----------------------------------------------------------
// Procedure that compiles the focused project and creates
// a table of contents page automatically.
// Note that the project must already have a page with the
// filename of "Table of Contents" and have a hierarchical block
// pointing to the top level sheet.
//
// Written by: Ron Wierckx, C.E.T, C.I.D
// Released as freeware/open source with the following exceptions:
//  1) Any changes/additions be released also as freeware/open source so that others will benefit
//  2) the script cannot be sold or traded for capital gain.

// If you end up using this script, please let me know!  You can email me at ron@rtds.com, or
// better yet, send me a postcard.  My daughter loves looking at postcards from around the world!
// I'm curious to find out how many people find this useful.

// Changes in version 5:
//         - TOC sheet will now be marked dirty so it will be saved when you quit Altium *WORKS
//         - Correctly remove old text when the new text color has changed
//         - Small bug fixes
// Changes in version 4:
//         - Moved scrubbing of old data until *after* new TOC parameters are set.  This is so
//           you can see what you want changed as you're entering in the new data
//         - Added parameter to set the width of the column.  This allows for varying font size,
//           sheets and column amounts.
//         - TOC sheet will now be marked dirty so it will be saved when you quit Altium *NOTE may not work
//         - Consolidated some procedures as they did the exact same thing (just different parameters passed)

// Notes about use:
//
// 1) Initially I wanted to delete all text on the page except for ones which were locked.  I could not
//    find a way to do this, as it appears the 'locked' attribute is not visible to the scripting system.
//    Instead, all text which has the same color as the TOC defined colour will be deleted.  If there is
//    any text you don't want deleted from the page, I suggest incrementing or decrementing one of the RGB
//    values by one.  This will make it unique enough so that it will not get caught up in the deletions.
//
// 2) The TOC page is the top document in your logical schematic project.  This script will compile before
//    creating the TOC so as to ensure it has the correct document where it places the data.  If your project
//    does not sucessfully compile because of heirarchical problems, this script will fail.
//
// 3) Offsets and separation units are in DXP default.  Play with the numbers to see what works best for you
//    The defaults are what I selected based on my default TOC page being 11 X 17 (ANSI) sized
//
// 4) The default font set in the constants section will be used only if there was no TOC on the page previous
//    to running this.  If a TOC was already on the page, the script will scrape that data from the parameters
//    defined on the sheet from the previous run and use it instead.
//
// 5) If (like me) you want to use this without the GUI because you use the same defaults all the time, you can
//    change the constant UseGUI to false.  This will compile and annotate the TOC sheet without a GUI prompt.

Const UseGUI = True ;

// These constants are used only on a virgin TOC sheet.  Subsequent updates will get the values from stored parameters on the sheet
// Feel free to change these if you like a different default
Const cFontName = 'Arial' ;
Const cFontSize = 24 ;
Const cFontBold = True ;
Const cFontUnderline = False ;
Const cFontItalic = False ;
Const cFontColor = 8388609 ;

Const cLineColor = 12517377 ;

Const cMaxLinesInTable      = 25   ; // Number of rows in a table before a new table is created
Const cTable1XOriginOffset  = 500  ; // Offset from left edge of schematic sheet where table will be placed
Const cTable1YOriginOffset  = 200  ; // Offset from bottom edge of schematic sheet where table will be placed
Const cTableXSeparation     = 800  ; // Distance between tables (horizontal)
Const cTableYSeparation     = 30   ; // Distance between table entries
Const cHorizTableSize       = 640  ; // Width of entries in column

Const cMaxLogicalDocuments = 100 ;

Const cIconError = 4 ;
Const cIconOk    = 3 ;
Const cIconInfo  = 107 ;

Var SelectedFont           : TFontID ;
    FontManager            ;
    FontName               : String ;
    FontSize               : Integer ;
    FontBold               : Boolean ;
    FontUnderline          : Boolean ;
    FontItalic             : Boolean ;
    FontStrikeout          : Boolean ;
    FontColor              : Integer ;
    FontColorOld           : Integer ;
    FontId                 : TFont   ;
    FontAttributes         : Integer ;

Var Table1XOriginOffset    : Integer ;
    Table1YOriginOffset    : Integer ;
    TableXSeparation       : Integer ;
    TableYSeparation       : Integer ;

Var LoopIterator           : Integer ;
    CurrentWorkspace       : IWorkspace ;
    CurrentProject         : IProject ;
    CurrentSheet           : IDocument ;
    LogicalDocumentCount   : Integer ;
    FoundTOC               : Boolean ;
    SheetTOC               : ISch_Document  ;
    MessagePanel           : IMessagesManager ;

    AServerDocument        : IServerDocument ;

Procedure CreateParameterByName (SheetTOC,ParameterName,ParameterText) ;
Var Iterator  : ISch_Iterator ;
    Parameter : ISch_Parameter ;
Begin
     Parameter          := SchServer.SchObjectFactory (eParameter, eCreate_Default) ;
     Parameter.Name     := ParameterName ;
     Parameter.ShowName := False ;
     Parameter.Text     := ParameterText ;
     Parameter.IsHidden := True ;
     SheetTOC.AddSchObject(Parameter) ;
end ;

Function CheckParameterIsValid (SheetTOC,ParameterName) : Boolean ;
Var Iterator    : ISch_Iterator ;
    Parameter   : ISch_Parameter ;
    ReturnValue : Boolean ;
Begin
     Iterator := SheetTOC.SchIterator_Create ;
     Iterator.SetState_IterationDepth(eIterateFirstLevel) ;
     Iterator.AddFilter_ObjectSet(MkSet(eParameter)) ;
     ReturnValue := False ;
     Try
       Parameter := Iterator.FirstSchObject;
       While Parameter <> Nil Do
       Begin
          if Parameter.Name = ParameterName then ReturnValue := True ;
          Parameter := Iterator.NextSchObject;
       End;
    Finally
        SheetTOC.SchIterator_Destroy(Iterator);
    End;
Result := ReturnValue ;

end ;

Function GetParameterByName (SheetTOC,ParameterName) : String ;
Var Iterator    : ISch_Iterator ;
    Parameter   : ISch_Parameter ;
    ReturnValue : String ;
Begin
     ReturnValue := '' ;
// Set up iterator to look for schematic sheet parameters
    Iterator := SheetTOC.SchIterator_Create;
    Iterator.SetState_IterationDepth(eIterateFirstLevel);
    Iterator.AddFilter_ObjectSet(MkSet(eParameter));
    Try
       Parameter := Iterator.FirstSchObject;
       While Parameter <> Nil Do
       Begin
          if Parameter.Name = ParameterName then ReturnValue := Parameter.Text ;
          Parameter := Iterator.NextSchObject;
       End;
    Finally
        SheetTOC.SchIterator_Destroy(Iterator);
    End;
Result := ReturnValue ;

End ;

Procedure SetParameterByName (SheetTOC,ParameterName,ParameterText) ;
Var Iterator    : ISch_Iterator ;
    Parameter   : ISch_Parameter ;
Begin
// Set up iterator to look for schematic sheet parameters
    Iterator := SheetTOC.SchIterator_Create;
    Iterator.SetState_IterationDepth(eIterateFirstLevel);
    Iterator.AddFilter_ObjectSet(MkSet(eParameter));
    Try
       Parameter := Iterator.FirstSchObject;
       While Parameter <> Nil Do
       Begin
          if Parameter.Name = ParameterName then Parameter.Text := ParameterText;
          Parameter := Iterator.NextSchObject;
       End;
    Finally
        SheetTOC.SchIterator_Destroy(Iterator);
    End;
End ;

Procedure ClearOldLines (SheetTOC) ;
Var Iterator ;
Var ObjectSelect ;
Var ObjectToDelete ;

Begin
// Clear off old Table of Contents lines information so we can start with a fresh sheet
Iterator := SheetTOC.SchIterator_Create;
If Iterator = Nil Then Exit ;
Iterator.AddFilter_ObjectSet(MkSet(eLine));
Try
   ObjectSelect := Iterator.FirstSchObject;
   While ObjectSelect <> Nil Do
         if (ObjectSelect.LineStyle = eLineStyleSolid) and (ObjectSelect.LineWidth = eMedium) and (ObjectSelect.Color = cLineColor) then
            begin
                 ObjectToDelete := ObjectSelect ;
                 ObjectSelect := Iterator.NextSchObject;
                 SheetTOC.RemoveSchObject(ObjectToDelete);
                 SheetTOC.GraphicallyInvalidate;
            end
         else
            ObjectSelect := Iterator.NextSchObject;
Finally
       SheetTOC.SchIterator_Destroy(Iterator);
end ;
end ;

Procedure ClearOldText (SheetTOC) ;
Var Iterator ;
Var ObjectSelect ;
Var ObjectToDelete ;

Begin
// Clear off old Table of Contents text information so we can start with a fresh sheet
   Iterator := SheetTOC.SchIterator_Create; // Create iterator
   If Iterator = Nil Then Exit ;
   Iterator.AddFilter_ObjectSet(MkSet(eLabel));
   Try
      ObjectSelect := Iterator.FirstSchObject;
      While ObjectSelect <> Nil Do
      begin
         if (ObjectSelect.Color = FontColorOld) then
            begin
                  ObjectToDelete := ObjectSelect ;
                  ObjectSelect := Iterator.NextSchObject ;
                  SheetTOC.RemoveSchObject(ObjectToDelete) ;
            end
         else ObjectSelect := Iterator.NextSchObject ;
      end ;
   Finally
       SheetTOC.SchIterator_Destroy(Iterator);
   end ;
end ;

Procedure CreateLine (TargetSheet,StartX,StartY,EndX,EndY) ;
Var CreateNewSchObject ;
Begin
     CreateNewSchObject           := SchServer.SchObjectFactory(eLine,eCreate_Default) ;
     CreateNewSchObject.LineStyle := eLineStyleSolid ;
     CreateNewSchObject.LineWidth := eMedium ;
     CreateNewSchObject.Color     := cLineColor ;
     CreateNewSchObject.Location  := Point(MilsToCoord(StartX * 10),MilsToCoord(StartY * 10)) ;
     CreateNewSchObject.Corner    := Point(MilsToCoord(EndX * 10),MilsToCoord(EndY * 10)) ;
     TargetSheet.AddSchObject(CreateNewSchObject) ;
end ;

Procedure CreateText (TargetSheet,SchText,StartX,StartY) ;
Var
  CreateNewSchObject ;
Begin
     CreateNewSchObject          := SchServer.SchObjectFactory(eLabel,eCreate_Default) ;
     CreateNewSchObject.Text     := SchText ;
     CreateNewSchObject.FontId   := FontManager.GetFontID(FontSize,0,FontUnderline,FontItalic,FontBold,False,FontName);
     CreateNewSchObject.Color    := FontColor ;
     CreateNewSchObject.Location := Point(MilsToCoord(StartX * 10),MilsToCoord(StartY * 10)) ;
     TargetSheet.AddSchObject(CreateNewSchObject) ;
End ;

Procedure CreatePageNumberText (TargetSheet,SchPageNumberText,StartX,StartY) ;
Var
  CreateNewSchObject ;
  Offset ;
Begin
if SchPageNumberText < 10 then Offset := 60 else Offset := 0 ;  // Align page numbers vertically on center
   CreateNewSchObject := SchServer.SchObjectFactory(eLabel,eCreate_Default) ;
   CreateNewSchObject.Text := IntToStr(SchPageNumberText) ;
   CreateNewSchObject.FontId := FontManager.GetFontID(FontSize,0,FontUnderline,FontItalic,FontBold,False,FontName);
   CreateNewSchObject.Color := FontColor ;
   CreateNewSchObject.Location := Point(MilsToCoord(StartX * 10 + Offset),MilsToCoord(StartY * 10)) ;
   TargetSheet.AddSchObject(CreateNewSchObject) ;
End ;

Procedure CreateTableOfContents ;

// Everything declared here ends up being a type "Variant" anyways, but it's easier for
// someone else to understand what the intent of the variable is if it's explicitly declared
Var
   SchSheet               : ISch_Document ;
   ParameterSheetTitle    : ISch_Parameter ;
   ParameterSheetNumber   : ISch_Parameter ;
   CompileResult          : LongBool ;
   SchematicPageCount     : Integer ;
   LoopIterator           : Integer ;
   SchDataSheet           : ISch_BasicContainer ;
   ArrayPageTitle         : Array [1..cMaxLogicalDocuments] of String ;
   Iterator               : ISch_Iterator ;
   TableXOffset           : Integer ;
   TableYOffset           : Integer ;
   TableOffsetTrigger     : Integer ;
   TableYOffset2          : Integer ;
   TableXOrigin           : Integer ;
   TableYOrigin           : Integer ;
   MaxLinesInTable        : Integer ;
   HorizTableSize         : Integer ;

Begin
// Get all the schematic sheet page titles and stuff them into a string array in the order of the page number
SchematicPageCount := 0 ;
For LoopIterator:= 1 to LogicalDocumentCount do
    begin
         CurrentSheet := CurrentProject.DM_LogicalDocuments(LoopIterator-1) ;
         if CurrentSheet.DM_DocumentKind = 'SCH' then
            begin
                 SchematicPageCount := SchematicPageCount + 1 ;
                 SchDataSheet := SchServer.GetSchDocumentByPath(CurrentSheet.DM_FullPath) ;
                 ParameterSheetNumber := FindParameterByName(SchDataSheet,'SheetNumber') ;
                 if (VarToStr(ParameterSheetNumber.text) = '*') or (VarToStr(ParameterSheetNumber.text) = '0') or (VarToStr(ParameterSheetNumber.text) = '') then
                    begin
                         MessagePanel.BeginUpdate;
                         MessagePanel.AddMessage ('Script','Empty or non-integer page number found','CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconError,False) ;
                         MessagePanel.EndUpdate;
                         Exit ;
                    end ;
                 ParameterSheetTitle := FindParameterByName (SchDataSheet,'Title') ;
                 ArrayPageTitle [ParameterSheetNumber.Text] :=  ParameterSheetTitle.Text ;
            end ;
    end ;

// Now create text labels on the table of contents page.  Keep printing out until all pages have been accounted for
LoopIterator := 1 ;
TableXOffset := 0 ;
TableYOffset := 0 ;

TableYOrigin       := CoordToMils (SheetTOC.GetState_SheetSizeY) / 10 - StrToInt(EditVerOffset.Text) ;
TableXOrigin       := StrToInt(EditHorOffset.Text) ;
TableXSeparation   := StrToInt(EditHorSeparation.Text) ;
TableYSeparation   := StrToInt(EditVerSeparation.Text) ;
TableOffsetTrigger := StrToInt(EditColumnEntryCount.Text) ;
MaxLinesInTable    := StrToInt(EditColumnEntryCount.Text) ;
HorizTableSize     := StrToInt(EditHorizTableSize.Text) ;

For LoopIterator := 1 to SchematicPageCount do
    begin
         If LoopIterator < TableOffsetTrigger then
            begin
                 CreatePageNumberText (SheetTOC,LoopIterator,TableXOrigin+40+TableXOffset,TableYOrigin - TableYOffset) ;
                 CreateText (SheetTOC,ArrayPageTitle [LoopIterator],TableXOrigin+120+TableXOffset,TableYOrigin - TableYOffset) ;
                 CreateLine (SheetTOC,TableXOrigin + TableXOffset,TableYOrigin - TableYOffset,TableXOrigin+HorizTableSize+TableXOffset,TableYOrigin - TableYOffset) ;  //
                 TableYOffset := TableYOffset + TableYSeparation ;
            end
         else
             begin
                  CreatePageNumberText (SheetTOC,LoopIterator,TableXOrigin+40+TableXOffset,TableYOrigin - TableYOffset) ;
                  CreateText (SheetTOC,ArrayPageTitle [LoopIterator],TableXOrigin+120+TableXOffset,TableYOrigin - TableYOffset) ;
                  CreateLine (SheetTOC,TableXOrigin + TableXOffset,TableYOrigin - TableYOffset,TableXOrigin + HorizTableSize + TableXOffset,TableYOrigin - TableYOffset) ;
                  // Frame table and label
                  CreateText (SheetTOC,'PAGE',TableXOrigin + 20+TableXOffset,TableYOrigin + TableYSeparation) ;
                  CreateText (SheetTOC,'PAGE NAME',TableXOrigin + 250+TableXOffset,TableYOrigin + TableYSeparation) ;

                  CreateLine (SheetTOC,TableXOrigin+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+TableXOffset,TableYOrigin - TableYOffset) ;
                  CreateLine (SheetTOC,TableXOrigin + 100+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+100+TableXOffset,TableYOrigin - TableYOffset) ;
                  CreateLine (SheetTOC,TableXOrigin + HorizTableSize+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+HorizTableSize+TableXOffset,TableYOrigin - TableYOffset) ;

                  CreateLine (SheetTOC,TableXOrigin+TableXOffset,TableYOrigin + TableYSeparation,TableXOrigin+HorizTableSize+TableXOffset,TableYOrigin + TableYSeparation) ;
                  if not (LoopIterator = SchematicPageCount) then
                     begin
                          TableXOffset := TableXOffset + TableXSeparation ; // Start new table at an offset from the last one determined by a constant
                          TableYOffset := 0 ;
                          TableOffsetTrigger := TableOffsetTrigger + MaxLinesInTable ;
                     end ;
             end ;
    end ;

// Frame the last table and label it
if not (LoopIterator > TableOffsetTrigger) then // we need this in case it works out that the table size is a multiple of total top sheets
   begin
        if LoopIterator <= TableOffsetTrigger then TableYOffset2 := 0 else TableYOffset2 := TableYSeparation;
           CreateLine (SheetTOC,TableXOrigin+TableXOffset,TableYOrigin + TableYSeparation - TableYOffset2,TableXOrigin+HorizTableSize+TableXOffset,TableYOrigin + TableYSeparation - TableYOffset2) ;
           CreateText (SheetTOC,'PAGE',TableXOrigin + 20+TableXOffset,TableYOrigin + TableYSeparation - TableYOffset2) ;
           CreateText (SheetTOC,'PAGE NAME',TableXOrigin + 250+TableXOffset,TableYOrigin + TableYSeparation - TableYOffset2) ;
           CreateLine (SheetTOC,TableXOrigin+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+TableXOffset,TableYOrigin - TableYOffset + TableYSeparation) ;
           CreateLine (SheetTOC,TableXOrigin + 100+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+100+TableXOffset,TableYOrigin - TableYOffset + TableYSeparation) ;
           CreateLine (SheetTOC,TableXOrigin + HorizTableSize+TableXOffset,TableYOrigin+TableYSeparation,TableXOrigin+HorizTableSize+TableXOffset,TableYOrigin - TableYOffset + TableYSeparation) ;
        end ;

MessagePanel.BeginUpdate;
MessagePanel.AddMessage ('Script','Finished Table of Contents creation','CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconOk,False) ;
MessagePanel.EndUpdate;
SheetTOC.GraphicallyInvalidate;
AServerDocument.Modified := true ;
End ;

Procedure StoreParameters ;
begin
     SetParameterByName (SheetTOC,'TOCFontName',FontName) ;
     SetParameterByName (SheetTOC,'TOCFontSize',IntToStr(FontSize)) ;
     SetParameterByName (SheetTOC,'TOCFontColor',VarToStr(FontColor)) ;
     SetParameterByName (SheetTOC,'TOCFontAttributes',IntToStr(FontAttributes)) ;
     SetParameterByName (SheetTOC,'TOCMaxLinesInTable',EditColumnEntryCount.Text) ;
     SetParameterByName (SheetTOC,'TOCVerticalOffset',EditVerOffset.Text) ;
     SetParameterByName (SheetTOC,'TOCHorizontalOffset',EditHorOffset.Text) ;
     SetParameterByName (SheetTOC,'TOCHorizontalSeparation',EditHorSeparation.Text) ;
     SetParameterByName (SheetTOC,'TOCVerticalSeparation',EditVerSeparation.Text) ;
     SetParameterByName (SheetTOC,'TOCHorizTableSize',EditHorizTableSize.Text) ;
end ;

Procedure TFormMain.ButtonTOCClick(Sender: TObject);
begin
     // Get rid of previous TOC if is on the sheet, so we have a fresh start
     ClearOldText  (SheetTOC);
     ClearOldLines (SheetTOC) ;
     SheetTOC.GraphicallyInvalidate;
     CreateTableOfContents ;
     StoreParameters ;
     Close ;
end;

Procedure TFormMain.EditBoxChange(Sender:TObject;Var Key:Char) ;
begin
     case Key of
          48..57 : ; // Number keys 0 -> 9
               8 : ; // Backspace key
     else Key := #0 ;
     end ;
end;

Procedure TFormMain.Timer1Timer(Sender: TObject);
begin
     Close ;
end;

Procedure TFormMain.ButtonSetFontClick(Sender: TObject);
Begin
     FontAttributes := 0 ;
     FontDialog.Font.Name := FontName ;
     FontDialog.Font.Size := FontSize ;
     FontDialog.Execute ;

     FontName := FontDialog.Font.Name ;
     FontSize := FontDialog.Font.Size ;
     if FontDialog.Font.Color < 0 then
        FontColor := 0 
     else
         begin
              FontColorOld := FontColor ;
              FontColor := FontDialog.Font.Color ;
         end ;
     LabelFontName.Font.Name := FontName ;
     LabelFontName.Font.Size := 10 ;
     LabelFontName.Caption := FontName + ' ' + IntToStr(FontSize) ;
     LabelFontName.Color := FontDialog.Font.Color ;

if InSet(fsBold,FontDialog.Font.Style) then
   begin
        CheckBoxBold.Checked := true ;
        FontBold := true ;
        FontAttributes := FontAttributes + 1 ;
   end
else
    begin
         CheckBoxBold.Checked := false ;
         FontBold := false ;
    end ;
if InSet(fsItalic,FontDialog.Font.Style) then
   begin
        CheckBoxItalic.Checked := true ;
        FontItalic := true ;
        FontAttributes := FontAttributes + 2 ;
   end
else
    begin
         CheckBoxItalic.Checked := false ;
         FontItalic := false ;
    end ;
if InSet(fsUnderline,FontDialog.Font.Style) then
   begin
        CheckBoxUnderline.Checked := true ;
        FontUnderline := true ;
        FontAttributes := FontAttributes + 4 ;
   end
else
    begin
         CheckBoxUnderline.Checked := false ;
         FontUnderline := false ;
    end ;

end;

Procedure TFormMain.FormMainCreate(Sender: TObject);
Begin
// Set up defaults that may have been already set in the sheet parameters here
FontName      := cFontName ;
FontSize      := cFontSize ;
FontBold      := cFontBold ;
FontUnderline := cFontUnderline ;
FontItalic    := cFontItalic ;
FontColor     := cFontColor ;

// Use timer to quit the script if an error is found
Timer1.Enabled := False ;

// Get an interface to the various needed pieces
CurrentWorkspace := GetWorkSpace ;                        If CurrentWorkspace = Nil then Timer1.Enabled := True ;
FontManager      := SchServer.FontManager ;               If FontManager      = Nil Then Timer1.Enabled := True;
CurrentProject   := CurrentWorkspace.DM_FocusedProject  ; If CurrentProject   = Nil then Timer1.Enabled := True ;
MessagePanel     := CurrentWorkspace.DM_MessagesManager ; If MessagePanel     = Nil Then Timer1.Enabled := True ;

CurrentProject.DM_Compile ;

// Show message panel and clear out old messages
CurrentWorkspace.DM_ShowMessageView;
MessagePanel.ClearMessages ;

// Indicate that the script is running
MessagePanel.BeginUpdate;
     MessagePanel.AddMessage ('Script','Creating Table of Contents for ' + CurrentProject.DM_ProjectFilename,'CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconInfo,False) ;
MessagePanel.EndUpdate;

// Check that there are not more than the maximum allowed logical documents in the project
LogicalDocumentCount := CurrentProject.DM_LogicalDocumentCount ;
if LogicalDocumentCount > cMaxLogicalDocuments then
begin
     ShowMessage ('Error: More than ' + VarToStr(cMaxLogicalDocuments) + ' logical schematic pages in project.  Cannot fit TOC on one sheet') ;
     Timer1.Enabled := true ;
end ;

// Display filename of apparent top level document
MessagePanel.BeginUpdate;
MessagePanel.AddMessage ('Script','Current Top level document is: ' + Currentproject.DM_TopLevelLogicalDocument.DM_FileName,'CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconInfo,False) ;
MessagePanel.EndUpdate;

// Get sheet interface for top level document
AServerDocument :=    Client.OpenDocument('SCH',CurrentProject.DM_TopLevelLogicalDocument.DM_FullPath) ;
Client.ShowDocument(AServerDocument) ;

SheetTOC := SchServer.GetSchDocumentByPath(CurrentProject.DM_TopLevelLogicalDocument.DM_FullPath) ;
if SheetTOC = Nil then
   begin
        MessagePanel.BeginUpdate;
        MessagePanel.AddMessage ('Script','Could not find top logical document','CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconError,False) ;
        MessagePanel.EndUpdate;
        Timer1.Enabled := True ;
   end ;

// Check if style parameters are already in the sheet parameter list.  If not, create them using the defaults from the contsants at the top of the script
if NOT (CheckParameterIsValid(SheetTOC,'TOCFontName'))  then CreateParameterByName(SheetTOC,'TOCFontName',FontName) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCFontSize'))  then CreateParameterByName(SheetTOC,'TOCFontSize',IntToStr(FontSize)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCFontColor')) then CreateParameterByName(SheetTOC,'TOCFontColor',IntToStr(FontColor)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCFontAttributes')) then
    begin
         FontAttributes := 0 ;
         if FontBold then FontAttributes := FontAttributes + 1 ;
         if FontItalic Then FontAttributes := FontAttributes + 2 ;
         if FontUnderline then FontAttributes := FontAttributes + 4 ;
         CreateParameterByName (SheetTOC,'TOCFontAttributes',IntToStr(FontAttributes)) ;
    end ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCMaxLinesInTable')) then CreateParameterByName(SheetTOC,'TOCMaxLinesInTable',IntToStr(cMaxLinesInTable)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCVerticalOffset')) then CreateParameterByName(SheetTOC,'TOCVerticalOffset',IntToStr(cTable1YOriginOffset)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCHorizontalOffset')) then CreateParameterByName(SheetTOC,'TOCHorizontalOffset',IntToStr(cTable1XOriginOffset)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCHorizontalSeparation')) then CreateParameterByName(SheetTOC,'TOCHorizontalSeparation',IntToStr(cTableXSeparation)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCVerticalSeparation')) then CreateParameterByName(SheetTOC,'TOCVerticalSeparation',IntToStr(cTableYSeparation)) ;
if NOT (CheckParameterIsValid(SheetTOC,'TOCHorizTableSize')) then CreateParameterByName(SheetTOC,'TOCHorizTableSize',IntToStr(cHorizTableSize)) ;


FontName := GetParameterByName(SheetTOC,'TOCFontName') ;
FontSize := StrToInt(GetParameterByName(SheetTOC,'TOCFontSize')) ;
FontColor := StrToInt(GetParameterByName(SheetTOC,'TOCFontColor')) ;
FontColorOld := FontColor ;

FontAttributes := StrToInt(GetParameterByName(SheetTOC,'TOCFontAttributes')) ;
if (FontAttributes MOD 2) = 1 then FontBold := true else FontBold := false ;
FontAttributes := FontAttributes DIV 2 ;
if (FontAttributes MOD 2) = 1 then FontItalic := true else FontItalic := false ;
FontAttributes := FontAttributes DIV 2 ;
if (FontAttributes MOD 2) = 1 then FontUnderline := true else FontUnderline := false ;
FontAttributes := StrToInt(GetParameterByName(SheetTOC,'TOCFontAttributes')) ;


EditColumnEntryCount.Text := GetParameterByName(SheetTOC,'TOCMaxLinesInTable') ;
EditVerOffset.Text        := GetParameterByName(SheetTOC,'TOCVerticalOffset') ;
EditHorOffset.Text        := GetParameterByName(SheetTOC,'TOCHorizontalOffset') ;
EditHorSeparation.Text    := GetParameterByName(SheetTOC,'TOCHorizontalSeparation') ;
EditVerSeparation.Text    := GetParameterByName(SheetTOC,'TOCVerticalSeparation') ;
EditHorizTableSize.Text   := GetParameterByName(SheetTOC,'TOCHorizTableSize') ;

MessagePanel.BeginUpdate;
      MessagePanel.AddMessage ('Script','Found font: ' + FontName + ' ' + IntToStr(FontSize),'CreateTableOfContents', CurrentProject.DM_ProjectFilename,'','',cIconInfo,False) ;
MessagePanel.EndUpdate;

LabelFontName.Caption  := FontName + ' ' + IntToStr(FontSize) ;
LabelFontName.Font.Size := 10 ;
LabelFontName.Font.Name := FontName ;
if FontBold then CheckBoxBold.Checked := true else CheckBoxBold.Checked := false ;
if FontItalic then CheckBoxItalic.Checked := true else CheckBoxItalic.Checked := false ;
if FontUnderline then CheckBoxUnderline.Checked := true else CheckBoxUnderline.Checked := false ;

if NOT (UseGUI) then // Get rid of previous TOC if is on the sheet, so we have a fresh start
   begin
        ClearOldText  (SheetTOC);
        ClearOldLines (SheetTOC) ;
        SheetTOC.GraphicallyInvalidate;
        CreateTableOfContents ;
        FormMain.Visible := false ;
        Timer1.Enabled := True ;
   end;

end ;

