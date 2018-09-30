{..............................................................................}
{ Summary   This script Modifies Comments ands (ver 5.0)            }
{                                                                              }
{ Copyright (c) Randy Clemmons, MAY 28, 2017                                   }
{                                                                              }
{ Enables Convert Special Strings                                              }
{ Sets All Component Comments to .                                   }
{ Places Designators on the Appropriate TopOverLay or BottomOverLay            }
{ Places Comments on specified Mechanical Layers                               }
{ Fixes Improperly Mirrored Designators and Comments                           }
{ Option to Place Test Points (TPs) on specified Mechanical Layers             }
{ This script upports undo (Ctrl Z)                                            }
{ Validates User Inputs for Font Heights and Layers                            ]
{                                                                              }
{ Online User Guide                                                            }
{ http://altiumpcbdesigner.blogspot.com/p/re.html                              }
{..............................................................................}

var
   Board       : IPCB_Board;
   Size        : Integer;
   Units       : String;

   // Code Performance Test Variables
   startTick  : DWord;
   delta      : DWord;
   Throttle   : DWord;
   Escape     : Boolean;

// Check for floating number
function IsStringANum(strFloat : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   // Test for number, search for dot or comma
   for i := 1 to Length(strFloat) do
      if not(((ord(strFloat[i]) > 47) and (ord(strFloat[i]) < 58)) or (ord(strFloat[i]) = 44) or (ord(strFloat[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(strFloat) do
      if ((ord(strFloat[i]) = 44) or (ord(strFloat[i]) = 46)) then
         Inc(dotCount);

   if dotCount > 1 then Result := False;
end;

// Left String Function
function LeftStr(InValue: String, Len: Integer): String;
begin
     Result := Copy(InValue, 1, Len);
end;

// Calculate the height of the true type text to best fit for Microsoft Sans Serif
function CalculateSize (Size:Integer,S:String,TextLength:Integer):Integer;
begin
     case TextLength of
          1 : Result := MMsToCoord(1.30*CoordToMMs(Size)-0.06);
          2 : Result := MMsToCoord(0.72*CoordToMMs(Size)+0.61);
          3 : Result := MMsToCoord(0.43*CoordToMMs(Size)+0.11);
          4 : Result := MMsToCoord(0.32*CoordToMMs(Size)+0.13);
          5 : Result := MMsToCoord(0.26*CoordToMMs(Size)+0.15);
          6 : Result := MMsToCoord(0.21*CoordToMMs(Size)+0.15);
          7 : Result := MMsToCoord(0.19*CoordToMMs(Size)-0.22);
          else Result := -1;
     end;
     // Use Stroke Fonts
     If (cbxUseStrokeFonts.Checked = True) then
     begin
          Result := Result*0.6;  //Scaled Result for Stroked Fonts
     end;
end;

function VerifyUserInputs(junk:String):boolean;
Var
ResultFlag  : boolen;

begin

   ResultFlag : = True;  // Initialize Result Flag
   Result := True;       // Initialize VerifyUserInputs Result

   // Verify Min Height is a Number
   if not (IsStringANum(EditMinHeight.Text)) then
   begin
        ResultFlag := False;
        EditMinHeight.Font.Color := clRed;
   end
   else
   begin
        EditMinHeight.Font.Color := clWindowText;
   end;

   // Verify Max Height is a Number
   if not (IsStringANum(EditMaxHeight.Text)) then
   begin
        ResultFlag := False;
        EditMaxHeight.Font.Color := clRed;
   end
   else
   begin
        EditMaxHeight.Font.Color := clWindowText;
   end;

   // Verify Comments Top Layer is a Valid Mech Layer
   if not (IsStringANum(EditCommentsTop.Text)) OR ((StrToInt(EditCommentsTop.Text) < 1) OR (StrToInt(EditCommentsTop.Text) > 32)) then
   begin
        ResultFlag := False;
        EditCommentsTop.Font.Color := clRed;
   end
   else
   begin
        EditCommentsTop.Font.Color := clWindowText;
   end;

   // Verify Comments Bottom Layer is a Valid Mech Layer
   if not (IsStringANum(EditCommentsBot.Text)) OR ((StrToInt(EditCommentsBot.Text) < 1) OR (StrToInt(EditCommentsBot.Text) > 32)) then
   begin
        ResultFlag := False;
        EditCommentsBot.Font.Color := clRed;
   end
   else
   begin
        EditCommentsBot.Font.Color := clWindowText;
   end;

   // Verify Test Points Top Layer is a Valid Mech Layer
   if not (IsStringANum(EditTestPointsTop.Text)) OR ((StrToInt(EditTestPointsTop.Text) < 1) OR (StrToInt(EditTestPointsTop.Text) > 32)) then
   begin
        ResultFlag := False;
        EditTestPointsTop.Font.Color := clRed;
   end
   else
   begin
        EditTestPointsTop.Font.Color := clWindowText;
   end;

   // Verify Test Points Bottom Layer is a Valid Mech Layer
   if not (IsStringANum(EditTestPointsBot.Text)) OR ((StrToInt(EditTestPointsBot.Text) < 1) OR (StrToInt(EditTestPointsBot.Text) > 32)) then
   begin
        ResultFlag := False;
        EditTestPointsBot.Font.Color := clRed;
   end
   else
   begin
        EditTestPointsBot.Font.Color := clWindowText;
   end;

   if ResultFlag = False then
   begin
        ShowMessage('Invalid User Input');
        Result := False;
   end;

end;

function Read_ini(junk:string):boolean;

var
  folderName : string;
  fileName : string;
  myFile   : TextFile;
  data     : string;
begin

     //showmessage (Board.FileName);
     //showmessage (PCBServer.GetCurrentPCBBoard.FileName);
     //showmessage (ExtractFilePath(Application.ExeName));
     //showmessage (ClientAPI_SpecialFolder_AltiumApplicationData);

     // For ini file Saved in Project Folder
     FileName := (ExtractFilePath(Board.FileName))+ 'Designators.ini';

     // Global ini file
     if Not FileExists(FileName) then
     begin
       // Open text file for writing
       folderName := ExtractFilePath(ClientAPI_SpecialFolder_AltiumApplicationData) + 'Scripts\';
       CreateDir (folderName);
       FileName := folderName + 'Designators.ini';
     end;

     if FileExists(fileName) then
     begin
         AssignFile(myFile, fileName);
         // Reopen the file in read mode
         Reset(myFile);
         // Read Lines from File
         if not Eof(myFile) then ReadLn(myFile, data);
            EditMinHeight.Text:= data;
         if not Eof(myFile) then ReadLn(myFile, data);
            EditMaxHeight.Text:= data;
         if not Eof(myFile) then ReadLn(myFile, data);
            EditCommentsTop.Text:= data;
         if not Eof(myFile) then ReadLn(myFile, data);
            EditCommentsBot.Text:= data;
         if not Eof(myFile) then ReadLn(myFile, data);
            EditTestPointsTop.Text:= data;
         if not Eof(myFile) then ReadLn(myFile, data);
            EditTestPointsBot.Text:= data;

         CloseFile(myFile);
         result := True
     end;
end;

function Write_ini(junk:string):boolean;

var
  folderName : string;
  fileName : string;
  myFile : TextFile;

begin
    // For .ini files Saved in Project Folder
    fileName := (ExtractFilePath(Board.FileName)) + 'Designators.ini';
    AssignFile(myFile, fileName);
      ReWrite(myFile);
      Write(myFile, EditMinHeight.Text + #13#10);
      Write(myFile, EditMaxHeight.Text + #13#10);
      Write(myFile, EditCommentsTop.Text + #13#10);
      Write(myFile, EditCommentsBot.Text + #13#10);
      Write(myFile, EditTestPointsTop.Text + #13#10);
      Write(myFile, EditTestPointsBot.Text + #13#10);
    CloseFile(myFile);
end;

function ModifyStrings (junk:String):boolean;
// Junk is used hide the Function
// And to Create the Apply and OK Buttons options
Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    S                       : TPCBString;
    MaxX                    : Integer;
    MinX                    : Integer;
    MaxY                    : Integer;
    MinY                    : Integer;
    MinW                    : float;  // Minimum Stroke Font Width mils
    X                       : Integer;
    Y                       : Integer;
    Size                    : Integer;
    TextLength              : Integer;
    Designator              : IPCB_Text;

    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    R                       : TCoordRect;
    i                       : integer;

    MaximumHeight           : float;   // millimeters
    MinimumHeight           : float;   // milimeters
    ShowOnce                : Boolean; // Only display the To many characters errors one time

    MechTop                 : IPCB_LayerObject;
    MechBot                 : IPCB_LayerObject;

    TestPointTop            : IPCB_LayerObject;
    TestPointBot            : IPCB_LayerObject;

    pcbIterator             : IPCB_BoardIterator;
    pcbObject               : IPCB_Primitive;

    ChangedFlag             : Boolean;

begin

     // Start Elapsed Timer
     Application.ProcessMessages; // Do Events
     startTick := GetTickCount;
     Throttle := 0;
     delta := GetTickCount - startTick;
     lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Starting';
     lblTime.Update;   // Update Form

     // Fetch User Data in Form

     Board := PCBServer.GetCurrentPCBBoard;

     if Board = nil then
     begin
          ShowMessage('Active Window is Not a .PcbDoc File');
          exit;
     end;

     // User defined Minimum Stroke Font Width in Mils
     MinW := 2;

     if RadioButtonMM.Checked then
     begin // Metric
        MaximumHeight := MMsToCoord(StrToFloat(EditMaxHeight.Text));
        MinimumHeight := MMsToCoord(StrToFloat(EditMinHeight.Text));
     end
     else
     begin // English
        MaximumHeight := MilsToCoord(StrToFloat(EditMaxHeight.Text));
        MinimumHeight := MilsToCoord(StrToFloat(EditMinHeight.Text));
     end;

     PCBSystemOptions := PCBServer.SystemOptions;

     If PCBSystemOptions = Nil Then Exit;

     // Disables Online DRC during designator movement to improve speed
     DRCSetting := PCBSystemOptions.DoOnlineDRC;
     PCBSystemOptions.DoOnlineDRC := False;

     // ***** User choices of Mechanical Layers for Comments

     MechTop := ILayer.MechanicalLayer(StrToInt(EditCommentsTop.Text));
     MechBot := ILayer.MechanicalLayer(StrToInt(EditCommentsBot.Text));

     // ***** Test Point Layers
     TestPointTop := ILayer.MechanicalLayer(StrToInt(EditTestPointsTop.Text));
     TestPointBot := ILayer.MechanicalLayer(StrToInt(EditTestPointsBot.Text));

     try

        // First Put Designators and Comments on Overlay Layers
        // Necessary Step to Fix Mirrored Strings

        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        Component := ComponentIteratorHandle.FirstPCBObject;

        //showinfo(Board.SelectecObjectCount);

        i := 0;
        while (Component <> Nil) AND (Board.SelectecObjectCount > i) AND Not Escape Do

        begin  // *****

           if EscKeyDown then begin Escape := True; showmessage('Escaping Initial Layers'); end;

           Component.Name.BeginModify;
           Component.Comment.BeginModify;

           // Move Designators to Overlay Layers
           if Component.Layer = eTopLayer then Component.Name.Layer := eTopOverlay
           else                                Component.Name.Layer := eBottomOverlay;

           // Move Comments to to Overlay Layers
           if Component.Layer = eTopLayer then Component.Comment.Layer := eTopOverlay
           else                                Component.Comment.Layer := eBottomOverlay;

           i := i +1;

           // Progress Meter - Small Performance Hit for User Feedback
           delta := GetTickCount - startTick;
           if delta > Throttle + 100 then  // fresh interval in Seconds
           begin
             Application.ProcessMessages; // Do Events
             lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Initial Layers';
             lblTime.Update;   // Update Form
             Throttle := delta;
           end;

           // Get the next component handle
           Component := ComponentIteratorHandle.NextPCBObject;

        end;  // Put Designators and Comments on Overlay Layers

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        // Fix Improperly Mirrored Strings
        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;

        pcbIterator := Board.BoardIterator_Create;
        pcbIterator.AddFilter_ObjectSet(MkSet(eTextObject));
        pcbIterator.AddFilter_LayerSet(AllLayers);
        pcbIterator.AddFilter_Method(eProcessAll);

        i := 0;
        pcbObject := pcbIterator.FirstPCBObject;
        while (pcbObject <> Nil) AND (Board.SelectecObjectCount > i) AND Not Escape Do
        begin

          if EscKeyDown then begin Escape := True; showmessage('Escaping Mirror Strings'); end;

          // ShowMessage(pcbObject.Text + 'Layer ' + IntToStr (pcbObject.Layer));

          pcbObject.BeginModify;

          // Fix Mirrored Top Overlay
          If (pcbObject.Layer = eTopOverlay) and (pcbObject.MirrorFlag = True) then
          begin
               pcbObject.SetState_Mirror := False;
          end;

          // Fix Bottom OverLay
          If (pcbObject.Layer = eBottomOverlay) and (pcbObject.MirrorFlag = False) then
          begin
               pcbObject.SetState_Mirror := True;
          end;

          pcbObject.EndModify;

          i := i +1;

          // Progress Meter - Small Performance Hit for User Feedback
          delta := GetTickCount - startTick;
          if delta > Throttle + 100 then  // fresh interval in Seconds
          begin
             Application.ProcessMessages; // Do Events
             lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Mirror Strings';
             lblTime.Update;   // Update Form
             Throttle := delta;
          end;

          // Get the next pcbObject
          pcbObject := pcbIterator.NextPCBObject;

        end; // Fix Improperly Mirrored Strings

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the pcbObjecthandle
        Board.BoardIterator_Destroy(pcbIterator);

        // Put Strings Back on the the Correct Layers
        // Do after Fixing Mirrored Strings
        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        i := 0;
        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) AND (Board.SelectecObjectCount > i) AND  Not Escape Do
        begin  // *****

           if EscKeyDown then begin Escape := True; showmessage('Escaping Move to Layer'); end;

           Component.BeginModify;

           // Move Designators to Overlay Layers
           if Component.Layer = eTopLayer then Component.Name.Layer := eTopOverlay
           else                                Component.Name.Layer := eBottomOverlay;

           // Move Comments to to Mechanical Layers
           if Component.Layer = eTopLayer then Component.Comment.Layer := MechTop
           else                                Component.Comment.Layer := MechBot;

           // Move Test Points to User Defined Layers
           if LeftStr(Component.Name.Text,2)= 'TP' then
           begin
              if Component.Layer = eTopLayer then Component.Comment.Layer := TestPointTop;
              if Component.Layer = eBottomLayer then Component.Comment.Layer := TestPointBot;
           end;

           Component.EndModify;

           i := i +1;

           // Progress Meter - Small Performance Hit for User Feedback
           delta := GetTickCount - startTick;
           if delta > Throttle + 100 then  // fresh interval in Seconds
           begin
             Application.ProcessMessages; // Do Events
             lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Move To Layer';
             lblTime.Update;   // Update Form
             Throttle := delta;
           end;

           // Get the next component handle
           Component := ComponentIteratorHandle.NextPCBObject;

        end;  // Put Strings Back on the the Correct Layers

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        // End of Fixing Mirrored Designators and Comments

        // Main big loop

        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        S := '';

        i := 0;

        // Progress Meter - Small Performance Hit for User Feedback
        delta := GetTickCount - startTick;
        if delta > Throttle + 100 then  // fresh interval in Seconds
        begin
          lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Modify Strings';
          lblTime.Update;   // Update Form
          Throttle := delta;
        end;

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) AND (Board.SelectecObjectCount > 0) AND Not Escape Do

        begin  // *****

            if EscKeyDown then begin Escape := True; showmessage('Escaping Modify Strings'); end;

            MaxX:= 0;
            MinX:= 999999999;

            MaxY:= 0;
            MinY:= 999999999;

            // UnLock all strings (Altium Script Bug ? - This does NOT Work)
            // Workaround - Using the PCB Inspector
            // Toggle Component Strings Locked to the Off State
            // Last Resort Save PCBDOC > Close then Re-Open PCBDOC
            Component.LockStrings := False;

            begin
                 R := Component.BoundingRectangleNoNameComment;
                 if R.left < MinX then MinX := R.left;
                 if R.bottom < MinY then MinY := R.bottom;
                 if R.right > MaxX then MaxX := R.right;
                 if R.top > MaxY then MaxY := R.top;

                 Y:=MaxY-MinY;
                 X:=MaxX-MinX;
            end;

            Designator := Component.Name;

            // Find text length so choose equation for size calculation
            S := Designator.GetDesignatorDisplayString;
            TextLength := Length(S);

            // Set the size based on the bounding rectangle
            if Y >= X then
            begin
                 Size := CalculateSize(Y,S,TextLength);
                 if Size >= X then
                    Size := CalculateSize(X,S,TextLength);
            end
            else
            begin
                  Size := CalculateSize(X,S,TextLength);
                  if Size >= Y then
                     Size := CalculateSize(Y,S,TextLength);
            end;

            if ((Size = -1) AND (ShowOnce = False)) then
            begin
                 ShowMessage('To many characters in one or more components such as (' + Component.Name.Text + '). More than 7 characters are not supported and these components will be ommited.');
                 ShowOnce := True;
            end;

            // Trim designator if its size Exceeds Min/Max Height
            if Size >  MaximumHeight then
               Size := MaximumHeight;

            if Size <  MinimumHeight then
               Size := MinimumHeight;

            // Begin Selected Comments and Designators
            begin

                If (Designator.Selected = True) or (Component.Selected) then
                begin

                    // notify that the Components are going to be modified
                    Component.BeginModify;
                    // Hide Designators
                    if (cbxHideRefs.Checked) then
                    begin
                         Component.NameOn := False;
                    end;

                    // UnHide Designators
                    if (cbxUnHideRefs.Checked) then
                    begin
                         Component.NameOn := True;
                    end;

                    Component.EndModify;

                    if (Size > 0) AND (cbxSizeRefs.Checked ) then
                    begin

                         // notify that the pcb object is going to be modified
                         Designator.BeginModify;

                         // Set Designator Size
                         Designator.Size := Size;
                         // Setup the True Type Fonts
                         Designator.UseTTFonts := True;
                         Designator.Italic := False;
                         Designator.Bold := True;
                         Designator.FontName := 'Microsoft Sans Serif';
                         Designator.Inverted := False;
                         // Use Stroke Fonts
                         If (cbxUseStrokeFonts.Checked = True) then
                         begin
                              Designator.UseTTFonts := False;
                              Designator.Width := Size/6;
                              if Designator.Width < MinW * 10000 then Designator.Width := MinW * 10000;
                         end;

                         Designator.EndModify;

                    end;

                    // Rotate the designator to improve readability
                    if cbxRotateRefs.Checked then
                       if Y > X then
                       begin
                            if (Component.Layer = 1) then
                            begin
                                 Designator.Rotation := 90;
                            end;
                            if (Component.Layer = 32) then
                            begin
                                 Designator.Rotation := 270;
                            end;
                            end
                            else
                            begin
                                 Designator.Rotation := 360;
                            end;

                    // Set the autoposition to Center if Option Checked After Rotation
                    if cbxCenterRefs.Checked then
                    begin
                         Component.ChangeNameAutoposition  :=  eAutoPos_CenterCenter;
                    end;

                    // Rotate Selected Designators to Component Rotation
                    // Places the Designator at the Origin of the Component
                    if cbxRotateSelectedStrings.Checked then
                    begin
                         Component.ChangeNameAutoposition  := eAutoPos_Manual;
                         Designator.Rotation := Component.Rotation;
                         Component.Name.XLocation := Component.X;
                         Component.Name.YLocation := Component.Y;
                    end;

                    // Move Designators to Overlay Layers
                    if Component.Layer = eTopLayer then Component.Name.Layer := eTopOverlay
                    else                                Component.Name.Layer := eBottomOverlay;

                    // Set Designator Position Mode to Manual
                    // Stops Comments from Pushing Designators off Center
                    Component.ChangeNameAutoposition :=  eAutoPos_Manual;
                    Designator.EndModify;
                    Designator.GraphicallyInvalidate;

                end; // End of Selected Designators

                // Begin Selected Comments

                If (Component.Comment.Selected = True) or (Component.Selected) then
                begin
                    //ShowMessage('Found ' + Component.Name.Text);

                    begin

                        // notify that the Components are going to be modified
                        Component.BeginModify;

                        // Hide Comments
                        if (cbxHideCmts.Checked) then
                           begin
                           Component.CommentOn := False;
                        end;

                        // UnHide Comments
                        if (cbxUnHideCmts.Checked) then
                           begin
                           Component.CommentOn := True;
                        end;
                        Component.EndModify;

                        // Notify that the pcb object is going to be modified
                        Component.Comment.BeginModify;

                        // Set Comment to .designator
                        Component.Comment.Text := '.designator';

                        // Rotate Comment
                        if cbxRotateCmts.Checked then
                            //If(Component.Rotation = 90) or (Component.Rotation = 270)then
                            if Y > X then   // Use XY Calc
                            begin
                                if Component.Layer = 1 then Component.Comment.Rotation := 90;    // Top Layer Component
                                if Component.Layer = 32 then Component.Comment.Rotation := 270;  // Bottom Layer Component
                            end
                            else
                            begin
                                Component.Comment.Rotation := 360;
                            end;
                        end;

                        // Set Comment String Height and Font
                        if (Size > 0) AND (cbxSizeCmts.Checked) then
                        begin
                            Component.Comment.Size := Size; // Add Shrinkage Desired i.e. -10000 = 1mil
                            // Set Up True Type Fonts
                            Component.Comment.UseTTFonts := True;
                            Component.Comment.FontName := 'Microsoft Sans Serif';
                            Component.Comment.Italic := False;
                            Component.Comment.Bold := True;
                            Component.Comment.Inverted := False;
                            // Use Stroke Fonts
                            If (cbxUseStrokeFonts.Checked = True) then
                            begin
                                 Component.Comment.UseTTFonts := False;
                                 Component.Comment.Width := Size/6;
                                 if Component.Comment.Width < MinW * 10000 then Component.Comment.Width := MinW * 10000;
                            end;
                        end;

                        // Auto Center Comments After Size and Rotation Changes
                        if cbxCenterCmts.Checked then
                        begin
                            Component.ChangeCommentAutoposition := eAutoPos_CenterCenter;
                        end;

                        // Rotate Selected Comments to Component Rotation
                        if cbxRotateSelectedStrings.Checked then
                        begin
                             Component.ChangeCommentAutoposition := eAutoPos_CenterCenter;
                             Component.Comment.Rotation := Component.Rotation;
                        end;

                        // Move Comments to Correct Mechanical Layers
                        if Component.Layer = eTopLayer then Component.Comment.Layer := MechTop
                        else                                Component.Comment.Layer := MechBot;

                        //ShowMessage(Component.Name.Text);

                        // Move Test Points to User Defined Layers
                        if LeftStr(Component.Name.Text,2)= 'TP' then
                        begin
                             // ShowMessage(Component.Name.Text);
                             if Component.Layer = eTopLayer then Component.Comment.Layer := TestPointTop;
                             if Component.Layer = eBottomLayer then Component.Comment.Layer := TestPointBot;
                        end;

                        // Change Postion to Manual
                        Component.ChangeCommentAutoposition := eAutoPos_Manual;
                        Component.Comment.EndModify;
                        Component.Comment.GraphicallyInvalidate;

                end;  // End of Selected Comments

            end; // End of Only Selected Designators and Comments

            if Component.selected then
               begin
                  // Small Performance Hit for Cool Effects
                  Component.selected : = False;
                  // Progress Meter - Small Performance Hit for User Feedback
                  delta := GetTickCount - startTick;
                  if delta > Throttle + 100 then  // fresh interval in Seconds
                     begin
                        Application.ProcessMessages; // Do Events (look for Escape Key)
                        lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Modified ' + Component.Name.Text;
                        lblTime.Update;   // Update Form
                        Throttle := delta;
                     end;
               end;

            // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;

        end; // *****

        lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Post Processing ';
        lblTime.Update;   // Update Form

        BeginHourGlass;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        EndHourGlass;

        //Refresh the screen (Not Needed)
        //Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    finally
        // Restore DRC setting
        PCBSystemOptions.DoOnlineDRC :=  DRCSetting;
    end;

    //Display Total Time
    delta := GetTickCount - startTick;
    lblTime.Caption := 'Elapsed Time : ' + IntToStr(delta/1000) + ' Sec - Done!';
    lblTime.Update;   // Update Form

end;

function SelectAll(junk:string):boolean;

Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;

    pcbIterator             : IPCB_BoardIterator;
    pcbObject               : IPCB_Primitive;

begin

      Board := PCBServer.GetCurrentPCBBoard;
      if Board = nil then
      begin
           ShowMessage('Active Window is Not a .PcbDoc File');
           exit;
      end;

        // Put Strings Back on the the Correct Layers after Fixing Mirrored Strings
        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        BeginHourGlass;

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) Do

        begin  // *****
           Component.selected : = True;
           Component := ComponentIteratorHandle.NextPCBObject;
        end;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        EndHourGlass;
end;

function SelectTop(junk:string):boolean;

Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;

    pcbIterator             : IPCB_BoardIterator;
    pcbObject               : IPCB_Primitive;

begin

      Board := PCBServer.GetCurrentPCBBoard;
      if Board = nil then
      begin
           ShowMessage('Active Window is Not a .PcbDoc File');
           exit;
      end;

        // Put Strings Back on the the Correct Layers after Fixing Mirrored Strings
        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_LayerSet(MkSet(eTopLayer));
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        BeginHourGlass;

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) Do

        begin  // *****
           Component.selected : = True;
           Component := ComponentIteratorHandle.NextPCBObject;
        end;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        EndHourGlass;
end;

function SelectBottom(junk:string):boolean;

Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;

    pcbIterator             : IPCB_BoardIterator;
    pcbObject               : IPCB_Primitive;

begin

      Board := PCBServer.GetCurrentPCBBoard;
      if Board = nil then
      begin
           ShowMessage('Active Window is Not a .PcbDoc File');
           exit;
      end;

        // Put Strings Back on the the Correct Layers after Fixing Mirrored Strings
        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_LayerSet(MkSet(eBottomLayer));
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        BeginHourGlass;

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) Do

        begin  // *****
           Component.selected : = True;
           Component := ComponentIteratorHandle.NextPCBObject;
        end;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        EndHourGlass;
end;

Procedure Modify_Designators;
Var
rtn       : boolean;
junk      : string;

begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then
   begin
        ShowMessage('Active Window is Not a .PcbDoc File');
        exit;
   end;

   // Convert Special Strings
   AddStringParameter ('ConvertSpecialStrings', 'True');
   RunProcess ('Pcb:SetupPreferences');
   ResetParameters;
   AddStringParameter('Action','Redraw');
   RunProcess('PCB:Zoom');

   // Use Show Mode to allow user to prewiew layers
   // Use Show Modal to Close after Execution
   FormAdjustDesignators.Show;

   rtn := Read_ini(junk);

   // Get Units
   if RadioButtonMM.Checked then
   begin // Metric
        Units := 'MM';
   end
   else
   begin // English
         Units := 'mils';
   end;

end;


function NudgeUp(junk:string):boolean;

var
rtn       : boolean;

begin
     EditMinHeight.Text := FormatFloat('0',StrToFloat(EditMinHeight.Text) + 1);
     EditMaxHeight.Text := EditMinHeight.Text;
     rtn := ModifyStrings(junk);
end;


function NudgeDown(junk:string):boolean;

var
rtn       : boolean;

begin
  if StrToFloat(EditMinHeight.Text) > 10 then
     EditMinHeight.Text := FormatFloat('0',StrToFloat(EditMinHeight.Text) - 1);
     EditMaxHeight.Text := EditMinHeight.Text;
     rtn := ModifyStrings(junk);
end;

procedure TFormAdjustDesignators.cbxAllCmtsClick(Sender: TObject);
begin

     if cbxAllCmts.Checked then
     begin
          cbxUnHideCmts.Checked := True;
          cbxSizeCmts.Checked := True;
          cbxRotateCmts.Checked := True;
          cbxCenterCmts.Checked := True;
     end
     else
     begin
          cbxHideCmts.Checked := False;
          cbxUnHideCmts.Checked := False;
          cbxSizeCmts.Checked := False;
          cbxRotateCmts.Checked := False;
          cbxCenterCmts.Checked := False;
     end;

end;

procedure TFormAdjustDesignators.cbxAllRefsClick(Sender: TObject);
begin

     if cbxAllRefs.Checked then
     begin
          cbxUnHideRefs.Checked := True;
          cbxSizeRefs.Checked := True;
          cbxRotateRefs.Checked := True;
          cbxCenterRefs.Checked := True;
     end
     else
     begin
          cbxHideRefs.Checked := False;
          cbxUnHideRefs.Checked := False;
          cbxSizeRefs.Checked := False;
          cbxRotateRefs.Checked := False;
          cbxCenterRefs.Checked := False;
     end;

end;

procedure TFormAdjustDesignators.btnApplyClick(Sender: TObject);
var
junk : string;
rtn  : boolean;

begin
     // Run Script and Leave Dialog Open
     rtn : = VerifyUserInputs(junk);
     if rtn <> False then
     begin
        rtn := ModifyStrings(junk);
     end;
     if Escape then Close;
end;

procedure TFormAdjustDesignators.btnSaveClick(Sender: TObject);
var
junk : string;
rtn  : boolean;

begin
     // Save Users Layer Choices
     rtn := Write_ini(junk);
     showinfo('Saved Min/Max Height and Layers');
end;

procedure TFormAdjustDesignators.RadioButtonMMClick(Sender: TObject);
begin
    // Convert mils to MM, round off to 3 decimals
    if Units = 'mils' then
    begin
         If (IsStringANum(EditMinHeight.Text)) then
         begin
              EditMinHeight.Text := FormatFloat('0.####',StrToFloat(EditMinHeight.Text) * 25.4/1000);
         end;
         If (IsStringANum(EditMaxHeight.Text)) then
         begin
              EditMaxHeight.Text := FormatFloat('0.####',StrToFloat(EditMaxHeight.Text) * 25.4/1000);
         end;
         Units:= 'MM';
    end;
end;

procedure TFormAdjustDesignators.RadioButtonMilClick(Sender: TObject);
begin
    // Convert MM to mils, round off to 3 decimals
    if Units = 'MM' then
    begin
         If (IsStringANum(EditMinHeight.Text)) then
         begin
              EditMinHeight.Text := FormatFloat('0',StrToFloat(EditMinHeight.Text) * 1000/25.4);
         end;
         If (IsStringANum(EditMaxHeight.Text)) then
         begin
              EditMaxHeight.Text := FormatFloat('0',StrToFloat(EditMaxHeight.Text) * 1000/25.4);
         end;
    end;
    Units:= 'mils';
end;

procedure TFormAdjustDesignators.btnSelectAllClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
     // Select All Components
     rtn := SelectAll(junk);
end;

procedure TFormAdjustDesignators.btnSelectTopClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
     // Select Top Components
     rtn := SelectTop(junk);
end;

procedure TFormAdjustDesignators.btnSelectBotClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
     // Select Bottom Components
     rtn := SelectBottom(junk);
end;

procedure TFormAdjustDesignators.EditCommentsTopChange(Sender: TObject);
begin
     EditTestPointsTop.Text := EditCommentsTop.Text;
end;


procedure TFormAdjustDesignators.EditCommentsBotChange(Sender: TObject);
begin
     EditTestPointsBot.Text := EditCommentsBot.Text;
end;


procedure TFormAdjustDesignators.btnUpClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
     EditMinHeight.Text := FormatFloat('0',StrToFloat(EditMinHeight.Text) + 1);
     EditMaxHeight.Text := EditMinHeight.Text;
     //rtn := NudgeUp(junk);
end;

procedure TFormAdjustDesignators.btnDownClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
  if StrToFloat(EditMinHeight.Text) > 10 then
     EditMinHeight.Text := FormatFloat('0',StrToFloat(EditMinHeight.Text) - 1);
     EditMaxHeight.Text := EditMinHeight.Text;
     //rtn := NudgeDown(junk);
end;

procedure TFormAdjustDesignators.btnLoadClick(Sender: TObject);

var
junk : string;
rtn  : boolean;

begin
  rtn := Read_ini(junk);
end;

