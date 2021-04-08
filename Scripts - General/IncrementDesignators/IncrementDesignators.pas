{..............................................................................}
{  This script was written for my own use, so no liability taken!              }
{  ----------------------------------------------------------------------------}
{  This script could be useful to modify designators of components (in SCH or  }
{  PCB), Pins (In SCHLIB) or Pads (ib PCBLIB). Component Prefix letters are    }
{  not changed.                                                                }
{                                                                              }
{  Script has two modes of working:                                            }
{                                                                              }
{  - Incrementing: This mode is used if "Swap Designators" is unchecked.       }
{                  It assigns "Next Index Number" to clicked primitive. In     }
{                  this mode "Increment Number" is also used ad increment step.}
{                                                                              }
{  - Swapping:     This mode is used if "Swap Designators" is unchecked.       }
{                  User needs to select two primitives and their designators   }
{                  will be swapped. Whe in SCHLIB, usre can shoose to move pin }
{                  names too (if "Move Pin Name" is checked)                   }
{                                                                              }
{  Created by: Petar Perisin                                                   }
{..............................................................................}

// Global Variables:
Var
   SchDoc   : ISch_Document;
   PCBBoard : IPCB_Board;
   DocKind  : String;
   UsedDes : TStringList;
{..............................................................................}


function IncrDesignator (Designator);
// This function keeps prefix letter(s) and combines them with the value of
// global incrementation counter "intCount" and returns new designator.
// LIMITATION: for the moment works with prefixes up to two letters only,
// that will be fixed later on.
Var
   AsciiCode   : Integer;
   NrNew       : String;
   intCount    : Integer;
   TextLength  : Integer;
   DesText     : String;

Begin
    TextLength := 1;
    AsciiCode := ord(Designator[1]);
    while AsciiCode > 64 do
    begin
       Inc(TextLength);
       AsciiCode := ord(Designator[TextLength]);
    end;
    Dec(TextLength);
    DesText := Designator;
    SetLength(DesText, TextLength);

    if (Dockind = 'SCH') and CheckBoxPinName.Checked then
    begin
       UsedDes.Delete(UsedDes.IndexOf(Designator));

       IntCount := 1;

       While (UsedDes.IndexOf(DesText + IntToStr(IntCount)) >= 0) do Inc(IntCount);

       UsedDes.Add(DesText + IntToStr(IntCount));

       Result := DesText + IntToStr(IntCount);
    end
    else
    begin
       NrNew := Edit_inc.Text;
       intcount := StrToInt(NrNew);
       NrNew := Edit_Nr.Text;
       IntCount := IntCount + StrToInt(NrNew);
       Edit_Nr.Text := IntToStr(IntCount);

       Result := DesText + NrNew;
    end;
End;
{..............................................................................}


function IncrSpatialIterator(Dummy:String);
// This function was taken from the examples and modified for my convenience.
//
Var
   boolLoc         : boolean;

   // SCH objects
   SpatialIterator : ISch_Iterator;
   Component       : ISch_Component;
   OldComponent    : ISch_Component;
   Pin             : ISch_Pin;
   PinClicked      : ISch_Pin;
   ALoc            : TLocation;


   // PCB Objects
   PCBIterator     : IPCB_BoardIterator;
   PCBComp         : IPCB_Component;
   PCBComp2        : IPCB_Component;
   Pad1            : IPCB_Pad2;
   Pad2            : IPCB_Pad2;

   // Global
   DesignatorOld   : String;
   DesignatorNew   : String;
   NameOld         : String;
   NameNew         : String;
   Tekst           : String;
   TextLength      : Integer;
   AsciiCode       : Integer;

Begin
    boolLoc := 0;

    DesignatorOld := '';     // initialising as Strings
    DesignatorNew := '';

    if DocKind = 'SCH' then
    begin
       // Using the ChooseRectangleInteractively method to capture the
       // Boundary coordinates clicked on the sheet by the user.
       ALoc := TLocation;

       if CheckBoxSwap.Checked then Tekst := 'Choose First Component'
       else                         Tekst := 'Choose Component';

       boolLoc := SchDoc.ChooseLocationInteractively(ALoc, Tekst);

       If Not boolLoc Then Exit;

       // Initialising Robot:
       SchServer.ProcessControl.PreProcess(SchDoc, '');

       // Create a spatial iterator with the boundary coordinates
       // Spatial iterator looks for Junctions and Components only.


       SpatialIterator := SchDoc.SchIterator_Create;
       If SpatialIterator = Nil Then Exit;
       Try
           SpatialIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
           SpatialIterator.AddFilter_Area(ALoc.X, ALoc.Y, ALoc.X+1, ALoc.Y+1);

           Component := SpatialIterator.FirstSchObject;
           While Component <> Nil Do
           Begin
               if (CheckBoxSwap.Checked = False) then
               begin
                   DesignatorOld := Component.Designator.Text;   // read old designator
                   DesignatorNew := IncrDesignator(DesignatorOld);  // compose new designator

                   SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       Component.Designator.Text := DesignatorNew;    // write new designator to the component
                   SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);
               end
               else
               begin
                  // This is when swap designators is done
                  OldComponent := Component;
               end;
               Component := SpatialIterator.NextSchObject;
           End;

       Finally
           SchDoc.SchIterator_Destroy(SpatialIterator);
       end;

       if CheckBoxSwap.Checked then
       begin
           // This is where we swap designators
           SpatialIterator := SchDoc.SchIterator_Create;

           // Getting second component
           boolLoc := SchDoc.ChooseLocationInteractively(ALoc,'Choose Second Component');
           If Not boolLoc Then Exit;

           If SpatialIterator = Nil Then Exit;
           Try
               SpatialIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
               SpatialIterator.AddFilter_Area(ALoc.X, ALoc.Y, ALoc.X+1, ALoc.Y+1);

               Component := SpatialIterator.FirstSchObject;
               While Component <> Nil Do
               Begin
                   NameOld := OldComponent.Designator.Text;   // read first designator
                   DesignatorOld := NameOld;
                   TextLength := 1;
                   AsciiCode := ord(NameOld[1]);
                   while AsciiCode > 64 do
                   begin
                      Inc(TextLength);
                      AsciiCode := ord(NameOld[TextLength]);
                   end;
                   Dec(TextLength);
                   SetLength(NameOld, TextLength);
                   Delete(DesignatorOld, 1, TextLength);

                   NameNew := Component.Designator.Text;  // read second designator
                   DesignatorNew := NameNew;
                   TextLength := 1;
                   AsciiCode := ord(NameNew[1]);
                   while AsciiCode > 64 do
                   begin
                      Inc(TextLength);
                      AsciiCode := ord(NameNew[TextLength]);
                   end;
                   Dec(TextLength);
                   SetLength(NameNew, TextLength);
                   Delete(DesignatorNew, 1, TextLength);


                   SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       Component.Designator.Text := NameNew + DesignatorOld;    // write new designator to the component
                   SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                   SchServer.RobotManager.SendMessage(OldComponent.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       OldComponent.Designator.Text := NameOld + DesignatorNew;    // write new designator to the component
                   SchServer.RobotManager.SendMessage(OldComponent.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                   Component := SpatialIterator.NextSchObject;
               End;

           Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
           end;
       end;

       // Post Process:
       SchServer.ProcessControl.PostProcess(SchDoc, '');

    end
    else if DocKind = 'SCHLIB' Then
    begin
       // Initialising Robot:

       if CheckBoxSwap.Checked then Tekst := 'Choose First Pin'
       else                         Tekst := 'Choose Pin';

       ALoc := TLocation;
       boolLoc := SchDoc.ChooseLocationInteractively(ALoc, Tekst);
       If Not boolLoc Then Exit;

       SchServer.ProcessControl.PreProcess(SchDoc, '');

       // Create a spatial iterator with the boundary coordinates
       // Spatial iterator looks for Junctions and Components only.
       SpatialIterator := SchDoc.SchIterator_Create;
       If SpatialIterator = Nil Then Exit;
       Try
           SpatialIterator.AddFilter_ObjectSet(MkSet(ePin));
           SpatialIterator.AddFilter_CurrentPartPrimitives;
           SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
           SpatialIterator.AddFilter_Area(ALoc.X, ALoc.Y, ALoc.X+1, ALoc.Y+1);

           Pin := SpatialIterator.FirstSchObject;
           While Pin <> Nil Do
           Begin
               if CheckBoxSwap.Checked = False then
               begin
                   DesignatorOld := Pin.Designator;   // read old designator
                   NameOld       := Pin.Name;
                   DesignatorNew := IncrDesignator(DesignatorOld);  // compose new designator
                   if (CheckBoxPinName.Checked) and (DesignatorOld <> DesignatorNew) then
                      NameNew := ''
                   else
                      NameNew := NameOld;

                   SchServer.RobotManager.SendMessage(Pin.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       Pin.Designator := DesignatorNew;    // write new designator to the pin
                       Pin.Name := NameNew;
                   SchServer.RobotManager.SendMessage(Pin.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);
               end;

               PinClicked := Pin;

               Pin := SpatialIterator.NextSchObject;
           End;

       Finally
           SchDoc.SchIterator_Destroy(SpatialIterator);
       end;


       if CheckBoxSwap.Checked = False then
       begin
           // We need one more iterator here if we want to replace pins
           SpatialIterator := SchDoc.SchIterator_Create;
           If SpatialIterator = Nil Then Exit;
           Try
               SpatialIterator.AddFilter_ObjectSet(MkSet(ePin));
               SpatialIterator.AddFilter_CurrentPartPrimitives;
               SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
               // SpatialIterator.AddFilter_Area(ALoc.X, ALoc.Y, ALoc.X+1, ALoc.Y+1);

               Pin := SpatialIterator.FirstSchObject;
               While Pin <> Nil Do
               Begin
                  if (Pin.Designator = DesignatorNew) and (CheckBoxPinName.Checked) and (Pin <> PinClicked) then
                  begin

                     SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                        PinClicked.Name := Pin.Name;
                     SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                     SchServer.RobotManager.SendMessage(Pin.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                        Pin.Designator := DesignatorOld;
                        Pin.Name       := NameOld;
                     SchServer.RobotManager.SendMessage(Pin.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                  end;
                  Pin := SpatialIterator.NextSchObject;
               end;

           Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
           end;

       end
       else
       begin
           // This is where we swap pins
           // This is where we swap designators
           SpatialIterator := SchDoc.SchIterator_Create;

           // Getting second component
           boolLoc := SchDoc.ChooseLocationInteractively(ALoc,'Choose Second Pin');
           If Not boolLoc Then Exit;

           If SpatialIterator = Nil Then Exit;
           Try
               SpatialIterator.AddFilter_ObjectSet(MkSet(ePin));
               SpatialIterator.AddFilter_CurrentPartPrimitives;
               SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
               SpatialIterator.AddFilter_Area(ALoc.X, ALoc.Y, ALoc.X+1, ALoc.Y+1);

               Pin := SpatialIterator.FirstSchObject;
               While Pin <> Nil Do
               Begin
                   DesignatorOld := PinClicked.Designator;   // old designator
                   DesignatorNew := Pin.Designator;          // new designator
                   NameOld       := PinClicked.Name;         // Old Name
                   NameNew       := Pin.Name;                // Old Name

                   SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       PinClicked.Designator := DesignatorNew;                       // write new designator
                       if  CheckBoxPinName.Checked then PinClicked.Name := NameNew;  // write new name
                   SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                   SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                       Pin.Designator := DesignatorOld;                              // write new designator
                       if  CheckBoxPinName.Checked then Pin.Name := NameOld;         // write new name
                   SchServer.RobotManager.SendMessage(PinClicked.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

                   Pin := SpatialIterator.NextSchObject;
               End;

           Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
           end;

       end;

       // Post Process:
       SchServer.ProcessControl.PostProcess(SchDoc, '');

       // Refresh:
       SchDoc.GraphicallyInvalidate;
    end
    else if DocKind = 'PCB' then
    begin
        PCBServer.PreProcess;


        if CheckBoxSwap.Checked then Tekst := 'Choose First Component'
        else                         Tekst := 'Choose Component';

        PCBComp := nil;
        PcbComp2 := nil;
        PCBComp := PCBBoard.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, Tekst);


        if (CheckBoxSwap.Checked and Assigned(PCBComp))  then
           PCBComp2 := PCBBoard.GetObjectAtCursor(MkSet(eComponentObject),AllLayers,'Choose Second Component');


        if (Assigned(PCBComp) and (not (CheckBoxSwap.Checked))) then
        begin
           boolLoc := True;
           DesignatorOld := PCBComp.Name.Text;
           DesignatorNew := IncrDesignator(DesignatorOld);

           PCBServer.SendMessageToRobots(PCBComp.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           PCBComp.Name.Text := DesignatorNew;
           PCBServer.SendMessageToRobots(PCBComp.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, PCBComp.I_ObjectAddress);

        end
        else if (Assigned(PCBComp) and Assigned(PCBComp2) and (CheckBoxSwap.Checked)) then
        begin
           boolLoc := True;

           NameOld := PCBComp.Name.Text;   // read first designator
           DesignatorOld := NameOld;
           TextLength := 1;
           AsciiCode := ord(NameOld[1]);
           while AsciiCode > 64 do
           begin
              Inc(TextLength);
              AsciiCode := ord(NameOld[TextLength]);
           end;
           Dec(TextLength);
           SetLength(NameOld, TextLength);
           Delete(DesignatorOld, 1, TextLength);

           NameNew := PCBComp2.Name.Text;  // read second designator
           DesignatorNew := NameNew;
           TextLength := 1;
           AsciiCode := ord(NameNew[1]);
           while AsciiCode > 64 do
           begin
              Inc(TextLength);
              AsciiCode := ord(NameNew[TextLength]);
           end;
           Dec(TextLength);
           SetLength(NameNew, TextLength);
           Delete(DesignatorNew, 1, TextLength);

           PCBServer.SendMessageToRobots(PCBComp.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           PCBComp.Name.Text := NameOld + DesignatorNew;
           PCBServer.SendMessageToRobots(PCBComp.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, PCBComp.I_ObjectAddress);

           PCBServer.SendMessageToRobots(PCBComp2.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           PCBComp2.Name.Text := NameNew + DesignatorOld;
           PCBServer.SendMessageToRobots(PCBComp2.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, PCBComp2.I_ObjectAddress);
        end
        else
           boolLoc := False;

        PCBServer.PostProcess;

        ResetParameters;
        AddStringParameter('Action','Redraw');
        RunProcess('PCB:Zoom');
    end
    else if DocKind = 'PCBLIB' then
    begin
        PCBServer.PreProcess;

        if CheckBoxSwap.Checked then Tekst := 'Choose First Pad'
        else                         Tekst := 'Choose Pad';


        Pad1 := PCBBoard.GetObjectAtCursor(MkSet(ePadObject),AllLayers, Tekst);
        if (CheckBoxSwap.Checked and Assigned(Pad1)) then
           Pad2 := PCBBoard.GetObjectAtCursor(MkSet(ePadObject),AllLayers,'Choose Second Pad');

        if (Assigned(Pad1) and (not (CheckBoxSwap.Checked))) then
        begin
           boolLoc := True;
           DesignatorOld := Pad1.Name;
           DesignatorNew := IncrDesignator(DesignatorOld);

           PCBServer.SendMessageToRobots(Pad1.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           Pad1.Name := DesignatorNew;
           PCBServer.SendMessageToRobots(Pad1.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Pad1.I_ObjectAddress);

        end
        else if (Assigned(Pad1) and Assigned(Pad2) and (CheckBoxSwap.Checked)) then
        begin
           boolLoc := True;
           DesignatorOld := Pad1.Name;
           DesignatorNew := Pad2.Name;

           PCBServer.SendMessageToRobots(Pad1.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           Pad1.Name := DesignatorNew;
           PCBServer.SendMessageToRobots(Pad1.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Pad1.I_ObjectAddress);

           PCBServer.SendMessageToRobots(Pad2.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
           Pad2.Name := DesignatorOld;
           PCBServer.SendMessageToRobots(Pad2.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
           PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Pad2.I_ObjectAddress);
        end
        else
           boolLoc := False;

        PCBServer.PostProcess;

        ResetParameters;
        AddStringParameter('Action','Redraw');
        RunProcess('PCB:Zoom');
    end;

    Result := boolLoc;   // False if choosing rectangle was aborted (e.g. via right mouse button)

End;
{..............................................................................}


function Start_Incr(Dummy:String);
// This function is called via the "Start" button.
// It contains a very little bit of error handling, but this does not go very far yet.
// The function containing the spatial iterator is called within the loop. When it
// returns False, the loop will be aborted.
Var
   boolGoOn : Boolean;
   Proj     : IProject;
   Doc      : IDocument;
   i        : Integer;
Begin
   If Edit_Nr.GetTextLen = 0 Then    // Error if empty Edit box
   Begin
        ShowInfo('Please enter starting number.');
        Edit_Nr.SetFocus;
        Exit;
   End;

   If (DocKind = 'SCH') and (CheckBoxPinName.Checked) Then
   begin
      CheckBoxPinName.Caption := 'Assign next free Designator';

      UsedDes := TStringList.Create;

      if GetWorkspace.DM_FocusedProject.DM_ProjectFileName = 'Free Documents' then
      begin
         Doc := GetWorkspace.DM_FocusedDocument;
         Doc.DM_Compile;

         for i := 0 to Doc.DM_ComponentCount - 1 do
            UsedDes.Add(Doc.DM_Components(i).DM_LogicalDesignator);

         UsedDes.Sort;
      end
      else
      begin
         GetWorkspace.DM_FocusedProject.DM_Compile;
         Doc := GetWorkspace.DM_FocusedProject.DM_DocumentFlattened;

         for i := 0 to Doc.DM_ComponentCount - 1 do
            UsedDes.Add(Doc.DM_Components(i).DM_PhysicalDesignator);

         UsedDes.Sort;
      end;
   end;

   // Form1.Hide;       // I get access violations with this - don't know why - but it would be nice to have this
   boolGoOn := True;      // intialising boolean value
   While boolGoOn do
      boolGoOn := IncrSpatialIterator('');     //boolean value for remebering escape via right-click

   Form1.Show;
   Form1.SetFocus;        // Form ist brought up front again

   //  if not CheckBoxSwap.Checked then
   //  begin
   //     Edit_Nr.SetFocus;
   //     Edit_Nr.SelectAll;
   //  end;
End;
{..............................................................................}


procedure TForm1.Button1Click(Sender: TObject);
Begin
   Start_Incr('');
End;
{..............................................................................}


procedure TForm1.Button2Click(Sender: TObject);
Begin
     Close;
End;
{..............................................................................}


procedure TForm1.Form1Create(Sender: TObject);
begin
   // Get the type of document
   DocKind := GetWorkspace.DM_FocusedDocument.DM_DocumentKind;

   If ((DocKind = 'SCH') or (DocKind = 'SCHLIB')) Then
   begin
      SchDoc := SchServer.GetCurrentSchDocument;
      If SchDoc = Nil Then Exit;

      CheckBoxPinName.Visible := True;
      CheckBoxPinName.Enabled := True;

      If DocKind = 'SCH' Then
      begin
         CheckBoxPinName.Caption := 'Assign next free Designator';

         Edit_Nr.Enabled := False;
         Edit_inc.Enabled := False;
      end;
   end
   else if ((DocKind = 'PCB') or (DocKind = 'PCBLIB')) then
   begin
      PCBBoard := PCBServer.GetCurrentPCBBoard;
      If PCBBoard = Nil Then Exit;

   end
   else exit;


   Start_Incr('');
end;


procedure TForm1.CheckBoxSwapClick(Sender: TObject);
begin
   if CheckboxSwap.Checked or (CheckBoxPinName.Checked and CheckBoxPinName.Visible)  then
   begin
      Edit_inc.Enabled := False;
      Edit_Nr.Enabled  := False;
   end
   else
   begin
      Edit_inc.Enabled := True;
      Edit_Nr.Enabled  := True;
      Edit_Nr.SetFocus;
      Edit_Nr.SelectAll;
   end;

   if (DocKind = 'SCH') and CheckBoxPinName.Checked and CheckBoxSwap.Checked then
         CheckBoxPinName.Checked := False;
end;



procedure TForm1.CheckBoxPinNameClick(Sender: TObject);
begin
   if CheckboxSwap.Checked or CheckBoxPinName.Checked then
   begin
      Edit_inc.Enabled := False;
      Edit_Nr.Enabled  := False;
   end
   else
   begin
      Edit_inc.Enabled := True;
      Edit_Nr.Enabled  := True;
      Edit_Nr.SetFocus;
      Edit_Nr.SelectAll;
   end;

   if (DocKind = 'SCH') and CheckBoxPinName.Checked and CheckBoxSwap.Checked then
         CheckBoxSwap.Checked := False;
end;

