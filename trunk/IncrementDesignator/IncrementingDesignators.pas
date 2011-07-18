{..............................................................................}
// This script was written for my own use, so no liability taken!
// ----------------------------------------------------------------------------
// This script could be useful to increment designators of components
// within a schematic in your own free order. Prefix letters (e.g. R for
// resistor) will survive.
// It uses a spatial iterator, that is why it is necessary to click twice
// (or as an alternative, triple-click directly on a component).
//
// Short instruction:
// * Execute the script
// * Type the starting number into the edit box
// * Hit "Enter" key or press "Start" button
// * Crosshairs will appear
// * Click twice to span a rectangle (containing one or more components)
// * All components within the rectangle will be incremented (positional order
//       is not taken into account, unfortunately! Alpha Prefix does not matter either.)
// * Click again twice, for next component(s)
//       (incrementation step still being remembered, it just goes on from the last value)
// * If you want to stop, or choose a new starting number: escape from the
//       crosshairs via right mouse button
// * Finally press "Close" button to close form window
//
// LIMITATIONS:
// - For the moment works with prefixes up to two letters only
// - Only very little error handling implemented!
// - I did not have much time, so did not work on optimisations
{..............................................................................}
{..............................................................................}


{..............................................................................}
// Global Variables:
Var
SchDoc   : ISch_Document;
PCBBoard : IPCB_Board;
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

    NrNew := Edit_Nr.Text;
    intcount := StrToInt(NrNew);
    Inc(IntCount);
    Edit_Nr.Text := IntToStr(IntCount);

    Result := DesText + NrNew;
End;
{..............................................................................}


function IncrSpatialIterator;
// This function was taken from the examples and modified for my convenience.
//
Var
   boolLoc         : boolean;

   // SCH objects
   SpatialIterator : ISch_Iterator;
   Component       : ISch_Component;
   Pin             : ISch_Pin;
   PinClicked      : ISch_Pin;
   DesignatorOld   : String;
   DesignatorNew   : String;
   NameOld         : String;
   NameNew         : String;
   ALoc            : TLocation;;


   // PCB Objects
   PCBIterator     : IPCB_BoardIterator;
   PCBComp         : IPCB_Component;
   x,y             : TCoord;

Begin
    boolLoc := 0;

    DesignatorOld := '';     // initialising as Strings
    DesignatorNew := '';

    if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCH' then
    begin
       // Using the ChooseRectangleInteractively method to capture the
       // Boundary coordinates clicked on the sheet by the user.
       ALoc := TLocation;
       boolLoc := SchDoc.ChooseLocationInteractively(ALoc,'Choose a location to click');

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
           //SpatialIterator.addfilt

           Component := SpatialIterator.FirstSchObject;
           While Component <> Nil Do
           Begin
               DesignatorOld := Component.Designator.Text;   // read old designator
               DesignatorNew := IncrDesignator(DesignatorOld);  // compose new designator

               SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                   Component.Designator.Text := DesignatorNew;    // write new designator to the component
               SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

               Component := SpatialIterator.NextSchObject;
           End;

       Finally
           SchDoc.SchIterator_Destroy(SpatialIterator);
       end;


       // Post Process:
       SchServer.ProcessControl.PostProcess(SchDoc, '');

    end
    else if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCHLIB' Then
    begin
       // Initialising Robot:
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
               PinClicked := Pin;
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



               Pin := SpatialIterator.NextSchObject;
           End;

       Finally
           SchDoc.SchIterator_Destroy(SpatialIterator);
       end;

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

       // Post Process:
       SchServer.ProcessControl.PostProcess(SchDoc, '');

       // Refresh:
       SchDoc.GraphicallyInvalidate;
    end
    else if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'PCB' then
    begin

        PCBComp := PCBBoard.GetObjectAtCursor(MkSet(eComponentObject),AllLayers,'Choose Component');                          
        if Assigned(PCBComp) then
        begin
           boolLoc := True;
           DesignatorOld := PCBComp.Name.Text;
           DesignatorNew := IncrDesignator(DesignatorOld);

           PCBComp.Name.Text  := DesignatorNew;

           ResetParameters;
           AddStringParameter('Action','Redraw');
           RunProcess('PCB:Zoom');
        end
        else boolLoc := False;
    end;

    Result := boolLoc;   // False if choosing rectangle was aborted (e.g. via right mouse button)

End;
{..............................................................................}


function Start_Incr;
// This function is called via the "Start" button.
// It contains a very little bit of error handling, but this does not go very far yet.
// The function containing the spatial iterator is called within the loop. When it
// returns False, the loop will be aborted.
Var
boolGoOn;

Begin
     If Edit_Nr.GetTextLen = 0 Then    // Error if empty Edit box
     Begin
          ShowInfo('Please enter starting number.');
          Edit_Nr.SetFocus;
          Exit;
     End;

     boolGoOn := True;      // intialising boolean value
     While boolGoOn = True Do
     Begin
           boolGoOn := IncrSpatialIterator;     //boolean value for remebering escape via right-click
     End;

     Form1.Visible := True;
     Form1.SetFocus;        // Form ist brought up front again
     Edit_Nr.SetFocus;
     Edit_Nr.SelectAll;
End;
{..............................................................................}


procedure TForm1.Button1Click(Sender: TObject);
Begin
//   Form1.Visible := False;
   Start_Incr;
End;
{..............................................................................}


procedure TForm1.Button2Click(Sender: TObject);
Begin
     Close;
End;
{..............................................................................}


procedure TForm1.Form1Create(Sender: TObject);
begin

   If SchServer <> Nil Then
   begin
      SchDoc := SchServer.GetCurrentSchDocument;
      If SchDoc = Nil Then Exit;

      If SchDoc.ObjectID = eSchLib Then
         CheckBoxPinName.Visible := True;
   end
   else if PCBServer <> nil then
   begin
      PCBBoard := PCBServer.GetCurrentPCBBoard;
      If PCBBoard = Nil Then Exit;

   end
   else exit;

   // Form1.Visible := False;

   Start_Incr;
end;
