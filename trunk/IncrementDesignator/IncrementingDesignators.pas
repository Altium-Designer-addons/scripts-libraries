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
CurrentDoc : ISch_Document;
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
SpatialIterator : ISch_Iterator;
Component       : ISch_Component;
Pin             : ISch_Pin;
PinClicked      : ISch_Pin;
DesignatorOld   : String;
DesignatorNew   : String;
NameOld         : String;
NameNew         : String;
ALoc            : TLocation;;
boolLoc         : boolean;


Begin

    DesignatorOld := '';     // initialising as Strings
    DesignatorNew := '';

    // Using the ChooseRectangleInteractively method to capture the
    // Boundary coordinates clicked on the sheet by the user.
    ALoc := TLocation;
    boolLoc := CurrentDoc.ChooseLocationInteractively(ALoc,'Choose a location to click');

    If Not boolLoc Then Exit;

    If CurrentDoc.ObjectID <> eSchLib Then
    begin
       // Initialising Robot:
       SchServer.ProcessControl.PreProcess(CurrentDoc, '');

       // Create a spatial iterator with the boundary coordinates
       // Spatial iterator looks for Junctions and Components only.
       SpatialIterator := CurrentDoc.SchIterator_Create;
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
           CurrentDoc.SchIterator_Destroy(SpatialIterator);
       end;


       // Post Process:
       SchServer.ProcessControl.PostProcess(CurrentDoc, '');

    end
    else if CurrentDoc.ObjectID = eSchLib Then
    begin
       // Initialising Robot:
       SchServer.ProcessControl.PreProcess(CurrentDoc, '');

       // Create a spatial iterator with the boundary coordinates
       // Spatial iterator looks for Junctions and Components only.
       SpatialIterator := CurrentDoc.SchIterator_Create;
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
           CurrentDoc.SchIterator_Destroy(SpatialIterator);
       end;

       SpatialIterator := CurrentDoc.SchIterator_Create;
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
           CurrentDoc.SchIterator_Destroy(SpatialIterator);
       end;

       // Post Process:
       SchServer.ProcessControl.PostProcess(CurrentDoc, '');

    end;

    // Refresh:
    CurrentDoc.GraphicallyInvalidate;

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

   If SchServer = Nil Then Exit;
   CurrentDoc := SchServer.GetCurrentSchDocument;
   If CurrentDoc = Nil Then Exit;

   If CurrentDoc.ObjectID = eSchLib Then
        CheckBoxPinName.Visible := True;

   // Form1.Visible := False;

   Start_Incr;
end;
