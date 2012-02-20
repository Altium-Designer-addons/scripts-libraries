{..............................................................................}
{ Summary   This scripts can be used to adjust designators on mech layers or   }
{           on slikscreen.                                                     }
{                                                                              }
{                                                                              }
{ Created by:     Mattias Ericson                                              }
{ Reviewed by:    Petar Perisin                                                }
{..............................................................................}


var
   Board       : IPCB_Board;
   MechPairs   : TStringList;
   MechSingles : TStringList;


function GetFirstLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   if Pos <> 0 then
      SetLength(Pair, Pos - 1);

   Result := Pair;
end;


function GetSecondLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   Delete(Pair, 1, Pos  + 7);

   Result := Pair;
end;


// Function that checks is string a float number or not
function IsStringANum(Tekst : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Tekst) do
      if not(((ord(Tekst[i]) > 47) and (ord(Tekst[i]) < 58)) or (ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Tekst) do
      if ((ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Inc(dotCount);

   if dotCount > 1 then Result := False;
end;


//Calculate the hight of the true type text to best fit for Microsoft Sans Serif
function CalculateSize (Size:Integer,S:String,TextLength:Integer):Integer;
begin
     case TextLength of
          1 : Result := MMsToCoord(1.3013*CoordToMMs(Size)-0.0597);
          2 : Result := MMsToCoord(0.7201*CoordToMMs(Size)+0.0612);
          3 : Result := MMsToCoord(0.4319*CoordToMMs(Size)+0.1116);
          4 : Result := MMsToCoord(0.3265*CoordToMMs(Size)+0.1327);
          5 : Result := MMsToCoord(0.2622*CoordToMMs(Size)+0.1508);
          6 : Result := MMsToCoord(0.2194*CoordToMMs(Size)+0.1519);
          7 : Result := MMsToCoord(0.1957*CoordToMMs(Size)-0.2201);
          else Result := -1;
     end;
end;



procedure TFormAdjustDesignators.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;



procedure TFormAdjustDesignators.FormAdjustDesignatorsShow(Sender: TObject);
var
   LayerPair : TMechanicalLayerPair;
   i, j      : Integer;
begin
   ComboBoxLayers.Clear;
   ComboBoxDesignators.Clear;

   if Board.MechanicalPairs.Count = 0 then
   begin
      RadioButtonSingle.Checked := True;
      RadioButtonPair.Enabled := False;
      RadioButtonLayerSingle.Checked := True;
      RadioButtonLayerPair.Enabled := False;

      for i := 1 to 32 do
         if Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].MechanicalLayerEnabled then
         begin
            ComboBoxLayers.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
            if comboBoxLayers.Items.Count = 1 then
               ComboBoxLayers.Text := ComboBoxLayers.Items[0];

            ComboBoxDesignators.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
            if ComboBoxDesignators.Items.Count = 1 then
               ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
         end;
   end
   else
   begin
      for i := 1 to 32 do
      begin
         for j := i + 1 to 32 do
            if Board.MechanicalPairs.PairDefined(ILayer.MechanicalLayer(i), ILayer.MechanicalLayer(j)) then
            begin
               MechPairs.Add(Board.LayerName(ILayer.MechanicalLayer(i)) + ' <----> ' + Board.LayerName(ILayer.MechanicalLayer(j)));
               ComboBoxLayers.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)) + ' <----> ' + Board.LayerName(ILayer.MechanicalLayer(j)));
               if comboBoxLayers.Items.Count = 1 then
                  ComboBoxLayers.Text := ComboBoxLayers.Items[0];


               ComboBoxDesignators.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)) + ' <----> ' + Board.LayerName(ILayer.MechanicalLayer(j)));
               if ComboBoxDesignators.Items.Count = 1 then
                  ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
            end;

         // Here I need to fill in MechSingles, if user switches:
         if Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].MechanicalLayerEnabled then
            MechSingles.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
      end;
   end;
end;



procedure TFormAdjustDesignators.RadioButtonSingleClick(Sender: TObject);
var
   i : Integer;
begin

   ComboBoxDesignators.Clear;

   for i := 0 to MechSingles.Count - 1 do
   begin
      ComboBoxDesignators.Items.Add(MechSingles[i]);
   end;

   ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
end;



procedure TFormAdjustDesignators.RadioButtonPairClick(Sender: TObject);
var
   i : integer;
begin

   ComboBoxDesignators.Clear;

   for i := 0 to MechPairs.Count - 1 do
   begin
      ComboBoxDesignators.Items.Add(MechPairs[i]);
   end;

   ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
end;



procedure TFormAdjustDesignators.RadioButtonLayerPairClick(Sender: TObject);
var
   i : integer;
begin

   ComboBoxLayers.Clear;

   for i := 0 to MechPairs.Count - 1 do
   begin
      ComboBoxLayers.Items.Add(MechPairs[i]);
   end;

   ComboBoxLayers.Text := ComboBoxLayers.Items[0];
end;



procedure TFormAdjustDesignators.RadioButtonLayerSingleClick(Sender: TObject);
var
   i : Integer;
begin

   ComboBoxLayers.Clear;

   for i := 0 to MechSingles.Count - 1 do
   begin
      ComboBoxLayers.Items.Add(MechSingles[i]);
   end;

   ComboBoxLayers.Text := ComboBoxLayers.Items[0];
end;



procedure TFormAdjustDesignators.ButtonOKClick(Sender: TObject);
Var
    Track                   : IPCB_Primitive;
    TrackIteratorHandle     : IPCB_GroupIterator;
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    ASetOfLayers            : IPCB_LayerSet;
    S                       : TPCBString;
    TrackCount              : Integer;
    MaxX                    : Integer;
    MinX                    : Integer;
    MaxY                    : Integer;
    MinY                    : Integer;
    X                       : Integer;
    Y                       : Integer;
    Size                    : Integer;
    TextLength              : Integer;
    Designator              : IPCB_Text;

    OldSize                 : Integer;
    OldUseTTFonts           : Boolean;
    OldItalic               : Boolean;
    OldBold                 : Boolean;
    OldInverted             : Boolean;
    OldFontName             : String;
    OldFontID               : TFontID;
    OldRotation             : Float;
    OldXLocation            : Integer;
    OldYLocation            : Integer;
    OldAutoPosition         : TTextAutoposition;

    MechDesignator          : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    R                       : TCoordRect;
    i                       : integer;
    FirstLayerName          : String;
    SecondLayerName         : String;

    MaximumHeight           : float;   // millimeters
    MinimumHeight           : float;   // milimeters
    UnHideDesignators       : Boolean; // Unhides all designators
    LockStrings             : Boolean; // Lock all strings
    BoundingLayers          : Boolean; // Look for bounding rectangle in selected layers
    Layer1                  : TLayer;
    Layer2                  : TLayer;  // Change this to the layer/layers that best represent the component
    Layer3                  : Integer;  // In many cases eTopOverlay OR eBottomOverLay will be used
    Layer4                  : Integer;  // Layers not used must be set to false e.g Layer3=false;
    ShowOnce                : Boolean; // Only display the To many characters errors one time
begin
     // Here we will read various stuff from form

     if RadioButtonMM.Checked then
     begin
        MaximumHeight := MMsToCoord(StrToFloat(EditMaxHeight.Text));
        MinimumHeight := MMsToCoord(StrToFloat(EditMinHeight.Text));
     end
     else
     begin
        MaximumHeight := MilsToCoord(StrToFloat(EditMaxHeight.Text));
        MinimumHeight := MilsToCoord(StrToFloat(EditMinHeight.Text));
     end;

     if CheckBoxUnhide.Checked then UnHideDesignators := True
     else                           UnHideDesignators := False;

     if CheckBoxLock.Checked then LockStrings := True
     else                         LockStrings := False;

     if CheckBoxOverlayPrimitives.Checked or CheckBoxMechPrimitives.Checked then BoundingLayers := True
     else                                                                        BoundingLayers := False;

     if CheckBoxOverlayPrimitives.Checked then
     begin
        Layer1 := eTopOverlay;
        Layer2 := eBottomOverlay;
     end
     else
     begin
        Layer1 := false;
        Layer2 := false;
     end;

     if CheckBoxOverlayPrimitives.Checked then
     begin
        for i := 1 to 32 do
        begin
           if RadioButtonLayerPair.Checked then
           begin
              if GetFirstLayerName(ComboBoxLayers.Text) = Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then
                 Layer3 := ILayer.MechanicalLayer(i);

              if GetSecondLayerName(ComboBoxLayers.Text) = Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then
                 Layer4 := ILayer.MechanicalLayer(i);
           end
           else
           begin
              if ComboBoxLayers.Text := Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then
              begin
                 Layer3 := ILayer.MechanicalLayer(i);
                 Layer4 := ILayer.MechanicalLayer(i);
              end;
           end;
        end;
     end
     else
     begin
        Layer3 := false;
        Layer4 := false;
     end;


     // Disables Online DRC during designator movement to improve speed
     PCBSystemOptions := PCBServer.SystemOptions;

     If PCBSystemOptions = Nil Then Exit;

     DRCSetting := PCBSystemOptions.DoOnlineDRC;
     PCBSystemOptions.DoOnlineDRC := false;

     try

        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        S := '';

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) Do
        begin

             MaxX:= 0;
             MinX:= 999999999;

             MaxY:= 0;
             MinY:= 999999999;

             TrackCount :=0;

             //Show hidden designators?
             if UnHideDesignators = true then
                Component.NameOn := true;
             //Lock all strings?
             if LockStrings = true then
                Component.LockStrings := true;

             TrackIteratorHandle := Component.GroupIterator_Create;
             TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));

             Track := TrackIteratorHandle.FirstPCBObject;
             while (Track <> Nil) Do
             begin
                  // Look for component's tracks on the layers choosen under settings only when BoundingLayers is true
                  If (((Track.Layer = Layer1) OR
                  (Track.Layer = Layer2) OR
                  (Track.Layer = Layer3) OR
                  (Track.Layer = Layer4)) AND (BoundingLayers = True))Then
                  begin
                       Inc(TrackCount);

                       if Track.X1>= MaxX then MaxX:=Track.X1;
                       if Track.X1<= MinX then MinX:=Track.X1;

                       if Track.X2>= MaxX then MaxX:=Track.X2;
                       if Track.X2<= MinX then MinX:=Track.X2;

                       if Track.Y1>= MaxY then MaxY:=Track.Y1;
                       if Track.Y1<= MinY then MinY:=Track.Y1;

                       if Track.Y2>= MaxY then MaxY:=Track.Y2;
                       if Track.Y2<= MinY then MinY:=Track.Y2;
                  end;

                  Track := TrackIteratorHandle.NextPCBObject;
             end;
             // Destroy the track interator
            Component.GroupIterator_Destroy(TrackIteratorHandle);


            //Calculate the width and hegith of the bounding rectangle
            if TrackCount > 0 then
            begin
                 Y:=MaxY-MinY;
                 X:=MaxX-MinX;
            end
            else
            begin
                 R := Component.BoundingRectangleNoNameComment;
                 if R.left < MinX then MinX := R.left;
                 if R.bottom < MinY then MinY := R.bottom;
                 if R.right > MaxX then MaxX := R.right;
                 if R.top > MaxY then MaxY := R.top;

                 Y:=MaxY-MinY;
                 X:=MaxX-MinX;
            end;

            Designator    := Component.Name;
            OldSize       := Designator.Size;
            OldUseTTFonts := Designator.UseTTFonts;
            OldItalic     := Designator.Italic;
            OldBold       := Designator.Bold;
            OldInverted   := Designator.Inverted;
            OldFontName   := Designator.FontName;
            OldFontID     := Designator.FontID;
            OldRotation   := Designator.Rotation;
            OldXLocation  := Designator.XLocation;
            OldYLocation  := Designator.YLocation;
            OldAutoPosition := Component.NameAutoPosition;

            // Find text length so choose equation for size calcualtion
            S := Designator.GetDesignatorDisplayString;
            TextLength := Length(S);

            // notify that the pcb object is going to be modified
            Designator.BeginModify;

            // Set the size based on the bounding rectangle
            if Y >= X then
            begin
                 Size := CalculateSize(Y,S,TextLength);
                 if Size >= X then
                      Designator.Size := CalculateSize(X,S,TextLength);

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

            if Size > 0 then
            begin

               Designator.Size := Size;
               // Setup the text properties
               Designator.UseTTFonts := True;
               Designator.Italic := False;
               Designator.Bold := True;
               Designator.Inverted := False;
               Designator.FontName := 'Microsoft Sans Serif';


               // Rotate the designator to increase the readabillity
               if Y > X then
               begin
                    if Designator.Layer = eTopOverlay then
                       Designator.Rotation := 90
                    else
                        Designator.Rotation := 270;
               end
               else
               begin
                    Designator.Rotation := 0;
               end;


               // Trim down designator if its size is bigger then the MaximumHeight constant
               if Designator.Size >  MaximumHeight then
                  Designator.Size := MaximumHeight;

               if Designator.Size <  MinimumHeight then
                  Designator.Size := MinimumHeight;


               // notify that the pcb object is modified
               Designator.EndModify;

               // Set the autoposition to the center-center
               Component.ChangeNameAutoposition  :=  eAutoPos_CenterCenter;

               if CheckBoxMech.Checked then
               begin
                  // here I need group iterator that will iterate all text object and look out for
                  // Those on selected mech layers.
                  TrackIteratorHandle := Component.GroupIterator_Create;
                  TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTextObject));
                  if RadioButtonPair.Checked then
                  begin
                     for i := 1 to 32 do
                     begin
                        if GetFirstLayerName(ComboBoxLayers.Text) = Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then
                           Layer3 := ILayer.MechanicalLayer(i);

                        if GetSecondLayerName(ComboBoxLayers.Text) = Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then
                           Layer4 := ILayer.MechanicalLayer(i);
                     end;
                  end
                  else
                  begin
                     for i := 1 to 32 do
                     begin
                        if GetFirstLayerName(ComboBoxLayers.Text) = Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name then 
                        begin
                           Layer3 := ILayer.MechanicalLayer(i);
                           Layer4 := ILayer.MechanicalLayer(i);
                        end;
                     end;
                  end;

                  TrackIteratorHandle.AddFilter_LayerSet(AllLayers);

                  MechDesignator := TrackIteratorHandle.FirstPCBObject;
                  while (MechDesignator <> Nil) Do
                  begin                                                        //                     \/ This function returns just string of MechDesignator.Text
                     if (((MechDesignator.Layer = Layer3) or (MechDesignator.Layer = Layer4)) and ((GetFirstLayerName(MechDesignator.Text) = '.Designator' ) or (MechDesignator.Text = Designator.Text))) then
                     begin
                        MechDesignator.Size       := Designator.Size;
                        MechDesignator.UseTTFonts := Designator.UseTTFonts;
                        MechDesignator.Italic     := Designator.Italic;
                        MechDesignator.Bold       := Designator.Bold;
                        MechDesignator.Inverted   := Designator.Inverted;
                        MechDesignator.FontName   := Designator.FontName;
                        MechDesignator.Rotation   := Designator.Rotation;
                        MechDesignator.XLocation  := Designator.XLocation;
                        MechDesignator.YLocation  := Designator.YLocation;

                     end;

                     MechDesignator := TrackIteratorHandle.NextPCBObject;
                  end;
                  // Destroy the track interator
                 Component.GroupIterator_Destroy(TrackIteratorHandle);

               end;

               if not CheckBoxOverlay.Checked then
               begin
                  Designator.BeginModify;
                  Designator.Size       := OldSize;
                  Designator.UseTTFonts := OldUseTTFonts;
                  Designator.Italic     := OldItalic;
                  Designator.Bold       := OldBold;
                  Designator.Inverted   := OldInverted;
                  Designator.FontName   := OldFontName;
                  Designator.FontID     := OldFontID;

                  Component.ChangeNameAutoposition(OldAutoPosition);

                  Designator.Rotation   := OldRotation;
                  Designator.XLocation  := OldXLocation;
                  Designator.YLocation  := OldYLocation;
                  Designator.EndModify;

                  Designator.GraphicallyInvalidate;
               end;

            end;

            // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;

        end;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        //Refresh the screen
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

    finally
           // Restore DRC setting
           PCBSystemOptions.DoOnlineDRC :=  DRCSetting;
    end;
    Close;
end;



procedure TFormAdjustDesignators.CheckBoxMechClick(Sender: TObject);
begin
   If CheckBoxMech.Checked then
   begin
      if MechPairs.Count <> 0 then
         RadioButtonPair.Enabled   := True;
      RadioButtonSingle.Enabled := True;
      ComboBoxDesignators.Enabled    := True;
   end
   else
   begin
      RadioButtonPair.Enabled   := False;
      RadioButtonSingle.Enabled := False;
      ComboBoxDesignators.Enabled    := False;
   end;
end;



procedure TFormAdjustDesignators.CheckBoxMechPrimitivesClick(Sender: TObject);
begin
   If CheckBoxMechPrimitives.Checked then
   begin
      if MechPairs.Count <> 0 then
         RadioButtonLayerPair.Enabled   := True;
      RadioButtonLayerSingle.Enabled := True;
      ComboBoxLayers.Enabled         := True;
   end
   else
   begin
      RadioButtonLayerPair.Enabled   := False;
      RadioButtonLayerSingle.Enabled := False;
      ComboBoxLayers.Enabled         := False;
   end;
end;



procedure TFormAdjustDesignators.EditMinHeightChange(Sender: TObject);
begin
   if not IsStringANum(EditMinHeight.Text) then
   begin
      ButtonOK.Enabled := False;
      EditMinHeight.Font.Color := clRed;
   end
   else
   begin
      EditMinHeight.Font.Color := clWindowText;
      if IsStringANum(EditMaxHeight.Text) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TFormAdjustDesignators.EditMaxHeightChange(Sender: TObject);
begin
   if not IsStringANum(EditMaxHeight.Text) then
   begin
      ButtonOK.Enabled := False;
      EditMaxHeight.Font.Color := clRed;
   end
   else
   begin
      EditMaxHeight.Font.Color := clWindowText;
      if IsStringANum(EditMinHeight.Text) then
         ButtonOK.Enabled := True;
   end;
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   MechPairs   := TStringList.Create;
   MechSingles := TStringList.Create;

   FormAdjustDesignators.ShowModal;
end;
