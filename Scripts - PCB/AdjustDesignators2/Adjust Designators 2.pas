{........................................................................................}
{ Summary:  This script can be used to adjust designators on mech layers or              }
{           on silkscreen. The designators are centred, rotated at 0 or 90 deg           }
{           depending on the component orientation and scaled appropriately.             }
{
{   Mechanical Designators are just extra text object(s) that are part of footprint.
    They can be special strings '.Designator' or just text with face value = designator
    The extra designators may be part of the library footprints or added by a
     script CopyDesignatorsToMechLayerPair.pas
}
{ Created by:     Mattias Ericson                                              }
{ Reviewed by:    Petar Perisin                                                }
{ Improvements:   Miroslav Dobrev, Stanislav Popelka                           }
{
 Last Update 30/09/2018 - added stroke font option
 Update 15/03/2016 (Miroslav Dobrev)
  - The script now works with Altium Designer version 14 and greater
  - The script now also works with hidden designator components normally,
    without the need to permanently un-hide the designators first
  - Broken requests to interface elements fixed
  - Other small fixes
 09/04/2021 v2.20 BLM Support for AD19+ mechlayers, refactored tangled inefficient loops.
 17/04/2021 v2.21 BLM Minor refactor to CalculateSize() parameters & code formatting.
                      Add constants for stroke text width ratios on mech & overlay layers
                      Fix the Layer selection for MechDesg., adjust width of mech designators
..........................................................................................}

const
    AD19VersionMajor  = 19;
    AD17MaxMechLayers = 32;
    AD19MaxMechLayers = 1024;

    cSilkTextWidthRatio = 5;      // ratio of text width to height for Overlay/silk layers
    cTextWidthRatio     = 10;     // for non-Overlay text

var
    VerMajor        : WideString;
    LegacyMLS       : boolean;
    Board           : IPCB_Board;
    LayerStack      : IPCB_LayerStack_V7;
    LayerObj        : IPCB_LayerObject_V7;
    MechLayer1      : IPCB_MechanicalLayer;
    MechLayer2      : IPCB_MechanicalLayer;
    MechLayerPairs  : IPCB_MechanicalLayerPairs;
    MaxMechLayers   : integer;
    ML1, ML2        : integer;
    slMechPairs     : TStringList;
    slMechSingles   : TStringList;

function Version(const dummy : boolean) : TStringList;         forward;
function GetFirstLayerName(Pair : String) : String;            forward;
function GetSecondLayerName(Pair : String) : String;           forward;
function Version(const dummy : boolean) : TStringList;         forward;
function IsStringANum(Tekst : String) : Boolean;               forward;
function CalculateSize (Size : Integer, S : String) : Integer; forward;


procedure TFormAdjustDesignators.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFormAdjustDesignators.FormAdjustDesignatorsShow(Sender: TObject);
var
    i, j            : Integer;

begin
    ComboBoxLayers.Clear;
    ComboBoxDesignators.Clear;

    LayerStack := Board.LayerStack_V7;

// are any layer pairs defined ?..
    if MechLayerPairs.Count = 0 then
    begin
        RadioButtonSingle.Checked := True;
        RadioButtonPair.Enabled := False;
        GroupBoxLayer.Caption := 'Choose Mech Layer:';

        RadioButtonLayer1.Enabled := False;
        RadioButtonLayer2.Enabled := False;
    end;

    for i := 1 to MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer1 := LayerStack.LayerObject_V7(ML1);

        if MechLayer1.MechanicalLayerEnabled then
        begin
            slMechSingles.Add(Board.LayerName(ML1));

            if (RadioButtonPair.Checked) then
            begin
                for j := (i + 1) to MaxMechLayers do
                begin
                    ML2 := LayerUtils.MechanicalLayer(j);
                    MechLayer2 := LayerStack.LayerObject_V7(ML2);
                    if MechLayer2.MechanicalLayerEnabled then
                    if MechLayerPairs.PairDefined(ML1, ML2) then
                    begin
                        slMechPairs.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        ComboBoxLayers.Items.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        if ComboBoxLayers.Items.Count = 1 then
                        begin
                            ComboBoxLayers.Text := ComboBoxLayers.Items[0];
//                            RadioButtonLayer1.caption := Board.LayerName(ML1);
//                            RadioButtonLayer2.caption := Board.LayerName(ML2);
                        end;
                        ComboBoxDesignators.Items.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        if ComboBoxDesignators.Items.Count = 1 then
                            ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
                    end;
                end;  // j
            end else
            begin
// single layer radio button ticked/checked.
                ComboBoxLayers.Items.Add(Board.LayerName(ML1));
                if ComboBoxLayers.Items.Count = 1 then
                    ComboBoxLayers.Text := ComboBoxLayers.Items[0];
                ComboBoxDesignators.Items.Add(Board.LayerName(ML1));
                if ComboBoxDesignators.Items.Count = 1 then
                    ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];

            end;
        end;
    end;
end;

procedure TFormAdjustDesignators.RadioButtonSingleClick(Sender: TObject);
var
   i : Integer;
begin

   ComboBoxDesignators.Clear;

   for i := 0 to (slMechSingles.Count - 1) do
   begin
      ComboBoxDesignators.Items.Add(slMechSingles[i]);
   end;

   ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
end;

procedure TFormAdjustDesignators.RadioButtonPairClick(Sender: TObject);
var
   i : integer;
begin

   ComboBoxDesignators.Clear;

   for i := 0 to (slMechPairs.Count - 1) do
   begin
      ComboBoxDesignators.Items.Add(slMechPairs[i]);
   end;

   ComboBoxDesignators.Text := ComboBoxDesignators.Items[0];
end;

procedure TFormAdjustDesignators.RadioButtonLayerPairClick(Sender: TObject);
var
   i : integer;
begin

   ComboBoxLayers.Clear;

   for i := 0 to (slMechPairs.Count - 1) do
   begin
      ComboBoxLayers.Items.Add(slMechPairs[i]);
   end;

   ComboBoxLayers.Text := ComboBoxLayers.Items[0];
end;

procedure TFormAdjustDesignators.RadioButtonLayerSingleClick(Sender: TObject);
var
   i : Integer;
begin

   ComboBoxLayers.Clear;

   for i := 0 to (slMechSingles.Count - 1) do
   begin
      ComboBoxLayers.Items.Add(slMechSingles[i]);
   end;

   ComboBoxLayers.Text := ComboBoxLayers.Items[0];
end;

procedure TFormAdjustDesignators.ButtonOKClick(Sender: TObject);
Var
    Track                   : IPCB_Primitive;
    GroupIterator           : IPCB_GroupIterator;
    Component               : IPCB_Component;
    ComponentIterator       : IPCB_BoardIterator;
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
    Designator              : IPCB_Text;

    OldSize                 : Integer;
    OldWidth                : Integer;
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
    OldVisibility           : Boolean;

    OldAutoPosComment       : TTextAutoposition;

    MechDesignator          : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    R                       : TCoordRect;
    i                       : integer;
    FirstLayerName          : String;
    SecondLayerName         : String;

    MaximumHeight           : float;   // millimeters
    MinimumHeight           : float;   // millimeters
    UnHideDesignators       : Boolean; // Unhides all designators
    LockStrings             : Boolean; // Lock all strings
    BoundingLayers          : Boolean; // Look for bounding rectangle in selected layers
    Layer1                  : TLayer;
    Layer2                  : TLayer;    // Change this to the layer/layers that best represent the component
    Layer3                  : Integer;   // In many cases eTopOverlay OR eBottomOverLay will be used
    Layer4                  : Integer;   // Layers not used must be set to false e.g Layer3=false;
    MDLayer3                : integer;   // mech designator layers
    MDLayer4                : Integer;
    ShowOnce                : Boolean;   // Only display the To many characters errors one time
begin
    // Here we will read various stuff from form

    LayerStack := Board.LayerStack_V7;

    // User defined Minimum Stroke Font Width in Mils
    if RadioButtonMM.Checked then
    begin
        MaximumHeight := MMsToCoord(StrToFloat(EditMaxHeight.Text));
        MinimumHeight := MMsToCoord(StrToFloat(EditMinHeight.Text));
    end else
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
    end else
    begin
        Layer1 := false;
        Layer2 := false;
    end;

    // Disables Online DRC during designator movement to improve speed
    PCBSystemOptions := PCBServer.SystemOptions;
    If PCBSystemOptions <> Nil Then
    begin
        DRCSetting := PCBSystemOptions.DoOnlineDRC;
        PCBSystemOptions.DoOnlineDRC := false;
    end;

    Layer3   := 0; Layer4   := 0;
    MDLayer3 := 0; MDLayer4 := 0;

    for i := 1 to MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer1 := LayerStack.LayerObject_V7[ML1];

    //  Check needs to be done for every component, otherwise doesn't work for some reason
        if CheckBoxMechPrimitives.Checked then
        begin
            if RadioButtonLayerPair.Checked then
            begin
                if GetFirstLayerName(ComboBoxLayers.Text) = MechLayer1.Name then
                    Layer3 := ML1;
                if GetSecondLayerName(ComboBoxLayers.Text) = MechLayer1.Name then
                    Layer4 := ML1;
            end else
            begin
                if ComboBoxLayers.Text := MechLayer1.Name then
                begin
                    Layer3 := ML1;
                    Layer4 := ML1;
                end;
            end;
        end;

        if CheckBoxMech.Checked then
        begin
            if RadioButtonPair.Checked then
            begin
                if GetFirstLayerName(ComboBoxDesignators.Text) = MechLayer1.Name then
                    MDLayer3 := ML1;
                if GetSecondLayerName(ComboBoxDesignators.Text) = MechLayer1.Name then
                    MDLayer4 := ML1;
            end else
            begin
                if GetFirstLayerName(ComboBoxDesignators.Text) = MechLayer1.Name then
                begin
                    MDLayer3 := ML1;
                    MDLayer4 := ML1;
                end;
           end;
        end;
    end;  // for

    ASetOfLayers := LayerSetUtils.SignalLayers;
    S := '';
    // Notify the pcbserver that we will make changes
    PCBServer.PreProcess;

    ComponentIterator := Board.BoardIterator_Create;
    ComponentIterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);
    ComponentIterator.AddFilter_Method(eProcessAll);
    Component := ComponentIterator.FirstPCBObject;
      
    while (Component <> Nil) Do
    begin
        MaxX:= kMinCoord;
        MinX:= kMaxCoord; 
        
        MaxY:= kMinCoord;
        MinY:= kMaxCoord;
        
        TrackCount := 0;
        
        // Save designator visibility and unhide
        Component.BeginModify;
        OldVisibility := Component.NameOn;
        Component.NameOn := true;
        // Lock all strings?
        if LockStrings = true then
            Component.LockStrings := true;
        
        ASetOfLayers := LayerSetUtils.EmptySet;
        if Layer1 <> 0 then ASetOfLayers.Include(Layer1);
        if Layer2 <> 0 then ASetOfLayers.Include(Layer2);
        if Layer3 <> 0 then ASetOfLayers.Include(Layer3);
        if Layer4 <> 0 then ASetOfLayers.Include(Layer4);
        GroupIterator := Component.GroupIterator_Create;
        GroupIterator.AddFilter_ObjectSet(MkSet(eTrackObject));
        GroupIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);  // Group filter DNW

        Track := GroupIterator.FirstPCBObject;
        while (Track <> Nil) Do
        begin
            // Look for component's tracks on the layers chosen under settings only when BoundingLayers is true
            If ASetOfLayers.Contains(Track.Layer) and (BoundingLayers) Then
            begin
                Inc(TrackCount);
                MaxX := Max(Track.X1, MaxX);
                MinX := Min(Track.X1, MinX);
                MaxX := Max(Track.X2, MaxX);
                MinX := Min(Track.X2, MinX);
                MaxY := Max(Track.Y1, MaxY);
                MinY := Min(Track.Y1, MinY);
                MaxY := Max(Track.Y2, MaxY);
                MinY := Min(Track.Y2, MinY);
            end;

            Track := GroupIterator.NextPCBObject;
        end;
        Component.GroupIterator_Destroy(GroupIterator);

        // Calculate the width and heigth of the bounding rectangle
        if TrackCount > 0 then
        begin
            Y := MaxY - MinY;
            X := MaxX - MinX;
        end else
        begin
            R := Component.BoundingRectangleNoNameComment;
            if R.left <   MinX then MinX := R.left;
            if R.bottom < MinY then MinY := R.bottom;
            if R.right >  MaxX then MaxX := R.right;
            if R.top >    MaxY then MaxY := R.top;

            Y := MaxY - MinY;
            X := MaxX - MinX;
        end;

        Designator        := Component.Name;
        OldSize           := Designator.Size;
        OldWidth          := Designator.Width;
        OldUseTTFonts     := Designator.UseTTFonts;
        OldItalic         := Designator.Italic;
        OldBold           := Designator.Bold;
        OldInverted       := Designator.Inverted;
        OldFontName       := Designator.FontName;
        OldFontID         := Designator.FontID;
        OldRotation       := Designator.Rotation;
        OldXLocation      := Designator.XLocation;
        OldYLocation      := Designator.YLocation;
        OldAutoPosition   := Component.GetState_NameAutoPos;
        OldAutoPosComment := Component.GetState_CommentAutoPos;
        
        // Find text length so choose equation for size calculation
        S := Designator.GetDesignatorDisplayString;
        
        // notify that the pcb object is going to be modified
        Component.Name.BeginModify;
        Component.Comment.BeginModify;
        
        // Set the size based on the bounding rectangle
        if Y >= X then
        begin
            Size := CalculateSize(Y, S);
            if Size >= X then
                Size := CalculateSize(X, S);
        end else
        begin
            Size := CalculateSize(X, S);
            if Size >= Y then
                Size := CalculateSize(Y, S);
        end;
        
        if ((Size = -1) AND (ShowOnce = False)) then
        begin
            ShowMessage('To many characters in one or more components such as (' + Component.Name.Text + '). More than 7 characters are not supported and these components will be ommited.');
            ShowOnce := True;
        end;

        if Size > 0 then
        begin
            // Setup the text properties
            Designator.UseTTFonts := True;
            Designator.Italic     := False;
            Designator.Bold       := True;
            Designator.Inverted   := False;
            Designator.FontName   := 'Microsoft Sans Serif';
            Designator.Size       := Size;

            If (cbxUseStrokeFonts.Checked = True) then
            begin
                Designator.UseTTFonts := False;
                Designator.Width := Designator.Size / cTextWidthRatio;
                // Thicker strokes for Overlay text
                if (Designator.Layer = eTopOverlay) or (Designator.Layer = eBottomOverlay) then
                    Designator.Width := Designator.Size / cSilkTextWidthRatio;
            end;


            // Rotate the designator to increase the readability
            if Y > X then
            begin
                if Designator.Layer = eTopOverlay then
                    Designator.Rotation := 90
                else
                    Designator.Rotation := 270;
            end else
            begin
                Designator.Rotation := 0;
            end;
            
            // Trim down designator if its size is bigger than the MaximumHeight constant
            if Designator.Size >  MaximumHeight then
            begin
                Designator.Size := MaximumHeight;
                Designator.Width := MaximumHeight / cTextWidthRatio;
            end;
            
            if Designator.Size <  MinimumHeight then
                Designator.Size := MinimumHeight;

            // Set the Designator AutoPosition to the center-center but stop comment being moved.
            Component.ChangeCommentAutoposition( eAutoPos_Manual );
            Component.ChangeNameAutoposition(eAutoPos_CenterCenter);
            Component.SetState_CommentAutoPos(OldAutoPosComment);
            
            
            if CheckBoxMech.Checked then
            begin
                // group iterate for all text object on specific mech layers
                ASetOfLayers := LayerSetUtils.EmptySet;
                if MDLayer3 <> 0 then ASetOfLayers.Include(MDLayer3);
                if MDLayer4 <> 0 then ASetOfLayers.Include(MDLayer4);
                GroupIterator := Component.GroupIterator_Create;
                GroupIterator.AddFilter_ObjectSet(MkSet(eTextObject));
                GroupIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);
                
                MechDesignator := GroupIterator.FirstPCBObject;
                while (MechDesignator <> Nil) Do
                begin
                     if ASetOfLayers.Contains(MechDesignator.Layer) then
                     if ((MechDesignator.GetState_UnderlyingString = '.Designator' ) or (MechDesignator.GetState_ConvertedString = Designator.GetState_ConvertedString)) then
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
                         If (cbxUseStrokeFonts.Checked = True) then
                         begin
                             MechDesignator.UseTTFonts := False;
                             MechDesignator.Width := MechDesignator.Size / cTextWidthRatio;
                             // Thicker strokes for Overlay text
                             if (MechDesignator.Layer = eTopOverlay) or (MechDesignator.Layer = eBottomOverlay) then
                                 MechDesignator.Width := MechDesignator.Size / cSilkTextWidthRatio;
                             end;
                     end;

                     MechDesignator := GroupIterator.NextPCBObject;
                end;
                // Destroy the track interator
                Component.GroupIterator_Destroy(GroupIterator);
            end;

            if not CheckBoxOverlay.Checked then
            begin
                Designator.Width      := OldWidth;
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
            end;

        end;

        // Restoring designator visibility
        if UnHideDesignators = false then
            Component.NameOn := OldVisibility;

        Component.Name.EndModify;
        Component.Comment.EndModify;
        Component.SetState_XSizeYSize;
        Component.EndModify;
        Component.GraphicallyInvalidate;

        // Get the next component
        Component := ComponentIterator.NextPCBObject;
    end;
    // Destroy the component iterator
    Board.BoardIterator_Destroy(ComponentIterator);

    // Notify the pcbserver that all changes have been made
    PCBServer.PostProcess;
    //Refresh the screen
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    // Restore DRC setting
    If PCBSystemOptions <> Nil Then
        PCBSystemOptions.DoOnlineDRC := DRCSetting;
    Close;
end;

procedure TFormAdjustDesignators.CheckBoxMechClick(Sender: TObject);
begin
   If CheckBoxMech.Checked then
   begin
      if MechLayerPairs.Count <> 0 then
         RadioButtonPair.Enabled := True;
      RadioButtonSingle.Enabled   := True;
      ComboBoxDesignators.Enabled := True;
   end
   else
   begin
      RadioButtonPair.Enabled     := False;
      RadioButtonSingle.Enabled   := False;
      ComboBoxDesignators.Enabled := False;
   end;
end;

procedure TFormAdjustDesignators.CheckBoxMechPrimitivesClick(Sender: TObject);
begin
   If CheckBoxMechPrimitives.Checked then
   begin
      if MechLayerPairs.Count <> 0 then
         RadioButtonLayerPair.Enabled := True;
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
    if Board = nil then
    begin
        ShowMessage('Focused Doc Not a .PcbDoc ');
        exit;
    end;

//  Check AD version for layer stack version
    VerMajor := Version(true).Strings(0);
    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        LegacyMLS     := false;
        MaxMechLayers := AD19MaxMechLayers;
    end;

    slMechPairs    := TStringList.Create;
    slMechSingles  := TStringList.Create;
    MechLayerPairs := Board.MechanicalPairs;

    FormAdjustDesignators.ShowModal;

    slMechPairs.Clear;
    slMechSingles.Clear;
end;

{.......................................................................................}
// Function that checks is string a float number or not
function IsStringANum(Tekst : String) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;
    // Test for number, dot or comma
    ChSet := SetUnion(MkSet(Ord('.'),Ord(',')), MkSetRange(Ord('0'), Ord('9')) );
    for i := 1 to Length(Tekst) do
       if not InSet(Ord(Tekst[i]), ChSet) then Result := false;

    // Test if we have more than one dot or comma
    dotCount := 0;
    ChSet := MkSet(Ord('.'),Ord(','));
    for i := 1 to Length(Tekst) do
       if InSet(Ord(Tekst[i]), ChSet) then
          Inc(dotCount);

    if dotCount > 1 then Result := False;
end;

//Calculate the hight of the true type text to best fit for Microsoft Serif
function CalculateSize (Size : Integer, S : String) : Integer;
var
    TextLength : Integer;
begin
    Result := -1;
    TextLength := Length(S);
    case TextLength of
        1 : Result := MMsToCoord(1.3013*CoordToMMs(Size)-0.0597);
        2 : Result := MMsToCoord(0.7201*CoordToMMs(Size)+0.0612);
        3 : Result := MMsToCoord(0.4319*CoordToMMs(Size)+0.1116);
        4 : Result := MMsToCoord(0.3265*CoordToMMs(Size)+0.1327);
        5 : Result := MMsToCoord(0.2622*CoordToMMs(Size)+0.1508);
        6 : Result := MMsToCoord(0.2194*CoordToMMs(Size)+0.1519);
        7 : Result := MMsToCoord(0.1957*CoordToMMs(Size)-0.2201);
    end;
    // Use Stroke Fonts
    If (cbxUseStrokeFonts.Checked = True) then
    begin
        Result := Result * 0.4;    //Scaled Result for Stroked Fonts
    end;
end;

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
   Delete(Pair, 1, Pos + 7);

   Result := Pair;
end;

function Version(const dummy : boolean) : TStringList;
begin
    Result               := TStringList.Create;
    Result.Delimiter     := '.';
    Result.Duplicates    := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;
