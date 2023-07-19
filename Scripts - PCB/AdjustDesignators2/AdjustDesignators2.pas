{........................................................................................}
{ Summary:  This script can be used to adjust designators on mech layers or              }
{           on silkscreen. The designators are centred, rotated at 0 or 90 deg           }
{           depending on the component orientation and scaled appropriately.             }
{
{   Mechanical Designators are just extra text object(s) that are part of footprint.
    They can be special strings '.Designator' or just text with face value = designator
    The extra designators may be part of the library footprints or added by a
     script CopyDesignatorsToMechLayerPair.pas

 Created by:     Mattias Ericson
 Reviewed by:    Petar Perisin
 Improvements:   Miroslav Dobrev, Stanislav Popelka, Brett Miller


 2023-07-19  v2.12   avoid V7 layerstack methods
                     further hacking to add rotation around XY..
 Update 28/09/2021 - Resize text bounding box before move() to fix multi-line text offsets.
 Update 05/05/2021 - Stop Comment moving with Designator AutoCenter.
                     Support AD19+ mech layers, handle existing multiline text
                     Add constants for text widths for overlay & non-overlay
                     Fix AD20+ some text not moving back correctly.
 Update 30/09/2018 - added stroke font option
 Update 15/03/2016 (Miroslav Dobrev)
  - The script now works with Altium Designer version 14 and greater
  - The script now also works with hidden designator components normally,
    without the need to permanently un-hide the designators first
  - Broken requests to interface elements fixed
  - Other small fixes

Note:  Method uses Designator auto-centre to calculate mechanical designator position.
       It does not handle TTF very well.

..........................................................................................}

const
    AD19VersionMajor  = 19;
    AD17MaxMechLayers = 32;
    AD19MaxMechLayers = 1024;

    cSilkTextWidthRatio = 5;      // ratio of text width to height for Overlay/silk layers
    cTextWidthRatio     = 10;     // for non-Overlay text
    cTopSide            = 'Top';
    cBottomSide         = 'Bottom';
    cSeparator          = ' <---> ';
var
    VerMajor        : WideString;
    LegacyMLS       : boolean;
    Board           : IPCB_Board;
    LayerStack      : IPCB_MasterLayerStack;
    LayerObj        : IPCB_LayerObject;
    MechLayer1      : IPCB_MechanicalLayer;
    MechLayer2      : IPCB_MechanicalLayer;
    MechLayerPairs  : IPCB_MechanicalLayerPairs;
    MaxMechLayers   : integer;
    ML1, ML2        : integer;
    slMechPairs     : TStringList;
    slMechSingles   : TStringList;

function Version(const dummy : boolean) : TStringList;                                  forward;
function GetFirstLayerName(Pair : String) : String;                                     forward;
function GetSecondLayerName(Pair : String) : String;                                    forward;
function Version(const dummy : boolean) : TStringList;                                  forward;
function IsStringANum(Tekst : String) : Boolean;                                        forward;
function CalculateSize (Size : Integer, S : String, UseStrokeFont : boolean) : Integer; forward;
function GetMechLayerObject(LS: IPCB_MasterLayerStack, i : integer, var MLID : TLayer) : IPCB_MechanicalLayer; forward;

procedure TFormAdjustDesignators.ButtonCancelClick(Sender: TObject);
begin
    FormAdjustDesignators.Close;
    slMechPairs.Clear;
    slMechSingles.Clear;
end;

procedure TFormAdjustDesignators.FormAdjustDesignatorsShow(Sender: TObject);
var
    i, j         : Integer;

begin
    ComboBoxLayers.Clear;
    ComboBoxDesignators.Clear;

// are any layer pairs defined ?..
    if MechLayerPairs.Count = 0 then
    begin
        RadioButtonSingle.Checked := True;
        RadioButtonPair.Enabled := False;
//        ComboBoxLayers.Text := 'Choose a Mech Layer:';
        RadioButtonLayerSingle.Checked := True;
        RadioButtonLayerPair.Enabled := False;
    end;

    for i := 1 to MaxMechLayers do
    begin
        MechLayer1 := GetMechLayerObject(LayerStack, i, ML1);

        if MechLayer1.MechanicalLayerEnabled then
        begin
            slMechSingles.Add(Board.LayerName(ML1));

            if (RadioButtonPair.Checked) then
            begin
                for j := (i + 1) to MaxMechLayers do
                begin
                    MechLayer2 := GetMechLayerObject(LayerStack, j, ML2);

                    if MechLayer2.MechanicalLayerEnabled then
                    if MechLayerPairs.PairDefined(ML1, ML2) then
                    begin
                        If (ansipos(cBottomSide, Board.LayerName(ML1)) > 0) or
                           (ansipos(cTopSide, Board.LayerName(ML2)) > 0) then IntSwap(ML1, ML2);

                        slMechPairs.Add(Board.LayerName(ML1) + cSeparator + Board.LayerName(ML2));
                        ComboBoxLayers.Items.Add(Board.LayerName(ML1) + cSeparator + Board.LayerName(ML2));
                        if ComboBoxLayers.Items.Count = 1 then
                            ComboBoxLayers.Text := ComboBoxLayers.Items(0);

                        ComboBoxDesignators.Items.Add(Board.LayerName(ML1) + cSeparator + Board.LayerName(ML2));
                        if ComboBoxDesignators.Items.Count = 1 then
                            ComboBoxDesignators.Text := ComboBoxDesignators.Items(0);
                    end;
                end;  // j
            end else
            begin
// single layer radio button ticked/checked.
                ComboBoxLayers.Items.Add(Board.LayerName(ML1));
                if ComboBoxLayers.Items.Count = 1 then
                    ComboBoxLayers.Text := ComboBoxLayers.Items(0);
                ComboBoxDesignators.Items.Add(Board.LayerName(ML1));
                if ComboBoxDesignators.Items.Count = 1 then
                    ComboBoxDesignators.Text := ComboBoxDesignators.Items(0);

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
    if slMechSingles.Count > 0 then
        ComboBoxDesignators.SetItemIndex(0);
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
    if slMechPairs.Count > 0 then
        ComboBoxDesignators.SetItemIndex(0);
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
    if slMechPairs.Count > 0 then
        ComboBoxLayers.SetItemIndex(0);
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
    if slMechSingles.Count > 0 then
        ComboBoxLayers.SetItemIndex(0);
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

    DesSize                 : Integer;
    DesWidth                : Integer;
    DesUseTTFonts           : Boolean;
    DesItalic               : Boolean;
    DesBold                 : Boolean;
    DesInverted             : Boolean;
    DesFontName             : String;
    DesFontID               : TFontID;
    DesRotation             : Float;
    DesXLocation            : Integer;
    DesYLocation            : Integer;
    DesAutoPosition         : TTextAutoposition;
    DesVisibility           : Boolean;
    DesAutoPosComment       : TTextAutoposition;

    DesMultiline            : boolean;
    DesMultilineTH          : TCoord;
    DesMultilineTW          : TCoord;
//    DesMultilineResizeEnabled : boolean;
    DesMultilineAuto        : TTextAutoposition;

    MechDesignator          : IPCB_Text;
    MechDesMultiline        : boolean;
    MechDesMultilineTH      : TCoord;
    MechDesMultilineTW      : TCoord;
    MechDesMultilineAuto    : TTextAutoposition;
    MechDesRotation         : Float;

    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    BRC                     : TPoint;
    BR                      : TCoordRect;
    i                       : integer;

    MaximumHeight           : TCoord;    // UI mils to tcoord
    MinimumHeight           : TCoord;     
    UnHideDesignators       : Boolean;   // Unhides all designators
    LockStrings             : Boolean;   // Lock all strings
    UseStrokeFont           : boolean;
    ModOverLayText          : boolean;
    ModMechText             : boolean;
    UseMultiLine            : boolean;
    BoundingLayers          : Boolean;   // Look for bounding rectangle in selected layers
    Layer1                  : TLayer;
    Layer2                  : TLayer;    // Change this to the layer/layers that best represent the component
    Layer3                  : Integer;   // In many cases eTopOverlay OR eBottomOverLay will be used
    Layer4                  : Integer;   // Layers not used must be set to false e.g Layer3=false;
    MDLayer3                : integer;   // mech designator layers
    MDLayer4                : Integer;
    ShowOnce                : Boolean;   // Only display the To many characters errors one time

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    LayerStack := Board.MasterLayerStack;

    // User defined Minimum Stroke Font Width in (mils)
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

    if cbxUseStrokeFonts.Checked then UseStrokefont := true
    else                              UseStrokefont := false;

    if cbxUseMultiLine.Checked then UseMultiLine := true
    else                            UseMultiLine := false;

    if CheckBoxOverlay.Checked then ModOverLayText := true
    else                            ModOverLayText := false;

    if CheckBoxMech.Checked then ModMechText := true
    else                         ModMechText := false;

    if (CheckBoxOverlayPrimitives.Checked or CheckBoxMechPrimitives.Checked) then BoundingLayers := True
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

    // Disable Online DRC during designator movement to improve speed
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
        MechLayer1 := GetMechLayerObject(LayerStack, i, ML1);

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

        if ModMechText then
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
        DesVisibility := Component.NameOn;
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
        GroupIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);     // Group filter DNW

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
            BR := Component.BoundingRectangleNoNameComment;
            if BR.left   < MinX then MinX := BR.left;
            if BR.bottom < MinY then MinY := BR.bottom;
            if BR.right  > MaxX then MaxX := BR.right;
            if BR.top    > MaxY then MaxY := BR.top;

            Y := MaxY - MinY;
            X := MaxX - MinX;
        end;

        Designator        := Component.Name;
        DesSize           := Designator.Size;
        DesWidth          := Designator.Width;
        DesUseTTFonts     := Designator.UseTTFonts;
        DesItalic         := Designator.Italic;
        DesBold           := Designator.Bold;
        DesInverted       := Designator.Inverted;
        DesFontName       := Designator.FontName;
        DesFontID         := Designator.FontID;
        DesRotation       := Designator.Rotation;
        DesXLocation      := Designator.XLocation;
        DesYLocation      := Designator.YLocation;
        DesAutoPosition   := Component.GetState_NameAutoPos;
        DesAutoPosComment := Component.GetState_CommentAutoPos;

        DesMultiline      := Designator.Multiline;
        DesMultilineTH    := Designator.MultilineTextHeight;
        DesMultilineTW    := Designator.MultilineTextWidth;
//        DesMultilineResizeEnabled := Designator.MultilineTextResizeEnabled;
        DesMultilineAuto  := Designator.MultilineTextAutoPosition;

        // Find text length so choose equation for size calculation
        S := Designator.GetDesignatorDisplayString;
        if Y >= X then
        begin
            Size := CalculateSize(Y, S, UseStrokeFont);
            if Size >= X then
                Size := CalculateSize(X, S, UseStrokeFont);
        end else
        begin
            Size := CalculateSize(X, S, UseStrokeFont);
            if Size >= Y then
                Size := CalculateSize(Y, S, UseStrokeFont);
        end;

        if ((Size = -1) AND (ShowOnce = False)) then
        begin
            ShowMessage('To many characters in one or more components such as (' + Component.Name.Text + '). More than 7 characters are not supported and these components will be ommited.');
            ShowOnce := True;
        end;

        // notify that the pcb object is going to be modified
        Component.Name.BeginModify;
        Component.Comment.BeginModify;

        if Size > 0 then
        begin
            // Setup the text properties
            Designator.UseTTFonts := True;
            Designator.Italic     := False;
            Designator.Bold       := True;
            Designator.Inverted   := False;
            Designator.FontName   := 'Microsoft Sans Serif';
            Designator.Size       := Size;
//            Designator.Rotation   := Component.Rotation;

            If (UseStrokeFont) then
            begin
                Designator.UseTTFonts := False;
                Designator.Width := Designator.Size / cTextWidthRatio;
                // Thicker strokes for Overlay text
                if (Designator.Layer = eTopOverlay) or (Designator.Layer = eBottomOverlay) then
                    Designator.Width := Designator.Size / cSilkTextWidthRatio;
            end;

            // Trim down designator if its size is bigger than the MaximumHeight constant
            if Size > MaximumHeight then
            begin
                Size := MaximumHeight;
                Designator.Width := MaximumHeight / cTextWidthRatio;
            end;

            if Size <  MinimumHeight then
                Size := MinimumHeight;

            Designator.Size := Size;

            Designator.SetState_Multiline(true);
            Designator.MultilineTextHeight := 0;
            Designator.MultilineTextWidth  := 0;
            Designator.SetState_Multiline(true);
            Designator.SetState_MultilineTextAutoPosition(eAutoPos_CenterCenter);   // CenterLeft
//            Designator.UpdateTextPosition;

            Designator.SetState_XSizeYSize;
            Designator.GraphicallyInvalidate;
            Component.Name.EndModify;

//     Set the Designator AutoPosition to the center-center but stop comment being moved.
            Component.ChangeCommentAutoposition( eAutoPos_Manual );
            Component.ChangeNameAutoposition(eAutoPos_CenterCenter);
            Component.SetState_NameAutoPos(eAutoPos_CenterCenter);

            if (ModMechText) then
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

                     if not MechDesignator.IsDesignator then
                     if ASetOfLayers.Contains(MechDesignator.Layer) then
                     if ((LowerCase(MechDesignator.GetState_UnderlyingString) = '.designator' ) or (MechDesignator.GetState_ConvertedString = Designator.GetState_ConvertedString)) then
                     begin
// MechDes maybe using Multiline but actual Designator is not.

                         MechDesMultiline      := MechDesignator.Multiline;
                         MechDesMultilineAuto  := MechDesignator.MultilineTextAutoPosition;

                         MechDesignator.SetState_Multiline(true);
                         MechDesignator.SetState_MultilineTextAutoPosition(eAutoPos_CenterCenter);
                         MechDesignator.TTFInvertedTextJustify := Designator.TTFInvertedTextJustify;
                         MechDesignator.Size                   := Designator.Size;
                         MechDesignator.MultilineTextHeight    := 0;
                         MechDesignator.MultilineTextWidth     := 0;
                         MechDesignator.SetState_Multiline(true);

                         MechDesignator.UseTTFonts := Designator.UseTTFonts;
                         MechDesignator.Italic     := Designator.Italic;
                         MechDesignator.Bold       := Designator.Bold;
                         MechDesignator.Inverted   := Designator.Inverted;
                         MechDesignator.FontName   := Designator.FontName;
                         MechDesignator.Rotation   := Designator.Rotation;
                         MechDesRotation := 0;

                         If (UseStrokeFont) then
                         begin
                             MechDesignator.UseTTFonts := False;
                             MechDesignator.Width      := MechDesignator.Size / cTextWidthRatio;
                             // Thicker strokes for Overlay text
                             if (MechDesignator.Layer = eTopOverlay) or (MechDesignator.Layer = eBottomOverlay) then
                                 MechDesignator.Width := MechDesignator.Size / cSilkTextWidthRatio;
                        end;

                        MechDesignator.SetState_XSizeYSize;
                        MechDesignator.MoveToXY(Designator.XLocation, Designator.YLocation);
                        BR  := MechDesignator.BoundingRectangle;
                        BRC := Point(BR.X1 + RectWidth(BR)/2, BR.Y1 + RectHeight(BR)/2 );

                        if (UseMultiLine) then
                            MechDesignator.SetState_Multiline(true)
                        else
                            MechDesignator.SetState_Multiline(MechDesMultiline);

                   // Rotate the designator to increase the readability
                        if Y > X then
                        begin
                            if Designator.Layer = eTopOverlay then
                                MechDesRotation := MechDesRotation + 90
                            else
                                MechDesRotation := MechDesRotation + 270;
                        end;


//                        MechDesRotation := MechDesRotation + Component.Rotation - (90 * (int((Component.Rotation+45) / 90)) -45 );
//                        if (MechDesRotation > 225) and (MechDesRotation <= 315) then MechDesRotation := MechDesRotation - 270;
//                        if (MechDesRotation > 45)  and (MechDesRotation <= 135) then MechDesRotation := MechDesRotation - 90;

                        if (MechDesRotation > 135) and (MechDesRotation <= 225) then MechDesRotation := MechDesRotation - 180;

                        MechDesRotation := MechDesRotation - MechDesignator.Rotation;
                        MechDesignator.RotateAroundXY(BRC.X, BRC.Y, MechDesRotation);

                        MechDesignator.GraphicallyInvalidate;
                     end;

                     MechDesignator := GroupIterator.NextPCBObject;
                end;
                // Destroy the track interator
                Component.GroupIterator_Destroy(GroupIterator);
            end;

//    Having copyied the position etc of Designator, return to original state
            if (not ModOverLayText) then
            begin
                Component.Name.BeginModify;
                Designator.Width      := DesWidth;
                Designator.Size       := DesSize;
                Designator.UseTTFonts := DesUseTTFonts;
                Designator.Italic     := DesItalic;
                Designator.Bold       := DesBold;
                Designator.Inverted   := DesInverted;
                Designator.FontName   := DesFontName;
                Designator.FontID     := DesFontID;
                Designator.Rotation   := DesRotation;
                Component.SetState_NameAutoPos(DesAutoPosition);

                Designator.MultilineTextHeight := DesMultilineTH;
                Designator.MultilineTextWidth  := DesMultilineTW;
                Designator.SetState_Multiline(DesMultiline);
                Designator.SetState_MultilineTextAutoPosition(DesMultilineAuto);

//   size of bounding box effects the move location.
                Designator.SetState_XSizeYSize;
                Designator.MoveToXY(DesXLocation, DesYLocation);

                Designator.GraphicallyInvalidate;
                Component.Name.EndModify;
            end else
            begin
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
            end;
            Component.ChangeCommentAutoposition(DesAutoPosComment);

        end;

        // Restoring designator visibility
        if UnHideDesignators = false then
            Component.NameOn := DesVisibility;

        Component.Comment.EndModify;
        Component.EndModify;
        Component.SetState_XSizeYSize;
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
//    Close;
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

    LayerStack := Board.MasterLayerStack;
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

    FormAdjustDesignators.FormStyle := fsStayOnTop;
    FormAdjustDesignators.Show;
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

//Calculate the height of the true type text to best fit for Microsoft Serif
function CalculateSize (Size : Integer, S : String, UseStrokeFont : boolean) : Integer;
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
    If (UseStrokeFont) then
    begin
        Result := Result * 0.4;    //Scaled Result for Stroked Fonts
    end;
end;
                                                    // cardinal      V7 LayerID
function GetMechLayerObject(LS: IPCB_MasterLayerStack, i : integer, var MLID : TLayer) : IPCB_MechanicalLayer;
begin
    if LegacyMLS then
    begin
        MLID := LayerUtils.MechanicalLayer(i);
        Result := LS.LayerObject_V7(MLID)
    end else
    begin
        Result := LS.GetMechanicalLayer(i);
        MLID := Result.LayerID;
    end;
end;

function GetFirstLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(cSeparator, Pair);
   if Pos <> 0 then
      SetLength(Pair, Pos - 1);

   Result := Pair;
end;

function GetSecondLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(cSeparator, Pair);
   Delete(Pair, 1, Pos + StrLen(cSeparator) - 1);

   Result := Pair;
end;

function Version(const dummy : boolean) : TStringList;
begin
    Result               := TStringList.Create;
    Result.Delimiter     := '.';
    Result.Duplicates    := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;
