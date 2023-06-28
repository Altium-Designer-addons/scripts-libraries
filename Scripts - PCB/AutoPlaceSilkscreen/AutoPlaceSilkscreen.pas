// How to use:
// 1) Run script from pcb layout
// 2) Any unplaced silkscreen will be moved off the board
// 3) A popup message box will appear on completion

// HALT EXECUTION: ctrl + PauseBreak

// TODO:
// - Iterate through all good placement positions, use the one with the lowest x/y --> x2/y2 delta square distance
// - Only allow 2 silk designators close to eachother if they are perpendicular to eachother
// - Option to move unplaced silkscreen on top of components at the end of the script?
// - Add Mechanical Layer options to GUI
// - Use Courtyard layer tracks

uses  // do `uses` actually do anything in Altium's DelphiScript?
    Winapi, ShellApi, Win32.NTDef, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, System, System.Diagnostics;

const
    NEWLINECODE = #13#10;
    TEXTBOXINIT = 'Example:' + NEWLINECODE + 'J3' + NEWLINECODE + 'SH1';
    DEBUGLEVEL = 0; // 0 = no debugging; 1 = Placement failures and passes ; 2 = step-by-step operations; 3 = extended debug info
    DEBUGANIMLEVEL = 1; // 0 = no animation; 1 = finished placements only; 2 = placement attempts
    DEBUGANIMWAIT = 1; // wait in ms between movement steps

var
    iDebugLevel: Integer;
    IsAtLeastAD19: Boolean;
    AllowUnderList: TStringList;
    MechLayerIDList: TStringList;
    NotPlaced: TObjectList;
    Board: IPCB_Board;
    //ContourUtil: IPCB_ContourUtilities; // doesn't work
    ContourMaker: IPCB_ContourMaker2;
    CmpOutlineLayerID: Integer;
    AvoidVias: Boolean;
    DictionaryCache: TStringList;
    TextProperites: TStringList;
    FormCheckListBox1: TCheckListBox;
    SilkscreenPositionDelta: TCoord;
    SilkscreenFixedWidth: TCoord;
    SilkscreenFixedSize: TCoord;
    SilkscreenIsFixedWidth: Boolean;
    SilkscreenIsFixedSize: Boolean;
    TryAlteredRotation: Integer;
    RotationStrategy: Integer;

{ function to transform text justification based on designator autoposition and rotation }
function AutopositionJustify(Silkscreen : IPCB_Text; tc_AutoPos : TTextAutoposition): TTextAutoposition;
var
    rotation : Integer;
    mirror   : Boolean;
    taller   : Boolean;
    rect     : TCoordRect;
    APSet    : TSet;

begin
    // AD19+ supports text justification for designators with AdvanceSnapping property
    if not IsAtLeastAD19 then
    begin
        Result := tc_AutoPos;
        Exit;
    end;

    Silkscreen.AdvanceSnapping := True;

    rotation := Round(Silkscreen.Rotation);
    mirror := false; // x Direction flips on the bottom layer
    if Layer2String(Silkscreen.Component.Layer) = 'Bottom Layer' then
        mirror := true;

    // Top|Bottom Left|Right autopos behaves differently for strings that are height>width
    rect := Silkscreen.BoundingRectangle;
    taller := (rect.Top - rect.Bottom) > (rect.Right - rect.Left);

    APSet := MkSet(eAutoPos_TopLeft, eAutoPos_BottomLeft, eAutoPos_TopRight, eAutoPos_BottomRight);

    if taller and InSet(tc_AutoPos, APSet) then rotation := (rotation + 180) mod 360; // easier to trick rotation than add all the cases below

    if rotation > 315 then rotation := 0;

    // technically AutoPosition being enabled should coerce to 0,90,180, or 270
    case rotation of
        0..45 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterLeft     : Result := eAutoPos_CenterRight;
                    eAutoPos_BottomLeft     : Result := eAutoPos_TopLeft;
                    eAutoPos_TopCenter      : Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_TopCenter;
                    eAutoPos_TopRight       : Result := eAutoPos_BottomRight;
                    eAutoPos_CenterRight    : Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomRight    : Result := eAutoPos_TopRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 0..45 }
        46..135 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_BottomRight;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_TopCenter else Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_TopRight;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_BottomCenter else Result := eAutoPos_TopCenter; //Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_TopLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 46..135 }
        136..225 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : Result := eAutoPos_TopRight;
                    eAutoPos_CenterLeft     : Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomLeft     : Result := eAutoPos_BottomRight;
                    eAutoPos_TopCenter      : Result := eAutoPos_TopCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_BottomCenter;
                    eAutoPos_TopRight       : Result := eAutoPos_TopLeft;
                    eAutoPos_CenterRight    : Result := eAutoPos_CenterRight;
                    eAutoPos_BottomRight    : Result := eAutoPos_BottomLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 136..225 }
        226..315 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_TopLeft;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_BottomCenter else Result := eAutoPos_TopCenter; //Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_TopRight;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_TopCenter else Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_BottomRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 226..315 }
        else Result := tc_AutoPos;
    end; { case rotation }

    // Set text justification according to Autoposition setting
    Silkscreen.TTFInvertedTextJustify := Result;

end; { AutopositionJustify }

function DebugContourInfo(contour : IPCB_Contour) : TStringList;
var
    PointList: TStringList;
    iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    if contour.Count > 0 then
    begin
        PointList.Add('contour points');
        for iPoint := 0 to contour.Count - 1 do
        begin
            PointList.Add(Format('%d:%s,%s', [iPoint,
                    CoordUnitToString(contour.x(iPoint) - Board.XOrigin, Board.DisplayUnit xor 1),
                    CoordUnitToString(contour.y(iPoint) - Board.YOrigin, Board.DisplayUnit xor 1)]));
        end;
    end
    else PointList.Add('Empty or invalid contour');

    Result := PointList;
end;

function DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList;
var
    PointList: TStringList;
    contour: IPCB_Contour;
    iPoly, iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    PointList.Clear;
    if poly.Count > 0 then
    begin
        for iPoly := 0 to poly.Count - 1 do
        begin
            contour := poly.Contour(iPoly);
            PointList.AddStrings(DebugContourInfo(contour))
        end;
    end
    else
    begin
        PointList.Add('Polygon has no contours');
    end;

    Result := PointList;
end;

// May want different Bounding Rectangles depending on the object
function Get_Obj_Rect(Obj: IPCB_ObjectClass): TCoordRect;
var
    Rect: TCoordRect;
    ObjID: Integer;
begin
    ObjID := Obj.ObjectId;
    if ObjID = eBoardObject then
    begin
        Rect := Obj.BoardOutline.BoundingRectangle;
    end
    else if ObjID = eComponentObject then
    begin
        // Rect := Obj.BoundingRectangleNoNameComment;
        Rect := Obj.BoundingRectangleNoNameCommentForSignals;
    end
    else
    begin
        Rect := Obj.BoundingRectangle;
    end;

    result := Rect;
end;

// Get GeometricPolygon using ContourMaker
function Get_Obj_Poly(Obj: IPCB_ObjectClass, Expansion: TCoord = 0): IPCB_GeometricPolygon;
var
    Poly: IPCB_GeometricPolygon;
    ObjID: Integer;
    OldRect: TCoordRect;
    NewContour: IPCB_Contour;
begin
    ObjID := Obj.ObjectId;
    if ObjID = eBoardObject then
    begin
        //Rect := Obj.BoardOutline.BoundingRectangle;
        Poly := Obj.BoardOutline.BoardOutline_GeometricPolygon;
    end
    else if ObjID = eComponentObject then
    begin
        //Rect := Obj.BoundingRectangleNoNameCommentForSignals;
        // TODO: figure out how to create a contour for a component
    end
    else if ObjID = eTrackObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := ContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else if ObjID = eRegionObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := ContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else if ObjID = eTextObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        if Obj.UseTTFonts then Poly := ContourMaker.MakeContour(Obj, Expansion, Obj.Layer)
        else Poly := ContourMaker.MakeContour(Obj, Expansion - (Obj.Width / 2), Obj.Layer);
    end
    else if ObjID = ePadObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := ContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else
    begin
        //Rect := Obj.BoundingRectangle;
        // For uknown types, fall back to a dumb bounding rectangle, but wrapped as IPCB_GeometricPolygon
        OldRect := Obj.BoundingRectangle;
        NewContour := PCBServer.PCBContourFactory;
        // Procedure AddPoint(x : Integer; y : Integer);
        NewContour.AddPoint(OldRect.Left, OldRect.Bottom);
        NewContour.AddPoint(OldRect.Right, OldRect.Bottom);
        NewContour.AddPoint(OldRect.Right, OldRect.Top);
        NewContour.AddPoint(OldRect.Left, OldRect.Top);
        // Function  AddContour(AContour : IPCB_Contour) : IPCB_Contour;
        Poly := PCBServer.PCBGeometricPolygonFactory;
        Poly.AddContour(NewContour);
    end;

    result := Poly;
end;

// Check if object coordinates are outside board edge
function Is_Outside_Board(Obj: IPCB_ObjectClass): Boolean;
var
    BoardRect, Rect: TCoordRect;
begin
    Rect := Get_Obj_Rect(Obj);
    BoardRect := Get_Obj_Rect(Board);

    if (Rect.Left < BoardRect.Left) or (Rect.Right > BoardRect.Right) or (Rect.Bottom < BoardRect.Bottom) or (Rect.Top > BoardRect.Top) then
    begin
        result := True;
        Exit; // return
    end;

    result := False;
end;

// Check if any part of object outline is outside board contour
function Is_Outside_Board_V2(Obj: IPCB_ObjectClass): Boolean;
var
    BoardPoly, SilkPoly: IPCB_GeometricPolygon;
    contour: IPCB_Contour;
    DebugString: WideString;
    CutoutContours: TInterfaceList;
    CutoutRegion: IPCB_Region;
    iter: IPCB_BoardIterator;
    iContour, iPoly, iPoint: Integer;
begin
    if iDebugLevel = 3 then DebugString := Format('Obj: %s', [Obj.Descriptor]);
    if iDebugLevel = 3 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Is_Outside_Board_V2 called' + NEWLINECODE + DebugString) = False then iDebugLevel := 0;
    // Function  PCBServer.PCBContourUtilities.PointInContour(AContour : IPCB_Contour; x : Integer; y : Integer) : Boolean;
    SilkPoly := Get_Obj_Poly(Obj);
    BoardPoly := Get_Obj_Poly(Board);

    if iDebugLevel = 3 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'BoardPoly' + NEWLINECODE +
            DebugGeometricPolygonInfo(BoardPoly).Text) = False then iDebugLevel := 0;

    // use board iterator to find all eRegionObject then further, all eRegionKind_BoardCutout
    CutoutContours := CreateObject(TInterfaceList);
    iter := Board.BoardIterator_Create;
    iter.AddFilter_ObjectSet(MkSet(eRegionObject));
    iter.AddFilter_LayerSet(MkSet(eMultiLayer));
    iter.AddFilter_Method(eProcessAll);
    CutoutRegion := iter.FirstPCBObject;
    while (CutoutRegion <> nil) do
    begin
        if CutoutRegion.Kind = eRegionKind_BoardCutout then
        begin
            // add all contours that are more than 0.1% different than the board outline itself
            if ABS(1 - (CutoutRegion.Area / BoardPoly.Area)) > 0.001 then CutoutContours.Add(CutoutRegion.GetMainContour);
        end;
        CutoutRegion := iter.NextPCBObject;
    end;

    if iDebugLevel = 3 then
        for iContour := 0 to CutoutContours.Count - 1 do
            if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Cutout contours' + NEWLINECODE + DebugContourInfo(CutoutContours[iContour]).Text) = False then iDebugLevel := 0;

    // check each silkscreen contour point is inside board outline's 0th contour.
    // TODO: board outlines and cutouts are handled, but there may be other cases
    if SilkPoly.Count > 0 then
    begin
        for iPoly := 0 to SilkPoly.Count - 1 do
        begin
            contour := SilkPoly.Contour(iPoly);
            for iPoint := 0 to contour.Count - 1 do
            begin
                // check for any point of the silkscreen lying outside of board outline
                if not PCBServer.PCBContourUtilities.PointInContour(BoardPoly.Contour(0), contour.x(iPoint), contour.y(iPoint)) then
                begin
                    result := True;
                    if iDebugLevel >= 1 then DebugString := 'Board outline check' + NEWLINECODE + Format('Obj: %s%sBoard: %s%sPoint Outside Board Contour: %s @ [%s, %s]',
                            [Obj.Descriptor, NEWLINECODE, Board.Identifier, NEWLINECODE, BoolToStr(result xor 1, True),
                            CoordUnitToString(contour.x(iPoint) - Board.XOrigin, Board.DisplayUnit xor 1), CoordUnitToString(contour.y(iPoint) - Board.YOrigin, Board.DisplayUnit xor 1)]);
                    if iDebugLevel >= 1 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', DebugString) = False then iDebugLevel := 0;
                    Exit;
                end
                else
                begin
                    // if point is inside, make sure it is not inside a board cutout
                    for iContour := 0 to CutoutContours.Count - 1 do
                    begin
                        if PCBServer.PCBContourUtilities.PointInContour(CutoutContours[iContour], contour.x(iPoint), contour.y(iPoint)) then
                        begin
                            result := True;
                            if iDebugLevel >= 1 then DebugString := 'Board cutout check' + NEWLINECODE + Format('Obj: %s%sBoard: %s%sPoint Inside Cutout Contour: %s @ [%s, %s]',
                                    [Obj.Descriptor, NEWLINECODE, Board.Identifier, NEWLINECODE, BoolToStr(result, True),
                                    CoordUnitToString(contour.x(iPoint) - Board.XOrigin, Board.DisplayUnit xor 1), CoordUnitToString(contour.y(iPoint) - Board.YOrigin, Board.DisplayUnit xor 1)]);
                            if iDebugLevel >= 1 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', DebugString) = False then iDebugLevel := 0;
                            Exit;
                        end;
                    end;
                end;
            end;
        end;
    end;

    if iDebugLevel >= 2 then DebugString := 'Silkscreen on board check' + NEWLINECODE + Format('Obj: %s%sBoard: %s%sAll points on board.',
            [Obj.Descriptor, NEWLINECODE, Board.Identifier, NEWLINECODE]);
    if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', DebugString) = False then iDebugLevel := 0;

    result := False;
end;

// Check if two layers are the on the same side of the board. Handle different layer names.
function Is_Same_Side(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass): Boolean;
var
    Layer1, Layer2: Integer;
begin
    Layer1 := Obj1.Layer;
    Layer2 := Obj2.Layer;
    if Obj1.ObjectId = eComponentBodyObject then
        Layer1 := Obj1.Component.Layer;
    if Obj2.ObjectId = eComponentBodyObject then
        Layer2 := Obj2.Component.Layer;

    // Top Layer
    if (Layer1 = eTopLayer) or (Layer1 = eTopOverlay) then
    begin
        if (Layer2 <> eBottomLayer) and (Layer2 <> eBottomOverlay) then
        begin
            result := True;
            Exit; // return True
        end;
    end
    // Bottom Layer
    else if (Layer1 = eBottomLayer) or (Layer1 = eBottomOverlay) then
    begin
        if (Layer2 <> eTopLayer) and (Layer2 <> eTopOverlay) then
        begin
            result := True;
            Exit; // return True
        end;
    end
    // Multi Layer
    else if (Layer1 = eMultiLayer) or (Layer2 = eMultiLayer) then
    begin
        result := True;
        Exit;
    end;

    result := False;
end;

// Guess silkscreen size based on component size
function Get_Silk_Size(Slk: IPCB_Text; Min_Size: Integer): Integer;
var
    Rect: TCoordRect;
    area: Integer;
    size: Integer;
begin
    // Stroke Width & Text Height
    Rect := Get_Obj_Rect(Slk.Component);
    area := CoordToMils(Rect.Right - Rect.Left) * CoordToMils(Rect.Top - Rect.Bottom);

    size := Int((82 * area) / (16700 + area));
    if size < Min_Size then
        size := Min_Size;

    result := size;
end;

// Checks if 2 objects are overlapping on the PCB
function Is_Overlapping(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean;
const
    SLKPAD = 40000; // Allowed Overlap = 4 mil
    PADPAD = 10000; // Margin beyond pad = 1 mil
var
    Rect1, Rect2: TCoordRect;
    L, R, T, B: Integer;
    L2, R2, T2, B2: Integer;
    Delta1, Delta2: Integer;
begin
    // Don't compare silkscreen object to itself
    if (Obj1.ObjectId = Obj2.ObjectId) and (Obj1.ObjectId = eTextObject) then
    begin
        if Obj1.IsDesignator and Obj2.IsDesignator then
        begin
            if Obj1.Text = Obj2.Text then
            begin
                result := False;
                Exit; // Continue
            end;
        end;
    end;

    // Continue if Hidden
    if Obj1.IsHidden or Obj2.IsHidden then
    begin
        result := False;
        Exit; // Continue
    end;

    // Continue if Layers Dont Match
    if not Is_Same_Side(Obj1, Obj2) then
    begin
        result := False;
        Exit; // Continue
    end;

    Rect1 := Get_Obj_Rect(Obj1);
    Rect2 := Get_Obj_Rect(Obj2);

    // Neg/Pos padding margins
    Delta1 := 0;
    Delta2 := 0;
    if (Obj1.ObjectId = eTextObject) and (Obj2.ObjectId = eTextObject) and Obj1.IsDesignator then
        Delta1 := -SLKPAD;
    if (Obj1.ObjectId = eTextObject) and (Obj2.ObjectId = eTextObject) and Obj2.IsDesignator then
        Delta2 := -SLKPAD;
    if (Obj1.ObjectId = ePadObject) then
        Delta1 := PADPAD;
    if (Obj2.ObjectId = ePadObject) then
        Delta2 := PADPAD;

    // Get Bounding Area For Both Objects
    L := Rect1.Left - Delta1;
    R := Rect1.Right + Delta1;
    T := Rect1.Top + Delta1;
    B := Rect1.Bottom - Delta1;

    L2 := Rect2.Left - Delta2;
    R2 := Rect2.Right + Delta2;
    T2 := Rect2.Top + Delta2;
    B2 := Rect2.Bottom - Delta2;

    if (B > T2) or (T < B2) or (L > R2) or (R < L2) then
    begin
        result := False;
        Exit; // Equivalent to return in C
    end;
    result := True;
end;

// Checks if 2 objects are overlapping on the PCB
function Is_Overlapping_V2(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean;
const
    SLKPAD = 40000; // Allowed Overlap = 4 mil
    PADPAD = 10000; // Margin beyond pad = 1 mil
    TEXTEXPANSION       = 50000; // [Coords] Expansion for other text objects = 5 mil
    PADEXPANSION        = 70000; // [Coords] Expansion for pads = 7 mil
    DEFAULTEXPANSION    = 40000; // [Coords] Expansion for everything else = 4 mil
var
    Poly1, Poly2: IPCB_GeometricPolygon;
    Expansion: TCoord;
    DebugString: WideString;
    Rect1, Rect2: TCoordRect;
    L, R, T, B: Integer;
    L2, R2, T2, B2: Integer;
    Delta1, Delta2: Integer;
begin
    if iDebugLevel = 3 then DebugString := Format('Obj1: %s%sObj2: %s', [Obj1.Descriptor, NEWLINECODE, Obj2.Descriptor]);
    if iDebugLevel = 3 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Is_Overlapping_V2 called' + NEWLINECODE + DebugString) = False then iDebugLevel := 0;

    // If silkscreen object equals itself, return False
    if Obj1.I_ObjectAddress = Obj2.I_ObjectAddress then
    begin
        result := False;
        Exit; // Continue
    end;

    // Continue if Hidden
    if Obj1.IsHidden or Obj2.IsHidden then
    begin
        result := False;
        Exit; // Continue
    end;

    // Continue if Layers Dont Match
    if not Is_Same_Side(Obj1, Obj2) then
    begin
        result := False;
        Exit; // Continue
    end;

    if Obj2.ObjectId = eTextObject then Expansion := TEXTEXPANSION
    else if Obj2.ObjectId = ePadObject then Expansion := PADEXPANSION
    else Expansion := DEFAULTEXPANSION;

    // Get geometric polygons for both objects
    Poly1 := Get_Obj_Poly(Obj1);    // in practice, Place_Silkscreen always uses Obj1 for silkscreen being manipulated, so don't add expansion to it
    Poly2 := Get_Obj_Poly(Obj2, Expansion);

    if iDebugLevel = 3 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug Poly1', DebugGeometricPolygonInfo(Poly1).Text) = False then iDebugLevel := 0;
    if iDebugLevel = 3 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug Poly2', DebugGeometricPolygonInfo(Poly2).Text) = False then iDebugLevel := 0;

    // IPCB_ContourUtilities Function  GeometricPolygonsTouch(AGeometricPolygon : IPCB_GeometricPolygon; BGeometricPolygon : IPCB_GeometricPolygon) : Boolean;
    result := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(Poly1, Poly2);

    if result or (iDebugLevel >= 2) then
    begin
        if iDebugLevel >= 1 then DebugString := Format('Obj1: %s%sObj2: %s%sGeometricPolygonsTouch: %s', [Obj1.Descriptor, NEWLINECODE, Obj2.Descriptor, NEWLINECODE, BoolToStr(result, True)]);
        if iDebugLevel >= 1 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', DebugString) = False then iDebugLevel := 0;
    end;
end;

// Returns correct layer set given the object being used
function Get_LayerSet(SlkLayer: Integer; ObjID: Integer): PAnsiChar;
var
    TopBot: Integer;
begin
    TopBot := eTopLayer;
    if (Layer2String(SlkLayer) = 'Bottom Overlay') then
        TopBot := eBottomLayer;

    result := MkSet(SlkLayer); // Default layer set
    if (ObjID = eComponentObject) or (ObjID = ePadObject) or (ObjID = eViaObject) then
    begin
        result := MkSet(TopBot, eMultiLayer);
    end
    else if (ObjID = eComponentBodyObject) then
    begin
        result := MkSet(CmpOutlineLayerID);
    end;
end;

function Allow_Under(Cmp: IPCB_Component; AllowUnderList: TStringList): Boolean;
var
    refdes: TPCB_String;
    i: Integer;
begin
    if (AllowUnderList <> nil) and (AllowUnderList.Count > 0) then
    begin
        For i := 0 to AllowUnderList.Count - 1 do
        begin
            refdes := LowerCase(AllowUnderList.Get(i));
            if LowerCase(Cmp.Name.Text) = refdes then
            begin
                result := True;
                Exit;
            end;
        end;
    end;
    result := False;
end;

// Get components for surrounding area
function IsOverObj(Slk: IPCB_Text; ObjID: Integer;
    Filter_Size: Integer): Boolean;
var
    Iterator: IPCB_SpatialIterator;
    Obj: IPCB_ObjectClass;
    Rect: TCoordRect;
    RectL, RectR, RectB, RectT: TCoord;
    RegIter: Boolean; // Regular Iterator
    Name: TPCBString;
begin
    // Spatial Iterators only work with Primitive Objects and not group objects like eComponentObject and dimensions
    if (ObjID = eComponentObject) then
    begin
        Iterator := Board.BoardIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(ObjID));
        Iterator.AddFilter_LayerSet(Get_LayerSet(Slk.Layer, ObjID));
        Iterator.AddFilter_Method(eProcessAll);
        RegIter := True;
    end
    else
    begin
        Rect := Get_Obj_Rect(Slk);
        RectL := Rect.Left - Filter_Size;
        RectR := Rect.Right + Filter_Size;
        RectT := Rect.Top + Filter_Size;
        RectB := Rect.Bottom - Filter_Size;

        Iterator := Board.SpatialIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(ObjID));
        Iterator.AddFilter_LayerSet(Get_LayerSet(Slk.Layer, ObjID));
        Iterator.AddFilter_Area(RectL, RectB, RectR, RectT);
        RegIter := False;
    end;

    // Iterate through components or pads or silkscreen etc. Depends on which object is passed in.
    Obj := Iterator.FirstPCBObject;
    while Obj <> nil do
    begin
        // Ignore Hidden Objects
        if Obj.IsHidden then
        begin
            Obj := Iterator.NextPCBObject;
            Continue;
        end;

        // Convert ComponentBody objects to Component objects
        if Obj.ObjectId = eComponentBodyObject then
        begin
            Obj := Obj.Component;

            // Allow under are user defined reference designators that can be ignored
            if (Obj = nil) or (Allow_Under(Obj, AllowUnderList)) or (Obj.Name.Layer <> Slk.Layer) then
            begin
                Obj := Iterator.NextPCBObject;
                Continue;
            end;
        end;

        try
            // Check if Silkscreen is overlapping with other object (component/pad/silk)
            if Is_Overlapping_V2(Slk, Obj) then
            begin
                result := True;
                Exit; // Equivalent to return in C
            end;
        except
            Name := Slk.Text;
        end;

        Obj := Iterator.NextPCBObject;
    end;

    // Destroy Iterator
    if RegIter then
    begin
        Board.BoardIterator_Destroy(Iterator);
    end
    else
    begin
        Board.SpatialIterator_Destroy(Iterator);
    end;

    result := False;
end;

// Moves silkscreen reference designators to board origin. Used as initialization step.
procedure Move_Silk_Off_Board(OnlySelected: Boolean);
var
    Iterator: IPCB_SpatialIterator;
    Slk: IPCB_Text;
begin
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    Iterator.AddFilter_IPCB_LayerSet(MkSet(eTopOverlay, eBottomOverlay));
    Iterator.AddFilter_Method(eProcessAll);

    // Iterate through silkscreen reference designators.
    Slk := Iterator.FirstPCBObject;
    while Slk <> nil do
    begin
        if Slk.IsDesignator then
        begin
            if (OnlySelected and Slk.Component.Selected) or (not OnlySelected) then
            begin
                TextProperites.Add(Slk.Text + '.Rotation=' + IntToStr(Slk.Rotation));
                TextProperites.Add(Slk.Text + '.Width=' + IntToStr(Slk.Width));
                TextProperites.Add(Slk.Text + '.Size=' + IntToStr(Slk.size));
                if IsAtLeastAD19 then TextProperites.Add(Slk.Text + '.TTFInvertedTextJustify=' + IntToStr(Slk.TTFInvertedTextJustify));
                TextProperites.Add(Slk.Text + '.XLocation=' + IntToStr(Slk.XLocation));
                TextProperites.Add(Slk.Text + '.YLocation=' + IntToStr(Slk.YLocation));

                Slk.BeginModify;
                Slk.MoveToXY(Board.XOrigin - 1000000, Board.YOrigin - 1000000); // Move slightly off board origin
                Slk.EndModify;
            end;
        end;

        Slk := Iterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iterator);
end;

procedure Move_Silk_Over_Comp(SlkList: TObjectList);
var
    Slk: IPCB_Text;
    i: Integer;
begin
    For i := 0 to SlkList.Count - 1 do
    begin
        Slk := SlkList[i];

        Slk.BeginModify;
        Slk.Component.ChangeNameAutoposition := eAutoPos_CenterCenter;
        AutopositionJustify(Slk, eAutoPos_CenterCenter);
        Slk.EndModify;
    end;
end;

procedure Restore_Comp(SlkList: TObjectList);
var
    Slk: IPCB_Text;
    i: Integer;
    _Index: Integer;
    X, Y: Integer;
begin
    For i := 0 to SlkList.Count - 1 do
    begin
        Slk := SlkList[i];

        Slk.BeginModify;

        _Index := TextProperites.IndexOfName(Slk.Text + '.Rotation');
        Slk.Rotation := TextProperites.ValueFromIndex[_Index];

        _Index := TextProperites.IndexOfName(Slk.Text + '.Width');
        Slk.Width := TextProperites.ValueFromIndex[_Index];

        _Index := TextProperites.IndexOfName(Slk.Text + '.Size');
        Slk.size := TextProperites.ValueFromIndex[_Index];

        Slk.EndModify;
        Slk.BeginModify;

        if IsAtLeastAD19 then
        begin
            _Index := TextProperites.IndexOfName(Slk.Text + '.TTFInvertedTextJustify');
            Slk.TTFInvertedTextJustify := TextProperites.ValueFromIndex[_Index];
        end;

        Slk.EndModify;
        Slk.BeginModify;

        _Index := TextProperites.IndexOfName(Slk.Text + '.XLocation');
        X := TextProperites.ValueFromIndex[_Index];

        _Index := TextProperites.IndexOfName(Slk.Text + '.YLocation');
        Y := TextProperites.ValueFromIndex[_Index];

        Slk.MoveToXY(X, Y);

        Slk.EndModify;
    end;
end;

function GetNextAutoPosition(iteration: Integer): Integer;
begin
    Case iteration of
        0:  result := eAutoPos_CenterRight;
        1:  result := eAutoPos_TopCenter;
        2:  result := eAutoPos_CenterLeft;
        3:  result := eAutoPos_BottomCenter;
        4:  result := eAutoPos_TopLeft;
        5:  result := eAutoPos_TopRight;
        6:  result := eAutoPos_BottomLeft;
        7:  result := eAutoPos_BottomRight;
        8:  result := eAutoPos_Manual;
    else
        result := eAutoPos_Manual;
    end;
end;

function StrFromRotationStrategy(RotationStrategy : Integer): String;
begin
    case RotationStrategy of
        0:  result := 'Component Rotation';
        1:  result := 'Horizontal Rotation';
        2:  result := 'Vertical Rotation';
        3:  result := 'Along Side';
        4:  result := 'Along Axis';
        5:  result := 'Along Pins';
        6:  result := 'KLC Style';
    else
        result := 'Invalid';
end;

function StrFromAutoPos(eAutoPos: TTextAutoposition): String;
begin
    { TTextAutoposition = ( eAutoPos_Manual,
                        eAutoPos_TopLeft,
                        eAutoPos_CenterLeft,
                        eAutoPos_BottomLeft,
                        eAutoPos_TopCenter,
                        eAutoPos_CenterCenter,
                        eAutoPos_BottomCenter,
                        eAutoPos_TopRight,
                        eAutoPos_CenterRight,
                        eAutoPos_BottomRight);}
    case eAutoPos of
        0:  result := 'eAutoPos_Manual';
        1:  result := 'eAutoPos_TopLeft';
        2:  result := 'eAutoPos_CenterLeft';
        3:  result := 'eAutoPos_BottomLeft';
        4:  result := 'eAutoPos_TopCenter';
        5:  result := 'eAutoPos_CenterCenter';
        6:  result := 'eAutoPos_BottomCenter';
        7:  result := 'eAutoPos_TopRight';
        8:  result := 'eAutoPos_CenterRight';
        9:  result := 'eAutoPos_BottomRight';
    else
        result := 'Invalid';
    end;
end;


function StrToAutoPos(iteration: String): Integer;
begin
    Case iteration of
        'CenterRight':  result := eAutoPos_CenterRight;
        'TopCenter':    result := eAutoPos_TopCenter;
        'CenterLeft':   result := eAutoPos_CenterLeft;
        'BottomCenter': result := eAutoPos_BottomCenter;
        'TopLeft':      result := eAutoPos_TopLeft;
        'TopRight':     result := eAutoPos_TopRight;
        'BottomLeft':   result := eAutoPos_BottomLeft;
        'BottomRight':  result := eAutoPos_BottomRight;
    else
        result := -1;
    end;
end;

procedure AutoPosDeltaAdjust(autoPos: Integer; X_offset: Integer; Y_offset: Integer; Silk: IPCB_Text; Layer: TPCBString);
var
    dx, dy, d: Integer;
    XOrigin, YOrigin: Integer;
    flipx: Integer;
    R: Integer;
begin
    d := SilkscreenPositionDelta;
    dx := 0;
    dy := 0;
    R := Silk.Rotation;
    flipx := 1; // x Direction flips on the bottom layer
    if Layer = 'Bottom Layer' then
        flipx := -1;

    Case autoPos of
        eAutoPos_CenterRight:
            dx := -d * flipx;
        eAutoPos_TopCenter:
            dy := -d;
        eAutoPos_CenterLeft:
            dx := d * flipx;
        eAutoPos_BottomCenter:
            dy := d;
        eAutoPos_TopLeft:
            dy := -d;
        eAutoPos_TopRight:
            dy := -d;
        eAutoPos_BottomLeft:
            dy := d;
        eAutoPos_BottomRight:
            dy := d;
    end;

    if (R = 90) or (R = 270) then
    begin
        if (autoPos = eAutoPos_TopLeft) or (autoPos = eAutoPos_BottomLeft) then
        begin
            dx := d * flipx;
        end
        else if (autoPos = eAutoPos_TopRight) or (autoPos = eAutoPos_BottomRight) then
        begin
            dx := -d * flipx;
        end;
    end;
    Silk.MoveByXY(dx + MilsToCoord(X_offset), dy + MilsToCoord(Y_offset));
end;

function MirrorBottomRotation(Text: IPCB_Text; Rotation: TAngle): TAngle;
begin
    result := Rotation;
    if Text.Layer = eBottomOverlay then
        result := 360 - Rotation;
end;

procedure Rotation_MatchSilk2Comp(Silk: IPCB_Text);
var
    R: Integer; // Component Rotation
begin
    R := Silk.Component.Rotation;

    if (R = 0) or (R = 180) or (R = 360) then
    begin
        Silk.Rotation := MirrorBottomRotation(Silk, 0);
    end
    else if (R = 90) or (R = 270) then
    begin
        Silk.Rotation := MirrorBottomRotation(Silk, 90);
    end;
end;

function CalculateHor(Component: IPCB_Component): Integer;
var
    CompIterator: IPCB_GroupIterator;
    Primitive: IPCB_Primitive;
    Pad: IPCB_Pad2;
    Text: IPCB_Text;
    OldRotation: Float;
    DictionaryX: TStringList;
    DictionaryY: TStringList;
    Line: String;
    Location: String;
    Number: String;
    i: Integer;
    iDict: Integer;
    Num: Integer;
    MaxX: Integer;
    MaxY: Integer;
    Rectangle: TCoordRect;
    BoundRect: TCoordRect;
    X1, Y1, X2, Y2: Integer;
    X, Y: Integer;
    Temp: Integer;
    PadX: Integer;
    PadY: Integer;
    PadMaxX: Integer;
    PadMinX: Integer;
    PadMaxY: Integer;
    PadMinY: Integer;
begin
    iDict := DictionaryCache.IndexOfName(Component.Pattern);
    if iDict <> -1 then
    begin
        result := DictionaryCache.ValueFromIndex[iDict];
        Exit;
    end;

    OldRotation := Component.Rotation;

    Component.BeginModify;
    Component.Rotation := 0;
    Component.EndModify;

    CompIterator := Component.GroupIterator_Create;
    CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    DictionaryX := CreateObject(TStringList);
    DictionaryY := CreateObject(TStringList);

    DictionaryX.NameValueSeparator := '=';
    DictionaryY.NameValueSeparator := '=';

    MaxX := 1;
    MaxY := 1;

    Pad := CompIterator.FirstPCBObject;

    while (Pad <> nil) do
    begin
        // Non-ideal
        PadX := IntToStr(Trunc(CoordToMMs(Pad.X) * 100));
        PadY := IntToStr(Trunc(CoordToMMs(Pad.Y) * 100));

        if DictionaryX.Count = 0 then
        begin
            PadMinX := Pad.X;
            PadMaxX := Pad.X;
            PadMinY := Pad.Y;
            PadMaxY := Pad.Y;
        end;

        if PadMinX > Pad.X then
            PadMinX := Pad.X;
        if PadMaxX < Pad.X then
            PadMaxX := Pad.X;
        if PadMinY > Pad.Y then
            PadMinY := Pad.Y;
        if PadMaxY < Pad.Y then
            PadMaxY := Pad.Y;

        iDict := DictionaryX.IndexOfName(PadX);

        if iDict = -1 then
            DictionaryX.Add(PadX + '=1')
        else
        begin
            Number := DictionaryX.ValueFromIndex[iDict];
            Num := StrToInt(Number) + 1;

            if Num > MaxX then
                MaxX := Num;

            Number := IntToStr(Num);
            DictionaryX.Put(iDict, PadX + '=' + Number);
        end;

        iDict := DictionaryY.IndexOfName(PadY);

        if iDict = -1 then
            DictionaryY.Add(PadY + '=1')
        else
        begin
            Number := DictionaryY.ValueFromIndex[iDict];
            Num := StrToInt(Number) + 1;

            if Num > MaxY then
                MaxY := Num;

            Number := IntToStr(Num);
            DictionaryY.Put(iDict, PadY + '=' + Number);
        end;

        Pad := CompIterator.NextPCBObject;
    end;
    Component.GroupIterator_Destroy(CompIterator);

    Component.BeginModify;
    Component.Rotation := OldRotation;
    Component.EndModify;

    if MaxY > MaxX then // more pads with a common Y-value
    begin
        // This is Horizontal component
        result := 1;
    end
    else if MaxY < MaxX then // more pads with a common X-value
    begin
        // This is Vertical component
        result := 0;
    end
    else
    begin
        if (PadMaxX - PadMinX) >= (PadMaxY - PadMinY) then
            result := 1
        else
            result := 0;
    end;
    DictionaryCache.Add(Component.Pattern + '=' + IntToStr(result));
end;

function CalculateHor2(Component: IPCB_Component): Integer;
var
    CompIterator: IPCB_GroupIterator;
    Primitive: IPCB_Primitive;
    Pad: IPCB_Pad2;
    Text: IPCB_Text;
    OldRotation: Float;
    DictionaryX: TStringList;
    DictionaryY: TStringList;
    Line: String;
    Location: String;
    Number: String;
    i: Integer;
    iDict: Integer;
    Num: Integer;
    MaxX: Integer;
    MaxY: Integer;
    Rectangle: TCoordRect;
    BoundRect: TCoordRect;
    X1, Y1, X2, Y2: Integer;
    X, Y: Integer;
    Temp: Integer;
    PadX: Integer;
    PadY: Integer;
    PadMaxX: Integer;
    PadMinX: Integer;
    PadMaxY: Integer;
    PadMinY: Integer;
    Pad1X: Integer;
    Pad1Y: Integer;
    EPS: Integer;
    Q1, Q2, Q3, Q4: Integer;
    Count: Integer;
begin
    iDict := DictionaryCache.IndexOfName(Component.Pattern);
    if iDict <> -1 then
    begin
        result := DictionaryCache.ValueFromIndex[iDict];
        Exit;
    end;

    OldRotation := Component.Rotation;

    Component.BeginModify;
    Component.Rotation := 0;
    Component.EndModify;

    CompIterator := Component.GroupIterator_Create;
    CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    DictionaryX := CreateObject(TStringList);
    DictionaryY := CreateObject(TStringList);

    DictionaryX.NameValueSeparator := '=';
    DictionaryY.NameValueSeparator := '=';

    MaxX := 1;
    MaxY := 1;

    Count := 0;
    Pad := CompIterator.FirstPCBObject;

    while (Pad <> nil) do
    begin
        if (Pad.Name = '1') then
        begin
            Pad1X := Pad.X;
            Pad1Y := Pad.Y;
        end;

        Count := Count + 1;
        Pad := CompIterator.NextPCBObject;
    end;

    Pad := CompIterator.FirstPCBObject;
    Q1 := 0;
    Q2 := 0;
    Q3 := 0;
    Q4 := 0;

    EPS := MMsToCoord(0.01);
    while (Pad <> nil) do
    begin
        if (Pad.Name <> '1') then
        begin
            if (Pad.X - Pad1X < EPS) and (Pad.Y - Pad1Y > -EPS) then
                Q1 := Q1 + 1;
            if (Pad.X - Pad1X > -EPS) and (Pad.Y - Pad1Y > -EPS) then
                Q2 := Q2 + 1;
            if (Pad.X - Pad1X > -EPS) and (Pad.Y - Pad1Y < EPS) then
                Q3 := Q3 + 1;
            if (Pad.X - Pad1X < EPS) and (Pad.Y - Pad1Y < EPS) then
                Q4 := Q4 + 1;
        end;

        // Non-ideal
        PadX := IntToStr(Trunc(CoordToMMs(Pad.X) * 100));
        PadY := IntToStr(Trunc(CoordToMMs(Pad.Y) * 100));

        if DictionaryX.Count = 0 then
        begin
            PadMinX := Pad.X;
            PadMaxX := Pad.X;
            PadMinY := Pad.Y;
            PadMaxY := Pad.Y;
        end;

        if PadMinX > Pad.X then
            PadMinX := Pad.X;
        if PadMaxX < Pad.X then
            PadMaxX := Pad.X;
        if PadMinY > Pad.Y then
            PadMinY := Pad.Y;
        if PadMaxY < Pad.Y then
            PadMaxY := Pad.Y;

        iDict := DictionaryX.IndexOfName(PadX);

        if iDict = -1 then
            DictionaryX.Add(PadX + '=1')
        else
        begin
            Number := DictionaryX.ValueFromIndex[iDict];
            Num := StrToInt(Number) + 1;

            if Num > MaxX then
                MaxX := Num;

            Number := IntToStr(Num);
            DictionaryX.Put(iDict, PadX + '=' + Number);
        end;

        iDict := DictionaryY.IndexOfName(PadY);

        if iDict = -1 then
            DictionaryY.Add(PadY + '=1')
        else
        begin
            Number := DictionaryY.ValueFromIndex[iDict];
            Num := StrToInt(Number) + 1;

            if Num > MaxY then
                MaxY := Num;

            Number := IntToStr(Num);
            DictionaryY.Put(iDict, PadY + '=' + Number);
        end;

        Pad := CompIterator.NextPCBObject;
    end;
    Component.GroupIterator_Destroy(CompIterator);

    Component.BeginModify;
    Component.Rotation := OldRotation;
    Component.EndModify;

    if (Q1 = 0) and (Q2 > 0) and (Q3 > 0) and (Q4 >= 0) then
    begin
        result := 1;
    end;
    if (Q2 = 0) and (Q1 >= 0) and (Q3 > 0) and (Q4 > 0) then
    begin
        result := 0;
    end;
    if (Q3 = 0) and (Q1 > 0) and (Q2 >= 0) and (Q4 > 0) then
    begin
        result := 1;
    end;
    if (Q4 = 0) and (Q1 > 0) and (Q2 > 0) and (Q3 >= 0) then
    begin
        result := 0;
    end;

    {
        if (Q1 = 0) and (Q2 = 0) and (Q3 > 0) and (Q4 = 0) Then
        begin
        Result := 1;
        end;
        if (Q1 = 0) and (Q2 = 0) and (Q3 = 0) and (Q4 > 0) Then
        begin
        Result := 1;
        end;
    }

    if (Q1 < Q4) and (Q2 < Q3) and (Q1 > 0) and (Q2 > 0) then
    begin
        result := 1;
    end;
    if (Q2 < Q1) and (Q3 < Q4) and (Q2 > 0) and (Q3 > 0) then
    begin
        result := 0;
    end;
    if (Q3 < Q2) and (Q4 < Q1) and (Q3 > 0) and (Q4 > 0) then
    begin
        result := 1;
    end;
    if (Q4 < Q3) and (Q1 < Q2) and (Q4 > 0) and (Q1 > 0) then
    begin
        result := 0;
    end;

    if (Count = 2) then
    begin
        if (PadMaxX - PadMinX) > (PadMaxY - PadMinY) then
            result := 1
        else
            result := 0;
    end;
    {
        if MaxY > MaxX then
        begin
        // This is Horizontal component
        Result := 1;
        end
        else if MaxY < MaxX then
        begin
        // This is Vertical component
        Result := 0;
        end
        else
        begin
        If (PadMaxX-PadMinX)>(PadMaxY-PadMinY) Then
        Result := 1
        Else
        Result := 0;
        end; }
    DictionaryCache.Add(Component.Pattern + '=' + IntToStr(result));
end;

procedure Rotation_Silk(Silk: IPCB_Text; SilkscreenHor: Integer; NameAutoPosition: Integer);
var
    R: Integer; // Component Rotation
begin
    Case RotationStrategy of
        0:  // 'Component Rotation'
            begin
                if (Silk.Component.Rotation = 0) or (Silk.Component.Rotation = 180) or (Silk.Component.Rotation = 360) then
                    Silk.Rotation := MirrorBottomRotation(Silk, 0)
                else if (Silk.Component.Rotation = 90) or (Silk.Component.Rotation = 270) then
                    Silk.Rotation := MirrorBottomRotation(Silk, 90)
                else Silk.Rotation := MirrorBottomRotation(Silk, Silk.Component.Rotation);
            end;
        1:  // 'Horizontal Rotation'
            begin
                Silk.Rotation := MirrorBottomRotation(Silk, 0);
            end;
        2:  // 'Vertical Rotation'
            begin
                Silk.Rotation := MirrorBottomRotation(Silk, 90);
            end;
        3:  // 'Along Side'
            begin
                Case NameAutoPosition of
                    eAutoPos_CenterRight:
                        Silk.Rotation := MirrorBottomRotation(Silk, 90);
                    eAutoPos_TopCenter:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                    eAutoPos_CenterLeft:
                        Silk.Rotation := MirrorBottomRotation(Silk, 90);
                    eAutoPos_BottomCenter:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                    eAutoPos_TopLeft:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                    eAutoPos_TopRight:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                    eAutoPos_BottomLeft:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                    eAutoPos_BottomRight:
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                end;
            end;
        4:  // 'Along Axis'
            begin
                if (Silk.Component.BoundingRectangle.Right - Silk.Component.BoundingRectangle.Left) > (Silk.Component.BoundingRectangle.Top - Silk.Component.BoundingRectangle.Bottom) then
                    Silk.Rotation := MirrorBottomRotation(Silk, 0)
                else
                    Silk.Rotation := MirrorBottomRotation(Silk, 90);
            end;
        5:  // 'Along Pins'
            begin
                if (SilkscreenHor = 1) then
                begin
                    if (Silk.Component.Rotation = 0) or (Silk.Component.Rotation = 180) or (Silk.Component.Rotation = 360) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 0)
                    else if (Silk.Component.Rotation = 90) or (Silk.Component.Rotation = 270) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 90);
                end
                else
                begin
                    if (Silk.Component.Rotation = 0) or (Silk.Component.Rotation = 180) or (Silk.Component.Rotation = 360) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 90)
                    else if (Silk.Component.Rotation = 90) or (Silk.Component.Rotation = 270) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                end;
            end;
        6:  // 'KLC Style')
            begin
                if (SilkscreenHor = 1) then
                begin
                    if (Silk.Component.Rotation = 0) or (Silk.Component.Rotation = 180) or (Silk.Component.Rotation = 360) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 0)
                    else if (Silk.Component.Rotation = 90) or (Silk.Component.Rotation = 270) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 90);
                end
                else
                begin
                    if (Silk.Component.Rotation = 0) or (Silk.Component.Rotation = 180) or (Silk.Component.Rotation = 360) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 90)
                    else if (Silk.Component.Rotation = 90) or (Silk.Component.Rotation = 270) then
                        Silk.Rotation := MirrorBottomRotation(Silk, 0);
                end;
            end;

    end;
end;

function Get_Iterator_Count(Iterator: IPCB_BoardIterator): Integer;
var
    cnt: Integer;
    Cmp: IPCB_Component;
begin
    cnt := 0;

    Cmp := Iterator.FirstPCBObject;
    while Cmp <> nil do
    begin
        Inc(cnt);
        Cmp := Iterator.NextPCBObject;
    end;
    result := cnt;
end;

{ normalizes IPCB_Text object to be right-reading }
function NormalizeText(var Text : IPCB_Text) : Boolean;
var
    Angle: Double;
    OldJustify, NewJustify: TTextAutoposition;
begin
    // AD19+ functions used for normalization
    if not IsAtLeastAD19 then
    begin
        Result := False;
        Exit;
    end;

    Result := False;

    // coerce angle to 0 ~ 360
    Angle := (Text.Rotation mod 360 + 360) mod 360; // technically an integer operation. TODO: make proper floating point mod throughout

    //if Text.Layer = eBottomOverlay then
        //result := 360 - Rotation;

    // rotate text to match Angle, based on how mirrored text reads from the flipside of the board
    if Text.Layer = eBottomOverlay then
    begin
        // mirrored text should be rotated to match layer flip behavior of text vs components
        if (Angle >= 90) and (Angle < 270) then Angle := (Angle + 180) mod 360
    end
    else
    begin
        // slightly different behavior at 90 and 270 for top side
        if (Angle > 90) and (Angle <= 270) then Angle := (Angle + 180) mod 360
    end;

    if Text.Rotation <> Angle then
    begin
        OldJustify := Text.TTFInvertedTextJustify;
        if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Normalizing Text Rotation' +
                NEWLINECODE + 'Text.Rotation: ' + FloatToStr(Text.Rotation) +
                NEWLINECODE + 'Target Angle: ' + FloatToStr(Angle) +
                NEWLINECODE + 'OldJustify: ' + IntToStr(OldJustify)) = False then iDebugLevel := 0;

        // need to transform justification setting
        case OldJustify of
            eAutoPos_TopLeft        : NewJustify := eAutoPos_BottomRight;
            eAutoPos_CenterLeft     : NewJustify := eAutoPos_CenterRight;
            eAutoPos_BottomLeft     : NewJustify := eAutoPos_TopRight;
            eAutoPos_TopCenter      : NewJustify := eAutoPos_BottomCenter;
            eAutoPos_BottomCenter   : NewJustify := eAutoPos_TopCenter;
            eAutoPos_TopRight       : NewJustify := eAutoPos_BottomLeft;
            eAutoPos_CenterRight    : NewJustify := eAutoPos_CenterLeft;
            eAutoPos_BottomRight    : NewJustify := eAutoPos_TopLeft;
            else                      NewJustify := OldJustify;
        end;

        Text.BeginModify;
        Text.TTFInvertedTextJustify := eAutoPos_CenterCenter; // uses center justification to rotate in place
        Text.Rotation := Angle;
        // need to EndModify and BeginModify here to refresh internal Text.Rotation cache, else changing justification will move text
        Text.EndModify;
        Text.BeginModify;
        Text.TTFInvertedTextJustify := NewJustify;
        Text.EndModify;
        Result := True;
    end
    else Result := False;

end;

function Place_Silkscreen(Silkscreen: IPCB_Text): Boolean;
const
    OFFSET_DELTA = 5; // [mils] Silkscreen placement will move the position around by this delta
    OFFSET_CNT = 6; // Number of attempts to offset position in x or y directions
    MIN_SILK_SIZE = 30; // [mils]
    ABS_MIN_SILK_SIZE = 25; // [mils]
    SILK_SIZE_DELTA = 5; // [mils] Decrement silkscreen size by this value if not placed
    FILTER_SIZE_MILS = 7; // [mils] Additional delta to check surrounding objects. Adds delta to object rectangle.
var
    NextAutoP: Integer;
    Placed: Boolean;
    xinc, yinc, xoff, yoff: Integer;
    SlkSize: Integer;
    FilterSize: Integer;
    Count, i: Integer;
    SilkscreenHor: Integer;
    DisplayUnit: TUnit;
    Coord: TCoord;
    AlteredRotation: Integer;
    StepList: TStringList;
    TempRotation: Double;
    TempRotated: Boolean;
    poly1, poly2: IPCB_GeometricPolygon;
    contour1, contour2: IPCB_Contour;
    iContour, iPoints: Integer;
    PointList: TStringList;
begin
    iDebugLevel := DEBUGLEVEL;

    StepList := CreateObject(TStringList);
    StepList.Delimiter := '|';
    result := True;
    Placed := False;

    DisplayUnit := Board.DisplayUnit;

    // Skip hidden silkscreen
    if Silkscreen.IsHidden then
    begin
        result := True;
        Exit;
    end;

    FilterSize := MilsToCoord(FILTER_SIZE_MILS);

    if RotationStrategy = 5 then
        SilkscreenHor := CalculateHor(Silkscreen.Component)
    else if RotationStrategy = 6 then
        SilkscreenHor := CalculateHor2(Silkscreen.Component)
    else
        SilkscreenHor := -1;

    For AlteredRotation := 0 to TryAlteredRotation do
    begin
        Silkscreen.BeginModify;

        // Get Silkscreen Size
        SlkSize := Get_Silk_Size(Silkscreen, MIN_SILK_SIZE);
        if SilkscreenIsFixedSize then
            Silkscreen.size := SilkscreenFixedSize
        else
            Silkscreen.size := MilsToCoord(SlkSize);
        if SilkscreenIsFixedWidth then
            Silkscreen.Width := SilkscreenFixedWidth
        else
            Silkscreen.Width := 2 * (Silkscreen.size / 10);

        Silkscreen.EndModify;

        // If not placed, reduce silkscreen size
        while (CoordToMils(Silkscreen.size) >= ABS_MIN_SILK_SIZE) or (SilkscreenIsFixedSize) do
        begin
            xoff := 0;
            // If not placed, increment x offset
            For xinc := 0 to OFFSET_CNT do
            begin
                if xinc > 0 then StepList.Add(NEWLINECODE);
                //StepList.Add(NEWLINECODE);
                yoff := 0;
                // If not placed, increment y offset
                For yinc := 0 to OFFSET_CNT do
                begin
                    //if yinc = OFFSET_CNT then StepList.Add(NEWLINECODE);
                    //StepList.Add(NEWLINECODE);
                    // If not placed Change Autoposition on Silkscreen
                    For i := 0 to FormCheckListBox1.Items.Count - 1 do
                    begin
                        if not FormCheckListBox1.Checked[i] then
                            Continue;

                        NextAutoP := StrToAutoPos(FormCheckListBox1.Items[i]);

                        Silkscreen.BeginModify;

                        if (Round(Silkscreen.Component.Rotation) mod 90 <> 0) then
                        begin
                            TempRotation := Silkscreen.Component.Rotation;
                            TempRotated := True;

                            SilkScreen.Component.Rotation := 0;
                            if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Temp-rotated') = False then iDebugLevel := 0;
                        end;

                        if iDebugLevel >= 1 then Silkscreen.Selected := True;

                        if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'SilkscreenHor: ' + IntToStr(SilkscreenHor) +
                                NEWLINECODE + 'RotationStrategy: ' + StrFromRotationStrategy(RotationStrategy)) = False then iDebugLevel := 0;

                        Rotation_Silk(Silkscreen, SilkscreenHor, NextAutoP);
                        if AlteredRotation = 1 then
                            Silkscreen.Rotation := 90 - Silkscreen.Rotation;

                        // need to EndModify and BeginModify here to refresh internal Text properties before changing Autoposition
                        Silkscreen.EndModify;
                        Silkscreen.BeginModify;

                        Silkscreen.Component.ChangeNameAutoposition := NextAutoP;
                        if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Autopositioned: ' + StrFromAutoPos(NextAutoP)) = False then iDebugLevel := 0;

                        // need to EndModify and BeginModify here to refresh internal Text properties, else changing justification may move text
                        Silkscreen.EndModify;
                        Silkscreen.BeginModify;

                        AutopositionJustify(Silkscreen, NextAutoP); // for AD19+, update text justification anchor to component side

                        AutoPosDeltaAdjust(NextAutoP, xoff * OFFSET_DELTA, yoff * OFFSET_DELTA, Silkscreen, Layer2String(Silkscreen.Component.Layer));
                        StepList.Add(Format('%d,%d', [xoff,yoff]));

                        Silkscreen.Component.ChangeNameAutoposition := eAutoPos_Manual;

                        if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'Adjusted Position' + NEWLINECODE +
                                'SilkscreenPositionDelta: ' + CoordUnitToString(SilkscreenPositionDelta, Board.DisplayUnit xor 1) + NEWLINECODE +
                                'X_offset: ' + CoordUnitToString(MilsToCoord(xoff * OFFSET_DELTA), Board.DisplayUnit xor 1) + NEWLINECODE +
                                'Y_offset: ' + CoordUnitToString(MilsToCoord(yoff * OFFSET_DELTA), Board.DisplayUnit xor 1)) = False then iDebugLevel := 0;

                        // TODO: Could do all clearance checks to parent component at this point, when bounding boxes may be more aligned
                        // TODO: in that case, you would ignore objects in host component after de-rotation

                        if TempRotated then
                        begin
                            Silkscreen.Component.Rotation := TempRotation;
                            TempRotated := False;
                            if iDebugLevel >= 2 then if ConfirmOKCancelWithCaption('Confirm or Cancel Debug', 'De-rotated') = False then iDebugLevel := 0;
                        end;

                        Silkscreen.EndModify;

                        if DEBUGANIMLEVEL = 2 then
                        begin
                            Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
                            Application.ProcessMessages;
                            Sleep(DEBUGANIMWAIT);
                        end;

                        // Silkscreen RefDes Overlap Detection
                        if IsOverObj(Silkscreen, eTextObject, FilterSize) then
                        begin
                            Continue;
                        end
                        // Silkscreen Tracks Overlap Detection
                        else if IsOverObj(Silkscreen, eTrackObject, FilterSize) then
                        begin
                            Continue;
                        end
                        // Silkscreen Regions Overlap Detection
                        else if IsOverObj(Silkscreen, eRegionObject, FilterSize) then
                        begin
                            Continue;
                        end
                        // Silkscreen Fills Overlap Detection
                        else if IsOverObj(Silkscreen, eFillObject, FilterSize) then
                        begin
                            Continue;
                        end
                        // same-side pad overlap detection
                        else if IsOverObj(Silkscreen, ePadObject, FilterSize) then
                        begin
                            Continue;
                        end
                        // Outside Board Edge
                        else if Is_Outside_Board_V2(Silkscreen) then
                        begin
                            Continue;
                        end
                        // Component Overlap Detection
                        else if IsOverObj(Silkscreen, eComponentBodyObject, FilterSize) then
                        begin
                            Continue;
                        end
                        else if (AvoidVias) and (IsOverObj(Silkscreen, eViaObject, FilterSize)) then
                        begin
                            Continue;
                        end
                        // PLACED
                        else
                        begin
                            Placed := True;

                            Silkscreen.GraphicallyInvalidate;
                            NormalizeText(Silkscreen);
                            if DEBUGANIMLEVEL = 1 then
                            begin
                                Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
                                Application.ProcessMessages;
                                Sleep(DEBUGANIMWAIT);
                            end;
                            if iDebugLevel >= 1 then ShowInfo(Silkscreen.Text + ' PLACED' + NEWLINECODE+ 'Offsets attempted:' + NEWLINECODE + StepList.DelimitedText);

                            if Silkscreen.Selected = True then Silkscreen.Selected := False;
                            Exit;
                        end;
                    end;

                    yoff := yoff * -1; // Toggle sign
                    if yoff >= 0 then
                        yoff := yoff + 1; // Toggle increment
                end;

                xoff := xoff * -1; // Toggle sign
                if xoff >= 0 then
                    xoff := xoff + 1; // Toggle increment
            end;

            if Placed or ((CoordToMils(Silkscreen.size) - SILK_SIZE_DELTA) < ABS_MIN_SILK_SIZE) then
            begin
                Break;
            end;

            if SilkscreenIsFixedSize then
            begin
                Break;
            end;

            Silkscreen.BeginModify;

            // No placement found, try reducing silkscreen size
            Silkscreen.size := Silkscreen.size - MilsToCoord(SILK_SIZE_DELTA);
            if SilkscreenIsFixedWidth then
                Silkscreen.Width := SilkscreenFixedWidth
            else
                Silkscreen.Width := Int(2 * (Silkscreen.size / 10) - 10000); // Width needs to change relative to size

            Silkscreen.EndModify;
        end;
    end;

    if not Placed then
    begin
        if iDebugLevel >= 2 then ShowInfo(StepList.DelimitedText);
        result := False;

        Silkscreen.BeginModify;

        // Reset Silkscreen Size
        SlkSize := Get_Silk_Size(Silkscreen, MIN_SILK_SIZE);
        if SilkscreenIsFixedSize then
            Silkscreen.size := SilkscreenFixedSize
        else
            Silkscreen.size := MilsToCoord(SlkSize);
        if SilkscreenIsFixedWidth then
            Silkscreen.Width := SilkscreenFixedWidth
        else
            Silkscreen.Width := 2 * (Silkscreen.size / 10);

        // Move off board for now
        Rotation_MatchSilk2Comp(Silkscreen);

        Silkscreen.EndModify;
        Silkscreen.BeginModify;

        Silkscreen.Component.ChangeNameAutoposition := eAutoPos_Manual;

        // need to EndModify and BeginModify here to refresh internal Text properties, else changing justification may move text
        Silkscreen.EndModify;
        Silkscreen.BeginModify;

        AutopositionJustify(Silkscreen, eAutoPos_CenterCenter);

        Silkscreen.MoveToXY(Board.XOrigin - 1000000, Board.YOrigin + 1000000);

        Silkscreen.EndModify;
    end;

    if Silkscreen.Selected = True then Silkscreen.Selected := False;
end;

// Try different rotations on squarish components
function Try_Rotation(SlkList: TObjectList): Integer;
const
    MAX_RATIO = 1.2; // Component is almost square, so we are safe to try a different rotation
var
    Slk: IPCB_Text;
    Rect: TCoordRect;
    i, L, w: Integer;
    PlaceCnt: Integer;
    Rotation: Integer;
begin

    PlaceCnt := 0;
    // iterate in reverse order because we may remove items
    For i := SlkList.Count - 1 downto 0 do
    begin
        Slk := SlkList[i];

        Rect := Get_Obj_Rect(Slk);
        L := Rect.Right - Rect.Left;
        w := Rect.Top - Rect.Bottom;
        if w < L then
        begin
            w := Rect.Right - Rect.Left;
            L := Rect.Top - Rect.Bottom;
        end;

        // Silk rotations that don't match component rotations don't look right, but
        // this is less of a concern with more square components
        if ((w / L) > MAX_RATIO) or ((w / L) < (1 / MAX_RATIO)) then
            Continue;

        Slk.BeginModify;

        Rotation := Slk.Rotation;
        if (Rotation = 0) or (Rotation = 180) or (Rotation = 360) then
        begin
            Slk.Rotation := MirrorBottomRotation(Slk, 90);
        end
        else if (Rotation = 90) or (Rotation = 270) then
        begin
            Slk.Rotation := MirrorBottomRotation(Slk, 0);
        end
        else
        begin
            Slk.Rotation := Slk.Component.Rotation;
        end;

        Slk.EndModify;

        // If not placed, reset the rotation back to its original value
        if Place_Silkscreen(Slk) then
        begin
            Inc(PlaceCnt);
            SlkList.Delete(i);  // if successfully placed, remove from list
        end
        else
        begin
            Slk.BeginModify;
            Slk.Rotation := Rotation; // Reset Original Rotation
            Slk.EndModify;
        end;
    end;

    Result := PlaceCnt;
end;

procedure RunGUI;
begin
    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    MEM_AllowUnder.Text := TEXTBOXINIT;
    Form_PlaceSilk.ShowModal;
end;

procedure AddMessage(MessageClass, MessageText: String);
begin
    // https://www.altium.com/ru/documentation/altium-nexus/wsm-api-types-and-constants/#Image%20Index%20Table
    // [!!!] 66 index for debug info
    GetWorkspace.DM_MessagesManager.BeginUpdate();
    GetWorkspace.DM_MessagesManager.AddMessage(MessageClass, MessageText, 'Auto Place Silkscreen', GetWorkspace.DM_FocusedDocument.DM_FileName, '', '', 75, MessageClass = 'APS Status');
    GetWorkspace.DM_MessagesManager.EndUpdate();
    GetWorkspace.DM_MessagesManager.UpdateWindow();
end;

{ .............................................................................. }
procedure Main(Place_Selected: Boolean; Place_OverComp: Boolean; Place_RestoreOriginal: Boolean; AllowUnderList: TStringList);
var
    Silkscreen: IPCB_Text;
    Cmp: IPCB_Component;
    Iterator: IPCB_BoardIterator;
    Count, PlaceCnt, NotPlaceCnt, i: Integer;
    NumSelected: Integer;
    //CurSelected: IPCB_Primitive;
    //NotPlaced: TObjectList; // use global var
    PCBSystemOptions: IPCB_SystemOptions;
    DRCSetting: Boolean;
    StartTime: TDateTime;
begin
    StartTime := Now();

    GetWorkspace.DM_MessagesManager.ClearMessages();
    GetWorkspace.DM_ShowMessageView();

    AddMessage('APS Event', 'Placing Started');

    // Set cursor to waiting.
    Screen.Cursor := crHourGlass;

    PCBServer.PreProcess;

    // Disables Online DRC during designator movement to improve speed
    PCBSystemOptions := PCBServer.SystemOptions;

    if PCBSystemOptions <> nil then
    begin
        DRCSetting := PCBSystemOptions.DoOnlineDRC;
        PCBSystemOptions.DoOnlineDRC := False;
    end;

    TextProperites := CreateObject(TStringList);
    TextProperites.NameValueSeparator := '=';

    DictionaryCache := CreateObject(TStringList);
    DictionaryCache.NameValueSeparator := '=';

    // Initialize silk reference designators to board origin coordinates.
    Move_Silk_Off_Board(Place_Selected);

    // Create the iterator that will look for Component Body objects only
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iterator.AddFilter_Method(eProcessAll);

    NotPlaced := CreateObject(TObjectList);
    NotPlaced.OwnsObjects := False; // required to not throw invalid pointer errors when NotPlaced is freed

    // count number of components selected so we can stop iterating early if we processed all selected
    NumSelected := 0;
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        //CurSelected := Board.SelectecObject[i];
        if Board.SelectecObject[i].ObjectId = eComponentObject then NumSelected := NumSelected + 1;
    end;

    ProgressBar1.Position := 0;
    ProgressBar1.Update;
    if Place_Selected then ProgressBar1.Max := NumSelected else ProgressBar1.Max := Get_Iterator_Count(Iterator);

    // Search for component body objects and get their Name, Kind, Area and OverallHeight values
    Count := 0;
    PlaceCnt := 0;
    NotPlaceCnt := 0;
    Cmp := Iterator.FirstPCBObject;
    while (Cmp <> nil) do
    begin
        if (Place_Selected and Cmp.Selected) or (not(Place_Selected)) then
        begin
            Silkscreen := Cmp.Name;

            if (Place_Silkscreen(Silkscreen)) then
            begin
                Inc(PlaceCnt);
            end
            else
            begin
                Inc(NotPlaceCnt);
                NotPlaced.Add(Silkscreen);
            end;

            Inc(Count);
        end;
        Cmp := Iterator.NextPCBObject;

        //ProgressBar1.Position := ProgressBar1.Position + 1;
        ProgressBar1.Position := Count;
        ProgressBar1.Update;

        AddMessage('APS Status',
            Format('%d of %d silkscreens placed (%f%%) in %d Second(s)',
                    [PlaceCnt, Count, PlaceCnt / Count * 100, Trunc((Now() - StartTime) * 86400)]));

        if Place_Selected and (Count >= NumSelected) then break;
    end;
    Board.BoardIterator_Destroy(Iterator);

    // Try different rotation for squarish components
    PlaceCnt := PlaceCnt + Try_Rotation(NotPlaced);

    // Move each silkscreen reference designator over its respective component
    if Place_OverComp then
        Move_Silk_Over_Comp(NotPlaced);
    if Place_RestoreOriginal then
        Restore_Comp(NotPlaced);

    if NotPlaced.Count > 0 then
    begin
        if ConfirmNoYes('Failed to place ' + IntToStr(NotPlaced.Count) + ' designators.' + NEWLINECODE + 'Select only failed placements?') then
        begin
            Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
            for NotPlaceCnt := 0 to NotPlaced.Count - 1 do
            begin
                NotPlaced[NotPlaceCnt].Component.Selected := True;
            end;
        end;
    end;

    // Restore DRC setting
    if PCBSystemOptions <> nil then
    begin
        PCBSystemOptions.DoOnlineDRC := DRCSetting;
    end;

    PCBServer.PostProcess;

    // Restore cursor to normal
    Screen.Cursor := crArrow;

    AddMessage('APS Event',
        Format('Placing finished with 0 contention(s). Failed to placed %d silkscreen(s) in %d Second(s)',
                [Count - PlaceCnt, Trunc((Now() - StartTime) * 86400)]));

    ShowInfo('Script execution complete. ' + IntToStr(PlaceCnt) + ' out of ' +
        IntToStr(Count) + ' Placed. ' + FloatToStr(Round((PlaceCnt / Count) * 100)) + '%');

end;
{ .............................................................................. }

procedure Split(Delimiter: Char; Text: TPCBString; ListOfStrings: TStringList);
begin
    ListOfStrings.Clear;
    ListOfStrings.Delimiter := Delimiter;
    ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
    ListOfStrings.DelimitedText := Text;
end;

// Unfortunately [rfReplaceAll] keeps throwing errors, so I had to write this function
function RemoveNewLines(Text: TPCBString): TPCBString;
var
    strlen: Integer;
    NewStr: TPCBString;
begin
    strlen := length(Text);
    NewStr := StringReplace(Text, NEWLINECODE, ',', rfReplaceAll);
    while length(NewStr) <> strlen do
    begin
        strlen := length(NewStr);
        NewStr := StringReplace(NewStr, NEWLINECODE, ',', rfReplaceAll);
        NewStr := StringReplace(NewStr, ' ', '', rfReplaceAll);
    end;
    result := NewStr;
end;

procedure WriteToIniFile(AFileName: String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    IniFile.WriteInteger('Window', 'Top', Form_PlaceSilk.Top);
    IniFile.WriteInteger('Window', 'Left', Form_PlaceSilk.Left);

    IniFile.WriteInteger('General', 'FilterOptions', RG_Filter.ItemIndex);
    IniFile.WriteInteger('General', 'FailedPlacementOptions', RG_Failures.ItemIndex);
    IniFile.WriteBool('General', 'AvoidVias', chkAvoidVias.Checked);
    IniFile.WriteInteger('General', 'RotationStrategy', RotationStrategyCb.ItemIndex);
    IniFile.WriteBool('General', 'TryAlteredRotation', TryAlteredRotationChk.Checked);
    IniFile.WriteBool('General', 'FixedSizeEnabled', FixedSizeChk.Checked);
    IniFile.WriteString('General', 'FixedSize', FixedSizeEdt.Text);
    IniFile.WriteBool('General', 'FixedWidthEnabled', FixedWidthChk.Checked);
    IniFile.WriteString('General', 'FixedWidth', FixedWidthEdt.Text);
    IniFile.WriteString('General', 'PositionDelta', PositionDeltaEdt.Text);

    // I know about loops, but...
    IniFile.WriteBool('General', 'Position1', PositionsClb.Checked[0]);
    IniFile.WriteBool('General', 'Position2', PositionsClb.Checked[1]);
    IniFile.WriteBool('General', 'Position3', PositionsClb.Checked[2]);
    IniFile.WriteBool('General', 'Position4', PositionsClb.Checked[3]);
    IniFile.WriteBool('General', 'Position5', PositionsClb.Checked[4]);
    IniFile.WriteBool('General', 'Position6', PositionsClb.Checked[5]);
    IniFile.WriteBool('General', 'Position7', PositionsClb.Checked[6]);
    IniFile.WriteBool('General', 'Position8', PositionsClb.Checked[7]);

    // Donts have good idea about cbCmpOutlineLayer and MEM_AllowUnder

    IniFile.Free;
end;

procedure ReadFromIniFile(AFileName: String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    Form_PlaceSilk.Top := IniFile.ReadInteger('Window', 'Top', Form_PlaceSilk.Top);
    Form_PlaceSilk.Left := IniFile.ReadInteger('Window', 'Left', Form_PlaceSilk.Left);

    RG_Filter.ItemIndex := IniFile.ReadInteger('General', 'FilterOptions', RG_Filter.ItemIndex);
    RG_Failures.ItemIndex := IniFile.ReadInteger('General', 'FailedPlacementOptions', RG_Failures.ItemIndex);
    chkAvoidVias.Checked := IniFile.ReadBool('General', 'AvoidVias', chkAvoidVias.Checked);
    RotationStrategyCb.ItemIndex := IniFile.ReadInteger('General', 'RotationStrategy', RotationStrategyCb.ItemIndex);
    TryAlteredRotationChk.Checked := IniFile.ReadBool('General', 'TryAlteredRotation', TryAlteredRotationChk.Checked);
    FixedSizeChk.Checked := IniFile.ReadBool('General', 'FixedSizeEnabled', FixedSizeChk.Checked);
    FixedSizeEdt.Text := IniFile.ReadString('General', 'FixedSize', FixedSizeEdt.Text);
    FixedWidthChk.Checked := IniFile.ReadBool('General', 'FixedWidthEnabled', FixedWidthChk.Checked);
    FixedWidthEdt.Text := IniFile.ReadString('General', 'FixedWidth', FixedWidthEdt.Text);
    PositionDeltaEdt.Text := IniFile.ReadString('General', 'PositionDelta', PositionDeltaEdt.Text);

    // I know about loops, but...
    PositionsClb.Checked[0] := IniFile.ReadString('General', 'Position1', PositionsClb.Checked[0]);
    PositionsClb.Checked[1] := IniFile.ReadString('General', 'Position2', PositionsClb.Checked[1]);
    PositionsClb.Checked[2] := IniFile.ReadString('General', 'Position3', PositionsClb.Checked[2]);
    PositionsClb.Checked[3] := IniFile.ReadString('General', 'Position4', PositionsClb.Checked[3]);
    PositionsClb.Checked[4] := IniFile.ReadString('General', 'Position5', PositionsClb.Checked[4]);
    PositionsClb.Checked[5] := IniFile.ReadString('General', 'Position6', PositionsClb.Checked[5]);
    PositionsClb.Checked[6] := IniFile.ReadString('General', 'Position7', PositionsClb.Checked[6]);
    PositionsClb.Checked[7] := IniFile.ReadString('General', 'Position8', PositionsClb.Checked[7]);

    IniFile.Free;
end;

function ConfigFilename(Dummy: String = ''): String;
begin
    result := ClientAPI_SpecialFolder_AltiumApplicationData + '\AutoPlaceSilkscreen.ini';
end;

procedure TForm_PlaceSilk.BTN_RunClick(Sender: TObject);
var
    Place_Selected: Boolean;
    Place_OverComp: Boolean;
    Place_RestoreOriginal: Boolean;
    StrNoSpace: TPCBString;
    i: Integer;
    DisplayUnit: TUnit;
begin
    RotationStrategy := RotationStrategyCb.GetItemIndex();  // moved from form creation

    HintLbl.Visible := True;
    HintLbl.Update;

    Place_Selected := RG_Filter.ItemIndex = 1;
    Place_OverComp := RG_Failures.ItemIndex = 0;
    Place_RestoreOriginal := RG_Failures.ItemIndex = 2;

    AvoidVias := chkAvoidVias.Checked;

    AllowUnderList := CreateObject(TStringList);
    if MEM_AllowUnder.Text <> TEXTBOXINIT then
    begin
        StrNoSpace := RemoveNewLines(MEM_AllowUnder.Text);
        Split(',', StrNoSpace, AllowUnderList);
    end;

    DisplayUnit := Board.DisplayUnit;   // need to xor with 1 to convert to eMetric or eImperial for StringToCoordUnit
    StringToCoordUnit(PositionDeltaEdt.Text, SilkscreenPositionDelta, DisplayUnit xor 1);

    DisplayUnit := Board.DisplayUnit;
    StringToCoordUnit(FixedSizeEdt.Text, SilkscreenFixedSize, DisplayUnit xor 1);

    DisplayUnit := Board.DisplayUnit;
    StringToCoordUnit(FixedWidthEdt.Text, SilkscreenFixedWidth, DisplayUnit xor 1);

    SilkscreenIsFixedSize := FixedSizeChk.Checked;
    SilkscreenIsFixedWidth := FixedWidthChk.Checked;

    if TryAlteredRotationChk.Checked then
        TryAlteredRotation := 1
    else
        TryAlteredRotation := 0;

    Main(Place_Selected, Place_OverComp, Place_RestoreOriginal, AllowUnderList);

    HintLbl.Visible := False;
    HintLbl.Update;

    Close;
end;

// When user first enters textbox, clear it
procedure TForm_PlaceSilk.MEM_AllowUnderEnter(Sender: TObject);
begin
    if MEM_AllowUnder.Text = TEXTBOXINIT then
        MEM_AllowUnder.Text := '';
end;

// New combobox item selected
procedure TForm_PlaceSilk.cbCmpOutlineLayerChange(Sender: TObject);
var
    idx: Integer;
    LayerIdx: TLayer;
    LayerObj: IPCB_LayerObject;
begin
    idx := cbCmpOutlineLayer.GetItemIndex();
    LayerObj := cbCmpOutlineLayer.Items[idx];

    LayerIdx := String2Layer(cbCmpOutlineLayer.Text);
    CmpOutlineLayerID := StrToInt(MechLayerIDList.Get(idx));
end;

procedure TForm_PlaceSilk.Form_PlaceSilkCreate(Sender: TObject);
const
    DEFAULT_CMP_OUTLINE_LAYER = 'Mechanical 13';
var
    MechIterator: IPCB_LayerObjectIterator;
    LayerObj: IPCB_LayerObject;
    idx: Integer;
begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
        Exit;

    //ContourUtil := PCBServer.PCBContourUtilities; // for some reason doesn't create an object instance - use directly instead
    ContourMaker := PCBServer.PCBContourMaker;

    MechLayerIDList := CreateObject(TStringList);

    idx := 0;
    CmpOutlineLayerID := 0;
    MechIterator := Board.MechanicalLayerIterator;
    while MechIterator.Next do
    begin
        LayerObj := MechIterator.LayerObject;

        cbCmpOutlineLayer.AddItem(LayerObj.Name, LayerObj);
        MechLayerIDList.Add(IntToStr(LayerObj.V6_LayerID));

        // Set default layer
        if (LayerObj.Name = DEFAULT_CMP_OUTLINE_LAYER) or (ContainsText(LayerObj.Name, 'Component Outline')) then
        begin
            cbCmpOutlineLayer.SetItemIndex(idx);
            CmpOutlineLayerID := LayerObj.V6_LayerID;
        end;

        Inc(idx);
    end;

    PositionsClb.Items.Clear;

    PositionsClb.Items.AddObject('TopCenter', eAutoPos_TopCenter);
    PositionsClb.Items.AddObject('CenterRight', eAutoPos_CenterRight);
    PositionsClb.Items.AddObject('BottomCenter', eAutoPos_BottomCenter);
    PositionsClb.Items.AddObject('CenterLeft', eAutoPos_CenterLeft);
    PositionsClb.Items.AddObject('TopLeft', eAutoPos_TopLeft);
    PositionsClb.Items.AddObject('TopRight', eAutoPos_TopRight);
    PositionsClb.Items.AddObject('BottomLeft', eAutoPos_BottomLeft);
    PositionsClb.Items.AddObject('BottomRight', eAutoPos_BottomRight);

    PositionsClb.Checked[0] := True;
    PositionsClb.Checked[1] := True;
    PositionsClb.Checked[2] := True;
    PositionsClb.Checked[3] := True;

    FormCheckListBox1 := PositionsClb;

    HintLbl.Left := (Form_PlaceSilk.ClientWidth - HintLbl.Width) div 2;

    ReadFromIniFile(ConfigFilename);
end;

procedure TForm_PlaceSilk.Form_PlaceSilkClose(Sender: TObject;
    var Action: TCloseAction);
begin
    WriteToIniFile(ConfigFilename);
end;
