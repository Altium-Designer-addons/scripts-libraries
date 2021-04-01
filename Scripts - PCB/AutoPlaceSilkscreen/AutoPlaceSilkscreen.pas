// How to use:
// 1. Run script from pcb layout. If you want to only place selected components' silkscreen, then select these components (not designators) before running the script.
// 2. A GUI will open. Select options and run.
// 3. Any unplaced silkscreen will be placed on top of components by default or may be placed off the board if selected.
// 4. A popup message box will appear on completion saying how many components were placed and what percentage were placed.

// Created By: Stephen Thompson, https://github.com/coffeenmusic

// HALT EXECUTION: ctrl + PauseBreak

//TODO:
//      - Iterate through all good placement positions, use the one with the lowest x/y --> x2/y2 delta square distance
//      - Only allow 2 silk designators close to eachother if they are perpendicular to eachother
//      - Use Courtyard layer tracks

Uses
  Winapi, ShellApi, Win32.NTDef, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, System, System.Diagnostics;

Const
    NEWLINECODE = #13#10;
    TEXTBOXINIT = 'Example:'+NEWLINECODE+'J3'+NEWLINECODE+'SH1';
Var
    AllowUnderList: TStringList;
    MechLayerIDList: TStringList;
    Board         : IPCB_Board;
    CmpOutlineLayerID : Integer;
    AvoidVias : Boolean;

// May want different Bounding Rectangles depending on the object
function Get_Obj_Rect(Obj: IPCB_ObjectClass): TCoordRect;
var
    Rect    : TCoordRect;
    ObjID : Integer;
begin
    ObjID := Obj.ObjectId;
    if ObjID = eBoardObject then
    begin
        Rect := Obj.BoardOutline.BoundingRectangle;
    end
    else if ObjID = eComponentObject then
    begin
        //Rect := Obj.BoundingRectangleNoNameComment;
        Rect := Obj.BoundingRectangleNoNameCommentForSignals;
    end
    else
    begin
        Rect := Obj.BoundingRectangle;
    end;

    result := Rect;
end;

// Check if object coordinates are outside board edge
function Is_Outside_Board(Obj: IPCB_ObjectClass): Boolean;
var
    BoardRect, Rect    : TCoordRect;
begin
    Rect := Get_Obj_Rect(Obj);
    BoardRect := Get_Obj_Rect(Board);

    if (Rect.Left < BoardRect.Left) or
       (Rect.Right > BoardRect.Right) or
       (Rect.Bottom < BoardRect.Bottom) or
       (Rect.Top > BoardRect.Top)
    then
    begin
         result := True;
         Exit; // return
    end;

    result := False;
end;

// Check if two layers are the on the same side of the board. Handle different layer names.
function Is_Same_Side(Obj1: IPCB_ObjectClass, Obj2: IPCB_ObjectClass): Boolean;
var
   Layer1, Layer2 : Integer;
begin
    Layer1 := Obj1.Layer; Layer2 := Obj2.Layer;
    if Obj1.ObjectId = eComponentBodyObject then Layer1 := Obj1.Component.Layer;
    if Obj2.ObjectId = eComponentBodyObject then Layer2 := Obj2.Component.Layer;

    // Top Layer
    if (Layer1 = eTopLayer) or (Layer1 = eTopOverlay) then
    begin
        if (Layer2 <> eBottomLayer) and (Layer2 <> eBottomOverlay) then
        begin
              result := True; Exit; // return True
        end;
    end
    // Bottom Layer
    else if (Layer1 = eBottomLayer) or (Layer1 = eBottomOverlay) then
    begin
         if (Layer2 <> eTopLayer) and (Layer2 <> eTopOverlay) then
         begin
              result := True; Exit; // return True
         end;
    end
    // Multi Layer
    else if (Layer1 = eMultiLayer) or (Layer2 = eMultiLayer) then
    begin
         result := True; Exit;
    end;

    result := False;
end;

// Guess silkscreen size based on component size
function Get_Silk_Size(Slk: IPCB_Text, Min_Size: Integer): Integer;
var
   Rect    : TCoordRect;
   area : Integer;
   size : Integer;
begin
    // Stroke Width & Text Height
    Rect := Get_Obj_Rect(Slk.Component);
    area := CoordToMils(Rect.Right - Rect.Left)*CoordToMils(Rect.Top - Rect.Bottom);

    size := Int((82*area)/(16700 + area));
    if size < Min_Size then size := Min_Size;

    result := size;
end;

// Checks if 2 objects are overlapping on the PCB
function Is_Overlapping(Obj1: IPCB_ObjectClass, Obj2: IPCB_ObjectClass): Boolean;
const
    SLKPAD = 40000; // Allowed Overlap = 4 mil
    PADPAD = 10000; // Margin beyond pad = 1 mil
var
    Rect1, Rect2    : TCoordRect;
    L, R, T, B  : Integer;
    L2, R2, T2, B2  : Integer;
    Delta1, Delta2 : Integer;
begin
    // If silkscreen object equals itself, return False
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
    If Obj1.IsHidden or Obj2.IsHidden Then
    Begin
        result := False;
        Exit; // Continue
    End;

    // Continue if Layers Dont Match
    if not Is_Same_Side(Obj1, Obj2) then
    begin
         result := False;
         Exit; // Continue
    end;

    Rect1 := Get_Obj_Rect(Obj1);
    Rect2 := Get_Obj_Rect(Obj2);

    // Neg/Pos padding margins
    Delta1 := 0; Delta2 := 0;
    if (Obj1.ObjectId = eTextObject) and (Obj2.ObjectId = eTextObject) and Obj1.IsDesignator then Delta1 := -SLKPAD;
    if (Obj1.ObjectId = eTextObject) and (Obj2.ObjectId = eTextObject) and Obj2.IsDesignator then Delta2 := -SLKPAD;
    if (Obj1.ObjectId = ePadObject) then Delta1 := PADPAD;
    if (Obj2.ObjectId = ePadObject) then Delta2 := PADPAD;

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

// Returns correct layer set given the object being used
function Get_LayerSet(SlkLayer: Integer, ObjID: Integer): PAnsiChar;
var
   TopBot : Integer;
begin
     TopBot := eTopLayer;
     if (Layer2String(SlkLayer) = 'Bottom Overlay') then TopBot := eBottomLayer;

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

function Allow_Under(Cmp : IPCB_Component, AllowUnderList: TStringList):Boolean;
var
    refdes : TPCB_String;
    i : Integer;
begin
    if AllowUnderList <> nil then
    begin
        For i := 0 to AllowUnderList.Count - 1 do
        begin
            refdes := LowerCase(AllowUnderList.Get(i));
            if LowerCase(Cmp.Name.Text) = refdes then
            begin
                result := True;
                exit;
            end;
        end;
    end;
    result := False;
end;

// Get components for surrounding area
function IsOverObj(Slk: IPCB_Text, ObjID: Integer, Filter_Size: Integer): Boolean;
var
    Iterator      : IPCB_SpatialIterator;
    Obj          : IPCB_ObjectClass;
    Rect : TCoordRect;
    RectL,RectR,RectB,RectT : TCoord;
    RegIter       : Boolean; // Regular Iterator
    Name : TPCBString;
begin
    // Spatial Iterators only work with Primitive Objects and not group objects like eComponentObject and dimensions
    if (ObjID = eComponentObject) then
    begin
        Iterator        := Board.BoardIterator_Create;
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
    While Obj <> NIL Do
    Begin
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
            if (Allow_Under(Obj, AllowUnderList)) or (Obj.Name.Layer <> Slk.Layer) then
            begin
                 Obj := Iterator.NextPCBObject;
                 Continue;
            end;
        end;

        Try
        // Check if Silkscreen is overlapping with other object (component/pad/silk)
        If Is_Overlapping(Slk, Obj) Then
        Begin
             result := True;
             Exit; // Equivalent to return in C
        End;
        Except
             Name := Slk.Text;
        End;

        Obj := Iterator.NextPCBObject;
    End;

    // Destroy Iterator
    If RegIter then
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
function Move_Silk_Off_Board(OnlySelected : Boolean);
var
    Iterator     : IPCB_SpatialIterator;
    Slk          : IPCB_Text;
begin
     Iterator        := Board.BoardIterator_Create;
     Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
     Iterator.AddFilter_IPCB_LayerSet(MkSet(eTopOverlay, eBottomOverlay));
     Iterator.AddFilter_Method(eProcessAll);

    // Iterate through silkscreen reference designators.
    Slk := Iterator.FirstPCBObject;
    While Slk <> NIL Do
    Begin
         if Slk.IsDesignator then
         begin
             if (OnlySelected and Slk.Component.Selected) or (not OnlySelected) then
             begin
                 Slk.Component.ChangeNameAutoposition := eAutoPos_Manual;
                 Slk.MoveToXY(Board.XOrigin - 1000000, Board.YOrigin - 1000000); // Move slightly off board origin
             end;
         end;

         Slk := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
end;

function Move_Silk_Over_Comp(SlkList: TObjectList);
var
    Slk          : IPCB_Text;
    i            : Integer;
begin
    PCBServer.PreProcess;
    For i := 0 To SlkList.Count - 1 Do
    Begin
        Slk := SlkList[i];

        PCBServer.SendMessageToRobots(Slk.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);

        Slk.Component.ChangeNameAutoposition := eAutoPos_CenterCenter;

        PCBServer.SendMessageToRobots(Slk.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
    End;

    PCBServer.PostProcess;
end;

function GetNextAutoPosition(iteration : Integer): Integer;
begin
  Case iteration of
       0 : result := eAutoPos_CenterRight;
       1 : result := eAutoPos_TopCenter;
       2 : result := eAutoPos_CenterLeft;
       3 : result := eAutoPos_BottomCenter;
       4 : result := eAutoPos_TopLeft;
       5 : result := eAutoPos_TopRight;
       6 : result := eAutoPos_BottomLeft;
       7 : result := eAutoPos_BottomRight;
       8 : result := eAutoPos_Manual;
  else     result := eAutoPos_Manual;
  end;
end;

function AutoPosDeltaAdjust(autoPos: Integer, X_offset: Integer, Y_offset: Integer, Silk : IPCB_Text, Layer: TPCBString);
const
    DELTAMILS = 20;
var
    dx,dy,d : Integer;
    xorigin, yorigin : Integer;
    flipx : Integer;
    r : Integer;
begin
  d := MilsToCoord(DELTAMILS);
  dx := 0;
  dy := 0;
  r := Silk.Rotation;
  flipx := 1; // x Direction flips on the bottom layer
  If Layer = 'Bottom Layer' Then
     flipx := -1;

  Case autoPos of
       eAutoPos_CenterRight : dx := -d*flipx;
       eAutoPos_TopCenter : dy := -d;
       eAutoPos_CenterLeft : dx := d*flipx;
       eAutoPos_BottomCenter : dy := d;
       eAutoPos_TopLeft : dy := -d;
       eAutoPos_TopRight : dy := -d;
       eAutoPos_BottomLeft : dy := d;
       eAutoPos_BottomRight : dy := d;
  end;

  If (r = 90) or (r = 270) Then
  Begin
      If (autoPos = eAutoPos_TopLeft) or (autoPos = eAutoPos_BottomLeft) Then
      Begin
          dx := d*flipx;
      End
      Else If (autoPos = eAutoPos_TopRight) or (autoPos = eAutoPos_BottomRight) Then
      Begin
          dx := -d*flipx;
      End;
  End;
  Silk.MoveByXY(dx + MilsToCoord(X_offset), dy + MilsToCoord(Y_offset));
end;

function Rotation_MatchSilk2Comp(Silk : IPCB_Text);
var
    r : Integer; // Component Rotation
begin
  r := Silk.Component.Rotation;

  If (r = 0) or (r = 180) or (r = 360) Then
  Begin
       Silk.Rotation := 0;
  End
  Else If (r = 90) or (r = 270) Then
  Begin
       If Silk.Layer = eTopOverlay Then
       Begin
            Silk.Rotation := 90;
       End
       Else
       Begin
            Silk.Rotation := 270; // Bottom silk should be mirrored and therefore 270 instead of 90
       End;
  End;
end;

function Get_Iterator_Count(Iterator: IPCB_BoardIterator): Integer;
var
    cnt : Integer;
    cmp : IPCB_Component;
begin
  cnt := 0;

  cmp := Iterator.FirstPCBObject;
  While cmp <> NIL Do
  Begin
       Inc(cnt);
       cmp := Iterator.NextPCBObject;
  End;
  result := cnt;
end;

function Place_Silkscreen(Silkscreen: IPCB_Text): Boolean;
const
    OFFSET_DELTA = 5; // [mils] Silkscreen placement will move the position around by this delta
    OFFSET_CNT = 3; // Number of attempts to offset position in x or y directions
    MIN_SILK_SIZE = 30; // [mils]
    ABS_MIN_SILK_SIZE = 25; // [mils]
    SILK_SIZE_DELTA = 5; // [mils] Decrement silkscreen size by this value if not placed
    FILTER_SIZE_MILS = 0; // [mils] Additional delta to check surrounding objects. Adds delta to object rectangle.
var
    NextAutoP      : Integer;
    Placed : Boolean;
    xinc, yinc, xoff, yoff : Integer;
    SlkSize : Integer;
    FilterSize : Integer;
    Count, i      : Integer;
begin
     result := True;
     Placed := False;

     // Skip hidden silkscreen
     If Silkscreen.IsHidden Then
     Begin
          result := False;
          Exit;
     End;

     FilterSize := MilsToCoord(FILTER_SIZE_MILS);

     // Get Silkscreen Size
     SlkSize := Get_Silk_Size(Silkscreen, MIN_SILK_SIZE);
     Silkscreen.Size := MilsToCoord(SlkSize);
     Silkscreen.Width := 2*(Silkscreen.Size/10);

     // If not placed, reduce silkscreen size
     While CoordToMils(Silkscreen.Size) >= ABS_MIN_SILK_SIZE Do
     Begin
          xoff := 0;
          // If not placed, increment x offset
          For xinc := 0 to OFFSET_CNT Do
          Begin
               yoff := 0;
               // If not placed, increment y offset
               For yinc := 0 to OFFSET_CNT Do
               Begin
                    // If not placed Change Autoposition on Silkscreen
                    For i := 0 to 8 Do
                    Begin
                         NextAutoP := GetNextAutoPosition(i);
                         Silkscreen.Component.ChangeNameAutoposition := NextAutoP;
                         AutoPosDeltaAdjust(NextAutoP, xoff*OFFSET_DELTA, yoff*OFFSET_DELTA, Silkscreen, Layer2String(Silkscreen.Component.Layer));


                         // Silkscreen RefDes Overlap Detection
                         If IsOverObj(Silkscreen, eTextObject, FilterSize) Then
                         Begin
                              Continue;
                         End
                         // Silkscreen Tracks Overlap Detection
                         Else If IsOverObj(Silkscreen, eTrackObject, FilterSize) Then
                         Begin
                              Continue;
                         End
                         Else If IsOverObj(Silkscreen, ePadObject, FilterSize) Then
                         Begin
                              Continue;
                         End
                         // Outside Board Edge
                         Else If Is_Outside_Board(Silkscreen) Then
                         Begin
                              Continue;
                         End
                         // Component Overlap Detection
                         Else If IsOverObj(Silkscreen, eComponentBodyObject, FilterSize) Then
                         Begin
                              Continue;
                         End
                         Else If (AvoidVias) And (IsOverObj(Silkscreen, eViaObject, FilterSize)) Then
                         Begin
                              Continue;
                         End
                         // PLACED
                         Else
                         Begin
                              Placed := True;
                              Exit;
                         End;
                    End;

                    yoff := yoff*-1; // Toggle sign
                    if yoff >= 0 then yoff := yoff + 1; // Toggle increment
               End;

               xoff := xoff*-1; // Toggle sign
               if xoff >= 0 then xoff := xoff +1; // Toggle increment
          End;

          if Placed or ((CoordToMils(Silkscreen.Size) - SILK_SIZE_DELTA) < ABS_MIN_SILK_SIZE) then
          begin
               Break;
          end;

          // No placement found, try reducing silkscreen size
          Silkscreen.Size := Silkscreen.Size - MilsToCoord(SILK_SIZE_DELTA);
          Silkscreen.Width := Int(2*(Silkscreen.Size/10) - 10000); // Width needs to change relative to size
     End;

     if not Placed then
     begin
          result := False;

          // Reset Silkscreen Size
          SlkSize := Get_Silk_Size(Silkscreen, MIN_SILK_SIZE);
          Silkscreen.Size := MilsToCoord(SlkSize);
          Silkscreen.Width := 2*(Silkscreen.Size/10);

          // Move off board for now
          PCBServer.PreProcess;
          PCBServer.SendMessageToRobots(Silkscreen.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
          Silkscreen.Component.ChangeNameAutoposition := eAutoPos_Manual;
          Silkscreen.MoveToXY(Board.XOrigin - 1000000, Board.YOrigin + 1000000);
          PCBServer.SendMessageToRobots(Silkscreen.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
          PCBServer.PostProcess;
     end;
end;

// Try different rotations on squarish components
function Try_Rotation(SlkList: TObjectList): Integer;
const
     MAX_RATIO = 1.2; // Component is almost square, so we are safe to try a different rotation
var
    Slk          : IPCB_Text;
    rect         : TCoordRect;
    i, l, w      : Integer;
    PlaceCnt     : Integer;
    Rotation     : Integer;
begin
    PCBServer.PreProcess;

    PlaceCnt := 0;
    For i := 0 To SlkList.Count - 1 Do
    Begin
        Slk := SlkList[i];

        rect := Get_Obj_Rect(Slk);
        l := rect.Right - rect.Left;
        w := rect.Top - rect.Bottom;
        if w < l then
        begin
            w := rect.Right - rect.Left;
            l := rect.Top - rect.Bottom;
        end;

        // Silk rotations that don't match component rotations don't look right, but
        // this is less of a concern with more square components
        if ((w/l) > MAX_RATIO) or ((w/l) < (1/MAX_RATIO)) then Continue;


        PCBServer.PreProcess;
        PCBServer.SendMessageToRobots(Slk.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);

        Rotation := Slk.Rotation;
        If (Rotation = 0) or (Rotation = 180) or (Rotation = 360) Then
        Begin
            Slk.Rotation := 90;
        End
        Else If (Rotation = 90) or (Rotation = 270) Then
        Begin
            Slk.Rotation := 0;
        End
        Else
        Begin
            Slk.Rotation := Slk.Component.Rotation;
        End;

        // If not placed, reset the rotation back to its original value
        If Place_Silkscreen(Slk) Then
        Begin
             Inc(PlaceCnt);
        End
        Else
        Begin
             Slk.Rotation := Rotation; // Reset Original Rotation
        End;

        PCBServer.SendMessageToRobots(Slk.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
        PCBServer.PostProcess;
    End;

    PCBServer.PostProcess;
end;

Procedure RunGUI;
Begin
    MEM_AllowUnder.Text := TEXTBOXINIT;
    Form_PlaceSilk.ShowModal;
End;

{..............................................................................}
function Main(Place_Selected: Boolean, Place_OverComp: Boolean, AllowUnderList: TStringList);
Var
    Silkscreen    : IPCB_Text;
    Cmp           : IPCB_Component;
    Iterator      : IPCB_BoardIterator;
    Count, PlaceCnt, NotPlaceCnt, i : Integer;
    NotPlaced     : TObjectList;
    Rotation      : Integer;
    X1, X2, Y1, Y2: Integer;
Begin
    // set cursor to waiting.
    Screen.Cursor      := crHourGlass;

    // Initialize silk reference designators to board origin coordinates.
    Move_Silk_Off_Board(Place_Selected);

    // Create the iterator that will look for Component Body objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iterator.AddFilter_Method(eProcessAll);

    NotPlaced := TObjectList.Create;

    ProgressBar1.Position := 0;
    ProgressBar1.Update;
    ProgressBar1.Max := Get_Iterator_Count(Iterator);

    // Search for component body objects and get their Name, Kind, Area and OverallHeight values
    Count := 0; PlaceCnt := 0; NotPlaceCnt := 0;
    Cmp := Iterator.FirstPCBObject;
    While (Cmp <> Nil) Do
    Begin
        if (Place_Selected and Cmp.Selected) or (not(Place_Selected)) then
        begin
            Silkscreen := Cmp.Name;
            Rotation_MatchSilk2Comp(Silkscreen); // Matches silk rotation to component rotation

            PCBServer.PreProcess;
            PCBServer.SendMessageToRobots(Silkscreen.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);

            if (Place_Silkscreen(Silkscreen)) then
            begin
                Inc(PlaceCnt);
            end
            else
            begin
                Inc(NotPlaceCnt);
                NotPlaced.Add(Silkscreen);
            end;

            PCBServer.SendMessageToRobots(Silkscreen.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
            PCBServer.PostProcess;

            Inc(Count);
        end;
        Cmp := Iterator.NextPCBObject;
        ProgressBar1.Position := ProgressBar1.Position + 1;
        ProgressBar1.Update;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // Try different rotation for squarish components
    PlaceCnt := PlaceCnt + Try_Rotation(NotPlaced);

    // Restore cursor to normal
    Screen.Cursor          := crArrow;

    // Move each silkscreen reference designator over its respective component
    If Place_OverComp Then Move_Silk_Over_Comp(NotPlaced);

    ShowMessage('Script execution complete. ' + IntToStr(PlaceCnt) + ' out of ' + IntToStr(Count) + ' Placed. ' + FloatToStr(Round((PlaceCnt/Count)*100)) + '%');
End;
{..............................................................................}

function Split(Delimiter: Char; Text: TPCBString; ListOfStrings: TStrings);
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Text;
end;

// Unfortunately [rfReplaceAll] keeps throwing errors, so I had to write this function
function RemoveNewLines(Text: TPCBString):TPCBString;
var
     strlen : Integer;
     NewStr : TPCBString;
begin
     strlen := length(Text);
     NewStr := StringReplace(Text, NEWLINECODE, ',', rfReplaceAll);
     While length(NewStr) <> strlen do
     begin
         strlen := length(NewStr);
         NewStr := StringReplace(NewStr, NEWLINECODE, ',', rfReplaceAll);
         NewStr := StringReplace(NewStr, ' ', '', rfReplaceAll);
     end;
     result := NewStr;
end;

procedure TForm_PlaceSilk.BTN_RunClick(Sender: TObject);
var
     Place_Selected : Boolean;
     Place_OverComp : Boolean;
     StrNoSpace : TPCBString;
     i : Integer;
begin
     MechLayerIDList.Free;

     Place_Selected := RG_Filter.ItemIndex = 1;
     Place_OverComp := RG_Failures.ItemIndex = 0;

     AvoidVias := chkAvoidVias.Checked;

     AllowUnderList := TStringList.Create;
     StrNoSpace := RemoveNewLines(MEM_AllowUnder.Text);
     Split(',', StrNoSpace, AllowUnderList);

     Main(Place_Selected, Place_OverComp, AllowUnderList);
     AllowUnderList.Free;
     Close;
end;

// When user first enters textbox, clear it
procedure TForm_PlaceSilk.MEM_AllowUnderEnter(Sender: TObject);
begin
    if MEM_AllowUnder.Text = TEXTBOXINIT then MEM_AllowUnder.Text := '';
end;


procedure TForm_PlaceSilk.Form_PlaceSilkActivate(Sender: TObject);
const
    DEFAULT_CMP_OUTLINE_LAYER = 'Mechanical 13';
var
    MechIterator : IPCB_LayerObjectIterator;
    LayerObj : IPCB_LayerObject;
    idx : Integer;
begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    MechLayerIDList := TStringList.Create;

    idx := 0;
    CmpOutlineLayerID := 0;
    MechIterator := Board.MechanicalLayerIterator;
    While MechIterator.Next Do
    Begin
        LayerObj := MechIterator.LayerObject;

        cbCmpOutlineLayer.AddItem(LayerObj.Name, LayerObj);
        MechLayerIDList.Add(IntToStr(LayerObj.V6_LayerID));

        // Set default layer
        If LayerObj.Name = DEFAULT_CMP_OUTLINE_LAYER Then
        Begin
            cbCmpOutlineLayer.SetItemIndex(idx);
            CmpOutlineLayerID := LayerObj.V6_LayerID;
        End;

        Inc(idx)
    End;
end;

// New combobox item selected
procedure TForm_PlaceSilk.cbCmpOutlineLayerChange(Sender: TObject);
var
    idx : Integer;
    LayerIdx : TLayer;
    LayerObj : IPCB_LayerObject;
begin
    idx := cbCmpOutlineLayer.GetItemIndex();
    LayerObj := cbCmpOutlineLayer.Items[idx];

    LayerIdx := String2Layer(cbCmpOutlineLayer.Text);
    CmpOutlineLayerID := StrToInt(MechLayerIDList.Get(idx));
end;

