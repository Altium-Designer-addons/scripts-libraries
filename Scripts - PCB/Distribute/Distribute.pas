// Created by:  Matija Markovic, Petar Perisin
// Edited by:   Ryan Rutledge
// For documentation see README.md

var
    Board               : IPCB_Board;
    PresetFilePath      : String;
    PresetList          : TStringList;
    SortedTracks        : TStringList;
    TrimPerpendicular   : Boolean; // test feature that trims dangling track ends

Const
    NumPresets = 14; // no longer just for presets, also used to save previous state


// critical function to get normalized line properties. k is slope, c is intercept
Procedure SetupDataFromTrack(var Prim1 : IPCB_Track, out IsVertical : Boolean, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord : Out Y2 : TCoord, out k : Double, out c : TCoord);
var
    a, b : Integer;
begin
    if Prim1.X1 = Prim1.X2 then
    begin
        IsVertical := True;
        X1         := Prim1.X1;
        X2         := Prim1.X2;
        if Prim1.Y1 < Prim1.Y2 then
        begin
            Y1 := Prim1.Y1;
            Y2 := Prim1.Y2;
        end
        else
        begin
            Y1 := Prim1.Y2;
            Y2 := Prim1.Y1;
        end;
        k := 0;
        c := Prim1.X1;
    end
    else // else track is not vertical
    begin
        if Prim1.X1 < Prim1.X2 then
        begin // track runs left to right
            X1 := Prim1.X1;
            Y1 := Prim1.Y1;
            X2 := Prim1.X2;
            Y2 := Prim1.Y2;
        end
        else // track runs right to left
        begin
            X1 := Prim1.X2;
            Y1 := Prim1.Y2;
            X2 := Prim1.X1;
            Y2 := Prim1.Y1;
        end;
        k := (Y2 - Y1) / (X2 - X1); // calculate track slope

        if (Abs(k) > 20) then
        begin // if slope k > 20, consider track as vertical
            X1 := Prim1.X1;
            X2 := Prim1.X2;

            // determine which X coordinate rounds best to coords?
            repeat
                a  := X1 mod 10;
                b  := X2 mod 10;
                X1 := X1 div 10;
                X2 := X2 div 10;
            until ((a <> 0) or (b <> 0));

            // if X1 fits best use that, else use X2
            if a = 0 then
            begin
                X1 := Prim1.X1;
                X2 := Prim1.X1;
            end
            else
            begin
                X1 := Prim1.X2;
                X2 := Prim1.X2;
            end;

            // normalize Y direction
            if Prim1.Y1 < Prim1.Y2 then
            begin
                Y1 := Prim1.Y1;
                Y2 := Prim1.Y2;
            end
            else
            begin
                Y1 := Prim1.Y2;
                Y2 := Prim1.Y1;
            end;

            c          := X1; // set intercept of line as X1
            IsVertical := True;
        end
        else // else if track has slope <= 20, calculate intercept point
        begin
            c          := Y1 - k * X1; //
            IsVertical := False;
        end;
    end;
end;


// function to create slope and intercept for virtual line perpendicular to a point
Function GetPerpendicularLine(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, out k2 : Double, out c2 : TCoord, out IsPrim2Vert : Boolean, X : TCoord, Y : TCoord) : Boolean;
begin
    Result := True;
    if IsPrim1Vert then
    begin // if first line is vertical, perpendicular line is simply horizontal
        IsPrim2Vert := False;
        k2          := 0;
        c2          := Y;
    end
    else if k1 = 0 then
    begin // if first line is horizontal, perpendicular line is simply vertical
        IsPrim2Vert := True;
        k2          := 999; // pretty sure this is unnecessary if IsPrim2Vert = True
        c2          := X;
    end
    else // if first line is not vertical or horizontal
    begin
        Result      := False; // return false if perpendicular line is not horizontal or vertical
        IsPrim2Vert := False;
        k2          := 1.0 / (-k1);    // perpendicular slope is negative reciprocal
        c2          := Y - (k2 * X); // perpendicular line intercept
    end;
end;


// function to test if two tracks have an intercept point (tracks are not parallel)
Function GetIntersection(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, k2 : Double, c2 : TCoord, IsPrim2Vert : Boolean, out X : TCoord, out Y : TCoord) : Boolean;
begin
    Result := True;
    if (IsPrim1Vert and IsPrim2Vert) or ((not IsPrim1Vert) and (not IsPrim2Vert) and (Abs(k1 - k2) < 0.01)) then
    begin
        // Parallel tracks
        Result := False;
    end
    else if IsPrim1Vert or IsPrim2Vert then
    begin
        if IsPrim1Vert then
        begin
            X := c1;
            Y := k2 * c1 + c2;
        end
        else // IsPrim2Vert
        begin
            X := c2;
            Y := k1 * c2 + c1;
        end;
    end
    else
    begin // neither line is vertical
        X := (c2 - c1) / (k1 - k2);
        Y := k1 * X + c1;
    end;
end;


// Function that calculates point to point distance
function PointToPointDistance(X1, Y1, X2, Y2) : Double;
begin
    Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;


// function to check if there is another track connected to the given endpoint of this track, and which of its ends is connected
Function GetAnotherTrackInPoint(Prim1 : IPCB_Track, X : TCoord, Y : TCoord, out OnFirstPoint : Boolean) : IPCB_Primitive;
var
    SIter : IPCB_SpatialIterator;
    Prim2 : IPCB_Track;
begin
    Result       := nil;
    OnFirstPoint := False;

    // Check if there is another track in hotspot
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
    SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
    SIter.AddFilter_Area(X - 1, Y - 1, X + 1, Y + 1);

    Prim2 := SIter.FirstPCBObject;
    While (Prim2 <> nil) do
    begin
        if (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and (not Prim2.TearDrop) then
        begin
            if (PointToPointDistance(Prim2.X1, Prim2.Y1, X, Y) <= 100) then
            begin
                Result       := Prim2;
                OnFirstPoint := True;
                Board.SpatialIterator_Destroy(SIter);
                exit;
            end;

            if (PointToPointDistance(Prim2.X2, Prim2.Y2, X, Y) <= 100) then
            begin
                Result := Prim2;
                Board.SpatialIterator_Destroy(SIter);
                exit;
            end;
        end;

        Prim2 := SIter.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(SIter);
end;


Function MoveTrackToIntercept(ThisTrackIndex : Integer, ConnectedTrackOneIndex : Integer, ConnectedTrackTwoIndex : Integer, TrimTrackIndex : Integer, TargetSlope : Double, TargetIntercept : TCoord, coef : Double, Reverse : Boolean, out LastIntercept : TCoord);
var
    k0, k1, k2                             : Double;    // k0 is throwaway
    c0, c1, c2                             : TCoord;    // c0, c1 are throwaway
    IsVert0, IsVert1, IsVert2              : Boolean;   // isVert0 is throwaway
    x01, x02, y01, y02                     : TCoord; // used for storing first line endpoints
    x11, x12, y11, y12                     : TCoord; // altium zapis koordinata
    x21, x22, y21, y22                     : TCoord;
    Prim0, Prim1, Prim2                    : IPCB_Primitive;
    X, Y                                   : TCoord;

begin
    Prim1 := SortedTracks.getObject(ThisTrackIndex);
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    if TrimPerpendicular then
    begin
        Prim0 := SortedTracks.getObject(TrimTrackIndex);
        SetupDataFromTrack(Prim0, IsVert0, x01, y01, x02, y02, k0, c0);
    end;

    Prim1.BeginModify;

    Prim2 := SortedTracks.getObject(ConnectedTrackOneIndex);    // track connected to first end of the moving track
    if (SortedTracks[ConnectedTrackOneIndex] <> '0') then
    begin // if there *is* a connected track on this end
        SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);
        if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
        begin // tracks intercept, X & Y are the point where they do
            // move this track's first point to the intercept point
            Prim1.X1 := X;
            Prim1.Y1 := Y;

            Prim2.BeginModify;
            if (SortedTracks[ConnectedTrackOneIndex] = '1') then // connected track was connected by its first point
            begin                           // move connected track's first point to the intercept point
                Prim2.X1 := X;
                Prim2.Y1 := Y;
            end
            else
            begin // move connected track's second point to the intercept point
                Prim2.X2 := X;
                Prim2.Y2 := Y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
        end;
    end
    else if TrimPerpendicular then
    begin // there was no track connected to the first point ('0' padded)
        if GetPerpendicularLine(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, x01, y01) then
        begin // GetPerpendicularLine returns true if perpendicular line is either vertical or horizontal
            if IsVert1 then Prim1.X1 := TargetIntercept // if IsVert1 is true then perpendicular line is horizontal (else X doesn't move anyway)
            else Prim1.Y1 := TargetSlope * Prim1.X1 + TargetIntercept;
        end
        else  // GetPerpendicularLine returns false if perpendicular line is neither vertical nor horizontal
        begin // extend/trim Prim1 to intercept virtual perpendicular line
            if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
            begin // tracks intercept, X & Y are the point where they do
                // move this track's first point to the intercept point with the perpendicular line
                Prim1.X1 := X;
                Prim1.Y1 := Y;
            end;
        end;
    end
    else // else TrimPerpendicular is False
    begin
        if IsVert1 then Prim1.X1 := TargetIntercept
        else Prim1.Y1 := TargetSlope * Prim1.X1 + TargetIntercept;
    end;

    Prim2 := SortedTracks.getObject(ConnectedTrackTwoIndex);    // track connected to second end of the moving track
    if (SortedTracks[ConnectedTrackTwoIndex] <> '0') then
    begin
        SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);

        if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
        begin
            Prim1.X2 := X;
            Prim1.Y2 := Y;

            Prim2.BeginModify;
            if (SortedTracks[ConnectedTrackTwoIndex] = '1') then
            begin
                Prim2.X1 := X;
                Prim2.Y1 := Y;
            end
            else
            begin
                Prim2.X2 := X;
                Prim2.Y2 := Y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
        end;
    end
    else if TrimPerpendicular then
    begin // there was no track connected to the first point ('0' padded)
        if GetPerpendicularLine(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, x02, y02) then
        begin // GetPerpendicularLine returns true if perpendicular line is either vertical or horizontal
            if IsVert1 then Prim1.X2 := TargetIntercept // if IsVert1 is true then perpendicular line is horizontal (else X doesn't move anyway)
            else Prim1.Y2 := TargetSlope * Prim1.X2 + TargetIntercept;
        end
        else  // GetPerpendicularLine returns false if perpendicular line is neither vertical nor horizontal
        begin // extend/trim Prim1 to intercept virtual perpendicular line
            if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
            begin // tracks intercept, X & Y are the point where they do
                // move this track's first point to the intercept point with the perpendicular line
                Prim1.X2 := X;
                Prim1.Y2 := Y;
            end;
        end;
    end
    else  // else TrimPerpendicular is False
    begin
        if IsVert1 then Prim1.X2 := TargetIntercept
        else Prim1.Y2 := TargetSlope * Prim1.X2 + TargetIntercept;
    end;

    Prim1.EndModify;
    Prim1.GraphicallyInvalidate;
    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim1.I_ObjectAddress);

    if Prim1.InNet then
    begin
        Prim1.Net.ConnectivelyInValidate;
    end;

    if Reverse then LastIntercept := TargetIntercept - (Prim1.Width / (2 * coef)) // subtract half of current track width for next pass (intercept at moved track edge)
    else LastIntercept := TargetIntercept + (Prim1.Width / (2 * coef)); // add half of current track width for next pass (intercept at moved track edge)

end;


Function DistributeForward(startc : TCoord, coef : Double, stepc : Tcoord);
var
    i                               : Integer;
    TrimTrackIndex                  : Integer;
    TargetSlope                     : Double;
    TargetIntercept, LastIntercept  : TCoord;
    IsVert1                         : Boolean;
    x11, x12, y11, y12              : TCoord;
    Prim1                           : IPCB_Primitive;

begin
    // start sliding tracks around
    i := 0;
    TrimTrackIndex := i;
    while i < SortedTracks.Count do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, TargetSlope, TargetIntercept);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := startc;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then
                TargetIntercept := (i / 3) * stepc + startc // if using centers, use step size directly
            else
                TargetIntercept := LastIntercept + stepc + Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, False, LastIntercept);

        i := i + 3; // advance to next track in sorted list
    end;
end;


Function DistributeBackward(startc : TCoord, coef : Double, stepc : Tcoord);
var
    i                               : Integer;
    TrimTrackIndex                  : Integer;
    TargetSlope                     : Double;
    TargetIntercept, LastIntercept  : TCoord;
    IsVert1                         : Boolean;
    x11, x12, y11, y12              : TCoord;
    Prim1                           : IPCB_Primitive;

begin
    // start sliding tracks around
    i := SortedTracks.Count - 3; // start at the end so we can work backward
    TrimTrackIndex := i;
    while i >= 0 do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, TargetSlope, TargetIntercept);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := startc;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then
                TargetIntercept := startc + ((3 + i - SortedTracks.Count) / 3) * stepc // if using centers, use step size directly
            else
                TargetIntercept := LastIntercept - stepc - Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, True, LastIntercept);

        i := i - 3; // jump back to the preceding track in the sorted list
    end;
end;


Function DistributeFromCenter(startc : TCoord, coef : Double, stepc : Tcoord);
var
    i                               : Integer;
    TrimTrackIndex                  : Integer;
    ListSplitIndex                  : Integer;
    TargetSlope                     : Double;
    TargetIntercept, LastIntercept  : TCoord;
    SplitIntercept                  : TCoord;
    IsVert1                         : Boolean;
    x11, x12, y11, y12              : TCoord;
    Prim1                           : IPCB_Primitive;

begin
    // first, find index to split track list for forward and backward operations
    // odd number of tracks means center will be a track
    if (SortedTracks.Count mod 6) <> 0 then
    begin   // odd track count
        // need to set middle track to center
        // 3 tracks (9 elements) should split at track 2 (i=3) and distribute 1 track forward and 1 back from center            0 < 3* > 6
        // 5 tracks (15 elements) should split at track 3 (i=6) and distribute 2 tracks forward and 2 back from center      0 < 3 < 6* > 9 > 12
        // 7 tracks (21 elements) should split at track 4 (i=9) and distribute 3 tracks forward and 3 back from center  0 < 3 < 6 < 9* > 12 > 15 > 18
        ListSplitIndex := (SortedTracks.Count - 3) div 2;
        SplitIntercept := startc;
    end
    else
    begin   // even track count
        // middle two tracks need to straddle center
        // 2 tracks (6 elements) should split at track 1 (i=0) and distribute 1 track forward                           0* > 3
        // 4 tracks (12 elements) should split at track 2 (i=3) and distribute 3 tracks forward and 1 back          0 < 3* > 6 > 9
        // 6 tracks (18 elements) should split at track 3 (i=6) and distribute 4 tracks forward and 2 back      0 < 3 < 6* > 9 > 12 > 15
        // 8 tracks (24 elements) should split at track 4 (i=9) and distribute 5 tracks forward and 3 back  0 < 3 < 6 < 9* > 12 > 15 > 18 > 21
        ListSplitIndex := (SortedTracks.Count div 2) - 3;
        Prim1 := SortedTracks.getObject(ListSplitIndex);    // need to grab anchor track to compensate for its width
        SplitIntercept := startc - (stepc / 2) - Prim1.Width / (2 * coef);
    end;

     // start sliding tracks around (forward section)
    i := ListSplitIndex;
    TrimTrackIndex := ListSplitIndex;
    while i < SortedTracks.Count do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, TargetSlope, TargetIntercept);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := SplitIntercept;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then
                TargetIntercept := (i / 3) * stepc + startc // if using centers, use step size directly
            else
                TargetIntercept := LastIntercept + stepc + Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, False, LastIntercept);

        i := i + 3; // advance to next track in sorted list
    end;

    // now start sliding tracks around (backward section)
    i := ListSplitIndex;
    TrimTrackIndex := ListSplitIndex;
    while i >= 0 do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, TargetSlope, TargetIntercept);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := SplitIntercept;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then
                TargetIntercept := startc + ((3 + i - SortedTracks.Count) / 3) * stepc // if using centers, use step size directly
            else
                TargetIntercept := LastIntercept - stepc - Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, True, LastIntercept);

        i := i - 3; // jump back to the preceding track in the sorted list
    end;
end;


Procedure InitialCheck(Var status : Integer);
var
    i                  : Integer;
    Prim1              : IPCB_Primitive;
    k1, k2             : Double;
    c1, c2,            : TCoord;
    IsVert1            : Boolean;
    IsVert2            : Boolean;
    x11, x12, y11, y12 : TCoord; // altium zapis koordinata
    x21, x22, y21, y22 : TCoord;
begin
    status := 0; // clear result status
    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
        exit;
    // testiramo da li otvren trenutb aktivan PCB doc

    // need to deselect anything that isn't an electrical track as these will cause errors elsewhere
    i := 0;
    While i < Board.SelectecObjectCount do
    begin
        Prim1 := Board.SelectecObject[i];
        if ((Prim1.ObjectId <> eTrackObject) Or (Not Prim1.InNet)) then
            Prim1.SetState_Selected(False)
        else
            i := i + 1; // advance iterator if current object remains selected
    end;


    If Board.SelectecObjectCount < 2 then
    begin
        Showmessage('Select at least 2 tracks that belong to nets');
        status := 1;
        exit;
    end;


    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];

        if (Prim1.ObjectId <> eTrackObject) then
        begin
            Showmessage('Select only tracks');
            status := 1;
            exit;
        end;
    end;

    Prim1 := Board.SelectecObject[0];
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    // check that all selected tracks are parallel with the first one
    for i := 1 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

        if ((IsVert1 <> IsVert2) or (Abs(k1 - k2) > 0.01)) then
        begin
            Showmessage('Selected tracks have to be parallel.');
            status := 1;
            exit;
        end;
    end;
end;


// main procedure to distribute tracks
procedure calculate(dummy : Integer = 0);
var
    i, j                                   : Integer;
    k1, k2                                 : Double;
    c1, c2, minc, midc, maxc, stepc, cFromWidths : TCoord;
    IsVert1                                : Boolean;
    IsVert2                                : Boolean;
    IsFirstPoint                           : Boolean;
    x01, x02, y01, y02                     : TCoord; // used for storing first line endpoints
    x11, x12, y11, y12                     : TCoord; // altium zapis koordinata
    x21, x22, y21, y22                     : TCoord;
    Prim1                                  : IPCB_Primitive;
    Prim2                                  : IPCB_Primitive;
    LastAdded                              : IPCB_Primitive;
    MaxNumOfChar                           : Integer; // najvci broj znamenki u stringu
    NumOfChar                              : Integer; // broj znamenki
    TempString                             : string;
    X, Y                                   : TCoord;
    coef                                   : Double;
    TempPresetList                         : TStringList;

begin
    if (Board <> PCBServer.GetCurrentPCBBoard) then
    begin
        Showmessage('Please start script using START procedure');
        close;
        exit;
    end;

    TrimPerpendicular := False; // hard coded off until I figure out how to fix the rounding error (eg. 15.002 instead of 15)
    // I suspect the track should be trimmed to *approximately* perpendicular, prioritizing the clearance over exact perpendicularity.

    // seeks min and max c i.e. range of intercept values (trazi min i max C tj grnice sirenja   vodova)
    // Board.NewUndo;
    // Start undo
    PCBServer.PreProcess;
    Prim1 := Board.SelectecObject[0];
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    minc        := c1;
    maxc        := c1;
    coef        := cos(arctan(k1));          // y-intercept coefficient based on slope (i.e. how much intercept shifts for a perpendicular shift of the track)
    cFromWidths := Prim1.Width / (2 * coef); // start cFromWidths with half of the first track's width

    // calculate extents of intercept values and sum of track widths in intercept units
    for i := 1 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

        if (minc > c2) then
            minc := c2;
        if (maxc < c2) then
            maxc := c2;

        cFromWidths := cFromWidths + Prim1.Width / coef; // add subsequent track widths
    end;

    cFromWidths := cFromWidths - Prim1.Width / (2 * coef); // subtract half of last track's width

    midc := (minc + maxc) div 2;    // average but floored to nearest multiple of 10

    // we have to sort the tracks in order because they are random (moramo srediti trackove po redu jer su izavrani random)
    SortedTracks := TStringList.Create;

    // SortedTracks.LoadFromFile('C:\');
    // SortedTracks.Count

    // populate the initial list (punjenje pocetne liste)
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

        TempString := IntToStr(c2);

        if c2 > 0 then
            TempString := '+' + TempString;

        SortedTracks.AddObject(TempString, Prim1); // store track object using intercept as key
    end;

    // pad strings with leading zeros for sort function (Punjenje stringova nulama   radi jednakosti)
    MaxNumOfChar := 0;
    for i        := 0 to SortedTracks.Count - 1 do
    begin
        NumOfChar := Length(SortedTracks.Get(i));
        if (MaxNumOfChar < NumOfChar) then
            MaxNumOfChar := NumOfChar;
    end;


    for i := 0 to SortedTracks.Count - 1 do
    begin
        TempString := SortedTracks[i];

        while (Length(TempString) < MaxNumOfChar) do
            Insert('0', TempString, 2);

        SortedTracks[i] := TempString;
    end;

    SortedTracks.Sort;

    // ShowMessage (SortedTracks[0][1]);  // displays the first member "0" and a string in its 1 position (prikazuje prvi clan "0" i string na njegovom 1 mjestu)
    if (SortedTracks[SortedTracks.Count - 1][1] = '-') then
    begin // if last track in sorted list has negative intercept
        i := 0;

        // advance i to last positive intercept
        While (SortedTracks[i][1] = '+') do
            Inc(i);

        j := 0;

        While (i < SortedTracks.Count) do
        begin // move negative intercepts to beginning of list (before positive intercepts)
            SortedTracks.Move(SortedTracks.Count - 1, j);
            Inc(j);
            Inc(i);
        end;

    end;


    {
        // Test case if all is good until now
        for i := 0 to SortedTracks.Count - 1 do
        begin
        Prim1 := SortedTracks.GetObject(i);
        Prim1.Selected := False;

        ShowMessage(SortedTracks[i]);

        Prim1.Selected := True;
        end;
    }

    // Add connected tracks to the track list, or pad the list if there is no track connected to a given end
    // (Dio koji puni object listu sa susjednim trackovima ako je track onda u listu ide YES a noj stavlja NO)
    i := 0;
    ResetParameters;
    AddStringParameter('Scope', 'All');
    RunProcess('PCB:DeSelect');
    while i < SortedTracks.Count do
    begin
        Prim1 := SortedTracks.getObject(i);
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

        // look for a track (call it track 1a) that is connected to this track's first point and insert it after this track in the list (provjera za prvu tocku i umece 1a liniju ispod 1 linije tracka)
        Prim2 := GetAnotherTrackInPoint(Prim1, Prim1.X1, Prim1.Y1, IsFirstPoint);

        if (Prim2 = nil) then
            SortedTracks.Insert(i + 1, '0') // no connected track so insert placeholder
        else
        begin // there is a connected track, validate it
            SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);
            // test whether there are any problems on the track e.g. two segments are parallel (test da li ima nekih problema na tracku npr paralelna dve u nastavku)
            // i.e. there must be a single contiguous track segment (tj mora biti jedan jedini track)
            if ((IsVert1 = IsVert2) and (Abs(k1 - k2) < 0.01)) then
            begin // connected segment is parallel
                Prim1.Selected := True;
                Prim2.Selected := True;
                Prim1.GraphicallyInvalidate;
                Prim2.GraphicallyInvalidate;

                Showmessage('Problem on selected tracks (connected tracks are parallel - try glossing?)');
            end;

            // insert the connected track keyed by which of its ends is connected
            if IsFirstPoint then
                SortedTracks.InsertObject(i + 1, '1', Prim2)
            else
                SortedTracks.InsertObject(i + 1, '2', Prim2);
        end;

        // look for another track (call it track 1b) that is connected to this track's second point and insert it after track 1a in the list.
        // this makes a total of 3 lines: the track of interest and the two tracks connected to it, if any (Provjera za drugu tocku i umece 1b ispod linije 1a. Ukupno 3 linije)
        Prim2 := GetAnotherTrackInPoint(Prim1, Prim1.X2, Prim1.Y2, IsFirstPoint);

        if (Prim2 = nil) then
            SortedTracks.Insert(i + 2, '0') // no connected track so insert placeholder
        else
        begin // there is a connected track, validate it
            SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);

            if ((IsVert1 = IsVert2) and (Abs(k1 - k2) < 0.01)) then
            begin // connected segment is parallel
                Prim1.Selected := True;
                Prim2.Selected := True;
                Prim1.GraphicallyInvalidate;
                Prim2.GraphicallyInvalidate;

                Showmessage('Problem on selected tracks (connected tracks are parallel - try glossing?)');
            end;

            // insert the connected track keyed by which of its ends is connected
            if IsFirstPoint then
                SortedTracks.InsertObject(i + 2, '1', Prim2)
            else
                SortedTracks.InsertObject(i + 2, '2', Prim2);
        end;

        i := i + 3; // jump ahead to next track of interest
    end;

    // there shouldn't be any tracks selected unless there were problems
    If Board.SelectecObjectCount <> 0 then
        exit;

    if RadioButtonCenters.Checked then
    begin
        stepc := (maxc - minc) / ((SortedTracks.Count / 3) - 1); // intercept increment is intercept extents divided by number of tracks - 1
        DistributeForward(minc, coef, stepc);
    end
    else if RadioButtonClearance.Checked then
    begin
        stepc := (maxc - minc - cFromWidths) / ((SortedTracks.Count / 3) - 1); // intercept increment is same as above but subtracting sum of track widths from intercept extents
        DistributeForward(minc, coef, stepc);
    end
    else
    begin // distributing using input value rather than between intercept extents
        TempString := EditDistance.Text;
        if (LastDelimiter(',.', TempString) <> 0) then
            TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

        // convert desired intercept step using intercept coefficient and selected units
        if (ButtonUnits.Caption = 'mm') then
            stepc := mmsToCoord(StrToFloat(TempString)) / (coef)
        else
            stepc := milsToCoord(StrToFloat(TempString)) / (coef);

        // CALL FUNCTION to actually move the tracks
        case RadioDirections.ItemIndex of
            0 : DistributeForward(minc, coef, stepc);
            1 : DistributeFromCenter(midc, coef, stepc);
            2 : DistributeBackward(maxc, coef, stepc);
        else DistributeForward(minc, coef, stepc);
        end;
   end;


    // Stop undo
    PCBServer.PostProcess;

    // build list of currect preset values
    TempPresetList := TStringList.Create;
    TempPresetList.Add(EditDistance.Text);
    TempPresetList.Add(tPreset1.Text);
    TempPresetList.Add(tPreset2.Text);
    TempPresetList.Add(tPreset3.Text);
    TempPresetList.Add(tPreset4.Text);
    TempPresetList.Add(tPreset5.Text);
    TempPresetList.Add(tPreset6.Text);
    TempPresetList.Add(tPreset7.Text);
    TempPresetList.Add(tPreset8.Text);
    TempPresetList.Add(RadioDirections.ItemIndex);
    TempPresetList.Add(RadioButtonClearance.Checked);
    TempPresetList.Add(RadioButtonCenters.Checked);
    TempPresetList.Add(RadioButtonClearanceVal.Checked);
    TempPresetList.Add(RadioButtonCentersVal.Checked);

    If TempPresetList.Equals(PresetList) then
    Begin
        // presets match saved list so do nothing
    End
    Else
    Begin
        // save new list to MyDistributePresets.txt
        TempPresetList.SaveToFile(PresetFilePath);
    End;

    // cleanup
    PresetList.Free;
    TempPresetList.Free;

    close;
end;


function EnableByValControls(NewEnable : Boolean);
begin
    ButtonPreset1.Enabled           := NewEnable;
    ButtonPreset2.Enabled           := NewEnable;
    ButtonPreset3.Enabled           := NewEnable;
    ButtonPreset4.Enabled           := NewEnable;
    ButtonPreset5.Enabled           := NewEnable;
    ButtonPreset6.Enabled           := NewEnable;
    ButtonPreset7.Enabled           := NewEnable;
    ButtonPreset8.Enabled           := NewEnable;
    tPreset1.Enabled                := NewEnable;
    tPreset2.Enabled                := NewEnable;
    tPreset3.Enabled                := NewEnable;
    tPreset4.Enabled                := NewEnable;
    tPreset5.Enabled                := NewEnable;
    tPreset6.Enabled                := NewEnable;
    tPreset7.Enabled                := NewEnable;
    tPreset8.Enabled                := NewEnable;
    ButtonUnits.Enabled             := NewEnable;
    RadioDirections.Enabled         := NewEnable;
    EditDistance.Enabled            := NewEnable;

    if NewEnable then
    begin
        EditDistance.SetFocus;
        EditDistance.SelectAll;
    end;
end;


function GetPresetButtonEnable : Boolean;
begin
    Result := ButtonPreset1.Enabled;
end;


function IsStringANum(Text : String) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;
    // Test for number, dot or comma
    ChSet := SetUnion(MkSet(Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
    for i := 1 to Length(Text) do
        if not InSet(Ord(Text[i]), ChSet) then
            Result := False;

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet    := MkSet(Ord('.'), Ord(','));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then
            Inc(dotCount);

    if dotCount > 1 then
        Result := False;
end;


procedure TFormDistribute.ButtonUnitsClick(Sender : TObject);
var
    TempString : String;
begin
    TempString := EditDistance.Text;
    if (LastDelimiter(',.', TempString) <> 0) then
        TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    If ButtonUnits.Caption = 'mil' then
    begin
        ButtonUnits.Caption := 'mm';
        EditDistance.Text   := CoordToMMs(milsToCoord(StrToFloat(TempString)));
    end
    else
    begin
        ButtonUnits.Caption := 'mil';
        EditDistance.Text   := CoordToMils(mmsToCoord(StrToFloat(TempString)));
    end;
    EditDistance.SetFocus;
end;


procedure TFormDistribute.EditDistanceChange(Sender : TObject);
begin

    If IsStringANum(EditDistance.Text) then
    begin
        EditDistance.Font.Color := clWindowText;
        ButtonOK.Enabled        := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        EditDistance.Font.Color := clRed;
    end;
end;


procedure TFormDistribute.ButtonCancelClick(Sender : TObject);
begin
    close;
end;


procedure TFormDistribute.RadioButtonCentersClick(Sender : TObject);
begin
    EnableByValControls(False);
end;


procedure TFormDistribute.RadioButtonClearanceClick(Sender : TObject);
begin
    EnableByValControls(False);
end;


procedure TFormDistribute.RadioButtonCentersValClick(Sender : TObject);
begin
    EnableByValControls(True);
    EditDistance.SetFocus;
end;


procedure TFormDistribute.RadioButtonClearanceValClick(Sender : TObject);
begin
    EnableByValControls(True);
    EditDistance.SetFocus;
end;


Procedure FastDistributeByCenterline;
var
    status : Integer;
begin
    InitialCheck(status);
    if status = 0 then
    begin
        calculate;
    end
    else
        exit;
end;


Procedure FastDistributeByClearance;
var
    status : Integer;
begin
    InitialCheck(status);
    if status = 0 then
    begin
        RadioButtonClearance.Checked := True;
        calculate;
    end
    else
        exit;
end;


Procedure Start;
var
    status : Integer;
begin
    InitialCheck(status);
    if status = 0 then
    begin
        PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyDistributePresets.txt';
        FormDistribute.ShowModal;
    end
    else
        exit;
end;


procedure TFormDistribute.ButtonOKClick(Sender : TObject);
begin
    calculate;
end;


procedure TFormDistribute.FormDistributeShow(Sender : TObject);
begin
    // Set direction control hint
    Application.HintHidePause := 11000; // extend hint show time
    RadioDirections.Hint := 'FWD: The original direction that the distribute script spreads tracks.'
        + sLineBreak
        + 'CEN: Redistributes tracks from the center of extents. For example, a pair of tracks will move symmetrically.'
        + sLineBreak
        + 'REV: Will reverse the direction of distribution i.e. what would normally be the last track is instead the first track.';

    // read from MyDistributePresets.txt
    PresetList := TStringList.Create;
    If FileExists(PresetFilePath) then
    Begin
        PresetList.LoadFromFile(PresetFilePath); // load presets from file if it exists

        // if PresetList file exists but count is short, just regenerate preset file from defaults
        If PresetList.Count < NumPresets then
        Begin
            // ShowMessage(PresetFilePath + ' exists but is not the correct length. Defaults will be used.');
            PresetList.Clear;
            PresetList.Add(EditDistance.Text);                  // PresetList[0]
            PresetList.Add(tPreset1.Text);                      // PresetList[1]
            PresetList.Add(tPreset2.Text);                      // PresetList[2]
            PresetList.Add(tPreset3.Text);                      // PresetList[3]
            PresetList.Add(tPreset4.Text);                      // PresetList[4]
            PresetList.Add(tPreset5.Text);                      // PresetList[5]
            PresetList.Add(tPreset6.Text);                      // PresetList[6]
            PresetList.Add(tPreset7.Text);                      // PresetList[7]
            PresetList.Add(tPreset8.Text);                      // PresetList[8]
            PresetList.Add(RadioDirections.ItemIndex);          // PresetList[9]
            PresetList.Add(RadioButtonClearance.Checked);       // PresetList[10]
            PresetList.Add(RadioButtonCenters.Checked);         // PresetList[11]
            PresetList.Add(RadioButtonClearanceVal.Checked);    // PresetList[12]
            PresetList.Add(RadioButtonCentersVal.Checked);      // PresetList[13]
            PresetList.SaveToFile(PresetFilePath);
        End;

        // set text boxes to match preset list (redundant if list was regenerated above
        // ShowMessage('Loading presets from ' + PresetFilePath);
        tPreset1.Text                   := PresetList[1];
        tPreset2.Text                   := PresetList[2];
        tPreset3.Text                   := PresetList[3];
        tPreset4.Text                   := PresetList[4];
        tPreset5.Text                   := PresetList[5];
        tPreset6.Text                   := PresetList[6];
        tPreset7.Text                   := PresetList[7];
        tPreset8.Text                   := PresetList[8];
        RadioDirections.ItemIndex       := PresetList[9];
        RadioButtonClearance.Checked    := PresetList[10];
        RadioButtonCenters.Checked      := PresetList[11];
        RadioButtonClearanceVal.Checked := PresetList[12];
        RadioButtonCentersVal.Checked   := PresetList[13];
        EditDistance.Text               := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    End
    Else
    Begin // if preset file didn't exist at all, create from defaults
        // ShowMessage(PresetFilePath + ' does not exist.');
        PresetList.Clear;
        PresetList.Add(EditDistance.Text);                  // PresetList[0]
        PresetList.Add(tPreset1.Text);                      // PresetList[1]
        PresetList.Add(tPreset2.Text);                      // PresetList[2]
        PresetList.Add(tPreset3.Text);                      // PresetList[3]
        PresetList.Add(tPreset4.Text);                      // PresetList[4]
        PresetList.Add(tPreset5.Text);                      // PresetList[5]
        PresetList.Add(tPreset6.Text);                      // PresetList[6]
        PresetList.Add(tPreset7.Text);                      // PresetList[7]
        PresetList.Add(tPreset8.Text);                      // PresetList[8]
        PresetList.Add(RadioDirections.ItemIndex);          // PresetList[9]
        PresetList.Add(RadioButtonClearance.Checked);       // PresetList[10]
        PresetList.Add(RadioButtonCenters.Checked);         // PresetList[11]
        PresetList.Add(RadioButtonClearanceVal.Checked);    // PresetList[12]
        PresetList.Add(RadioButtonCentersVal.Checked);      // PresetList[13]
        PresetList.SaveToFile(PresetFilePath);
    end;

    if Board.SelectecObjectCount = 2 then
    begin
        RadioButtonClearance.Enabled    := False;
        RadioButtonCenters.Enabled      := False;
        RadioButtonClearanceVal.Checked := True;
        EnableByValControls(True);
    end
    else if (RadioButtonClearanceVal.Checked or RadioButtonCentersVal.Checked) then
    begin
         EnableByValControls(True);
    end
    else EnableByValControls(False);

end;


procedure ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    // ShowMessage(textbox.Text);
    if IsStringANum(textbox.Text) then
    begin
        If Sender <> EditDistance then
            EditDistance.Text := textbox.Text;
        ButtonOK.Enabled    := True;
    end
    else
        ButtonOK.Enabled := False;

end;


procedure UserKeyPress(Sender : TObject; var Key : Char); // programmatically, OnKeyPress fires before OnChange event and "catches" the key press
begin
    if (ButtonOK.Enabled) And (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if GetPresetButtonEnable then
            calculate(0);
    end;
end;


procedure PresetButtonClicked(Sender : TObject);
begin
    // ShowMessage('PresetButtonClicked');
    If Sender = ButtonPreset1 then
        EditDistance.Text := tPreset1.Text
    Else If Sender = ButtonPreset2 then
        EditDistance.Text := tPreset2.Text
    Else If Sender = ButtonPreset3 then
        EditDistance.Text := tPreset3.Text
    Else If Sender = ButtonPreset4 then
        EditDistance.Text := tPreset4.Text
    Else If Sender = ButtonPreset5 then
        EditDistance.Text := tPreset5.Text
    Else If Sender = ButtonPreset6 then
        EditDistance.Text := tPreset6.Text
    Else If Sender = ButtonPreset7 then
        EditDistance.Text := tPreset7.Text
    Else If Sender = ButtonPreset8 then
        EditDistance.Text := tPreset8.Text;
    calculate(1);
end;


procedure TFormDistribute.RadioDirectionsClick(Sender: TObject);
begin
    if (GetPresetButtonEnable and FormDistribute.Active) then EditDistance.SetFocus;
end;
