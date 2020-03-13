{ Version 0.1 (12/1/2009)
   -- initial implemenation - Ben Benardos

v0.2 05/08/2019 BLM  Add 2nd loop to the Pick; 2 <ESC> to exit
v0.3 09/08/2019      Faster unique union index, set statusbar text.

}
Const
   MyTrkWidth = 10;    // width of dashed line
   MyDashLen  = 10;    // spacing of dashes ??

Var
    Board       : IPCB_Board;
    IsSpace     : Boolean;
    I           : Integer;
    TrackWidth  : TCoord;
    TrackLayer  : TLayer;
    Pattern     : TList;
    PatternUnit : TCoord;

Function SetPattern_Dash(const Scale : TCoord);
Begin
     Pattern.Clear;
     Pattern.Add(3);
     Pattern.Add(2);
     //Pattern.Add(1);
     //Pattern.Add(1);
     PatternUnit := Scale;
End;

Function SetPattern_DashDot(const Scale : TCoord);
Begin
     Pattern.Clear;
     Pattern.Add(10);
     Pattern.Add(5);
     Pattern.Add(1);
     Pattern.Add(5);
     PatternUnit := Scale;
End;

Function Length(x, y : Double) : Double;
Var
   d : Double;
Begin
     // workaround for what seems to be wrapping
     x := x / k1Mil;
     y := y / k1Mil;
     d := x * x + y * y;
     Result := Sqrt(d) * k1Mil;
End;

function AddTrack(P1X, P1Y, P2X, P2Y : TCoord) : IPCB_Track;
Begin
    Result    := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Result.X1 := P1X;
    Result.Y1 := P1Y;
    Result.X2 := P2X;
    Result.Y2 := P2Y;
    Result.Layer := TrackLayer;
    Result.Width := TrackWidth;
    Board.AddPCBObject(Result)
End;

Procedure Interpolate(P1X, P1Y, P2X, P2Y : TCoord; t : Double, Var X, Y : TCoord);
Var
    dx, dy, l, ux, uy : Double;
Begin
    dx := p2X - p1X;
    dy := p2Y - p1Y;
    l := Length(dx, dy);
    ux := dx / l;
    uy := dy / l;

    x := Round(p1X + ux * t);
    y := Round(p1y + uy * t);
End;

Procedure GetDashLength(I : Integer; Var DashLength : Double);
Begin
    I := I Mod Pattern.Count;
    DashLength := Pattern[I] * PatternUnit;
End;

Procedure AddDashes(P1X, P1Y, P2X, P2Y : TCoord; Var tRemainder : Double, UnionIndex : integer);
Var
    tMax, tStart, tEnd : Double;
    tChange : Double;
    StartPointX, StartPointY, EndPointX, EndPointY : TCoord;
    Go      : Boolean;
    Track   : IPCB_Track;

Begin
    tMax := Length(p1x - p2x, p1y - p2y);

    tStart := 0;
    Go := True;
    While Go Do
    Begin
        if tRemainder > 0 Then
        Begin
            tChange := tRemainder;
            tRemainder := 0;
        End
        Else
            GetDashLength(I, tChange);
        
        tEnd := tStart + tChange;
        
        if tEnd > tMax then
        Begin
            tRemainder := tEnd - tMax;
            tEnd := tMax;
            Go := False;
        end;

        if Not IsSpace then
        Begin
            Interpolate(P1X, P1Y, P2X, P2Y, tStart, StartPointX, StartPointY);
            Interpolate(P1X, P1Y, P2X, P2Y, tEnd,   EndPointX,   EndPointY);
            Track := AddTrack(StartPointX, StartPointY, EndPointX, EndPointY);
            Track.UnionIndex := UnionIndex;
        End;

        tStart := tEnd;

        If Go Then
        Begin
            IsSpace := Not IsSpace;
            Inc(I);
        End;
    End;
End;

Function  DoesUnionIndexExist(Index : Integer) : Boolean;
Var
   Iterator  : IPCB_BoardIterator;
   Primitive : IPCB_Primitive;

Begin
    Result := False;

    Iterator := Board.BoardIterator_Create;
//    Iterator.SetState_FilterAll;   // slow
    Iterator.AddFilter_ObjectSet(AllPrimitives);

    Primitive := Iterator.FirstPCBObject;
    While (Primitive <> Nil) Do
    Begin
        If Primitive.UnionIndex = Index Then
        Begin
           Result := True;
           Break;
        End;

        Primitive := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
End;

Procedure SetupUnionIndex(var UnionIndex : Integer);
Begin
     // Randomize;
     Repeat
      //   UnionIndex := Random(2147483647);
         UnionIndex := GetHashID_ForString(GetWorkSpace.DM_GenerateUniqueID);
     Until Not DoesUnionIndexExist(UnionIndex);
End;

procedure MakeLines;
Var
    P1X, P1Y, P2X, P2Y : TCoordPoint;
    tRemainder : Double;
    UnionIndex : Integer;

Begin
     Board := PCBServer.GetCurrentPCBBoard;
     If Board = Nil Then
     Begin
          ShowError('Current document is not a PCB');
          Exit;
     End;

     Pattern := TList.Create;
     TrackWidth :=  MilsToCoord(MyTrkWidth);
     SetPattern_Dash(MilsToCoord(MyDashLen));
     // SetPattern_DashDot(MyDashLen));

    While Board.ChooseLocation(P1X, P1Y, 'Pick Start Point') do
    begin
        TrackLayer := Board.CurrentLayer;
        tRemainder := 0;
        I := 0;
        IsSpace := False;

        SetupUnionIndex(UnionIndex);

        Pcbserver.PreProcess;
        while Board.ChooseLocation(P2X, P2Y, 'Pick corner or End') do
        begin
            AddDashes(P1X, P1Y, P2X, P2Y, tRemainder, UnionIndex);
//            Board.GraphicalView_ZoomRedraw;
            P1X := P2X;
            P1Y := P2Y;
        end;
        Pcbserver.PostProcess;
        Board.GraphicalView_ZoomRedraw;
    end;
    Pattern.Clear;
    Pattern.Free;
end;
