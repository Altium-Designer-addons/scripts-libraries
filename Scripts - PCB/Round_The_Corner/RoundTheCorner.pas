uses
  Winapi, ShellApi, Win32.NTDef, Windows, Messages, SysUtils, Classes,
  Graphics,
  Controls, Forms, Dialogs, System, System.Diagnostics, Math;

var
  Options: array [0 .. 5] of Boolean;
  Chord: TCoord;

function DetectAngle(V: Single; maxdelta: Single = 10): Integer;
var
  i, ii: Integer;
  delta: Single;
  mindelta: Single;
  angle: Single;
begin
  mindelta := 100000;
  while V < 0 do
    V := V + 360;
  while V >= 360 do
    V := V - 360;
  for i := 0 to 4 do
  begin
    angle := i * 90;
    delta := abs(angle - V);
    if delta < mindelta then
    begin
      ii := i mod 4;
      mindelta := delta;
    end;
  end;
  Result := ii * 90;
  if maxdelta < mindelta then
    Result := -1;
end;

function CrossProduct(x1, y1, x2, y2: Single): Single;
begin
  Result := x1 * y2 - y1 * x2;
end;

procedure AddSegment(Polygon: IPCB_Polygon; Segment: TPolySegment;
  Index: Integer);
var
  tmp: TPolySegment;
  i: Integer;
begin
  Polygon.PointCount := Polygon.PointCount + 1;
  for i := Polygon.PointCount - 1 downto Index + 1 do
  begin
    tmp := Polygon.Segments[i - 1];
    Polygon.Segments[i] := tmp;
  end;

  tmp := Segment;
  Polygon.Segments[Index] := tmp;
end;

procedure DeleteSegment(Polygon: IPCB_Polygon; Index: Integer);
var
  tmp: TPolySegment;
  i: Integer;
begin
  for i := Index to Polygon.PointCount - 2 do
  begin
    tmp := Polygon.Segments[i + 1];
    Polygon.Segments[i] := tmp;
  end;
  Polygon.PointCount := Polygon.PointCount - 1;
end;

function VectorAngle(x1, y1, x2, y2: Single): Single;
var
  dot, det: Single;
begin
  dot := x1 * x2 + y1 * y2;
  det := x1 * y2 - y1 * x2;
  Result := ArcTan2(det, dot);
end;

function VectorLength(x, y: Single): Single;
begin
  Result := sqrt(x * x + y * y);
end;

function Intersect(line1V1X, line1V1Y, line1V2X, line1V2Y, line2V1X, line2V1Y,
  line2V2X, line2V2Y: Single; var x, y: Single): Boolean;
var
  A1, B1, C1: Single;
  A2, B2, C2: Single;
  det: Single;
begin
  // Line1
  A1 := line1V2Y - line1V1Y;
  B1 := line1V1X - line1V2X;
  C1 := A1 * line1V1X + B1 * line1V1Y;

  // Line2
  A2 := line2V2Y - line2V1Y;
  B2 := line2V1X - line2V2X;
  C2 := A2 * line2V1X + B2 * line2V1Y;

  det := A1 * B2 - A2 * B1;
  if (det = 0) then
  begin
    Result := false; // parallel lines
  end
  else
  begin
    x := (B2 * C1 - B1 * C2) / det;
    y := (A1 * C2 - A2 * C1) / det;
    Result := true; // parallel lines
  end;
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

  Result := Rect;
end;

function Is_Near_Board(Board: IPCB_Board; vx, vy: Single): Boolean;
var
  BoardRect: TCoordRect;
  eps: Single;
  dx1, dx2, dy1, dy2: Single;
begin
  eps := 0.001;

  BoardRect := Get_Obj_Rect(Board);
  dx1 := abs(vx - CoordToMMs(BoardRect.Left));
  dx2 := abs(vx - CoordToMMs(BoardRect.Right));
  dy1 := abs(vy - CoordToMMs(BoardRect.Top));
  dy2 := abs(vy - CoordToMMs(BoardRect.Bottom));

  if (dx1 < eps) or (dx2 < eps) or (dy1 < eps) or (dy2 < eps) then
  begin
    Result := true;
    Exit; // return
  end;

  Result := false;
end;

procedure ProcessBevel(Polygon: IPCB_Polygon; PolygonRpt: TStringList);
var
  i: Integer;
  PolySeg: TPolySegment;
  ncx, ncy: TCoord;
  nvx, nvy: TCoord;
  r, nr: TCoord;
  A1, A2: Integer;
  I1: Integer;
  cp: Single;
  v1x, v1y, v2x, v2y, v3x, v3y: Single;
  a, a_1, a_2, a_3: Single;
  PrevI, NextI, NextNextI: Integer;
  _chord, mmnr: Single;
  l1, l2, l3: Single;
  MinAngle: Single;
  MinEdgeLengthMMs: Single;
begin
  _chord := CoordToMMs(Chord);
  mmnr := _chord / sqrt(2);
  nr := MMsToCoord(mmnr);

  MinAngle := 10;
  MinEdgeLengthMMs := _chord * 2.5;
  i := 0;
  while (i < Polygon.PointCount) do
  begin
    PrevI := (i - 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextI := (i + 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextNextI := (i + 2 + Polygon.PointCount) mod Polygon.PointCount;

    v1x := CoordToMMs(Polygon.Segments[i].vx) -
      CoordToMMs(Polygon.Segments[PrevI].vx);
    v1y := CoordToMMs(Polygon.Segments[i].vy) -
      CoordToMMs(Polygon.Segments[PrevI].vy);

    v2x := CoordToMMs(Polygon.Segments[NextI].vx) -
      CoordToMMs(Polygon.Segments[i].vx);
    v2y := CoordToMMs(Polygon.Segments[NextI].vy) -
      CoordToMMs(Polygon.Segments[i].vy);

    v3x := CoordToMMs(Polygon.Segments[NextNextI].vx) -
      CoordToMMs(Polygon.Segments[NextI].vx);
    v3y := CoordToMMs(Polygon.Segments[NextNextI].vy) -
      CoordToMMs(Polygon.Segments[NextI].vy);

    l2 := VectorLength(v2x, v2y);

    a_1 := VectorAngle(v1x, v1y, 1, 0);
    a_2 := VectorAngle(v2x, v2y, 1, 0);
    a_3 := VectorAngle(v3x, v3y, 1, 0);

    a_2 := (a_2 / Pi * 180);

    a_1 := DetectAngle(a_1 / Pi * 180, MinAngle);
    a_3 := DetectAngle(a_3 / Pi * 180, MinAngle);

    PolygonRpt.Add(' ----------------------');
    PolygonRpt.Add(' A1: ' + FloatToStr(a_1));
    PolygonRpt.Add(' A2: ' + FloatToStr(a_2));
    PolygonRpt.Add(' A3: ' + FloatToStr(a_3));
    PolygonRpt.Add(' L: ' + FloatToStr(l2));

    _chord := CoordToMMs(Chord);
    mmnr := _chord / sqrt(2);
    nr := MMsToCoord(mmnr);

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and
      (Polygon.Segments[NextI].Kind = ePolySegmentLine) and
      (((a_1 = 0) and (a_3 = 90)) or ((a_1 = 90) and (a_3 = 180)) or
      ((a_1 = 180) and (a_3 = 270)) or ((a_1 = 270) and (a_3 = 0)) or
      ((a_3 = 0) and (a_1 = 90)) or ((a_3 = 90) and (a_1 = 180)) or
      ((a_3 = 180) and (a_1 = 270)) or ((a_3 = 270) and (a_1 = 0))) and
      ((abs(abs(a_2) - 45) < MinAngle) or (abs(abs(a_2) - 135) < MinAngle)) and
      (l2 < MinEdgeLengthMMs) then
    begin
      Intersect(CoordToMMs(Polygon.Segments[PrevI].vx),
        CoordToMMs(Polygon.Segments[PrevI].vy),
        CoordToMMs(Polygon.Segments[i].vx), CoordToMMs(Polygon.Segments[i].vy),
        CoordToMMs(Polygon.Segments[NextI].vx),
        CoordToMMs(Polygon.Segments[NextI].vy),
        CoordToMMs(Polygon.Segments[NextNextI].vx),
        CoordToMMs(Polygon.Segments[NextNextI].vy), nvx, nvy);

      PolySeg := Polygon.Segments[NextI];
      PolySeg.vx := MMsToCoord(nvx);
      PolySeg.vy := MMsToCoord(nvy);
      Polygon.Segments[NextI] := PolySeg;

      DeleteSegment(Polygon, i);
      i := i - 1;
    end;
    i := i + 1;
  end;
end;

procedure ProcessTooth(Polygon: IPCB_Polygon; PolygonRpt: TStringList);
var
  i: Integer;
  v2x, v2y: Single;
  PrevI, NextI: Integer;
  l2: Single;
  MinEdgeLengthMMs: Single;
begin
  MinEdgeLengthMMs := 0.3;

  i := 0;
  while (i < Polygon.PointCount) do
  begin
    PrevI := (i - 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextI := (i + 1 + Polygon.PointCount) mod Polygon.PointCount;

    v2x := CoordToMMs(Polygon.Segments[NextI].vx) -
      CoordToMMs(Polygon.Segments[i].vx);
    v2y := CoordToMMs(Polygon.Segments[NextI].vy) -
      CoordToMMs(Polygon.Segments[i].vy);

    l2 := VectorLength(v2x, v2y);

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and (l2 < MinEdgeLengthMMs)
    then
    begin
      DeleteSegment(Polygon, i);
      i := i - 1;
    end;
    i := i + 1;
  end;
end;

procedure ProcessDots(Polygon: IPCB_Polygon; PolygonRpt: TStringList);
var
  i: Integer;
  PolySeg: TPolySegment;
  I1: Integer;
  v1x, v1y, v2x, v2y, v3x, v3y: Single;
  a, a_1, a_2, a_3: Single;
  PrevI, NextI, NextNextI: Integer;
  MinAngle: Single;
begin
  MinAngle := 2;

  i := 0;
  while (i < Polygon.PointCount) do
  begin
    PrevI := (i - 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextI := (i + 1 + Polygon.PointCount) mod Polygon.PointCount;

    v1x := CoordToMMs(Polygon.Segments[i].vx) -
      CoordToMMs(Polygon.Segments[PrevI].vx);
    v1y := CoordToMMs(Polygon.Segments[i].vy) -
      CoordToMMs(Polygon.Segments[PrevI].vy);

    v2x := CoordToMMs(Polygon.Segments[NextI].vx) -
      CoordToMMs(Polygon.Segments[i].vx);
    v2y := CoordToMMs(Polygon.Segments[NextI].vy) -
      CoordToMMs(Polygon.Segments[i].vy);

    a_1 := VectorAngle(v1x, v1y, v2x, v2y) / Pi * 180;
    while a_1 < 0 do
      a_1 := a_1 + 360;
    while a_1 >= 360 do
      a_1 := a_1 - 360;

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and

      ((a_1 < MinAngle) or (a_1 > 360 - MinAngle)) then
    begin
      DeleteSegment(Polygon, i);
      i := i - 1;
    end;
    i := i + 1;
  end;
end;

procedure ProcessAlign(Polygon: IPCB_Polygon; PolygonRpt: TStringList);
var
  i: Integer;
  PolySeg: TPolySegment;
  ncx, ncy: TCoord;
  nvx, nvy: TCoord;
  r, nr: TCoord;
  A1, A2: Integer;
  I1: Integer;
  cp: Single;
  v1x, v1y, v2x, v2y, v3x, v3y: Single;
  a, a_1, a_2, a_3: Single;
  PrevI, NextI, NextNextI: Integer;
  _chord, mmnr: Single;
  l1, l2, l3: Single;
  MinAngle: Single;
begin
  // Dummy
  MinAngle := 10;
  i := 0;
  while (i < Polygon.PointCount) do
  begin
    PrevI := (i - 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextI := (i + 1 + Polygon.PointCount) mod Polygon.PointCount;

    v1x := CoordToMMs(Polygon.Segments[i].vx) -
      CoordToMMs(Polygon.Segments[PrevI].vx);
    v1y := CoordToMMs(Polygon.Segments[i].vy) -
      CoordToMMs(Polygon.Segments[PrevI].vy);

    v2x := CoordToMMs(Polygon.Segments[NextI].vx) -
      CoordToMMs(Polygon.Segments[i].vx);
    v2y := CoordToMMs(Polygon.Segments[NextI].vy) -
      CoordToMMs(Polygon.Segments[i].vy);

    l1 := VectorLength(v1x, v1y);
    l2 := VectorLength(v2x, v2y);

    a_1 := VectorAngle(v2x, v2y, 1, 0);
    a_1 := DetectAngle(a_1 / Pi * 180, MinAngle);

    PolygonRpt.Add(' ----------------------');
    PolygonRpt.Add(' A1: ' + FloatToStr(a_1));

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and (a_1 = 0) or (a_1 = 180)
    then
    begin
      PolySeg := Polygon.Segments[NextI];
      nvx := PolySeg.vx;
      nvy := PolySeg.vy;

      PolySeg := Polygon.Segments[i];
      PolySeg.vy := nvy;
      Polygon.Segments[i] := PolySeg;
    end;

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and (a_1 = 90) or (a_1 = 270)
    then
    begin
      PolySeg := Polygon.Segments[NextI];
      nvx := PolySeg.vx;
      nvy := PolySeg.vy;

      PolySeg := Polygon.Segments[i];
      PolySeg.vx := nvx;
      Polygon.Segments[i] := PolySeg;
    end;
    i := i + 1;
  end;
end;

procedure ProcessCorner(Board: IPCB_Board; Polygon: IPCB_Polygon;
  PolygonRpt: TStringList);
var
  i: Integer;
  PolySeg: TPolySegment;
  ncx, ncy: TCoord;
  nvx, nvy: TCoord;
  r, nr: TCoord;
  A1, A2: Integer;
  I1: Integer;
  cp: Single;
  v1x, v1y, v2x, v2y, v3x, v3y: Single;
  a, a_1, a_2, a_3: Single;
  PrevI, NextI, NextNextI: Integer;
  _chord, mmnr: Single;
  l1, l2, l3: Single;
begin
  i := 0;
  while (i < Polygon.PointCount) do
  begin
    PrevI := (i - 1 + Polygon.PointCount) mod Polygon.PointCount;
    NextI := (i + 1 + Polygon.PointCount) mod Polygon.PointCount;

    v1x := CoordToMMs(Polygon.Segments[i].vx) -
      CoordToMMs(Polygon.Segments[PrevI].vx);
    v1y := CoordToMMs(Polygon.Segments[i].vy) -
      CoordToMMs(Polygon.Segments[PrevI].vy);

    v2x := CoordToMMs(Polygon.Segments[NextI].vx) -
      CoordToMMs(Polygon.Segments[i].vx);
    v2y := CoordToMMs(Polygon.Segments[NextI].vy) -
      CoordToMMs(Polygon.Segments[i].vy);

    l1 := VectorLength(v1x, v1y);
    l2 := VectorLength(v2x, v2y);

    _chord := CoordToMMs(Chord);
    mmnr := _chord / sqrt(2);
    nr := MMsToCoord(mmnr);

    if (Polygon.Segments[PrevI].Kind = ePolySegmentLine) and
      (Polygon.Segments[i].Kind = ePolySegmentLine) and (l1 > 1.2 * mmnr) and
      (l2 > 1.2 * mmnr) and
      not(Is_Near_Board(Board, CoordToMMs(Polygon.Segments[i].vx),
      CoordToMMs(Polygon.Segments[i].vy))) then
    begin
      a_1 := VectorAngle(v1x, v1y, 1, 0);
      a_2 := VectorAngle(v2x, v2y, 1, 0);

      a_1 := DetectAngle(a_1 / Pi * 180);
      a_2 := DetectAngle(a_2 / Pi * 180);

      a := a_1;

      PolygonRpt.Add(' I-1: ' + IntToStr(PrevI));
      PolygonRpt.Add(' A1: ' + FloatToStr(a_1));
      PolygonRpt.Add(' A2: ' + FloatToStr(a_2));

      if (a_1 = 0) and (a_2 = 90) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy - nr;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx - nr;
        PolySeg.cy := PolySeg.vy - nr;
        PolySeg.vx := PolySeg.vx - nr;
        PolySeg.vy := PolySeg.vy;
        PolySeg.Angle1 := 0;
        PolySeg.Angle2 := 90;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 90) and (a_2 = 0) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx + nr;
        PolySeg.vy := PolySeg.vy;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx + nr;
        PolySeg.cy := PolySeg.vy + nr;
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy + nr;
        PolySeg.Angle1 := 180;
        PolySeg.Angle2 := 270;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 90) and (a_2 = 180) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx - nr;
        PolySeg.vy := PolySeg.vy;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx - nr;
        PolySeg.cy := PolySeg.vy + nr;
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy + nr;
        PolySeg.Angle1 := 270;
        PolySeg.Angle2 := 0;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 180) and (a_2 = 90) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy - nr;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx + nr;
        PolySeg.cy := PolySeg.vy - nr;
        PolySeg.vx := PolySeg.vx + nr;
        PolySeg.vy := PolySeg.vy;
        PolySeg.Angle1 := 90;
        PolySeg.Angle2 := 180;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 180) and (a_2 = 270) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy + nr;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx + nr;
        PolySeg.cy := PolySeg.vy + nr;
        PolySeg.vx := PolySeg.vx + nr;
        PolySeg.vy := PolySeg.vy;
        PolySeg.Angle1 := 180;
        PolySeg.Angle2 := 270;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 270) and (a_2 = 180) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx - nr;
        PolySeg.vy := PolySeg.vy;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx - nr;
        PolySeg.cy := PolySeg.vy - nr;
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy - nr;
        PolySeg.Angle1 := 0;
        PolySeg.Angle2 := 90;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 270) and (a_2 = 0) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx + nr;
        PolySeg.vy := PolySeg.vy;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx + nr;
        PolySeg.cy := PolySeg.vy - nr;
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy - nr;
        PolySeg.Angle1 := 90;
        PolySeg.Angle2 := 180;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
      if (a_1 = 0) and (a_2 = 270) then
      begin
        PolySeg := Polygon.Segments[i];
        PolySeg.vx := PolySeg.vx;
        PolySeg.vy := PolySeg.vy + nr;
        AddSegment(Polygon, PolySeg, i + 1);

        PolySeg := Polygon.Segments[i];
        PolySeg.Kind := ePolySegmentArc;
        PolySeg.Radius := nr;
        PolySeg.cx := PolySeg.vx - nr;
        PolySeg.cy := PolySeg.vy + nr;
        PolySeg.vx := PolySeg.vx - nr;
        PolySeg.vy := PolySeg.vy;
        PolySeg.Angle1 := 270;
        PolySeg.Angle2 := 0;
        Polygon.Segments[i] := PolySeg;
        i := i + 1;
      end;
    end;
    i := i + 1;
  end;
end;

procedure ProcessRadius(Polygon: IPCB_Polygon; PolygonRpt: TStringList);
var
  i: Integer;
  PolySeg: TPolySegment;
  ncx, ncy: TCoord;
  nvx, nvy: TCoord;
  r, nr: TCoord;
  A1, A2: Integer;
  I1: Integer;
  cp: Single;
  v1x, v1y, v2x, v2y, v3x, v3y: Single;
  a, a_1, a_2, a_3: Single;
  PrevI, NextI, NextNextI: Integer;
  _chord, mmnr: Single;
  l1, l2, l3: Single;
begin
  // Segments of a polygon
  for i := 0 to Polygon.PointCount - 1 do
  begin
    if Polygon.Segments[i].Kind = ePolySegmentArc then
    begin
      PolygonRpt.Add(' Polygon Segment Line at X: ' +
        FloatToStr(CoordToMMs(Polygon.Segments[i].vx)));
      PolygonRpt.Add(' Polygon Segment Line at Y: ' +
        FloatToStr(CoordToMMs(Polygon.Segments[i].vy)));
      // PolygonRpt.Add(' Polygon Segment Arc 1  : ' + FloatToStr(Polygon.Segments[I].Angle1));
      // PolygonRpt.Add(' Polygon Segment Arc 2  : ' + FloatToStr(Polygon.Segments[I].Angle2));
      // PolygonRpt.Add(' Polygon Segment Radius : ' + FloatToStr(CoordToMMs(Polygon.Segments[I].Radius)));
      // PolygonRpt.Add(' Polygon Segment Center at X: ' + FloatToStr(CoordToMMs(Polygon.Segments[I].cx)));
      // PolygonRpt.Add(' Polygon Segment Center at Y: ' + FloatToStr(CoordToMMs(Polygon.Segments[I].cy)));

      _chord := CoordToMMs(Chord);
      mmnr := _chord / sqrt(2);
      nr := MMsToCoord(mmnr);

      // Polygon.Segments[I].Radius := MMsToCoord(1);
      // If (I = -1) Then
      A1 := DetectAngle(Polygon.Segments[i].Angle1);
      A2 := DetectAngle(Polygon.Segments[i].Angle2);
      I1 := (i + 1) mod Polygon.PointCount;

      v1x := CoordToMMs(Polygon.Segments[I1].vx) -
        CoordToMMs(Polygon.Segments[i].vx);
      v1y := CoordToMMs(Polygon.Segments[I1].vy) -
        CoordToMMs(Polygon.Segments[i].vy);

      v2x := CoordToMMs(Polygon.Segments[i].cx) -
        CoordToMMs(Polygon.Segments[i].vx);
      v2y := CoordToMMs(Polygon.Segments[i].cy) -
        CoordToMMs(Polygon.Segments[i].vy);

      PolygonRpt.Add(' v1x: ' + FloatToStr(v1x));
      PolygonRpt.Add(' v1y: ' + FloatToStr(v1y));

      PolygonRpt.Add(' v2x: ' + FloatToStr(v2x));
      PolygonRpt.Add(' v2y: ' + FloatToStr(v2y));

      cp := CrossProduct(v1x, v1y, v2x, v2y);

      PolygonRpt.Add(' A1: ' + IntToStr(A1));
      PolygonRpt.Add(' A2: ' + IntToStr(A2));

      PolygonRpt.Add(' CP: ' + FloatToStr(cp));
      if cp > 0 then
        PolygonRpt.Add(' CP: +')
      else
        PolygonRpt.Add(' CP: -');
      // cp := 0;

      if (A1 = 0) and (A2 = 90) and (cp < 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx + r - nr;
        nvy := Polygon.Segments[i].vy;
        ncx := Polygon.Segments[i].cx + r - nr;
        ncy := Polygon.Segments[i].cy + r - nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx;
        nvy := Polygon.Segments[I1].vy + r - nr;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 0) and (A2 = 90) and (cp > 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx;
        nvy := Polygon.Segments[i].vy + r - nr;
        ncx := Polygon.Segments[i].cx + r - nr;
        ncy := Polygon.Segments[i].cy + r - nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx + r - nr;
        nvy := Polygon.Segments[I1].vy;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 90) and (A2 = 180) and (cp < 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx;
        nvy := Polygon.Segments[i].vy + r - nr;
        ncx := Polygon.Segments[i].cx - r + nr;
        ncy := Polygon.Segments[i].cy + r - nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx - r + nr;
        nvy := Polygon.Segments[I1].vy;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 90) and (A2 = 180) and (cp > 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx - r + nr;
        nvy := Polygon.Segments[i].vy;
        ncx := Polygon.Segments[i].cx - r + nr;
        ncy := Polygon.Segments[i].cy + r - nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx;
        nvy := Polygon.Segments[I1].vy + r - nr;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 180) and (A2 = 270) and (cp < 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx - r + nr;
        nvy := Polygon.Segments[i].vy;
        ncx := Polygon.Segments[i].cx - r + nr;
        ncy := Polygon.Segments[i].cy - r + nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx;
        nvy := Polygon.Segments[I1].vy - r + nr;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 180) and (A2 = 270) and (cp > 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx;
        nvy := Polygon.Segments[i].vy - r + nr;
        ncx := Polygon.Segments[i].cx - r + nr;
        ncy := Polygon.Segments[i].cy - r + nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx - r + nr;
        nvy := Polygon.Segments[I1].vy;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 270) and (A2 = 0) and (cp < 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx;
        nvy := Polygon.Segments[i].vy - r + nr;
        ncx := Polygon.Segments[i].cx + r - nr;
        ncy := Polygon.Segments[i].cy - r + nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx + r - nr;
        nvy := Polygon.Segments[I1].vy;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
      if (A1 = 270) and (A2 = 0) and (cp > 0) then
      begin
        r := Polygon.Segments[i].Radius;
        nvx := Polygon.Segments[i].vx + r - nr;
        nvy := Polygon.Segments[i].vy;
        ncx := Polygon.Segments[i].cx + r - nr;
        ncy := Polygon.Segments[i].cy - r + nr;

        PolySeg := Polygon.Segments[i];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        PolySeg.Radius := nr;
        PolySeg.cx := ncx;
        PolySeg.cy := ncy;
        Polygon.SetState_Segments(i, PolySeg);

        nvx := Polygon.Segments[I1].vx;
        nvy := Polygon.Segments[I1].vy - r + nr;

        PolySeg := Polygon.Segments[I1];
        PolySeg.vx := nvx;
        PolySeg.vy := nvy;
        Polygon.SetState_Segments(I1, PolySeg);
      end;
    end;
  end;
end;

procedure IteratePolygons(Dummy: String = '');
var
  Board: IPCB_Board;
  Polygon: IPCB_Polygon;
  Iterator: IPCB_BoardIterator;
  PolygonRpt: TStringList;
  FileName: TPCBString;
  Document: IServerDocument;
  PolyNo: Integer;
begin
  // Retrieve the current board
  Board := PCBServer.GetCurrentPCBBoard;
  if Board = Nil then
    Exit;

  // Search for Polygons and for each polygon found
  // get its attributes and put them in a TStringList object
  // to be saved as a text file.
  Iterator := Board.BoardIterator_Create;
  Iterator.AddFilter_ObjectSet(MkSet(ePolyObject));
  Iterator.AddFilter_LayerSet(AllLayers);
  Iterator.AddFilter_Method(eProcessAll);

  PolyNo := 0;
  PolygonRpt := TStringList.Create;

  Polygon := Iterator.FirstPCBObject;
  while (Polygon <> Nil) do
  begin
    Inc(PolyNo);
    PolygonRpt.Add('Polygon No : ' + IntToStr(PolyNo));

    if Options[0] then
      ProcessTooth(Polygon, PolygonRpt);
    if Options[1] then
      ProcessDots(Polygon, PolygonRpt);
    if Options[2] then
      ProcessBevel(Polygon, PolygonRpt);
    if Options[3] then
      ProcessAlign(Polygon, PolygonRpt);
    if Options[4] then
      ProcessCorner(Board, Polygon, PolygonRpt);
    if Options[5] then
      ProcessRadius(Polygon, PolygonRpt);

    Polygon.Rebuild;
    Polygon := Iterator.NextPCBObject;
  end;
  Board.BoardIterator_Destroy(Iterator);

  // [!!!] Debug only
  if (false) then
  begin

    // The TStringList contains Polygon data and is saved as
    // a text file.
    FileName := ChangeFileExt(Board.FileName, '.pol');
    PolygonRpt.SaveToFile(FileName);
    PolygonRpt.Free;

    // Display the Polygons report
    Document := Client.OpenDocument('Text', FileName);
    if Document <> Nil then
      Client.ShowDocument(Document);
  end;
end;

procedure CalculatePolyRegionArea(Dummy: String = '');
begin
  IteratePolygons;
end;

function ConfigFilename(Dummy: String = ''): String;
begin
  Result := ClientAPI_SpecialFolder_AltiumApplicationData +
    '\RoundTheCorner.ini'
end;

procedure WriteToIniFile(AFileName: String);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(AFileName);

  IniFile.WriteInteger('Window', 'Top', MainFrm.Top);
  IniFile.WriteInteger('Window', 'Left', MainFrm.Left);

  IniFile.WriteBool('General', 'Option1', CheckBox1.Checked);
  IniFile.WriteBool('General', 'Option2', CheckBox2.Checked);
  IniFile.WriteBool('General', 'Option3', CheckBox3.Checked);
  IniFile.WriteBool('General', 'Option4', CheckBox4.Checked);
  IniFile.WriteBool('General', 'Option5', CheckBox5.Checked);
  IniFile.WriteBool('General', 'Option6', CheckBox6.Checked);

  IniFile.WriteString('General', 'Chord', ChordEdt.Text);

  IniFile.Free;
end;

procedure ReadFromIniFile(AFileName: String);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(AFileName);

  MainFrm.Top := IniFile.ReadInteger('Window', 'Top', MainFrm.Top);
  MainFrm.Left := IniFile.ReadInteger('Window', 'Left', MainFrm.Left);

  CheckBox1.Checked := IniFile.ReadString('General', 'Option1',
    CheckBox1.Checked);
  CheckBox2.Checked := IniFile.ReadString('General', 'Option2',
    CheckBox2.Checked);
  CheckBox3.Checked := IniFile.ReadString('General', 'Option3',
    CheckBox3.Checked);
  CheckBox4.Checked := IniFile.ReadString('General', 'Option4',
    CheckBox4.Checked);
  CheckBox5.Checked := IniFile.ReadString('General', 'Option5',
    CheckBox5.Checked);
  CheckBox6.Checked := IniFile.ReadString('General', 'Option6',
    CheckBox6.Checked);

  ChordEdt.Text := IniFile.ReadString('General', 'Chord', ChordEdt.Text);

  IniFile.Free;
end;

procedure RunGUI;
begin
  MainFrm.ShowModal;
end;

procedure TMainFrm.RunBtnClick(Sender: TObject);
var
  Board: IPCB_Board;
  DisplayUnit: TUnit;
begin
  // Retrieve the current board
  Board := PCBServer.GetCurrentPCBBoard;
  if Board = Nil then
    Exit;

  Options[0] := CheckBox1.Checked;
  Options[1] := CheckBox2.Checked;
  Options[2] := CheckBox3.Checked;
  Options[3] := CheckBox4.Checked;
  Options[4] := CheckBox5.Checked;
  Options[5] := CheckBox6.Checked;

  DisplayUnit := Board.DisplayUnit;
  StringToCoordUnit(ChordEdt.Text, Chord, DisplayUnit);

  CalculatePolyRegionArea('');

  Close;
end;

procedure TMainFrm.MainFrmClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteToIniFile(ConfigFilename);
end;

procedure TMainFrm.MainFrmCreate(Sender: TObject);
begin
  ReadFromIniFile(ConfigFilename);
end;
