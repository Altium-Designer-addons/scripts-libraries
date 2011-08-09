{..............................................................................}
{ Summary   This scripts can be used to create room based on selected objects, }
{           or to create room based on selected polygon.                       }
{                                                                              }
{           Since arcs do not work in rooms, this script splits arcs in lines. }
{           User can sontrol this - AngleBreak is assigned at the beginning.   }
{           It contains number of degrees used to split an arc, e.g. if it is  }
{           10.0, that means arc will be split to line every 10 degrees.       } 
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure RoomFromPoly;
var
    Board      : IPCB_Board;
    Iterator   : IPCB_Boarditerator;

    flag       : Integer;
    Lista      : TStringList;
    Line       : String;


    TempPoly   : IPCB_Polygon;
    Polygon    : IPCB_Polygon;
    NewRoom    : IPCB_ConfinementConstraint;
    I          : Integer;
    Angle1     : Double;
    Angle2     : Double;
    Radius     : Integer;
    Cx, Cy     : Integer;
    Vx, Vy     : Integer;

    IncPoint   : Integer;
    AngleBreak : Double;

    Angle1X    : Integer;
    Angle1Y    : Integer;
    Angle2X    : Integer;
    Angle2Y    : Integer;
    NewSegment : TPolySegment;
begin
    AngleBreak := 10.0;
    // Split arcs by degrees. This means that arc will be split in lines by the
    // Number of degrees set in Angle Break variable.

    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // This is where we will add part for selected objects

    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    flag := 0;
    Lista := TStringList.Create;
    Line := '';


    TempPoly := Iterator.FirstPCBObject;

    While (TempPoly <> Nil) Do
    Begin
       Lista.Add(TempPoly.I_ObjectAddress);
       //Line := Line + IntToStr(TempPoly.I_ObjectAddress) + #13#10;

       if TempPoly.Selected = True then
       begin
          flag := 1;
          Polygon := TempPoly;
       end;
       TempPoly := Iterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iterator);

    // ShowMessage(Line);
    // In this case no poly is selected - create poly from selected objects
    if flag = 0 then
    begin

       ResetParameters;
       AddStringParameter('Action','TO_POLYGON');
       RunProcess('PCB:ConvertSelected');

       Iterator := Board.BoardIterator_Create;
       Iterator.AddFilter_ObjectSet(MkSet(ePolyObject));
       Iterator.AddFilter_LayerSet(AllLayers);
       Iterator.AddFilter_Method(eProcessAll);


       TempPoly := Iterator.FirstPCBObject;

       While (TempPoly <> Nil) Do
       Begin
          flag := 0;
          for i := 0 to Lista.Count - 1 do
          begin
             Line := Lista[i];
             if Line = TempPoly.I_ObjectAddress then
                flag := 1;
          end;

          if flag = 0 then
          begin
              Polygon := TempPoly;
              flag := 2;
              break;
          end;
          TempPoly := Iterator.NextPCBObject;
       end;
       Board.BoardIterator_Destroy(Iterator);
    end;


    if flag <> 0 then
    begin

       NewRoom := PCBServer.PCBRuleFactory(eRule_ConfinementConstraint);

       NewRoom.NetScope  := eNetScope_AnyNet;
       NewRoom.LayerKind := eRuleLayerKind_SameLayer;

       NewRoom.PointCount := Polygon.PointCount;

       IncPoint := 0;

       NewSegment := TPolySegment;

       For I := 0 To Polygon.PointCount Do
       Begin
          NewRoom.Segments[I+IncPoint] := Polygon.Segments[I];
          If Polygon.Segments[I].Kind = ePolySegmentArc then
          begin

             // Get the data
             Vx := Polygon.Segments[I].vx;
             Vy := Polygon.Segments[I].vy;
             Angle1 := Polygon.Segments[I].Angle1;
             Angle2 := Polygon.Segments[I].Angle2;
             Radius := Polygon.Segments[I].Radius;
             Cx     := Polygon.Segments[I].Cx;
             Cy     := Polygon.Segments[I].Cy;

             // Now I need to figure out which one point is closer to vertex

             Angle1x := Cx + Int(Radius * cos(Angle1 * PI / 180));
             Angle1y := Cy + Int(Radius * sin(Angle1 * PI / 180));

             Angle2x := Cx + Int(Radius * cos(Angle2 * PI / 180));
             Angle2y := Cy + Int(Radius * sin(Angle2 * PI / 180));

             if ((abs(Angle1x - Vx) < abs (Angle2x - Vx)) and (abs(Angle1y - Vy) < abs (Angle2y - Vy))) then
             begin

                // angle1 is first
                Angle1 := Angle1 + AngleBreak;
                if Angle2 = 0 then Angle2 := 360;

                NewSegment.Kind := ePolySegmentLine;

                While Angle1 < Angle2  do
                begin
                   Inc(IncPoint);
                   NewRoom.PointCount := Polygon.PointCount + IncPoint;

                   Angle1x := Cx + Int(Radius * cos(Angle1 * PI / 180));
                   Angle1y := Cy + Int(Radius * sin(Angle1 * PI / 180));

                   NewSegment.vx := Angle1x;
                   NewSegment.vy := Angle1y;

                   Angle1 := Angle1 + AngleBreak;

                   NewRoom.Segments[I + IncPoint] := NewSegment;
                end;
             end
             else
             begin

                // angle2 is first
                if Angle2 < Angle1 then Angle2 := 360 + Angle2;
                Angle2 := Angle2 - AngleBreak;

                NewSegment.Kind := ePolySegmentLine;

                While Angle1 < Angle2  do
                begin
                   Inc(IncPoint);
                   NewRoom.PointCount := Polygon.PointCount + IncPoint;

                   Angle2x := Cx + Int(Radius * cos(Angle2 * PI / 180));
                   Angle2y := Cy + Int(Radius * sin(Angle2 * PI / 180));

                   NewSegment.vx := Angle2x;
                   NewSegment.vy := Angle2y;

                   Angle2 := Angle2 - AngleBreak;

                   NewRoom.Segments[I + IncPoint] := NewSegment;
                end;
             end;
          end;
       End;

       NewRoom.ConstraintLayer := eTopLayer;
       NewRoom.Kind := eConfineOut;

       NewRoom.Name := 'Room_From_Poly';
       NewRoom.Comment := 'Custom room definition (confinement constraint) rule.';

       Board.AddPCBObject(NewRoom);
       if flag = 2 then
          Board.RemovePCBObject(Polygon);

       ResetParameters;
       AddStringParameter('Action','Redraw');
       RunProcess('PCB:Zoom');
    end;
end;
