{..............................................................................}
{ Summary   This script ccan be used to clean nets within choosen net class.   }
{                                                                              }
{           In the form you need to choose net class which you wish to clean.  }
{           If you want to clean all nets, choose "All nets" net class.        }
{                                                                              }
{           After that there is tolerance. Although it can be modified, it is  }
{           best to keep it default, but if some tracks do not get merged that }
{           way, you can increase it.                                          }
{                                                                              }
{           There are 7 checkboxes available after that, that work on choosen  }
{           net class:                                                         }
{           - Remove short primitives: it will remove all tracks shorter than  }
{             Tolerance, and all arcs who have total angle difference smaller  }
{             than 1°                                                          }
{           - Fix Overlapping primitives - this one will fix overlaps on       }
{             tracks and arcs. It will merge them. If there is a T connection  }
{             upper tracks in it will be merged into single track. Same goes   }
{             with X connections                                               }
{           - Split "T" tracks - in case you have T connection, previous       }
{             procedure would merge that in 2 tracks. This procedure is        }
{             executed after it ,and it will split track if electrical hotspot }
{             is found in it's middle.                                         }
{           - Split X tracks (unfinished) - same as above, but splits X        }
{             connections.                                                     }
{           - Fix Bad Connections (unfinished) - it will try to find tracks    }
{             and arcs that have endpoints that do not match EXACTLY to        }
{             electrical hotspot of another object, but are close to it.       }
{             it will then try to match endpoints.                             }
{           - Select Bad connections - basically executes script that selects  }
{             bad connections, so user can fix them himself.                   }
{           - Remove Short Bad connections - cycles through bad connections    }
{             and deletes those whose length = Width / 10. Mostly these are    }
{             unnecessary.                                                     }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board      : IPCB_Board;
   Tolerance  : TCoord;
   Segments   : TStringList;


// Function that takes track and modifies it's data for trigonometry use
Procedure SetupDataFromTrack(var Prim1 : IPCB_Track, out IsVertical : Boolean, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord : Out Y2 : TCoord, out k : Double, out c : TCoord);
var
   a, b : Integer;
begin
   if Prim1.x1 = Prim1.x2 then
   begin
      IsVertical := True;
      X1 := Prim1.x1;
      X2 := Prim1.x2;
      if Prim1.y1 < Prim1.y2 then
      begin
         Y1 := Prim1.y1;
         Y2 := Prim1.y2;
      end
      else
      begin
         Y1 := Prim1.y2;
         Y2 := Prim1.y1;
      end;
      k := 0;
      c := Prim1.x1;
   end
   else
   begin
      if Prim1.x1 < Prim1.x2 then
      begin
         X1 := Prim1.x1;
         Y1 := Prim1.y1;
         X2 := Prim1.x2;
         Y2 := Prim1.y2;
      end
      else
      begin
         X1 := Prim1.x2;
         Y1 := Prim1.y2;
         X2 := Prim1.x1;
         Y2 := Prim1.y1;
      end;
      k  := (Y2 - Y1)/(X2 - X1);

      // This is still Vertical if k < 20
      if (Abs(k) > 20) then
      begin
         X1 := Prim1.x1;
         X2 := Prim1.x2;

        repeat
           a := X1 mod 10;
           b := X2 mod 10;
           X1 := X1 div 10;
           X2 := X2 div 10;
        until ((a <> 0) or (b <> 0));

        if a <> 0 then
        begin
           X1 := Prim1.x2;
           X2 := Prim1.x2;
        end
        else
        begin
           X1 := Prim1.x1;
           X2 := Prim1.x1;
        end;

        if Prim1.y1 < Prim1.y2 then
        begin
           Y1 := Prim1.y1;
           Y2 := Prim1.y2;
        end
        else
        begin
           Y1 := Prim1.y2;
           Y2 := Prim1.y1;
        end;

        c := X1;
        IsVertical := True;
      end
      else
      begin
         c := Y1 - k * X1;
         IsVertical := False;
      end;
   end;
end;


// Function that calculates point to line distance
Function PointToLineDistance(k : double, c : TCoord, IsPrimVert : Boolean, X : TCoord, Y : TCoord) : Double;
begin
   if IsPrimVert then
      Result := Abs(c - X)
   else
      Result := (Abs(k*X - Y + c))/(sqrt(sqr(k) + 1));
end;


// Function that calculates point to point distance
function PointToPointDistance(X1, Y1, X2, Y2) : double;
begin
   Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;


// Function that returns true if two lines intersect, and false if they are parallel
// It also returns X and Y coordinates of intersection
Function GetIntersection(k1 : Double, c1 : TCoord, IsPrim1Vert : boolean, k2 : Double, c2 : TCoord, IsPrim2Vert : Boolean, out X : TCoord, out Y : TCoord) : Boolean;
begin
   Result := True;
   if (IsPrim1Vert and IsPrim2Vert) or ((not IsPrim1Vert) and (not IsPrim2Vert) and (abs(k1 - k2) < 0.01)) then
   begin
      // Parallel tracks
      Result := False;
   end
   else if IsPrim1Vert or IsPrim2Vert then
   begin
      if IsPrim1Vert then
      begin
         X := c1;
         Y := k2*c1 + c2;
      end
      else
      begin
         X := c2;
         Y := k1*c2 + c1;
      end;
   end
   else
   begin
      X := (c2 - c1)/(k1 - k2);
      Y := k1*X + c1;
   end;
end;


// Unused for now - It is also supposed to calculate intersection between two lines with different data
Procedure GetSplitPoint(k : Double, c : TCoord, IsPrimVert : Boolean, X1 : TCoord, Y1 : TCoord, X2 : TCoord, Y2 : TCoord, out X : TCoord, out Y : TCoord);
var
   k2 : Double;
   c2 : TCoord;
   IsPrim2Vert : Boolean;
begin
   if (k = 0) and (not IsPrimVert) then
   begin
      k2 := 0;

      if (PointToLineDistance(k, c, IsPrimVert, X1, Y1) < PointToLineDistance(k, c, IsPrimVert, X2, Y2)) then
         c2 := Y1 - k2 * X1  // point 1 is closer
      else
         c2 := Y2 - k2 * X2; // point 2 is closer

      IsPrim2Vert := True;
   end
   else
   begin
      if IsPrimVert then k2 := 0
      else               k2 := -1/k;
      IsPrim2Vert := False;

      if (PointToLineDistance(k, c, IsPrimVert, X1, Y1) < PointToLineDistance(k, c, IsPrimVert, X2, Y2)) then
         c2 := Y1 - k2 * X1  // point 1 is closer
      else
         c2 := Y2 - k2 * X2; // point 2 is closer
   end;

   GetIntersection(k, c, IsPrimVert, k2, c2, IsPrim2Vert, X, Y);
end;


// Function returnes true if point is on IPCB_track (on line and between start and end point of a track).
function IsPointOnTrack(Track : IPCB_Track, X : TCoord, Y : TCoord) : boolean;
var
   k, c           : TCoord;
   IsPrimVert     : Boolean;
   X1, Y1, X2, Y2 : TCoord;
begin
   SetupDataFromTrack(Track, IsPrimVert, X1, Y1, X2, Y2, k, c);

   Result := False;

   if (PointToLineDistance(k, c, IsPrimVert, X, Y) < Tolerance) then
   begin
      if IsPrimVert then
      begin
         if ((Y > Y1) and (Y < Y2)) then
            Result := True;
      end
      else
      begin
         if ((X > X1) and (X < X2)) then
            Result := True;
      end;
   end;
end;


// Function that gets one track and checks if it has overlaps. Selected tracks are disregarded
Procedure FixOverlapsOnTrack(Prim1 : IPCB_Track);
var
   SIter       : IPCB_SpatialIterator;
   Prim2       : IPCB_Primitive;
   k1, k2      : Double;
   c1, c2      : TCoord;
   X11, Y11    : TCoord;
   X12, Y12    : TCoord;
   X21, Y21    : TCoord;
   X22, Y22    : TCoord;
   IsPrim1Vert : Boolean;
   IsPrim2Vert : Boolean;
   IsExtended  : Boolean;
begin
   repeat
      IsExtended := False;

      SetupDataFromTrack(Prim1, IsPrim1Vert, X11, Y11, X12, Y12, k1, c1);

      SIter := Board.SpatialIterator_Create;
      SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
      SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
      SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

      Prim2 := SIter.FirstPCBObject;
      While (Prim2 <> nil) do
      begin
         if (not Prim2.Selected) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and Prim2.InNet and (Prim2.Net.Name = Prim1.Net.Name) and
            (Prim1.Width = Prim2.Width) and (not Prim2.TearDrop) and (Prim2.IsFreePrimitive)then
         begin

            SetupDataFromTrack(Prim2, IsPrim2Vert, X21, Y21, X22, Y22, k2, c2);

            if IsPrim1Vert and IsPrim2Vert and (Abs(X11 - X21) < Tolerance) then
            begin
               Prim1.BeginModify;
               if (Y11 < Y21) then Prim1.y1 := Y11
               else                Prim1.y1 := Y21;

               if (Y12 > Y22) then Prim1.y2 := Y12
               else                Prim1.y2 := Y22;

               Prim1.EndModify;
               Prim1.GraphicallyInvalidate;
               Prim2.Selected := True;
               IsExtended := True;

               // Here I need to update Y11 and Y12, based on new info
               Y11 := Prim1.y1;
               Y12 := Prim1.y2;
            end
            else if (not IsPrim1Vert) and (not IsPrim2Vert) then
            begin                         //Abs((c1 - c2) * sin((pi / 2) - arctan(k1))
               if (Abs(k1 - k2) < 0.1) and (PointToLineDistance(k1, c1, IsPrim1Vert, X21, Y21) < Tolerance) then
               begin
                  Prim1.BeginModify;
                  if (X11 < X21) then
                  begin
                     Prim1.x1 := X11;
                     Prim1.y1 := Y11;
                  end
                  else
                  begin
                     Prim1.x1 := X21;
                     Prim1.y1 := Y21;
                  end;

                  if (X12 > X22) then
                  begin
                     Prim1.x2 := X12;
                     Prim1.y2 := Y12;
                  end
                  else
                  begin
                     Prim1.x2 := X22;
                     Prim1.y2 := Y22;
                  end;
                  Prim1.EndModify;

                  Prim1.GraphicallyInvalidate;
                  Prim2.Selected := True;
                  IsExtended := True;
                  // Here I need to update X11 and X12, based on new info
                  X11 := Prim1.x1;
                  Y11 := Prim1.y1;
                  X12 := Prim1.x2;
                  Y12 := Prim1.y2;
               end;
            end;
         end;
         Prim2 := SIter.NextPCBObject;
      end;
      Board.SpatialIterator_Destroy(SIter);
   until (IsExtended = False);
end;


// Function that gets one arc and checks if it has overlaps. Selected arcs are disregarded
Procedure FixOverlapsOnArc(Prim1 : IPCB_Arc);
var
   SIter      : IPCB_SpatialIterator;
   Prim2      : IPCB_Primitive;
   IsExtended : Boolean;
begin
   repeat
      IsExtended := False;
      SIter := Board.SpatialIterator_Create;
      SIter.AddFilter_ObjectSet(MkSet(eArcObject));
      SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
      SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

      Prim2 := SIter.FirstPCBObject;
      While (Prim2 <> nil) do
      begin
         if (not Prim2.Selected) and (not Prim2.TearDrop) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress)  and Prim2.InNet and
            (Prim2.Net.Name = Prim1.Net.Name) and (Prim2.IsFreePrimitive) and (Prim1.LineWidth = Prim2.LineWidth) and
            (Abs(Prim1.Radius - Prim2.Radius) < Tolerance) and (Abs(Prim1.XCenter - Prim2.XCenter) < Tolerance) and (Abs(Prim1.YCenter - Prim2.YCenter) < Tolerance) then
         begin
            if (Prim1.StartAngle < Prim1.EndAngle) and (Prim2.StartAngle < Prim2.EndAngle) then
            begin
               if not ((Prim1.StartAngle >= Prim2.EndAngle) or (Prim2.StartAngle >= Prim1.EndAngle)) then
               begin
                  Prim1.BeginModify;

                  if Prim2.StartAngle <= Prim1.StartAngle then Prim1.StartAngle := Prim2.StartAngle;
                  if Prim2.EndAngle   <= Prim1.EndAngle   then Prim1.EndAngle   := Prim2.EndAngle;

                  Prim1.EndModify;
                  Prim1.GraphicallyInvalidate;
                  Prim2.Selected := True;
                  IsExtended := True;
               end;
            end
            else if ((Prim1.StartAngle < Prim1.EndAngle) and (Prim2.StartAngle > Prim2.EndAngle)) or
                    ((Prim1.StartAngle > Prim1.EndAngle) and (Prim2.StartAngle < Prim2.EndAngle)) then
            begin
               if (Prim1.StartAngle <= Prim2.EndAngle) or (Prim2.StartAngle <= Prim1.EndAngle) then
               begin
                  Prim1.BeginModify;
                  if (Prim1.StartAngle <= Prim2.EndAngle) and (Prim2.StartAngle <= Prim1.EndAngle) then
                  begin
                     Prim1.StartAngle := 0;
                     Prim1.EndAngle   := 360;
                  end
                  else
                  begin
                     if Prim1.StartAngle <= Prim2.EndAngle then Prim1.StartAngle := Prim2.StartAngle;
                     if Prim1.EndAngle >= Prim2.StartAngle then Prim1.EndAngle   := Prim2.EndAngle;
                  end;

                  Prim1.EndModify;
                  Prim1.GraphicallyInvalidate;
                  Prim2.Selected := True;
                  IsExtended := True;
               end;
            end
            else if (Prim1.StartAngle > Prim1.EndAngle) and (Prim2.StartAngle > Prim2.EndAngle) then
            begin
               // They always overlap here
               Prim1.BeginModify;
               if (Prim1.StartAngle <= Prim2.EndAngle) or (Prim2.StartAngle <= Prim1.EndAngle) then
               begin
                  Prim1.StartAngle := 0;
                  Prim1.EndAngle   := 360;
               end
               else
               begin
                  if Prim1.StartAngle > Prim2.StartAngle then Prim1.StartAngle := Prim2.StartAngle;
                  if Prim1.EndAngle   < Prim2.EndAngle   then Prim1.EndAngle   := Prim2.EndAngle;
               end;

               Prim1.EndModify;
               Prim1.GraphicallyInvalidate;
               Prim2.Selected := True;
               IsExtended := True;
            end;
         end;
         Prim2 := SIter.NextPCBObject;
      end;
      Board.SpatialIterator_Destroy(SIter);
   until (IsExtended = False);
end;


// Procedure that checks if track has another track's hotspot on it. If it does, it will be split:
Procedure CheckTrackTrackCrossover(Prim1 : IPCB_Track);
var
   Prim2       : IPCB_Primitive;
   SIter       : IPCB_SpatialIterator;
   k1, k2      : Double;
   c1, c2      : TCoord;
   X11, Y11    : TCoord;
   X12, Y12    : TCoord;
   X21, Y21    : TCoord;
   X22, Y22    : TCoord;
   IsPrim1Vert : Boolean;
   IsPrim2Vert : Boolean;

   NewPrim     : IPCB_Track;
   MaxDist     : TCoord;
   MinDist     : Double;

   SplitPrim1     : Boolean;
   SplitPrim2     : Boolean;
   atFirstPoint   : Boolean;
   X, Y           : TCoord;
   TrackToSplit   : integer;
begin
   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
   SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

   Prim2 := SIter.FirstPCBObject;
   While (Prim2 <> nil) do
   begin
      if (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and
         (not Prim2.TearDrop) and (Prim2.IsFreePrimitive) and (not Prim2.Selected) then
      begin
         if Prim1.Width > Prim2.Width then MaxDist := Prim1.Width
         else                              MaxDist := Prim2.Width;

         if Prim1.Width > Prim2.Width then MinDist := Prim2.Width
         else                              MinDist := Prim1.Width;

         MaxDist := (MaxDist + MinDist)/4;
         MinDist := MinDist/5;

         if not (
            (PointToPointDistance(Prim1.x1, Prim1.y1, Prim2.x1, Prim2.y1) < MaxDist) or
            (PointToPointDistance(Prim1.x1, Prim1.y1, Prim2.x2, Prim2.y2) < MaxDist) or
            (PointToPointDistance(Prim1.x2, Prim1.y2, Prim2.x1, Prim2.y1) < MaxDist) or
            (PointToPointDistance(Prim1.x2, Prim1.y2, Prim2.x2, Prim2.y2) < MaxDist)) then
         begin
            // I moved this here since this sounds like complex function
            if Board.PrimPrimDistance(Prim1,Prim2) = 0 then
            begin

               // Prim1 and Prim2 data setup, and find intersection of two lines
               SetupDataFromTrack(Prim1, IsPrim1Vert, X11, Y11, X12, Y12, k1, c1);
               SetupDataFromTrack(Prim2, IsPrim2Vert, X21, Y21, X22, Y22, k2, c2);

               if (GetIntersection(k1, c1, IsPrim1Vert, k2, c2, IsPrim2Vert, X, Y)) then
               begin
                  // We have intersection now. We need to check distance from points
                  SplitPrim1 := True;
                  SplitPrim2 := True;

                  if (PointToPointDistance(Prim1.x1, Prim1.y1, X, Y) < MinDist) then
                  begin
                     SplitPrim1 := False;
                     Prim1.x1 := X;
                     Prim1.y1 := Y;
                  end;

                  if (PointToPointDistance(Prim1.x2, Prim1.y2, X, Y) < MinDist) then
                  begin
                     SplitPrim1 := False;
                     Prim1.x2 := X;
                     Prim1.y2 := Y;
                  end;

                  if (PointToPointDistance(Prim2.x1, Prim2.y1, X, Y) < MinDist) then
                  begin
                     SplitPrim2 := False;
                     Prim2.x1 := X;
                     Prim2.y1 := Y;
                  end;

                  if (PointToPointDistance(Prim2.x2, Prim2.y2, X, Y) < MinDist) then
                  begin
                     SplitPrim2 := False;
                     Prim2.x2 := X;
                     Prim2.y2 := Y;
                  end;

                  if (Prim1.x1 = Prim1.x2) and (Prim1.y1 = Prim1.y2) then
                  begin
                     Prim1.Selected := True;
                     SplitPrim2 := False;
                  end;

                  if (Prim2.x1 = Prim2.x2) and (Prim2.y1 = Prim2.y2) then
                  begin
                     Prim2.Selected := True;
                     SplitPrim1 := False;
                  end;

                  if (SplitPrim1) and (IsPointOnTrack(Prim1,X,Y)) then
                  begin
                     NewPrim := Prim1.Replicate;
                     Prim1.BeginModify;
                     Prim1.x1 := X;
                     Prim1.y1 := Y;
                     Prim1.EndModify;
                     Prim1.GraphicallyInvalidate;

                     NewPrim.BeginModify;
                     NewPrim.x2 := X;
                     NewPrim.y2 := Y;
                     NewPrim.EndModify;
                     Board.AddPCBObject(NewPrim);
                     Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, NewPrim.I_ObjectAddress);
                     NewPrim.GraphicallyInvalidate;
                     CheckTrackTrackCrossOver(Prim1);
                     CheckTrackTrackCrossOver(NewPrim);
                     Board.SpatialIterator_Destroy(SIter);
                     exit;
                  end;

                  if (SplitPrim2) and (IsPointOnTrack(Prim2,X,Y)) then
                  begin
                     NewPrim := Prim2.Replicate;

                     Prim2.BeginModify;
                     Prim2.x1 := X;
                     Prim2.y1 := Y;
                     Prim2.EndModify;
                     Prim2.GraphicallyInvalidate;

                     NewPrim.BeginModify;
                     NewPrim.x2 := X;
                     NewPrim.y2 := Y;
                     NewPrim.EndModify;
                     Board.AddPCBObject(NewPrim);
                     Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, NewPrim.I_ObjectAddress);
                     NewPrim.GraphicallyInvalidate;

                     CheckTrackTrackCrossOver(Prim2);
                     CheckTrackTrackCrossOver(NewPrim);
                     Board.SpatialIterator_Destroy(SIter);
                     exit;
                  end;
                  break;
               end;
            end;
         end;
      end;
      Prim2 := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);
end;


// Procedure that checks if track in the middle has Pad or via's hotspot. If it does, it will be split:
procedure CheckTrackPadCrossOver(Prim1 : IPCB_Track);
var
   Prim2       : IPCB_Primitive;
   SIter       : IPCB_SpatialIterator;
   k1, k2      : Double;
   c1, c2      : TCoord;
   X1, Y1      : TCoord;
   X2, Y2      : TCoord;
   IsPrim1Vert : Boolean;
   IsPrim2Vert : Boolean;

   NewPrim     : IPCB_Track;
   X, Y        : TCoord;
begin
   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(ePadObject, eViaObject));
   SIter.AddFilter_LayerSet(MkSet(eMultiLayer, Prim1.Layer));
   SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

   Prim2 := SIter.FirstPCBObject;
   While (Prim2 <> nil) do
   begin

      if ((Prim2.ObjectId = ePadObject) or ((Prim2.ObjectId = eViaObject) and Prim2.IntersectLayer(Prim1.Layer))) and
         (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and (Board.PrimPrimDistance(Prim1,Prim2) = 0) then
      begin

         // Setup data from track
         SetupDataFromTrack(Prim1, IsPrim1Vert, X1, Y1, X2, Y2, k1, c1);

         // Manual setup data - line perpendicualr to track that goes through center of pad
         if (k1 = 0) and (not IsPrim1Vert) then
         begin
            k2 := 0;
            c2 := Prim2.x;
            IsPrim2Vert := True;

         end
         else
         begin
            if (k1 = 0) then k2 := 0
            else             k2 := -1/k1;

            c2 := Prim2.y - k2 * Prim2.x;
            IsPrim2Vert := False;
         end;

         if (GetIntersection(k1, c1, IsPrim1Vert, k2, c2, IsPrim2Vert, X, Y)
            and (PointToPointDistance(X1,Y1,X,Y) > Tolerance)
            and (PointToPointDistance(X2,Y2,X,Y) > Tolerance)
            and (PointToLineDistance(k1, c1, IsPrim1Vert, Prim2.x, Prim2.y) < Tolerance)
            and IsPointOnTrack(Prim1,X,Y)) then
         begin
            NewPrim := Prim1.Replicate;
            Prim1.BeginModify;
            Prim1.x1 := X;
            Prim1.y1 := Y;
            Prim1.EndModify;
            Prim1.GraphicallyInvalidate;

            NewPrim.BeginModify;
            NewPrim.x2 := X;
            NewPrim.y2 := Y;
            NewPrim.EndModify;
            Board.AddPCBObject(NewPrim);
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, NewPrim.I_ObjectAddress);
            NewPrim.GraphicallyInvalidate;

            CheckTrackPadCrossOver(Prim1);
            CheckTrackPadCrossOver(NewPrim);
            Board.SpatialIterator_Destroy(SIter);
            exit;
         end;
      end;

      Prim2 := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);
end;


// Procedue that checks track for bad connections. If track has bad connection, it is selected.
Procedure CheckConnection(Prim1 : IPCB_Primitive, X : TCoord, Y : TCoord);
var
   SIter : IPCB_SpatialIterator;
   Prim2 : IPCB_Primitive;
   Found : Boolean;
begin
   if Prim1.Selected then exit;

   // Check if there is another track/arc in hotspot
   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
   SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
   SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

   Prim2 := SIter.FirstPCBObject;
   While (Prim2 <> nil) do
   begin
      if (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and (not Prim2.TearDrop) then
      begin
         if (Prim2.ObjectId = eTrackObject) then
         begin
            if (PointToPointDistance(Prim2.x1, Prim2.y1, X, Y) < Tolerance) or (PointToPointDistance(Prim2.x2, Prim2.y2, X, Y) < Tolerance) then
            begin
               Board.SpatialIterator_Destroy(SIter);
               exit;
            end;
         end
         else if (Prim2.ObjectId = eArcObject) then
         begin
            if (PointToPointDistance(Prim2.StartX, Prim2.StartY, X, Y) < Tolerance) or (PointToPointDistance(Prim2.EndX, Prim2.EndY, X, Y) < Tolerance) then
            begin
               Board.SpatialIterator_Destroy(SIter);
               exit;
            end;
         end;
      end;

      Prim2 := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);

   // Check if there is another pad/via in hotspot
   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(ePadObject, eViaObject));
   SIter.AddFilter_LayerSet(MkSet(Prim1.Layer, String2Layer('Multi Layer')));
   SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

   Prim2 := SIter.FirstPCBObject;
   While (Prim2 <> nil) do
   begin
      if (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) then
      begin
         if Prim2.ObjectId = ePadObject then
         begin
            if (PointToPointDistance(Prim2.x, Prim2.y, X, Y) < Tolerance) then
            begin
               Board.SpatialIterator_Destroy(SIter);
               exit;
            end;
         end
         else if Prim2.ObjectId = eViaObject then
         begin
            if Prim2.IntersectLayer(Prim1.Layer) and (PointToPointDistance(Prim2.x, Prim2.y, X, Y) < Tolerance) then
            begin
               Board.SpatialIterator_Destroy(SIter);
               exit;
            end;
         end;
      end;

      Prim2 := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);

   // If it did not exit until now, this means that there is no prim on this end
   Prim1.Selected := True;
end;


// Procedure that accepts net class and selects short tracks and arcs in it
Procedure SelectShortPrimitivesInNetClass(NetClass : IPCB_ObjectClass);
Var
    Iterator : IPCB_BoardIterator;
    net      : IPCB_Net;
    GrIter   : IPCB_GroupIterator;
    Prim     : IPCB_Primitive;
begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    While Net <> NIl Do
    Begin
        If NetClass.IsMember(Net) Then
        begin
           GrIter := Net.GroupIterator_Create;
           GrIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

           Prim := GrIter.FirstPCBObject;
           While Prim <> NIl Do
           Begin
              if (not Prim.TearDrop) and Prim.IsFreePrimitive and (not Prim.Selected) then
                 if (Prim.ObjectID = eTrackObject) then
                 begin
                    if ((Abs(Prim.x2 - Prim.x1) < Tolerance) and (Abs(Prim.y2 - Prim.y1) < Tolerance)) then
                       Prim.Selected := True;
                 end
                 else if (Prim.ObjectID = eArcObject) then
                 begin
                    if ((Prim.EndAngle - Prim.StartAngle) < 1) then
                       Prim.Selected := True;
                 end;

              Prim := GrIter.NextPCBObject;
           End;
           Net.GroupIterator_Destroy(GrIter);
        end;
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
end;


// Procedure that fixed overlaping tracks and arcs on given net class
Procedure FixOverlapsInNetClass(NetClass : IPCB_ObjectClass);
Var
    Iterator : IPCB_BoardIterator;
    net      : IPCB_Net;
    GrIter   : IPCB_GroupIterator;
    Prim     : IPCB_Prim;
begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    While Net <> NIl Do
    Begin
        If NetClass.IsMember(Net) Then
        begin
           GrIter := Net.GroupIterator_Create;
           GrIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

           Prim := GrIter.FirstPCBObject;
           While Prim <> NIl Do
           Begin
              if (not Prim.TearDrop) and Prim.IsFreePrimitive and (not Prim.Selected) then
                 if (Prim.ObjectID = eTrackObject) then
                    FixOverlapsOnTrack(Prim)
                 else if (Prim.ObjectID = eArcObject) then
                    FixOverlapsOnArc(Prim);

              Prim := GrIter.NextPCBObject;
           End;
           Net.GroupIterator_Destroy(GrIter);
        end;
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
end;


// Procedure that splits cross over tracks on given net class
Procedure SplitCrossOverTracksInNetClass(NetClass : IPCB_ObjectClass);
Var
    Iterator : IPCB_BoardIterator;
    net      : IPCB_Net;
    GrIter   : IPCB_GroupIterator;
    Prim     : IPCB_Prim;
Begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    While Net <> NIl Do
    Begin
        If NetClass.IsMember(Net) Then
        begin
           GrIter := Net.GroupIterator_Create;
           GrIter.AddFilter_ObjectSet(MkSet(eTrackObject));

           Prim := GrIter.FirstPCBObject;
           While Prim <> NIl Do
           Begin
              if (not Prim.TearDrop) and Prim.IsFreePrimitive and (not Prim.Selected) then
                 CheckTrackTrackCrossOver(Prim);

              Prim := GrIter.NextPCBObject;
           End;
           Net.GroupIterator_Destroy(GrIter);
        end;
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);


    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    While Net <> NIl Do
    Begin
        If NetClass.IsMember(Net) Then
        begin
           GrIter := Net.GroupIterator_Create;
           GrIter.AddFilter_ObjectSet(MkSet(eTrackObject));

           Prim := GrIter.FirstPCBObject;
           While Prim <> NIl Do
           Begin
              if (not Prim.TearDrop) and Prim.IsFreePrimitive and (not Prim.Selected) then
                 CheckTrackPadCrossOver(Prim);

              Prim := GrIter.NextPCBObject;
           End;
           Net.GroupIterator_Destroy(GrIter);
        end;
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
end;


// Procedure for selecting bad connections in a net class
Procedure SelectBadConnectionsInNetClass(NetClass : IPCB_ObjectClass);
Var
   Iterator : IPCB_BoardIterator;
   net      : IPCB_Net;
   GrIter   : IPCB_GroupIterator;
   Prim     : IPCB_Prim;
   NetName  : TPCBString;
begin

   Iterator := Board.BoardIterator_Create;
   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
   Net := Iterator.FirstPCBObject;
   NetName := Net.Name;

   While Net <> nil do
   begin
     // ShowMessage(Net.Name);
      If NetClass.IsMember(Net) Then
      begin
         GrIter := Net.GroupIterator_Create;
         GrIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

         Prim := GrIter.FirstPCBObject;
         While Prim <> NIl Do
         Begin
            if (not Prim.TearDrop) and Prim.IsFreePrimitive and (not Prim.Selected) then
               if (Prim.ObjectID = eTrackObject) then
               begin
                  CheckConnection(Prim, Prim.x1, Prim.y1);
                  CheckConnection(Prim, Prim.x2, Prim.y2);
               end
               else if (Prim.ObjectID = eArcObject) and (not((Prim.StartAngle = 0) and (Prim.EndAngle = 360))) then
               begin
                  CheckConnection(Prim, Prim.StartX, Prim.StartY);
                  CheckConnection(Prim, Prim.EndX, Prim.EndY);
               end;
            Prim := GrIter.NextPCBObject;
         End;
         Net.GroupIterator_Destroy(GrIter);
      end;
      Net := Iterator.NextPCBObject;
      // I got a bug here - iterator would never break out of this loop inone of
      // my test cases, so I needed to add this funny two lines.
      if Net = nil then break
      else if NetName = Net.Name then break;
   End;
   Board.BoardIterator_Destroy(Iterator);
end;


// Unused in version 2.0
procedure CreateRectangle(X, Y, Width, var Rectangle : TCoordRect);
begin
   Rectangle.Left   := X - Width;
   Rectangle.Bottom := Y - Width;
   Rectangle.Right  := X + Width;
   Rectangle.Top    := Y + Width;
end;


// Unused in version 2.0
Procedure CheckConnectionsOnPoint(Prim : IPCB_Primitive, BaseRect : TCoordRect, NextRect : TCoordRect, Segment : Integer);
var
   TestPrim : IPCB_Primitive;
   SIter    : IPCB_SpatialIterator;
   i        : Integer;
begin

   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, ePadObject, eViaObject));
   SIter.AddFilter_LayerSet(MkSet(Prim1.Layer, eMultiLayer));
   SIter.AddFilter_Area(NextRect.Left, NextRect.Bottom, NextRect.Right, NextRect.Top);

   TestPrim := SIter.FirstPCBObject;
   While (TestPrim <> nil) do
   begin
      // Here we need to do lot of checks - first we will check if we have at least one primitive touching this one

      TestPrim := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);
end;


// OK, so this is the algorithm for fixing bad connextions
// This one will be hard to make, since there are millon of cases that need to be satisfied, but anyway.
// First part of Algorithm goes through all tracks and arcs in design and tries to
// see which ones have endpoints that do not end EXACTLY on endpoint of another object
// Those that are found need to be fixed in really hard way

// Unused in version 2.0
Procedure FixBadConnections(dummy : integer = 0);
var
   BIter     : IPCB_BoardIterator;
   Prim1     : IPCB_Primitive;
   Rectangle : TCoordRect;
   X1, Y1    : TCoord;
   X2, Y2    : TCoord;
   Width     : TCoord;
begin

   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   Segments := TStringList.Create;
   Rectangle := TCoordRect;

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject{, eArcobject}));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      if Prim1.InNet and (not Prim1.TearDrop) and Prim1.IsFreePrimitive then
      begin
      // I need to move check in separate function since check can be executed recursively
         if Prim.ObjectID = eTrackObject then
         begin
            X1 := Prim1.x1;
            Y1 := Prim1.y1;
            X2 := Prim1.X2;
            Y2 := Prim1.Y2;
            Width := Prim1.Width;
         end
         else
         begin
            X1 := Prim1.StartX;
            Y1 := Prim1.StartY;
            X2 := Prim1.EndX;
            Y2 := Prim1.EndY;
            Width := Prim1.LineWidth;
         end;
         Segments.Clear;
         Segments.AddObject('0',Prim1);
         CreateRectangle(X1, Y1, Width, Rectangle);
         CheckConnectionsOnPoint(Prim1, Rectangle, Rectangle, 0);


         Segments.Clear;
         Segments.AddObject('0',Prim1);
         CreateRectangle(X2, Y2, Width, Rectangle);
         CheckConnectionsOnPoint(Prim1, Rectangle, Rectangle, 0);
      end;

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;

end;


// Procedure for splits cross overs on entire pcb.
// Used it while testing. It is not used in final script
Procedure SplitCrossOverTracks(dummy : integer = 0);
var
   BIter  : IPCB_BoardIterator;
   Prim1  : IPCB_Track;
begin

   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      if Prim1.InNet and (not Prim1.TearDrop) and Prim1.IsFreePrimitive and (not Prim1.Selected) then
      // I need to move check in separate function since check can be executed recursively
         CheckTrackTrackCrossOver(Prim1);

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);


   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      if Prim1.InNet and (not Prim1.TearDrop) and Prim1.IsFreePrimitive and (not Prim1.Selected) then
      // I need to move check in separate function since check can be executed recursively
         CheckTrackPadCrossOver(Prim1);

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;
end;


// Procedure for fixing overlapping tracks on entire pcb.
// Used it while testing. It is not used in final script
Procedure FixOverlappingArcs(Dummy : Integer = 0);
var
   BIter      : IPCB_BoardIterator;
   Prim1      : IPCB_Primitive;
begin
   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eArcObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;
   While Prim1 <> nil do
   begin

      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (Prim1.IsFreePrimitive) then
         FixOverlapsOnArc(Prim1);

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;
end;


// Procedure for fixing overlapping tracks on entire pcb.
// Used it while testing. It is not used in final script
Procedure FixOverlappingTracks(dummy : integer = 0);
var
   BIter       : IPCB_BoardIterator;
   Prim1       : IPCB_Primitive;
begin

   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (Prim1.IsFreePrimitive) then
         FixOverlapsOnTrack(Prim1);

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;
end;


// Procedure for selects short tracks on entire pcb.
// Used it while testing. It is not used in final script
procedure RemoveShortTracks(Dummy : Integer = 0);
var
   BIter      : IPCB_BoardIterator;
   Prim1      : IPCB_Primitive;
begin

   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;
   While Prim1 <> nil do
   begin
      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (Prim1.IsFreePrimitive) then
      begin
         if ((Abs(Prim1.x2 - Prim1.x1) < Tolerance) and (Abs(Prim1.y2 - Prim1.y1) < Tolerance)) then
            Prim1.Selected := True;
      end;
      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;
end;


// Procedure for selects short arcs on entire pcb.
// Used it while testing. It is not used in final script
procedure RemoveShortArcs(Dummy : Integer = 0);
var
   BIter      : IPCB_BoardIterator;
   Prim1      : IPCB_Primitive;
begin

   // In case of reuse - Clear selection before
   // ResetParameters;
   // AddStringParameter('Scope','All');
   // RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eArcObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;
   While Prim1 <> nil do
   begin
      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (Prim1.IsFreePrimitive) then
      begin
         if ((Prim1.EndAngle - Prim1.StartAngle) < 1) then Prim1.Selected := True;
      end;
      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // In case of reuse: Delete Selected objects
   // ResetParameters;
   // RunProcess('PCB:Clear');
   // Board.ViewManager_FullUpdate;
end;



Procedure Start;
Var
   Iterator : IPCB_BoardIterator;
   c        : IPCB_ObjectClass;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

// Commented out part was used while testing and it is not used any more

//   Tolerance := 1000;
//
//   // In case of reuse - Clear selection before
//   ResetParameters;
//   AddStringParameter('Scope','All');
//   RunProcess('PCB:DeSelect');
//
//   RemoveShortTracks;
//   RemoveShortArcs;
//   FixOverlappingTracks;
//   FixOverlappingArcs;
//
//   // I need to delete selection before moving on
//   ResetParameters;
//   RunProcess('PCB:Clear'); // Deletes selected objects
//
//   // I need two more functions:
//   // one for fixing primitives that cross each other - I need to split them :-(
//   // This is so hard to do it gives me headacke
//   SplitCrossOverTracks;
//
//   // I need to delete selection before moving on
//   ResetParameters;
//   RunProcess('PCB:Clear'); // Deletes selected objects
//
//   // one for fixing bad connections,
//   // FixBadConnections;
//
//   Board.ViewManager_FullUpdate;


   Iterator := Board.BoardIterator_Create;

   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
   c := Iterator.FirstPCBObject;
   While c <> NIl Do
   Begin
       If c.MemberKind = eClassMemberKind_Net Then
           ComboBoxClasses.Items.AddObject(c.Name, C);
       c := Iterator.NextPCBObject;
   End;

   ComboBoxClasses.ItemIndex := 0;

   FixOverlapsForm.ShowModal;
End;

// Functions Thant I Need
// IsPointOnTrack(Track : IPCB_Track, X : TCoord, Y : TCoord) : boolean
// IsPointOnArc  (Track : IPCB_Arc,   X : TCoord, Y : TCoord) : boolean
//     - function that checks if point is on a Arc

// GetTrackTrackIntersection(Track1 : IPCB_Track, Track2 : IPCB_Track, out X : TCoord, out Y : TCoord) : Boolean;
//     - function that checks intersection between two tracks

// GetTrackArcIntersection(Track : IPCB_Track, Arc : IPCB_Arc, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord, out Y2 : TCoord) : Integer;
// GetArcArcIntersection  (Arc1  : IPCB_Arc,  Arc2 : IPCB_Arc, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord, out Y2 : TCoord) : Integer;

// -----------------------------------------------------------------------------
//                      Form procedures & functions:
// -----------------------------------------------------------------------------

function IsStringANum(Tekst : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   if Tekst = '' then Result := False;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Tekst) do
      if not(((ord(Tekst[i]) > 47) and (ord(Tekst[i]) < 58)) or (ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Tekst) do
      if ((ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
      begin
         Inc(dotCount);
         if (i = 1) or (i = Length(Tekst)) then Result := False;
      end;

   if dotCount > 1 then Result := False;
end;

procedure TFixOverlapsForm.ButtonUnitsClick(Sender: TObject);
begin
   If ButtonUnits.Caption = 'mil' then
      ButtonUnits.Caption := 'mm'
   else
      ButtonUnits.Caption := 'mil'
end;

procedure TFixOverlapsForm.ButtonCancelClick(Sender: TObject);
begin
   close;
end;

procedure TFixOverlapsForm.ButtonOKClick(Sender: TObject);
var
   StringTolerance : String;
   OnLineDRC       : Boolean;
   KillList        : TStringList;
   Prim            : IPCB_Primitive;
   i               : Integer;
begin
   StringTolerance := EditTolerance.Text;
   if (LastDelimiter(',.', StringTolerance) <> 0) then StringTolerance[LastDelimiter(',.', StringTolerance)] := DecimalSeparator;

   if (ButtonUnits.Caption = 'mm') then Tolerance := mmsToCoord(StrToFloat(StringTolerance))
   else                                 Tolerance := milsToCoord(StrToFloat(StringTolerance));

   OnLineDRC := PCBServer.SystemOptions.DoOnlineDRC;
   PCBServer.SystemOptions.DoOnlineDRC := False;

   if CheckBoxShortPrims.Checked then
   begin
      ResetParameters;
      AddStringParameter('Scope','All');
      RunProcess('PCB:DeSelect');

      SelectShortPrimitivesInNetClass(ComboBoxClasses.Items.GetObject(ComboBoxClasses.ItemIndex));

      // I need to delete selection before moving on
      ResetParameters;
      RunProcess('PCB:Clear'); // Deletes selected objects
   end;

   if CheckBoxFixOverlaps.Checked then
   begin
      ResetParameters;
      AddStringParameter('Scope','All');
      RunProcess('PCB:DeSelect');

      FixOverlapsInNetClass(ComboBoxClasses.Items.GetObject(ComboBoxClasses.ItemIndex));

      // I need to delete selection before moving on
      ResetParameters;
      RunProcess('PCB:Clear'); // Deletes selected objects
   end;

   if CheckBoxSplitTTracks.Checked then
   begin
      ResetParameters;
      AddStringParameter('Scope','All');
      RunProcess('PCB:DeSelect');

      SplitCrossOverTracksInNetClass(ComboBoxClasses.Items.GetObject(ComboBoxClasses.ItemIndex));

      // I need to delete selection before moving on
      ResetParameters;
      RunProcess('PCB:Clear'); // Deletes selected objects
   end;

   ResetParameters;
   AddStringParameter('Scope','All');
   RunProcess('PCB:DeSelect');


   if CheckBoxShortBadConnections.Checked then
   begin
      KillList := TStringList.Create;
      while True do
      begin
         KillList.Clear;

         SelectBadConnectionsInNetClass(ComboBoxClasses.Items.GetObject(ComboBoxClasses.ItemIndex));

         if Board.SelectecObjectCount > 0 then
         begin
            // We have selected bad connections, and on them we will check if they are short. If yes, then we will remove them

            for i := 0 to Board.SelectecObjectCount - 1 do
            begin
               Prim := Board.SelectecObject[i];

               if (Prim.ObjectID = eTrackObject) then
               begin
                  if (PointToPointDistance(Prim.x1, Prim.y1, Prim.x2, Prim.y2) < (Prim.Width / 10)) then
                     KillList.AddObject(IntToStr(i),Prim);
               end
               else if (Prim.ObjectID = eArcObject) then
               begin
                  if ((Prim.EndAngle - Prim.StartAngle) < 1) then
                     KillList.AddObject(IntToStr(i),Prim);
               end;
            end;

            if KillList.Count = 0 then break;

            For i := 0 TO KillList.Count - 1 Do
            begin
               Prim := KillList.GetObject(i);
               Board.RemovePCBObject(Prim);
            end;

            ResetParameters;
            AddStringParameter('Scope','All');
            RunProcess('PCB:DeSelect');
         end
         else break;
      end;
   end
   else if CheckBoxSelectBadConnections.Checked then
      SelectBadConnectionsInNetClass(ComboBoxClasses.Items.GetObject(ComboBoxClasses.ItemIndex));


   if (CheckBoxSelectBadConnections.Checked) and (Board.SelectecObjectCount > 0) then
         Client.PostMessage('PCB:RunQuery','Apply=True|Expr=IsSelected|Mask=True',
                Length('Apply=True|Expr=IsSelected|Mask=True'), Client.CurrentView);

   PCBServer.SystemOptions.DoOnlineDRC := OnLineDRC;

   close;
end;

procedure TFixOverlapsForm.EditToleranceChange(Sender: TObject);
begin
   If IsStringANum(EditTolerance.Text) then
   begin
      EditTolerance.Font.Color := clWindowText;
      ButtonOK.Enabled := True;
   end
   else
   begin
      ButtonOK.Enabled := False;
      EditTolerance.Font.Color := clRed;
   end;
end;

procedure TFixOverlapsForm.CheckBoxFixOverlapsClick(Sender: TObject);
begin
   CheckBoxSplitTTracks.Enabled := CheckBoxFixOverlaps.Checked;
   CheckBoxSplitTTracks.Checked := CheckBoxFixOverlaps.Checked;
end;

procedure TFixOverlapsForm.CheckBoxSelectBadConnectionsClick(Sender: TObject);
begin
   CheckBoxShortBadConnections.Enabled := CheckBoxSelectBadConnections.Checked;
   CheckBoxShortBadConnections.Checked := CheckBoxSelectBadConnections.Checked;
end;


