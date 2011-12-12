{..............................................................................}
{ Summary   This scripts can be used to select connected tracks and arcs on    }
{           same layer.                                                        }
{                                                                              }
{           It works better than SelectConnectedTracks because it does not     }
{           requaire that tracks and arcs have same connection point. They     }
{           only have to be connected.                                         }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board : IPCB_Board;
   Lista : TStringList;

function SelectOthers(Prim : IPCB_Primitive);
var
   Iterator : IPCB_SpatialIterator;
   Prim2    : IPCB_Primitive;
   Prim3    : IPCB_Primitive;
   i        : Integer;
   flag     : Boolean;
   Rectang  : TCoordRect;
begin
   Rectang := Prim.BoundingRectangle;

   Iterator := Board.SpatialIterator_Create;
   Iterator.AddFilter_LayerSet(MkSet(Prim.Layer));
   Iterator.AddFilter_Area(Rectang.Left, Rectang.Bottom, Rectang.Right, Rectang.Top);
   Iterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

   Prim2 := Iterator.FirstPCBObject;

   while Prim2 <> nil do
   begin
      if Prim.InPolygon then
         Flag := False
      else
         Flag := True;

      if Flag then
         for i := 0 to Lista.Count - 1 do
         begin
            Prim3 := Lista.GetObject(i);
            if Prim3 = Prim2 then
               Flag := False;
         end;

      if Flag then
         if Board.PrimPrimDistance(Prim, Prim2) = 0 then
         begin
            Prim2.Selected := True;
            Lista.AddObject(Lista.Count + 1, Prim2);
            SelectOthers(Prim2);
         end;

      Prim2 := Iterator.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(Iterator);
end;


Procedure SelectConnected;
var
   Prim  : IPCB_Primitive;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   Lista := TStringList.Create;

   while True do
   begin

      Prim := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject), AllLayers, 'Choose Track or Arc');

      if Prim = nil then exit;

      Prim.Selected := True;
      Lista.AddObject(Lista.Count + 1, Prim);

      SelectOthers(Prim);
   end;
end;
