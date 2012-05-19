{..............................................................................}
{ Summary   This script checks weather Vias are connected on only one layer.   }
{           If not, it is masked.                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

Procedure SelectViaAntennas;
var
   Board         : IPCB_Board;
   Iter          : IPCB_BoardIterator;
   SpIter        : IPCB_SpatialIterator;
   Via           : IPCB_Via;
   Prim          : IPCB_Primitive;
   TheLayerStack : IPCB_LayerStack;
   LayerObj      : IPCB_LayerObject;
   Rectangle     : TCoordRect;
   Connected     : Integer;

begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   ResetParameters;
   AddStringParameter('Scope', 'All');
   RunProcess('PCB:DeSelect');

   Iter := Board.BoardIterator_Create;
   Iter.AddFilter_ObjectSet(MkSet(eViaObject));
   Iter.AddFilter_AllLayers;

   Via := Iter.FirstPCBObject;
   TheLayerStack := Board.LayerStack;

   While (Via <> nil) do
   begin
      LayerObj := TheLayerStack.FirstLayer;
      Connected := 0;
      Rectangle := Via.BoundingRectangle;

      While LayerObj <> nil do
      begin
         if Via.IntersectLayer(LayerObj.V7_LayerID) then
            if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
            begin

               SpIter := Board.SpatialIterator_Create;
               SpIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, ePadObject, eFillObject, eRegionObject));
               SpIter.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);
               SpIter.AddFilter_LayerSet(MkSet(LayerObj.LayerID));

               Prim := SpIter.FirstPCBObject;

               While (Prim <> Nil) Do
               Begin
                  if Board.PrimPrimDistance(Prim, Via) = 0 then
                  begin
                     Inc(Connected);
                     break;
                  end;

                  Prim := SpIter.NextPCBObject;
               End;
               Board.SpatialIterator_Destroy(SpIter);
            end
            else
            begin
               if Via.IsConnectedToPlane[LayerObj.LayerID] then
                  Inc(Connected);
            end;
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      if Connected = 1 then Via.Selected := True;

      Via := Iter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(Iter);

   Client.PostMessage('PCB:RunQuery','Apply=True|Expr=IsSelected|Mask=True', Length('Apply=True|Expr=IsSelected|Mask=True'), Client.CurrentView);
end;
