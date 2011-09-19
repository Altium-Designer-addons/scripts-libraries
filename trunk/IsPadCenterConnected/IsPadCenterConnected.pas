{..............................................................................}
{ Summary   This script checks weather Pad has track in it's center.           }
{           If not, pad is being selected.                                     }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure IsPadCenterConnected;
Var

    Board                   : IPCB_Board;
    Track                   : IPCB_Primitive;
    Pad                     : IPCB_Primitive;
    TrackIteratorHandle     : IPCB_SpatialIterator;
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    PadIteratorHandle       : IPCB_GroupIterator;
    TheLayerStack           : IPCB_LayerStack;
    LayerObj                : IPCB_LayerObject;
    Rectangle               : TCoordRect;
    TrackFoundFlag          : Integer;
    LayerFlag               : Integer;

Begin

    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    ResetParameters;
    AddStringParameter('Scope', 'All');
    RunProcess('PCB:DeSelect');

    ComponentIteratorHandle := Board.BoardIterator_Create;
    ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIteratorHandle.AddFilter_LayerSet(AllLayers);
    ComponentIteratorHandle.AddFilter_Method(eProcessAll);


    Component := ComponentIteratorHandle.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
        PadIteratorHandle := Component.GroupIterator_Create;
        PadIteratorHandle.AddFilter_ObjectSet(MkSet(ePadObject));

        Pad := PadIteratorHandle.FirstPCBObject;
        While (Pad <> Nil) Do
        Begin
            Rectangle := Pad.BoundingRectangle;


            if Layer2String(Pad.Layer) = 'Multi Layer' then
            begin
               Pad.Selected := False;
               TheLayerStack := Board.LayerStack;

               LayerObj := TheLayerStack.FirstLayer;

               While LayerObj <> nil do
               begin
                  if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
                  begin
                     TrackIteratorHandle        := Board.SpatialIterator_Create;
                     TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));
                     TrackIteratorHandle.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);
                     TrackIteratorHandle.AddFilter_LayerSet(MkSet(LayerObj.LayerID));

                     Track := TrackIteratorHandle.FirstPCBObject;

                     TrackFoundFlag  := 0;
                     LayerFlag       := 0;

                     While (Track <> Nil) Do
                     Begin
                        if Track.InNet and Pad.InNet then
                           If Track.Net.Name = Pad.Net.Name then
                           begin

                              TrackFoundFlag := 1;
                              if (((Track.x1 = Pad.x) and (Track.y1 = Pad.y)) or ((Track.x2 = Pad.x) and (Track.y2 = Pad.y))) then
                                 LayerFlag := 1;

                           end;

                        Track := TrackIteratorHandle.NextPCBObject;
                     End;

                     if (TrackFoundFlag = 1) and (LayerFlag = 0) then
                        Pad.Selected := True;

                  end;

                  LayerObj := TheLayerStack.NextLayer(LayerObj);
               end;

            end
            else
            begin
               Pad.Selected               := True;
               TrackIteratorHandle        := Board.SpatialIterator_Create;
               TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));
               TrackIteratorHandle.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);

               if Layer2String(Pad.Layer) = 'Top Layer' then
                  TrackIteratorHandle.AddFilter_LayerSet(MkSet(eTopLayer))
               else if Layer2String(Pad.Layer) = 'Bottom Layer' then
                  TrackIteratorHandle.AddFilter_LayerSet(MkSet(eBottomLayer));

               Track := TrackIteratorHandle.FirstPCBObject;
               While (Track <> Nil) Do
               Begin
                   if (((Track.x1 = Pad.x) and (Track.y1 = Pad.y)) or ((Track.x2 = Pad.x) and (Track.y2 = Pad.y))) then
                      Pad.Selected := False;


                   Track := TrackIteratorHandle.NextPCBObject;
               End;

               if not Pad.Innet then Pad.Selected := False;
            end;

            Board.SpatialIterator_Destroy(TrackIteratorHandle);

            Pad := PadIteratorHandle.NextPCBObject;
        End;
        // fetch source designator of component
        Component.GroupIterator_Destroy(PadIteratorHandle);

        Component := ComponentIteratorHandle.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(ComponentIteratorHandle);

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
End;
{..............................................................................}

{..............................................................................}      
