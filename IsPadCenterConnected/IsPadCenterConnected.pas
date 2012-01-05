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
    Track                   : IPCB_Track;
    Pad                     : IPCB_Pad2;
    TrackIteratorHandle     : IPCB_SpatialIterator;
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    PadIteratorHandle       : IPCB_GroupIterator;
    TheLayerStack           : IPCB_LayerStack;
    LayerObj                : IPCB_LayerObject;
    Rectangle               : TCoordRect;
    TrackFoundFlag          : Integer;
    LayerFlag               : Integer;

    Left                    : Integer;
    Right                   : Integer;
    Top                     : Integer;
    Bottom                  : Integer;

    k, c                    : Float;
    i                       : Integer;
    PointX, PointY          : Integer;
    aboveLine               : Integer;
    onLine                  : Integer;
    belowLine               : Integer;

    Parameters              : String;
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
            if Pad.Innet then
            begin
               if Layer2String(Pad.Layer) = 'Multi Layer' then
               begin
                  Pad.Selected := False;
                  TheLayerStack := Board.LayerStack;

                  LayerObj := TheLayerStack.FirstLayer;

                  While LayerObj <> nil do
                  begin
                     if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
                     begin
                        Rectangle := Pad.BoundingRectangleOnLayer(LayerObj.LayerID);

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

                                 if Board.PrimPrimDistance(Track, Pad) = 0 then
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
                  Rectangle := Pad.BoundingRectangleOnLayer(Pad.Layer_V6);
                  TrackIteratorHandle        := Board.SpatialIterator_Create;
                  TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));
                  TrackIteratorHandle.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);

                  if Layer2String(Pad.Layer) = 'Top Layer' then
                     TrackIteratorHandle.AddFilter_LayerSet(MkSet(eTopLayer))
                  else if Layer2String(Pad.Layer) = 'Bottom Layer' then
                     TrackIteratorHandle.AddFilter_LayerSet(MkSet(eBottomLayer));

                  Track := TrackIteratorHandle.FirstPCBObject;
                  while (Track <> Nil) Do
                  begin
                      if (((Track.x1 = Pad.x) and (Track.y1 = Pad.y)) or ((Track.x2 = Pad.x) and (Track.y2 = Pad.y))) then
                         Pad.Selected := False;


                      Track := TrackIteratorHandle.NextPCBObject;
                  end;
                  Board.SpatialIterator_Destroy(TrackIteratorHandle);
               end;
            end;

            Pad := PadIteratorHandle.NextPCBObject;
        End;
        // fetch source designator of component
        Component.GroupIterator_Destroy(PadIteratorHandle);

        Component := ComponentIteratorHandle.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(ComponentIteratorHandle);

    Parameters := 'Apply=True|Expr=IsSelected|Index=1|Zoom=True|Select=True|Mask=True';
    Client.PostMessage('PCB:RunQuery', Parameters, Length(Parameters), Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}      
