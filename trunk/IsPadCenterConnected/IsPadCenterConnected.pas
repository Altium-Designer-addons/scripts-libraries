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
                              // I will need much deeper Check here, because 45° tracks get detected
                              // if their bounding rectangle is within pad.

                              Left   := Rectangle.Left   - Track.Width / 2;
                              Right  := Rectangle.Right  + Track.Width / 2;
                              Bottom := Rectangle.Bottom - Track.Width / 2;
                              Top    := Rectangle.Top    + Track.Width / 2;

                              if ((Track.x1 > Left)   and (Track.x1 < Right)) and
                                 ((Track.y1 > Bottom) and (Track.y1 < Top)) then

                                          TrackFoundFlag := 1

                              else if ((Track.x2 > Left)   and (Track.x2 < Right)) and
                                      ((Track.y2 > Bottom) and (Track.y2 < Top)) then

                                          TrackFoundFlag := 1

                              else if (Track.x1 = Track.x2) and (Track.x1 > Left) and (Track.x1 < Right) then

                                          TrackFoundFlag := 1

                              else if (Track.y1 = Track.y2) and (Track.y1 > Bottom) and (Track.y1 < Top) then

                                          TrackFoundFlag := 1

                              else
                              begin
                                 // now I need real trigonometry here.


                                 k := (Track.y2 - Track.y1) / (Track.x2 - Track.x1);
                                 c := Track.y1 - k * Track.x1;

                                 aboveLine := 0;
                                 onLine    := 0;
                                 belowLine := 0;


                                 for i := 1 to 4 do
                                 begin
                                    if i = 1 then
                                    begin
                                       PointX := Left;
                                       PointY := Bottom;
                                    end
                                    else if i = 2 then
                                    begin
                                       PointX := Right;
                                       PointY := Bottom;
                                    end
                                    else if i = 3 then
                                    begin
                                       PointX := Left;
                                       PointY := Top;
                                    end
                                    else if i = 4 then
                                    begin
                                       PointX := Right;
                                       PointY := Top;
                                    end;

                                    if (k * PointX + c < PointY) then
                                       aboveLine := 1
                                    else if (k * PointX + c > PointY) then
                                       belowLine := 1
                                    else
                                       onLine := 1;
                                 end;

                                 // Now is the time to check
                                 if OnLine or (AboveLine and Belowline) then
                                    TrackFoundFlag := 1;
                              end;

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
