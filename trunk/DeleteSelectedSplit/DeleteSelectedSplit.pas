{..............................................................................}
{ Summary   This Script Deletes selected split plane                           }
{                                                                              }
{           Actually it creates region based on selected split plane,          }
{           so copper area will not be there.                                  }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

procedure DeleteSelectedSplits;
var
    Board         : IPCB_Board;
    BoardIterator : IPCB_BoardIterator;
    Split         : IPCB_SplitPlane;
    Region        : IPCB_Region;
    NewRegion     : IPCB_Region;
    PolyIterator  : IPCB_GroupIterator;
    Contour       : IPCB_Contour;
    I,J             : Integer;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Iterator for split planes
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eSplitPlaneObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    // Start iterating through regions
    Split := BoardIterator.FirstPCBObject;

    while (Split <> Nil) do
    begin
       if Split.Selected then
       begin
          PolyIterator := Split.GroupIterator_Create;
          PolyIterator.AddFilter_ObjectSet(MkSet(eRegionObject));

          Region := PolyIterator.FirstPCBObject;

          NewRegion := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension,eCreate_Default);
          NewRegion.Layer := Region.Layer;

          Contour := Region.MainContour.Replicate;
          NewRegion.SetOutlineContour(Contour);

          For I := 0 To Region.HoleCount - 1 Do
          begin
              Contour := Region.Holes[I];
              NewRegion.GeometricPolygon.AddContourIsHole(Contour, True);
          end;

          Board.AddPCBObject(NewRegion);
          NewRegion.GraphicallyInvalidate;

          // Workaround to force redraw of split;
          I := Split.x;
          Split.x := 1;
          Split.GraphicallyInvalidate;
          Split.x := I;
          Split.GraphicallyInvalidate;
       end;
       Split := BoardIterator.NextPCBObject;
    end;
    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');
end;
