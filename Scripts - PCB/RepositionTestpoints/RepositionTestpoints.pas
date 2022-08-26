{..............................................................................}
{ Summary   This script is used to place testpoints in bulk without having     }
{           to scroll over and manually select testpoints individually.        }
{                                                                              }
{           The script works by you selecting an object with a net, the        }
{           corresponding testpoint with a matching net is selected, then      }
{           finally runs the process 'Reposition Selected Components'.         }
{           Testpoints that are locked or already placed on their net          }
{           will not be selected.                                              }
{                                                                              }
{           Hold Shift to select multiple nets before placement                }
{           Hold Ctrl to allow selecting placed testpoints                     }
{                                                                              }
{           This script assumes that all testpoints have a Pad with an         }
{           assigned net and use the designator prefix 'TP'                    }
{                                                                              }
{           Changelog:                                                         }
{           - v1.0 - Initial Release                                           }
{           - v1.1 - Added dummy args to hide private procedures               }
{                                                                              }
{           Possible Future Improvements:                                      }
{           - Move testpoint to the layer of the selected trace                }
{           - Shift clicking on already selected net selects all TPs with net  }
{           - Fix issue with Testpoint Placement check with polygons           }
{                                                                              }
{ Created by:    Corey Beyer                                                   }
{..............................................................................}

{..............................................................................}
var
    Board : IPCB_Board;



function TestpointIsPlaced(Testpoint : IPCB_Component, TestpointNet : IPCB_Net) : Boolean;
//  Known Issues with Function:
//      - Can incorrectly assume testpoint is connected if polygon is not perfect square/rectangle
//        or if the copper bounds include the testpoint despite not being poured over it.
var
    PadIterator : IPCB_GroupIterator;
    SpatialIterator : IPCB_SpatialIterator;
    PolygonIterator : IPCB_BoardIterator;
    TestpointBoundary : TCoordRect;
    Pad : IPCB_Pad;
    Polygon : IPCB_Polygon;
    Primitive : IPCB_Primitive;
    IsPlaced : Boolean;
begin
    IsPlaced := False;

    TestpointBoundary := Testpoint.BoundingRectangle;

    PadIterator := Testpoint.GroupIterator_Create;
    PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    SpatialIterator := Board.SpatialIterator_Create;
    SpatialIterator.AddFilter_AllLayers;
    SpatialIterator.AddFilter_Area(TestpointBoundary.Left, TestpointBoundary.Bottom, TestpointBoundary.Right, TestpointBoundary.Top);
    SpatialIterator.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject, eFillObject, eRegionObject, eViaObject));

    PolygonIterator := Board.BoardIterator_Create;
    PolygonIterator.AddFilter_AllLayers;
    PolygonIterator.AddFilter_Area(TestpointBoundary.Left, TestpointBoundary.Bottom, TestpointBoundary.Right, TestpointBoundary.Top);
    PolygonIterator.AddFilter_ObjectSet(MkSet(ePolyObject));

    Pad := PadIterator.FirstPCBObject;
    while Pad <> nil do
    begin
        if (Pad.Net = TestpointNet) then
        begin

            // Check for overlapping Copper
            Primitive := SpatialIterator.FirstPCBObject;
            while Primitive <> nil do
            begin
                if ((Primitive.Layer = Pad.Layer) or (Pad.Layer = eMultiLayer)) and (Board.PrimPrimDistance(Primitive, Pad) = 0) and (Primitive.Net = TestpointNet) then
                begin
                    IsPlaced := True;
                    break;
                end;
                Primitive := SpatialIterator.NextPCBObject;
            end;

            // Check for overlapping Polygons
            Polygon := PolygonIterator.FirstPCBObject;
            while Polygon <> nil do
            begin
                if ((Polygon.Layer = Pad.Layer) or (Pad.Layer = eMultiLayer)) and (Board.PrimPrimDistance(Polygon, Pad) = 0) and (Polygon.Net = TestpointNet) then
                begin
                    IsPlaced := True;
                    break;
                end;
                Polygon := PolygonIterator.NextPCBObject;
            end;

        end;
        if IsPlaced then break;
        Pad := PadIterator.NextPCBObject;
    end;

    Testpoint.GroupIterator_Destroy(PadIterator);
    Board.SpatialIterator_Destroy(SpatialIterator);
    Board.BoardIterator_Destroy(PolygonIterator);

    Result := IsPlaced;
end;



function ComponentHasNetPad(Component : IPCB_Component, Net : IPCB_Net) : Boolean;
var
    PadIterator : IPCB_GroupIterator;
    Pad : IPCB_Primitive;

begin
    Result := False;

    if (Component <> nil) then
    begin
        PadIterator := Component.GroupIterator_Create;
        PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

        Pad := PadIterator.FirstPCBObject;
        while (Pad <> nil) do
        begin
            if (Pad.Net = Net) then
            begin
                Result := True;
                break;
            end;
            Pad := PadIterator.NextPCBObject;
        end;

        Component.GroupIterator_Destroy(PadIterator);
    end;

end;



function GetTestpointWithNet(Net : IPCB_Net) : IPCB_Component;
var
    Iterator : IPCB_BoardIterator;
    Component : IPCB_Component;

begin
    Result := nil;

    if (Net <> nil) then
    begin
        Iterator := Board.BoardIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));

        Component := Iterator.FirstPCBObject;
        while Component <> nil do
        begin
            if Component.Moveable and (Component.SourceDesignator like 'TP*') and ComponentHasNetPad(Component, Net) and (not TestpointIsPlaced(Component, Net) or ControlKeyDown) then
            begin
                Result := Component;
                break;
            end;
            Component := Iterator.NextPCBObject;
        end;

        Board.BoardIterator_Destroy(Iterator);
    end;
end;



function SelectNet(Const Dummy : Integer) : IPCB_Net;
var
    Primitive : IPCB_Primitive;

begin
    Result := nil;
    Primitive := Board.GetObjectAtCursor(MkSet(eArcObject, eViaObject, eTrackObject, eFillObject, ePadObject, eNetObject, ePolyObject, eRegionObject), AllLayers, 'Select a Net');
    if (Primitive <> nil) and (Primitive.Net <> nil) then
        Result := Primitive.Net;
end;



procedure Start;
var
    Net : IPCB_Net;
    Testpoint : IPCB_Component;
    TestpointList : TInterfaceList;
    i : Integer;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then Exit;

    TestpointList := TInterfaceList.Create;

    while True do
    begin
        Net :=  SelectNet(0);
        if (Net = nil) and (TestpointList.Count = 0) then
            break;

        Testpoint := GetTestpointWithNet(Net);
        if (Testpoint <> nil) then
        begin
            Testpoint.Selected := True;
            TestpointList.Add(Testpoint);
        end;

        if (ShiftKeyDown or (TestpointList.Count = 0)) and (Net <> nil) then
            continue;

        ResetParameters;
        AddStringParameter('RepositionSelected', 'True');
        RunProcess('PCB:PlaceComponent');

        for i := 0 to TestpointList.Count - 1 do
        begin
            TestpointList[i].Selected := False;
        end;
        TestpointList.Clear;

    end;

    TestpointList.Free;
end;
