{..............................................................................}
{ Summary   This script will update net on all selected object on mouse click. }
{                                                                              }
{           Prior to executing script you need to select objects whose nets    }
{           you wish to modify. After that you execute a script, and it will   }
{           ask you to click on one object. Net from that object will be added }
{           to all selected objects.                                           }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure UpdateNetOnSelectionWithClick;
var
   Board     : IPCB_Board;
   Prim      : IPCB_Primitive;
   Net       : IPCB_Net;
   i         : Integer;
   LSet      : IPCB_Layerset;
   ALayerSet : TLayerSet;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   ALayerSet := MkSet(eTopLayer,   eMidLayer1,  eMidLayer2,  eMidLayer3,  eMidLayer4,  eMidLayer5,  eMidLayer6,  eMidLayer7,  eMidLayer8,
                      eMidLayer9,  eMidLayer10, eMidLayer11, eMidLayer12, eMidLayer13, eMidLayer14, eMidLayer15, eMidLayer16, eMidLayer17,
                      eMidLayer18, eMidLayer19, eMidLayer20, eMidLayer21, eMidLayer22, eMidLayer23, eMidLayer24, eMidLayer25, eMidLayer26,
                      eMidLayer27, eMidLayer28, eMidLayer29, eMidLayer30, eBottomLayer, eMultiLayer);

   Prim := Board.GetObjectAtCursor(MkSet(eArcObject, eViaObject, eTrackObject, eFillObject, ePadObject, ePolyObject, eRegionObject), ALayerSet, 'Choose primitive');

   if Prim = nil then exit;

   if Prim.InPolygon then Prim := Prim.Polygon;

   Net := Prim.Net;

   LSet := LayerSet.SignalLayers;
   LSet.Include(String2Layer('Multi Layer'));

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim := Board.SelectecObject[i];

      if Prim.InPolygon then Prim := Prim.Polygon;

      if LSet.Contains(Prim.Layer) then
      Begin
         if (Prim.ObjectId = ePolyObject) then
            Prim.Net := Net
         else if (Net = nil) and Board.SelectecObject[i].InNet then
            Prim.Net.RemovePCBObject(Prim)
         else if (Net <> nil) and (Prim.ObjectId <> ePolyObject) then
            Net.AddPCBObject(Prim)
         else continue;

         Board.ViewManager_GraphicallyInvalidatePrimitive(Board.SelectecObject[i]);
      end;
   end;

   Board.ViewManager_FullUpdate;
end;

