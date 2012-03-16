{..............................................................................}
{ Summary   This scripts can be used to measure copper area of a selected poly }
{           or region. You need to have selected only one poly or one region.  }
{                                                                              }
{           In case you select poly, it will measure overall poly area         }
{           If it is a solid polygon, it will also measure it's copper area.   }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


function RegionArea(Region : IPCB_Region) : Double;
var
   Area : Double;
   i : Integer;
begin
  Area := Region.MainContour.Area;
  for i := 0 to Region.HoleCount - 1 do
     Area := Area - Region.Holes[i].Area;
  Result := Area;
end;


Procedure CalculateCopperArea;
var
   Poly       : IPCB_Polygon;
   Region     : IPCB_Region;
   CopperArea : Double;
   Iter       : IPCB_GroupIterator;
begin
   If PCBServer.GetCurrentPCBBoard = nil then exit;

   if not ((PCBServer.GetCurrentPCBBoard.SelectecObjectCount = 1) and ((PCBServer.GetCurrentPCBBoard.SelectecObject[0].ObjectID = ePolyObject) or (PCBServer.GetCurrentPCBBoard.SelectecObject[0].ObjectID = eRegionObject))) then
   begin
      ShowMessage('Please select only one Poly or one Region');
      exit;
   end;

   if (PCBServer.GetCurrentPCBBoard.SelectecObject[0].ObjectID = ePolyObject) then
   begin
      Poly := PCBServer.GetCurrentPCBBoard.SelectecObject[0];

      if Poly.PolyHatchStyle <> ePolySolid then
      begin
         ShowMessage('Overall Poly Area: ' + FloatToStr(Poly.AreaSize / 100000000) + #13#10#13#10 + 'Copper Area Calculation is not supported in Hatched Poly.');
         exit;
      end;

      Iter := Poly.GroupIterator_Create;

      CopperArea := 0;
      Region := Iter.FirstPCBObject;
      While Region <> nil do
      begin
         CopperArea := CopperArea + RegionArea(Region);

         Region := Iter.NextPCBObject;
      end;
      Poly.GroupIterator_Destroy(Iter);

      ShowMessage('Overall Poly Area:   ' + FormatFloat(',0', Poly.AreaSize / 100000000) + ' sq mils' + #13#10 + 'Copper Area:           ' + FormatFloat(',0',CopperArea / 100000000) + ' sq mils.');

   end
   else
   begin
      CopperArea := RegionArea(PCBServer.GetCurrentPCBBoard.SelectecObject[0]);
      ShowMessage('Region Area:   ' + FormatFloat(',0',CopperArea / 100000000) + ' sq mils.');
   end;
end;
