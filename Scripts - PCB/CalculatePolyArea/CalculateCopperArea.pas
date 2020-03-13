{..............................................................................}
{ Summary   This scripts can be used to measure copper area of a selected poly }
{           or region. You need to have selected only one poly or one region.  }
{                                                                              }
{           In case you select poly, it will measure overall poly area         }
{           If it is a solid polygon, it will also measure it's copper area.   }
{                                                                              }
{           In version 2.0 you can switch units, by clicking on 'Retry'.       }
{           Unit number is being copied to clipboard.                          }
{           To set default (initial) unit, modify 'Units' variable in line 30  }
{           according to instructions described there.                         }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


Procedure ShowArea(PolyArea : Double; CopperArea : Double);
var
   button   : Integer;
   Units    : Integer;
   CBString : String;
   CLPBRD   : TClipBoard;
begin
   //---------------------------------------------------------------------------
   //       S E T    I N I T I A L    U N I T    H E R E
   //---------------------------------------------------------------------------
   Units := 0;
   // 0 - sq mil
   // 1 - sq inch
   // 2 - sq mm
   // 3 - sq cm

   if PolyArea = 0 then
   begin
      // Region area
      repeat
         if Units = 0 then
         begin
            // mils
            CBString := FormatFloat(',0',CopperArea / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0',CopperArea / 100000000) + ' sq mil.' + #13#10#13#10 + 'Click ''Retry'' to display in sq inch.',mtCustom,mkset(mbOK,mbRetry), 0);
            Inc(Units);
         end
         else if Units = 1 then
         begin
            // inches
            CBString := FormatFloat(',0.######',CopperArea / 100000000 / 1000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',CopperArea / 100000000 / 1000000) + ' sq inch.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mm.',mtCustom,mkset(mbRetry,mbOK), 0);
            Inc(Units);
         end
         else if Units = 2 then
         begin
            // milimeters
            CBString := FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 / 100000000) + ' sq mm.' + #13#10#13#10 + 'Click ''Retry'' to display in sq cm.',mtCustom,mkset(mbRetry,mbOK), 0);
            Inc(Units);
         end
         else if Units = 3 then
         begin
            // centimeters
            CBString := FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 * 0.01 / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 * 0.01 / 100000000) + ' sq cm.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mil.',mtCustom,mkset(mbRetry,mbOK), 0);
            Units := 0;
         end;
      until button = 1;
   end
   else if CopperArea = 0 then
   begin
      // Hatched poly
      repeat
         if Units = 0 then
         begin
            // mils
            CBString := FormatFloat(',0',PolyArea / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0',PolyArea / 100000000) + ' sq mil.' + #13#10 + 'Copper Area Calculation is not supported in Hatched Poly.' + #13#10#13#10 + 'Click ''Retry'' to display in sq inch.',mtCustom,mkset(mbOK,mbRetry), 0);
            Inc(Units);
         end
         else if Units = 1 then
         begin
            // inches
            CBString := FormatFloat(',0.######',PolyArea / 100000000 / 1000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',PolyArea / 100000000 / 1000000) + ' sq inch.' + #13#10 + 'Copper Area Calculation is not supported in Hatched Poly.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mm.',mtCustom,mkset(mbRetry,mbOK), 0);
            Inc(Units);
         end
         else if Units = 2 then
         begin
            // milimeters
            CBString := FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 / 100000000) + ' sq mm.' + #13#10 + 'Copper Area Calculation is not supported in Hatched Poly.' + #13#10#13#10 + 'Click ''Retry'' to display in sq cm.',mtCustom,mkset(mbRetry,mbOK), 0);
            Inc(Units);
         end
         else if Units = 3 then
         begin
            // centimeters
            CBString := FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 * 0.01 / 100000000);
            button := MessageDLG('Overall Poly Area: ' + FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 * 0.01 / 100000000) + ' sq cm.' + #13#10 + 'Copper Area Calculation is not supported in Hatched Poly.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mil.',mtCustom,mkset(mbRetry,mbOK), 0);
            Units := 0;
         end;
      until button = 1;
   end
   else
   begin
      // Solid poly
      repeat
         if Units = 0 then
         begin
            // mils
            CBString := FormatFloat(',0',CopperArea / 100000000);
            button := MessageDLG('Overall Poly Area:  ' + FormatFloat(',0',PolyArea / 100000000) + ' sq mil.' + #13#10 + 'Copper Area:          ' + FormatFloat(',0',CopperArea / 100000000) + ' sq mil.' + #13#10#13#10 + 'Click ''Retry'' to display in sq inch.',mtCustom,mkset(mbOK,mbRetry), 0);
            Inc(Units);
         end
         else if Units = 1 then
         begin
            // inches
            CBString := FormatFloat(',0.######',CopperArea / 100000000 / 1000000);
            button := MessageDLG('Overall Poly Area:  ' + FormatFloat(',0.######',PolyArea / 100000000 / 1000000) + ' sq inch.' + #13#10 + 'Copper Area:          ' + FormatFloat(',0.######',CopperArea / 100000000 / 1000000) + ' sq inch.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mm.',mtCustom,mkset(mbOK,mbRetry), 0);
            Inc(Units);
         end
         else if Units = 2 then
         begin
            // milimeters
            CBString := FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 / 100000000);
            button := MessageDLG('Overall Poly Area:  ' + FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 / 100000000) + ' sq mm.' + #13#10 + 'Copper Area:          ' + FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 / 100000000) + ' sq mm.' + #13#10#13#10 + 'Click ''Retry'' to display in sq cm.',mtCustom,mkset(mbOK,mbRetry), 0);
            Inc(Units);
         end
         else if Units = 3 then
         begin
            // centimeters
            CBString := FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 * 0.01 / 100000000);
            button := MessageDLG('Overall Poly Area:  ' + FormatFloat(',0.######',PolyArea * 0.0254 * 0.0254 * 0.01 / 100000000) + ' sq cm.' + #13#10 + 'Copper Area:          ' + FormatFloat(',0.######',CopperArea * 0.0254 * 0.0254 * 0.01 / 100000000) + ' sq cm.' + #13#10#13#10 + 'Click ''Retry'' to display in sq mil.',mtCustom,mkset(mbOK,mbRetry), 0);
            Units := 0;
         end;
      until button = 1;
   end;

   CLPBRD := TClipboard.Create;
   CLPBRD.AsText := CBString;
end;


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
   Board      : IPCB_Board;
   Poly       : IPCB_Polygon;
   Region     : IPCB_Region;
   CopperArea : Double;
   Iter       : IPCB_GroupIterator;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   If Board = nil then exit;

   if not ((Board.SelectecObjectCount = 1) and ((Board.SelectecObject[0].ObjectID = ePolyObject) or (Board.SelectecObject[0].ObjectID = eRegionObject))) then
   begin
      ShowMessage('Please select only one Poly or one Region');
      exit;
   end;

   if (Board.SelectecObject[0].ObjectID = ePolyObject) then
   begin
      Poly := Board.SelectecObject[0];

      if Poly.PolyHatchStyle <> ePolySolid then
      begin
         ShowArea(Poly.AreaSize,0);
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

      ShowArea(Poly.AreaSize,CopperArea);
   end
   else
   begin
      CopperArea := RegionArea(Board.SelectecObject[0]);
      ShowArea(0,CopperArea);
   end;
end;
