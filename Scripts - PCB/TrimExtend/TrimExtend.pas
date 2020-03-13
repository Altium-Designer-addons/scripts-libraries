{..............................................................................}
{ Summary   This script Trims or Extends many tracks to the first              }
{           (Destination) track that was selected. It is popular AutoCAD       }
{           function available in many 3D tools.                               }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure TrimExtend;
Var

    Board            : IPCB_Board;

    DestinTrack      : IPCB_Track;
    kDestin          : Float;

    Track2Modify     : IPCB_Track;
    kModify          : Float;

    PointX, PointY   : Integer;
    CursorX, CursorY : Integer;
    helpY            : Integer;

Begin

   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then Exit;


   while True do
   begin

      DestinTrack := Board.GetObjectAtCursor(MkSet(eTrackObject), Mkset(Board.CurrentLayer), 'Select Destination Track');
      if DestinTrack = nil then exit;

      While True do
      begin

         Track2Modify := Board.GetObjectAtCursor(MkSet(eTrackObject), Mkset(Board.CurrentLayer), 'Select Tracks to Extend');
         if Track2Modify = nil then break;

         PCBServer.PreProcess;
         Track2Modify.BeginModify;

         Board.NewUndo;

         // Get the cursor location - used for trim, when lines cross over
         CursorX := Board.XCursor;
         CursorY := Board.YCursor;

         if DestinTrack.x1 = DestinTrack.x2 then
         begin
            // this is the case when DestinTrack is vertical
            if Track2Modify.x1 <> Track2Modify.x2 then
            begin
               kModify := (Track2Modify.y2 - Track2Modify.y1)/(Track2Modify.x2 - Track2Modify.x1);

               // Nw we check if this line crosses over DestinTrack
               if ((Track2Modify.x1 <= DestinTrack.x1) and (Track2Modify.x2 <= DestinTrack.x1)) or ((Track2Modify.x1 >= DestinTrack.x1) and (Track2Modify.x2 >= DestinTrack.x1)) then
               begin
                  // Here tracks do not cross over

                  // Now we need to figure out which point from Track2Modify is
                  // closer to the Destintrack
                  if Abs(Track2Modify.x1 - DestinTrack.x1) < Abs(Track2Modify.x2 - DestinTrack.x1) then
                  begin
                     // this is Track2Modify.x1 is closer
                     Track2Modify.y1 := Track2Modify.y1 + Int(kModify * (DestinTrack.x1 - Track2Modify.x1));
                     Track2Modify.x1 := DestinTrack.x1;
                  end
                  else
                  begin
                     // This is Track2Modify.x2 is closer
                     Track2Modify.y2 := Track2Modify.y2 + Int(kModify * (DestinTrack.x2 - Track2Modify.x2));
                     Track2Modify.x2 := DestinTrack.x2;
                  end;
               end
               else
               begin
                  // Lines cross over, DestinTrack is vertical
                  if CursorX < DestinTrack.x1 then
                  begin
                     // keep left side

                     // we need to check wich point is on the left side
                     if Track2Modify.x2 < DestinTrack.x1 then
                     begin
                        // this is Track2Modify.x1 needs to be modified
                        Track2Modify.y1 := Track2Modify.y1 + Int(kModify * (DestinTrack.x1 - Track2Modify.x1));
                        Track2Modify.x1 := DestinTrack.x1;
                     end
                     else
                     begin
                        // This is Track2Modify.x2 needs to be modified
                        Track2Modify.y2 := Track2Modify.y2 + Int(kModify * (DestinTrack.x2 - Track2Modify.x2));
                        Track2Modify.x2 := DestinTrack.x2;
                     end;
                  end
                  else
                  begin
                     // Keeep right side

                     // we need to check wich point is on the right side
                     if Track2Modify.x2 > DestinTrack.x1 then
                     begin
                        // this is Track2Modify.x1 needs to be modified
                        Track2Modify.y1 := Track2Modify.y1 + Int(kModify * (DestinTrack.x1 - Track2Modify.x1));
                        Track2Modify.x1 := DestinTrack.x1;
                     end
                     else
                     begin
                        // This is Track2Modify.x2 needs to be modified
                        Track2Modify.y2 := Track2Modify.y2 + Int(kModify * (DestinTrack.x2 - Track2Modify.x2));
                        Track2Modify.x2 := DestinTrack.x2;
                     end;
                  end;
               end;
            end
            else ShowMessage('Tracks are parallel');
         end
         else
         begin
            // we will calculate K for destination track
            kDestin := (DestinTrack.y2 - DestinTrack.y1)/(DestinTrack.x2 - DestinTrack.x1);

            if Track2Modify.x1 <> Track2Modify.x2 then
            begin
               kModify := (Track2Modify.y2 - Track2Modify.y1)/(Track2Modify.x2 - Track2Modify.x1);

               if Abs(kDestin - kModify) < 0.1 then
                  ShowMessage('Tracks are parallel')
               else
               begin

                  // find cross point
                  PointX := Int((DestinTrack.y1 - Track2Modify.y1 + (kModify * Track2Modify.x1) - (kDestin * Destintrack.x1)) / (kModify - kDestin));
                  PointY := int(Track2Modify.y1 + kModify * (PointX - Track2Modify.x1));

                  // now we need to find out weather this 2 lines cross or not
                  if ((Track2Modify.x1 <= PointX) and (Track2Modify.x2 <= PointX)) or ((Track2Modify.x1 >= PointX) and (Track2Modify.x2 >= PointX)) then
                  begin
                     // Here tracks do not cross over

                     // Now we need to figure out which point from Track2Modify is
                     // closer to the Destintrack
                     if Abs(Track2Modify.x1 - PointX) < Abs(Track2Modify.x2 - PointX) then
                     begin
                        // this is Track2Modify.x1 is closer
                        Track2Modify.x1 := PointX;
                        Track2Modify.y1 := PointY;
                     end
                     else
                     begin
                        // This is Track2Modify.x2 is closer
                        Track2Modify.x2 := PointX;
                        Track2Modify.y2 := PointY;
                     end;
                  end
                  else
                  begin
                     // Lines cross over, they are not vertical

                     // We need to calculate weather point is above or below the DestinTrack
                     helpY := DestinTrack.y1 + Int(kDestin * (CursorX - DestinTrack.x1));

                     if CursorY < helpY then
                     begin
                        // keep part under axis

                        // now we need to figure out which point of Track2Modify is above the DestinTrack
                        helpY := DestinTrack.y1 + Int(kDestin * (Track2Modify.x1 - DestinTrack.x1));

                        if Track2Modify.y1 < helpY then
                        begin
                           // first point is under DestinTrack - modify second point
                           Track2Modify.x2 := PointX;
                           Track2Modify.y2 := PointY;
                        end
                        else
                        begin
                           // first point is above DestinTrack - modify it
                           Track2Modify.x1 := PointX;
                           Track2Modify.y1 := PointY;
                        end;
                     end
                     else
                     begin
                        // keep part above axis

                        // now we need to figure out which point of Track2Modify is under the DestinTrack
                        helpY := DestinTrack.y1 + Int(kDestin * (Track2Modify.x1 - DestinTrack.x1));

                        if Track2Modify.y1 > helpY then
                        begin
                           // first point is above DestinTrack - modify second point
                           Track2Modify.x2 := PointX;
                           Track2Modify.y2 := PointY;
                        end
                        else
                        begin
                           // first point is under DestinTrack - modify it
                           Track2Modify.x1 := PointX;
                           Track2Modify.y1 := PointY;
                        end;
                     end;
                  end;
               end;
            end
            else
            begin
               // Here we have vertical Track2Modify line.

               // We need to check weather it crosses over destin track
               PointY := DestinTrack.y1 + Int(kDestin * (Track2Modify.x1 - DestinTrack.x1));

               // Check weather linec cross over
               if ((Track2Modify.y1 <= PointY) and (Track2Modify.y2 <= PointY)) or ((Track2Modify.y1 >= PointY) and (Track2Modify.y2 >= PointY)) then
               begin
                  // This is where lines do not cross over

                  // Find point closer to DestinTrack
                  if Abs(Track2Modify.y1 - PointY) < Abs(Track2Modify.y2 - PointY) then
                     // this is Track2Modify.y1 is closer
                     Track2Modify.y1 := PointY
                  else
                     // This is Track2Modify.y2 is closer
                     Track2Modify.y2 := PointY;
               end
               else
               begin
                  // Lines cross over, Track2Modify is vertical
                  if CursorY < PointY then
                  begin
                     // we keep lower part

                     // we need to check wich point is under
                     if Track2Modify.y2 < PointY then
                        // this is Track2Modify.y1 needs to be modified
                        Track2Modify.y1 := PointY
                     else
                        // This is Track2Modify.y2 needs to be modified
                        Track2Modify.y2 := PointY;
                  end
                  else
                  begin
                     // we keep upper part

                     // we need to check wich point is above
                     if Track2Modify.y2 > PointY then
                        // this is Track2Modify.y1 needs to be modified
                        Track2Modify.y1 := PointY
                     else
                        // This is Track2Modify.y2 needs to be modified
                        Track2Modify.y2 := PointY;
                  end;
               end;
            end;
         end;
         Track2Modify.GraphicallyInvalidate;

         Board.NewUndo();
         Track2Modify.EndModify;
         Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Track2Modify.I_ObjectAddress);
         PCBServer.PostProcess;
      end;
   end;
end;
