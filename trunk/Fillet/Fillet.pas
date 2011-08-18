{..............................................................................}
{ Summary   This Script creates fillet on selected tracks                      }
{                                                                              }
{           Radius value = (shorter of two lines) * 0,5 * Slider Position      }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board         : IPCB_Board;

   (* I need string list to memorize radius of each track.
   Since arc is added betwen two tracks, I will need to find lesser value of the two

   Data will be stored in stringlist in order:
   I_ObjectAdress1;radius1
   I_ObjectAdress2;radius2
   ....
   ....
   I_ObjectAdressN;radiusN
   *)
   RadiusList     : TStringList;



procedure TForm1.ButtonCancelClick(Sender: TObject);
Var                     
   close;
end;



procedure TForm1.Form1Create(Sender: TObject);
var
    Iterator1 : IPCB_BoardIterator;
    Track     : IPCB_Track;
    Leng      : Integer;
    Flag      : Integer;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Iterator for first track
    Iterator1 := Board.BoardIterator_Create;
    Iterator1.AddFilter_ObjectSet(MkSet(eTrackObject));
    Iterator1.AddFilter_LayerSet(AllLayers);
    Iterator1.AddFilter_Method(eProcessAll);

    // Start iterating through tracks
    Track := Iterator1.FirstPCBObject;

    flag := 0;
    RadiusList := TStringList.Create;


    while (Track <> Nil) do
    begin
       if Track.Selected then
       begin
          Leng := Int(sqrt(sqr(Track.x2 - Track.x1) + sqr(Track.y2 - Track.y1)) * 0.5);
          RadiusList.Add(IntToStr(Track.I_ObjectAddress) + ';' + IntToStr(Leng));

          flag := 1;
       end;
       Track := Iterator1.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iterator1);

    If flag = 0 then
    begin
       showMessage('No Selected Tracks');
       close;
    end;
end;



procedure TForm1.ButtonOKClick(Sender: TObject);
var
    Iterator1     : IPCB_BoardIterator;
    Iterator2     : IPCB_BoardIterator;
    FirstTrack    : IPCB_Track;
    SecondTrack   : IPCB_Track;
    XCommon       : Integer;
    YCommon       : Integer;  
    angle1        : Double;
    angle2        : Double;
    Radius        : Integer;
    k1            : Double;
    k2            : Double;
    FirstCommon   : Integer;
    SecondCommon  : Integer;
    Xc            : Integer;
    Yc            : Integer;
    i, j          : integer;
    flag          : integer;
    X1, X2, Y1, Y2: Integer;
    StartAngle    : Double;
    StopAngle     : Double;
    Arc           : IPCB_Arc;
    Line          : String;
    ObjAdr        : Integer;
    Leng          : Integer;
begin
    // Iterator for first track
    Iterator1 := Board.BoardIterator_Create;
    Iterator1.AddFilter_ObjectSet(MkSet(eTrackObject));
    Iterator1.AddFilter_LayerSet(AllLayers);
    Iterator1.AddFilter_Method(eProcessAll);

     // Start iterating through tracks
    FirstTrack := Iterator1.FirstPCBObject;

    while (FirstTrack <> Nil) do
    begin
       if FirstTrack.Selected then
       begin
          // Iterator for second track
          Iterator2 := Board.BoardIterator_Create;
          Iterator2.AddFilter_ObjectSet(MkSet(eTrackObject));
          Iterator2.AddFilter_LayerSet(AllLayers);
          Iterator2.AddFilter_Method(eProcessAll);

          // Start iterating through tracks
          SecondTrack := Iterator2.FirstPCBObject;

          while (SecondTrack <> Nil) do
          begin
             if SecondTrack.Selected then
             begin
                if (SecondTrack.I_ObjectAddress <> FirstTrack.I_ObjectAddress) and
                (SecondTrack.Layer = FirstTrack.Layer) and (
                ((SecondTrack.x1 = FirstTrack.x1) and (SecondTrack.y1 = FirstTrack.y1)) or
                ((SecondTrack.x2 = FirstTrack.x1) and (SecondTrack.y2 = FirstTrack.y1)) or
                ((SecondTrack.x2 = FirstTrack.x2) and (SecondTrack.y2 = FirstTrack.y2)) or
                ((SecondTrack.x1 = FirstTrack.x2) and (SecondTrack.y1 = FirstTrack.y2))) then
                begin

                   // Here we are and two tracks are connected. now I need to check the point in common.
                   if (SecondTrack.x1 = FirstTrack.x1) and (SecondTrack.y1 = FirstTrack.y1) then
                   begin
                      XCommon := FirstTrack.x1;
                      YCommon := FirstTrack.y1;
                      FirstCommon  := 1;
                      SecondCommon := 1;
                   end
                   else if (SecondTrack.x2 = FirstTrack.x1) and (SecondTrack.y2 = FirstTrack.y1) then
                   begin
                      XCommon := FirstTrack.x1;
                      YCommon := FirstTrack.y1;
                      FirstCommon  := 1;
                      SecondCommon := 2;
                   end
                   else if (SecondTrack.x2 = FirstTrack.x2) and (SecondTrack.y2 = FirstTrack.y2) then
                   begin
                      XCommon := FirstTrack.x2;
                      YCommon := FirstTrack.y2;
                      FirstCommon  := 2;
                      SecondCommon := 2;
                   end
                   else if (SecondTrack.x1 = FirstTrack.x2) and (SecondTrack.y1 = FirstTrack.y2) then
                   begin
                      XCommon := FirstTrack.x2;
                      YCommon := FirstTrack.y2;
                      FirstCommon  := 2;
                      SecondCommon := 1;
                   end;

                   // now the angles of FirstTrack
                   if FirstCommon = 1 then
                   begin
                      // First point is common
                      if FirstTrack.x2 = FirstTrack.x1 then
                      begin
                         if FirstTrack.y1 > FirstTrack.y2 then Angle1 := 3 * PI / 2
                         else                                  Angle1 := PI / 2;
                      end
                      else
                         Angle1 := arctan((FirstTrack.y2 - FirstTrack.y1)/(FirstTrack.x2 - FirstTrack.x1));
                      if FirstTrack.x2 < FirstTrack.x1 then Angle1 := Angle1 + pi;
                   end
                   else
                   begin
                      // Second point is common
                      if FirstTrack.x2 = FirstTrack.x1 then
                      begin
                         if FirstTrack.y1 < FirstTrack.y2 then Angle1 := 3 * PI / 2
                         else                                  Angle1 := PI / 2;
                      end
                      else
                         Angle1 := arctan((FirstTrack.y1 - FirstTrack.y2)/(FirstTrack.x1 - FirstTrack.x2));
                      if FirstTrack.x1 < FirstTrack.x2 then Angle1 := Angle1 + pi;
                   end;

                   if Angle1 < 0 then Angle1 := 2 * pi + Angle1;

                   // now the angles of SecondTrack
                   if SecondCommon = 1 then
                   begin
                      // First point is common
                      if SecondTrack.x2 = SecondTrack.x1 then
                       begin
                         if SecondTrack.y1 > SecondTrack.y2 then Angle2 := 3 * PI / 2
                         else                                    Angle2 := PI / 2;
                      end
                      else
                         Angle2 := arctan((SecondTrack.y2 - SecondTrack.y1)/(SecondTrack.x2 - SecondTrack.x1));
                      if SecondTrack.x2 < SecondTrack.x1 then Angle2 := Angle2 + pi;
                   end
                   else
                   begin
                      // Second point is common
                      if SecondTrack.x2 = SecondTrack.x1 then
                      begin
                         if SecondTrack.y1 < SecondTrack.y2 then Angle2 := 3 * PI / 2
                         else                                    Angle2 := PI / 2;
                      end
                      else
                         Angle2 := arctan((SecondTrack.y1 - SecondTrack.y2)/(SecondTrack.x1 - SecondTrack.x2));
                      if SecondTrack.x1 < SecondTrack.x2 then Angle2 := Angle2 + pi;
                   end;

                   if Angle2 < 0 then Angle2 := 2 * pi + Angle2;

                   // Now we need to check weather we will be placing any arcs
                   if not ((Angle1 = Angle2) or
                      ((Angle1 > Angle2) and (Angle1 - PI = Angle2)) or
                      ((Angle1 < Angle2) and (Angle2 - PI = Angle1))) then
                      begin

                         i := 0;
                         flag := 0;

                         while i < RadiusList.Count do
                         begin
                            Line := RadiusList[i];
                            j := LastDelimiter(';', Line);
                            
                            ObjAdr := StrToInt(Copy(Line, 1, j-1));
                            Leng   := Int(StrToInt(Copy(Line, j+1, length(Line))) * ScrollBarPerc.Position / 100);

                            // ShowMessage(Line + ' ' + Copy(Line, 1, j-1) + ' ' + Copy(Line, j+1, length(Line)) + ' ' + IntToStr(FirstTrack.I_ObjectAddress) + ' ' + IntToStr(SecondTrack.I_ObjectAddress));

                            if (ObjAdr = FirstTrack.I_ObjectAddress) then
                            begin
                               if flag = 0 then           Radius := Leng
                               else if Radius > Leng then Radius := Leng;
                               flag := 1;
                            end;

                            if (ObjAdr = SecondTrack.I_ObjectAddress) then
                            begin
                               if flag = 0 then           Radius := Leng
                               else if Radius > Leng then Radius := Leng;
                               flag := 1;
                            end;
                            Inc(i);
                         end;

                         // modify point of FirstTrack
                         if FirstCommon = 1 then
                         begin
                            FirstTrack.x1 := FirstTrack.x1 + Radius*cos(Angle1);
                            FirstTrack.y1 := FirstTrack.y1 + Radius*sin(Angle1);
                            X1 := FirstTrack.x1;
                            Y1 := FirstTrack.y1;
                         end
                         else
                         begin
                            FirstTrack.x2 := FirstTrack.x2 + Radius*cos(Angle1);
                            FirstTrack.y2 := FirstTrack.y2 + Radius*sin(Angle1);
                            X1 := FirstTrack.x2;
                            Y1 := FirstTrack.y2;
                         end;

                         if Angle1 < 0 then Angle1 := pi + Angle1;

                         // modify point of SecondTrack
                         if SecondCommon = 1 then
                         begin
                            SecondTrack.x1 := SecondTrack.x1 + Radius*cos(Angle2);
                            SecondTrack.y1 := SecondTrack.y1 + Radius*sin(Angle2);
                            X2 := SecondTrack.x1;
                            Y2 := SecondTrack.y1;
                         end
                         else
                         begin
                            SecondTrack.x2 := SecondTrack.x2 + Radius*cos(Angle2);
                            SecondTrack.y2 := SecondTrack.y2 + Radius*sin(Angle2);
                            X2 := SecondTrack.x2;
                            Y2 := SecondTrack.y2;
                         end;

                         // Calculate X center of arc
                         if      ((Angle1 = 0) or (Angle1 = pi)) then Xc := X1
                         else if ((Angle2 = 0) or (Angle2 = pi)) then Xc := X2
                         else
                         begin
                            k1 := tan(pi/2 + Angle1);
                            k2 := tan(pi/2 + Angle2);

                            Xc := (Y2 - Y1 + k1 * X1 - k2 * X2) / (k1 - k2);
                         end;

                         // Calculate Y center of
                         if      ((Angle1 = Pi / 2) or (Angle1 = pi * 3 / 2)) then
                            Yc := Y1
                         else if ((Angle2 = Pi / 2) or (Angle2 = pi * 3 / 2)) then
                            Yc := Y2
                         else
                         begin
                            if ((Angle1 <> 0) and (Angle1 <> pi))      then Yc := tan(pi/2 + Angle1) * (Xc - X1) + Y1
                            else if ((Angle2 <> 0) and (Angle2 <> pi)) then Yc := tan(pi/2 + Angle2) * (Xc - X2) + Y2;
                         end;

                         // now we need to see what is first angle and what is second angle of an arc
                         if      ((Angle1 > Angle2) and (Angle1 - Angle2 < Pi)) then
                         begin
                            StartAngle := Pi / 2 +  Angle1;
                            StopAngle  := 3 * PI / 2 + Angle2;
                         end
                         else if ((Angle1 > Angle2) and (Angle1 - Angle2 > Pi)) then
                         begin
                            StartAngle := Pi / 2 +  Angle2;
                            StopAngle  := Angle1 - Pi / 2;
                         end
                         else if ((Angle1 < Angle2) and (Angle2 - Angle1 < Pi)) then
                         begin
                            StartAngle := Pi / 2 +  Angle2;
                            StopAngle  := 3 * PI / 2 + Angle1;
                         end
                         else if ((Angle1 < Angle2) and (Angle2 - Angle1 > Pi)) then
                         begin
                            StartAngle := Pi / 2 +  Angle1;
                            StopAngle  := Angle2 - Pi / 2;
                         end;

                         // Count radius - I have no idea why

                         Arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
                         Arc.XCenter    := Int(Xc);
                         Arc.YCenter    := Int(Yc);
                         Arc.Radius     := sqrt(sqr(X1 - Xc) + sqr(Y1 - Yc));;
                         Arc.LineWidth  := FirstTrack.Width;
                         Arc.StartAngle := StartAngle * 180 / pi;
                         Arc.EndAngle   := StopAngle * 180 / pi;
                         Arc.Layer      := FirstTrack.Layer;
                         Board.AddPCBObject(Arc);
                         Arc.Selected   := True;

                         // Check weather we need to delete some tracks
                         If ScrollBarPerc.Position = 100 then
                         begin
                            if ((FirstTrack.x1 = FirstTrack.x2)   and (FirstTrack.y1 = FirstTrack.y2))   then Board.RemovePCBObject(FirstTrack);
                            if ((SecondTrack.x1 = SecondTrack.x2) and (SecondTrack.y1 = SecondTrack.y2)) then Board.RemovePCBObject(SecondTrack);
                         end;
                      end;
                end;
             end;
           SecondTrack := Iterator2.NextPCBObject;
        end;
        Board.BoardIterator_Destroy(Iterator2);


       end;
       FirstTrack := Iterator1.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iterator1);

    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');

    close;
end;


