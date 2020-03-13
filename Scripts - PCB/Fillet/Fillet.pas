{..............................................................................}
{ Summary   This Script creates fillet on selected tracks                      }
{                                                                              }
{           Radius value = (shorter of two lines) * percentage OR fixed value  }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{ Edited by:    Ryan Rutledge                                                  }
{           Edited 2018-09-15 to support any-angle tracks and fixed radius     }
{..............................................................................}

{..............................................................................}
var
    Board         : IPCB_Board;
    MinDistance   : Double;

    (* I need string list to memorize radius of each track.
    Since arc is added betwen two tracks, I will need to find lesser value of the two

    Data will be stored in stringlist in order:
    I_ObjectAdress1;radius1
    I_ObjectAdress2;radius2
    ....
    ....
    I_ObjectAdressN;radiusN
    *)
    RadiusList         :TStringList;




function IsStringANum(Text : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   if Text = '' then Result := False;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Text) do
      if not(((ord(Text[i]) > 47) and (ord(Text[i]) < 58)) or (ord(Text[i]) = 44) or (ord(Text[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Text) do
      if ((ord(Text[i]) = 44) or (ord(Text[i]) = 46)) then
      begin
         Inc(dotCount);
         if (i = 1) or (i = Length(Text)) then Result := False;
      end;

   if dotCount > 1 then Result := False;
end;

procedure TForm1.ButtonOKClick(Sender: TObject);
var
    FirstTrack         :IPCB_Primitive;
    SecondTrack        :IPCB_Primitive;
    XCommon            :Integer;
    YCommon            :Integer;
    angle1             :Double;
    angle2             :Double;
    Radius             :Integer;
    k1                 :Double;
    k2                 :Double;
    FirstCommon        :Integer;
    SecondCommon       :Integer;
    Xc                 :Integer;
    Yc                 :Integer;
    i, j               :integer;
    flag               :integer;
    X1, X2, Y1, Y2     :Integer;
    StartAngle         :Double;
    StopAngle          :Double;
    ArcAngle           :Double;
    HalfAngle          :Double;
    Arc                :IPCB_Arc;
    Line               :String;
    TempString         :String;
    ObjAdr             :Int64;
    Leng               :Integer;
    ShortLength        :Integer;
    NumCommon          :Integer;
    Ratio              :Double;
    a, b               :Integer;

begin
    // Start undo
    PCBServer.PreProcess;

    NumCommon := 0;
    Ratio     := 0;

    TempString := tRadius.Text;
    if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    if (RadioUnitsMM.Checked = true) then Radius := mmsToCoord(StrToFloat(TempString))
    else if (RadioUnitsMils.Checked = true) then Radius := milsToCoord(StrToFloat(TempString))
    else Ratio := StrToFloat(TempString);

    For a := 0 to Board.SelectecObjectCount - 1 do
    begin
        FirstTrack := Board.SelectecObject[a];
        for b := 0 to Board.SelectecObjectCount - 1 do
        begin
            SecondTrack := Board.SelectecObject[b];
            if (FirstTrack.ObjectId = eTrackObject) and (SecondTrack.ObjectId = eTrackObject) and (a <> b) and
                (SecondTrack.Layer = FirstTrack.Layer) and (
                ((SecondTrack.x1 = FirstTrack.x1) and (SecondTrack.y1 = FirstTrack.y1)) or
                ((SecondTrack.x2 = FirstTrack.x1) and (SecondTrack.y2 = FirstTrack.y1)) or
                ((SecondTrack.x2 = FirstTrack.x2) and (SecondTrack.y2 = FirstTrack.y2)) or
                ((SecondTrack.x1 = FirstTrack.x2) and (SecondTrack.y1 = FirstTrack.y2))) then
            begin // two tracks are connected; find common point
                NumCommon := 1; // at least one common point was found so use this to suppress message after procedure

                // Here we are and two tracks are connected. now I need to check the point in common.
                if (SecondTrack.x1 = FirstTrack.x1) and (SecondTrack.y1 = FirstTrack.y1) then
                begin
                    XCommon := FirstTrack.x1;
                    YCommon := FirstTrack.y1;
                    FirstCommon := 1;
                    SecondCommon := 1;
                end
                else if (SecondTrack.x2 = FirstTrack.x1) and (SecondTrack.y2 = FirstTrack.y1) then
                begin
                    XCommon := FirstTrack.x1;
                    YCommon := FirstTrack.y1;
                    FirstCommon := 1;
                    SecondCommon := 2;
                end
                else if (SecondTrack.x2 = FirstTrack.x2) and (SecondTrack.y2 = FirstTrack.y2) then
                begin
                    XCommon := FirstTrack.x2;
                    YCommon := FirstTrack.y2;
                    FirstCommon := 2;
                    SecondCommon := 2;
                end
                else if (SecondTrack.x1 = FirstTrack.x2) and (SecondTrack.y1 = FirstTrack.y2) then
                begin
                    XCommon := FirstTrack.x2;
                    YCommon := FirstTrack.y2;
                    FirstCommon := 2;
                    SecondCommon := 1;
                end;

                // now the angles of FirstTrack
                if FirstCommon = 1 then
                begin
                    // First point is common
                    if FirstTrack.x2 = FirstTrack.x1 then
                    begin
                        if FirstTrack.y1 > FirstTrack.y2 then Angle1 := 3 * PI / 2
                        else Angle1 := PI / 2;
                    end
                    else
                        Angle1 := arctan((FirstTrack.y2 - FirstTrack.y1) / (FirstTrack.x2 - FirstTrack.x1));
                    if FirstTrack.x2 < FirstTrack.x1 then Angle1 := Angle1 + pi;
                end
                else begin
                    // Second point is common
                    if FirstTrack.x2 = FirstTrack.x1 then
                    begin
                        if FirstTrack.y1 < FirstTrack.y2 then Angle1 := 3 * PI / 2
                        else Angle1 := PI / 2;
                    end
                    else
                        Angle1 := arctan((FirstTrack.y1 - FirstTrack.y2) / (FirstTrack.x1 - FirstTrack.x2));
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
                        else Angle2 := PI / 2;
                    end
                    else
                        Angle2 := arctan((SecondTrack.y2 - SecondTrack.y1) / (SecondTrack.x2 - SecondTrack.x1));
                    if SecondTrack.x2 < SecondTrack.x1 then Angle2 := Angle2 + pi;
                end
                else begin
                    // Second point is common
                    if SecondTrack.x2 = SecondTrack.x1 then
                    begin
                        if SecondTrack.y1 < SecondTrack.y2 then Angle2 := 3 * PI / 2
                        else Angle2 := PI / 2;
                    end
                    else
                        Angle2 := arctan((SecondTrack.y1 - SecondTrack.y2) / (SecondTrack.x1 - SecondTrack.x2));
                    if SecondTrack.x1 < SecondTrack.x2 then Angle2 := Angle2 + pi;
                end;

                if Angle2 < 0 then Angle2 := 2 * pi + Angle2;
                //ShowMessage('first_X1,Y1: ' + IntToStr(FirstTrack.x1) + ',' + IntToStr(FirstTrack.y1) + ',' + FloatToStr(Angle1));

                // Now we need to check weather we will be placing any arcs
                if not ((Angle1 = Angle2) or
                    ((Angle1 > Angle2) and (Angle1 - pi = Angle2)) or
                    ((Angle1 < Angle2) and (Angle2 - pi = Angle1))) then
                begin

                    i    := 0;
                    flag := 0;

                    while i < RadiusList.Count do
                    begin
                        Line := RadiusList[i];
                        j    := LastDelimiter(';', Line);

                        ObjAdr := StrToInt64(Copy(Line, 1, j - 1));
                        if Ratio > 0 then    
                        begin 
                            Leng := Int(StrToInt64(Copy(Line, j + 1, length(Line))) * Ratio / 100)
                        end
                        else Leng                 := Radius;

                        // ShowMessage(Line + ' ' + Copy(Line, 1, j-1) + ' ' + Copy(Line, j+1, length(Line)) + ' ' + IntToStr(FirstTrack.I_ObjectAddress) + ' ' + IntToStr(SecondTrack.I_ObjectAddress));

                        if (ObjAdr = FirstTrack.I_ObjectAddress) then
                        begin
                            if flag = 0 then    Radius := Leng
                            //if flag = 0 then           Radius := Radius
                            else if Radius > Leng then Radius := Leng;
                            //else if Radius > Leng then Radius := Radius;
                            flag := 1;
                        end;

                        if (ObjAdr = SecondTrack.I_ObjectAddress) then
                        begin
                            if flag = 0 then    Radius := Leng
                            //if flag = 0 then           Radius := Radius
                            else if Radius > Leng then Radius := Leng;
                            //else if Radius > Leng then Radius := Radius;
                            flag := 1;
                        end;
                        Inc(i);
                    end;
                    // hard-coded hack to create fixed radius fillet
                    //ShowMessage('Radius: ' + IntToStr(Radius));

                    // Calculate arc angles
                    // now we need to see what is first angle and what is second angle of an arc
                    if ((Angle1 > Angle2) and (Angle1 - Angle2 < Pi)) then
                    begin
                        StartAngle := Pi / 2 + Angle1;
                        StopAngle  := 3 * PI / 2 + Angle2;
                    end
                    else if ((Angle1 > Angle2) and (Angle1 - Angle2 > Pi)) then
                    begin
                        StartAngle := Pi / 2 + Angle2;
                        StopAngle  := Angle1 - Pi / 2;
                    end
                    else if ((Angle1 < Angle2) and (Angle2 - Angle1 < Pi)) then
                    begin
                        StartAngle := Pi / 2 + Angle2;
                        StopAngle  := 3 * PI / 2 + Angle1;
                    end
                    else if ((Angle1 < Angle2) and (Angle2 - Angle1 > Pi)) then
                    begin
                        StartAngle := Pi / 2 + Angle1;
                        StopAngle  := Angle2 - Pi / 2;
                    end;

                    // need to figure out the total angle of the fillet, then use the half-angle to figure out how much to shorten tracks
                    ArcAngle := StopAngle - StartAngle;
                    HalfAngle := ArcAngle / 2;
                    if (HalfAngle = pi / 2) or (HalfAngle = (3 * pi / 2)) then    // is right angle, so shorten by 1 radius (also, tan() will fail)
                        ShortLength := Radius
                    else if ratio > 0 then
                    begin
                        ShortLength := Radius;
                        Radius      := ShortLength / tan(HalfAngle);
                    end
                    else ShortLength := Radius * tan(HalfAngle);
                    //ShowMessage('ArcAngle: ' + FloatToStr(ArcAngle));

                    // modify point of FirstTrack
                    FirstTrack.BeginModify;
                    if FirstCommon = 1 then
                    begin
                        //FirstTrack.x1 := FirstTrack.x1 + Radius*cos(Angle1);
                        //FirstTrack.y1 := FirstTrack.y1 + Radius*sin(Angle1);
                        FirstTrack.x1 := FirstTrack.x1 + ShortLength * cos(Angle1);
                        FirstTrack.y1 := FirstTrack.y1 + ShortLength * sin(Angle1);
                        X1            := FirstTrack.x1;
                        Y1            := FirstTrack.y1;
                    end
                    else begin
                        FirstTrack.x2 := FirstTrack.x2 + ShortLength * cos(Angle1);
                        FirstTrack.y2 := FirstTrack.y2 + ShortLength * sin(Angle1);
                        X1            := FirstTrack.x2;
                        Y1            := FirstTrack.y2;
                    end;
                    FirstTrack.EndModify;

                    if Angle1 < 0 then Angle1 := pi + Angle1;

                    // modify point of SecondTrack
                    SecondTrack.BeginModify;
                    if SecondCommon = 1 then
                    begin
                        SecondTrack.x1 := SecondTrack.x1 + ShortLength * cos(Angle2);
                        SecondTrack.y1 := SecondTrack.y1 + ShortLength * sin(Angle2);
                        X2             := SecondTrack.x1;
                        Y2             := SecondTrack.y1;
                    end
                    else begin
                        SecondTrack.x2 := SecondTrack.x2 + ShortLength * cos(Angle2);
                        SecondTrack.y2 := SecondTrack.y2 + ShortLength * sin(Angle2);
                        X2             := SecondTrack.x2;
                        Y2             := SecondTrack.y2;
                    end;
                    SecondTrack.EndModify;

                    // Calculate X center of arc
                    if ((Angle1 = 0) or (Angle1 = pi)) then Xc := X1
                    else if ((Angle2 = 0) or (Angle2 = pi)) then Xc := X2
                    else begin
                        k1 := tan(pi / 2 + Angle1);
                        k2 := tan(pi / 2 + Angle2);

                        Xc := (Y2 - Y1 + k1 * X1 - k2 * X2) / (k1 - k2);
                    end;

                    // Calculate Y center of
                    if ((Angle1 = Pi / 2) or (Angle1 = pi * 3 / 2)) then
                        Yc := Y1
                    else if ((Angle2 = Pi / 2) or (Angle2 = pi * 3 / 2)) then
                        Yc := Y2
                    else if ((Angle1 <> 0) and (Angle1 <> pi)) then Yc := tan(pi / 2 + Angle1) * (Xc - X1) + Y1
                    else if ((Angle2 <> 0) and (Angle2 <> pi)) then Yc := tan(pi / 2 + Angle2) * (Xc - X2) + Y2;


                    // Count radius - I have no idea why


                    Arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
                    Arc.XCenter := Int(Xc);
                    Arc.YCenter := Int(Yc);
                    Arc.Radius := sqrt(sqr(X1 - Xc) + sqr(Y1 - Yc));;
                    Arc.LineWidth := FirstTrack.Width;
                    Arc.StartAngle := StartAngle * 180 / pi;
                    Arc.EndAngle := StopAngle * 180 / pi;
                    Arc.Layer := FirstTrack.Layer;
                    if FirstTrack.InNet then Arc.Net := FirstTrack.Net;
                    Board.AddPCBObject(Arc);
                    Arc.Selected := False;

                    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, FirstTrack.I_ObjectAddress);
                    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, SecondTrack.I_ObjectAddress);
                    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Arc.I_ObjectAddress);

                end;
            end;
        end;
    end;

    // RDR: This section's undo doesn't work properly, the removed track never comes back, so below I'm capping the ratios to 0.01 and 99.99
    if Ratio = 100 then
    begin
        // Start undo
        //PCBServer.PreProcess;
        for i := 0 to Board.SelectecObjectCount - 1 do
        begin
            FirstTrack := Board.SelectecObject[i];
            if ((FirstTrack.x1 = FirstTrack.x2) and (FirstTrack.y1 = FirstTrack.y2)) then RadiusList.AddObject(FirstTrack.I_ObjectAddress, FirstTrack);
        end;

        for i := 0 to Radiuslist.Count - 1 do
        begin
            Board.RemovePCBObject(RadiusList.GetObject(i));
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, RadiusList.GetObject(i));
        end;
        // Stop undo
        //PCBServer.PostProcess;
    end;

    RadiusList.Clear;
    
    // Display a message if no fillets have been added by script, presumably because there is an issue with track joints.
    // If even one fillet is added, don't show message. The intent is to be informative if the user runs the script and it does nothing.
    if NumCommon = 0 then ShowMessage('Selected tracks have no common point (track ends must meet EXACTLY)');

    ResetParameters;
    AddStringParameter('Scope', 'All');
    RunProcess('PCB:DeSelect');

    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');

    // Stop undo
    PCBServer.PostProcess;
    close;
end;

procedure Start;
var
    Track     : IPCB_Primitive;
    Leng      : Integer;
    Flag      : Integer;
    i         : Integer;
    Distance  : Double;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    flag       := 0;
    RadiusList := TStringList.Create;

    // Deselect any non-track objects
    i := 0;
    While i < Board.SelectecObjectCount do
    begin
        Track := Board.SelectecObject[i];
        if Track.ObjectId <> eTrackObject then Track.SetState_Selected(false)
        else i := i + 1;	// only iterate if object wasn't deselected
    end;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Track := Board.SelectecObject[i];
       (*
       if Track.ObjectID <> eTrackObject then
       begin
          showMessage('Select only Tracks');
          exit;
       end;  *)

        Leng := Int(sqrt(sqr(Track.x2 - Track.x1) + sqr(Track.y2 - Track.y1)) * 1);
        RadiusList.Add(IntToStr(Track.I_ObjectAddress) + ';' + IntToStr(Leng));

        flag := 1;
    end;

    If flag = 0 then
    begin
        showMessage('No Selected Tracks');
        exit;
    end;

    MinDistance := -1;
    for i := 1 to Board.SelectecObjectCount - 1 do
        if Track.ObjectID <> eTrackObject then
        begin
            Distance := sqrt(sqr(Board.SelectecObject[i].x1 - Board.SelectecObject[i].x2) + sqr(Board.SelectecObject[i].y1 - Board.SelectecObject[i].y2));

            if (Distance < Mindistance) or (MinDistance = -1) then Mindistance := Distance;
        end;

    Mindistance := MinDistance / 2;

    Form1.ShowModal;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
    close;
end;

procedure TForm1.tRadiusChange(Sender: TObject);
begin
    if IsStringANum(tRadius.Text) then
    begin
        tRadius.Font.Color := clWindowText;
        ButtonOK.Enabled := True;
    end
    else begin
        ButtonOK.Enabled := False;
        tRadius.Font.Color := clRed;
    end;
    if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) >= 100) then 
    begin
        tRadius.Text := '99.99';
        ShowInfo('% input limited to 99.99%');
    end
    else if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) <= 0) then 
    begin
        tRadius.Text := '0.01';
        ShowInfo('% input limited to 0.01%');
    end;
end;

procedure TForm1.RadioUnitsRatioClick(Sender: TObject);
begin
    if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) >= 100) then tRadius.Text := '99.99'
    else if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) <= 0) then tRadius.Text := '0.01';
end;

