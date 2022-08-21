{..............................................................................}
{ Summary   This Script creates fillet on selected tracks                      }
{                                                                              }
{           Radius value = (shorter of two lines) * percentage OR fixed value  }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{ Edited by:    Ryan Rutledge, Brett Miller                                    }
{           Edited 2018-09-15 to support any-angle tracks and fixed radius     }
{           Edited 2022-08-01 to add configurable presets                      }
{           Edit : 2022-08-06  cleanup TStringList object methods etc.        }
{..............................................................................}

{..............................................................................}
uses
    IniFiles;
var
    Board         : IPCB_Board;
    MinDistance   : Double;

    (* I need string list to memorize radius of each track.
    Since arc is added betwen two tracks, I will need to find lesser value of the two

    Data will be stored in a TStringList as (string, object) tuples
    (radius as string, Prim_Object)  *)

    RadiusList     : TStringList;
    PresetFilePath : String;
    PresetList     : TStringList;

function IsStringANum(Text : String) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;
    // Test for number, dot or comma
    ChSet := SetUnion(MkSet(Ord('.'),Ord(',')), MkSetRange(Ord('0'), Ord('9')) );
    for i := 1 to Length(Text) do
       if not InSet(Ord(Text[i]), ChSet) then Result := false;

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet := MkSet(Ord('.'),Ord(','));
    for i := 1 to Length(Text) do
       if InSet(Ord(Text[i]), ChSet) then
          Inc(dotCount);

    if dotCount > 1 then Result := False;
end;

procedure DoFillets(const dummy : integer);
var
    Prim                : IPCB_Primitive;
    FirstTrack          :IPCB_Primitive;
    SecondTrack         :IPCB_Primitive;
    Arc                 :IPCB_Arc;
    XCommon             :Integer;
    YCommon             :Integer;
    angle1              :Double;
    angle2              :Double;
    Radius              :Integer;
    k1                  :Double;
    k2                  :Double;
    FirstCommon         :Integer;
    SecondCommon        :Integer;
    Xc                  :Integer;
    Yc                  :Integer;
    i, j                :integer;
    flag                :integer;
    X1, X2, Y1, Y2      :Integer;
    StartAngle          :Double;
    StopAngle           :Double;
    ArcAngle            :Double;
    HalfAngle           :Double;
    TempString          :String;
    Leng                :Integer;
    ShortLength         :Integer;
    NumCommon           :Integer;
    Ratio               :Double;
    a, b                :Integer;
    TempPresetList      :TStringList;

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

    For a := 0 to (Board.SelectecObjectCount - 1) do
    begin
        FirstTrack := Board.SelectecObject(a);
        for b := 0 to Board.SelectecObjectCount - 1 do
        begin
            SecondTrack := Board.SelectecObject(b);
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

                // check whether will be placing any arcs
                if not ((Angle1 = Angle2) or
                    ((Angle1 > Angle2) and (Angle1 - pi = Angle2)) or
                    ((Angle1 < Angle2) and (Angle2 - pi = Angle1))) then
                begin

                    i    := 0;
                    flag := 0;

                    while i < RadiusList.Count do
                    begin
                        Prim := RadiusList.GetObject(i);
                        Leng := StrToInt(RadiusList.Strings(i));

                        if Ratio > 0 then
                        begin
                            Leng := Int(Leng * Ratio / 100)
                        end
                        else Leng := Radius;

                        // ShowMessage( IntToStr(Prim.I_ObjectAddress) + ' ' + RadiusList.Strings(i) + ' ' + IntToStr(FirstTrack.I_ObjectAddress) + ' ' + IntToStr(SecondTrack.I_ObjectAddress));

                        if (Prim = FirstTrack) then
                        begin
                            if flag = 0 then    Radius := Leng
                            //if flag = 0 then           Radius := Radius
                            else if Radius > Leng then Radius := Leng;
                            //else if Radius > Leng then Radius := Radius;
                            flag := 1;
                        end;

                        if (Prim = SecondTrack) then
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
                    FirstTrack.GraphicallyInvalidate;

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
                    SecondTrack.GraphicallyInvalidate;

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
                    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Arc.I_ObjectAddress);
                    Arc.Selected := False;

                end;
            end;
        end;
    end;

    // RDR: This section's undo doesn't work properly, the removed track never comes back, so below I'm capping the ratios to 0.01 and 99.99
    if Ratio = 100 then
    begin
        for i := 0 to (Board.SelectecObjectCount - 1) do
        begin
            FirstTrack := Board.SelectecObject(i);
            if ((FirstTrack.x1 = FirstTrack.x2) and (FirstTrack.y1 = FirstTrack.y2)) then RadiusList.AddObject(IntToStr(i), FirstTrack);
        end;

        for i := 0 to (Radiuslist.Count - 1) do
        begin
            FirstTrack := RadiusList.GetObject(i);
            Board.RemovePCBObject(FirstTrack);
        end;
    end;

    RadiusList.Clear;

    // Stop undo
    PCBServer.PostProcess;

    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    // build list of currect preset values
    TempPresetList := TStringList.Create;
    TempPresetList.Add(tPreset1.Text);
    TempPresetList.Add(tPreset2.Text);
    TempPresetList.Add(tPreset3.Text);
    TempPresetList.Add(tPreset4.Text);
    TempPresetList.Add(tPreset5.Text);
    TempPresetList.Add(tPreset6.Text);
    TempPresetList.Add(tPreset7.Text);
    TempPresetList.Add(tPreset8.Text);
    TempPresetList.Add(tRadius.Text);
    TempPresetList.Add(RadioUnitsMils.Checked);
    TempPresetList.Add(RadioUnitsMM.Checked);
    TempPresetList.Add(RadioUnitsRatio.Checked);

    If TempPresetList.Equals(PresetList) then
    Begin
        //presets match saved list so do nothing
    End
    Else Begin
        // save new list to MyPresetList.txt
        TempPresetList.SaveToFile(PresetFilePath);
    End;

    // cleanup
    PresetList.Free;
    TempPresetList.Free;

    // Display a message if no fillets have been added by script, presumably because there is an issue with track joints.
    // If even one fillet is added, don't show message. The intent is to be informative if the user runs the script and it does nothing.
    if NumCommon = 0 then ShowMessage('Selected tracks have no common point (track ends must meet EXACTLY)');

    close;
end;

procedure Start;
var
    Prim      : IPCB_Primitive;
    Leng      : Integer;
    Flag      : Integer;
    i         : Integer;
    Distance  : Double;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    flag       := 0;
    RadiusList := TStringList.Create;
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyFilletPresets.txt';
    
//  Deselect any non-track objects in a self modifying "collection"
    i := 0;
    While i < Board.SelectecObjectCount do
    begin
        Prim := Board.SelectecObject(i);
        if Prim.ObjectId <> eTrackObject then
            Prim.SetState_Selected(false)
        else
            i := i + 1;    // only iterate if object wasn't deselected
    end;

    for i := 0 to (Board.SelectecObjectCount - 1) do
    begin
        Prim := Board.SelectecObject(i);
       (*
       if Track.ObjectID <> eTrackObject then
       begin
          showMessage('Select only Tracks');
          exit;
       end;  *)
        if Prim.ObjectId = eTrackObject then
        begin
            Leng := Power((Prim.x2 - Prim.x1),2) + Power((Prim.y2 - Prim.y1),2);
            Leng := Int(Sqrt(Leng));
            RadiusList.AddObject(IntToStr(Leng), Prim);
            flag := 1;
        end;
    end;

    If flag = 0 then
    begin
        showMessage('No Selected Tracks');
        exit;
    end;

    MinDistance := -1;
    for i := 1 to (Board.SelectecObjectCount - 1) do
    begin
        Prim := Board.SelectecObject(i);
        if Prim.ObjectID <> eTrackObject then
        begin
//    this can never get run ??
            Distance := sqrt(sqr(Prim.x1 - Prim.x2) + sqr(Prim.y1 - Prim.y2));

            if (Distance < Mindistance) or (MinDistance = -1) then Mindistance := Distance;
        end;
    end;

    Mindistance := MinDistance / 2;

    Form1.ShowModal;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
    close;
end;

procedure TForm1.RadioUnitsRatioClick(Sender: TObject);
begin
    if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) >= 100) then tRadius.Text := '99.99'
    else if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) <= 0) then tRadius.Text := '0.01';
end;

procedure TForm1.Form1Show(Sender: TObject);
var
    I : Integer;
begin
    // read from MyFilletPresets.txt
    PresetList := TStringList.Create;
    If FileExists(PresetFilePath) then
    Begin
        PresetList.LoadFromFile(PresetFilePath);    // load presets from file if it exists

        // if PresetList file exists but count is short, just regenerate preset file from defaults
        If PresetList.Count < 12 then
        Begin
            //ShowMessage(PresetFilePath + ' exists but is not the correct length. Defaults will be used.');
            PresetList.Clear;
            PresetList.Add(tPreset1.Text);              //PresetList[0]
            PresetList.Add(tPreset2.Text);              //PresetList[1]
            PresetList.Add(tPreset3.Text);              //PresetList[2]
            PresetList.Add(tPreset4.Text);              //PresetList[3]
            PresetList.Add(tPreset5.Text);              //PresetList[4]
            PresetList.Add(tPreset6.Text);              //PresetList[5]
            PresetList.Add(tPreset7.Text);              //PresetList[6]
            PresetList.Add(tPreset8.Text);              //PresetList[7]
            PresetList.Add(tRadius.Text);               //PresetList[8]
            PresetList.Add(RadioUnitsMils.Checked);     //PresetList[9]
            PresetList.Add(RadioUnitsMM.Checked);       //PresetList[10]
            PresetList.Add(RadioUnitsRatio.Checked);    //PresetList[11]
            PresetList.SaveToFile(PresetFilePath);
        End;

        //set text boxes to match preset list (redundant if list was regenerated above
        //ShowMessage('Loading presets from ' + PresetFilePath);
        tPreset1.Text := PresetList[0];
        tPreset2.Text := PresetList[1];
        tPreset3.Text := PresetList[2];
        tPreset4.Text := PresetList[3];
        tPreset5.Text := PresetList[4];
        tPreset6.Text := PresetList[5];
        tPreset7.Text := PresetList[6];
        tPreset8.Text := PresetList[7];
        tRadius.Text := PresetList[8];
        if (PresetList[9] = 'True') then RadioUnitsMils.Checked := true
        else if (PresetList[10] = 'True') then RadioUnitsMM.Checked := true
        else RadioUnitsRatio.Checked := true;
    End
    Else Begin      //if preset file didn't exist at all, create from defaults
        //ShowMessage(PresetFilePath + ' does not exist.');
        PresetList.Clear;
        PresetList.Add(tPreset1.Text);              //PresetList[0]
        PresetList.Add(tPreset2.Text);              //PresetList[1]
        PresetList.Add(tPreset3.Text);              //PresetList[2]
        PresetList.Add(tPreset4.Text);              //PresetList[3]
        PresetList.Add(tPreset5.Text);              //PresetList[4]
        PresetList.Add(tPreset6.Text);              //PresetList[5]
        PresetList.Add(tPreset7.Text);              //PresetList[6]
        PresetList.Add(tPreset8.Text);              //PresetList[7]
        PresetList.Add(tRadius.Text);               //PresetList[8]
        PresetList.Add(RadioUnitsMils.Checked);     //PresetList[9]
        PresetList.Add(RadioUnitsMM.Checked);       //PresetList[10]
        PresetList.Add(RadioUnitsRatio.Checked);    //PresetList[11]
        PresetList.SaveToFile(PresetFilePath);
    end;
end;


procedure ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    //ShowMessage(textbox.Text);
    if IsStringANum(textbox.Text) then
    begin
        If Sender <> tRadius then tRadius.Text := textbox.Text;
        ButtonOK.Enabled := True;
    end
    else ButtonOK.Enabled := False;

    if (RadioUnitsRatio.Checked = true) and (StrToFloat(textbox.Text) >= 100) then
    begin
        textbox.Text := '99.99';
        ShowInfo('% input limited to 99.99% to avoid zero-length stubs');
    end
    else if (RadioUnitsRatio.Checked = true) and (StrToFloat(textbox.Text) <= 0) then
    begin
        textbox.Text := '0.01';
        ShowInfo('% input limited to 0.01% to avoid zero-length stubs');
    end;
end;

procedure UserKeyPress(Sender: TObject; var Key: Char);     //programmatically, OnKeyPress fires before OnChange event and "catches" the key press
begin
    if (ButtonOK.Enabled) And (Ord(Key) = 13) then
    begin
        Key := #0; //catch and discard key press to avoid beep
        DoFillets(0);
    end;
end;

procedure TForm1.ButtonOKClick(Sender: TObject);
begin
    DoFillets(1);
end;

procedure PresetButtonClicked(Sender: TObject);
begin
    If Sender = Button1 then tRadius.Text := tPreset1.Text
    Else If Sender = Button2 then tRadius.Text := tPreset2.Text
    Else If Sender = Button3 then tRadius.Text := tPreset3.Text
    Else If Sender = Button4 then tRadius.Text := tPreset4.Text
    Else If Sender = Button5 then tRadius.Text := tPreset5.Text
    Else If Sender = Button6 then tRadius.Text := tPreset6.Text
    Else If Sender = Button7 then tRadius.Text := tPreset7.Text
    Else If Sender = Button8 then tRadius.Text := tPreset8.Text;
    DoFillets(2);
end;
