const
    cScriptName = 'RotateSymbol';
    cScriptVersion = '1.10';
    sLineBreak2 = sLineBreak + sLineBreak;

var
    CurrentSch : ISch_Sheet;
    ComponentList : TInterfaceList;
    ParameterList : TInterfaceList;


procedure GlobalInit(dummy : Integer = 0);
begin
    if (SchServer = nil) then exit;

    // Obtain the current schematic document interface.
    CurrentSch := SchServer.GetCurrentSchDocument;
    if CurrentSch = nil then exit;

    ComponentList := CreateObject(TInterfaceList);
    ParameterList := CreateObject(TInterfaceList);
end;

procedure FillComponentList(CurrentSch : ISch_Sheet);
var
    CompIter    : ISch_Iterator;
    Comp        : ISch_Component;
begin
    ComponentList.Clear;
    CompIter := CurrentSch.SchIterator_Create;
    CompIter.AddFilter_ObjectSet(MkSet(eSchComponent));
    try
        Comp := CompIter.FirstSchObject;
        while Comp <> nil do
        begin
            if Comp.Selection then ComponentList.Add(Comp);
            Comp := CompIter.NextSchObject;
        end;
    finally
        CurrentSch.SchIterator_Destroy(CompIter);
    end;
end;

procedure FillParameterList(Comp : ISch_Component);
var
    ParamIter   : ISch_Iterator;
    Param       : ISch_Parameter;
    DebugStr    : String;
begin
    DebugStr := '';
    ParameterList.Clear;

    if not Comp.Designator.GetState_IsHidden then
    begin
        ParameterList.Add(Comp.Designator);
        //DebugStr := Comp.Designator.Name;
    end;

    ParamIter := Comp.SchIterator_Create;
    try
        ParamIter.AddFilter_ObjectSet(MkSet(eParameter));
        Param := ParamIter.FirstSchObject;
        while Param <> nil do
        begin
            //ReportList.Add('  Name: ' + Pin.Name + ' Designator: ' + Pin.Designator + ' Orientation: ' +  OrientationToStr(Pin.Designator));
            if not Param.GetState_IsHidden then
            begin
                ParameterList.Add(Param);
                //if DebugStr <> '' then DebugStr := DebugStr + sLineBreak;
                //DebugStr := DebugStr + Param.Name;
            end;
            Param := ParamIter.NextSchObject;
        end;
    finally
        Comp.SchIterator_Destroy(ParamIter);
    end;
    //ShowInfo('Debugging - parameter names:' + sLineBreak + '---------------------' + sLineBreak+ DebugStr);
end;

procedure ReorientSymbol(Clockwise : Boolean = False);
var
    CompIdx         : Integer;
    Comp            : ISch_Component;
    InitOrientation : TRotationBy90;
    WarnText        : String;
begin
    WarnText := 'WARNING!!! THIS OVERRIDES SCHEMATIC SYMBOL''S INTERNAL ORIENTATION! Only run this for symbols that do not match library symbol''s orientation.' + sLineBreak2 +
                'TRUE ORIENTATION OF SYMBOL WILL BE DIFFERENT FROM LIBRARY ZERO ORIENTATION WITH NO VISIBLE INDICATOR THIS IS THE CASE' + sLineBreak2 +
                'ONLY USE THIS IF YOU KNOW WHAT YOU ARE DOING! USE FULL COMPONENT PLACEMENT TO RESET' + sLineBreak2 +
                'Press OK to continue';

    if not ConfirmOKCancel(WarnText) then exit;

    GlobalInit;

    FillComponentList(CurrentSch);

    SchServer.ProcessControl.PreProcess(CurrentSch, '');

    for CompIdx := 0 to ComponentList.Count - 1 do
    begin
        Comp := ComponentList[CompIdx];

        InitOrientation := Comp.GetState_Orientation;
        Comp.UpdatePart_PreProcess;

        case InitOrientation of
            eRotate0    : begin
                if Clockwise then Comp.SetState_OrientationWithoutRotating(eRotate270) else Comp.SetState_OrientationWithoutRotating(eRotate90);
            end;
            eRotate90   : begin
                if Clockwise then Comp.SetState_OrientationWithoutRotating(eRotate0) else Comp.SetState_OrientationWithoutRotating(eRotate180);
            end;
            eRotate180  : begin
                if Clockwise then Comp.SetState_OrientationWithoutRotating(eRotate90) else Comp.SetState_OrientationWithoutRotating(eRotate270);
            end;
            eRotate270  : begin
                if Clockwise then Comp.SetState_OrientationWithoutRotating(eRotate180) else Comp.SetState_OrientationWithoutRotating(eRotate0);
            end;
        end;

        Comp.UpdatePart_PostProcess;
        Comp.GraphicallyInvalidate;
    end;

    Client.SendMessage('Sch:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    SchServer.ProcessControl.PostProcess(CurrentSch, '');

    ComponentList.Clear;
end;

procedure ReorientCCW;
begin
    ReorientSymbol(False);
end;

procedure ReorientCW;
begin
    ReorientSymbol(True);
end;

function GetPinHotspot(Pin : ISch_Pin) : TPoint;
begin
    if Pin = nil then exit;

    case Pin.Orientation of
        eRotate0    : begin
            Result := Point(Pin.Location.x + Pin.PinLength, Pin.Location.y);
        end;
        eRotate90   : begin
            Result := Point(Pin.Location.x, Pin.Location.y + Pin.PinLength);
        end;
        eRotate180  : begin
            Result := Point(Pin.Location.x - Pin.PinLength, Pin.Location.y);
        end;
        eRotate270  : begin
            Result := Point(Pin.Location.x, Pin.Location.y - Pin.PinLength);
        end;
    end;
end;

function HasPinCount(Comp : ISch_Component; PinCount : Integer) : Boolean;
var
    PinIter     : ISch_Iterator;
    Pin         : ISch_Pin;
begin
    Result := False;
    if Comp = nil then exit;

    PinIter := Comp.SchIterator_Create;
    try
        PinIter.AddFilter_ObjectSet(MkSet(ePin));

        Pin := PinIter.FirstSchObject;
        while (Pin <> nil) and (PinCount >= 0) do
        begin
            Dec(PinCount);
            if PinCount < 0 then break; // early exit condition
            Pin := PinIter.NextSchObject;
        end;
    finally
        Comp.SchIterator_Destroy(PinIter);
    end;

    Result := PinCount = 0; // true if iterated pin count matches starting PinCount
end;

function GetCompOrigin(Comp : ISch_Component) : TPoint;
var
    PinIter     : ISch_Iterator;
    Pin         : ISch_Pin;
    Hotspot1    : TPoint;
    Hotspot2    : TPoint;
begin
    Result := nil;
    if Comp = nil then exit;
    // if component only has 2 pins, use the midpoint as the centroid, otherwise just use part "Location" i.e. lib origin
    if HasPinCount(Comp, 2) then
    begin
        PinIter := Comp.SchIterator_Create;
        try
            PinIter.AddFilter_ObjectSet(MkSet(ePin));

            Pin := PinIter.FirstSchObject;
            Hotspot1 := GetPinHotspot(Pin);

            Pin := PinIter.NextSchObject;
            Hotspot2 := GetPinHotspot(Pin);
        finally
            Comp.SchIterator_Destroy(PinIter);
        end;

        Result := Point((Hotspot1.x + Hotspot2.x) div 2, (Hotspot1.y + Hotspot2.y) div 2);
    end
    else Result := Comp.Location;
end;

procedure RotateInPlace(Clockwise : Boolean = False);
var
    CompIdx         : Integer;
    ParamIdx        : Integer;
    Comp            : ISch_Component;
    Param           : ISch_Parameter;
    CompOrigin      : TPoint;
begin
    GlobalInit;

    FillComponentList(CurrentSch);

    SchServer.ProcessControl.PreProcess(CurrentSch, '');

    for CompIdx := 0 to ComponentList.Count - 1 do
    begin
        Comp := ComponentList[CompIdx];

        FillParameterList(Comp);

        //CompOrigin := Comp.Location;
        CompOrigin := GetCompOrigin(Comp);

        Comp.UpdatePart_PreProcess;
        try
            // set all visible parameters to Manual position so they rotate around the component
            for ParamIdx := 0 to ParameterList.Count - 1 do
            begin
                Param := ParameterList[ParamIdx];
                Param.Autoposition := False;
            end;

            // rotate part in place by 90°
            if Clockwise then Comp.RotateBy90(CompOrigin, eRotate270) else Comp.RotateBy90(CompOrigin, eRotate90);

            // rotate all the parameters 90° the other way using part as origin
            for ParamIdx := 0 to ParameterList.Count - 1 do
            begin
                Param := ParameterList[ParamIdx];
                if Clockwise then Param.RotateBy90(CompOrigin, eRotate90) else Param.RotateBy90(CompOrigin, eRotate270);
            end;

        finally
            Comp.UpdatePart_PostProcess;
            Comp.GraphicallyInvalidate;
        end;

    end;

    Client.SendMessage('Sch:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    SchServer.ProcessControl.PostProcess(CurrentSch, '');

    ParameterList.Clear;
    ComponentList.Clear;
end;

procedure _RotateCCW;
begin
    RotateInPlace(False);
end;

procedure _RotateCW;
begin
    RotateInPlace(True);
end;

procedure _Mirror;
var
    CompIdx         : Integer;
    ParamIdx        : Integer;
    Comp            : ISch_Component;
    Param           : ISch_Parameter;
    CompOrigin      : TPoint;
begin
    GlobalInit;

    FillComponentList(CurrentSch);

    SchServer.ProcessControl.PreProcess(CurrentSch, '');

    for CompIdx := 0 to ComponentList.Count - 1 do
    begin
        Comp := ComponentList[CompIdx];

        FillParameterList(Comp);

        //CompOrigin := Comp.Location;
        CompOrigin := GetCompOrigin(Comp);

        Comp.UpdatePart_PreProcess;
        try
            // set all visible parameters to Manual position so they rotate around the component
            for ParamIdx := 0 to ParameterList.Count - 1 do
            begin
                Param := ParameterList[ParamIdx];
                Param.Autoposition := False;
            end;

            // mirror component
            Comp.Mirror(CompOrigin);

            // mirror parameters back to original position
            for ParamIdx := 0 to ParameterList.Count - 1 do
            begin
                Param := ParameterList[ParamIdx];
                Param.Mirror(CompOrigin);
            end;

        finally
            Comp.UpdatePart_PostProcess;
            Comp.GraphicallyInvalidate;
        end;

    end;

    Client.SendMessage('Sch:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    SchServer.ProcessControl.PostProcess(CurrentSch, '');

    ParameterList.Clear;
    ComponentList.Clear;
end;
