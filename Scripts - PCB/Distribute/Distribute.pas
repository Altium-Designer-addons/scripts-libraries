{ Created by:  Matija Markovic, Petar Perisin }
{ Edited by:   Ryan Rutledge }
{ For documentation see README.md }

var
    Board             : IPCB_Board;
    SortedPads        : TStringList;
    SortedTracks      : TStringList;
    SortedVias        : TStringList;
    TrimPerpendicular : Boolean; // test feature that trims dangling track ends
    DebuggingEnabled  : Boolean; // debug feature will log before-and-after results of tracks modified by script
    DebugFilePath     : string;
    DebugList         : TStringList;
    PadDebugFilePath  : string;
    PadDebugList      : TStringList;
    PadModeEnabled    : Boolean;
    ViaDebugFilePath  : string;
    ViaDebugList      : TStringList;
    ViaModeEnabled    : Boolean;

const
    ScriptVersion = '1.60';
    ScriptTitle = 'Distribute';
    cConfigFileName = 'MyDistributeConfig.ini';
    sLineBreak2 = sLineBreak + sLineBreak;


procedure _Start; forward;
procedure _StartWithDebug; forward;
procedure About; forward;
procedure AddToDebugListAfter(var Prim : IPCB_Track; LastIntercept : TCoord); forward;
procedure AddToDebugListBefore(var Prim : IPCB_Track; TargetSlope : Double; TargetIntercept : TCoord); forward;
procedure AddToDebugListFirstVia(var Prim1 : IPCB_Via; var Prim2 : IPCB_Track); forward;
procedure AddToDebugListSecondVia(var Prim1 : IPCB_Via; var Prim2 : IPCB_Track; const viaminc, viamaxc, midc : TCoord); forward;
procedure calculate(LaunchedFromGUI : Boolean); forward;
function CompileSortedPads(const dummy : Integer = 0) : Boolean; forward;
procedure CompileSortedTracks(const dummy : Integer = 0); forward;
function CompileSortedVias(const dummy : Integer = 0) : Boolean; forward;
function ConfigFile_GetPath(dummy : String = ''): String; forward;
procedure ConfigFile_Read(AFileName : String); forward;
procedure ConfigFile_Write(AFileName : String); forward;
procedure ConfigFile_Update(ConfigFile : TIniFile); forward;
function DebugContourInfo(contour : IPCB_Contour) : TStringList; forward;
function DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList; forward;
function DistributeBackward(startc, stepc : TCoord; coef : Double); forward;
function DistributeForward(startc, stepc : TCoord; coef : Double); forward;
function DistributeFromCenter(startc, stepc : TCoord; coef : Double); forward;
procedure EnableBetweenPadViaControls(NewEnable : Boolean); forward;
procedure EnableByValControls(NewEnable : Boolean); forward;
procedure FastDistributeByCenterline; forward;
procedure FastDistributeByClearance; forward;
function GetAnotherTrackInPoint(Prim1 : IPCB_Track; X, Y : TCoord; out OnFirstPoint : Boolean) : IPCB_Primitive; forward;
function GetEdgeIntercept(const ThisTrackIndex : Integer; const coef : Double; out LastIntercept : TCoord; const Reverse : Boolean); forward;
function GetIntersection(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; k2 : Double; c2 : TCoord; IsPrim2Vert : Boolean; out X, Y : TCoord) : Boolean; forward;
function GetPadPoly(Obj: IPCB_Pad; Expansion: TCoord = 0; ALayer : TV7_Layer = eNoLayer) : IPCB_GeometricPolygon; forward;
function GetParallelLine(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; out k2 : Double; out c2 : TCoord; out IsPrim2Vert : Boolean; X, Y : TCoord) : Boolean; forward;
function GetPerpendicularLine(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; out k2 : Double; out c2 : TCoord; out IsPrim2Vert : Boolean; X, Y : TCoord) : Boolean; forward;
function GetPresetButtonEnable(const dummy : Integer) : Boolean; forward;
function InitialCheck(var status : Integer) : Integer; forward;
function IsStringANum(Text : string) : Boolean; forward;
procedure LoadPresetListFromFile(const dummy : Integer); forward;
function MoveTrackToIntercept(ThisTrackIndex, ConnectedTrackOneIndex, ConnectedTrackTwoIndex, TrimTrackIndex : Integer; coef, TargetSlope : Double; TargetIntercept, out LastIntercept : TCoord; Reverse : Boolean); forward;
procedure PadAndSort(var list : TStringList); forward;
function PointToPointDistance(X1, Y1, X2, Y2 : TCoord) : Double; forward;
procedure PresetButtonClicked(Sender : TObject); forward;
procedure SelectListItems(const list : TStringList); forward;
function SetupDataFromPad(var PrimPad : IPCB_Pad; ALayer : TV7_Layer; k_midpoint : Double; c_midpoint : TCoord; IsMidVert : Boolean; out k : Double; out c : TCoord; out IsIntVert : Boolean; out X, Y : TCoord) : Boolean; forward;
procedure SetupDataFromTrack(var Prim1 : IPCB_Track; out IsVertical : Boolean; out k : Double; out c, X1, Y1, X2, Y2 : TCoord); forward;
procedure SetupDataFromVia(var PrimVia : IPCB_Via; var PrimTrack : IPCB_Track; out IsIntVert : Boolean; out k : Double; out c, X, Y, size : TCoord); forward;
procedure Start; forward;
procedure StartWithDebug; forward;
procedure TFormDistribute.ButtonCancelClick(Sender : TObject); forward;
procedure TFormDistribute.ButtonOKClick(Sender : TObject); forward;
procedure TFormDistribute.ButtonUnitsClick(Sender : TObject); forward;
procedure TFormDistribute.ButtonPadViaUnitsClick(Sender : TObject); forward;
procedure TFormDistribute.CheckBoxTrimEndsClick(Sender : TObject); forward;
procedure TFormDistribute.CheckBoxPadViaClearanceClick(Sender : TObject); forward;
procedure TFormDistribute.EditDistanceChange(Sender : TObject); forward;
procedure TFormDistribute.EditPadViaClearanceChange(Sender : TObject); forward;
procedure TFormDistribute.FormDistributeShow(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonCentersClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonCentersValClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonClearanceClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonClearanceValClick(Sender : TObject); forward;
procedure TFormDistribute.RadioDirectionsClick(Sender : TObject); forward;
procedure UserKeyPress(Sender : TObject; var Key : Char); forward;
procedure ValidateOnChange(Sender : TObject); forward;
function ViasExistOnTrackLayer(const dummy : Integer) : Boolean; forward;



procedure _Start;
begin
    Start;
end;

procedure _StartWithDebug;
begin
    StartWithDebug;
end;


{......................................................................................................................}
procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak2 +
        'Use "_Start" to launch GUI. Use "FastDistributeByClearance" or ' + sLineBreak +
        '"FastDistributeByCenterLine" to skip GUI for some operations' + sLineBreak2 +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/Distribute' + sLineBreak2 +
        'Settings save location:' + sLineBreak + ConfigFile_GetPath;

    ShowInfo(MsgText, 'About Script');
end;
{......................................................................................................................}


{......................................................................................................................}
{ debugging function }
procedure AddToDebugListAfter(var Prim : IPCB_Track; LastIntercept : TCoord);
var
    X1, Y1, X2, Y2  : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    TempDebugList   : TStringList;

begin
    TempDebugList := CreateObject(TStringList);
    TempDebugList.CommaText := DebugList[DebugList.Count - 1];
    DebugList.Delete(DebugList.Count - 1);
    SetupDataFromTrack(Prim, IsVert, k, c, X1, Y1, X2, Y2);
    TempDebugList.Append(IntToStr(X1));
    TempDebugList.Append(IntToStr(Y1));
    TempDebugList.Append(IntToStr(X2));
    TempDebugList.Append(IntToStr(Y2));
    TempDebugList.Append(FloatToStr(k));
    TempDebugList.Append(IntToStr(c));
    TempDebugList.Append(IntToStr(LastIntercept));
    DebugList.Append(TempDebugList.CommaText);

end;
{......................................................................................................................}


{......................................................................................................................}
{ debugging function }
procedure AddToDebugListBefore(var Prim : IPCB_Track; TargetSlope : Double; TargetIntercept : TCoord);
var
    X1, Y1, X2, Y2  : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    TempDebugList   : TStringList;

begin
    TempDebugList := CreateObject(TStringList);
    SetupDataFromTrack(Prim, IsVert, k, c, X1, Y1, X2, Y2);
    TempDebugList.Append(IntToStr(X1));
    TempDebugList.Append(IntToStr(Y1));
    TempDebugList.Append(IntToStr(X2));
    TempDebugList.Append(IntToStr(Y2));
    TempDebugList.Append(FloatToStr(k));
    TempDebugList.Append(IntToStr(c));
    TempDebugList.Append(FloatToStr(TargetSlope));
    TempDebugList.Append(IntToStr(TargetIntercept));
    DebugList.Append(TempDebugList.CommaText);

end;
{......................................................................................................................}


{......................................................................................................................}
{ debugging function }
procedure AddToDebugListFirstVia(var Prim1 : IPCB_Via; var Prim2 : IPCB_Track);
var
    X1, Y1          : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    ViaSize         : TCoord;
    TempDebugList   : TStringList;

begin
    TempDebugList := CreateObject(TStringList);
    SetupDataFromVia(Prim1, Prim2, IsVert, k, c, X1, Y1, ViaSize);
    TempDebugList.Append(IntToStr(X1));
    TempDebugList.Append(IntToStr(Y1));
    TempDebugList.Append(IntToStr(ViaSize));
    TempDebugList.Append(FloatToStr(k));
    TempDebugList.Append(IntToStr(c));
    ViaDebugList.Append(TempDebugList.CommaText);

end;
{......................................................................................................................}


{......................................................................................................................}
{ debugging function }
procedure AddToDebugListSecondVia(var Prim1 : IPCB_Via; var Prim2 : IPCB_Track; const viaminc, viamaxc, midc : TCoord);
var
    X1, Y1, X2, Y2  : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    ViaSize         : TCoord;
    TempDebugList   : TStringList;
    trackslope      : Double;
    trackintercept  : TCoord;

begin
    TempDebugList := CreateObject(TStringList);
    TempDebugList.CommaText := ViaDebugList[ViaDebugList.Count - 1];
    ViaDebugList.Delete(ViaDebugList.Count - 1);
    SetupDataFromVia(Prim1, Prim2, IsVert, k, c, X1, Y1, ViaSize);
    TempDebugList.Append(IntToStr(X1));
    TempDebugList.Append(IntToStr(Y1));
    TempDebugList.Append(IntToStr(ViaSize));
    TempDebugList.Append(FloatToStr(k));
    TempDebugList.Append(IntToStr(c));
    SetupDataFromTrack(Prim2, IsVert, trackslope, trackintercept, X1, Y1, X2, Y2);
    TempDebugList.Append(FloatToStr(trackslope));
    TempDebugList.Append(IntToStr(trackintercept));
    TempDebugList.Append(IntToStr(viaminc));
    TempDebugList.Append(IntToStr(viamaxc));
    TempDebugList.Append(IntToStr(midc));
    ViaDebugList.Append(TempDebugList.CommaText);

end;
{......................................................................................................................}


{......................................................................................................................}
{ main procedure to distribute tracks }
procedure calculate(LaunchedFromGUI : Boolean);
var
    i, j                                                    : Integer;
    k1, k2                                                  : Double;
    c1, c2, minc, midc, maxc, stepc, cFromWidths            : TCoord;
    viaminc, viamaxc, viasize, viaclearance, cTotalWidths   : TCoord;
    IsVert1                                                 : Boolean;
    IsVert2                                                 : Boolean;
    IsFirstPoint                                            : Boolean;
    x11, x12, y11, y12                                      : TCoord; // altium zapis koordinata
    x21, x22, y21, y22                                      : TCoord;
    Prim1                                                   : IPCB_Primitive;
    Prim2                                                   : IPCB_Primitive;
    MaxNumOfChar                                            : Integer; // najvci broj znamenki u stringu
    NumOfChar                                               : Integer; // broj znamenki
    TempString                                              : string;
    coef                                                    : Double;

begin
    if (Board <> PCBServer.GetCurrentPCBBoard) then
    begin
        ShowError('Please start script using START procedure');
        close;
        exit;
    end;

    if not LaunchedFromGUI then TrimPerpendicular := False; // assume don't trim if GUI wasn't used

    if LaunchedFromGUI then
    begin
        // clear these flags if the GUI selections make them not applicable, to save some processing
        PadModeEnabled := PadModeEnabled and ((RadioDirections.Enabled and (RadioDirections.ItemIndex = 1)) or CheckBoxPadViaClearance.Checked);
        ViaModeEnabled := ViaModeEnabled and ((RadioDirections.Enabled and (RadioDirections.ItemIndex = 1)) or CheckBoxPadViaClearance.Checked);
    end;

    Board.NewUndo;

    // Start undo
    PCBServer.PreProcess;
    try

        // compile list of sorted tracks
        CompileSortedTracks(0);

        // seeks min and max c i.e. range of intercept values (trazi min i max C tj grnice sirenja   vodova)
        // moved intercept stats here to operate on sorted data instead of original selection. doing before sorting could cause issues with distribute by clearance if first selected track is a different width.
        Prim1 := SortedTracks.getObject(0);
        SetupDataFromTrack(Prim1, IsVert1, k1, c1, x11, y11, x12, y12);

        minc        := c1;
        maxc        := c1;

        if IsVert1 then coef := 1.0 // if first track has been coerced to vertical, coef needs to be 1 for later width compensation
        else coef        := cos(arctan(k1)); // y-intercept coefficient based on slope (i.e. how much intercept shifts for a perpendicular shift of the track)

        cFromWidths := Prim1.Width / (2 * coef);    // start cFromWidths with half of the first track's width
        cTotalWidths := Prim1.Width / coef;         // start cTotalWidths with first track's width

        // calculate extents of intercept values and sum of track widths in intercept units
        for i := 1 to SortedTracks.Count - 1 do
        begin
            Prim1 := SortedTracks.getObject(i);
            SetupDataFromTrack(Prim1, IsVert2, k2, c2, x21, y21, x22, y22);

            if (minc > c2) then minc := c2;
            if (maxc < c2) then maxc := c2;

            cFromWidths := cFromWidths + Prim1.Width / coef;    // add subsequent track widths
            cTotalWidths := cTotalWidths + Prim1.Width / coef;  // add subsequent track widths
        end;

        cFromWidths := cFromWidths - Prim1.Width / (2 * coef); // subtract half of last track's width

        midc := (Round(minc + maxc)) div 2; // midline between the outer pair of tracks

        if DebuggingEnabled then
        begin
            ViaDebugFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\DistributeScriptViaDebug.csv';
            ViaDebugList := CreateObject(TStringList);
            ViaDebugList.Append('Via1 X,Via1 Y,Via1 size,Via1 slope,Via1 intercept,Via2 X,Via2 Y,Via2 size,Via2 slope,Via2 intercept,ref track slope,ref track intercept, viaminc, viamaxc, midc');

            PadDebugFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\DistributeScriptPadDebug.csv';
            PadDebugList := CreateObject(TStringList);
            PadDebugList.Append('Pad1 Point X,Pad1 Point Y,Pad1 slope,Pad1 intercept,Pad2 Point X,Pad2 Point Y,Pad2 slope,Pad2 intercept,ref track slope,ref track intercept, viaminc, viamaxc, midc');
        end;

        // compile list of sorted pads (MUST come after list of sorted tracks) and calculate point on outline nearest the first track
        // re-use via variables to run the same execution path as between-vias
        if PadModeEnabled and CompileSortedPads then
        begin
            Prim2 := SortedTracks.getObject(0);         // get first track from the SortedTracks list

            //Prim1 := SortedPads.getObject(0); // get first pad from the SortedPads list
            viaminc := StrToInt(SortedPads[0]); // pads are pre-processed so c is already the intercept we care about
            //if DebuggingEnabled then AddToDebugListFirstPad(Prim1, viaminc); // TODO


            //Prim1 := SortedPads.getObject(1); // get second pad from the SortedPads list
            viamaxc := StrToInt(SortedPads[1]); // pads are pre-processed so c is already the intercept we care about

            if PadModeEnabled and CheckBoxPadViaClearance.Checked then
            begin
                TempString := EditPadViaClearance.Text;
                if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

                // convert desired intercept step using intercept coefficient and selected units
                if (ButtonPadViaUnits.Caption = 'mm') then viaclearance := mmsToCoord(StrToFloat(TempString)) / (coef)
                else viaclearance                                    := milsToCoord(StrToFloat(TempString)) / (coef);

                viaminc := viaminc + viaclearance;  // add user clearance
                viamaxc := viamaxc - viaclearance;  // subtract user clearance
            end;

            viaminc := Ceil(viaminc / 20) * 20;     // apply some rounding to make sure to clear by the requested amount
            viamaxc := Floor(viamaxc / 20) * 20;

            midc := (viaminc + viamaxc) div 2;   // midline between the vias, accounting for their size

            //if DebuggingEnabled then AddToDebugListSecondPad(Prim1, viaminc, viamaxc, midc); // TODO
        end
        else if ViaModeEnabled and (CompileSortedVias and ViasExistOnTrackLayer(0)) then
        begin
            Prim2 := SortedTracks.getObject(0);         // get first track from the SortedTracks list

            Prim1 := SortedVias.getObject(0);           // get first via from the SortedVias list
            SetupDataFromVia(Prim1, Prim2, IsVert1, k1, c1, x11, y11, viasize);
            viaminc := c1 + viasize / (2 * coef);          // add via pad radius

            if DebuggingEnabled then AddToDebugListFirstVia(Prim1, Prim2);

            Prim1 := SortedVias.getObject(1);           // get second via from the SortedVias list
            SetupDataFromVia(Prim1, Prim2, IsVert1, k1, c1, x11, y11, viasize);
            viamaxc := c1 - viasize / (2 * coef);          // subtract via pad radius

            if ViaModeEnabled and CheckBoxPadViaClearance.Checked then
            begin
                TempString := EditPadViaClearance.Text;
                if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

                // convert desired intercept step using intercept coefficient and selected units
                if (ButtonPadViaUnits.Caption = 'mm') then viaclearance := mmsToCoord(StrToFloat(TempString)) / (coef)
                else viaclearance                                    := milsToCoord(StrToFloat(TempString)) / (coef);

                viaminc := viaminc + viaclearance;  // add user clearance
                viamaxc := viamaxc - viaclearance;  // subtract user clearance
            end;

            viaminc := Ceil(viaminc / 20) * 20;     // apply some rounding to make sure to clear by the requested amount
            viamaxc := Floor(viamaxc / 20) * 20;

            midc := (viaminc + viamaxc) div 2;   // midline between the vias, accounting for their size

            if DebuggingEnabled then AddToDebugListSecondVia(Prim1, Prim2, viaminc, viamaxc, midc);
        end;

        // Add connected tracks to the track list, or pad the list if there is no track connected to a given end
        // (Dio koji puni object listu sa susjednim trackovima ako je track onda u listu ide YES a noj stavlja NO)
        i := 0;
        ResetParameters;
        AddStringParameter('Scope', 'All');
        RunProcess('PCB:DeSelect');
        while i < SortedTracks.Count do
        begin
            Prim1 := SortedTracks.getObject(i);
            SetupDataFromTrack(Prim1, IsVert1, k1, c1, x11, y11, x12, y12);

            // look for a track (call it track 1a) that is connected to this track's first point and insert it after this track in the list (provjera za prvu tocku i umece 1a liniju ispod 1 linije tracka)
            Prim2 := GetAnotherTrackInPoint(Prim1, Prim1.X1, Prim1.Y1, IsFirstPoint);

            if (Prim2 = nil) then SortedTracks.Insert(i + 1, '0') // no connected track so insert placeholder
            else
            begin // there is a connected track, validate it
                SetupDataFromTrack(Prim2, IsVert2, k2, c2, x21, y21, x22, y22);
                // test whether there are any problems on the track e.g. two segments are parallel (test da li ima nekih problema na tracku npr paralelna dve u nastavku)
                // i.e. there must be a single contiguous track segment (tj mora biti jedan jedini track)
                if ((IsVert1 = IsVert2) and (Abs(k1 - k2) < 0.01)) then
                begin // connected segment is parallel
                    Prim1.Selected := True;
                    Prim2.Selected := True;
                    Prim1.GraphicallyInvalidate;
                    Prim2.GraphicallyInvalidate;

                    ShowError('Problem on selected tracks (connected tracks are parallel - try glossing?)');
                end;

                // insert the connected track keyed by which of its ends is connected
                if IsFirstPoint then SortedTracks.InsertObject(i + 1, '1', Prim2)
                else SortedTracks.InsertObject(i + 1, '2', Prim2);
            end;

            // look for another track (call it track 1b) that is connected to this track's second point and insert it after track 1a in the list.
            // this makes a total of 3 lines: the track of interest and the two tracks connected to it, if any (Provjera za drugu tocku i umece 1b ispod linije 1a. Ukupno 3 linije)
            Prim2 := GetAnotherTrackInPoint(Prim1, Prim1.X2, Prim1.Y2, IsFirstPoint);

            if (Prim2 = nil) then SortedTracks.Insert(i + 2, '0') // no connected track so insert placeholder
            else
            begin // there is a connected track, validate it
                SetupDataFromTrack(Prim2, IsVert2, k2, c2, x21, y21, x22, y22);

                if ((IsVert1 = IsVert2) and (Abs(k1 - k2) < 0.01)) then
                begin // connected segment is parallel
                    Prim1.Selected := True;
                    Prim2.Selected := True;
                    Prim1.GraphicallyInvalidate;
                    Prim2.GraphicallyInvalidate;

                    ShowError('Problem on selected tracks (connected tracks are parallel - try glossing?)');
                end;

                // insert the connected track keyed by which of its ends is connected
                if IsFirstPoint then SortedTracks.InsertObject(i + 2, '1', Prim2)
                else SortedTracks.InsertObject(i + 2, '2', Prim2);
            end;

            i := i + 3; // jump ahead to next track of interest
        end;

        // there shouldn't be any tracks selected unless there were problems
        if Board.SelectecObjectCount <> 0 then exit;

        // all is well and about to start distributing, check if debugging is enabled
        if DebuggingEnabled then
        begin
            DebugFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\DistributeScriptDebug.csv';
            DebugList := CreateObject(TStringList);
            DebugList.Append('X1 before,Y1 before,X2 before,Y2 before,slope before,intercept before,TargetSlope,TargetIntercept,X1 after,Y1 after,X2 after,Y2 after,slope after,intercept after,LastIntercept');
        end;

        if RadioButtonCenters.Checked then
        begin
            stepc := (maxc - minc) / ((SortedTracks.Count / 3) - 1); // intercept increment is intercept extents divided by number of tracks - 1
            DistributeForward(minc, stepc, coef);
        end
        else if RadioButtonClearance.Checked then
        begin
            if (PadModeEnabled or ViaModeEnabled) and CheckBoxPadViaClearance.Checked then
            begin
                stepc := (viamaxc - viaminc - cTotalWidths) / ((SortedTracks.Count / 3) - 1); // same intercept increment as above but between vias and subtracting sum of track widths
                DistributeFromCenter(midc, stepc, coef);
            end
            else
            begin
                stepc := (maxc - minc - cFromWidths) / ((SortedTracks.Count / 3) - 1); // intercept increment is same as above but subtracting sum of track widths from intercept extents
                DistributeForward(minc, stepc, coef);
            end;
        end
        else
        begin // distributing using input value rather than between intercept extents
            TempString := EditDistance.Text;
            if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

            // convert desired intercept step using intercept coefficient and selected units
            if (ButtonUnits.Caption = 'mm') then stepc := mmsToCoord(StrToFloat(TempString)) / (coef)
            else stepc                                 := milsToCoord(StrToFloat(TempString)) / (coef);

            { CALL FUNCTION to actually move the tracks }
            case RadioDirections.ItemIndex of
                0 : DistributeForward(minc, stepc, coef);
                1 : DistributeFromCenter(midc, stepc, coef);
                2 : DistributeBackward(maxc, stepc, coef);
                else DistributeForward(minc, stepc, coef);
            end;
        end;

        if PadModeEnabled and ((CheckBoxPadViaClearance.Checked) or (RadioDirections.ItemIndex = 1)) then SelectListItems(SortedPads);
        if ViaModeEnabled and ((CheckBoxPadViaClearance.Checked) or (RadioDirections.ItemIndex = 1)) then SelectListItems(SortedVias);

        // save last-used values and presets
        if LaunchedFromGUI then ConfigFile_Write(ConfigFile_GetPath);

        if DebuggingEnabled then
        begin
            PadDebugList.SaveToFile(PadDebugFilePath);
            ViaDebugList.SaveToFile(ViaDebugFilePath);
            DebugList.SaveToFile(DebugFilePath);
            ShowInfo('Debugging output saved');
        end;

    finally
        // Stop undo
        PCBServer.PostProcess;

        close;

    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ Function to compile sorted list of pads. Note that pad count checks are assumed to have been done by InitialCheck procedure }
function CompileSortedPads(const dummy : Integer = 0) : Boolean;
var
    i, j                        : Integer;
    TempString                  : string;
    PrimTrack                   : IPCB_Primitive;
    PrimPad, PrimPad2           : IPCB_Primitive;
    IsVert_track                : Boolean;
    x11, x12, y11, y12          : TCoord;
    IsMidVert, IsIntVert        : Boolean;
    x_midpoint, y_midpoint      : TCoord;
    X_pad, Y_pad                : TCoord;
    k_track, k_midpoint, k_pad  : Double;
    c_track, c_midpoint, c_pad  : TCoord;
    PadsNotOnLayer              : Integer;
begin
    Result := False;
    PadsNotOnLayer := 0;
    if (SortedTracks = nil) or (SortedTracks.Count < 1) then exit;

    SortedPads := CreateObject(TStringList);

    // iterate through selection like CompileSortedTracks, but for the two pads.
    // Note that this will use SortedTracks list to project an intercept point for a parallel line passing through the closest point on each pad
    PrimTrack :=  SortedTracks.getObject(0);
    SetupDataFromTrack(PrimTrack, IsVert_track, k_track, c_track, x11, y11, x12, y12);

    // get midpoint between 2 pads in selection
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        PrimPad := Board.SelectecObject[i];
        if PrimPad.ObjectId = ePadObject then
        begin
            x_midpoint := PrimPad.x;
            y_midpoint := PrimPad.y;

            for j := i + 1 to Board.SelectecObjectCount - 1 do
            begin
                PrimPad2 := Board.SelectecObject[j];
                if (PrimPad2.ObjectId = ePadObject) and (PrimPad.I_ObjectAddress <> PrimPad2.I_ObjectAddress) then
                begin
                    x_midpoint := (x_midpoint + PrimPad2.x) div 2; // average x coord
                    y_midpoint := (y_midpoint + PrimPad2.y) div 2; // average y coord

                    break; // found second pad, break
                end;
            end;

            break; // either found both pads or ran out of selection, break either way
        end;
    end;

    // get the parallel line that intercepts midpoint
    GetParallelLine(k_track, c_track, IsVert_track, k_midpoint, c_midpoint, IsMidVert, x_midpoint, y_midpoint);

    // populate the list with only pads
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        PrimPad := Board.SelectecObject[i];
        if PrimPad.ObjectId = ePadObject then
        begin
            if SetupDataFromPad(PrimPad, PrimTrack.Layer, k_midpoint, c_midpoint, IsMidVert, k_pad, c_pad, IsIntVert, X_pad, Y_pad) then
            begin
                TempString := IntToStr(c_pad);

                if c_pad > 0 then TempString := '+' + TempString;

                SortedPads.AddObject(TempString, PrimPad); // store via object using intercept as key
            end
            else Inc(PadsNotOnLayer);
        end;
    end;

    if SortedPads.Count = 2 then
    begin
        // pad and sort list
        PadAndSort(SortedPads);
        Result := True;
    end
    else
    begin
        if PadsNotOnLayer > 0 then
        begin
            ShowWarning(IntToStr(PadsNotOnLayer) + ' pad(s) in selection don''t exist on track layers. Invalid pads will be deselected.');
        end;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ CompileSortedTracks }
procedure CompileSortedTracks(const dummy : Integer = 0);
var
    i                           : Integer;
    TempString                  : string;
    Prim1                       : IPCB_Primitive;
    k2                          : Double;
    c2                          : TCoord;
    IsVert2                     : Boolean;
    x21, x22, y21, y22          : TCoord;

begin
    // we have to sort the tracks in order because they are random
    SortedTracks := CreateObject(TStringList);

    // populate the initial list with only tracks
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Prim1 := Board.SelectecObject[i];
        if Prim1.ObjectId = eTrackObject then
        begin
            SetupDataFromTrack(Prim1, IsVert2, k2, c2, x21, y21, x22, y22);

            TempString := IntToStr(c2);

            if c2 > 0 then TempString := '+' + TempString;

            SortedTracks.AddObject(TempString, Prim1); // store track object using intercept as key
        end;
        i := i + 1;
    end;

    // pad and sort list
    PadAndSort(SortedTracks);
end;
{......................................................................................................................}


{......................................................................................................................}
{ Function to compile sorted list of vias. Note that via count checks are assumed to have been done by InitialCheck procedure }
function CompileSortedVias(const dummy : Integer = 0) : Boolean;
var
    i                           : Integer;
    TempString                  : string;
    PrimTrack                   : IPCB_Primitive;
    PrimVia                     : IPCB_Primitive;
    IsIntVert                   : Boolean;
    X, Y                        : TCoord;
    size                        : TCoord;
    c1                          : TCoord;
    k1                          : Double;

begin
    Result := False;
    if (SortedTracks = nil) or (SortedTracks.Count < 1) then exit;

    SortedVias := CreateObject(TStringList);

    // iterate through selection like CompileSortedTracks, but for the two vias.
    // Note that this will use SortedTracks list to project an intercept point for a parallel line passing through the via
    PrimTrack :=  SortedTracks.getObject(0);
    // populate the initial list with only vias
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        PrimVia := Board.SelectecObject[i];
        if PrimVia.ObjectId = eViaObject then
        begin
            SetupDataFromVia(PrimVia, PrimTrack, IsIntVert, k1, c1, X, Y, size);

            TempString := IntToStr(c1);

            if c1 > 0 then TempString := '+' + TempString;

            SortedVias.AddObject(TempString, PrimVia); // store via object using intercept as key
        end;
        i := i + 1;
    end;

    if SortedVias.Count = 2 then
    begin
        // pad and sort list
        PadAndSort(SortedVias);
        Result := True;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function ConfigFile_GetPath(dummy : String = ''): String;
begin
    result := ClientAPI_SpecialFolder_AltiumApplicationData + '\' + cConfigFileName;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure ConfigFile_Read(AFileName : String);
var
    IniFile: TIniFile;
begin
    if not FileExists(AFileName) then
    begin
        // ini file doesn't exist, try to fall back on older format file
        LoadPresetListFromFile(0);
        exit;
    end;

    IniFile := TIniFile.Create(AFileName);

    ConfigFile_Update(IniFile); // call separate function to handle any changes to key names for backward compatibility

    FormDistribute.Top    := IniFile.ReadInteger('Window Position', 'Top', FormDistribute.Top);
    FormDistribute.Left   := IniFile.ReadInteger('Window Position', 'Left', FormDistribute.Left);

    tPreset1.Text := IniFile.ReadString('Presets', 'Preset1', tPreset1.Text);
    tPreset2.Text := IniFile.ReadString('Presets', 'Preset2', tPreset2.Text);
    tPreset3.Text := IniFile.ReadString('Presets', 'Preset3', tPreset3.Text);
    tPreset4.Text := IniFile.ReadString('Presets', 'Preset4', tPreset4.Text);
    tPreset5.Text := IniFile.ReadString('Presets', 'Preset5', tPreset5.Text);
    tPreset6.Text := IniFile.ReadString('Presets', 'Preset6', tPreset6.Text);
    tPreset7.Text := IniFile.ReadString('Presets', 'Preset7', tPreset7.Text);
    tPreset8.Text := IniFile.ReadString('Presets', 'Preset8', tPreset8.Text);

    RadioDirections.ItemIndex       := IniFile.ReadInteger('Last Used', 'Distribute Direction', RadioDirections.ItemIndex);
    RadioButtonClearance.Checked    := IniFile.ReadBool('Last Used', 'Evenly Distribute Gaps', RadioButtonClearance.Checked);
    RadioButtonCenters.Checked      := IniFile.ReadBool('Last Used', 'Evenly Distribute Centerlines', RadioButtonCenters.Checked);
    RadioButtonClearanceVal.Checked := IniFile.ReadBool('Last Used', 'Distribute Clearance By Value', RadioButtonClearanceVal.Checked);
    RadioButtonCentersVal.Checked   := IniFile.ReadBool('Last Used', 'Distribute Centers By Value', RadioButtonCentersVal.Checked);
    CheckBoxTrimEnds.Checked        := IniFile.ReadBool('Last Used', 'Trim Track Ends', CheckBoxTrimEnds.Checked);
    EditPadViaClearance.Text        := IniFile.ReadString('Last Used', 'Pad & Via Clearance', EditPadViaClearance.Text);
    ButtonUnits.Caption             := IniFile.ReadString('Last Used', 'Units', ButtonUnits.Caption);
    ButtonPadViaUnits.Caption       := IniFile.ReadString('Last Used', 'Pad & Via Units', ButtonPadViaUnits.Caption);

    // Main input field needs to be set last because changing some other values trigger it
    EditDistance.Text               := IniFile.ReadString('Last Used', 'By-Value Distance', EditDistance.Text);

    IniFile.Free;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure ConfigFile_Write(AFileName : String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    IniFile.WriteInteger('Window Position', 'Top', FormDistribute.Top);
    IniFile.WriteInteger('Window Position', 'Left', FormDistribute.Left);

    IniFile.WriteString('Presets', 'Preset1', tPreset1.Text);
    IniFile.WriteString('Presets', 'Preset2', tPreset2.Text);
    IniFile.WriteString('Presets', 'Preset3', tPreset3.Text);
    IniFile.WriteString('Presets', 'Preset4', tPreset4.Text);
    IniFile.WriteString('Presets', 'Preset5', tPreset5.Text);
    IniFile.WriteString('Presets', 'Preset6', tPreset6.Text);
    IniFile.WriteString('Presets', 'Preset7', tPreset7.Text);
    IniFile.WriteString('Presets', 'Preset8', tPreset8.Text);

    IniFile.WriteInteger('Last Used', 'Distribute Direction', RadioDirections.ItemIndex);
    IniFile.WriteBool('Last Used', 'Evenly Distribute Gaps', RadioButtonClearance.Checked);
    IniFile.WriteBool('Last Used', 'Evenly Distribute Centerlines', RadioButtonCenters.Checked);
    IniFile.WriteBool('Last Used', 'Distribute Clearance By Value', RadioButtonClearanceVal.Checked);
    IniFile.WriteBool('Last Used', 'Distribute Centers By Value', RadioButtonCentersVal.Checked);
    IniFile.WriteBool('Last Used', 'Trim Track Ends', CheckBoxTrimEnds.Checked);
    IniFile.WriteString('Last Used', 'Pad & Via Clearance', EditPadViaClearance.Text);
    IniFile.WriteString('Last Used', 'Units', ButtonUnits.Caption);
    IniFile.WriteString('Last Used', 'Pad & Via Units', ButtonPadViaUnits.Caption);
    IniFile.WriteString('Last Used', 'By-Value Distance', EditDistance.Text);

    IniFile.Free;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure ConfigFile_Update(ConfigFile : TIniFile);
var
    IniSection, OldSetting, NewSetting, SettingValue : String;
begin
    // v1.60 changed via modes to pad/via
    begin
        IniSection := 'Last Used';
        OldSetting := 'Via Clearance';
        NewSetting := 'Pad & Via Clearance';
        if ConfigFile.ValueExists(IniSection, OldSetting) then
        begin
            SettingValue := ConfigFile.ReadString(IniSection, OldSetting, ''); // read the old setting
            ConfigFile.DeleteKey(IniSection, OldSetting); // delete the old key
            ConfigFile.WriteString(IniSection, NewSetting, SettingValue); // write the new key
        end;

        IniSection := 'Last Used';
        OldSetting := 'Via Units';
        NewSetting := 'Pad & Via Units';
        if ConfigFile.ValueExists(IniSection, OldSetting) then
        begin
            SettingValue := ConfigFile.ReadString(IniSection, OldSetting, ''); // read the old setting
            ConfigFile.DeleteKey(IniSection, OldSetting); // delete the old key
            ConfigFile.WriteString(IniSection, NewSetting, SettingValue); // write the new key
        end;

    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function DebugContourInfo(contour : IPCB_Contour) : TStringList;
var
    PointList: TStringList;
    iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    if contour.Count > 0 then
    begin
        PointList.Add('contour points');
        for iPoint := 0 to contour.Count - 1 do
        begin
            PointList.Add(Format('%d:%s,%s', [iPoint, CoordToDisplayX(contour.x(iPoint)), CoordToDisplayY(contour.y(iPoint))]));
        end;
    end
    else PointList.Add('Empty or invalid contour');

    Result := PointList;
end;
{......................................................................................................................}


{......................................................................................................................}
function DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList;
var
    PointList: TStringList;
    contour: IPCB_Contour;
    iPoly, iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    PointList.Clear;
    if (poly <> nil) and (poly.Count > 0) then
    begin
        for iPoly := 0 to poly.Count - 1 do
        begin
            contour := poly.Contour(iPoly);
            PointList.AddStrings(DebugContourInfo(contour));
        end;
    end
    else
    begin
        PointList.Add('Polygon has no contours');
    end;

    Result := PointList;
end;
{......................................................................................................................}


{......................................................................................................................}
function DistributeBackward(startc, stepc : TCoord; coef : Double);
var
    i, j                           : Integer;
    TrimTrackIndex                 : Integer;
    TargetSlope, k                 : Double;
    TargetIntercept, LastIntercept : TCoord;
    IsVert1                        : Boolean;
    x11, x12, y11, y12             : TCoord;
    Prim1                          : IPCB_Primitive;

begin
    // start sliding tracks around
    i              := SortedTracks.Count - 3; // start at the end so we can work backward
    j              := 0; // track step counter
    TrimTrackIndex := i;
    while i >= 0 do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, k, TargetIntercept, x11, y11, x12, y12);

        if i = TrimTrackIndex then
        begin
            TargetSlope     := k;
            TargetIntercept := startc;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then TargetIntercept := startc - j * stepc // if using centers, use step size directly
            else TargetIntercept := LastIntercept - stepc - Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, coef, TargetSlope, TargetIntercept, LastIntercept, True);

        i := i - 3; // jump back to the preceding track in the sorted list
        inc(j); // increment track step counter
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function DistributeForward(startc, stepc : TCoord; coef : Double);
var
    i, j                           : Integer;
    TrimTrackIndex                 : Integer;
    TargetSlope, k                 : Double;
    TargetIntercept, LastIntercept : TCoord;
    IsVert1                        : Boolean;
    x11, x12, y11, y12             : TCoord;
    Prim1                          : IPCB_Primitive;

begin
    // start sliding tracks around
    i              := 0;
    j              := 0; // track step counter
    TrimTrackIndex := i;
    while i < SortedTracks.Count do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, k, TargetIntercept, x11, y11, x12, y12);

        if i = TrimTrackIndex then
        begin
            TargetSlope     := k;
            TargetIntercept := startc;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then TargetIntercept := startc + j * stepc // if using centers, use step size directly
            else TargetIntercept := LastIntercept + stepc + Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, coef, TargetSlope, TargetIntercept, LastIntercept, False);

        i := i + 3; // advance to next track in sorted list
        inc(j); // increment track step counter
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function DistributeFromCenter(startc, stepc : TCoord; coef : Double);
var
    i, j                           : Integer;
    TrimTrackIndex                 : Integer;
    ListSplitIndex                 : Integer;
    TargetSlope, k                 : Double;
    TargetIntercept, LastIntercept : TCoord;
    SplitIntercept                 : TCoord;
    IsVert1                        : Boolean;
    x11, x12, y11, y12             : TCoord;
    Prim1                          : IPCB_Primitive;

begin
    // first, find index to split track list for forward and backward operations
    // odd number of tracks means center will be a track
    if (SortedTracks.Count mod 6) <> 0 then
    begin // odd track count
        // need to set middle track to center
        // 3 tracks (9 elements) should split at track 2 (i=3) and distribute 1 track forward and 1 back from center            0 < 3* > 6
        // 5 tracks (15 elements) should split at track 3 (i=6) and distribute 2 tracks forward and 2 back from center      0 < 3 < 6* > 9 > 12
        // 7 tracks (21 elements) should split at track 4 (i=9) and distribute 3 tracks forward and 3 back from center  0 < 3 < 6 < 9* > 12 > 15 > 18
        ListSplitIndex := (SortedTracks.Count - 3) div 2;
        SplitIntercept := startc;
    end
    else
    begin // even track count
        // middle two tracks need to straddle center
        // 2 tracks (6 elements) should split at track 1 (i=0) and distribute 1 track forward                           0* > 3
        // 4 tracks (12 elements) should split at track 2 (i=3) and distribute 3 tracks forward and 1 back          0 < 3* > 6 > 9
        // 6 tracks (18 elements) should split at track 3 (i=6) and distribute 4 tracks forward and 2 back      0 < 3 < 6* > 9 > 12 > 15
        // 8 tracks (24 elements) should split at track 4 (i=9) and distribute 5 tracks forward and 3 back  0 < 3 < 6 < 9* > 12 > 15 > 18 > 21
        ListSplitIndex := (SortedTracks.Count div 2) - 3;
        if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then SplitIntercept := startc - (stepc / 2)
        else
        begin
            Prim1          := SortedTracks.getObject(ListSplitIndex); // need to grab anchor track to compensate for its width
            SplitIntercept := startc - (stepc / 2) - Prim1.Width / (2 * coef);
        end;
    end;

    // start sliding tracks around (forward section)
    i              := ListSplitIndex;
    j              := 0; // track step counter
    TrimTrackIndex := ListSplitIndex;
    while i < SortedTracks.Count do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, k, TargetIntercept, x11, y11, x12, y12);

        if i = TrimTrackIndex then
        begin
            TargetSlope     := k;
            TargetIntercept := SplitIntercept;
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then TargetIntercept := SplitIntercept + j * stepc // if using centers, use step size directly
            else TargetIntercept := LastIntercept + stepc + Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width
        end;

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, coef, TargetSlope, TargetIntercept, LastIntercept, False);

        i := i + 3; // advance to next track in sorted list
        inc(j); // increment track step counter
    end;

    // now start sliding tracks around (backward section)
    i              := ListSplitIndex;
    j              := 0;
    TrimTrackIndex := ListSplitIndex;
    while i >= 0 do
    begin
        Prim1          := SortedTracks.getObject(i);
        Prim1.Selected := True;
        SetupDataFromTrack(Prim1, IsVert1, k, TargetIntercept, x11, y11, x12, y12);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := SplitIntercept;
            GetEdgeIntercept(i, coef, LastIntercept, True);
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then TargetIntercept := SplitIntercept - j * stepc // if using centers, use step size directly
            else TargetIntercept := LastIntercept - stepc - Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width

            MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, coef, TargetSlope, TargetIntercept, LastIntercept, True);
        end;

        i := i - 3; // jump back to the preceding track in the sorted list
        inc(j); // increment track step counter
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ function to enable or disable controls related to between-via distribute mode }
procedure EnableBetweenPadViaControls(NewEnable : Boolean);
begin
    if RadioButtonClearance.Checked then
    begin
        CheckBoxPadViaClearance.Enabled    := NewEnable;
        if Board.SelectecObjectCount > 3 then
        begin
            EditPadViaClearance.Enabled    := CheckBoxPadViaClearance.Checked;
            ButtonPadViaUnits.Enabled      := CheckBoxPadViaClearance.Checked;
        end;
    end
    else
    begin
        CheckBoxPadViaClearance.Enabled    := False;
        EditPadViaClearance.Enabled        := False;
        ButtonPadViaUnits.Enabled          := False;
    end;

    CheckBoxPadViaClearance.Visible    := NewEnable;
    EditPadViaClearance.Visible        := NewEnable;
    ButtonPadViaUnits.Visible          := NewEnable;

end;
{......................................................................................................................}


{......................................................................................................................}
{ function to enable or disable controls related to by-value distribute mode }
procedure EnableByValControls(NewEnable : Boolean);
begin
    ButtonPreset1.Enabled   := NewEnable;
    ButtonPreset2.Enabled   := NewEnable;
    ButtonPreset3.Enabled   := NewEnable;
    ButtonPreset4.Enabled   := NewEnable;
    ButtonPreset5.Enabled   := NewEnable;
    ButtonPreset6.Enabled   := NewEnable;
    ButtonPreset7.Enabled   := NewEnable;
    ButtonPreset8.Enabled   := NewEnable;
    tPreset1.Enabled        := NewEnable;
    tPreset2.Enabled        := NewEnable;
    tPreset3.Enabled        := NewEnable;
    tPreset4.Enabled        := NewEnable;
    tPreset5.Enabled        := NewEnable;
    tPreset6.Enabled        := NewEnable;
    tPreset7.Enabled        := NewEnable;
    tPreset8.Enabled        := NewEnable;
    ButtonUnits.Enabled     := NewEnable;
    RadioDirections.Enabled := NewEnable;
    EditDistance.Enabled    := NewEnable;

    if NewEnable then
    begin
        EditDistance.SetFocus;
        EditDistance.SelectAll;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure FastDistributeByCenterline;
var
    status : Integer;
begin
    InitialCheck(status);
    if status = 0 then
    begin
        DebuggingEnabled := False;
        RadioButtonClearance.Checked := False;
        RadioButtonCenters.Checked   := True;
        calculate(False);
    end
    else exit;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure FastDistributeByClearance;
var
    status   : Integer;
    InitialCheckResult : Integer;
begin
    InitialCheckResult := InitialCheck(status);
    if (InitialCheckResult = 3) then PadModeEnabled := True else PadModeEnabled := False;
    if (InitialCheckResult = 2) then ViaModeEnabled := True else ViaModeEnabled := False;

    if status = 0 then
    begin
        if (PadModeEnabled or ViaModeEnabled) and (Board.SelectecObjectCount = 3) then
        begin
            // two pads/vias and one track selected, do track centering instead
            DebuggingEnabled                := False;
            RadioButtonClearance.Checked    := True;
            CheckBoxPadViaClearance.Checked := True;
            calculate(False);
        end
        else
        begin
            DebuggingEnabled                := False;
            RadioButtonCenters.Checked      := False;
            RadioButtonClearance.Checked    := True;
            calculate(False);
        end;
    end
    else exit;
end;
{......................................................................................................................}


{......................................................................................................................}
{ checks if there is another track connected to the given endpoint of this track, and which of its ends is connected }
function GetAnotherTrackInPoint(Prim1 : IPCB_Track; X, Y : TCoord; out OnFirstPoint : Boolean) : IPCB_Primitive;
var
    SIter : IPCB_SpatialIterator;
    Prim2 : IPCB_Track;
begin
    Result       := nil;
    OnFirstPoint := False;

    // Check if there is another track in hotspot
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
    SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
    SIter.AddFilter_Area(X - 1, Y - 1, X + 1, Y + 1);

    Prim2 := SIter.FirstPCBObject;
    while (Prim2 <> nil) do
    begin
        if (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and (not Prim2.TearDrop) then
        begin
            if (PointToPointDistance(Prim2.X1, Prim2.Y1, X, Y) <= 100) then
            begin
                Result       := Prim2;
                OnFirstPoint := True;
                Board.SpatialIterator_Destroy(SIter);
                exit;
            end;

            if (PointToPointDistance(Prim2.X2, Prim2.Y2, X, Y) <= 100) then
            begin
                Result := Prim2;
                Board.SpatialIterator_Destroy(SIter);
                exit;
            end;
        end;

        Prim2 := SIter.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(SIter);
end;
{......................................................................................................................}


{......................................................................................................................}
{ Gets width-aware edge intercept }
function GetEdgeIntercept(const ThisTrackIndex : Integer; const coef : Double; out LastIntercept : TCoord; const Reverse : Boolean);
var
    k1                      : Double;   // k1 is throwaway
    c1                      : TCoord;
    IsVert1                 : Boolean;  // isVert1 is throwaway
    x11, x12, y11, y12      : TCoord;   // throwaway
    Prim1                   : IPCB_Primitive;

begin
    Prim1 := SortedTracks.getObject(ThisTrackIndex);
    SetupDataFromTrack(Prim1, IsVert1, k1, c1, x11, y11, x12, y12);

    if Reverse then LastIntercept := c1 - (Prim1.Width / (2 * coef)) // subtract half of current track width for next pass (intercept at moved track edge)
    else LastIntercept            := c1 + (Prim1.Width / (2 * coef)); // add half of current track width for next pass (intercept at moved track edge)

end;
{......................................................................................................................}


{......................................................................................................................}
{ function to test if two tracks have an intercept point (tracks are not parallel) }
function GetIntersection(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; k2 : Double; c2 : TCoord; IsPrim2Vert : Boolean; out X, Y : TCoord) : Boolean;
begin
    Result := True;
    if (IsPrim1Vert and IsPrim2Vert) or ((not IsPrim1Vert) and (not IsPrim2Vert) and (Abs(k1 - k2) < 0.01)) then
    begin
        // Parallel tracks
        Result := False;
    end
    else if IsPrim1Vert or IsPrim2Vert then
    begin
        if IsPrim1Vert then
        begin
            X := c1;
            Y := k2 * c1 + c2;
        end
        else // IsPrim2Vert
        begin
            X := c2;
            Y := k1 * c2 + c1;
        end;
    end
    else
    begin // neither line is vertical
        X := (c2 - c1) / (k1 - k2);
        Y := k1 * X + c1;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ Get GeometricPolygon using PCBServer.PCBContourMaker }
function GetPadPoly(Obj: IPCB_Pad; Expansion: TCoord = 0; ALayer : TV7_Layer = eNoLayer) : IPCB_GeometricPolygon;
var
    Poly : IPCB_GeometricPolygon;
    ViaExpansion : TCoord;
    ViaRadius : TCoord;
begin
    if Obj.ObjectId = ePadObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        if ALayer = eNoLayer then ALayer := Obj.Layer;

        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, ALayer);
    end;

    result := Poly;
end;


{......................................................................................................................}
{function to create slope and intercept for virtual line parallel to an ordinate line and passing through a point (k1,c1,IsPrim1Vert are for ordinate line)}
function GetParallelLine(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; out k2 : Double; out c2 : TCoord; out IsPrim2Vert : Boolean; X, Y : TCoord) : Boolean;
begin
    Result := True;

    if IsPrim1Vert then
    begin // If the first line is vertical, the parallel line is also vertical
        IsPrim2Vert := True;
        c2          := X;
    end
    else if k1 = 0 then
    begin // If the first line is horizontal, the parallel line is also horizontal
        IsPrim2Vert := False;
        k2          := 0;
        c2          := Y;
    end
    else // If the first line is not vertical or horizontal
    begin
        Result      := False; // Return false if the parallel line is not horizontal or vertical
        IsPrim2Vert := False;
        k2          := k1;
        c2          := Y - (k2 * X); // Parallel line intercept
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ function to create slope and intercept for virtual line perpendicular to a point (k1,c1,IsPrim1Vert are for ordinate line; k2,c2,IsPrim2Vert are for perpendicular line; X,Y are a point that the perpendicular line passes through)}
function GetPerpendicularLine(k1 : Double; c1 : TCoord; IsPrim1Vert : Boolean; out k2 : Double; out c2 : TCoord; out IsPrim2Vert : Boolean; X, Y : TCoord) : Boolean;
begin
    Result := True;
    if IsPrim1Vert then
    begin // if first line is vertical, perpendicular line is simply horizontal
        IsPrim2Vert := False;
        k2          := 0;
        c2          := Y;
    end
    else if k1 = 0 then
    begin // if first line is horizontal, perpendicular line is simply vertical
        IsPrim2Vert := True;
        c2          := X;
    end
    else // if first line is not vertical or horizontal
    begin
        Result      := False; // return false if perpendicular line is not horizontal or vertical
        IsPrim2Vert := False;
        k2          := 1.0 / (-k1);  // perpendicular slope is negative reciprocal
        c2          := Y - (k2 * X); // perpendicular line intercept
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function GetPresetButtonEnable(const dummy : Integer) : Boolean;
begin
    Result := ButtonPreset1.Enabled;
end;
{......................................................................................................................}


{......................................................................................................................}
{ InitialCheck function performs initial validation checks on selected objects. Now returns ViaCount for use in logic. }
function InitialCheck(var status : Integer) : Integer;
var
    i                  : Integer;
    Prim1              : IPCB_Primitive;
    k1, k2             : Double;
    c1, c2,            : TCoord;
    IsVert1, IsVert2   : Boolean;
    x11, x12, y11, y12 : TCoord;
    x21, x22, y21, y22 : TCoord;
    PadCount           : Integer;
    ViaCount           : Integer;
begin
    status := 0; // clear result status
    Result := 0;

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    PadCount := 0;
    ViaCount := 0;

    // Count pads and vias without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eViaObject) then ViaCount := ViaCount + 1;
        if (Prim1.ObjectId = ePadObject) then PadCount := PadCount + 1;
    end;

    // need to deselect anything that isn't a pad, via, or electrical track as these will cause errors elsewhere)
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (PadCount <> 2) then if (Prim1.ObjectId = ePadObject) then Prim1.Selected := False; // deselect any pads unless count is 2
        if (ViaCount <> 2) or (PadCount = 2) then if (Prim1.ObjectId = eViaObject) then Prim1.Selected := False; // deselect and vias unless count is 2 or pads are already 2
        if (((Prim1.ObjectId <> eTrackObject) and (Prim1.ObjectId <> ePadObject) and (Prim1.ObjectId <> eViaObject)) or ((Prim1.ObjectId = eTrackObject) and not Prim1.InNet)) then Prim1.Selected := False;
    end;

    // make sure there are enough objects selected to operate upon
    if ((PadCount + ViaCount = 2) and (Board.SelectecObjectCount < 3)) or ((PadCount + ViaCount <> 2) and (Board.SelectecObjectCount < 2)) then
    begin
        if DebuggingEnabled then
        begin
            ShowError('Select at least 2 electrical tracks or ONE pair of pads or vias and at least one electrical track.' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'PadCount: ' + IntToStr(PadCount) + sLineBreak +
                        'ViaCount: ' + IntToStr(ViaCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else ShowError('Select at least 2 electrical tracks or ONE pair of pads or vias and at least one electrical track.');

        status := 1;
        exit;
    end;

    // Since we're allowing vias now, we should instead iterate until reaching the first track
    // Find the first track and store its index in 'i'
    for i := 0 to Board.SelectecObjectCount - 1 do
        if Board.SelectecObject[i].ObjectId = eTrackObject then
        begin
            // Set up the initial track data
            Prim1 := Board.SelectecObject[i];
            SetupDataFromTrack(Prim1, IsVert1, k1, c1, x11, y11, x12, y12);
            break;
        end;

    // Check that all selected tracks are parallel with the first one
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if Prim1.ObjectId = eTrackObject then
        begin
            SetupDataFromTrack(Prim1, IsVert2, k2, c2, x21, y21, x22, y22);

            if ((IsVert1 <> IsVert2) or (Abs(k1 - k2) > 0.01)) then
            begin
                if DebuggingEnabled then
                begin
                    ShowError('Selected tracks have to be parallel.' + sLineBreak + sLineBreak +
                                '-- Debugging Info --' + sLineBreak +
                                'IsVert1: ' + BoolToStr(IsVert1, True) + sLineBreak +
                                'IsVert2: ' + BoolToStr(IsVert2, True) + sLineBreak +
                                'k1: ' + FloatToStr(k1) + sLineBreak +
                                'k2: ' + FloatToStr(k2) + sLineBreak +
                                'Abs(k1 - k2): ' + FloatToStr(Abs(k1 - k2)));
                end
                else ShowError('Selected tracks have to be parallel.');

                status := 1;
                exit;
            end;
        end;
    end;

    if PadCount = 2 then Result := 3 else if ViaCount = 2 then Result := 2;
end;
{......................................................................................................................}


{......................................................................................................................}
function IsStringANum(Text : string) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;

    // Test for number, dot or comma
    ChSet := SetUnion(MkSet(Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
    for i := 1 to Length(Text) do
        if not InSet(Ord(Text[i]), ChSet) then Result := False;

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet := MkSet(Ord('.'), Ord(','));
    for i := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

    if dotCount > 1 then Result := False;
end;
{......................................................................................................................}


{......................................................................................................................}
{ ** DEPRECATED - replaced with ConfigFile_Read ** function to load preset list from file }
procedure LoadPresetListFromFile(const dummy : Integer);
const
    NumPresets = 18; // no longer needed with ini format
var
    PresetFilePath    : string;
    PresetList        : TStringList;
begin
    // default file name is MyDistributePresets.txt
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyDistributePresets.txt';
    PresetList     := CreateObject(TStringList);
    if FileExists(PresetFilePath) then
    begin
        if DebuggingEnabled then ShowInfo('Loading presets from ' + PresetFilePath);
        PresetList.LoadFromFile(PresetFilePath); // load presets from file if it exists

        case PresetList.Count of
            14 : PresetList.Add(CheckBoxTrimEnds.Checked);  // PresetList[14] (14-element PresetList implies v1.4)
            15 :
                begin
                    PresetList.Add(EditPadViaClearance.Text);  // PresetList[15] (added in v1.54)
                    PresetList.Add(ButtonUnits.Caption);    // PresetList[16] (added in v1.54)
                    PresetList.Add(ButtonPadViaUnits.Caption); // PresetList[17] (added in v1.54)
                end;
            NumPresets :
                begin
                    // do nothing
                end;
            else exit; // if PresetList.Count < NumPresets then PresetList file exists but count is short, just exit and use defaults
        end;

        // set text boxes to match preset list (redundant if list was regenerated above)
        tPreset1.Text                   := PresetList[1];
        tPreset2.Text                   := PresetList[2];
        tPreset3.Text                   := PresetList[3];
        tPreset4.Text                   := PresetList[4];
        tPreset5.Text                   := PresetList[5];
        tPreset6.Text                   := PresetList[6];
        tPreset7.Text                   := PresetList[7];
        tPreset8.Text                   := PresetList[8];
        RadioDirections.ItemIndex       := PresetList[9];
        RadioButtonClearance.Checked    := PresetList[10];
        RadioButtonCenters.Checked      := PresetList[11];
        RadioButtonClearanceVal.Checked := PresetList[12];
        RadioButtonCentersVal.Checked   := PresetList[13];
        CheckBoxTrimEnds.Checked        := PresetList[14];
        EditPadViaClearance.Text           := PresetList[15];
        ButtonUnits.Caption             := PresetList[16];
        ButtonPadViaUnits.Caption          := PresetList[17];
        EditDistance.Text               := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    end
    else exit; // if preset file didn't exist at all, just exit (older file format deprecated)

end;
{......................................................................................................................}


{......................................................................................................................}
{ bundles up track move functionality that is common among all modes }
function MoveTrackToIntercept(ThisTrackIndex, ConnectedTrackOneIndex, ConnectedTrackTwoIndex, TrimTrackIndex : Integer; coef, TargetSlope : Double; TargetIntercept, out LastIntercept : TCoord; Reverse : Boolean);
var
    k0, k1, k2                : Double;  // k0 is throwaway
    c0, c1, c2                : TCoord;  // c0, c1 are throwaway
    IsVert0, IsVert1, IsVert2 : Boolean; // isVert0 is throwaway
    x01, x02, y01, y02        : TCoord;  // used for storing first line endpoints
    x11, x12, y11, y12        : TCoord;  // altium zapis koordinata
    x21, x22, y21, y22        : TCoord;
    Prim0, Prim1, Prim2       : IPCB_Primitive;
    X, Y                      : TCoord;

begin
    Prim1 := SortedTracks.getObject(ThisTrackIndex);
    SetupDataFromTrack(Prim1, IsVert1, k1, c1, x11, y11, x12, y12);

    if TrimPerpendicular then
    begin
        Prim0 := SortedTracks.getObject(TrimTrackIndex);
        SetupDataFromTrack(Prim0, IsVert0, k0, c0, x01, y01, x02, y02);
    end;

    if DebuggingEnabled then AddToDebugListBefore(Prim1, TargetSlope, TargetIntercept);

    Prim1.BeginModify;

    Prim2 := SortedTracks.getObject(ConnectedTrackOneIndex); // track connected to first end of the moving track
    if (SortedTracks[ConnectedTrackOneIndex] <> '0') then
    begin // if there *is* a connected track on this end
        SetupDataFromTrack(Prim2, IsVert2, k2, c2, x21, y21, x22, y22);
        if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
        begin // tracks intercept, X & Y are the point where they do
            // move this track's first point to the intercept point
            Prim1.X1 := X;
            Prim1.Y1 := Y;

            Prim2.BeginModify;
            if (SortedTracks[ConnectedTrackOneIndex] = '1') then // connected track was connected by its first point
            begin // move connected track's first point to the intercept point
                Prim2.X1 := X;
                Prim2.Y1 := Y;
            end
            else // else connected track was connected by its second point
            begin // move connected track's second point to the intercept point
                Prim2.X2 := X;
                Prim2.Y2 := Y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
        end;
    end
    else if TrimPerpendicular then
    begin // there was no track connected to the first point ('0' padded) and we are trimming
        if GetPerpendicularLine(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, x01, y01) then
        begin // GetPerpendicularLine returns true if perpendicular line is either vertical or horizontal
            if IsVert1 then
            begin // if ordinate line IsVert1 is true then perpendicular line is horizontal
                Prim1.X1 := TargetIntercept; // set to TargetIntercept

                // if vertical and actual Y coords don't match output of SetupDataFromTrack, then they were coerced and swapped
                if Prim1.Y1 = y12 then Prim1.Y1 := y02 // use swapped end instead
                else                   Prim1.Y1 := y01; // trim Y coord

            end
            else // ordinate line is horizontal, perpendicular line is vertical
            begin
                Prim1.Y1 := TargetIntercept; // set to TargetIntercept
                Prim1.X1 := x01; // trim X coord
            end;
        end
        else // GetPerpendicularLine returns false if perpendicular line is neither vertical nor horizontal
        begin // extend/trim Prim1 to intercept virtual perpendicular line
            if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
            begin // tracks intercept, X & Y are the point where they do
                // move this track's first point to the intercept point with the perpendicular line
                Prim1.X1 := X;
                Prim1.Y1 := Y;
                Prim1.Y1 := TargetSlope * Prim1.X1 + TargetIntercept; // set final Y coord after approximate trim
            end;
        end;
    end
    else // else TrimPerpendicular is False
    begin
        if IsVert1 then Prim1.X1 := TargetIntercept
        else Prim1.Y1            := TargetSlope * Prim1.X1 + TargetIntercept;
    end;

    Prim2 := SortedTracks.getObject(ConnectedTrackTwoIndex); // track connected to second end of the moving track
    if (SortedTracks[ConnectedTrackTwoIndex] <> '0') then
    begin
        SetupDataFromTrack(Prim2, IsVert2, k2, c2, x21, y21, x22, y22);

        if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
        begin
            Prim1.X2 := X;
            Prim1.Y2 := Y;

            Prim2.BeginModify;
            if (SortedTracks[ConnectedTrackTwoIndex] = '1') then
            begin
                Prim2.X1 := X;
                Prim2.Y1 := Y;
            end
            else
            begin
                Prim2.X2 := X;
                Prim2.Y2 := Y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
        end;
    end
    else if TrimPerpendicular then
    begin // there was no track connected to the second point ('0' padded) and we are trimming
        if GetPerpendicularLine(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, x02, y02) then
        begin // GetPerpendicularLine returns true if perpendicular line is either vertical or horizontal
            if IsVert1 then
            begin // if ordinate line IsVert1 is true then perpendicular line is horizontal
                Prim1.X2 := TargetIntercept; // set to TargetIntercept

                // if vertical and actual Y coords don't match output of SetupDataFromTrack, then they were coerced and swapped
                if Prim1.Y1 = y12 then Prim1.Y2 := y01 // use swapped end instead
                else                   Prim1.Y2 := y02; // trim Y coord

            end
            else // ordinate line is horizontal, perpendicular line is vertical
            begin
                Prim1.Y2 := TargetIntercept; // set to TargetIntercept
                Prim1.X2 := x02; // trim X coord
            end;
        end
        else // GetPerpendicularLine returns false if perpendicular line is neither vertical nor horizontal
        begin // extend/trim Prim1 to intercept virtual perpendicular line
            if GetIntersection(TargetSlope, TargetIntercept, IsVert1, k2, c2, IsVert2, X, Y) then
            begin // tracks intercept, X & Y are the point where they do
                // move this track's first point to the intercept point with the perpendicular line
                Prim1.X2 := X;
                Prim1.Y2 := Y;
                Prim1.Y2 := TargetSlope * Prim1.X2 + TargetIntercept; // set final Y coord after approximate trim
            end;
        end;
    end
    else // else TrimPerpendicular is False
    begin
        if IsVert1 then Prim1.X2 := TargetIntercept
        else Prim1.Y2            := TargetSlope * Prim1.X2 + TargetIntercept;
    end;

    Prim1.EndModify;
    Prim1.GraphicallyInvalidate;
    Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim1.I_ObjectAddress);

    if Prim1.InNet then
    begin
        Prim1.Net.ConnectivelyInValidate;
    end;

    if Reverse then LastIntercept := TargetIntercept - (Prim1.Width / (2 * coef)) // subtract half of current track width for next pass (intercept at moved track edge)
    else LastIntercept            := TargetIntercept + (Prim1.Width / (2 * coef)); // add half of current track width for next pass (intercept at moved track edge)

    if DebuggingEnabled then AddToDebugListAfter(Prim1, LastIntercept);

end;
{......................................................................................................................}


{......................................................................................................................}
{ PadAndSort }
procedure PadAndSort(var list : TStringList);
var
    i, j                        : Integer;
    NumOfChar, MaxNumOfChar     : Integer;
    TempString                  : string;

begin
    // pad strings with leading zeros for sort function
    MaxNumOfChar := 0;
    for i        := 0 to list.Count - 1 do
    begin
        NumOfChar                                       := Length(list[i]);
        if (MaxNumOfChar < NumOfChar) then MaxNumOfChar := NumOfChar;
    end;

    for i := 0 to list.Count - 1 do
    begin
        TempString := list[i];

        while (Length(TempString) < MaxNumOfChar) do Insert('0', TempString, 2);

        list[i] := TempString;
    end;

    list.Sort;

    // Sort list again to put negative intercepts at the front
    // ShowMessage (list[0][1]);  // display the first character of the first element's string
    if (list[list.Count - 1][1] = '-') then
    begin // if last track in sorted list has negative intercept
        i := 0;

        // advance i to last positive intercept
        while (list[i][1] = '+') do inc(i);

        j := 0;

        while (i < list.Count) do
        begin // move negative intercepts to beginning of list (before positive intercepts)
            list.Move(list.Count - 1, j);
            inc(j);
            inc(i);
        end;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ calculates point to point distance }
function PointToPointDistance(X1, Y1, X2, Y2 : TCoord) : Double;
begin
    Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;
{......................................................................................................................}


{......................................................................................................................}
procedure PresetButtonClicked(Sender : TObject);
begin
    if DebuggingEnabled then ShowInfo(Sender.Name + ' PresetButtonClicked');
    if Sender = ButtonPreset1 then EditDistance.Text      := tPreset1.Text
    else if Sender = ButtonPreset2 then EditDistance.Text := tPreset2.Text
    else if Sender = ButtonPreset3 then EditDistance.Text := tPreset3.Text
    else if Sender = ButtonPreset4 then EditDistance.Text := tPreset4.Text
    else if Sender = ButtonPreset5 then EditDistance.Text := tPreset5.Text
    else if Sender = ButtonPreset6 then EditDistance.Text := tPreset6.Text
    else if Sender = ButtonPreset7 then EditDistance.Text := tPreset7.Text
    else if Sender = ButtonPreset8 then EditDistance.Text := tPreset8.Text;
    calculate(True);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure SelectListItems(const list : TStringList);
var
    Prim : IPCB_Primitive;
    i    : Integer;
begin
    for i := 0 to list.Count - 1 do
    begin
        Prim := list.getObject(i);
        Prim.Selected := True;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ For PrimPad and a midpoint line, provides parallel line that projects through the closest point on the pad on the selected layer. }
function SetupDataFromPad(var PrimPad : IPCB_Pad; ALayer : TV7_Layer; k_midpoint : Double; c_midpoint : TCoord; IsMidVert : Boolean; out k : Double; out c : TCoord; out IsIntVert : Boolean; out X, Y : TCoord) : Boolean;
var
    ViaLayer                    : TLayer;
    TrackLayer                  : TLayer;
    IsVert_track                : Boolean;
    x11, x12, y11, y12          : TCoord;
    k_temp                      : Double;
    c_temp                      : TCoord;
    x_midpoint, y_midpoint      : TCoord;
    X_temp, Y_temp              : TCoord;
    PadOutlinePoly              : IPCB_GeometricPolygon;
    PadOutlineContour           : IPCB_Contour;
    PointIdx                    : Integer;
    LineDist, TempDist          : TCoord;
begin
    Result := False;
    LineDist := 2147483647;

    // Check if PrimPad exists
    if (PrimPad = nil) then exit;

    // Get a geometric polygon exactly outlining the pad on the selected layer. This will let us accommodate pads of any shape.
    PadOutlinePoly := GetPadPoly(PrimPad, 0, ALayer);
    if PadOutlinePoly = nil then exit;
    if PadOutlinePoly.Count = 0 then exit;
    PadOutlineContour := PadOutlinePoly.Contour(0);
    if PadOutlineContour.Count = 0 then exit;

    // after making sure we got a contour with points, we need to find the point with the closest distance to the midpoint line
    for PointIdx := 0 to PadOutlineContour.Count - 1 do
    begin
        // calculate parallel line that intersects this point on the pad
        X_temp := PadOutlineContour.x(PointIdx);
        Y_temp := PadOutlineContour.y(PointIdx);
        GetParallelLine(k_midpoint, c_midpoint, IsMidVert, k_temp, c_temp, IsIntVert, X_temp, Y_temp);

        // closeness based on intercept coordinate
        TempDist := Abs(c_midpoint - c_temp);

        if TempDist < LineDist then
        begin
            LineDist := TempDist;
            k := k_temp;
            c := c_temp;
            X := X_temp;
            Y := Y_temp;
        end;
    end;

    Result := True; // indicates we found a valid contour and processed it
end;
{......................................................................................................................}


{......................................................................................................................}
{ critical function to get normalized line properties. k is slope, c is intercept. }
procedure SetupDataFromTrack(var Prim1 : IPCB_Track; out IsVertical : Boolean; out k : Double; out c, X1, Y1, X2, Y2 : TCoord);
var
    a, b : Integer;
begin
    if Prim1.X1 = Prim1.X2 then
    begin
        IsVertical := True;
        X1         := Prim1.X1;
        X2         := Prim1.X2;
        if Prim1.Y1 < Prim1.Y2 then
        begin
            Y1 := Prim1.Y1;
            Y2 := Prim1.Y2;
        end
        else
        begin
            Y1 := Prim1.Y2;
            Y2 := Prim1.Y1;
        end;
        k := 0;
        c := Prim1.X1;
    end
    else // else track is not vertical
    begin
        if Prim1.X1 < Prim1.X2 then
        begin // track runs left to right
            X1 := Prim1.X1;
            Y1 := Prim1.Y1;
            X2 := Prim1.X2;
            Y2 := Prim1.Y2;
        end
        else // track runs right to left
        begin
            X1 := Prim1.X2;
            Y1 := Prim1.Y2;
            X2 := Prim1.X1;
            Y2 := Prim1.Y1;
        end;
        k := (Y2 - Y1) / (X2 - X1); // calculate track slope

        if (Abs(k) > 20) then
        begin // if slope k > 20, consider track as vertical (this is necessary to prevent Y-intercept number overflow)
            X1 := Prim1.X1;
            X2 := Prim1.X2;
            k := 0;

            // determine which X coordinate rounds best to coords?
            repeat
                a  := X1 mod 10;
                b  := X2 mod 10;
                X1 := X1 div 10;
                X2 := X2 div 10;
            until ((a <> 0) or (b <> 0));

            // if X1 fits best use that, else use X2
            if a = 0 then
            begin
                X1 := Prim1.X1;
                X2 := Prim1.X1;
            end
            else
            begin
                X1 := Prim1.X2;
                X2 := Prim1.X2;
            end;

            // normalize Y direction
            if Prim1.Y1 < Prim1.Y2 then
            begin
                Y1 := Prim1.Y1;
                Y2 := Prim1.Y2;
            end
            else
            begin
                Y1 := Prim1.Y2;
                Y2 := Prim1.Y1;
            end;

            c          := X1; // set intercept of line as X1
            IsVertical := True;
        end
        else // else if track has slope <= 20, calculate intercept point
        begin
            c          := Y1 - k * X1;
            IsVertical := False;
        end;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ Checks if PrimVia and PrimTrack are on the same layer and provides via coordinates, size, and projected line parallel to provided track, and returns True if successful. }
procedure SetupDataFromVia(var PrimVia : IPCB_Via; var PrimTrack : IPCB_Track; out IsIntVert : Boolean; out k : Double; out c, X, Y, size : TCoord);
var
    ViaLayer                : TLayer;
    TrackLayer              : TLayer;
    IsVert1                 : Boolean;
    x11, x12, y11, y12      : TCoord;
    k1, k2                  : Double;
    c1                      : TCoord;

begin
    size := 0;

    // Check if PrimVia and PrimTrack exist
    if (PrimVia = nil) or (PrimTrack = nil) then exit;

    // Set the via coordinates and size
    X := PrimVia.X;
    Y := PrimVia.Y;

    // If PrimVia intersects the same layer as PrimTrack, use pad size on layer, otherwise use hole size
    if PrimVia.IntersectLayer(PrimTrack.Layer) then size := Max(PrimVia.StackSizeOnLayer(PrimTrack.Layer), PrimVia.HoleSize) else size := PrimVia.HoleSize;

    // Get the parameters of the track (only actually using 'k1', 'c1')
    SetupDataFromTrack(PrimTrack, IsVert1, k1, c1, x11, y11, x12, y12);

    // Get the parameters of a parallel line that intersects the via
    GetParallelLine(k1, c1, IsVert1, k, c, IsIntVert, X, Y);

end;
{......................................................................................................................}


{......................................................................................................................}
procedure Start;
var
    status : Integer;
    InitialCheckResult : Integer;
begin
    DebuggingEnabled := False;
    InitialCheckResult := InitialCheck(status);
    if (InitialCheckResult = 3) then PadModeEnabled := True else PadModeEnabled := False;
    if (InitialCheckResult = 2) then ViaModeEnabled := True else ViaModeEnabled := False;

    if status = 0 then
    begin
        FormDistribute.ShowModal;
    end
    else exit;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure StartWithDebug;
var
    status : Integer;
    InitialCheckResult : Integer;
begin
    DebuggingEnabled := True;
    InitialCheckResult := InitialCheck(status);
    if (InitialCheckResult = 3) then PadModeEnabled := True else PadModeEnabled := False;
    if (InitialCheckResult = 2) then ViaModeEnabled := True else ViaModeEnabled := False;

    if status = 0 then
    begin
        FormDistribute.ShowModal;
    end
    else exit;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.ButtonCancelClick(Sender : TObject);
begin
    close;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.ButtonOKClick(Sender : TObject);
begin
    calculate(True);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.ButtonUnitsClick(Sender : TObject);
var
    TempString : string;
begin
    TempString := EditDistance.Text;
    if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    if ButtonUnits.Caption = 'mil' then
    begin
        ButtonUnits.Caption := 'mm';
        EditDistance.Text   := CoordToMMs(milsToCoord(StrToFloat(TempString)));
    end
    else
    begin
        ButtonUnits.Caption := 'mil';
        EditDistance.Text   := CoordToMils(mmsToCoord(StrToFloat(TempString)));
    end;
    EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.ButtonPadViaUnitsClick(Sender : TObject);
var
    TempString : string;
begin
    TempString := EditPadViaClearance.Text;
    if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    if ButtonPadViaUnits.Caption = 'mil' then
    begin
        ButtonPadViaUnits.Caption := 'mm';
        EditPadViaClearance.Text   := CoordToMMs(milsToCoord(StrToFloat(TempString)));
    end
    else
    begin
        ButtonPadViaUnits.Caption := 'mil';
        EditPadViaClearance.Text   := CoordToMils(mmsToCoord(StrToFloat(TempString)));
    end;
    EditPadViaClearance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.CheckBoxTrimEndsClick(Sender : TObject);
begin
    TrimPerpendicular := CheckBoxTrimEnds.Checked;
    if (GetPresetButtonEnable(0) and FormDistribute.Active) then EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.CheckBoxPadViaClearanceClick(Sender : TObject);
begin
    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);
    if (CheckBoxPadViaClearance.Checked and FormDistribute.Active and EditPadViaClearance.Enabled) then EditPadViaClearance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.EditDistanceChange(Sender : TObject);
begin

    if IsStringANum(EditDistance.Text) then
    begin
        EditDistance.Font.Color := clWindowText;
        ButtonOK.Enabled        := True;
        ButtonUnits.Enabled     := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        ButtonUnits.Enabled     := False;
        EditDistance.Font.Color := clRed;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.EditPadViaClearanceChange(Sender : TObject);
begin

    if IsStringANum(Sender.Text) then
    begin
        Sender.Font.Color       := clWindowText;
        ButtonOK.Enabled        := True;
        ButtonPadViaUnits.Enabled  := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        ButtonPadViaUnits.Enabled  := False;
        Sender.Font.Color       := clRed;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.FormDistributeShow(Sender : TObject);
begin
    // set version label
    LabelVersion.Caption := 'v' + ScriptVersion;

    // Set direction control hint
    Application.HintHidePause := 11000; // extend hint show time
    RadioDirections.Hint      := 'FWD: The original direction that the distribute script spreads tracks.' + sLineBreak +
        'CEN/VIA: Redistributes tracks from the center of extents or the midpoint between two vias.' + sLineBreak +
        'CEN/VIA examples: a pair of tracks and no vias will move symmetrically; a single track with two vias will center the track between vias.' + sLineBreak +
        'REV: Will reverse the direction of distribution i.e. what would normally be the last track is instead the first track.';

    // load previous settings from config file
    ConfigFile_Read(ConfigFile_GetPath);

    if Board.SelectecObjectCount = 2 then
    begin
        RadioButtonClearance.Enabled := False;
        RadioButtonCenters.Enabled   := False;
        EnableByValControls(True);
        if not RadioButtonCentersVal.Checked then RadioButtonClearanceVal.Checked := True;
    end
    else if RadioButtonClearance.Checked and (Board.SelectecObjectCount = 3) and (ViaModeEnabled or PadModeEnabled) then
    begin
        CheckBoxPadViaClearance.Checked := True;
        EnableByValControls(False);
    end
    else if (RadioButtonClearanceVal.Checked or RadioButtonCentersVal.Checked) then
    begin
        EnableByValControls(True);
    end
    else
    begin
        EnableByValControls(False);
    end;

    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);

end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonCentersClick(Sender : TObject);
begin
    EnableByValControls(False);
    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonCentersValClick(Sender : TObject);
begin
    EnableByValControls(True);
    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);
    EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonClearanceClick(Sender : TObject);
begin
    EnableByValControls(False);
    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonClearanceValClick(Sender : TObject);
begin
    EnableByValControls(True);
    EnableBetweenPadViaControls(ViaModeEnabled or PadModeEnabled);
    EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioDirectionsClick(Sender : TObject);
begin
    if (GetPresetButtonEnable(0) and FormDistribute.Active) then EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
// programmatically, OnKeyPress fires before OnChange event and "catches" the key press
procedure UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if GetPresetButtonEnable(0) then calculate(True);
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    if IsStringANum(textbox.Text) then
    begin
        if Sender <> EditDistance then EditDistance.Text := textbox.Text;
        ButtonOK.Enabled := True;
    end
    else ButtonOK.Enabled := False;

end;
{......................................................................................................................}


{......................................................................................................................}
{ Function to make sure vias exist on same layer as all tracks }
function ViasExistOnTrackLayer(const dummy : Integer) : Boolean;
var
    i                           : Integer;
    TempString                  : string;
    TrackLayer                  : TLayer;
    PrimTrack                   : IPCB_Primitive;
    PrimVia                     : IPCB_Primitive;
    IsIntVert                   : Boolean;
    TracksOnOneLayer            : Boolean;
    X, Y                        : TCoord;
    size                        : TCoord;
    c1                          : TCoord;
    k1                          : Double;

begin
    Result := False;
    TracksOnOneLayer := True;

    i := 0;
    while i < SortedTracks.Count do
    begin
        PrimTrack :=  SortedTracks.getObject(i);

        if i = 0 then
        begin
            TrackLayer := PrimTrack.Layer;
        end;

        if (PrimTrack.Layer <> TrackLayer) then
        begin
            TracksOnOneLayer := False;
            ShowInfo('Tracks must all be on same layer to use via centering. Falling back to track center.');
            exit;
        end;

        i := i + 3; // advance to next track in sorted list
    end;

    if (TracksOnOneLayer) and (SortedVias.getObject(0).IntersectLayer(TrackLayer)) and (SortedVias.getObject(1).IntersectLayer(TrackLayer)) then
    begin
        Result := True;
    end
    else
    begin
        Result := ConfirmNoYes('One or both of the selected vias do not exist on the tracks'' layer.' + sLineBreak + sLineBreak + 'Do you want to use projected drills for centering?');
    end;
end;
{......................................................................................................................}
