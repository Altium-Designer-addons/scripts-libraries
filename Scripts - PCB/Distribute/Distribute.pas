{ Created by:  Matija Markovic, Petar Perisin }
{ Edited by:   Ryan Rutledge }
{ For documentation see README.md }

var
    Board             : IPCB_Board;
    PresetFilePath    : string;
    PresetList        : TStringList;
    SortedTracks      : TStringList;
    SortedVias        : TStringList;
    TrimPerpendicular : Boolean; // test feature that trims dangling track ends
    DebuggingEnabled  : Boolean; // debug feature will log before-and-after results of tracks modified by script
    DebugFilePath     : string;
    DebugList         : TStringList;
    ViaDebugFilePath  : string;
    ViaDebugList      : TStringList;

const
    NumPresets = 15; // no longer just for presets, also used to save previous state
    ScriptVersion = '1.52';
    ScriptTitle = 'Distribute';
    PresetFileName = 'MyDistributePresets.txt';


procedure About; forward;
procedure AddToDebugListAfter(var Prim : IPCB_Track, LastIntercept : TCoord); forward;
procedure AddToDebugListBefore(var Prim : IPCB_Track, TargetSlope : Double, TargetIntercept : TCoord); forward;
procedure AddToDebugListFirstVia(var Prim1 : IPCB_Via, var Prim2 : IPCB_Track); forward;
procedure AddToDebugListSecondVia(var Prim1 : IPCB_Via, var Prim2 : IPCB_Track, const viaminc : TCoord, const viamaxc : TCoord, const midc : TCoord); forward;
procedure BuildPresetList(var TempPresetList : TStringList); forward;
procedure calculate(LaunchedFromGUI : Boolean); forward;
procedure CompileSortedTracks(const dummy : Integer); forward;
function CompileSortedVias(const dummy : Integer) : Boolean; forward;
function DistributeBackward(startc : TCoord, coef : Double, stepc : TCoord); forward;
function DistributeForward(startc : TCoord, coef : Double, stepc : TCoord); forward;
function DistributeFromCenter(startc : TCoord, coef : Double, stepc : TCoord); forward;
procedure EnableByValControls(NewEnable : Boolean); forward;
procedure FastDistributeByCenterline; forward;
procedure FastDistributeByClearance; forward;
function GetAnotherTrackInPoint(Prim1 : IPCB_Track, X : TCoord, Y : TCoord, out OnFirstPoint : Boolean) : IPCB_Primitive; forward;
function GetEdgeIntercept(const ThisTrackIndex : Integer, const coef : Double, const Reverse : Boolean, out LastIntercept : TCoord); forward;
function GetIntersection(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, k2 : Double, c2 : TCoord, IsPrim2Vert : Boolean, out X : TCoord, out Y : TCoord) : Boolean; forward;
function GetParallelLine(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, out k2 : Double, out c2 : TCoord, out IsPrim2Vert : Boolean, const X : TCoord, const Y : TCoord) : Boolean; forward;
function GetPerpendicularLine(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, out k2 : Double, out c2 : TCoord, out IsPrim2Vert : Boolean, X : TCoord, Y : TCoord) : Boolean; forward;
function GetPresetButtonEnable(const dummy : Integer) : Boolean; forward;
function InitialCheck(var status : Integer) : Integer; forward;
function IsStringANum(Text : string) : Boolean; forward;
procedure LoadPresetListFromFile(const dummy : Integer); forward;
function MoveTrackToIntercept(ThisTrackIndex : Integer, ConnectedTrackOneIndex : Integer, ConnectedTrackTwoIndex : Integer, TrimTrackIndex : Integer, TargetSlope : Double, TargetIntercept : TCoord, coef : Double, Reverse : Boolean, out LastIntercept : TCoord); forward;
procedure PadAndSort(var list : TStringList); forward;
function PointToPointDistance(X1, Y1, X2, Y2) : Double; forward;
procedure PresetButtonClicked(Sender : TObject); forward;
procedure SetupDataFromTrack(var Prim1 : IPCB_Track, out IsVertical : Boolean, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord : out Y2 : TCoord, out k : Double, out c : TCoord); forward;
procedure SetupDataFromVia(var PrimVia : IPCB_Via, var PrimTrack : IPCB_Track, out k : Double, out c : TCoord, out IsIntVert : Boolean, out X : TCoord, out Y : TCoord, out size : TCoord); forward;
procedure Start; forward;
procedure StartWithDebug; forward;
procedure TFormDistribute.ButtonCancelClick(Sender : TObject); forward;
procedure TFormDistribute.ButtonOKClick(Sender : TObject); forward;
procedure TFormDistribute.ButtonUnitsClick(Sender : TObject); forward;
procedure TFormDistribute.CheckBoxTrimEndsClick(Sender : TObject); forward;
procedure TFormDistribute.EditDistanceChange(Sender : TObject); forward;
procedure TFormDistribute.FormDistributeShow(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonCentersClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonCentersValClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonClearanceClick(Sender : TObject); forward;
procedure TFormDistribute.RadioButtonClearanceValClick(Sender : TObject); forward;
procedure TFormDistribute.RadioDirectionsClick(Sender : TObject); forward;
procedure UserKeyPress(Sender : TObject, var Key : Char); forward;
procedure ValidateOnChange(Sender : TObject); forward;
function ViasExistOnTrackLayer(const dummy : Integer) : Boolean; forward;


{......................................................................................................................}
procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak + sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak + sLineBreak +
        'Settings save location:' + sLineBreak +
        ClientAPI_SpecialFolder_AltiumApplicationData + '\' + PresetFileName;

    ShowInfo(MsgText, 'About');
end;
{......................................................................................................................}


{......................................................................................................................}
{ debugging function }
procedure AddToDebugListAfter(var Prim : IPCB_Track, LastIntercept : TCoord);
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
    SetupDataFromTrack(Prim, IsVert, X1, Y1, X2, Y2, k, c);
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
procedure AddToDebugListBefore(var Prim : IPCB_Track, TargetSlope : Double, TargetIntercept : TCoord);
var
    X1, Y1, X2, Y2  : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    TempDebugList   : TStringList;

begin
    TempDebugList := CreateObject(TStringList);
    SetupDataFromTrack(Prim, IsVert, X1, Y1, X2, Y2, k, c);
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
procedure AddToDebugListFirstVia(var Prim1 : IPCB_Via, var Prim2 : IPCB_Track);
var
    X1, Y1          : TCoord;
    k               : Double;
    c               : TCoord;
    IsVert          : Boolean;
    ViaSize         : TCoord;
    TempDebugList   : TStringList;

begin
    TempDebugList := CreateObject(TStringList);
    SetupDataFromVia(Prim1, Prim2, k, c, IsVert, X1, Y1, ViaSize);
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
procedure AddToDebugListSecondVia(var Prim1 : IPCB_Via, var Prim2 : IPCB_Track, const viaminc : TCoord, const viamaxc : TCoord, const midc : TCoord);
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
    SetupDataFromVia(Prim1, Prim2, k, c, IsVert, X1, Y1, ViaSize);
    TempDebugList.Append(IntToStr(X1));
    TempDebugList.Append(IntToStr(Y1));
    TempDebugList.Append(IntToStr(ViaSize));
    TempDebugList.Append(FloatToStr(k));
    TempDebugList.Append(IntToStr(c));
    SetupDataFromTrack(Prim2, IsVert, X1, Y1, X2, Y2, trackslope, trackintercept);
    TempDebugList.Append(FloatToStr(trackslope));
    TempDebugList.Append(IntToStr(trackintercept));
    TempDebugList.Append(IntToStr(viaminc));
    TempDebugList.Append(IntToStr(viamaxc));
    TempDebugList.Append(IntToStr(midc));
    ViaDebugList.Append(TempDebugList.CommaText);

end;
{......................................................................................................................}


{......................................................................................................................}
{ function to populate a TStringList with preset values }
procedure BuildPresetList(var TempPresetList : TStringList);
begin
    TempPresetList.Clear;
    TempPresetList.Add(EditDistance.Text);
    TempPresetList.Add(tPreset1.Text);
    TempPresetList.Add(tPreset2.Text);
    TempPresetList.Add(tPreset3.Text);
    TempPresetList.Add(tPreset4.Text);
    TempPresetList.Add(tPreset5.Text);
    TempPresetList.Add(tPreset6.Text);
    TempPresetList.Add(tPreset7.Text);
    TempPresetList.Add(tPreset8.Text);
    TempPresetList.Add(RadioDirections.ItemIndex);
    TempPresetList.Add(RadioButtonClearance.Checked);
    TempPresetList.Add(RadioButtonCenters.Checked);
    TempPresetList.Add(RadioButtonClearanceVal.Checked);
    TempPresetList.Add(RadioButtonCentersVal.Checked);
    TempPresetList.Add(CheckBoxTrimEnds.Checked);
end;
{......................................................................................................................}


{......................................................................................................................}
{ main procedure to distribute tracks }
procedure calculate(LaunchedFromGUI : Boolean);
var
    i, j                                                    : Integer;
    k1, k2                                                  : Double;
    c1, c2, minc, midc, maxc, stepc, cFromWidths            : TCoord;
    viaminc, viamaxc, viasize                               : TCoord;
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
    TempPresetList                                          : TStringList;

begin
    if (Board <> PCBServer.GetCurrentPCBBoard) then
    begin
        ShowError('Please start script using START procedure');
        close;
        exit;
    end;

    if not LaunchedFromGUI then TrimPerpendicular := False; // assume don't trim if GUI wasn't used

    Board.NewUndo;

    // Start undo
    PCBServer.PreProcess;
    try

        // compile list of sorted tracks
        CompileSortedTracks(0);

        // seeks min and max c i.e. range of intercept values (trazi min i max C tj grnice sirenja   vodova)
        // moved intercept stats here to operate on sorted data instead of original selection. doing before sorting could cause issues with distribute by clearance if first selected track is a different width.
        Prim1 := SortedTracks.getObject(0);
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

        minc        := c1;
        maxc        := c1;

        if IsVert1 then coef := 1.0 // if first track has been coerced to vertical, coef needs to be 1 for later width compensation
        else coef        := cos(arctan(k1)); // y-intercept coefficient based on slope (i.e. how much intercept shifts for a perpendicular shift of the track)

        cFromWidths := Prim1.Width / (2 * coef); // start cFromWidths with half of the first track's width

        // calculate extents of intercept values and sum of track widths in intercept units
        for i := 1 to SortedTracks.Count - 1 do
        begin
            Prim1 := SortedTracks.getObject(i);
            SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

            if (minc > c2) then minc := c2;
            if (maxc < c2) then maxc := c2;

            cFromWidths := cFromWidths + Prim1.Width / coef; // add subsequent track widths
        end;

        cFromWidths := cFromWidths - Prim1.Width / (2 * coef); // subtract half of last track's width

        midc := (Round(minc + maxc)) div 2; // midline between the outer pair of tracks

        if DebuggingEnabled then
        begin
            ViaDebugFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\DistributeScriptViaDebug.csv';
            ViaDebugList := CreateObject(TStringList);
            ViaDebugList.Append('Via1 X,Via1 Y,Via1 size,Via1 slope,Via1 intercept,Via2 X,Via2 Y,Via2 size,Via2 slope,Via2 intercept,ref track slope,ref track intercept, viaminc, viamaxc, midc');
        end;

        // KEY BEHAVIOR: in "CEN/VIA" distribute direction mode, via midpoint will take priority. Note that two stacked vias can be used to force a trajectory.
        // compile list of sorted vias (MUST come after list of sorted tracks) and calculate midpoint if successful
        // calculate midpoint accounting for via size so that differently-sized vias don't bias the result
        if CompileSortedVias(0) and ViasExistOnTrackLayer(0) then
        begin
            Prim2 := SortedTracks.getObject(0);         // get first track from the SortedTracks list

            Prim1 := SortedVias.getObject(0);           // get first via from the SortedVias list
            SetupDataFromVia(Prim1, Prim2, k1, c1, IsVert1, x11, y11, viasize);
            viaminc := c1 + viasize / (2 * coef);          // add via pad radius

            if DebuggingEnabled then AddToDebugListFirstVia(Prim1, Prim2);

            Prim1 := SortedVias.getObject(1);           // get second via from the SortedVias list
            SetupDataFromVia(Prim1, Prim2, k1, c1, IsVert1, x11, y11, viasize);
            viamaxc := c1 - viasize / (2 * coef);          // subtract via pad radius

            midc := (Round(viaminc + viamaxc)) div 2;   // midline between the vias, accounting for their size

            if DebuggingEnabled then AddToDebugListSecondVia(Prim1, Prim2, viaminc, viamaxc, midc);
        end;

        {
            // Test case if all is good until now
            for i := 0 to SortedTracks.Count - 1 do
            begin
            Prim1 := SortedTracks.GetObject(i);
            Prim1.Selected := False;

            ShowInfo(SortedTracks[i]);

            Prim1.Selected := True;
            end;
        }

        // Add connected tracks to the track list, or pad the list if there is no track connected to a given end
        // (Dio koji puni object listu sa susjednim trackovima ako je track onda u listu ide YES a noj stavlja NO)
        i := 0;
        ResetParameters;
        AddStringParameter('Scope', 'All');
        RunProcess('PCB:DeSelect');
        while i < SortedTracks.Count do
        begin
            Prim1 := SortedTracks.getObject(i);
            SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

            // look for a track (call it track 1a) that is connected to this track's first point and insert it after this track in the list (provjera za prvu tocku i umece 1a liniju ispod 1 linije tracka)
            Prim2 := GetAnotherTrackInPoint(Prim1, Prim1.X1, Prim1.Y1, IsFirstPoint);

            if (Prim2 = nil) then SortedTracks.Insert(i + 1, '0') // no connected track so insert placeholder
            else
            begin // there is a connected track, validate it
                SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);
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
                SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);

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
            DistributeForward(minc, coef, stepc);
        end
        else if RadioButtonClearance.Checked then
        begin
            stepc := (maxc - minc - cFromWidths) / ((SortedTracks.Count / 3) - 1); // intercept increment is same as above but subtracting sum of track widths from intercept extents
            DistributeForward(minc, coef, stepc);
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
                0 : DistributeForward(minc, coef, stepc);
                1 : DistributeFromCenter(midc, coef, stepc);
                2 : DistributeBackward(maxc, coef, stepc);
                else DistributeForward(minc, coef, stepc);
            end;
        end;

        if LaunchedFromGUI then
        begin
            // build list of currect preset values
            TempPresetList := CreateObject(TStringList);
            BuildPresetList(TempPresetList);
            if TempPresetList.Equals(PresetList) then
            begin
                // presets match saved list so do nothing
            end
            else
            begin
                // save new list to MyDistributePresets.txt
                TempPresetList.SaveToFile(PresetFilePath);
            end;

        end;

        if DebuggingEnabled then
        begin
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
{ CompileSortedTracks }
procedure CompileSortedTracks(const dummy : Integer);
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
            SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

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
function CompileSortedVias(const dummy : Integer) : Boolean;
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
            SetupDataFromVia(PrimVia, PrimTrack, k1, c1, IsIntVert, X, Y, size);

            TempString := IntToStr(c1);

            if c1 > 0 then TempString := '+' + TempString;

            SortedVias.AddObject(TempString, PrimVia); // store track object using intercept as key
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
function DistributeBackward(startc : TCoord, coef : Double, stepc : TCoord);
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
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k, TargetIntercept);

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

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, True, LastIntercept);

        i := i - 3; // jump back to the preceding track in the sorted list
        inc(j); // increment track step counter
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function DistributeForward(startc : TCoord, coef : Double, stepc : TCoord);
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
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k, TargetIntercept);

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

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, False, LastIntercept);

        i := i + 3; // advance to next track in sorted list
        inc(j); // increment track step counter
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
function DistributeFromCenter(startc : TCoord, coef : Double, stepc : TCoord);
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
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k, TargetIntercept);

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

        MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, False, LastIntercept);

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
        SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k, TargetIntercept);

        if i = TrimTrackIndex then
        begin
            TargetIntercept := SplitIntercept;
            GetEdgeIntercept(i, coef, True, LastIntercept);
        end
        else
        begin // calculate target intercepts
            if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then TargetIntercept := SplitIntercept - j * stepc // if using centers, use step size directly
            else TargetIntercept := LastIntercept - stepc - Prim1.Width / (2 * coef); // if using clearances, use previous track intercept plus step size plus half of this track's width

            MoveTrackToIntercept(i, i + 1, i + 2, TrimTrackIndex, TargetSlope, TargetIntercept, coef, True, LastIntercept);
        end;

        i := i - 3; // jump back to the preceding track in the sorted list
        inc(j); // increment track step counter
    end;
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
    ViaCount : Integer;
begin
    ViaCount := InitialCheck(status);
    if status = 0 then
    begin
        if (ViaCount = 2) and (Board.SelectecObjectCount = 3) then
        begin
            // two vias and one track selected, do track centering instead
            DebuggingEnabled := False;
            RadioButtonCenters.Checked      := False;
            RadioButtonClearance.Checked    := False;
            RadioDirections.ItemIndex       := 1;
            calculate(False);
        end
        else
        begin
            DebuggingEnabled := False;
            RadioButtonCenters.Checked   := False;
            RadioButtonClearance.Checked := True;
            calculate(False);
        end;
    end
    else exit;
end;
{......................................................................................................................}


{......................................................................................................................}
{ checks if there is another track connected to the given endpoint of this track, and which of its ends is connected }
function GetAnotherTrackInPoint(Prim1 : IPCB_Track, X : TCoord, Y : TCoord, out OnFirstPoint : Boolean) : IPCB_Primitive;
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
function GetEdgeIntercept(const ThisTrackIndex : Integer, const coef : Double, const Reverse : Boolean, out LastIntercept : TCoord);
var
    k1                      : Double;   // k1 is throwaway
    c1                      : TCoord;
    IsVert1                 : Boolean;  // isVert1 is throwaway
    x11, x12, y11, y12      : TCoord;   // throwaway
    Prim1                   : IPCB_Primitive;

begin
    Prim1 := SortedTracks.getObject(ThisTrackIndex);
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    if Reverse then LastIntercept := c1 - (Prim1.Width / (2 * coef)) // subtract half of current track width for next pass (intercept at moved track edge)
    else LastIntercept            := c1 + (Prim1.Width / (2 * coef)); // add half of current track width for next pass (intercept at moved track edge)

end;
{......................................................................................................................}


{......................................................................................................................}
{ function to test if two tracks have an intercept point (tracks are not parallel) }
function GetIntersection(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, k2 : Double, c2 : TCoord, IsPrim2Vert : Boolean, out X : TCoord, out Y : TCoord) : Boolean;
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
{function to create slope and intercept for virtual line parallel to an ordinate line and passing through a point (k1,c1,IsPrim1Vert are for ordinate line)}
function GetParallelLine(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, out k2 : Double, out c2 : TCoord, out IsPrim2Vert : Boolean, const X : TCoord, const Y : TCoord) : Boolean;
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
function GetPerpendicularLine(k1 : Double, c1 : TCoord, IsPrim1Vert : Boolean, out k2 : Double, out c2 : TCoord, out IsPrim2Vert : Boolean, X : TCoord, Y : TCoord) : Boolean;
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
    ViaCount           : Integer;
begin
    status := 0; // clear result status
    Result := 0;

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    ViaCount := 0;

    // Count vias without deselecting them (assume vias should be in a net
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if ((Prim1.ObjectId = eViaObject) and (Prim1.InNet)) then ViaCount := ViaCount + 1;
    end;

    // If there are greater or less than 2 vias, deselect all vias
    if (ViaCount <> 2) then
    begin
        // need to deselect anything that isn't an electrical track as these will cause errors elsewhere
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if ((Prim1.ObjectId <> eTrackObject) or (not Prim1.InNet)) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end
    else
    begin
        // need to deselect anything that isn't an electrical track or the two vias as these will cause errors elsewhere)
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if (((Prim1.ObjectId <> eTrackObject) and (Prim1.ObjectId <> eViaObject)) or (not Prim1.InNet)) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end;


    // make sure there are enough objects selected to operate upon
    if ((ViaCount = 2) and (Board.SelectecObjectCount < 3)) or ((ViaCount <> 2) and (Board.SelectecObjectCount < 2)) then
    begin
        if DebuggingEnabled and (ViaCount <> 2) then
        begin
            ShowError('Select at least 2 tracks or ONE pair of vias and a track that all belong to nets.' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ViaCount: ' + IntToStr(ViaCount) + sLineBreak +
                        'Selected Track Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else if DebuggingEnabled then
        begin
            ShowError('Select at least 2 tracks or ONE pair of vias and a track that all belong to nets.' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ViaCount: ' + IntToStr(ViaCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else ShowError('Select at least 2 tracks or ONE pair of vias and a track that all belong to nets.');

        status := 1;
        exit;
    end;

    // Since we're allowing vias now, we should instead iterate until reaching the first track
    // Find the first track and store its index in 'i'
    i := 0;
    while (Board.SelectecObject[i].ObjectId <> eTrackObject) and (i < Board.SelectecObjectCount - 1) do
    begin
        i := i + 1;
    end;

    // Set up the initial track data
    Prim1 := Board.SelectecObject[i];
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    // Check that all selected tracks are parallel with the first one, skipping vias
    i := i + 1;
    while i < Board.SelectecObjectCount do
    begin
        Prim1 := Board.SelectecObject[i];
        if Prim1.ObjectId = eTrackObject then
        begin
            SetupDataFromTrack(Prim1, IsVert2, x21, y21, x22, y22, k2, c2);

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
        i := i + 1;
    end;
    Result := ViaCount;
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
{ function to load preset list from file }
procedure LoadPresetListFromFile(const dummy : Integer);
begin
    // default file name is MyDistributePresets.txt
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\' + PresetFileName;
    PresetList     := CreateObject(TStringList);
    if FileExists(PresetFilePath) then
    begin
        // ShowInfo('Loading presets from ' + PresetFilePath);
        PresetList.LoadFromFile(PresetFilePath); // load presets from file if it exists

        case PresetList.Count of
            14 : PresetList.Add(CheckBoxTrimEnds.Checked); // PresetList[14] (14-element PresetList implies v1.4)
            NumPresets :
                begin
                    // do nothing
                end
            else  // if PresetList.Count < NumPresets then PresetList file exists but count is short, just regenerate preset file from defaults
                begin
                    // ShowInfo(PresetFilePath + ' exists but is not the correct length. Defaults will be used.');
                    BuildPresetList(PresetList);
                    PresetList.SaveToFile(PresetFilePath);
                end;
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
        EditDistance.Text               := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    end
    else
    begin // if preset file didn't exist at all, create from defaults
        // ShowInfo(PresetFilePath + ' does not exist.');
        BuildPresetList(PresetList);
        PresetList.SaveToFile(PresetFilePath);
    end;

end;
{......................................................................................................................}


{......................................................................................................................}
{ bundles up track move functionality that is common among all modes }
function MoveTrackToIntercept(ThisTrackIndex : Integer, ConnectedTrackOneIndex : Integer, ConnectedTrackTwoIndex : Integer, TrimTrackIndex : Integer, TargetSlope : Double,
    TargetIntercept : TCoord, coef : Double, Reverse : Boolean, out LastIntercept : TCoord);
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
    SetupDataFromTrack(Prim1, IsVert1, x11, y11, x12, y12, k1, c1);

    if TrimPerpendicular then
    begin
        Prim0 := SortedTracks.getObject(TrimTrackIndex);
        SetupDataFromTrack(Prim0, IsVert0, x01, y01, x02, y02, k0, c0);
    end;

    if DebuggingEnabled then AddToDebugListBefore(Prim1, TargetSlope, TargetIntercept);

    Prim1.BeginModify;

    Prim2 := SortedTracks.getObject(ConnectedTrackOneIndex); // track connected to first end of the moving track
    if (SortedTracks[ConnectedTrackOneIndex] <> '0') then
    begin // if there *is* a connected track on this end
        SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);
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
        SetupDataFromTrack(Prim2, IsVert2, x21, y21, x22, y22, k2, c2);

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
function PointToPointDistance(X1, Y1, X2, Y2) : Double;
begin
    Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;
{......................................................................................................................}


{......................................................................................................................}
procedure PresetButtonClicked(Sender : TObject);
begin
    // ShowInfo('PresetButtonClicked');
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
{ critical function to get normalized line properties. k is slope, c is intercept. }
procedure SetupDataFromTrack(var Prim1 : IPCB_Track, out IsVertical : Boolean, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord : out Y2 : TCoord, out k : Double, out c : TCoord);
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
            c          := Y1 - k * X1; //
            IsVertical := False;
        end;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
{ Checks if PrimVia and PrimTrack are on the same layer and provides via coordinates, size, and projected line parallel to provided track, and returns True if successful. }
procedure SetupDataFromVia(var PrimVia : IPCB_Via, var PrimTrack : IPCB_Track, out k : Double, out c : TCoord, out IsIntVert : Boolean, out X : TCoord, out Y : TCoord, out size : TCoord);
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

    // Check if PrimVia intersects the same layer as PrimTrack
    if PrimVia.IntersectLayer(PrimTrack.Layer) then
    begin
        // Set the via coordinates and size
        X := PrimVia.X;
        Y := PrimVia.Y;
        size := Max(PrimVia.StackSizeOnLayer(PrimTrack.Layer), PrimVia.HoleSize); // size should be assumed to be at least HoleSize
    end;

        // Get the parameters of the track (only actually using 'k1', 'c1')
        SetupDataFromTrack(PrimTrack, IsVert1, x11, y11, x12, y12, k1, c1);

        // Get the parameters of the parallel line
        GetParallelLine(k1, c1, IsVert1, k, c, IsIntVert, X, Y);

end;
{......................................................................................................................}


{......................................................................................................................}
procedure Start;
var
    status : Integer;
begin
    DebuggingEnabled := False;
    InitialCheck(status);
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
begin
    DebuggingEnabled := True;
    InitialCheck(status);
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
procedure TFormDistribute.CheckBoxTrimEndsClick(Sender : TObject);
begin
    TrimPerpendicular := CheckBoxTrimEnds.Checked;
    if (GetPresetButtonEnable(0) and FormDistribute.Active) then EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.EditDistanceChange(Sender : TObject);
begin

    if IsStringANum(EditDistance.Text) then
    begin
        EditDistance.Font.Color := clWindowText;
        ButtonOK.Enabled        := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        EditDistance.Font.Color := clRed;
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

    // read presets from file
    LoadPresetListFromFile(0);

    if Board.SelectecObjectCount = 2 then
    begin
        RadioButtonClearance.Enabled := False;
        RadioButtonCenters.Enabled   := False;
        EnableByValControls(True);
        if not RadioButtonCentersVal.Checked then RadioButtonClearanceVal.Checked := True;
    end
    else if (RadioButtonClearanceVal.Checked or RadioButtonCentersVal.Checked) then
    begin
        EnableByValControls(True);
    end
    else EnableByValControls(False);

end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonCentersClick(Sender : TObject);
begin
    EnableByValControls(False);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonCentersValClick(Sender : TObject);
begin
    EnableByValControls(True);
    EditDistance.SetFocus;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonClearanceClick(Sender : TObject);
begin
    EnableByValControls(False);
end;
{......................................................................................................................}


{......................................................................................................................}
procedure TFormDistribute.RadioButtonClearanceValClick(Sender : TObject);
begin
    EnableByValControls(True);
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
procedure UserKeyPress(Sender : TObject, var Key : Char);
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
    // ShowInfo(textbox.Text);
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
        ShowError('One or both of the selected vias do not exist on the tracks'' layer.');
    end;
end;
{......................................................................................................................}
