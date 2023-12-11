{..............................................................................}
{ Summary   This Script creates fillet on selected tracks                      }
{ Created by:   Petar Perisin                                                  }
{ Edited by:    Corey Beyer, Brett Miller, Ryan Rutledge                       }
{..............................................................................}

const
    cScriptVersion      = '2.00';
    cScriptTitle        = 'Fillet';
    cConfigFileName     = 'MyFilletSettings.ini';
    cDEBUGLEVEL         = 2;
    eModeRadius         = 0;
    eModeRatio          = 1;
    eArcModeAbsolute    = 0;
    eArcModeRelative    = 1;
    eRoundingModeOff    = 0;
    eRoundingModeOn     = 1;
    eGlossModeOff       = 0;
    eGlossModeOn        = 1;
    eArcSortNormal      = 0;
    eArcSortReverse     = 1;
    cSectorAngleDegTol  = 1000; // coincident arc sector alignment tolerance in degrees (set to 1000 to use full sectors)
    sLineBreak2         = sLineBreak + sLineBreak;


var
    iDebugLevel         : Integer;
    Board               : IPCB_Board;
    PresetFilePath      : String;
    PresetList          : TStringList;
    SelectionMemory     : TStringList;
    TrackDictionary     : TStringList;
    StubRemovalList     : TInterfaceList;
    Mode, ArcMode       : Integer;
    RoundingMode        : Integer;
    GlossMode           : Integer;
    ArcSortOrder        : Integer;
    Radius              : Integer;
    Ratio               : Double;
    Units               : TUnit;


procedure _Start; forward;
procedure About; forward;
function CoordToDisplayStr(coords : TCoord) : String; forward;
function CoordToDisplayX(coords : TCoord) : String; forward;
function CoordToDisplayY(coords : TCoord) : String; forward;
function CoordToMils2(coords : TCoord) : Double; forward;
function GetArcAngleBetweenTracks(FirstTrack, SecondTrack : IPCB_Track) : Double; forward;
function GetCommonPointsForTracks(FirstTrack, SecondTrack : IPCB_Primitive; out FirstTrackEnd, SecondTrackEnd : Integer; RoundToDigit : Integer = 0) : Boolean; forward;
procedure Inspect_IPCB_Track(var Track : IPCB_Track; const MyLabel : string = ''); forward;
function IsStringANum(Text : string; AllowNegative : Boolean = False) : Boolean; forward;
function AngleNormalize(Angle : Double; minVal : Double = 0; maxVal : Double = 999.999) : Double; forward;
function ArcOrderCompare(Arc1, Arc2 : IPCB_Arc) : Integer; forward;
function ArcSectorsOverlapAndAlign(Arc1, Arc2 : IPCB_Arc; SectorAngle : Double = 999.999) : Integer; forward;
procedure ArcAnglesCalculate(AnArc : IPCB_Arc; out StartAngle, EndAngle, HalfAngle : Double); forward;
procedure CoincidentArcCleanup(ListOfPrimitives : TStringList); forward;
procedure CoincidentArcGroupGloss(ArcDictionary : TStringList); forward;
function ConfigFile_GetPath(dummy : String = '') : String; forward;
procedure ConfigFile_Read(AFileName : String); forward;
procedure ConfigFile_Write(AFileName : String); forward;
procedure ConfigFile_LegacyRead(const dummy : Integer = 0); forward;
procedure ConvertFormUnits(const dummy : Integer); forward;
function PointToPointDistance(X1, Y1, X2, Y2 : TCoord) : Double; forward;
function PointToTrackDistance(ATrack : IPCB_Track; X, Y : TCoord) : TCoord; forward;
procedure RemoveTrackUndoSafe(ATrack : IPCB_Track); forward;
procedure ReselectPrimitives(ListOfPrimitives : TStringList); forward;
function RoundCoords(coords : TCoord; round_mult : Double; UserUnits : TUnit = -1) : TCoord; forward;
procedure CreateArcBetweenTracks(ArcRadius : Integer; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive); forward;
function GetArcOrTrackOnEnd(FirstPrim : IPCB_Primitive; const FirstPrimEnd : Integer; out SecondPrimEnd : Integer; OnlySelected : Boolean = True) : IPCB_Primitive; forward;
procedure GetIntersectingTrackLengths(FirstTrack, SecondTrack : IPCB_Primitive; var FirstTrackLength, SecondTrackLength : TCoord); forward;
procedure GetIntersectionPointBetweenTracks(FirstTrack, SecondTrack : IPCB_Primitive; var Xp, Yp : TCoord); forward;
procedure ExtendTracksOverArc(AnArc : IPCB_Primitive; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive); forward;
procedure ExtendTrackToPoint(ATrack : IPCB_Primitive; Xp, Yp: TCoord); forward;
function DebugLevelStr(dummy : String = '') : String; forward;
procedure DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug'); forward;
function DetermineArcRadiusFromArc(AnArc : IPCB_Primitive; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive) : Integer; forward;
function DetermineArcRadiusFromTracks(FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive) : Integer; forward;
function DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack : IPCB_Primitive; Projected : Boolean = False) : TCoord; forward;
function GetProjectedTrackLength(ATrack : IPCB_Track; OnlySelected : Boolean = True) : TCoord; forward;
function GetProjectedInterceptingTrackAtEnd(ATrack : IPCB_Track; const ATrackEnd : Integer; OnlySelected : Boolean = True) : IPCB_Track; forward;
function GetCommonPointForTrackWithArc(ATrack : IPCB_Primitive; AnArc : IPCB_Primitive; RoundToDigit : Integer = 0) : Integer; forward;
procedure DoFillets(dummy : Integer = 0); forward;
procedure Start; forward;
function IsPointWithinArcAngles(AnArc : IPCB_Arc; X, Y : TCoord; SectorAngle : Double = 999.999) : Boolean; forward;
function IsPointWithinArcSector(AnArc : IPCB_Arc; X, Y : TCoord; SectorAngle : Double = 999.999) : Boolean; forward;
function IsPointWithinRadius(AnArc : IPCB_Arc; X, Y : TCoord) : Boolean; forward;
procedure ArcDictionaryAdd(var DictionaryList : TStringList; AnArc : IPCB_Arc); forward;
function ArcDictionaryDebug(const DictionaryList : TStringList) : String; forward;
procedure ArcDictionaryInitialize(out DictionaryList : TStringList); forward;
procedure ArcDictionaryInsert(var DictionaryList : TStringList; AnArc : IPCB_Arc; idx : Integer); forward;
function ArcDictionaryItem(const DictionaryList : TStringList; const idx : Integer) : IPCB_Arc; forward;
function ArcDictionaryPop(var DictionaryList : TStringList; idx : Integer) : IPCB_Arc; forward;
procedure ArcDictionarySort(var DictionaryList : TStringList; ReverseOrder : Boolean = False); forward;
function ArcDictionarySwap(var DictionaryList : TStringList; Arc1, Arc2 : IPCB_Arc) : Boolean; forward;
procedure TrackDictionaryAdd(var DictionaryList : TStringList; ATrack : IPCB_Track); forward;
procedure TrackDictionaryCompileList(out DictionaryList : TStringList); forward;
function TrackDictionaryDebug(const DictionaryList : TStringList) : String; forward;
function TrackDictionaryGetPointToTrackDistance(const DictionaryList : TStringList; ATrack : IPCB_Track; X, Y : TCoord) : TCoord; forward;
function TrackDictionaryGetTrackCoords(const DictionaryList : TStringList; ATrack : IPCB_Track; out X1, Y1, X2, Y2 : TCoord) : Boolean; forward;
procedure TrackDictionaryInitialize(out DictionaryList : TStringList); forward;
function TrackDictionaryItem(const DictionaryList : TStringList; idx : Integer) : IPCB_Track; forward;
function TrackDictionaryValueFormatString(const ATrack : IPCB_Track) : String; forward;
procedure TrackDictionaryValueParse(const ValueString : String; out X1, Y1, X2, Y2 : TCoord); forward;
procedure TFormFillet.FormFilletShow(Sender: TObject); forward;
procedure TFormFillet.FormFilletClose(Sender: TObject; var Action: TCloseAction); forward;
procedure TFormFillet.ButtonCancelClick(Sender: TObject); forward;
procedure TFormFillet.RadioUnitsRatioClick(Sender: TObject); forward;
procedure TFormFillet.ValidateOnChange(Sender : TObject); forward;
procedure TFormFillet.UserKeyPress(Sender: TObject; var Key: Char); forward;
procedure TFormFillet.ButtonOKClick(Sender: TObject); forward;
procedure TFormFillet.LabelVersionClick(Sender : TObject); forward;
procedure TFormFillet.PresetButtonClicked(Sender: TObject); forward;


procedure _Start;
begin
    Start;
end;


procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "Start" to apply fillets between connected tracks' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/Fillet' + sLineBreak +
        sLineBreak +
        'Settings save location:' + sLineBreak +
        ConfigFile_GetPath;

    ShowInfo(MsgText, 'About');
end;


function CoordToDisplayStr(coords : TCoord) : String;
const
    MAXINT = 2147483647;
    MININT = -2147483647;
begin
    if coords < MININT then coords := MININT
    else if coords > MAXINT then coords := MAXINT;

    //result := CoordUnitToString(coords, Board.DisplayUnit xor 1); // only uses 2 digits of precision for metric

    if (Board.DisplayUnit xor 1) = eImperial then Result := FloatToStr(CoordToMils(coords)) + 'mil'
    else Result := FloatToStr(CoordToMMs(coords)) + 'mm';
end;

function CoordToDisplayX(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.XOrigin, Board.DisplayUnit xor 1);
end;

function CoordToDisplayY(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.YOrigin, Board.DisplayUnit xor 1);
end;

function CoordToMils2(coords : TCoord) : Double;
begin
    //Result := CoordToMils2(coords) + (coords mod 10) / 10000;
    Result := coords / 10000;
end;


function GetArcAngleBetweenTracks(FirstTrack, SecondTrack : IPCB_Track) : Double;
var
    dx1, dy1, dx2, dy2      : TCoord;
    FirstAngle, SecondAngle : Double;
    ArcStartAngle, ArcEndAngle : Double;
    DebugString : String;
    DebugCheck : Boolean;
    ArcAngle : Double;
    BadAngle : Boolean;
    T1P1Dist, T1P2Dist, T2P1Dist, T2P2Dist : Double;
    Xp, Yp: TCoord; // coordinates of intersection point
begin
    GetIntersectionPointBetweenTracks(FirstTrack, SecondTrack, Xp, Yp);

    // Determine the deltas for the first track
    T1P1Dist := PointToPointDistance(FirstTrack.x1, FirstTrack.y1, Xp, Yp);
    T1P2Dist := PointToPointDistance(FirstTrack.x2, FirstTrack.y2, Xp, Yp);
    if T1P1Dist < T1P2Dist then
    begin // first point on first track is closest to intersection
        dx1 := FirstTrack.X2 - FirstTrack.X1;
        dy1 := FirstTrack.Y2 - FirstTrack.Y1;
    end
    else
    begin
        dx1 := FirstTrack.X1 - FirstTrack.X2;
        dy1 := FirstTrack.Y1 - FirstTrack.Y2;
    end;

    // Determine the deltas for the second track
    T2P1Dist := PointToPointDistance(SecondTrack.x1, SecondTrack.y1, Xp, Yp);
    T2P2Dist := PointToPointDistance(SecondTrack.x2, SecondTrack.y2, Xp, Yp);
    if T2P1Dist < T2P2Dist then
    begin // first point on second track is closest to intersection
        dx2 := SecondTrack.X2 - SecondTrack.X1;
        dy2 := SecondTrack.Y2 - SecondTrack.Y1;
    end
    else
    begin
        dx2 := SecondTrack.X1 - SecondTrack.X2;
        dy2 := SecondTrack.Y1 - SecondTrack.Y2;
    end;

    // Calculate the angles of the tracks
    FirstAngle := ArcTan2(dy1, dx1);
    SecondAngle := ArcTan2(dy2, dx2);

    // Normalize the track angles to be within the range [0, 2*Pi]
    if FirstAngle < 0 then FirstAngle := FirstAngle + 2 * Pi;
    if FirstAngle >= (2 * Pi) then FirstAngle := FirstAngle - 2 * Pi;

    if SecondAngle < 0 then SecondAngle := SecondAngle + 2 * Pi;
    if SecondAngle >= (2 * Pi) then SecondAngle := SecondAngle - 2 * Pi;

    ArcStartAngle := (FirstAngle + (Pi / 2));
    ArcEndAngle := (SecondAngle - (Pi / 2));
    ArcAngle := ArcEndAngle - ArcStartAngle;
    if ArcAngle < 0 then ArcAngle := ArcAngle + 2 * Pi;
    BadAngle := (ArcAngle < 0) or (ArcAngle > Pi);

    //  if arc angle isn't within range [0, Pi] then swap the arc ends in calc
    if BadAngle then
    begin
        ArcStartAngle := (SecondAngle + (Pi / 2));
        ArcEndAngle := (FirstAngle - (Pi / 2));
        ArcAngle := ArcEndAngle - ArcStartAngle;
        if ArcAngle < 0 then ArcAngle := ArcAngle + 2 * Pi;
        BadAngle := (ArcAngle < 0) or (ArcAngle > Pi);
    end;

    // NOTE: this is the Arc Angle needed to connect the two tracks with a tangent arc, regardless of radius
    if BadAngle then Result := 999 else Result := ArcAngle;

    if iDebugLevel > 2 then
    begin
        DebugCheck := (ArcAngle < 0) or (ArcAngle >= Pi);

        DebugString := Format('ArcStartAngle = %f; ArcEndAngle = %f', [ArcStartAngle / Pi * 180, ArcEndAngle / Pi * 180]) + sLineBreak2;
        DebugString := DebugString + Format('ArcEndAngle - ArcStartAngle = %f', [(ArcEndAngle - ArcStartAngle) / Pi * 180]) + sLineBreak2;
        DebugString := DebugString + Format('ArcEndAngle - ArcStartAngle + 360 = %f', [(ArcEndAngle - ArcStartAngle) / Pi * 180 + 360]) + sLineBreak2;
        DebugString := DebugString + Format('Bad Angle Order = %s; ArcAngle = %f', [BoolToStr(DebugCheck, True), ArcAngle / Pi * 180]) + sLineBreak2;
        DebugString := DebugString + Format('FirstAngle = %f; SecondAngle = %f', [FirstAngle / Pi * 180, SecondAngle / Pi * 180]) + sLineBreak2;
        DebugString := DebugString + Format('SecondAngle - FirstAngle = %f', [(SecondAngle - FirstAngle) / Pi * 180]);

        DebugMessage(2, DebugString, 'Confirm or Cancel Debug');
    end;
end;

function GetCommonPointsForTracks(FirstTrack, SecondTrack : IPCB_Primitive; out FirstTrackEnd, SecondTrackEnd : Integer; RoundToDigit : Integer = 0) : Boolean;
begin
    if not ((FirstTrack <> nil) and (FirstTrack.ObjectId = eTrackObject) and (SecondTrack <> nil) and (SecondTrack.ObjectId = eTrackObject)) then
    begin
        Result := False;
        exit;
    end;

    // determine which end of tracks connect to the other
    if      ( (RoundTo(FirstTrack.x1, RoundToDigit) = RoundTo(SecondTrack.x1, RoundToDigit)) and (RoundTo(FirstTrack.y1, RoundToDigit) = RoundTo(SecondTrack.y1, RoundToDigit)) ) then
    begin
        FirstTrackEnd := 1;
        SecondTrackEnd := 1;
    end
    else if ( (RoundTo(FirstTrack.x1, RoundToDigit) = RoundTo(SecondTrack.x2, RoundToDigit)) and (RoundTo(FirstTrack.y1, RoundToDigit) = RoundTo(SecondTrack.y2, RoundToDigit)) ) then
    begin
        FirstTrackEnd := 1;
        SecondTrackEnd := 2;
    end
    else if ( (RoundTo(FirstTrack.x2, RoundToDigit) = RoundTo(SecondTrack.x1, RoundToDigit)) and (RoundTo(FirstTrack.y2, RoundToDigit) = RoundTo(SecondTrack.y1, RoundToDigit)) ) then
    begin
        FirstTrackEnd := 2;
        SecondTrackEnd := 1;
    end
    else if ( (RoundTo(FirstTrack.x2, RoundToDigit) = RoundTo(SecondTrack.x2, RoundToDigit)) and (RoundTo(FirstTrack.y2, RoundToDigit) = RoundTo(SecondTrack.y2, RoundToDigit)) ) then
    begin
        FirstTrackEnd := 2;
        SecondTrackEnd := 2;
    end
    else
    begin
        FirstTrackEnd := 0;
        SecondTrackEnd := 0;
    end;

    Result := (FirstTrackEnd > 0) and (SecondTrackEnd > 0);
    DebugMessage(3, 'GetCommonPointsForTracks returned ' + BoolToStr(Result, True), 'Confirm or Cancel Debug');
end;

procedure Inspect_IPCB_Track(var Track : IPCB_Track; const MyLabel : string = '');
{ IPCB_Text inspector for debugging }
begin
    ShowInfo('DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------' + sLineBreak +
                Format('%s : %s', ['x1                         ' , IntToStr(Track.x1              )             ]) + sLineBreak +
                Format('%s : %s', ['y1                         ' , IntToStr(Track.y1              )             ]) + sLineBreak +
                Format('%s : %s', ['x2                         ' , IntToStr(Track.x2              )             ]) + sLineBreak +
                Format('%s : %s', ['y2                         ' , IntToStr(Track.y2              )             ]) + sLineBreak +
                Format('%s : %s', ['Width                      ' , IntToStr(Track.Width           )             ]) + sLineBreak +
                Format('%s : %s', ['GetState_Length            ' , IntToStr(Track.GetState_Length )             ]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString             ' ,          Track.ObjectIDString                ]) + sLineBreak +
                Format('%s : %s', ['Identifier                 ' ,          Track.Identifier                    ]) + sLineBreak +
                Format('%s : %s', ['Descriptor                 ' ,          Track.Descriptor                    ]) + sLineBreak +
                Format('%s : %s', ['Detail                     ' ,          Track.Detail                        ]) + sLineBreak
                , 'Confirm IPCB_Track Info (partial)');
end;


function IsStringANum(Text : string; AllowNegative : Boolean = False) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    hyphenCount : Integer;
    ChSet    : TSet;
begin
    Result := True;

    if AllowNegative then
    begin
        // Test for number, hyphen, dot, or comma
        ChSet := SetUnion(MkSet(Ord('-'), Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
        for i := 1 to Length(Text) do
            if not InSet(Ord(Text[i]), ChSet) then Result := False;

        // test if there is a hyphen that isn't leading
        ChSet := MkSet(Ord('-'));
        for i := 2 to Length(Text) do
            if InSet(Ord(Text[i]), ChSet) then Result := False;

        // Test for more than one hyphen
        hyphenCount := 0;
        ChSet    := MkSet(Ord('-'));
        for i    := 1 to Length(Text) do
            if InSet(Ord(Text[i]), ChSet) then inc(hyphenCount);

        // Test for more than one dot or comma
        dotCount := 0;
        ChSet    := MkSet(Ord('.'), Ord(','));
        for i    := 1 to Length(Text) do
            if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

        if (dotCount > 1) or (hyphenCount > 1) then Result := False;
    end
    else
    begin
        // Test for number, dot, or comma
        ChSet := SetUnion(MkSet(Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
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
end;

function AngleNormalize(Angle : Double; minVal : Double = 0; maxVal : Double = 999.999) : Double;
begin
    if maxVal = 999.999 then maxVal := 2 * Pi; // default to normalizing to range [0, 2*Pi]

    while (Angle < minVal) or (Angle >= maxVal) do
    begin   // normalize value to range [minVal, maxVal]
        if Angle < minVal then Angle := Angle + maxVal
        else if Angle >= maxVal then Angle := Angle - maxVal;
    end;

    Result := Angle;
end;

function ArcOrderCompare(Arc1, Arc2 : IPCB_Arc) : Integer;
var
    WhichInside : Integer;
    CenterDistance : Double;
begin
    Result := False; // indicates Arc1 is inside Arc2 (or comparison failed)
    if (Arc1 = nil) or (Arc2 = nil) then exit;

    WhichInside := 0;

    CenterDistance := PointToPointDistance(Arc1.XCenter, Arc1.YCenter, Arc2.XCenter, Arc2.YCenter);
    if CenterDistance < 100 then Result := Arc1.Radius > Arc2.Radius // true means Arc2 is inside Arc1
    else // center points are far enough apart to attempt to calculate angle between them
    begin
        // Check if arcs overlap and which is "inside" (i.e. the outside arc has its center in the inside arc's sector)
        if IsPointWithinArcSector(Arc1, Arc2.XCenter, Arc2.YCenter) then WhichInside := 1 // Arc1 is inside
        else if IsPointWithinArcSector(Arc2, Arc1.XCenter, Arc1.YCenter) then WhichInside := 2; // Arc2 is inside
    end;

    Result := WhichInside;
end;

function ArcSectorsOverlapAndAlign(Arc1, Arc2 : IPCB_Arc; SectorAngle : Double = 999.999) : Integer;
var
    Arc1StartAngle, Arc1EndAngle, Arc1HalfAngle : Double;
    Arc2StartAngle, Arc2EndAngle, Arc2HalfAngle : Double;
    WhichInside : Integer;
    CenterDistance : Double;
    CenterAngle : Double;
    dx, dy : TCoord;
begin
    Result := 0; // return 0 unless all checks pass (arcs overlap and are aligned)
    if (Arc1 = nil) or (Arc2 = nil) then exit;

    WhichInside := 0;

    ArcAnglesCalculate(Arc1, Arc1StartAngle, Arc1EndAngle, Arc1HalfAngle);
    ArcAnglesCalculate(Arc2, Arc2StartAngle, Arc2EndAngle, Arc2HalfAngle);

    CenterDistance := PointToPointDistance(Arc1.XCenter, Arc1.YCenter, Arc2.XCenter, Arc2.YCenter);
    if CenterDistance < 10 then if Arc1.Radius <= Arc2.Radius then Result := 1 else Result := 2 // if arc centers are within 0.001 mil then consider them coincident and determine "inside" by radius
    else // center distance is great enough to try to calculate alignment angle
    begin
        // Check if arcs overlap and which is "inside" (i.e. the outside arc has its center in the inside arc's sector)
        if IsPointWithinArcSector(Arc1, Arc2.XCenter, Arc2.YCenter, SectorAngle) then WhichInside := 1 // Arc1 is inside
        else if IsPointWithinArcSector(Arc2, Arc1.XCenter, Arc1.YCenter, SectorAngle) then WhichInside := 2; // Arc2 is inside

        //if WhichInside = 1 then
        //begin // Arc1 is inside
            //dx := Arc2.XCenter - Arc1.XCenter;
            //dy := Arc2.YCenter - Arc1.YCenter;
            //CenterAngle := AngleNormalize(ArcTan2(dy, dx));
        //end
        //else if WhichInside = 2 then
        //begin // Arc2 is inside
            //dx := Arc1.XCenter - Arc2.XCenter;
            //dy := Arc1.YCenter - Arc2.YCenter;
            //CenterAngle := AngleNormalize(ArcTan2(dy, dx));
        //end
        //else    // if neither is inside, then they don't overlap
        //begin
            //exit;
        //end;

        // compare Arc half-angles to CenterAngle to see if they align within 1°
        //if  (Arc1HalfAngle >= (CenterAngle - Pi / 180)) and (Arc1HalfAngle <= (CenterAngle + Pi / 180)) and
            //(Arc2HalfAngle >= (CenterAngle - Pi / 180)) and (Arc2HalfAngle <= (CenterAngle + Pi / 180)) then Result := WhichInside;


        // compare Arc half-angles to see if they align within 1° - if arcs overlap and they are pointing the same way, they align
        if  (Arc1HalfAngle >= (Arc2HalfAngle - Pi / 180)) and (Arc1HalfAngle <= (Arc2HalfAngle + Pi / 180)) and
            (Arc2HalfAngle >= (Arc1HalfAngle - Pi / 180)) and (Arc2HalfAngle <= (Arc1HalfAngle + Pi / 180)) then Result := WhichInside;
    end;
end;

procedure ArcAnglesCalculate(AnArc : IPCB_Arc; out StartAngle, EndAngle, HalfAngle : Double);
begin
    StartAngle := 0; EndAngle := 0; HalfAngle := 0;
    if AnArc = nil then exit;

    StartAngle := AngleNormalize(AnArc.StartAngle * Pi / 180);
    EndAngle := AngleNormalize(AnArc.EndAngle * Pi / 180);

    HalfAngle := AngleNormalize(EndAngle - StartAngle);
    HalfAngle := (HalfAngle / 2) + StartAngle;
end;



procedure CoincidentArcCleanup(ListOfPrimitives : TStringList);
var
    UnprocessedArcs             : TStringList;
    CurrentGlossGroup           : TStringList;
    AnArc, GroupArc             : IPCB_Arc;
    CompareResult               : Integer;
    MemIdx, ProcIdx, GroupIdx   : Integer;
    MatchFound                  : Boolean;
begin
    // Change mode settings to override fillet behavior to manually adjust radii
    if (Mode = eArcModeRelative) and (Ratio >= 45) then ArcSortOrder := eArcSortReverse;
    Mode := eModeRadius;
    ArcMode := eArcModeAbsolute;
    RoundingMode := eRoundingModeOff;   // since innermost radii would be rounded, concentricity is more important than rounding successive arcs
    // besides, if the user wanted to do a rounding pass afterward, they could just do a Relative mode pass with Ratio = 100%

    // TODO: implement routine to group all arcs that are candidates for making coincidental then call `CoincidentArcGroupGloss` to process them
    ArcDictionaryInitialize(UnprocessedArcs);
    ArcDictionaryInitialize(CurrentGlossGroup);

    for MemIdx := 0 to ListOfPrimitives.Count - 1 do
    begin
        AnArc := ListOfPrimitives.Objects[MemIdx];
        if AnArc = nil then continue; // just in case an item no longer exists
        if AnArc.ObjectId = eArcObject then ArcDictionaryAdd(UnprocessedArcs, AnArc);
    end; // at this point UnprocessedArcs has all the arcs we want to evaluate but not all will be glossed

    DebugMessage(2, UnprocessedArcs.Text + sLineBreak2 + ArcDictionaryDebug(UnprocessedArcs), 'Confirm UnprocessedArcs List');

    // TODO: need to check each arc in current group against all unprocessed arcs, and only stop looking once a complete pass is made without finding a match
    while UnprocessedArcs.Count > 0 do
    begin
        // start new group with first unprocessed arc
        CurrentGlossGroup.Clear;
        ArcDictionaryAdd(CurrentGlossGroup, ArcDictionaryPop(UnprocessedArcs, 0));

        // loop through both unprocessed arcs and group arcs to find any matches; stop when a complete pass of both has no match
        repeat
            MatchFound := False;
            // Find overlapping and aligned arcs for the current group
            for ProcIdx := 0 to UnprocessedArcs.Count - 1 do
            begin
                // check next unprocessed arc against all arcs in the current group
                AnArc := ArcDictionaryItem(UnprocessedArcs, ProcIdx);
                for GroupIdx := 0 to CurrentGlossGroup.Count - 1 do
                begin
                    GroupArc := ArcDictionaryItem(CurrentGlossGroup, GroupIdx);

                    if iDebugLevel >= 3 then DebugMessage(3, 'Arcs' + sLineBreak + AnArc.Descriptor + sLineBreak + GroupArc.Descriptor + sLineBreak2 + 'ArcSectorsOverlapAndAlign = ' + IntToStr(ArcSectorsOverlapAndAlign(AnArc, GroupArc, cSectorAngleDegTol)), 'Confirm CompareResult');

                    CompareResult := ArcSectorsOverlapAndAlign(AnArc, GroupArc, cSectorAngleDegTol);
                    if CompareResult > 0 then
                    begin
                        ArcDictionaryAdd(CurrentGlossGroup, ArcDictionaryPop(UnprocessedArcs, ProcIdx)); // pop from unprocessed list and add to current group
                        MatchFound := True;
                        break; // break so we can check the next unprocessed arc
                    end;
                end;

                if MatchFound then break; // if any match was found, restart the search
            end;

        until not MatchFound;

        DebugMessage(2, CurrentGlossGroup.Text + sLineBreak2 + ArcDictionaryDebug(CurrentGlossGroup), 'Confirm Unsorted CurrentGlossGroup List');

        // sort and process the current group
        if CurrentGlossGroup.Count > 1 then CoincidentArcGroupGloss(CurrentGlossGroup);
    end;
end;

procedure CoincidentArcGroupGloss(ArcDictionary : TStringList);
var
    ArcRadius : TCoord;
    AnArc : IPCB_Arc;
    XC, YC : TCoord;
    ListIdx : Integer;
    throwaway : Integer;
    FirstTrackList, SecondTrackList : TStringList;
    FirstTrack, SecondTrack : IPCB_Track;
    FirstTrackDistance, SecondTrackDistance : Double;
begin
    if ArcDictionary.Count < 2 then
    begin
        ShowError('CoincidentArcGroupGloss called without multiple arcs. This should never happen.');
        exit;
    end;
    ArcDictionarySort(ArcDictionary, ArcSortOrder = eArcSortReverse);
    DebugMessage(2, ArcDictionary.Text + sLineBreak2 + ArcDictionaryDebug(ArcDictionary), 'Confirm Sorted ArcDictionary List');

    TrackDictionaryInitialize(FirstTrackList);
    TrackDictionaryInitialize(SecondTrackList);

    // first arc is inside arc, use that for coincident point
    AnArc := ArcDictionaryItem(ArcDictionary, 0);
    XC := AnArc.XCenter;
    YC := AnArc.YCenter;


    // First remove all the arcs EXCEPT THE FIRST ONE else there can be issues with zero-length tracks due to iteration order
    for ListIdx := 1 to ArcDictionary.Count - 1 do
    begin
        AnArc := ArcDictionaryItem(ArcDictionary, ListIdx);

        // arcs that are already coincident should be skipped, and this is the easiest place to do it
        if (PointToPointDistance(AnArc.XCenter, AnArc.YCenter, XC, YC) < 100) then continue; // if we got here and arcs are already within 0.01 mil (TBD?), assume they don't need glossing

        AnArc.Selected := True; // required for other processing functions
        FirstTrack := GetArcOrTrackOnEnd(AnArc, 1, throwaway, True);
        SecondTrack := GetArcOrTrackOnEnd(AnArc, 2, throwaway, True);
        if (FirstTrack = nil) or (SecondTrack = nil) then continue; // if for some unknown reason connected tracks couldn't be found, skip this one

        TrackDictionaryAdd(FirstTrackList, FirstTrack);
        TrackDictionaryAdd(SecondTrackList, SecondTrack);

        // Remove arc between tracks after remembering settings we need to restore
        ExtendTracksOverArc(AnArc, FirstTrack, SecondTrack);
    end;

    DebugMessage(2, FirstTrackList.Text + sLineBreak2 + TrackDictionaryDebug(FirstTrackList), 'Confirm FirstTrack List');
    DebugMessage(2, SecondTrackList.Text + sLineBreak2 + TrackDictionaryDebug(SecondTrackList), 'Confirm SecondTrack List');

    // Now go over the saved track pairs and apply new radii
    for ListIdx := 0 to FirstTrackList.Count - 1 do
    begin
        FirstTrack := TrackDictionaryItem(FirstTrackList, ListIdx);
        SecondTrack := TrackDictionaryItem(SecondTrackList, ListIdx);
        if (FirstTrack = nil) or (SecondTrack = nil) then continue;

        // recalculate Radius target based on distance from coincident point to track
        FirstTrackDistance := TrackDictionaryGetPointToTrackDistance(TrackDictionary, FirstTrack, XC, YC);
        SecondTrackDistance := TrackDictionaryGetPointToTrackDistance(TrackDictionary, SecondTrack, XC, YC);
        Radius := Min(FirstTrackDistance, SecondTrackDistance); // modify the global radius setting on the fly

        // create new arcs with the modified radius
        ArcRadius := DetermineArcRadiusFromTracks(FirstTrack, SecondTrack);
        CreateArcBetweenTracks(ArcRadius, FirstTrack, SecondTrack);
    end;
end;



function ConfigFile_GetPath(dummy : String = '') : String;
begin
    result := ClientAPI_SpecialFolder_AltiumApplicationData + '\' + cConfigFileName;
end;

procedure ConfigFile_Read(AFileName : String);
var
    IniFile: TIniFile;
begin
    if not FileExists(AFileName) then
    begin
        // ini file doesn't exist, try to fall back on older format file
        ConfigFile_LegacyRead;
        exit;
    end;

    IniFile := TIniFile.Create(AFileName);
    try
        FormFillet.Top    := IniFile.ReadInteger('Window Position', 'Top', FormFillet.Top);
        FormFillet.Left   := IniFile.ReadInteger('Window Position', 'Left', FormFillet.Left);

        tPreset1.Text := IniFile.ReadString('Presets', 'Preset1', tPreset1.Text);
        tPreset2.Text := IniFile.ReadString('Presets', 'Preset2', tPreset2.Text);
        tPreset3.Text := IniFile.ReadString('Presets', 'Preset3', tPreset3.Text);
        tPreset4.Text := IniFile.ReadString('Presets', 'Preset4', tPreset4.Text);
        tPreset5.Text := IniFile.ReadString('Presets', 'Preset5', tPreset5.Text);
        tPreset6.Text := IniFile.ReadString('Presets', 'Preset6', tPreset6.Text);
        tPreset7.Text := IniFile.ReadString('Presets', 'Preset7', tPreset7.Text);
        tPreset8.Text := IniFile.ReadString('Presets', 'Preset8', tPreset8.Text);

        RadioUnitsMils.Checked          := IniFile.ReadBool('Last Used', 'Units in Mils', RadioUnitsMils.Checked);
        RadioUnitsMM.Checked            := IniFile.ReadBool('Last Used', 'Units in mm', RadioUnitsMM.Checked);
        RadioUnitsRatio.Checked         := IniFile.ReadBool('Last Used', 'Units in ratio', RadioUnitsRatio.Checked);
        RadioArcsAbsolute.Checked       := IniFile.ReadBool('Last Used', 'Absolute Radius Arcs', RadioArcsAbsolute.Checked);
        RadioArcsRelative.Checked       := IniFile.ReadBool('Last Used', 'Relative Radius Arcs', RadioArcsRelative.Checked);
        CheckBoxRounding.Checked        := IniFile.ReadBool('Last Used', 'Round Final Radius', CheckBoxRounding.Checked);
        CheckBoxGlossCoincident.Checked := IniFile.ReadBool('Last Used', 'Gloss Coincident Arcs', CheckBoxGlossCoincident.Checked);
        tRadius.Text                    := IniFile.ReadString('Last Used', 'Radius Value', tRadius.Text); // read this last because it triggers validation
    finally
        IniFile.Free;
    end;
end;

procedure ConfigFile_Write(AFileName : String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);
    try
        IniFile.WriteInteger('Window Position', 'Top', FormFillet.Top);
        IniFile.WriteInteger('Window Position', 'Left', FormFillet.Left);

        IniFile.WriteString('Presets', 'Preset1', tPreset1.Text);
        IniFile.WriteString('Presets', 'Preset2', tPreset2.Text);
        IniFile.WriteString('Presets', 'Preset3', tPreset3.Text);
        IniFile.WriteString('Presets', 'Preset4', tPreset4.Text);
        IniFile.WriteString('Presets', 'Preset5', tPreset5.Text);
        IniFile.WriteString('Presets', 'Preset6', tPreset6.Text);
        IniFile.WriteString('Presets', 'Preset7', tPreset7.Text);
        IniFile.WriteString('Presets', 'Preset8', tPreset8.Text);

        IniFile.WriteString('Last Used', 'Radius Value', tRadius.Text);
        IniFile.WriteBool('Last Used', 'Units in Mils', RadioUnitsMils.Checked);
        IniFile.WriteBool('Last Used', 'Units in mm', RadioUnitsMM.Checked);
        IniFile.WriteBool('Last Used', 'Units in ratio', RadioUnitsRatio.Checked);
        IniFile.WriteBool('Last Used', 'Absolute Radius Arcs', RadioArcsAbsolute.Checked);
        IniFile.WriteBool('Last Used', 'Relative Radius Arcs', RadioArcsRelative.Checked);
        IniFile.WriteBool('Last Used', 'Round Final Radius', CheckBoxRounding.Checked);
        IniFile.WriteBool('Last Used', 'Gloss Coincident Arcs', CheckBoxGlossCoincident.Checked);
    finally
        IniFile.Free;
    end;
end;

{ ** DEPRECATED - replaced with ConfigFile_Read ** function to load preset list from file }
procedure ConfigFile_LegacyRead(const dummy : Integer = 0);
const
    NumPresets = 14; // no longer needed with ini format
var
    PresetFilePath    : string;
    PresetList        : TStringList;
begin
    // default legacy file name is MyFilletPresets.txt
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyFilletPresets.txt';
    PresetList := CreateObject(TStringList);
    if FileExists(PresetFilePath) then
    begin
        // Load presets from file if it exists
        PresetList.LoadFromFile(PresetFilePath);

        // if PresetList.Count < NumPresets then PresetList file exists but count is short, just exit and use defaults
        if PresetList.Count < NumPresets then exit;

        // Set UI text boxes to match preset list (redundant if list was regenerated above)
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
        if (PresetList[12] = 'True') then RadioArcsAbsolute.Checked := true
        else RadioArcsRelative.Checked := true;

    end
    else exit; // if preset file didn't exist at all, just exit (older file format deprecated)
end;

procedure ConvertFormUnits(const dummy : Integer);
begin
    if IsStringANum(tRadius.Text) then
    begin
        // Radius Value and Buttons
        if (RadioUnitsMM.Checked = true) then
        begin
            Mode := eModeRadius;
            Units := eMetric;
            Radius := mmsToCoord(StrToFloat(tRadius.Text));
        end
        else if (RadioUnitsMils.Checked = true) then
        begin
            Mode := eModeRadius;
            Units := eImperial;
            Radius := milsToCoord(StrToFloat(tRadius.Text));
        end
        else
        begin
            Mode := eModeRatio;
            Units := Board.DisplayUnit xor 1;   // need to xor with 1 to convert to eMetric or eImperial for StringToCoordUnit, etc.
            Ratio := StrToFloat(tRadius.Text);
        end;
    end;

    // Arc Mode Buttons
    if      (RadioArcsAbsolute.Checked = true) then ArcMode := eArcModeAbsolute
    else if (RadioArcsRelative.Checked = true) then ArcMode := eArcModeRelative;

    // rounding setting checkbox
    if CheckBoxRounding.Checked then RoundingMode := eRoundingModeOn else RoundingMode := eRoundingModeOff;

    // coincident gloss setting checkbox
    if CheckBoxGlossCoincident.Checked then GlossMode := eGlossModeOn else GlossMode := eGlossModeOff;

    // default arc sort order for coincident glossing
    ArcSortOrder := eArcSortNormal;
end;

function PointToPointDistance(X1, Y1, X2, Y2 : TCoord) : Double;
begin
    Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;

function PointToTrackDistance(ATrack : IPCB_Track; X, Y : TCoord) : TCoord;
var
    X1, Y1, X2, Y2 : TCoord;
begin
    Result := 0;
    if ATrack = nil then exit;
    if ATrack.ObjectId <> eTrackObject then exit;

    X1 := ATrack.x1;
    Y1 := ATrack.y1;
    X2 := ATrack.x2;
    Y2 := ATrack.y2;

    // scale down all numbers (and hopefully use floating point math) to avoid overflows - Altium math operations are limited to 32bit
    X := X / 100;
    Y := Y / 100;
    X1 := X1 / 100;
    Y1 := Y1 / 100;
    X2 := X2 / 100;
    Y2 := Y2 / 100;

    // sourced from https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
    Result := Round(Abs(( ((X2 - X1)*(Y1 - Y)) - ((X1 - X)*(Y2 - Y1)) ) / Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1))) * 100);
end;

procedure RemoveTrackUndoSafe(ATrack : IPCB_Track);
begin
    // Undo apparently has trouble restoring tracks that were exactly zero length when deleted
    if (ATrack.x1 = ATrack.x2) and (ATrack.y1 = ATrack.y2) then
    begin
        //Inspect_IPCB_Track(ATrack, 'Before Nudging');
        ATrack.BeginModify;
        ATrack.x1 := ATrack.x1 + 1; // just a tiny nudge so undo will be able to recover track
        ATrack.EndModify;
        ATrack.GraphicallyInvalidate;
        //Inspect_IPCB_Track(ATrack, 'After Nudging');
    end;

    Board.BeginModify;
    Board.RemovePCBObject(ATrack);
    Board.EndModify;
end;

procedure ReselectPrimitives(ListOfPrimitives : TStringList);
var
    Prim    :IPCB_Primitive;
    i       :Integer;
begin
    for i := 0 to ListOfPrimitives.Count - 1 do
    begin
        Prim := ListOfPrimitives.Objects[i];
        if Prim <> nil then Prim.Selected := True; // safety check in case some of the items were removed
    end;
end;


function RoundCoords(coords : TCoord; round_mult : Double; UserUnits : TUnit = -1) : TCoord;
begin
    if UserUnits = -1 then
    begin
        Result := Round(coords / round_mult) * round_mult;
        exit;
    end;

    case UserUnits of
        eImperial: begin
            Result := MilsToCoord(Round(CoordToMils2(coords) / round_mult) * round_mult);
        end;
        eMetric: begin
            Result := MMsToCoord(Round(CoordToMMs(coords) / round_mult) * round_mult);
        end;
        else begin
            Result := coords; // invalid
        end;
    end;
end;


procedure CreateArcBetweenTracks(ArcRadius : Integer; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive);
var
    AnArc                   :IPCB_Arc;
    CommonPoint             :array [1..2] of Integer;
    Angle                   :array [1..2] of Double;
    StartAngle, StopAngle   :Double;
    HalfAngle               :Double;
    RemovedLength           :Integer;
    X1, X2, Xc, Y1, Y2, Yc  :Integer;
    A1, A2                  :Double;
begin
    if (ArcRadius = 0) or (FirstTrack = nil) or (SecondTrack = nil) then exit;

    // Find common point between tracks
    if (SecondTrack.x1 = FirstTrack.x1) and (SecondTrack.y1 = FirstTrack.y1) then
    begin
        CommonPoint[1] := 1;
        CommonPoint[2] := 1;
    end
    else if (SecondTrack.x2 = FirstTrack.x1) and (SecondTrack.y2 = FirstTrack.y1) then
    begin
        CommonPoint[1] := 1;
        CommonPoint[2] := 2;
    end
    else if (SecondTrack.x1 = FirstTrack.x2) and (SecondTrack.y1 = FirstTrack.y2) then
    begin
        CommonPoint[1] := 2;
        CommonPoint[2] := 1;
    end
    else if (SecondTrack.x2 = FirstTrack.x2) and (SecondTrack.y2 = FirstTrack.y2) then
    begin
        CommonPoint[1] := 2;
        CommonPoint[2] := 2;
    end;

    // Find angle of first track
    if CommonPoint[1] = 1 then
    begin
        if FirstTrack.x2 = FirstTrack.x1 then
        begin
            if FirstTrack.y1 > FirstTrack.y2 then Angle[1] := 3 * Pi / 2
            else Angle[1] := Pi / 2;
        end
        else
            Angle[1] := arctan((FirstTrack.y2 - FirstTrack.y1) / (FirstTrack.x2 - FirstTrack.x1));
        if FirstTrack.x2 < FirstTrack.x1 then Angle[1] := Angle[1] + Pi;
    end
    else begin
        if FirstTrack.x2 = FirstTrack.x1 then
        begin
            if FirstTrack.y1 < FirstTrack.y2 then Angle[1] := 3 * Pi / 2
            else Angle[1] := Pi / 2;
        end
        else
            Angle[1] := arctan((FirstTrack.y1 - FirstTrack.y2) / (FirstTrack.x1 - FirstTrack.x2));
        if FirstTrack.x1 < FirstTrack.x2 then Angle[1] := Angle[1] + Pi;
    end;

    if Angle[1] < 0 then Angle[1] := 2 * Pi + Angle[1];

    // Find angle of second track
    if CommonPoint[2] = 1 then
    begin
        if SecondTrack.x2 = SecondTrack.x1 then
        begin
            if SecondTrack.y1 > SecondTrack.y2 then Angle[2] := 3 * Pi / 2
            else Angle[2] := Pi / 2;
        end
        else
            Angle[2] := arctan((SecondTrack.y2 - SecondTrack.y1) / (SecondTrack.x2 - SecondTrack.x1));
        if SecondTrack.x2 < SecondTrack.x1 then Angle[2] := Angle[2] + Pi;
    end
    else begin
        if SecondTrack.x2 = SecondTrack.x1 then
        begin
            if SecondTrack.y1 < SecondTrack.y2 then Angle[2] := 3 * Pi / 2
            else Angle[2] := Pi / 2;
        end
        else
            Angle[2] := arctan((SecondTrack.y1 - SecondTrack.y2) / (SecondTrack.x1 - SecondTrack.x2));
        if SecondTrack.x1 < SecondTrack.x2 then Angle[2] := Angle[2] + Pi;
    end;

    if Angle[2] < 0 then Angle[2] := 2 * Pi + Angle[2];

    // Place arc if tracks are at different angles
    if not ((Angle[1] = Angle[2]) or
        ((Angle[1] > Angle[2]) and (Angle[1] - Pi = Angle[2])) or
        ((Angle[1] < Angle[2]) and (Angle[2] - Pi = Angle[1]))) then
    begin

        // Find arc angles
        if ((Angle[1] > Angle[2]) and (Angle[1] - Angle[2] < Pi)) then
        begin
            StartAngle := Pi / 2 + Angle[1];
            StopAngle  := 3 * Pi / 2 + Angle[2];
        end
        else if ((Angle[1] > Angle[2]) and (Angle[1] - Angle[2] > Pi)) then
        begin
            StartAngle := Pi / 2 + Angle[2];
            StopAngle  := Angle[1] - Pi / 2;
        end
        else if ((Angle[1] < Angle[2]) and (Angle[2] - Angle[1] < Pi)) then
        begin
            StartAngle := Pi / 2 + Angle[2];
            StopAngle  := 3 * Pi / 2 + Angle[1];
        end
        else if ((Angle[1] < Angle[2]) and (Angle[2] - Angle[1] > Pi)) then
        begin
            StartAngle := Pi / 2 + Angle[1];
            StopAngle  := Angle[2] - Pi / 2;
        end;

        // Determine how much to shorten tracks
        HalfAngle := (StopAngle - StartAngle) / 2;
        if (HalfAngle = pi / 2) or (HalfAngle = (3 * pi / 2)) then  // is right angle, so shorten by 1 radius (also, tan() will fail)
            RemovedLength := ArcRadius
        else
            RemovedLength := ArcRadius * tan(HalfAngle);

        // Modify first track
        FirstTrack.BeginModify;
        if CommonPoint[1] = 1 then
        begin
            FirstTrack.x1 := FirstTrack.x1 + RemovedLength * cos(Angle[1]);
            FirstTrack.y1 := FirstTrack.y1 + RemovedLength * sin(Angle[1]);
            X1            := FirstTrack.x1;
            Y1            := FirstTrack.y1;
        end
        else begin
            FirstTrack.x2 := FirstTrack.x2 + RemovedLength * cos(Angle[1]);
            FirstTrack.y2 := FirstTrack.y2 + RemovedLength * sin(Angle[1]);
            X1            := FirstTrack.x2;
            Y1            := FirstTrack.y2;
        end;
        FirstTrack.EndModify;
        FirstTrack.GraphicallyInvalidate;

        // Modify second track
        SecondTrack.BeginModify;
        if CommonPoint[2] = 1 then
        begin
            SecondTrack.x1 := SecondTrack.x1 + RemovedLength * cos(Angle[2]);
            SecondTrack.y1 := SecondTrack.y1 + RemovedLength * sin(Angle[2]);
            X2             := SecondTrack.x1;
            Y2             := SecondTrack.y1;
        end
        else begin
            SecondTrack.x2 := SecondTrack.x2 + RemovedLength * cos(Angle[2]);
            SecondTrack.y2 := SecondTrack.y2 + RemovedLength * sin(Angle[2]);
            X2             := SecondTrack.x2;
            Y2             := SecondTrack.y2;
        end;
        SecondTrack.EndModify;
        SecondTrack.GraphicallyInvalidate;

        // Calculate X center of arc
        if      ((Angle[1] = 0) or (Angle[1] = Pi)) then Xc := X1
        else if ((Angle[2] = 0) or (Angle[2] = Pi)) then Xc := X2
        else begin
            A1 := tan(Pi / 2 + Angle[1]);
            A2 := tan(Pi / 2 + Angle[2]);

            Xc := (Y2 - Y1 + A1 * X1 - A2 * X2) / (A1 - A2);
        end;

        // Calculate Y center of arc
        if      ((Angle[1] = Pi / 2) or (Angle[1] = Pi * 3 / 2)) then Yc := Y1
        else if ((Angle[2] = Pi / 2) or (Angle[2] = Pi * 3 / 2)) then Yc := Y2
        else if ((Angle[1] <> 0) and (Angle[1] <> Pi)) then Yc := tan(Pi / 2 + Angle[1]) * (Xc - X1) + Y1
        else if ((Angle[2] <> 0) and (Angle[2] <> Pi)) then Yc := tan(Pi / 2 + Angle[2]) * (Xc - X2) + Y2;

        // Create arc
        AnArc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
        AnArc.XCenter := Int(Xc);
        AnArc.YCenter := Int(Yc);
        AnArc.Radius := sqrt(sqr(X1 - Xc) + sqr(Y1 - Yc));
        AnArc.LineWidth := FirstTrack.Width;
        AnArc.StartAngle := StartAngle * 180 / Pi;
        AnArc.EndAngle := StopAngle * 180 / Pi;
        AnArc.Layer := FirstTrack.Layer;
        if FirstTrack.InNet then AnArc.Net := FirstTrack.Net;
        Board.AddPCBObject(AnArc);
        Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, AnArc.I_ObjectAddress);
        AnArc.Selected := False;

        // Add arc to selection memory to reselect later
        SelectionMemory.AddObject(AnArc.I_ObjectAddress, AnArc);

        // add little stubs to a list of tracks to clean up later
        if (FirstTrack.GetState_Length < 5) or (SecondTrack.GetState_Length < 5) then
        begin
            if FirstTrack.GetState_Length   < 5 then StubRemovalList.Add(FirstTrack);
            if SecondTrack.GetState_Length  < 5 then StubRemovalList.Add(SecondTrack);
        end;
    end;
end;


{ checks if there is another selected track or arc connected to the given endpoint of this primitive, and which of its ends is connected }
function GetArcOrTrackOnEnd(FirstPrim : IPCB_Primitive; const FirstPrimEnd : Integer; out SecondPrimEnd : Integer; OnlySelected : Boolean = True) : IPCB_Primitive;
var
    SIter : IPCB_SpatialIterator;
    SecondPrim : IPCB_Primitive;
    Xp, Yp : TCoord;
begin
    SecondPrim := nil;
    SecondPrimEnd := 0;
    Result := SecondPrim;

    if (FirstPrim.ObjectId <> eTrackObject) and (FirstPrim.ObjectId <> eArcObject) then exit;

    if FirstPrimEnd = 1 then
    begin
        if FirstPrim.ObjectId = eTrackObject then Xp := FirstPrim.x1 else Xp := FirstPrim.StartX;
        if FirstPrim.ObjectId = eTrackObject then Yp := FirstPrim.y1 else Yp := FirstPrim.StartY;
    end
    else // FirstPrimEnd = 2
    begin
        if FirstPrim.ObjectId = eTrackObject then Xp := FirstPrim.x2 else Xp := FirstPrim.EndX;
        if FirstPrim.ObjectId = eTrackObject then Yp := FirstPrim.y2 else Yp := FirstPrim.EndY;
    end;

    // Check if there is another track or arc in hotspot
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    SIter.AddFilter_LayerSet(MkSet(FirstPrim.Layer));
    SIter.AddFilter_Area(Xp - 1, Yp - 1, Xp + 1, Yp + 1);

    SecondPrim := SIter.FirstPCBObject;
    while (SecondPrim <> nil) do
    begin
        // check if object is selected OR in selection memory because it was added by previous processing
        if (FirstPrim.I_ObjectAddress <> SecondPrim.I_ObjectAddress) and ((SelectionMemory.IndexOf(SecondPrim.I_ObjectAddress) <> -1) or SecondPrim.Selected or not OnlySelected) then
        begin
            if SecondPrim.ObjectId = eTrackObject then
            begin
                if (PointToPointDistance(SecondPrim.x1, SecondPrim.y1, Xp, Yp) < 10) then
                begin
                    Result := SecondPrim;
                    SecondPrimEnd := 1;
                    break;
                end
                else if (PointToPointDistance(SecondPrim.x2, SecondPrim.y2, Xp, Yp) < 10) then
                begin
                    Result := SecondPrim;
                    SecondPrimEnd := 2;
                    break;
                end;
            end
            else if (SecondPrim.ObjectId = eArcObject) and (FirstPrim.ObjectId <> eArcObject) then // SecondPrim is arc but don't chain arcs to arcs
            begin
                if (PointToPointDistance(SecondPrim.StartX, SecondPrim.StartY, Xp, Yp) < 10) then
                begin
                    Result := SecondPrim;
                    SecondPrimEnd := 1;
                    break;
                end
                else if (PointToPointDistance(SecondPrim.EndX, SecondPrim.EndY, Xp, Yp) < 10) then
                begin
                    Result := SecondPrim;
                    SecondPrimEnd := 2;
                    break;
                end;
            end;
        end;

        SecondPrim := SIter.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(SIter);
end;


procedure GetIntersectingTrackLengths(FirstTrack, SecondTrack : IPCB_Primitive; var FirstTrackLength, SecondTrackLength : TCoord);
var
    Xp, Yp : Double;
    T1Dist1, T1Dist2, T2Dist1, T2Dist2 : TCoord;
begin
    GetIntersectionPointBetweenTracks(FirstTrack, SecondTrack, Xp, Yp);

    T1Dist1 := Int(PointToPointDistance(FirstTrack.x1, FirstTrack.y1, Xp, Yp));
    T1Dist2 := Int(PointToPointDistance(FirstTrack.x2, FirstTrack.y2, Xp, Yp));
    FirstTrackLength := Max(T1Dist1, T1Dist2);

    T2Dist1 := Int(PointToPointDistance(SecondTrack.x1, SecondTrack.y1, Xp, Yp));
    T2Dist2 := Int(PointToPointDistance(SecondTrack.x2, SecondTrack.y2, Xp, Yp));
    SecondTrackLength := Max(T2Dist1, T2Dist2);
end;


procedure GetIntersectionPointBetweenTracks(FirstTrack, SecondTrack : IPCB_Primitive; var Xp, Yp : TCoord);
var
    T1X1, T1X2, T2X1, T2X2  : Double;
    T1Y1, T1Y2, T2Y1, T2Y2  : Double;
    Denominator : Double;
begin
    // convert to mils to avoid overflow in intersection calculation
    //T1X1 := CoordToMils2(FirstTrack.x1);
    //T1Y1 := CoordToMils2(FirstTrack.y1);
    //T1X2 := CoordToMils2(FirstTrack.x2);
    //T1Y2 := CoordToMils2(FirstTrack.y2);
    //T2X1 := CoordToMils2(SecondTrack.x1);
    //T2Y1 := CoordToMils2(SecondTrack.y1);
    //T2X2 := CoordToMils2(SecondTrack.x2);
    //T2Y2 := CoordToMils2(SecondTrack.y2);

    // use the tracks' original coordinates for calculating intersection points in case they've been trimmed to short to use now
    TrackDictionaryGetTrackCoords(TrackDictionary, FirstTrack, T1X1, T1Y1, T1X2, T1Y2);
    TrackDictionaryGetTrackCoords(TrackDictionary, SecondTrack, T2X1, T2Y1, T2X2, T2Y2);
    T1X1 := CoordToMils2(T1X1);
    T1Y1 := CoordToMils2(T1Y1);
    T1X2 := CoordToMils2(T1X2);
    T1Y2 := CoordToMils2(T1Y2);
    T2X1 := CoordToMils2(T2X1);
    T2Y1 := CoordToMils2(T2Y1);
    T2X2 := CoordToMils2(T2X2);
    T2Y2 := CoordToMils2(T2Y2);

    // Find intersection point of tracks (sourced from https://en.wikipedia.org/wiki/Line-line_intersection)
    Denominator := ( ((T1X1-T1X2)*(T2Y1-T2Y2)) - ((T1Y1-T1Y2)*(T2X1-T2X2)) );
    if (Denominator = 0) then ShowError(FirstTrack.Identifier + ' and ' + sLineBreak + SecondTrack.Identifier + ' do not intersect.');
    if Denominator <> 0 then
    begin
        Xp := ( (((T1X1*T1Y2)-(T1Y1*T1X2))*(T2X1-T2X2)) - ((T1X1-T1X2)*((T2X1*T2Y2)-(T2Y1*T2X2))) )   /   Denominator;
        Yp := ( (((T1X1*T1Y2)-(T1Y1*T1X2))*(T2Y1-T2Y2)) - ((T1Y1-T1Y2)*((T2X1*T2Y2)-(T2Y1*T2X2))) )   /   Denominator;

        // Change units back to type TCoord
        Xp := MilsToCoord(Xp);
        Yp := MilsToCoord(Yp);
    end
    else
    begin
        Xp := nil;
        Yp := nil;
    end;
end;


procedure ExtendTracksOverArc(AnArc : IPCB_Primitive; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive);
var
    Xp, Yp          :Integer;
    Dist1, Dist2    :Integer;
begin
    // Find intersection point
    GetIntersectionPointBetweenTracks(FirstTrack, SecondTrack, Xp, Yp);

    // Delete Arc
    AnArc.SetState_Selected(false);
    Board.BeginModify;
    Board.RemovePCBObject(AnArc);
    Board.EndModify;

    // Extend first track to intersection point
    ExtendTrackToPoint(FirstTrack, Xp, Yp);

    // Extend second track to intersection point
    ExtendTrackToPoint(SecondTrack, Xp, Yp);
end;


procedure ExtendTrackToPoint(ATrack : IPCB_Primitive; Xp, Yp: TCoord);
var
    Dist1, Dist2    : TCoord;
begin
    // Move closest end of track to intersection point
    Dist1 := Int(PointToPointDistance(ATrack.x1, ATrack.y1, Xp, Yp));
    Dist2 := Int(PointToPointDistance(ATrack.x2, ATrack.y2, Xp, Yp));

    ATrack.BeginModify;
    if Dist1 < Dist2 then
    begin
        ATrack.x1 := Xp;
        ATrack.y1 := Yp;
    end
    else begin
        ATrack.x2 := Xp;
        ATrack.y2 := Yp;
    end;
    ATrack.EndModify;
    ATrack.GraphicallyInvalidate;
end;


function DebugLevelStr(dummy : String = '') : String;
begin
    Result := '-------------------------  Debug Level: ' + IntToStr(iDebugLevel) + '  -------------------------' + sLineBreak;
end;

procedure DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug');
begin
    // iDebugLevel must be an Integer global variable initialized before first call to this procedure
    if iDebugLevel >= ShowLevel then
    begin
        msg := DebugLevelStr + msg;
        // if user clicks on Cancel, downgrade the debug level by 1 until it reaches 0
        if ConfirmOKCancelWithCaption(Caption, msg) = False then
            iDebugLevel := Max(iDebugLevel - 1, 0);
    end;
end;


function DetermineArcRadiusFromArc(AnArc : IPCB_Primitive; FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive) : Integer;
var
    ArcRadius               : TCoord;
    MaxRadius               : TCoord;
    ProjectedMaxRadius      : TCoord;
    Xp, Yp                  : TCoord;
    Dist1, Dist2            : TCoord;
    MaxLength1, MaxLength2  : TCoord;
begin
    // get max allowable projected and real radii for given tracks
    begin
        // calculate the max radius based on projecting through any existing fillets (important for chaining in ratio mode)
        ProjectedMaxRadius := DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack, True);

        // calculate the max radius between two tracks only, ignoring any chained primitives
        MaxRadius := DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack);
    end;

    // Determine desired radius
    begin
        if Mode = eModeRadius then
        begin
            if ArcMode = eArcModeAbsolute then
            begin
                ArcRadius := Radius;
            end
            else if ArcMode = eArcModeRelative then
            begin
                ArcRadius := AnArc.Radius + Radius;
            end;
        end
        else if Mode = eModeRatio then
        begin
            if ArcMode = eArcModeAbsolute then
            begin
                // in absolute ratio mode, calculate ratio for chained tracks using projected lengths
                DebugMessage(3, 'ProjectedMaxRadius = ' + CoordToDisplayStr(ProjectedMaxRadius) + sLineBreak2 + 'MaxRadius = ' + CoordToDisplayStr(MaxRadius), 'Confirm or Cancel Debug');
                ArcRadius := Max(ProjectedMaxRadius, MaxRadius) * (Ratio / 100);
            end
            else if ArcMode = eArcModeRelative then
            begin
                ArcRadius := AnArc.Radius * Ratio / 100;
            end;
        end;
    end;

    // round final arc radius if enabled
    if RoundingMode = eRoundingModeOn then
    begin
        case Units of
            eImperial : ArcRadius := RoundCoords(ArcRadius, 1.0, Units);
            eMetric :   ArcRadius := RoundCoords(ArcRadius, 0.025, Units);
        end;
    end;

    if ArcRadius < 0 then ArcRadius := 0;

    Result := Min(ArcRadius, MaxRadius);
end;


function DetermineArcRadiusFromTracks(FirstTrack : IPCB_Primitive; SecondTrack : IPCB_Primitive) : Integer;
var
    ArcRadius           : TCoord;
    MaxRadius           : TCoord;
    ProjectedMaxRadius  : TCoord;
begin
    // Determine max radius from shortest track length
    begin
        // calculate the max radius based on projecting through any existing fillets (important for chaining in ratio mode)
        ProjectedMaxRadius := DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack, True);

        // calculate the max radius between two tracks only, ignoring any chained primitives
        MaxRadius := DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack);
    end;

    // Determine desired radius
    begin
        if Mode = eModeRadius then
        begin
            ArcRadius := Radius;
        end
        else if Mode = eModeRatio then
        begin
            // in ratio mode, calculate ratio for chained tracks using projected lengths
            DebugMessage(3, 'ProjectedMaxRadius = ' + CoordToDisplayStr(ProjectedMaxRadius) + sLineBreak2 + 'MaxRadius = ' + CoordToDisplayStr(MaxRadius), 'Confirm or Cancel Debug');
            ArcRadius := Max(ProjectedMaxRadius, MaxRadius) * (Ratio / 100);
        end;
    end;

    // round final arc radius if enabled
    if RoundingMode = eRoundingModeOn then
    begin
        case Units of
            eImperial : ArcRadius := RoundCoords(ArcRadius, 1.0, Units);
            eMetric :   ArcRadius := RoundCoords(ArcRadius, 0.025, Units);
        end;
    end;

    if ArcRadius < 0 then ArcRadius := 0;

    Result := Min(ArcRadius, MaxRadius);
end;


function DetermineMaxRadiusBetweenTracks(FirstTrack, SecondTrack : IPCB_Primitive; Projected : Boolean = False) : TCoord;
var
    FirstTrackMaxLength, SecondTrackMaxLength : TCoord;
    ArcAngle : Double;
    TrackAngle, TrackHalfAngle : Double;
    MaxRadius : Double;
begin
    // Max radius is determined by shorter of the two tracks and their angle
    if Projected then // get length as projected to adjacent selected tracks in chain
    begin
        FirstTrackMaxLength := GetProjectedTrackLength(FirstTrack);
        SecondTrackMaxLength := GetProjectedTrackLength(SecondTrack);
    end
    else GetIntersectingTrackLengths(FirstTrack, SecondTrack, FirstTrackMaxLength, SecondTrackMaxLength);

    ArcAngle := GetArcAngleBetweenTracks(FirstTrack, SecondTrack);
    TrackAngle := Pi - ArcAngle;
    TrackHalfAngle := TrackAngle / 2;

    if (TrackHalfAngle = Pi / 2) then MaxRadius := Min(FirstTrackMaxLength, SecondTrackMaxLength) // for right angle, use shorter track length
    else MaxRadius := tan(TrackHalfAngle) * Min(FirstTrackMaxLength, SecondTrackMaxLength);

    Result := Int(MaxRadius);
end;


// calculates how long the track would be if it were projected to the adjacent chained track(s)
function GetProjectedTrackLength(ATrack : IPCB_Track; OnlySelected : Boolean = True) : TCoord;
var
    FirstEndTrack, SecondEndTrack : IPCB_Track;
    End1Xp, End1Yp, End2Xp, End2Yp : TCoord;
begin
    Result := 0;
    if ATrack = nil then exit;

    // find the point where this track would intercept the next track (if that track is selected)
    FirstEndTrack := GetProjectedInterceptingTrackAtEnd(ATrack, 1, OnlySelected);
    if FirstEndTrack <> nil then
    begin
        GetIntersectionPointBetweenTracks(ATrack, FirstEndTrack, End1Xp, End1Yp);
    end
    else // else we aren't chaining to the next track, just use current end location
    begin
        End1Xp := ATrack.x1;
        End1Yp := ATrack.y1;
    end;

    // now the other end
    SecondEndTrack := GetProjectedInterceptingTrackAtEnd(ATrack, 2, OnlySelected);
    if SecondEndTrack <> nil then
    begin
        GetIntersectionPointBetweenTracks(ATrack, SecondEndTrack, End2Xp, End2Yp);
    end
    else // else we aren't chaining to the next track, just use current end location
    begin
        End2Xp := ATrack.x2;
        End2Yp := ATrack.y2;
    end;

    // Calculate the distance between the two projected intercept points
    Result := Int(PointToPointDistance(End1Xp, End1Yp, End2Xp, End2Yp));
end;


function GetProjectedInterceptingTrackAtEnd(ATrack : IPCB_Track; const ATrackEnd : Integer; OnlySelected : Boolean = True) : IPCB_Track;
var
    OtherTrack : IPCB_Track;
    OtherTrackEnd : Integer;
    IntermediatePrim : IPCB_Primitive;
    IntermediatePrimEnd : Integer;
    IntermediatePrimOtherEnd : Integer;
begin
    OtherTrack := nil;

    IntermediatePrim := GetArcOrTrackOnEnd(ATrack, ATrackEnd, IntermediatePrimEnd, OnlySelected);

    if (IntermediatePrim <> nil) and (IntermediatePrim.ObjectId = eTrackObject) then OtherTrack := IntermediatePrim
    else if (IntermediatePrim <> nil) then // intermediate primitive is an arc, get the track at its other end
    begin
        IntermediatePrimOtherEnd := (IntermediatePrimEnd + 1) mod 2; // 2 -> 1, 1 -> 2
        OtherTrack := GetArcOrTrackOnEnd(IntermediatePrim, IntermediatePrimOtherEnd, OtherTrackEnd, OnlySelected);

        if (OtherTrack <> nil) and (OtherTrack.ObjectId <> eTrackObject) then OtherTrack := nil;
    end;

    if (OtherTrack <> nil) then Result := OtherTrack else Result := nil;
end;


function GetCommonPointForTrackWithArc(ATrack : IPCB_Primitive; AnArc : IPCB_Primitive; RoundToDigit : Integer = 0) : Integer;
var
    CommonPoint : Integer;
begin
    if ( (RoundTo(ATrack.x1, RoundToDigit) = RoundTo(AnArc.StartX, RoundToDigit)) or (RoundTo(ATrack.x1, RoundToDigit) = RoundTo(AnArc.EndX, RoundToDigit)) ) and
       ( (RoundTo(ATrack.y1, RoundToDigit) = RoundTo(AnArc.StartY, RoundToDigit)) or (RoundTo(ATrack.y1, RoundToDigit) = RoundTo(AnArc.EndY, RoundToDigit)) ) then
    begin
        CommonPoint := 1;
    end
    else if ( (RoundTo(ATrack.x2, RoundToDigit) = RoundTo(AnArc.StartX, RoundToDigit)) or (RoundTo(ATrack.x2, RoundToDigit) = RoundTo(AnArc.EndX, RoundToDigit)) ) and
            ( (RoundTo(ATrack.y2, RoundToDigit) = RoundTo(AnArc.StartY, RoundToDigit)) or (RoundTo(ATrack.y2, RoundToDigit) = RoundTo(AnArc.EndY, RoundToDigit)) ) then
    begin
        CommonPoint := 2;
    end
    else begin
        CommonPoint := 0;
    end;

    Result := CommonPoint;
end;


procedure DoFillets(dummy : Integer = 0);
var
    Prim                            : IPCB_Primitive;
    AnArc                           : IPCB_Primitive;
    FirstTrack                      : IPCB_Primitive;
    SecondTrack                     : IPCB_Primitive;
    IsValidSelection                : Boolean;
    a, b, c                         : Integer;
    ObjIdx, Obj2Idx                 : Integer;
    FirstIdx, SecondIdx             : Integer;
    ArcRadius                       : Integer;
    ArcAngle                        : Double;
    IncrementLoop                   : Boolean;
    FirstTrackEnd, SecondTrackEnd   : Integer;
    RoundToDigit                    : Integer;
    UserMessage                     : String;
begin
    // Instantiate lists and variables
    ConvertFormUnits(0);
    SelectionMemory := CreateObject(TStringList);
    SelectionMemory.Sorted := True;
    SelectionMemory.Duplicates := dupIgnore;

    TrackDictionaryInitialize(TrackDictionary);
    TrackDictionaryCompileList(TrackDictionary);

    StubRemovalList := CreateObject(TInterfaceList);

    // Start undo process
    PCBServer.PreProcess;

    UserMessage := 'Operation completed successfully'; // might not use because it slows you down to dismiss message

    // Rounding for overlapping arc/track points because previously created arc locations are not perfectly placed on tracks by script (ex: 7100.0000 mil != 7100.0001 mil)
    // But as long as they are close enough then we don't care since we will delete the arc and extend tracks to overlap again anyway
    RoundToDigit := 2;

    // Loop through selected primitives for overlapping vertices
    IsValidSelection := False;
    ObjIdx := 0;
    while ObjIdx < Board.SelectecObjectCount do
    begin
        IncrementLoop := True;
        Prim := Board.SelectecObject(ObjIdx);

        // Primitive is Track
        if (Prim.ObjectId = eTrackObject) then
        begin
            FirstTrack := Prim;

            // Find overlapping track
            for Obj2Idx := 0 to Board.SelectecObjectCount - 1 do
            begin
                SecondTrack := Board.SelectecObject(Obj2Idx);
                if (Obj2Idx = ObjIdx) then
                begin
                    // Do nothing but also don't update UserMessage because this isn't a failure
                end
                else if (Obj2Idx <> ObjIdx) and (SecondTrack.Layer = FirstTrack.Layer) and GetCommonPointsForTracks(FirstTrack, SecondTrack, FirstTrackEnd, SecondTrackEnd, RoundToDigit) then
                begin
                    // Find arc angle needed to connect tracks
                    ArcAngle := GetArcAngleBetweenTracks(FirstTrack, SecondTrack);
                    DebugMessage(3, Format('Tracks form angle of %f°', [RoundCoords(ArcAngle / Pi * 180, 0.001)]), 'Confirm or Cancel Debug');

                    // Only process if arc angle won't be over 175°
                    if ArcAngle < 3.055 then
                    begin
                        IsValidSelection := True;

                        // Create arc
                        ArcRadius := DetermineArcRadiusFromTracks(FirstTrack, SecondTrack);
                        CreateArcBetweenTracks(ArcRadius, FirstTrack, SecondTrack);
                    end
                    else UserMessage := 'Angle between some of the connected tracks is too acute (<5°) to risk adding arc';
                end
                else
                begin
                    UserMessage := 'Some of the selected tracks have no common point (track ends must meet EXACTLY)';
                    if Board.SelectecObjectCount < 3 then UserMessage := UserMessage + sLineBreak2 + 'NOTE: Select at least two joined tracks, optionally connected by an existing fillet.';
                end;
            end;
        end
        // Primitive is Arc
        else if (Prim.ObjectId = eArcObject) then
        begin
            AnArc := Prim;
            FirstTrackEnd := 0;
            SecondTrackEnd := 0;

            // Find first overlapping track
            for FirstIdx := 0 to Board.SelectecObjectCount - 1 do
            begin
                FirstTrack := Board.SelectecObject(FirstIdx);
                if ((FirstTrack.ObjectId = eTrackObject) and (FirstIdx <> ObjIdx) and (FirstTrack.Layer = AnArc.Layer)) then
                begin
                    FirstTrackEnd := GetCommonPointForTrackWithArc(FirstTrack, AnArc, RoundToDigit);
                    if FirstTrackEnd <> 0 then break;
                end;
            end;

            // Don't look for second track if we didn't find the first one
            //if FirstTrackEnd = 0 then continue;

            // Find second overlapping track
            for SecondIdx := 0 to Board.SelectecObjectCount - 1 do
            begin
                SecondTrack := Board.SelectecObject(SecondIdx);
                if ((SecondTrack.ObjectId = eTrackObject) and (SecondIdx <> ObjIdx) and (SecondIdx <> FirstIdx) and (SecondTrack.Layer = AnArc.Layer)) then
                begin
                    SecondTrackEnd := GetCommonPointForTrackWithArc(SecondTrack, AnArc, RoundToDigit);
                    if SecondTrackEnd <> 0 then break;
                end;
            end;

            DebugMessage(3, Format( '%d, %d', [FirstTrackEnd, SecondTrackEnd] ), 'Confirm or Cancel Debug');

            // Update arc radius if both arc vertices are connected to tracks
            if (FirstTrackEnd <> 0) and (SecondTrackEnd <> 0) then
            begin

                // Find arc angle needed to connect tracks
                ArcAngle := GetArcAngleBetweenTracks(FirstTrack, SecondTrack);
                DebugMessage(3, Format('Tracks form angle of %f°', [RoundCoords(ArcAngle / Pi * 180, 0.001)]), 'Confirm or Cancel Debug');

                // Only process if arc angle isn't over 175°
                if ArcAngle < 3.055 then
                begin
                    // Check they converge in the direction of the arc (probably not needed with arc angle check above)

                    IsValidSelection := True;

                    ArcRadius := DetermineArcRadiusFromArc(AnArc, FirstTrack, SecondTrack);
                    ExtendTracksOverArc(AnArc, FirstTrack, SecondTrack);
                    CreateArcBetweenTracks(ArcRadius, FirstTrack, SecondTrack);

                    // Don't increment loop when updating arc (since it gets deselected)
                    IncrementLoop := False;
                end
                else UserMessage := 'Angle between some of the connected tracks is too acute (<5°) to modify arc radius';
            end;
        end;

        if IncrementLoop then ObjIdx := ObjIdx + 1;
    end;

    // `SelectionMemory` has the list of all the arcs that were added, use that in `CoincidentArcCleanup`
    if GlossMode = eGlossModeOn then
    begin
        CoincidentArcCleanup(SelectionMemory);
    end;

    if not IsValidSelection then
    begin
        // TODO: Update this with error message with better description for newly added arc updating functionality
        ShowWarning(UserMessage);
    end;

    // Clean up stubby tracks
    for ObjIdx := 0 to StubRemovalList.Count - 1 do
    begin
        Prim := StubRemovalList[ObjIdx];
        // arc glossing might have extended some of these again, so check their length again before deletion
        if Prim.GetState_Length < 5 then RemoveTrackUndoSafe(Prim);
    end;

    // End undo process
    PCBServer.PostProcess;

    // Reselect all previously selected and newly created arcs
    ReselectPrimitives(SelectionMemory);

    //Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    // save current config
    ConfigFile_Write(ConfigFile_GetPath);

    close;
end;


procedure Start;
var
    Prim      :IPCB_Primitive;
    i         :Integer;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    if not Assigned(iDebugLevel) then iDebugLevel := cDEBUGLEVEL;

    // Deselect anything that is not an Arc or Track
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Prim := Board.SelectecObject(i);
        if (Prim.ObjectId = eTrackObject) or (Prim.ObjectId = eArcObject) then i := i + 1
        else Prim.SetState_Selected(false);
    end;

    // Exit if nothing is selected
    if Board.SelectecObjectCount = 0 then
    begin
        ShowInfo('No Selected Tracks/Arcs');
        exit;
    end;

    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyFilletPresets.txt';

    FormFillet.ShowModal;
end;



// Tests if a point lies between the angles of an arc's "arms" (sourced from https://stackoverflow.com/a/13675772/3966477)
function IsPointWithinArcAngles(AnArc : IPCB_Arc; X, Y : TCoord; SectorAngle : Double = 999.999) : Boolean;
var
    StartVecX, StartVecY, EndVecX, EndVecY, PointVecX, PointVecY : TCoord;
    IsCWFromEnd, IsCCWFromStart : Boolean;
    IsSectorObtuse : Boolean;
    ArcHalfAngle, HalfSectorAngle, NewStartAngle, NewEndAngle : Double;
    Radius : TCoord;
begin
    Result := False;
    if AnArc = nil then exit;
    if AnArc.ObjectId <> eArcObject then exit;

    // if SectorAngle is used, need to override start and end vectors
    if SectorAngle < 999 then
    begin
        Radius := AnArc.Radius;
        HalfSectorAngle := AngleNormalize(SectorAngle * Pi / 360); // sector angle is in degrees for convenience, HalfSectorAngle should be in radians
        ArcHalfAngle := (AngleNormalize((AnArc.StartAngle * Pi / 180) + (AnArc.EndAngle * Pi / 180))) / 2; // calculate arc half-angle in radians
        NewStartAngle := AngleNormalize(ArcHalfAngle - HalfSectorAngle);
        NewEndAngle := AngleNormalize(ArcHalfAngle + HalfSectorAngle);

        // Calculate new vectors based on adjusted angles
        StartVecX   := (Radius * Cos(NewStartAngle)) / 100;
        StartVecY   := (Radius * Sin(NewStartAngle)) / 100;
        EndVecX     := (Radius * Cos(NewEndAngle  )) / 100;
        EndVecY     := (Radius * Sin(NewEndAngle  )) / 100;
    end
    else
    begin
        StartVecX := (AnArc.StartX - AnArc.XCenter) / 100;
        StartVecY := (AnArc.StartY - AnArc.YCenter) / 100;
        EndVecX := (AnArc.EndX - AnArc.XCenter) / 100;
        EndVecY := (AnArc.EndY - AnArc.YCenter) / 100;
    end;

    PointVecX := (X - AnArc.XCenter) / 100;
    PointVecY := (Y - AnArc.YCenter) / 100;

    DebugMessage(4, Format('StartVecX VarType = %d | StartVecY VarType = %d | EndVecX VarType = %d | EndVecY VarType = %d | PointVecX VarType = %d | PointVecY VarType = %d',
                        [   VarType(StartVecX),      VarType(StartVecY),       VarType(EndVecX),     VarType(EndVecY),      VarType(PointVecX),      VarType(PointVecY)]));

    IsCWFromEnd := ( (-EndVecX * PointVecY) + (EndVecY * PointVecX) ) >= 0;          // comparison uses 32 bits
    IsCCWFromStart := ( (-StartVecX * PointVecY) + (StartVecY * PointVecX) ) <= 0;   // scale down to avoid overflows

    // note that the result should be inverted if the sector spans more than 180 deg. (End is clockwise from start in vector terms)
    IsSectorObtuse := -StartVecX * EndVecY + StartVecY * EndVecX >= 0;

    Result := (IsCWFromEnd and IsCCWFromStart) xor IsSectorObtuse;
end;

function IsPointWithinArcSector(AnArc : IPCB_Arc; X, Y : TCoord; SectorAngle : Double = 999.999) : Boolean;
begin
    Result := False;
    if AnArc = nil then exit;
    if AnArc.ObjectId <> eArcObject then exit;

    Result := IsPointWithinArcAngles(AnArc, X, Y, SectorAngle) and IsPointWithinRadius(AnArc, X, Y);
end;

function IsPointWithinRadius(AnArc : IPCB_Arc; X, Y : TCoord) : Boolean;
begin
    Result := False;
    if AnArc = nil then exit;
    if AnArc.ObjectId <> eArcObject then exit;

    Result := (Sqr(X - AnArc.XCenter) + Sqr(Y - AnArc.YCenter)) <= Sqr(AnArc.Radius);
end;



procedure ArcDictionaryAdd(var DictionaryList : TStringList; AnArc : IPCB_Arc);
var
    idx : Integer;
begin
    if DictionaryList = nil then exit;

    idx := DictionaryList.IndexOf(AnArc.I_ObjectAddress);
    if idx = -1 then DictionaryList.AddObject(AnArc.I_ObjectAddress, AnArc);
end;

function ArcDictionaryDebug(const DictionaryList : TStringList) : String;
var
    idx : Integer;
    AnArc : IPCB_Arc;
begin
    if DictionaryList = nil then exit;

    Result := '';

    for idx := 0 to DictionaryList.Count - 1 do
    begin
        AnArc := ArcDictionaryItem(DictionaryList, idx);
        if AnArc = nil then continue;

        Result := Result + sLineBreak + AnArc.Descriptor;
    end;
end;

procedure ArcDictionaryInitialize(out DictionaryList : TStringList);
begin
    DictionaryList := CreateObject(TStringList);
    DictionaryList.Sorted := False; // DO NOT make these sorted
    DictionaryList.Clear; // just in case
end;

procedure ArcDictionaryInsert(var DictionaryList : TStringList; AnArc : IPCB_Arc; idx : Integer);
begin
    if DictionaryList = nil then exit;

    if (idx >= DictionaryList.Count) then ArcDictionaryAdd(DictionaryList, AnArc)
    else DictionaryList.InsertObject(Max(0, idx), AnArc.I_ObjectAddress, AnArc);
end;

function ArcDictionaryItem(const DictionaryList : TStringList; const idx : Integer) : IPCB_Arc;
begin
    Result := nil;
    if DictionaryList = nil then exit;

    // retrieve an arc from the list if it has a valid index
    if (idx > -1) and (idx < DictionaryList.Count) then
    begin
        Result := DictionaryList.Objects[idx];
    end;
end;

function ArcDictionaryPop(var DictionaryList : TStringList; idx : Integer) : IPCB_Arc;
begin
    Result := nil;
    if DictionaryList = nil then exit;

    // pop an arc from the list if it has a valid index
    if (idx > -1) and (idx < DictionaryList.Count) then
    begin
        Result := DictionaryList.Objects[idx];
        DictionaryList.Delete(idx);
    end;
end;

procedure ArcDictionarySort(var DictionaryList : TStringList; ReverseOrder : Boolean = False);
var
    OuterIdx, InnerIdx : Integer;
    Moved : Boolean;
    Arc1, Arc2 : IPCB_Arc;
    CompareResult : Integer;
begin
    if DictionaryList = nil then exit;

    // Sort using custom sort algorithm
    repeat
        Moved := False;

        for OuterIdx := 0 to DictionaryList.Count - 2 do
        begin
            Arc1 := ArcDictionaryItem(DictionaryList, OuterIdx);

            for InnerIdx := OuterIdx + 1 to DictionaryList.Count - 1 do
            begin
                Arc2 := ArcDictionaryItem(DictionaryList, InnerIdx);

                if ReverseOrder then CompareResult := ArcOrderCompare(Arc2, Arc1) else CompareResult := ArcOrderCompare(Arc1, Arc2);
                if CompareResult = 2 then // adjacent and second arg is inside
                begin
                    // Remove Arc2 from its current position and insert it next to Arc1
                    Arc2 := ArcDictionaryPop(DictionaryList, InnerIdx);
                    ArcDictionaryInsert(DictionaryList, Arc2, OuterIdx);  // Insert before Arc1

                    Moved := True;
                    break; // Break the inner loop and start the outer loop again
                end;
            end;

            if Moved then Break; // If any arc was moved, restart the sorting process
        end;

    until not Moved; // continue until no arcs are moved in a complete pass
end;

function ArcDictionarySwap(var DictionaryList : TStringList; Arc1, Arc2 : IPCB_Arc) : Boolean;
var
    idx1, idx2 : Integer;
begin
    Result := False; // false indicates no swap occurred
    if DictionaryList = nil then exit;

    idx1 := DictionaryList.IndexOf(Arc1.I_ObjectAddress);
    idx2 := DictionaryList.IndexOf(Arc2.I_ObjectAddress);
    if (idx1 = -1) or (idx2 = -1) then exit;

    if idx1 > idx2 then
    begin
        DictionaryList.Delete(idx1);
        DictionaryList.Delete(idx2);
        ArcDictionaryInsert(DictionaryList, Arc1, idx2);
        ArcDictionaryInsert(DictionaryList, Arc2, idx1);
    end
    else if idx1 < idx2 then
    begin
        DictionaryList.Delete(idx2);
        DictionaryList.Delete(idx1);
        ArcDictionaryInsert(DictionaryList, Arc2, idx1);
        ArcDictionaryInsert(DictionaryList, Arc1, idx2);
    end;

    Result := True; // swap occurred
end;



procedure TrackDictionaryAdd(var DictionaryList : TStringList; ATrack : IPCB_Track);
begin
    if DictionaryList.IndexOfName(IntToStr(ATrack.I_ObjectAddress)) <> -1 then exit; // check for existing before adding

    DictionaryList.AddObject(IntToStr(ATrack.I_ObjectAddress) + '=' + TrackDictionaryValueFormatString(ATrack), ATrack);
end;

procedure TrackDictionaryCompileList(out DictionaryList : TStringList);
var
    ObjIdx : Integer;
    ATrack : IPCB_Track;
begin
    DictionaryList.Clear;

    ObjIdx := 0;
    for ObjIdx := 0 to Board.SelectecObjectCount - 1 do
    begin
        ATrack := Board.SelectecObject(ObjIdx);
        // NOTE: once a track is added, its coordinates are a snapshot until the track is deleted and re-added
        if ATrack.ObjectId = eTrackObject then TrackDictionaryAdd(DictionaryList, ATrack);
    end;
end;

function TrackDictionaryDebug(const DictionaryList : TStringList) : String;
var
    idx : Integer;
    ATrack : IPCB_Track;
begin
    if DictionaryList = nil then exit;

    Result := '';

    for idx := 0 to DictionaryList.Count - 1 do
    begin
        ATrack := TrackDictionaryItem(DictionaryList, idx);
        if ATrack = nil then continue;

        Result := Result + sLineBreak + ATrack.Descriptor;
    end;
end;

function TrackDictionaryGetPointToTrackDistance(const DictionaryList : TStringList; ATrack : IPCB_Track; X, Y : TCoord) : TCoord;
var
    DictIdx : Integer;
    X1, Y1, X2, Y2 : TCoord;
begin
    Result := 0;
    if ATrack = nil then exit;

    // parse original coordinates from dictionary value string; fall back on current track coordinates if not in dictionary
    DictIdx := DictionaryList.IndexOfName(IntToStr(ATrack.I_ObjectAddress));
    if DictIdx <> -1 then
    begin
        TrackDictionaryValueParse(DictionaryList.ValueFromIndex[DictIdx], X1, Y1, X2, Y2);
        Result := True;
    end
    else
    begin
        X1 := ATrack.x1;
        Y1 := ATrack.y1;
        X2 := ATrack.x2;
        Y2 := ATrack.y2;
    end;
    // scale down all numbers (and hopefully use floating point math) to avoid overflows - Altium math operations are limited to 32bit
    X := X / 100;
    Y := Y / 100;
    X1 := X1 / 100;
    Y1 := Y1 / 100;
    X2 := X2 / 100;
    Y2 := Y2 / 100;

    // sourced from https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
    Result := Round(Abs(( ((X2 - X1)*(Y1 - Y)) - ((X1 - X)*(Y2 - Y1)) ) / Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1))) * 100);
end;

function TrackDictionaryGetTrackCoords(const DictionaryList : TStringList; ATrack : IPCB_Track; out X1, Y1, X2, Y2 : TCoord) : Boolean;
var
    DictIdx : Integer;
begin
    Result := False;
    if ATrack = nil then exit;

    // parse original coordinates from dictionary value string, fall back on current track coordinates
    DictIdx := DictionaryList.IndexOfName(IntToStr(ATrack.I_ObjectAddress));
    if DictIdx <> -1 then
    begin
        TrackDictionaryValueParse(DictionaryList.ValueFromIndex[DictIdx], X1, Y1, X2, Y2);
        Result := True;
    end
    else
    begin
        X1 := ATrack.x1;
        Y1 := ATrack.y1;
        X2 := ATrack.x2;
        Y2 := ATrack.y2;
    end;
end;

procedure TrackDictionaryInitialize(out DictionaryList : TStringList);
begin
    DictionaryList := CreateObject(TStringList);
    DictionaryList.Sorted := False; // DO NOT make these sorted
    DictionaryList.Clear; // just in case
end;

function TrackDictionaryItem(const DictionaryList : TStringList; idx : Integer) : IPCB_Track;
begin
    Result := nil;
    if DictionaryList = nil then exit;

    // retrieve track from the list if it has a valid index
    if (idx > -1) and (idx < DictionaryList.Count) then
    begin
        Result := DictionaryList.Objects[idx];
    end;
end;

function TrackDictionaryValueFormatString(const ATrack : IPCB_Track) : String;
begin
    Result := '0|0|0|0';
    if ATrack = nil then exit;
    if ATrack.ObjectId <> eTrackObject then exit;

    Result := Format('%d|%d|%d|%d', [ATrack.x1, ATrack.y1, ATrack.x2, ATrack.y2]);
end;

procedure TrackDictionaryValueParse(const ValueString : String; out X1, Y1, X2, Y2 : TCoord);
var
    TempString : String;
    DelimPos, idx : Integer;
    Value : Integer;
begin
    TempString := ValueString;
    idx := 0;

    X1 := 0; Y1:= 0; X2 := 0; Y2 := 0;

    repeat
        DelimPos := Pos('|', TempString); // find delimiter position
        if DelimPos = 0 then DelimPos := Length(TempString) + 1;

        Value := StrToInt(Copy(TempString, 1, DelimPos - 1));
        case idx of
            0 : X1 := Value;
            1 : Y1 := Value;
            2 : X2 := Value;
            3 : Y2 := Value;
        end;

        Inc(idx);
        Delete(TempString, 1, DelimPos); // remove processed value

    until (DelimPos = Length(TempString) + 1) or (idx >= 4);
end;



procedure TFormFillet.FormFilletShow(Sender: TObject);
begin
    LabelVersion.Caption := 'About v' + cScriptVersion;

    Application.HintHidePause := 12000; // extend hint show time

    // Read previous settings from file
    ConfigFile_Read(ConfigFile_GetPath);
    ButtonOK.Enabled := IsStringANum(tRadius.Text, RadioArcsRelative.Checked and (not RadioUnitsRatio.Checked));
end;


procedure TFormFillet.FormFilletClose(Sender: TObject; var Action: TCloseAction);
begin
    // save current config
    //ConfigFile_Write(ConfigFile_GetPath); // don't save here, only save after DoFillets
end;


procedure TFormFillet.ButtonCancelClick(Sender: TObject);
begin
    close;
end;


procedure TFormFillet.RadioUnitsRatioClick(Sender: TObject);
begin
    if IsStringANum(tRadius.Text) then
    begin
        if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) > 100) and (not RadioArcsRelative.Checked) then tRadius.Text := '100'
        else if (RadioUnitsRatio.Checked = true) and (StrToFloat(tRadius.Text) < 0) then tRadius.Text := '0';
    end;
end;


procedure TFormFillet.ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    if IsStringANum(textbox.Text) then
    begin
        If Sender <> tRadius then tRadius.Text := textbox.Text;
        ButtonOK.Enabled := IsStringANum(tRadius.Text, RadioArcsRelative.Checked and (not RadioUnitsRatio.Checked));

        if (RadioUnitsRatio.Checked = true) and (StrToFloat(textbox.Text) > 100) and (not RadioArcsRelative.Checked) then
        begin
            textbox.Text := '100';
            ShowInfo('% input limited to 100% to avoid reversing tracks');
        end
        else if (RadioUnitsRatio.Checked = true) and (StrToFloat(textbox.Text) < 0) then
        begin
            textbox.Text := '0';
            ShowInfo('% input limited to 0% to avoid reversing tracks');
        end;
    end
    else ButtonOK.Enabled := False;
end;


procedure TFormFillet.UserKeyPress(Sender: TObject; var Key: Char);     //programmatically, OnKeyPress fires before OnChange event and "catches" the key press
begin
    if (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; //catch and discard key press to avoid beep
        DoFillets(0);
    end
    else if (not RadioArcsRelative.Checked) and (Ord(Key) = 13) then
    begin
        ShowInfo('Negative values only allowed for Relative arc mode and fixed radius. OK button disabled.');
    end
    else
    begin
        ButtonOK.Enabled := IsStringANum(tRadius.Text, RadioArcsRelative.Checked and (not RadioUnitsRatio.Checked));
    end;
end;


procedure TFormFillet.ButtonOKClick(Sender: TObject);
begin
    DoFillets(1);
end;


procedure TFormFillet.LabelVersionClick(Sender : TObject);
begin
    About;
end;


procedure TFormFillet.PresetButtonClicked(Sender: TObject);
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
