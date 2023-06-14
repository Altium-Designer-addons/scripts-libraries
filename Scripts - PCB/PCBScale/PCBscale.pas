{..............................................................................}
{                                                                              }
{ Summary   This script can be used to scale selected PCB Objects.             }
{                                                                              }
{           Known issues:                                                      }
{           - Does not work for dimensions and coordinates at the moment       }
{           - scales only extruded 3D models (I do not know how to get radius) }
{                                                                              }
{           Changelog:                                                         }
{           - v1.0 - Initial Release                                           }
{                                                                              }
{           - v1.1 - updated for text objects in AD19+, some format cleanup    }
{                                                                              }
{           - v2.0 - some changes (wishes for forum)                           }
{           - added Pre- and Post Process for Undo                             }
{           - added edit fields for entering new Boardsize                     }
{             (the scale factor will recalculated from this value              }
{           - added combo box to select Ref Point (center was wanted)          }
{                                                                              }
{           - v2.1 - merged V2 from forum with v1.1                            }
{           - changed anchor selection and added more options                  }
{           - streamlined some operations to refocus on scale input            }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{ Updated by:    Ryan Rutledge (with motivation by Dennis Saputelli)           }
{..............................................................................}

{..............................................................................}
var
    X, Y            : TCoord;
    Ratio           : Float;
    Board           : IPCB_Board;
    IsAtLeastAD19   : Boolean;

const
    ScriptVersion = '2.1';
    ScriptTitle = 'PCBScale';


procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak + sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries';

    ShowInfo(MsgText, 'About');
end;


procedure TFormPCBscale.LabelVersionClick(Sender: TObject);
begin
    About;
end;


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

    if Length(Text) = 0 then Result := False;
end;


{ IPCB_Text inspector for debugging }
procedure Inspect_IPCB_Text(var Text : IPCB_Text3, const MyLabel : string = '');
var
    actualTextBr    : TCoordRect;
    sBrText         : String;
begin
    if Text = nil then
    begin
        ShowError('Text object is nil');
        exit;
    end;

    actualTextBr := Text.GetActualTextBr;
    sBrText := Format('GetActualTextBr[x1,y1;x2,y2]: [%d,%d;%d,%d]', [ actualTextBr.x1, actualTextBr.y1, actualTextBr.x2, actualTextBr.y2 ]);

    ShowInfo('DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------' + sLineBreak +
                Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
                Format('%s : %s', ['AllowGlobalEdit',  BoolToStr(Text.AllowGlobalEdit, True)]) + sLineBreak +
                Format('%s : %s', ['Descriptor',  Text.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail',  Text.Detail]) + sLineBreak +
                Format('%s : %s', ['EnableDraw',  BoolToStr(Text.EnableDraw, True)]) + sLineBreak +
                Format('%s : %s', ['FontID',  IntToStr(Text.FontID)]) + sLineBreak +
                Format('%s : %s', ['Handle',  Text.Handle]) + sLineBreak +
                Format('%s : %s', ['Identifier',  Text.Identifier]) + sLineBreak +
                Format('%s : %s', ['IsSaveable',  BoolToStr(Text.IsSaveable(eAdvPCBFormat_Binary_V6), True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1',  BoolToStr(Text.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2',  BoolToStr(Text.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3',  BoolToStr(Text.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['MultiLine',  BoolToStr(Text.Multiline, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextAutoPosition',  IntToStr(Text.MultilineTextAutoPosition)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextHeight',  IntToStr(Text.MultilineTextHeight)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Text.MultilineTextResizeEnabled, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextWidth',  IntToStr(Text.MultilineTextWidth)]) + sLineBreak +
                Format('%s : %s', ['ObjectId',  IntToStr(Text.ObjectId)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString',  Text.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Text.PadCacheRobotFlag, True)]) + sLineBreak +
                Format('%s : %s', ['SnapPointX',  IntToStr(Text.SnapPointX)]) + sLineBreak +
                Format('%s : %s', ['SnapPointY',  IntToStr(Text.SnapPointY)]) + sLineBreak +
                Format('%s : %s', ['StringXPosition',  IntToStr(Text.StringXPosition)]) + sLineBreak +
                Format('%s : %s', ['StringYPosition',  IntToStr(Text.StringYPosition)]) + sLineBreak +
                sBrText + sLineBreak +
                Format('%s : %s', ['Text',  Text.Text]) + sLineBreak +
                Format('%s : %s', ['TextKind',  IntToStr(Text.TextKind)]) + sLineBreak +
                Format('%s : %s', ['TTFInvertedTextJustify',  IntToStr(Text.TTFInvertedTextJustify)]) + sLineBreak +
                Format('%s : %s', ['UseTTFonts',  BoolToStr(Text.UseTTFonts, True)]) + sLineBreak +
                Format('%s : %s', ['Used',  BoolToStr(Text.Used, True)]) + sLineBreak +
                Format('%s : %s', ['UserRouted',  BoolToStr(Text.UserRouted, True)]) + sLineBreak +
                Format('%s : %s', ['ViewableObjectID',  IntToStr(Text.ViewableObjectID)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak
                , 'IPCB_Text Info (partial)');
end;


// V2 new helper function to update the values in the Editboxes and the Label
procedure UpdateFields(Sender: TObject);
var
    BR: TCoordRect;
    TempString : String;
    Scale: float;
begin
    BR := Board.BoardOutline.BoundingRectangle;
    TempString := EditRatio.Text;
    if not IsStringANum(TempString) then exit;
    if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
    scale:= StrToFloat(TempString);
    if RBmm.Checked then
    begin
        LabelActualBoardSize.Caption := 'Boardsize = '+FloatToStr(CoordToMMs(BR.right-BR.left))+ 'mm x ' + FloatToStr(CoordToMMs(BR.top-BR.bottom))+'mm';
        ENewWidth.Text := CoordToMMs((BR.right-BR.left)*Scale);
        ENewHeight.Text:= CoordToMMs((BR.top-BR.bottom)*Scale);
    end
    else
    begin
        LabelActualBoardSize.Caption := 'Boardsize = '+FloatToStr(CoordToMils(BR.right-BR.left))+ 'mil x ' + FloatToStr(CoordToMils(BR.top-BR.bottom))+'mil';
        ENewWidth.Text := CoordToMils((BR.right-BR.left)*Scale);
        ENewHeight.Text:= CoordToMils((BR.top-BR.bottom)*Scale);
    end;

    if FormPCBscale.Showing and ((Sender = RBmm) or (Sender = RBmil)) then EditRatio.SetFocus;
end;



procedure TFormPCBscale.ButtonCancelClick(Sender: TObject);
begin
    close;
end;



procedure TFormPCBscale.EditRatioChange(Sender: TObject);
begin
    if IsStringANum(EditRatio.Text) then
    begin
        EditRatio.Font.Color := clWindowText;
        ButtonOK.Enabled := True;
        UpdateFields(Sender);
    end
    else
    begin
        ButtonOK.Enabled := False;
        EditRatio.Font.Color := clRed;
    end;
end;


// Done
Procedure ScaleTrack(Prim : IPCB_Track);
begin
    Prim.x1    := X + Int(Ratio * (Prim.x1 - X));
    Prim.x2    := X + Int(Ratio * (Prim.x2 - X));
    Prim.y1    := Y + Int(Ratio * (Prim.y1 - Y));
    Prim.y2    := Y + Int(Ratio * (Prim.y2 - Y));
    Prim.Width := Int(Prim.Width * Ratio);
end;

// Done
Procedure ScaleArc(Prim : IPCB_Arc);
begin
    Prim.XCenter   := X + Int(Ratio * (Prim.XCenter - X));
    Prim.YCenter   := Y + Int(Ratio * (Prim.YCenter - Y));
    Prim.Radius    := Int(Prim.Radius * Ratio);
    Prim.LineWidth := Int(Prim.LineWidth * Ratio);
end;

// Done
Procedure ScalePad(Prim : IPCB_Pad2);
var
    i : Integer;
begin
    Prim.x := X + Int(Ratio * (Prim.x - X));
    Prim.y := Y + Int(Ratio * (Prim.y - Y));
    Prim.HoleSize := Int(Prim.HoleSize * Ratio);
    if Prim.HoleType = eSlotHole then
        Prim.HoleWidth := Int(Prim.HoleWidth * Ratio);

    // I'll do pad later - It's complicated
    if Prim.Mode = ePadMode_Simple then
    begin
        Prim.TopXSize := Int(Prim.TopXSize * Ratio);
        Prim.TopYSize := Int(Prim.TopYSize * Ratio);
    end
    else if Prim.Mode = ePadMode_LocalStack then
    begin
        Prim.TopXSize := Int(Prim.TopXSize * Ratio);
        Prim.TopYSize := Int(Prim.TopYSize * Ratio);
        Prim.MidXSize := Int(Prim.MidXSize * Ratio);
        Prim.MidYSize := Int(Prim.MidYSize * Ratio);
        Prim.BotXSize := Int(Prim.BotXSize * Ratio);
        Prim.BotYSize := Int(Prim.BotYSize * Ratio);
    end
    else
    begin
        Prim.XSizeOnLayer[String2Layer('Top Layer')]         := Int(Prim.XSizeOnLayer[String2Layer('Top Layer')] * Ratio);
        Prim.YSizeOnLayer[String2Layer('Top Layer')]         := Int(Prim.YSizeOnLayer[String2Layer('Top Layer')] * Ratio);
        Prim.XStackSizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.XStackSizeOnLayer[String2Layer('Top Layer')] * Ratio);
        Prim.YStackSizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.YStackSizeOnLayer[String2Layer('Top Layer')] * Ratio);
        Prim.XPadOffset[String2Layer('Top Layer')]           := Int(Prim.XPadOffset[String2Layer('Top Layer')] * Ratio);
        Prim.YPadOffset[String2Layer('Top Layer')]           := Int(Prim.YPadOffset[String2Layer('Top Layer')] * Ratio);
        Prim.XSizeOnLayer[String2Layer('Bottom Layer')]      := Int(Prim.XSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
        Prim.YSizeOnLayer[String2Layer('Bottom Layer')]      := Int(Prim.YSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
        Prim.XStackSizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.XStackSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
        Prim.YStackSizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.YStackSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
        Prim.XPadOffset[String2Layer('Bottom Layer')]        := Int(Prim.XPadOffset[String2Layer('Bottom Layer')] * Ratio);
        Prim.YPadOffset[String2Layer('Bottom Layer')]        := Int(Prim.YPadOffset[String2Layer('Bottom Layer')] * Ratio);

        for i := 1 to 30 do
        begin
            Prim.XSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))]      := Int(Prim.XSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
            Prim.YSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))]      := Int(Prim.YSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
            Prim.XStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.XStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
            Prim.YStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.YStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
            Prim.XPadOffset[String2Layer('Mid Layer ' + IntToStr(i))]        := Int(Prim.XPadOffset[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
            Prim.YPadOffset[String2Layer('Mid Layer ' + IntToStr(i))]        := Int(Prim.YPadOffset[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
        end;
    end;
end;

// Done
Procedure ScaleVia(Prim : IPCB_Via);
var
    i : Integer;
begin
    Prim.x := X + Int(Ratio * (Prim.x - X));
    Prim.y := Y + Int(Ratio * (Prim.y - Y));
    Prim.HoleSize := Int(Prim.HoleSize * Ratio);
    Prim.Size :=     Int(Prim.Size * Ratio);

    if Prim.Mode <> ePadMode_Simple then
    begin
        Prim.SizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.SizeOnLayer[String2Layer('Top Layer')] * Ratio);
        Prim.SizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.SizeOnLayer[String2Layer('Bottom Layer')] * Ratio);

        for i := 1 to 30 do
            Prim.SizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.SizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
    end;
end;

// Done
Procedure ScaleFill(Prim : IPCB_Fill);
begin
    Prim.X1Location := X + Int(Ratio * (Prim.X1Location - X));
    Prim.X2Location := X + Int(Ratio * (Prim.X2Location - X));
    Prim.Y1Location := Y + Int(Ratio * (Prim.Y1Location - Y));
    Prim.Y2Location := Y + Int(Ratio * (Prim.Y2Location - Y));
end;

// Done
Procedure ScaleRegion(Prim : IPCB_Region);
var
    i, j : Integer;
    Contour : IPCB_Contour;
    Xa, Ya  : Integer;
    Prim2   : IPCB_Region;
begin
    Contour := PCBServer.PCBContourFactory;
    Prim2 := Prim.Replicate;

    for i := 1 to Prim.MainContour.Count do
    begin
        Xa := X + Int(Ratio * (Prim.MainContour.x[i] - X));
        Ya := Y + Int(Ratio * (Prim.MainContour.y[i] - Y));
        Contour.AddPoint(Xa, Ya);
    end;

    Prim.SetOutlineContour(Contour);
    Contour.Clear;

    for j := 0 to Prim2.HoleCount - 1 do
    begin
        for i := 1 to Prim2.Holes[j].Count do
        begin
            Xa := X + Int(Ratio * (Prim2.Holes[j].x[i] - X));
            Ya := Y + Int(Ratio * (Prim2.Holes[j].y[i] - Y));
            Contour.AddPoint(Xa, Ya);
        end;
        Prim.GeometricPolygon.AddContourIsHole(Contour, True);
        Contour.Clear;
    end;

    Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScaleText(Prim : IPCB_Text);
begin
    //Inspect_IPCB_Text(Prim);
    if IsAtLeastAD19 then
    begin
        Prim.SnapPointX := X + Int(Ratio * (Prim.SnapPointX - X));
        Prim.SnapPointY := Y + Int(Ratio * (Prim.SnapPointY - Y));
    end
    else begin
        Prim.XLocation := X + Int(Ratio * (Prim.XLocation - X));
        Prim.YLocation := Y + Int(Ratio * (Prim.YLocation - Y));
    end;

    Prim.Size      := Int(Prim.Size  * Ratio);
    Prim.Width     := Int(Prim.Width * Ratio);
    If Prim.Inverted then
        if Prim.UseInvertedRectangle then
        begin
            Prim.InvRectHeight := Int(Prim.InvRectHeight * Ratio);
            Prim.InvRectWidth  := Int(Prim.InvRectWidth  * Ratio);
            Prim.TTFOffsetFromInvertedRect := Int(Prim.TTFOffsetFromInvertedRect  * Ratio);
        end
        else
            Prim.InvertedTTTextBorder := Int(Prim.InvertedTTTextBorder * Ratio);

    if Prim.TextKind = eText_BarCode then
    begin
        Prim.BarCodeFullHeight := Int(Prim.BarCodeFullHeight * Ratio);
        Prim.BarCodeXMargin  := Int(Prim.BarCodeXMargin  * Ratio);
        Prim.BarCodeYMargin  := Int(Prim.BarCodeYMargin  * Ratio);
        if Prim.BarCodeRenderMode = eRender_ByMinWidth then
            Prim.BarCodeMinWidth := Int(Prim.BarCodeMinWidth * Ratio)
        else
            Prim.BarCodeFullWidth := Int(Prim.BarCodeFullWidth * Ratio);
    end;
    Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScalePoly(Polygon : IPCB_Polygon);
var
    i    : Integer;
    Iter : IPCB_GroupIterator;
    Prim : IPCB_Primitive;
    NewSegment : TPolySegment;
begin
    For I := 0 To Polygon.PointCount Do
    begin
        NewSegment := TPolySegment;
        NewSegment.Kind := Polygon.Segments[I].Kind;
        If Polygon.Segments[I].Kind = ePolySegmentArc then
        begin
            NewSegment.cx := X + Int(Ratio * (Polygon.Segments[I].cx - X));
            NewSegment.cy := Y + Int(Ratio * (Polygon.Segments[I].cy - Y));
            NewSegment.Radius := Int(Polygon.Segments[I].Radius * Ratio);
        end
        else
        begin
            NewSegment.vx := X + Int(Ratio * (Polygon.Segments[I].vx - X));
            NewSegment.vy := Y + Int(Ratio * (Polygon.Segments[I].vy - Y));
        end;
        Polygon.Segments[I] := NewSegment;
    end;

    Iter := Polygon.GroupIterator_Create;

    Prim := Iter.FirstPCBObject;
    While Prim <> nil do
    begin
        Prim.BeginModify;
        case Prim.ObjectId of
            eTrackObject         : ScaleTrack(Prim);
            eArcObject           : ScaleArc(Prim);
            eRegionObject        : ScaleRegion(Prim);
        end;
        Prim.EndModify;
        Prim.GraphicallyInvalidate;
        Prim := Iter.NextPCBObject;
    end;
    Polygon.GroupIterator_Destroy(Iter);
    Polygon.GraphicallyInvalidate;
end;

// Can not read radius - can't scale cylinder - so - Done
Procedure Scale3DModel(Prim : IPCB_ComponentBody);
var
    Xa, Ya  : TCoord;
    Radius  : TCoord;
    Contour : IPCB_Contour;
    i       : Integer;
begin
    Xa := Prim.BoundingRectangle.Right - Prim.BoundingRectangle.Left;
    Ya := Prim.BoundingRectangle.Top   - Prim.BoundingRectangle.Bottom;


    if Prim.Model.ModelType = e3DModelType_Extruded then
    begin
        Contour := PCBServer.PCBContourFactory;
        for i := 1 to Prim.MainContour.Count do
        begin
            Xa := X + Int(Ratio * (Prim.MainContour.x[i] - X));
            Ya := Y + Int(Ratio * (Prim.MainContour.y[i] - Y));
            Contour.AddPoint(Xa, Ya);
        end;

        Prim.SetOutlineContour(Contour);
        Prim.OverallHeight  := Int(Prim.OverallHeight * Ratio);
        Prim.StandoffHeight := Int(Prim.StandoffHeight * Ratio);
    end
    else if Prim.Model.ModelType = e3DModelType_Cylinder then
    begin
        Radius := Int(Xa / 2);
        Prim.MoveByXY(Xa - X, Ya - Y);
        Prim.ModelFactory_UpdateModel(Int(Xa / 2 * Ratio),0,e3DModelType_Cylinder);
    end
    else if Prim.Model.ModelType = e3DModelType_Sphere then
    begin
        Prim.MoveByXY(Xa - X, Ya - Y);
    end
    else if Prim.Model.ModelType = e3DModelType_Generic then
    begin
        Prim.MoveByXY(Xa - X, Ya - Y);
    end;
    Prim.GraphicallyInvalidate;
end;

// Not done
Procedure ScaleDimension(Prim : IPCB_Primitive);
begin
    exit;   // not done
end;

// Not done - bad placement and can not refresh
Procedure ScaleCoordinate(Prim : IPCB_Coordinate);
begin
    exit;   // not done

    Prim.LineWidth  := Int(Prim.LineWidth * Ratio);
    Prim.Size       := Int(Prim.Size * Ratio);
    Prim.MoveByXY(Int((Prim.BoundingRectangle.Left - X + (Prim.Size + Prim.LineWidth) / 2) * Ratio), Int((Prim.BoundingRectangle.Bottom - Y + (Prim.Size + Prim.LineWidth) / 2) * Ratio));

    Prim.TextHeight := Int(Prim.TextHeight * Ratio);
    if not Prim.UseTTFonts then
        Prim.TextWidth := Int(Prim.TextWidth * Ratio);

    Prim.Track1.Width := Prim.LineWidth;
    Prim.Track2.Width := Prim.LineWidth;
    Prim.Text.Size := Prim.Size;
    if not Prim.UseTTFonts then
        Prim.Text.Width := Prim.TextWidth;

    Prim.Track1.GraphicallyInvalidate;
    Prim.Track2.GraphicallyInvalidate;
    Prim.Text.GraphicallyInvalidate;

end;

// Done
Procedure ScaleRoom(Prim : IPCB_ConfinementConstraint);
var
    i    : Integer;
    Iter : IPCB_GroupIterator;
    NewSegment : TPolySegment;
begin
    For I := 0 To Prim.PointCount Do
    begin
        NewSegment := TPolySegment;
        NewSegment.Kind := Prim.Segments[I].Kind;
        If Prim.Segments[I].Kind = ePolySegmentArc then
        begin
            NewSegment.cx := X + Int(Ratio * (Prim.Segments[I].cx - X));
            NewSegment.cy := Y + Int(Ratio * (Prim.Segments[I].cy - Y));
            NewSegment.Radius := Int(Prim.Segments[I].Radius * Ratio);
        end
        else
        begin
            NewSegment.vx := X + Int(Ratio * (Prim.Segments[I].vx - X));
            NewSegment.vy := Y + Int(Ratio * (Prim.Segments[I].vy - Y));
        end;
        Prim.Segments[I] := NewSegment;
    end;

    Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScaleComponent(Component : IPCB_Component);
var
    Iter : IPCB_GroupIterator;
    Prim : IPCB_Primitive;
    TempX, TempY : TCoord;
begin

    Component.x := X + Int(Ratio * (Component.x - X));
    Component.y := Y + Int(Ratio * (Component.y - Y));

    TempX := X;
    TempY := Y;

    X := Component.x;
    Y := Component.y;

    ScaleText(Component.Name);
    ScaleText(Component.Comment);

    Iter := Component.GroupIterator_Create;

    Prim := Iter.FirstPCBObject;
    While Prim <> nil do
    begin
        Prim.BeginModify;
        case Prim.ObjectId of
            eTrackObject         : ScaleTrack(Prim);
            eArcObject           : ScaleArc(Prim);
            ePadObject           : ScalePad(Prim);
            eViaObject           : ScaleVia(Prim);
            eFillobject          : ScaleFill(Prim);
            eRegionObject        : ScaleRegion(Prim);
            eTextObject          : ScaleText(Prim);
            ePolyObject          : ScalePoly(Prim);
            eComponentBodyObject : Scale3DModel(Prim);
            eDimensionObject     : ScaleDimension(Prim);    // not done
            eCoordinateObject    : ScaleCoordinate(Prim);   // not done
        end;
        Prim.EndModify;
        Prim.GraphicallyInvalidate;

        Prim := Iter.NextPCBObject;
    end;
    Component.GroupIterator_Destroy(Iter);

    X := TempX;
    Y := TempY;
end;


// V2 a new width for the boardsize was entered recalculate Scalefactor and update fields
procedure TFormPCBscale.ENewWidthExit(Sender: TObject);
var
    TempString : String;
    Scale: float;
    BR: TCoordRect;
    NewWidth: integer;
begin
    BR := Board.BoardOutline.BoundingRectangle;
    TempString := ENewWidth.Text;
    if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
    if RBmm.Checked then
        NewWidth:= mmstocoord(StrToFloat(TempString))
    else
        NewWidth:= milstocoord(StrToFloat(TempString));
    Scale:=NewWidth/(BR.right-BR.left);
    EditRatio.Text:=floattostrf(Scale,ffGeneral,5,0);
    UpdateFields(Sender);
end;

// V2 a new Height for the boardsize was entered recalculate Scalefactor and update fields
procedure TFormPCBscale.ENewHeightExit(Sender: TObject);
var
    TempString : String;
    Scale: float;
    BR: TCoordRect;
    NewHeight: integer;
begin
    BR := Board.BoardOutline.BoundingRectangle;
    TempString := ENewHeight.Text;
    if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
    if RBmm.Checked then
        NewHeight:= mmstocoord(StrToFloat(TempString))
    else
        NewHeight:= milstocoord(StrToFloat(TempString));
    Scale:=NewHeight/(BR.top-BR.bottom);
    EditRatio.Text:=floattostrf(Scale,ffGeneral,5,0);
    UpdateFields(Sender);
end;


procedure FocusEditRatio(Sender : TObject);
begin
    EditRatio.SetFocus;
end;


function GetAnchorChoice(GB : TGroupBox) : TRadioButton;
var
    i : Integer;
    radioButton : TRadioButton;
begin
    Result := nil;

    for i:= 0 to GB.ControlCount - 1 do
    begin
        radioButton := GB.Controls[i];
        if radioButton.Checked then
        begin
            Result := radioButton;
            break;
        end;
    end;
end;


procedure TFormPCBscale.GroupBoxAnchorClick(Sender: TObject);
begin
    //ShowInfo(GetAnchorChoice(GroupBoxAnchor).Name);   // for debugging
end;


procedure ScaleSelection(const dummy : Boolean);
var
    Rectang    : TCoordRect;
    TempString : String;
    i          : Integer;
    Prim       : IPCB_Primitive;
    left,right,top,bottom  : Integer;
begin
    Board := PCBServer.GetCurrentPCBBoard;

    if Board = nil then exit;
    if Board.SelectecObjectCount = 0 then exit;

    // V2 added PreProcess for Undo
    PCBServer.PreProcess;       // Initialize robots in PCB Server (for UNDO)

    TempString := EditRatio.Text;
    if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    Ratio := StrToFloat(TempString);

    if not (Ratio > 0) then
    begin
        ShowError('Scale cannot be 0');
        exit;
    end;

    // V2 changed/added find not only left and bottom minimum also right and top maximum (for Ref Point)
    left  := Board.SelectecObject[0].BoundingRectangle.Left;
    right := Board.SelectecObject[0].BoundingRectangle.Right;
    top   := Board.SelectecObject[0].BoundingRectangle.Top;
    bottom:= Board.SelectecObject[0].BoundingRectangle.Bottom;

    // original
    //X := Board.SelectecObject[0].BoundingRectangle.Left;
    //Y := Board.SelectecObject[0].BoundingRectangle.Bottom;
    // get min max for X,Y Coordinates (go thru all selected objects and find most left,right.. coordinate)
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim := Board.SelectecObject[i];
        if left > Prim.BoundingRectangle.Left then
            left := Prim.BoundingRectangle.Left;
        if right < Prim.BoundingRectangle.Right then
            right := Prim.BoundingRectangle.Right;
        if top < Prim.BoundingRectangle.top then
            top := Prim.BoundingRectangle.top;
        if bottom > Prim.BoundingRectangle.Bottom then
            bottom := Prim.BoundingRectangle.Bottom;
    end;

    // V2 added set Ref Point (variable X,Y) according combobox selection
    // RBTopLeft, RBCenterLeft, RBBottomLeft, RBTopCenter, RBCenter, RBBottomCenter, RBTopRight, RBCenterRight, RBBottomRight;
    case GetAnchorChoice(GroupBoxAnchor).Tag of
        1:  // RBTopLeft :
        begin
            X := left;
            Y := top;
        end;
        2:  // RBCenterLeft :
        begin
            X := left;
            Y := bottom+(top-bottom)/2;
        end;
        3:  // RBBottomLeft :
        begin
            X := left;
            Y := bottom;
        end;
        4:  // RBTopCenter :
        begin
            X := left+(right-left)/2;
            Y := top;
        end;
        5:  // RBCenter :
        begin
            X := left+(right-left)/2;
            Y := bottom+(top-bottom)/2;
        end;
        6:  // RBBottomCenter :
        begin
            X := left+(right-left)/2;
            Y := bottom;
        end;
        7:  // RBTopRight :
        begin
            X := right;
            Y := top;
        end;
        8:  // RBCenterRight :
        begin
            X := right;
            Y := bottom+(top-bottom)/2;
        end;
        9:  // RBBottomRight :
        begin
            X := right;
            Y := bottom;
        end;
    end;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim := Board.SelectecObject[i];

        if ((Prim.InComponent or Prim.InCoordinate or Prim.InDimension or Prim.InPolygon) = False) then
        begin
            Prim.BeginModify;
            case Prim.ObjectId of
                eTrackObject                : ScaleTrack(Prim);
                eArcObject                  : ScaleArc(Prim);
                ePadObject                  : ScalePad(Prim);
                eViaObject                  : ScaleVia(Prim);
                eFillobject                 : ScaleFill(Prim);
                eRegionObject               : ScaleRegion(Prim);
                eTextObject                 : ScaleText(Prim);
                eComponentObject            : ScaleComponent(Prim);
                ePolyObject                 : ScalePoly(Prim);
                eComponentBodyObject        : Scale3DModel(Prim);
                eDimensionObject            : ScaleDimension(Prim);
                eCoordinateObject           : ScaleCoordinate(Prim);
                eRuleObject                 : ScaleRoom(Prim);
            end;
            Prim.EndModify;
            Prim.GraphicallyInvalidate;
        end;
    end;
    // V2 added PostProcess for Undo
    Pcbserver.PostProcess;  // Clean up robots in PCB Server    (for UNDO)

    Board.ViewManager_FullUpdate;
    close;
end;


procedure TFormPCBscale.ButtonOKClick(Sender: TObject);
begin
    ScaleSelection(True);
end;


// programmatically, OnKeyPress fires before OnChange event and "catches" the key press
procedure UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if Sender = EditRatio then ScaleSelection(False) else EditRatio.SetFocus;
    end;
end;


Procedure Start;
begin
    Board := PCBServer.GetCurrentPCBBoard;

    if Board = nil then exit;
    if Board.SelectecObjectCount = 0 then
    begin
        ShowMessage('There are no selected objects');
        exit;
    end;
    // V2 update Editboxes and Label before showing the form
    UpdateFields(nil);

    // set version label
    LabelVersion.Caption := '"' + ScriptTitle + '" v' + ScriptVersion;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    FormPCBscale.ShowModal;
end;

