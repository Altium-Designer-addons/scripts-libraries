'----------------------------------------------------
' Draw rounded rectangle spiral track
' Copyright (c) 2005 Mooretronics Pty Ltd
' Melbourne Australia
'----------------------------------------------------
' This script will draw a trace around a user defined rectangle.
' User enters the size of the rectangle to draw tracks around,
' the clearance, track width, track to track clearance and number
' of turns to draw.
' Metric support added
' Mouse select start point added
' Improve dialog, DXP style
' get current layer
' much faster calls now
' grouped undo
' removed Str2Int functions so it can handle non integer values
' fixed metric issue, force change to imperial before placing objects
' make some cosmetic changes
' set dialog to open in middle of screen
' add layer names to Layers list again
'....................................................

' Darren Moore
' May 2005

' Version 0.7

'....................................................

'====================================================
' Spartak Timchev (sparky@omegatim.com)
' August 2012

' Version 0.7b

' * Center of winding is selected instead of start of winding
' * Windings start at 9 o'clock
' * Proper single winding
'====================================================

'====================================================
' Spartak Timchev (sparky@omegatim.com)
' August 22 2012

' Version 0.7c

' * Option to select winding start position
' * Radius cannot be larger than maximum possible
' * Radius optimisation on "left center" windings
' * Arc optimisation on single winding and large radius
' * Option to create as component or as free primitives
' * Layers Combo works now
' * Several interface enhancements
'====================================================

Option Explicit

    Dim   Board
    DIM   xm,ym    'mouse coordinate

'-----------------------------------------------------------------------
Sub PlacePlanarTxFormCreate(Sender)
    SizeX.Color = RGB(236, 233, 216) '&HD8E9EC&
    SizeY.Color = RGB(236, 233, 216)
    CenterX.Color = RGB(236, 233, 216)
    CenterY.Color = RGB(236, 233, 216)
    Spacing.Color = RGB(236, 233, 216)
    ClearanceInside.Color = RGB(236, 233, 216)
    SizeRadius.Color = RGB(236, 233, 216)
    TrackWidth.Color = RGB(236, 233, 216)
    Loops.Color = RGB(236, 233, 216)
End Sub
'-----------------------------------------------------------------------

Sub XPBitBtn1Click(Sender)
    dim House,AX,AY

    ' force values to imperial
    Metric.Checked = false
    AX  =  (CenterX.Text * 1) + (Board.XOrigin/10000)
    AY  =  (CenterY.Text * 1) + (Board.YOrigin/10000)

    PCBServer.PreProcess
    set House = Nothing
    if CreateGroup.ItemIndex = 0 then
        House = PCBServer.PCBObjectFactory(eComponentObject, eNoDimension, eCreate_Default)
        'Set the reference point of the Component
        House.X             = MilsToCoord(AX)
        House.Y             = MilsToCoord(AY)
        House.Layer         = eTopLayer
        House.PrimitiveLock = False
        'Designator text
        House.NameOn        = False
        House.Name.Text     = "Winding"
        'Comment text
        House.CommentOn     = False
        House.Comment.Text  = "Comment"
    else
        set House = Board
    end if

    If House is Nothing then
       Close
       exit sub
    end if

    select case StartGroup.ItemIndex
        case 0: CreateLeftTop House,AX,AY
        case 2: CreateLeftBottom House,AX,AY
        case else: CreateLeftCenter House,AX,AY
    end select

    if CreateGroup.ItemIndex = 0 then
       PCBServer.SendMessageToRobots Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,House.I_ObjectAddress
       Board.AddPCBObject(House)
    end if

    Pcbserver.PostProcess
    ' Refresh PCB workspace.
    ResetParameters
    Call AddStringParameter("Action", "Redraw")
    RunProcess("PCB:Zoom")
    Close
End Sub

sub CreateLeftTop(House,AX,AY)
    DIM   NowX, NowY, RadiusNow, Clearance
    DIM   WLength, WHeight, FLength, FHeight, Radius, TrackSpacing, TWidth, Windings, Layer, Track, Arc
    dim   p0

    if Windings=1 then
       CreateLeftCenter House,AX,AY
       exit sub
    end if

    WLength      =  (SizeX.Text * 1)
    WHeight      =  (SizeY.Text * 1)
    Radius       =  (SizeRadius.Text * 1)
    If Radius > (WLength / 2) then Radius = (WLength / 2)
    If Radius > (WHeight / 2) then Radius = (WHeight / 2)
    TWidth       =  (TrackWidth.Text)
    Clearance    =  ((ClearanceInside.Text) * 1)
    FLength      =  (WLength - (Radius * 2))
    FHeight      =  (WHeight - (Radius * 2))
    TrackSpacing =  (Spacing.Text * 1)
    Layer        =  String2Layer(LayerListBox.Text)
    Windings     =  ((Loops.Text) * 1)
    RadiusNow    =  Radius + Clearance + (TWidth / 2)

    NowX         =  (AX - (FLength / 2))
    NowY         =  (AY + (WHeight / 2) + Clearance + (TWidth / 2))

    PlacePlanarTxForm.Hide

    FOR p0 = 1 TO Windings

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX + FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX + FLength

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY - RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 0
        Arc.EndAngle   = 90
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY - FHeight)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowY           = NowY - FHeight

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX - RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 270
        Arc.EndAngle   = 0
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX - FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX - FLength

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY + RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 180
        Arc.EndAngle   = 270
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY + RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY + FHeight + TWidth + TrackSpacing)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowY           = NowY + FHeight + TWidth + TrackSpacing

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX + RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 90
        Arc.EndAngle   = 180
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY + RadiusNow

        RadiusNow      = RadiusNow + TrackSpacing + TWidth

    NEXT
end sub

sub CreateLeftCenter(House,AX,AY)
    DIM   NowX, NowY, RadiusNow, Clearance
    DIM   WLength, WHeight, FLength, FHeight, Radius, TrackSpacing, TWidth, Windings, Layer, Track, Arc
    dim   p0
    dim   RadInc1, RadInc2, Angle

    WLength      =  (SizeX.Text * 1)
    WHeight      =  (SizeY.Text * 1)
    Radius       =  (SizeRadius.Text * 1)
    If Radius > (WLength / 2) then Radius = (WLength / 2)
    If Radius > (WHeight / 2) then Radius = (WHeight / 2)
    TWidth       =  (TrackWidth.Text)
    Clearance    =  ((ClearanceInside.Text) * 1)
    FLength      =  (WLength - (Radius * 2))
    FHeight      =  (WHeight - (Radius * 2))
    TrackSpacing =  (Spacing.Text * 1)
    Layer        =  String2Layer(LayerListBox.Text)
    Windings     =  ((Loops.Text) * 1)
    RadiusNow    =  Radius + Clearance + (TWidth / 2)

    NowX         =  (AX - (WLength / 2) - Clearance - (TWidth / 2))
    NowY         =  (AY)
    Angle        =  90
    RadInc1      =  TrackSpacing + TWidth
    RadInc2      =  RadInc1
    if (RadiusNow + RadInc1) > ((WHeight / 2) + Clearance + (TWidth / 2)) then
       RadInc1 = (WHeight / 2) + Clearance + (TWidth / 2) - RadiusNow
    end if
    RadInc2      = RadInc2 - RadInc1

    if Windings = 1 then
       NowY = NowY + (TWidth / 2) + (TrackSpacing / 2)
       RadInc1 = 0
       RadInc2 = 0
       If RadiusNow > ((WHeight / 2) + Clearance - (TrackSpacing / 2)) then
          Angle = (180 * ArcCos(1-(((WHeight / 2) + Clearance - (TrackSpacing / 2)) / RadiusNow)))/3.1415926535897932
       end if
    end if

    PlacePlanarTxForm.Hide

    FOR p0 = 1 TO Windings

        If Angle = 90 then
           Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
           Track.Width    = MilsToCoord(TWidth)
           Track.X1       = MilsToCoord(NowX)
           Track.Y1       = MilsToCoord(NowY)
           Track.X2       = MilsToCoord(NowX)
           Track.Y2       = MilsToCoord(NowY + (FHeight / 2))
           if Windings = 1 then Track.Y2 = Track.Y2 - MilsToCoord((TWidth / 2) - (TrackSpacing / 2))
           Track.Layer    = Layer
           House.AddPCBObject(Track)
        end if

        NowY           = NowY + (FHeight / 2)
        if Windings = 1 then NowY = NowY - (TWidth / 2) - (TrackSpacing / 2)

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX + RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 90
        Arc.EndAngle   = 90 + Angle
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY + RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX + FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX + FLength

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY - RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 0
        Arc.EndAngle   = 90
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY - FHeight)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowY           = NowY - FHeight

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX - RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 270
        Arc.EndAngle   = 0
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX - FLength - RadInc2)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX - FLength - RadInc2

        RadiusNow      = RadiusNow + RadInc1

        'if Windings <> 1 then RadiusNow = RadiusNow + TrackSpacing + TWidth

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY + RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 270 - Angle
        Arc.EndAngle   = 270
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY + RadiusNow

        if Angle = 90 then
            Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
            Track.Width    = MilsToCoord(TWidth)
            Track.X1       = MilsToCoord(NowX)
            Track.Y1       = MilsToCoord(NowY)
            Track.X2       = MilsToCoord(NowX)
            Track.Y2       = MilsToCoord(AY) 'NowY + FHeight/2 - TrackSpacing - TWidth)
            if Windings = 1 then Track.Y2 = Track.Y2 - MilsToCoord((TWidth / 2) + (TrackSpacing / 2))
            Track.Layer    = Layer
            House.AddPCBObject(Track)
        end if

        NowY           = AY 'NowY + (FHeight / 2) - TrackSpacing - TWidth

        RadiusNow = RadiusNow + RadInc2

    NEXT
end sub

sub CreateLeftBottom(House,AX,AY)
    DIM   NowX, NowY, RadiusNow, Clearance
    DIM   WLength, WHeight, FLength, FHeight, Radius, TrackSpacing, TWidth, Windings, Layer, Track, Arc
    dim   p0

    if Windings = 1 then
        CreateLeftCenter House,AX,AY
        exit sub
    end if

    WLength      =  (SizeX.Text * 1)
    WHeight      =  (SizeY.Text * 1)
    Radius       =  (SizeRadius.Text * 1)
    If Radius > (WLength / 2) then Radius = (WLength / 2)
    If Radius > (WHeight / 2) then Radius = (WHeight / 2)
    TWidth       =  (TrackWidth.Text)
    Clearance    =  ((ClearanceInside.Text) * 1)
    FLength      =  (WLength - (Radius * 2))
    FHeight      =  (WHeight - (Radius * 2))
    TrackSpacing =  (Spacing.Text * 1)
    Layer        =  String2Layer(LayerListBox.Text)
    Windings     =  ((Loops.Text) * 1)
    RadiusNow    =  Radius + Clearance + (TWidth / 2)

    NowX         =  (AX - (WLength / 2) - Clearance - (TWidth / 2))
    NowY         =  (AY - (FHeight / 2))

    PlacePlanarTxForm.Hide

    FOR p0 = 1 TO Windings

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY + FHeight)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowY           = NowY + FHeight

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX + RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 90
        Arc.EndAngle   = 180
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY + RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX + FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX + FLength

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY - RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 0
        Arc.EndAngle   = 90
        House.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY - FHeight)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowY           = NowY - FHeight

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX - RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 270
        Arc.EndAngle   = 0
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX - FLength - TWidth - TrackSpacing)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        House.AddPCBObject(Track)

        NowX           = NowX - FLength - TWidth - TrackSpacing

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY + RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 180
        Arc.EndAngle   = 270
        House.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY + RadiusNow
        RadiusNow      = RadiusNow + TrackSpacing + TWidth

    NEXT
end sub

'-----------------------------------------------------------------------
Sub MetricClick(Sender)
    If Metric.Checked Then
        CenterX.Text = (CenterX.Text / 39.37)
        CenterY.Text = (CenterY.Text / 39.37)
        SizeX.Text = (SizeX.Text / 39.37)
        SizeY.Text = (SizeY.Text / 39.37)
        Spacing.Text = (Spacing.Text / 39.37)
        ClearanceInside.Text = (ClearanceInside.Text / 39.37)
        SizeRadius.Text = (SizeRadius.Text / 39.37)
        TrackWidth.Text = (TrackWidth.Text / 39.37)
    Else
        CenterX.Text = (CenterX.Text * 39.37)
        CenterY.Text = (CenterY.Text * 39.37)
        SizeX.Text = (SizeX.Text * 39.37)
        SizeY.Text = (SizeY.Text * 39.37)
        Spacing.Text = (Spacing.Text * 39.37)
        ClearanceInside.Text = (ClearanceInside.Text * 39.37)
        SizeRadius.Text = (SizeRadius.Text * 39.37)
        TrackWidth.Text = (TrackWidth.Text * 39.37)
    End If
End Sub

'-----------------------------------------------------------------------
Sub Main
    Set Board = PCBServer.GetCurrentPCBBoard
    If Board is Nothing Then Exit Sub
    If Board.ChooseLocation(xm,ym,"Choose PlanarTx center location") Then
       PlacePlanarTxForm.showmodal
    End If
End Sub

'-----------------------------------------------------------------------
Sub XPBitBtn2Click(Sender)
    close
End Sub

Sub PlacePlanarTxFormShow(Sender)
    dim LayerIterator
    xm = (xm - Board.XOrigin)/10000               'convert to mils, offset to relative origin
    ym = (ym - Board.YOrigin)/10000               '

    PlacePlanarTxForm.CenterX.Text = xm
    PlacePlanarTxForm.CenterY.Text = ym

    LayerIterator = Board.ElectricalLayerIterator
    While LayerIterator.Next
        LayerListBox.Items.AddObject Layer2String(LayerIterator.LayerObject.LayerID),""
    Wend
    PlacePlanarTxForm.LayerListBox.Text = Layer2String(Board.CurrentLayer)    'get current layer

    If (Board.DisplayUnit) Then
        PlacePlanarTxForm.Metric.Checked = false
    Else
        PlacePlanarTxForm.Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
    End If

End Sub


