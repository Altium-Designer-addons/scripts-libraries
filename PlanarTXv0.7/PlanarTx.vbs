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

Option Explicit

    Dim   Board
    DIM   xm,ym    'mouse coordinate

'-----------------------------------------------------------------------
Sub PlacePlanarTxFormCreate(Sender)

    'Set Board = PCBServer.GetCurrentPCBBoard
    'If Board is Nothing Then Exit Sub

    'If Not Board.ChooseLocation(xm,ym,"Choose PlanarTx center location") Then
    'End If

'    xm = (xm - Board.XOrigin)/10000               'convert to mils, offset to relative origin
'    ym = (ym - Board.YOrigin)/10000               '

'    PlacePlanarTxForm.CenterX.Text = xm
'    PlacePlanarTxForm.CenterY.Text = ym

'    PlacePlanarTxForm.LayerListBox.Text = Layer2String(Board.CurrentLayer)    'get current layer

'    If (Board.DisplayUnit) Then
'        PlacePlanarTxForm.Metric.Checked = false
'    Else
'        PlacePlanarTxForm.Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
'    End If

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

    DIM   NowX, NowY, RadiusNow, Clearance
    DIM   WLength, WHeight, FLength, FHeight, Radius, TrackSpacing, TWidth, Windings, Layer, Track, Arc
    dim   p0

    ' force values to imperial
    Metric.Checked = false

    TWidth       =  (TrackWidth.Text)
    Radius       =  (SizeRadius.Text)
    Clearance    =  ((ClearanceInside.Text) * 1)
    WLength      =  (SizeX.Text)
    WHeight      =  (SizeY.Text)
    FLength      =  (WLength - (Radius * 2))
    FHeight      =  (WHeight - (Radius * 2))
    TrackSpacing =  (Spacing.Text)
    Layer        =  String2Layer(LayerListBox.Text)
    Windings     =  ((Loops.Text) * 1)
    NowX         =  (CenterX.Text + (Board.XOrigin/10000) - (WLength / 2) - Clearance - (TWidth / 2))
    NowY         =  (CenterY.Text + (Board.YOrigin/10000))
    RadiusNow    =  Radius + Clearance + (TWidth / 2)

    if Windings = 1 then NowY = NowY + (TWidth / 2) + (TrackSpacing / 2)

    PlacePlanarTxForm.Hide

    PCBServer.PreProcess

    FOR p0 = 1 TO Windings

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY + (FHeight / 2))
        if Windings = 1 then Track.Y2 = Track.Y2 - MilsToCoord((TWidth / 2) - (TrackSpacing / 2))
        Track.Layer    = Layer
        Board.AddPCBObject(Track)

        NowY           = NowY + (FHeight / 2)
        if Windings = 1 then NowY = NowY - (TWidth / 2) - (TrackSpacing / 2)

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX + RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 90
        Arc.EndAngle   = 180
        Board.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY + RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX + FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        Board.AddPCBObject(Track)

        NowX           = NowX + FLength

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY - RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 0
        Arc.EndAngle   = 90
        Board.AddPCBObject(Arc)

        NowX           = NowX + RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY - FHeight)
        Track.Layer    = Layer
        Board.AddPCBObject(Track)

        NowY           = NowY - FHeight

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX - RadiusNow)
        Arc.YCenter    = MilsToCoord(NowY)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 270
        Arc.EndAngle   = 0
        Board.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY - RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX - FLength)
        Track.Y2       = MilsToCoord(NowY)
        Track.Layer    = Layer
        Board.AddPCBObject(Track)

        NowX           = NowX - FLength

        if Windings <> 1 then RadiusNow = RadiusNow + TrackSpacing + TWidth

        Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
        Arc.LineWidth  = MilsToCoord(TWidth)
        Arc.XCenter    = MilsToCoord(NowX)
        Arc.YCenter    = MilsToCoord(NowY + RadiusNow)
        Arc.Radius     = MilsToCoord(RadiusNow)
        Arc.Layer      = Layer
        Arc.StartAngle = 180
        Arc.EndAngle   = 270
        Board.AddPCBObject(Arc)

        NowX           = NowX - RadiusNow
        NowY           = NowY + RadiusNow

        Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
        Track.Width    = MilsToCoord(TWidth)
        Track.X1       = MilsToCoord(NowX)
        Track.Y1       = MilsToCoord(NowY)
        Track.X2       = MilsToCoord(NowX)
        Track.Y2       = MilsToCoord(NowY + FHeight/2 - TrackSpacing - TWidth)
        if Windings = 1 then Track.Y2 = Track.Y2 + MilsToCoord((TWidth / 2) + (TrackSpacing / 2))
        Track.Layer    = Layer
        Board.AddPCBObject(Track)

        NowY           = NowY + (FHeight / 2) - TrackSpacing - TWidth

    NEXT

    Pcbserver.PostProcess
    ' Refresh PCB workspace.
    ResetParameters
    Call AddStringParameter("Action", "Redraw")
    RunProcess("PCB:Zoom")

    Close
End Sub


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
    'Set Board = PCBServer.GetCurrentPCBBoard
    'If Board is Nothing Then Exit Sub

    'If Not Board.ChooseLocation(xm,ym,"Choose PlanarTx center location") Then
    'End If

    xm = (xm - Board.XOrigin)/10000               'convert to mils, offset to relative origin
    ym = (ym - Board.YOrigin)/10000               '

    PlacePlanarTxForm.CenterX.Text = xm
    PlacePlanarTxForm.CenterY.Text = ym

    PlacePlanarTxForm.LayerListBox.Text = Layer2String(Board.CurrentLayer)    'get current layer

    If (Board.DisplayUnit) Then
        PlacePlanarTxForm.Metric.Checked = false
    Else
        PlacePlanarTxForm.Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
    End If


End Sub
