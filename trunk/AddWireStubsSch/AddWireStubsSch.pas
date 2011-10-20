{==============================================================================}
{ ---- Add Wire Stubs to Schematic Pins ---------------------------------------}
{------------------------------------------------------------------------------}
{-                                                                            -}
{-  This scripts aims to add small segments of wires to unconnected pins in   -}
{-  current schematic sheet equiped by net labels related to each pin.        -}
{-  If there is a wire existing on pin no action is done for such pin.        -}
{-  The script was developed for easier drawing of schematics with FPGA       -}
{-  devices where small segments of wires are commonly used to wire down the  -}
{-  schematic symbol for easier pin swapping.                                 -}
{-  Net labels can be easily replaced by final names in SCH List panel (Smart -}
{-  Grid Paste) from pin file or by importing changes from FPGA project.      -}
{-                                                                            -}
{-  Startup procedure  ::: AddWireStubsSchRun :::                             -}
{-                                                                            -}
{-  CUSTOMIZATION can by done by direct edditing of CONSTANTS                 -}
{-                                                                            -}
{-  OnlySelectedComps - script process only selected or all components        -}
{-  StubLength - length of wire segments in multiple of visible grid units    -}
{-  AddNetLabels - script creates Net labels for new wire segments            -}
{-  DesToLabels - pin designator will be copied to net label (otherwise pin   -}
{-                name is used ad net label)                                  -}
{-  LabelOffset - offset of new net labels from pin in up, down, left, right  -}
{-                directions separately                                       -}
{-                                                                            -}
{-  Known issues:                                                             -}
{-     - created net labels can not be rotatet with space.                    -}
{-                                                                            -}
{-     ---  Please report script issues to Petr.Tosovsky@edatools.cz   ---    -}
{-                                                                            -}
{-  Author: Petr Tosovsky, Retry, www.edatools.cz                             -}
{-                                                                            -}
{-  Script was developed from Altium script PlaceSchObjects.PAS from          -}
{-  Scripting Gallery and with inspiration of FormatPaintBrush.pas from       -}
{-  Petar Perisin, Altium Designer addons page.                               -}
{-                                                                            -}
{==============================================================================}

{..............................................................................}
Var
    SchDoc           : ISch_Document;
    WorkSpace        : IWorkSpace;
    SchIterator      : ISch_Iterator;
    SchComponent     : ISch_Component;
    Pin              : ISch_Pin;
    PinIterator      : ISch_Iterator;
    Location         : TLocation;
    GridSize         : TCoord;
    LocX, LocY, Len  : Integer;
    LabelText        : String;

Const

    OnlySelectedComps = True; // Add stubs only to selected components or to all
    StubLength = 3;       // Length of wire stub attached to the pin defined by
                          // multiple of visible grid
    AddNetLabels = True;  // By this you can enable if Net Lables will be
                          // automatically added to the wire stubs
    DesigToLabels = False;// If this value is true Pin Designators are used as
                          // Netlabels, for false Pin Names are used

    // Offset should be smaller than StubLength
    LabelOffsetTop = 1;   // Top (pin orientation 90°) offset of Net label
                          // position defined by multiple of visible grid
    LabelOffsetBot = 3;   // Bottom (pin orientation 270°) offset of Net label
                          // position defined by multiple of visible grid
    LabelOffsetRight = 1; // Right (pin orientation 0°) offset of Net label
                          // position defined by multiple of visible grid
    LabelOffsetLeft = 3;  // Left (pin orientation 180°) offset of Net label
                          // position defined by multiple of visible grid
{..............................................................................}

{..............................................................................}
Function SortVertices(WireVertices : String) : Integer;
Var
   NewValue : String;
Begin
     //X1=4540|Y1=4540|X2=4540|Y2=3450|X2=3540|Y2=4560|....
     If Pos('|', WireVertices) > 0 Then
     Begin
          NewValue := Copy(WireVertices, Pos('=', WireVertices) + 1, pos('|',
                                   WireVertices) - pos('=', WireVertices) - 1);
          Result := NewValue;
     End;
End;
{..............................................................................}

{..............................................................................}
Function VerticesTrim(WireVertices : String) : String;
Var
   NewValue : String;
Begin
     If Pos('|', WireVertices) > 0 Then
     Begin
          Delete(WireVertices, 1, pos('|', WireVertices));
          Result := WireVertices;
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchNetLabel(X, Y: Integer, Text : String, Rotate : Integer);
Var
    SchNetlabel : ISch_Netlabel;
Begin
    SchNetlabel := SchServer.SchObjectFactory(eNetlabel,eCreate_GlobalCopy);
    If SchNetlabel = Nil Then Exit;

    SchDoc.AddSchObject(SchNetlabel);

    SchNetlabel.Text        := Text;

    SchServer.RobotManager.SendMessage(SchNetlabel.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);


    SchNetlabel.MoveToXY(MilsToCoord(X), MilsToCoord(Y));
    SchNetlabel.RotateBy90(Point(MilsToCoord(X), MilsToCoord(Y)), Rotate);

    SchNetlabel.SetState_xSizeySize;

    SchServer.RobotManager.SendMessage(SchNetlabel.I_ObjectAddress, c_BroadCast, SCHM_EndModify, c_NoEventData);

    SchServer.RobotManager.SendMessage(SchDoc.I_ObjectAddress,c_BroadCast,
           SCHM_PrimitiveRegistration,SchNetlabel.I_ObjectAddress);

    SchNetlabel.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchWire(NumberOfVertices : Integer, Vertices : String,
                                         LineWidth : Tsize);
Var
   ScriptParametres : String;
   SchWire          : ISch_Wire;
   I                : Integer;
   X                : Integer;
   Y                : Integer;
   WireVertices     : String;
Begin
     SchWire := SchServer.SchObjectFactory(eWire,eCreate_GlobalCopy);
     If SchWire = Nil Then Exit;

     // Number of vertices. Always 2 for a single wire
     WireVertices := Vertices;

     X := SortVertices(WireVertices);
     WireVertices := VerticesTrim(WireVertices);

     Y := SortVertices(WireVertices);
     WireVertices := VerticesTrim(WireVertices);

     // Set the line width based on TSize type
     SchWire.SetState_LineWidth := LineWidth;

     // Starting point for the vertex
     Schwire.Location := Point(MilsToCoord(X), MilsToCoord(Y));
     Schwire.InsertVertex := 1;
     SchWire.SetState_Vertex(1, Point(MilsToCOord(X), MilsToCoord(Y)));

     For I := 2 to NumberOfVertices Do
     Begin
          Schwire.InsertVertex := I;
          X                    := SortVertices(WireVertices);

          WireVertices         := VerticesTrim(WireVertices);

          Y                    := SortVertices(WireVertices);
          WireVertices         := VerticesTrim(WireVertices);

          SchWire.SetState_Vertex(I, Point(MilsToCoord(X), MilsToCoord(Y)));
     End;
     SchDoc.RegisterSchObjectInContainer(SchWire);
End;
{..............................................................................}

{..............................................................................}
Function CheckPinWire(X, Y, Len, Orientation : Integer): Boolean;
Var
   SpatialIterator : ISch_Iterator;
   Obj             : TObject;
Begin
   // Check pin if a wire is connected
   SpatialIterator := SchDoc.SchIterator_Create;
   If SpatialIterator = Nil Then
      Begin
           ShowMessage('Unable to create iterator for checking pins');
           Exit;
      End;
   Try
      //SpatialIterator add filtering arounf Pin connection point
        Case Orientation Of
          eRotate0   : Begin
              SpatialIterator.AddFilter_Area(X+Len-1, Y-1, X+Len+1, Y+1);
              End;
          eRotate90  : Begin
              SpatialIterator.AddFilter_Area(X-1, Y+Len-1, X+1, Y+Len+1);
              End;
          eRotate180 : Begin
              SpatialIterator.AddFilter_Area(X-Len-1, Y-1, X-Len+1, Y+1);
              End;
          eRotate270 : Begin
              SpatialIterator.AddFilter_Area(X-1, Y-Len-1, X+1, Y-Len+1);
              End;
        End;

      Result := False; // Wire doesn't exist yet

      Obj := SpatialIterator.FirstSchObject;
      While Obj <> Nil Do
      Begin
         if Obj.ObjectId = eWire then Result := True;// There is a wire existing
         Obj := SpatialIterator.NextSchObject;
      End;
   Finally
      SchDoc.SchIterator_Destroy(SpatialIterator);
   End;
End;
{..............................................................................}

{..............................................................................}
Procedure AddWireStubsSchRun;
Begin
    If SchServer = Nil Then Exit;
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    // Create an iterator to look for components only
    SchIterator := SchDoc.SchIterator_Create;
    SchIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    GridSize := SchDoc.VisibleGridSize / cInternalPrecision;

    Try
        SchComponent := SchIterator.FirstSchObject;
        While SchComponent <> Nil Do
        Begin

          if (OnlySelectedComps = False) OR (SchComponent.Selection = True) then
            Begin
            // Look for Pins associated with this component.
            PinIterator := SchComponent.SchIterator_Create;
            PinIterator.AddFilter_ObjectSet(MkSet(ePin));
            PinIterator.AddFilter_CurrentPartPrimitives;
            Try
                Pin := PinIterator.FirstSchObject;
                While Pin <> Nil Do
                Begin
                    // Check pin location
                    Location := Pin.GetState_Location;
                    LocX := Location.X / cInternalPrecision;
                    LocY := Location.Y / cInternalPrecision;
                    Len  := Pin.PinLength / cInternalPrecision;

                    if CheckPinWire(Location.X, Location.Y, Pin.PinLength,
                                                Pin.Orientation) = False then
                    Begin
                    // Create wire stub
                    Case Pin.Orientation Of
                      eRotate0   : Begin
                          PlaceASchWire(2, 'X1='+ IntToStr(LocX+Len) +
                                          '|Y1='+ IntToStr(LocY) +
                                          '|X2='+ IntToStr(LocX+Len+(StubLength*GridSize)) +
                                          '|Y2='+ IntToStr(LocY)+'|', eSmall);
                          End;
                      eRotate90  : Begin
                          PlaceASchWire(2, 'X1='+ IntToStr(LocX) +
                                          '|Y1='+ IntToStr(LocY+Len) +
                                          '|X2='+ IntToStr(LocX) +
                                          '|Y2='+ IntToStr(LocY+Len+(StubLength*GridSize)) +'|', eSmall);
                          End;
                      eRotate180 : Begin
                          PlaceASchWire(2, 'X1='+ IntToStr(LocX-Len) +
                                          '|Y1='+ IntToStr(LocY) +
                                          '|X2='+ IntToStr(LocX-Len-(StubLength*GridSize)) +
                                          '|Y2='+ IntToStr(LocY)+'|', eSmall);
                          End;
                      eRotate270 : Begin
                          PlaceASchWire(2, 'X1='+ IntToStr(LocX) +
                                          '|Y1='+ IntToStr(LocY-Len) +
                                          '|X2='+ IntToStr(LocX) +
                                          '|Y2='+ IntToStr(LocY-Len-(StubLength*GridSize))+'|', eSmall);
                          End;
                    End;

                    // Select what will be used as a Net Label
                    if  DesigToLabels = False then
                       LabelText := Pin.Name
                    else
                       LabelText := Pin.Designator;

                    //  Add Net Label
                    if AddNetLabels = True then
                    Case Pin.Orientation Of
                      eRotate0   : Begin
                          PlaceASchNetLabel(LocX+Len+(LabelOffsetRight*GridSize)
                                                     ,LocY,LabelText,eRotate0);
                          End;
                      eRotate90  : Begin
                          PlaceASchNetLabel(LocX,LocY+Len+(LabelOffsetTop*GridSize)
                                                     ,LabelText,eRotate90);
                          End;
                      eRotate180 : Begin
                          PlaceASchNetLabel(LocX-Len-(LabelOffsetLeft*GridSize)
                                                     ,LocY,LabelText,eRotate0);
                          End;
                      eRotate270 : Begin
                          PlaceASchNetLabel(LocX,LocY-Len-(LabelOffsetBot*GridSize)
                                                     ,LabelText,eRotate90);
                          End;
                    End;
                    End;
                    Pin := PinIterator.NextSchObject;
                    End;

            Finally
                SchComponent.SchIterator_Destroy(PinIterator);
            End;
            End;
            SchComponent := SchIterator.NextSchObject;
            End;
    Finally
        SchDoc.SchIterator_Destroy(SchIterator);
    End;

    SchDoc.GraphicallyInvalidate;

    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('Sch:Zoom');
End;
{..............................................................................}

{..............................................................................}
