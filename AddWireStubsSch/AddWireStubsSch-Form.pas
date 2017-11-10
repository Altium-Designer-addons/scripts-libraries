{==============================================================================}
{ ---- Add Wire Stubs to Schematic Pins ---------------------------------------}
{------------------------------------------------------------------------------}
{-                                                                            -}
{-  This scripts aims to add small segments of wires to unconnected pins in   -}
{-  current schematic sheet equiped by net labels related to each pin.        -}
{-  If there is a wire, a power port or a No ERC symbol existing on pin,      -}
{-  then no action is done for such pin.                                      -}
{-  The script was developed for easier drawing of schematics with FPGA       -}
{-  devices where small segments of wires are commonly used to wire down the  -}
{-  schematic symbol for easier pin swapping.                                 -}
{-  Net labels can be easily replaced by final names in SCH List panel (Smart -}
{-  Grid Paste) from pin file or by importing changes from FPGA project.      -}
{-                                                                            -}
{-  Startup procedure  ::: AddWireStubsSch-Form.pas :::                       -}
{-                                                                            -}
{-  CUSTOMIZATION can by done through a simple GUI                            -}
{-                                                                            -}
{-  OnlySelectedComps - script process only selected or all components        -}
{-  StubLength - length of wire segments in multiple of visible grid units    -}
{-  AddNetLabels - script creates Net labels for new wire segments            -}
{-  DesToLabels - pin designator will be copied to net label (otherwise pin   -}
{-                name is used ad net label)                                  -}
{-  LabelOffset - offset of new net labels from pin in up, down, left, right  -}
{-                directions separately                                       -}
{-                                                                            -}
{-                                                                            -}
{-     Please report script issues to:                                        -}
{-     https://github.com/altium-designer-addons/scripts-libraries/issues     -}
{-                                                                            -}
{-  Author: Petr Tosovsky, Retry, www.edatools.cz                             -}
{-          Cyril Andreatta, extension with GUI                               -}
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

// Add stubs only to selected components or to all
OnlySelectedComps : Boolean;
// By this you can enable if Net Labels will be
// automatically added to the wire stubs
AddNetLabels      : Boolean;
// If this value is true Pin Designators are used as
// Netlabels, for false Pin Names are used
DesigToLabels     : Boolean;

// Length of wire stub attached to the pin defined by
// multiple of visible grid
StubLength        : Integer;
// Top (pin orientation 90°) offset of Net label
// position defined by multiple of visible grid
LabelOffsetTop    : Integer;
// Bottom (pin orientation 270°) offset of Net label
// position defined by multiple of visible grid
LabelOffsetBot    : Integer;
// Right (pin orientation 0°) offset of Net label
// position defined by multiple of visible grid
LabelOffsetRight  : Integer;
// Left (pin orientation 180°) offset of Net label
// position defined by multiple of visible grid
LabelOffsetLeft   : integer;

Const
// Could not find the correct definition from Altium Script Reference
PowerPort = 39;
NoErc = 35;

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
			// SpatialIterator add filtering around Pin connection point
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
		// Do not add a wire if there is already a wire attached or there is
		// a PowerPort or a NoERC Symbol on the pin
		if ((Obj.ObjectId = eWire) or (Obj.ObjectId = PowerPort) or (Obj.ObjectId = NoErc)) then
			Result := True;

		Obj := SpatialIterator.NextSchObject;
	End;
	Finally
		SchDoc.SchIterator_Destroy(SpatialIterator);
	End;
End;
{..............................................................................}

{..............................................................................}
function StringInList(pattern : String, ignorelist : TStrings, caseSensitive : Boolean): Boolean;
var I: Integer;
begin
	Result := True;
	for i := 0 to ignorelist.Count - 1 do
	begin
		if caseSensitive then
		begin
			// case sensitive matches
			if ignorelist[i] = pattern then Exit
				end
			else
			begin
				// case insensitive matches
				if LowerCase(ignorelist[i]) = LowerCase(pattern) then Exit
		end
	end;
	Result := False;
end;
{..............................................................................}

{..............................................................................}
Procedure AddWireStubsSchRun;
var componentCount: Integer;
Begin
	If SchServer = Nil Then
	begin
		ShowMessage('Could not connect to SchServer!');
		Exit;
	end;
	SchDoc := SchServer.GetCurrentSchDocument;
	If SchDoc = Nil Then
	begin
		ShowMessage('No Schematic document found. This script has to be started from an open Schematic Document.');
		Exit;
	end;

	// Create an iterator to look for components only
	SchIterator := SchDoc.SchIterator_Create;
	SchIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

	GridSize := SchDoc.VisibleGridSize / cInternalPrecision;
	componentCount := 0;

	Try
		SchComponent := SchIterator.FirstSchObject;
		While SchComponent <> Nil Do
		Begin
			if (OnlySelectedComps = False) OR (SchComponent.Selection = True) then
			Begin
				inc(componentCount);
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

						// Select what will be used as a Net Label
						if  DesigToLabels = False then
							LabelText := Pin.Name
						else
							LabelText := Pin.Designator;

						// do not place stubs on pins matching any of the ignore patterns
						if StringInList(LabelText, lstIgnore.Items, chkCaseSensitive.Checked) = false then
						begin
							if CheckPinWire(Location.X, Location.Y, Pin.PinLength, Pin.Orientation) = False then
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
			end;

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
		if (componentCount = 0) then
		begin
			ShowMessage('No component was selected. Either select the components to add stubs to, or uncheck "Add stubs only on selected Components"');
		end;
	End;
	
	SchDoc.GraphicallyInvalidate;

	ResetParameters;
	AddStringParameter('Action', 'Redraw');
	RunProcess('Sch:Zoom');
End;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.btnAddStubsClick(Sender: TObject);
begin
	// Read settings from GUI
	OnlySelectedComps  := chkOnlySelectedComps.Checked;
	AddNetLabels       := chkAddNetlabels.Checked;
	DesigToLabels      := chkDesignatorToLabel.Checked;

	StubLength         := txtStub.Text;
	if (StubLength < 1) then
	begin
		StubLength := 1;
		txtStub.Text := 1;
	end;

	LabelOffsetTop     := txtOffsetTop.Text;
	LabelOffsetBot     := txtOffsetBot.Text;
	LabelOffsetRight   := txtOffsetRight.Text;
	LabelOffsetLeft    := txtOffsetLeft.Text;

	AddWireStubsSchRun;
end;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.CancelClick(Sender: TObject);
begin
	close;
end;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.btnRemoveClick(Sender: TObject);
begin
	lstIgnore.DeleteSelected();
end;
{..............................................................................}

{..............................................................................}
procedure AddListItem();
begin
	lstIgnore.AddItem(txtIgnore.Text, Nil);
	txtIgnore.Clear;
end;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.btnAddClick(Sender: TObject);
begin
	AddListItem;
end;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.txtIgnoreKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #13 then
	AddListItem;
end;
{..............................................................................}

{..............................................................................}
procedure TfrmAddWireStubs.lstIgnoreKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = 46 then
		lstIgnore.DeleteSelected();
	end;
{..............................................................................}
