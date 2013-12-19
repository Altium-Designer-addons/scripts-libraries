{..............................................................................}
{ Resize board shape based on Embedded Board Arrays on a current PCB document. }
{ Created by: Miklos Zsikla                                                    }
{..............................................................................}

{..............................................................................}

Var
    Board     : IPCB_Board;
    TheLayerStack : IPCB_LayerStack;
    Emb       : IPCB_EmbeddedBoard;
    Iterator  : IPCB_BoardIterator;
    delta     : Double;
    EmbNumb   : Integer;
    R         : TCoordRect;
    tempR     : TCoordRect;
    Track     : IPCB_Track;
    FoReteg   : Integer;
    TrackWidth : Double;

Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then
   begin
        ShowMessage('Active Window is Not a .PcbDoc File');
        exit;
   end;

   // Only show window if .PcbDoc is opened
   Form1.Show;

end;

Function PlaceLine(X1: TCoord, Y1: TCoord, X2: TCoord, Y2: TCoord);
Begin
        Track := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
        Track.X1 := X1;
        Track.Y1 := Y1;
        Track.X2 := X2;
        Track.Y2 := Y2;
        Track.Layer := FoReteg;
        Track.Width := TrackWidth;

        // Add the new Track
        Board.AddPCBObject(Track);
        Track.Selected := True;
End;

Function ChangeUnits(Igaz : Boolean);
Begin
if MetricBox.Checked = False & EmbNumb <> 0 then
begin
     Edit1.Text := FloatToStr(CoordToMils(MMsToCoord(StrToFloat(Edit1.Text))));
     Edit2.Text := FloatToStr(CoordToMils(MMsToCoord(StrToFloat(Edit2.Text))));
     Label_width.Caption    := 'mil';
     Label_pullback.Caption := 'mil';
end;
if MetricBox.Checked = True then
begin                      
     Edit1.Text := FloatToStr(CoordToMms(MilsToCoord(StrToFloat(Edit1.Text))));
     Edit2.Text := FloatToStr(CoordToMms(MilsToCoord(StrToFloat(Edit2.Text))));
     Label_width.Caption    := 'mm';
     Label_pullback.Caption := 'mm';
end;
EmbNumb := 1;
End;

Procedure ReSizeIt(Igaz : Boolean);
Begin

    // Deselect All
    ResetParameters;
    AddStringParameter('Scope', 'All');
    RunProcess('PCB:DeSelect');

    // Init
    EmbNumb := 0;
    FoReteg := StrToInt(ComboBox2.Items[ComboBox1.ItemIndex]) ;
    RunButton.Enabled := False;

    if MetricBox.Checked then
    begin
       TrackWidth := MMsToCoord(StrToFloat(Edit1.Text));
       delta      := MMsToCoord(StrToFloat(Edit2.Text));
    end
    else
    begin
       TrackWidth := MilsToCoord(StrToFloat(Edit1.Text) );
       delta      := MilsToCoord(StrToFloat(Edit2.Text));
    end;

    // Create the iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eEmbeddedBoardObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for Embedded Board Arrays
    Emb := Iterator.FirstPCBObject;
    While (Emb <> Nil) Do
    Begin

    if EmbNumb = 0 then R := Emb.BoundingRectangle
    else
    begin
         tempR := Emb.BoundingRectangle;
         if tempR.left < R.left     then R.left := tempR.left;
         if tempR.bottom < R.bottom then R.bottom := tempR.bottom;
         if tempR.right > R.right   then R.right := tempR.right;
         if tempR.top > R.top       then R.top := tempR.top;
    end;
       Inc(EmbNumb);
       Emb := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    If EmbNumb = 0 then // if PCB does not contain Embedded Board arrays
    begin
         ShowError('No Embedded Board Array was found. Please place first an Embedded Board Array!');
    end
    Else
    begin

    // Place lines
        R.left   := R.left - delta;
        R.bottom := R.bottom - delta;
        R.right  := R.right + delta;
        R.top    := R.top + delta;

        PlaceLine(R.left, R.bottom, R.right, R.bottom);
        PlaceLine(R.left, R.bottom, R.left, R.top);
        PlaceLine(R.left, R.top, R.right, R.top);
        PlaceLine(R.right, R.bottom, R.right, R.top);

    // Rebuild board shape
    ResetParameters;
    AddStringParameter('MODE', 'BOARDOUTLINE_FROM_SEL_PRIMS');
    RunProcess('PCB:PlaceBoardOutline');

    // Redraw PCB Worksheet
    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');

    // Clear Mask
    ResetParameters;
    AddStringParameter('Clear', 'True');
    RunProcess('PCB:RunQuery');

    end;

    // Remove these 2 lines to keep window shown
    Form1.Hide;
    Exit;
End;

procedure Betolt(Igaz : Boolean);
var
   k            : Integer;
   db           : Integer;
   MechLayerObj : IPCB_MechanicalLayer;
begin

     TheLayerStack  :=  Board.Layerstack ;
     If  TheLayerStack  =  Nil  Then  Close;

     EmbNumb    := 0;
     db         := 0;
    For  k  :=  eMechanical1 to  eMechanical16  Do
    Begin
         MechLayerObj  :=  TheLayerStack.LayerObject[k];
         if MechLayerObj.MechanicalLayerEnabled = True then
            begin
                 ComboBox1.Items.Add(Layer2String(TheLayerStack.LayerObject[k].LayerID));
                 ComboBox2.Items.Add(k);
                 Inc(db);
            end;
    End;

    If db = 0 then
    begin
         MechLayerObj := TheLayerStack.LayerObject[eMechanical1];
         MechLayerObj.MechanicalLayerEnabled := True;
         Board.LayerIsDisplayed[eMechanical1] := True;
         Board.ViewManager_UpdateLayerTabs;

         ComboBox1.Items.Add(Layer2String(TheLayerStack.LayerObject[eMechanical1].LayerID));
         ComboBox2.Items.Add(eMechanical1);
    end;

    ComboBox1.ItemIndex := 0;

    if UnitToString(Board.DisplayUnit) = 'Imperial (mil)' then
       begin
            MetricBox.Checked := 0;
            Label_width.Caption    := 'mil';
            Label_pullback.Caption := 'mil';
            Edit1.Text := FloatToStr( 10 ); // default values for Imperial units
            Edit2.Text := FloatToStr( 300 );
       end;
    if UnitToString(Board.DisplayUnit) = 'Metric (mm)' then
       begin
            MetricBox.Checked := 1;
            Label_width.Caption    := 'mm';
            Label_pullback.Caption := 'mm';
            Edit1.Text := FloatToStr( 0.1 ); // default values for Metric units
            Edit2.Text := FloatToStr( 7 );
       end;

end;



{..............................................................................}

{..............................................................................}

procedure TForm1.Form1Activate(Sender: TObject);
begin
Start();
Betolt(True);
end;

procedure TForm1.RunButtonClick(Sender: TObject);
begin
ReSizeIt(True);
end;

procedure TForm1.MetricBoxClick(Sender: TObject);
begin
ChangeUnits(True);
end;

procedure TForm1.PlaceEmbButtonClick(Sender: TObject);
begin
    ResetParameters;
    RunProcess('PCB:PlaceEmbeddedBoard');
end;

procedure TForm1.CancelButtonClick(Sender: TObject);
begin
Close;
end;

{..............................................................................}

