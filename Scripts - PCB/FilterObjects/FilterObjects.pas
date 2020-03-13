{..............................................................................}
{ Summary   This scripts can be used to filter objects based on their type     }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

Uses
    IniFiles;
Var
    IniFileName    : String;
    Rectangle      : String;

{..............................................................................}

{..............................................................................}
procedure ReadFromIniFile(AFileName : String);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    if (IniFile.ReadBool('Objects','Tracks',True)) then
        CheckBoxTracks.Checked := 1
    else
        CheckBoxTracks.Checked := 0;

    if (IniFile.ReadBool('Objects','Arcs',True)) then
        CheckBoxArcs.Checked := 1
    else
        CheckBoxArcs.Checked := 0;

    if (IniFile.ReadBool('Objects','Vias',True)) then
        CheckBoxVias.Checked := 1
    else
        CheckBoxVias.Checked := 0;

    if (IniFile.ReadBool('Objects','Pads',True)) then
        CheckBoxPads.Checked := 1
    else
        CheckBoxPads.Checked := 0;

    if (IniFile.ReadBool('Objects','Strings',True)) then
        CheckBoxStrings.Checked := 1
    else
        CheckBoxStrings.Checked := 0;

    if (IniFile.ReadBool('Objects','Regions',True)) then
        CheckBoxRegions.Checked := 1
    else
        CheckBoxRegions.Checked := 0;

    if (IniFile.ReadBool('Objects','Fills',True)) then
        CheckBoxFills.Checked := 1
    else
        CheckBoxFills.Checked := 0;

    if (IniFile.ReadBool('Objects','Rooms',True)) then
        CheckBoxRooms.Checked := 1
    else
        CheckBoxRooms.Checked := 0;

    if (IniFile.ReadBool('Objects','Connections',True)) then
        CheckBoxConnections.Checked := 1
    else
        CheckBoxConnections.Checked := 0;

    if (IniFile.ReadBool('Objects','Components',True)) then
        CheckBoxComponents.Checked := 1
    else
        CheckBoxComponents.Checked := 0;

    if (IniFile.ReadBool('Objects','Polygons',True)) then
        CheckBoxPolygons.Checked := 1
    else
        CheckBoxPolygons.Checked := 0;

    if (IniFile.ReadBool('Objects','Dimensions',True)) then
        CheckBoxDimensions.Checked := 1
    else
        CheckBoxDimensions.Checked := 0;

    if (IniFile.ReadBool('Layers','Top',True)) then
        CheckBoxTop.Checked := 1
    else
        CheckBoxTop.Checked := 0;

    if (IniFile.ReadBool('Layers','Mid',True)) then
        CheckBoxMid.Checked := 1
    else
        CheckBoxMid.Checked := 0;

    if (IniFile.ReadBool('Layers','Bottom',True)) then
        CheckBoxBottom.Checked := 1
    else
        CheckBoxBottom.Checked := 0;

    if (IniFile.ReadBool('Layers','Planes',True)) then
        CheckBoxPlane.Checked := 1
    else
        CheckBoxPlane.Checked := 0;

    if (IniFile.ReadBool('Layers','Mech',True)) then
        CheckBoxMech.Checked := 1
    else
        CheckBoxMech.Checked := 0;

    if (IniFile.ReadBool('Layers','Overlay',True)) then
        CheckBoxOverlay.Checked := 1
    else
        CheckBoxOverlay.Checked := 0;

    if (IniFile.ReadBool('Layers','Solder',True)) then
        CheckBoxSolder.Checked := 1
    else
        CheckBoxSolder.Checked := 0;

    if (IniFile.ReadBool('Layers','Paste',True)) then
        CheckBoxPaste.Checked := 1
    else
        CheckBoxPaste.Checked := 0;

    if (IniFile.ReadBool('Layers','Drill',True)) then
        CheckBoxDrill.Checked := 1
    else
        CheckBoxDrill.Checked := 0;

    if (IniFile.ReadBool('Options','CurrentLayer',False)) then
    begin
        CheckBoxCurrentLayer.Checked := 1;

        CheckBoxTop.Enabled            := False;
        CheckBoxMid.Enabled            := False;
        CheckBoxBottom.Enabled         := False;
        CheckBoxPlane.Enabled          := False;
        CheckBoxMech.Enabled           := False;
        CheckBoxOverlay.Enabled        := False;
        CheckBoxSolder.Enabled         := False;
        CheckBoxPaste.Enabled          := False;
        CheckBoxDrill.Enabled          := False;
    end
    else
        CheckBoxCurrentLayer.Checked := 0;

    FormFilterObjects.Top    := IniFile.ReadInteger('Window', 'DialogueTop',    FormFilterObjects.Top);
    FormFilterObjects.Height := IniFile.ReadInteger('Window', 'DialogueHeight', FormFilterObjects.Height);
    FormFilterObjects.Left   := IniFile.ReadInteger('Window', 'DialogueLeft',   FormFilterObjects.Left);
    FormFilterObjects.Width  := IniFile.ReadInteger('Window', 'DialogueWidth',  FormFilterObjects.Width);

    IniFile.Free;
end;
{..............................................................................}

{..............................................................................}
Procedure WriteToIniFile(AFileName : String);
Var
    IniFile : TIniFile;
Begin
    IniFile := TIniFile.Create(AFileName);    

    if CheckBoxTracks.Checked          then IniFile.WriteBool('Objects','Tracks',True)
    else                                    IniFile.WriteBool('Objects','Tracks',False);

    if CheckBoxArcs.Checked            then IniFile.WriteBool('Objects','Arcs',True)
    else                                    IniFile.WriteBool('Objects','Arcs',False);

    if CheckBoxVias.Checked            then IniFile.WriteBool('Objects','Vias',True)
    else                                    IniFile.WriteBool('Objects','Vias',False);

    if CheckBoxPads.Checked            then IniFile.WriteBool('Objects','Pads',True)
    else                                    IniFile.WriteBool('Objects','Pads',False);

    if CheckBoxStrings.Checked         then IniFile.WriteBool('Objects','Strings',True)
    else                                    IniFile.WriteBool('Objects','Strings',False);

    if CheckBoxRegions.Checked         then IniFile.WriteBool('Objects','Regions',True)
    else                                    IniFile.WriteBool('Objects','Regions',False);

    if CheckBoxFills.Checked           then IniFile.WriteBool('Objects','Fills',True)
    else                                    IniFile.WriteBool('Objects','Fills',False);

    if CheckBoxComponents.Checked      then IniFile.WriteBool('Objects','Components',True)
    else                                    IniFile.WriteBool('Objects','Components',False);

    if CheckBoxPolygons.Checked        then IniFile.WriteBool('Objects','Polygons',True)
    else                                    IniFile.WriteBool('Objects','Polygons',False);

    if CheckBoxDimensions.Checked      then IniFile.WriteBool('Objects','Dimensions',True)
    else                                    IniFile.WriteBool('Objects','Dimensions',False);

    if CheckBoxRooms.Checked           then IniFile.WriteBool('Objects','Rooms',True)
    else                                    IniFile.WriteBool('Objects','Rooms',False);

    if CheckBoxConnections.Checked     then IniFile.WriteBool('Objects','Connections',True)
    else                                    IniFile.WriteBool('Objects','Connections',False);

    if CheckBoxTop.Checked             then IniFile.WriteBool('Layers','Top',True)
    else                                    IniFile.WriteBool('Layers','Top',False);

    if CheckBoxMid.Checked             then IniFile.WriteBool('Layers','Mid',True)
    else                                    IniFile.WriteBool('Layers','Mid',False);

    if CheckBoxBottom.Checked          then IniFile.WriteBool('Layers','Bottom',True)
    else                                    IniFile.WriteBool('Layers','Bottom',False);

    if CheckBoxPlane.Checked           then IniFile.WriteBool('Layers','Planes',True)
    else                                    IniFile.WriteBool('Layers','Planes',False);

    if CheckBoxMech.Checked            then IniFile.WriteBool('Layers','Mech',True)
    else                                    IniFile.WriteBool('Layers','Mech',False);

    if CheckBoxOverlay.Checked         then IniFile.WriteBool('Layers','Overlay',True)
    else                                    IniFile.WriteBool('Layers','Overlay',False);

    if CheckBoxSolder.Checked          then IniFile.WriteBool('Layers','Solder',True)
    else                                    IniFile.WriteBool('Layers','Solder',False);

    if CheckBoxPaste.Checked           then IniFile.WriteBool('Layers','Paste',True)
    else                                    IniFile.WriteBool('Layers','Paste',False);

    if CheckBoxDrill.Checked           then IniFile.WriteBool('Layers','Drill',True)
    else                                    IniFile.WriteBool('Layers','Drill',False);

    if CheckBoxCurrentLayer.Checked    then IniFile.WriteBool('Options','CurrentLayer',True)
    else                                    IniFile.WriteBool('Options','CurrentLayer',False);

    IniFile.WriteInteger('Window', 'DialogueTop',    FormFilterObjects.Top);
    IniFile.WriteInteger('Window', 'DialogueHeight', FormFilterObjects.Height);
    IniFile.WriteInteger('Window', 'DialogueLeft',   FormFilterObjects.Left);
    IniFile.WriteInteger('Window', 'DialogueWidth',  FormFilterObjects.Width);

    IniFile.Free;
End;
{..............................................................................}

{..............................................................................}
procedure TFormFilterObjects.ButtonCloseClick(Sender: TObject);
begin
   close;
end;


procedure TFormFilterObjects.ButtonOKClick(Sender: TObject);
Var
    Parameters      : String;
    Objects         : String;
    Layers          : String;
    Parents         : String;
    ProcessLauncher : IProcessLauncher;
begin
    Parameters := 'Apply=True|Expr=';
    Objects := '';
    Layers  := '';
    Parents := '';
    if CheckBoxTracks.Checked          then Objects := Objects + 'IsTrack or ';

    if CheckBoxArcs.Checked            then Objects := Objects + 'IsArc or ';

    if CheckBoxVias.Checked            then Objects := Objects + 'IsVia or ';

    if CheckBoxPads.Checked            then Objects := Objects + 'IsPad or ';

    if CheckBoxStrings.Checked         then Objects := Objects + 'IsText or ';

    if CheckBoxRegions.Checked         then Objects := Objects + 'IsRegion or ';

    if CheckBoxFills.Checked           then Objects := Objects + 'IsFill or ';

    if CheckBoxRooms.Checked           then Objects := Objects + 'IsRoom or ';

    if CheckBoxComponents.Checked      then Objects := Objects + 'IsComponent or ';

    if CheckBoxPolygons.Checked        then Objects := Objects + 'IsPoly or ';

    if CheckBoxDimensions.Checked      then Objects := Objects + 'IsDimension or IsCoordinate or ';

    if CheckBoxTop.Checked             then Layers := Layers + 'OnTopLayer or OnMultiLayer or ';

    if CheckBoxMid.Checked             then Layers := Layers + 'OnMid or OnMultiLayer or ';

    if CheckBoxBottom.Checked          then Layers := Layers + 'OnBottomLayer or OnMultiLayer or ';

    if CheckBoxPlane.Checked           then Layers := Layers + 'OnPlane or ';

    if CheckBoxMech.Checked            then Layers := Layers + 'OnMechanical or ';

    if CheckBoxOverlay.Checked         then Layers := Layers + 'OnSilkscreen or ';

    if CheckBoxSolder.Checked          then Layers := Layers + 'OnSolderMask or ';

    if CheckBoxPaste.Checked           then Layers := Layers + 'OnPaste or ';

    if CheckBoxDrill.Checked           then Layers := Layers + '(Layer = ''DrillGuide'') or (Layer = ''DrillDrawing'') or ';
    

    if Objects <> '' then
    begin
       SetLength(Objects, Length(Objects) - 4);
       Parameters := Parameters + '(' + Objects + ') and ';
    end;

    if (not CheckBoxCurrentLayer.Checked) then
    begin
       if Layers <> '' then
       begin
          SetLength(Layers, Length(Layers) - 4);
          Parameters := Parameters + '(' + Layers + ') and ';
       end;
    end
    else
       Parameters := Parameters + '(Layer = ''' + PCBServer.GetCurrentPCBBoard.LayerName(PcbServer.GetCurrentPCBBoard.CurrentLayer) + ''') and ';

    if Parents <> '' then
    begin
       SetLength(Parents, Length(Parents) - 4);
       Parameters := Parameters + '(' + Parents + ') and ';
    end;

    If Parameters <> 'Apply=True|Expr=' then SetLength(Parameters, Length(Parameters) - 5);

    If CheckBoxWithinArea.Checked then Parameters := Parameters + Rectangle;

    if CheckBoxConnections.Checked then Parameters := Parameters + ' or IsConnection';

    Parameters := Parameters + '|Index=1|Zoom=False|Select=False|Mask=True';

    WriteToIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\FilterObjectsScriptData');     // Saving condition of checkboxes

    ProcessLauncher := Client;
    ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
    ProcessLauncher.PostMessage('PCB:RunQuery', Parameters, Length(Parameters), Client.CurrentView);
    close;
end;



procedure TFormFilterObjects.CheckBoxAllObjectsClick(Sender: TObject);
begin
   if CheckBoxAllObjects.Checked then
   begin
      CheckBoxTracks.Checked         := True;
      CheckBoxArcs.Checked           := True;
      CheckBoxVias.Checked           := True;
      CheckBoxPads.Checked           := True;
      CheckBoxStrings.Checked        := True;
      CheckBoxRegions.Checked        := True;
      CheckBoxFills.Checked          := True;
      CheckBoxComponents.Checked     := True;
      CheckBoxPolygons.Checked       := True;
      CheckBoxDimensions.Checked     := True;
      CheckBoxRooms.Checked          := True;
      CheckBoxConnections.Checked    := True;
   end
   else
   begin
      CheckBoxTracks.Checked         := False;
      CheckBoxArcs.Checked           := False;
      CheckBoxVias.Checked           := False;
      CheckBoxPads.Checked           := False;
      CheckBoxStrings.Checked        := False;
      CheckBoxRegions.Checked        := False;
      CheckBoxFills.Checked          := False;
      CheckBoxComponents.Checked     := False;
      CheckBoxPolygons.Checked       := False;
      CheckBoxDimensions.Checked     := False;
      CheckBoxRooms.Checked          := False;
      CheckBoxConnections.Checked    := False;
   end;
end;



procedure TFormFilterObjects.CheckBoxAllLayersClick(Sender: TObject);
begin
   if CheckBoxAllLayers.Checked then
   begin
      CheckBoxTop.Enabled            := True;
      CheckBoxMid.Enabled            := True;
      CheckBoxBottom.Enabled         := True;
      CheckBoxPlane.Enabled          := True;
      CheckBoxMech.Enabled           := True;
      CheckBoxOverlay.Enabled        := True;
      CheckBoxSolder.Enabled         := True;
      CheckBoxPaste.Enabled          := True;
      CheckBoxDrill.Enabled          := True;

      CheckBoxTop.Checked            := True;
      CheckBoxMid.Checked            := True;
      CheckBoxBottom.Checked         := True;
      CheckBoxPlane.Checked          := True;
      CheckBoxMech.Checked           := True;
      CheckBoxOverlay.Checked        := True;
      CheckBoxSolder.Checked         := True;
      CheckBoxPaste.Checked          := True;
      CheckBoxDrill.Checked          := True;

      CheckBoxCurrentLayer.Checked   := False;
   end
   else
   begin
      CheckBoxTop.Enabled            := True;
      CheckBoxMid.Enabled            := True;
      CheckBoxBottom.Enabled         := True;
      CheckBoxPlane.Enabled          := True;
      CheckBoxMech.Enabled           := True;
      CheckBoxOverlay.Enabled        := True;
      CheckBoxSolder.Enabled         := True;
      CheckBoxPaste.Enabled          := True;
      CheckBoxDrill.Enabled          := True;

      CheckBoxTop.Checked            := False;
      CheckBoxMid.Checked            := False;
      CheckBoxBottom.Checked         := False;
      CheckBoxPlane.Checked          := False;
      CheckBoxMech.Checked           := False;
      CheckBoxOverlay.Checked        := False;
      CheckBoxSolder.Checked         := False;
      CheckBoxPaste.Checked          := False;
      CheckBoxDrill.Checked          := False;

      CheckBoxCurrentLayer.Checked   := False;
   end;
end;



procedure TFormFilterObjects.CheckBoxCurrentLayerClick(Sender: TObject);
begin
   if CheckBoxCurrentLayer.Checked then
   begin
      CheckBoxTop.Enabled            := False;
      CheckBoxMid.Enabled            := False;
      CheckBoxBottom.Enabled         := False;
      CheckBoxPlane.Enabled          := False;
      CheckBoxMech.Enabled           := False;
      CheckBoxOverlay.Enabled        := False;
      CheckBoxSolder.Enabled         := False;
      CheckBoxPaste.Enabled          := False;
      CheckBoxDrill.Enabled          := False;
   end
   else
   begin
      CheckBoxTop.Enabled            := True;
      CheckBoxMid.Enabled            := True;
      CheckBoxBottom.Enabled         := True;
      CheckBoxPlane.Enabled          := True;
      CheckBoxMech.Enabled           := True;
      CheckBoxOverlay.Enabled        := True;
      CheckBoxSolder.Enabled         := True;
      CheckBoxPaste.Enabled          := True;
      CheckBoxDrill.Enabled          := True;
   end;
end;



procedure TFormFilterObjects.CheckBoxWithinAreaClick(Sender: TObject);
var
   X1, Y1, X2, Y2, tmp: Integer;
begin
   if CheckBoxWithinArea.Checked then
   begin
      Rectangle := '';
      if PCBServer.GetCurrentPCBBoard.ChooseRectangleByCorners('Select first corner','Select second corner', X1, Y1, X2, Y2) then
      begin

         if X1 > X2 then
         begin
            tmp := X1;
            X1 := X2;
            X2 := tmp;
         end;

         if Y1 > Y2 then
         begin
            tmp := Y1;
            Y1 := Y2;
            Y2 := tmp;
         end;

         Rectangle := 'and InRegion(' + FloatToStr(CoordToMils(X1)) + ',' + FloatToStr(CoordToMils(Y1)) + ',' + FloatToStr(CoordToMils(X2)) + ',' + FloatToStr(CoordToMils(Y2)) + ')';
         CheckBoxComponents.Checked := False;
         CheckBoxPolygons.Checked   := False;
         CheckBoxDimensions.Checked := False;

         CheckBoxWithinArea.Checked := True;
      end
      else
         CheckBoxWithinArea.Checked := False;
   end;
end;



procedure TFormFilterObjects.CheckBoxComponentsClick(Sender: TObject);
begin
   if CheckBoxComponents.Checked then CheckBoxWithinArea.Checked := False;
end;



procedure TFormFilterObjects.CheckBoxPolygonsClick(Sender: TObject);
begin
   if CheckBoxPolygons.Checked then CheckBoxWithinArea.Checked := False;
end;



procedure TFormFilterObjects.CheckBoxDimensionsClick(Sender: TObject);
begin
   if CheckBoxDimensions.Checked then CheckBoxWithinArea.Checked := False;
end;



Procedure Start;
begin
   If PCBServer.GetCurrentPCBBoard = nil then exit;

   ReadFromIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\FilterObjectsScriptData');
   Rectangle := '';

   FormFilterObjects.Show;
end;

