{..............................................................................}
{ Summary   This scripts can be used to filter objects based on their type     }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

Uses
    IniFiles;
Var
    IniFileName : String;

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

    if (IniFile.ReadBool('Layers','Signals',True)) then
        CheckBoxSignal.Checked := 1
    else
        CheckBoxSignal.Checked := 0;

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

    if (IniFile.ReadBool('Parents','FreePrims',True)) then
        CheckBoxFreePrims.Checked := 1
    else
        CheckBoxFreePrims.Checked := 0;

    if (IniFile.ReadBool('Parents','ComponentPrims',True)) then
        CheckBoxComponentPrims.Checked := 1
    else
        CheckBoxComponentPrims.Checked := 0;

    IniFile.Free;
end;
{..............................................................................}

{..............................................................................}
Procedure WriteToIniFile(AFileName : String);
Var
    IniFile : TIniFile;
Begin
    IniFile := TIniFile.Create(AFileName);    

    if CheckBoxTracks.Checked         then IniFile.WriteBool('Objects','Tracks',True)
    else                                   IniFile.WriteBool('Objects','Tracks',False);

    if CheckBoxArcs.Checked           then IniFile.WriteBool('Objects','Arcs',True)
    else                                   IniFile.WriteBool('Objects','Arcs',False);

    if CheckBoxVias.Checked           then IniFile.WriteBool('Objects','Vias',True)
    else                                   IniFile.WriteBool('Objects','Vias',False);

    if CheckBoxPads.Checked           then IniFile.WriteBool('Objects','Pads',True)
    else                                   IniFile.WriteBool('Objects','Pads',False);

    if CheckBoxStrings.Checked        then IniFile.WriteBool('Objects','Strings',True)
    else                                   IniFile.WriteBool('Objects','Strings',False);

    if CheckBoxRegions.Checked        then IniFile.WriteBool('Objects','Regions',True)
    else                                   IniFile.WriteBool('Objects','Regions',False);

    if CheckBoxFills.Checked          then IniFile.WriteBool('Objects','Fills',True)
    else                                   IniFile.WriteBool('Objects','Fills',False);

    if CheckBoxComponents.Checked     then IniFile.WriteBool('Objects','Components',True)
    else                                   IniFile.WriteBool('Objects','Components',False);

    if CheckBoxPolygons.Checked       then IniFile.WriteBool('Objects','Polygons',True)
    else                                   IniFile.WriteBool('Objects','Polygons',False);

    if CheckBoxDimensions.Checked     then IniFile.WriteBool('Objects','Dimensions',True)
    else                                   IniFile.WriteBool('Objects','Dimensions',False);

    if CheckBoxSignal.Checked         then IniFile.WriteBool('Layers','Signals',True)
    else                                   IniFile.WriteBool('Layers','Signals',False);

    if CheckBoxPlane.Checked          then IniFile.WriteBool('Layers','Planes',True)
    else                                   IniFile.WriteBool('Layers','Planes',False);

    if CheckBoxMech.Checked           then IniFile.WriteBool('Layers','Mech',True)
    else                                   IniFile.WriteBool('Layers','Mech',False);

    if CheckBoxOverlay.Checked        then IniFile.WriteBool('Layers','Overlay',True)
    else                                   IniFile.WriteBool('Layers','Overlay',False);

    if CheckBoxSolder.Checked         then IniFile.WriteBool('Layers','Solder',True)
    else                                   IniFile.WriteBool('Layers','Solder',False);

    if CheckBoxPaste.Checked          then IniFile.WriteBool('Layers','Paste',True)
    else                                   IniFile.WriteBool('Layers','Paste',False);

    if CheckBoxDrill.Checked          then IniFile.WriteBool('Layers','Drill',True)
    else                                   IniFile.WriteBool('Layers','Drill',False);

    if CheckBoxFreePrims.Checked      then IniFile.WriteBool('Parents','FreePrims',True)
    else                                   IniFile.WriteBool('Parents','FreePrims',False);

    if CheckBoxComponentPrims.Checked then IniFile.WriteBool('Parents','ComponentPrims',True)
    else                                   IniFile.WriteBool('Parents','ComponentPrims',False);


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
    if CheckBoxTracks.Checked         then Objects := Objects + 'IsTrack or ';

    if CheckBoxArcs.Checked           then Objects := Objects + 'IsArc or ';

    if CheckBoxVias.Checked           then Objects := Objects + 'IsVia or ';

    if CheckBoxPads.Checked           then Objects := Objects + 'IsPad or ';

    if CheckBoxStrings.Checked        then Objects := Objects + 'IsText or ';

    if CheckBoxRegions.Checked        then Objects := Objects + 'IsRegion or ';

    if CheckBoxFills.Checked          then Objects := Objects + 'IsFill or ';

    if CheckBoxComponents.Checked     then Objects := Objects + 'IsComponent or ';

    if CheckBoxPolygons.Checked       then Objects := Objects + 'IsPoly or ';

    if CheckBoxDimensions.Checked     then Objects := Objects + 'IsDimension or IsCoordinate or ';

    if CheckBoxSignal.Checked         then Layers := Layers + 'OnSignal or OnMultiLayer or ';

    if CheckBoxPlane.Checked          then Layers := Layers + 'OnPlane or ';

    if CheckBoxMech.Checked           then Layers := Layers + 'OnMechanical or ';

    if CheckBoxOverlay.Checked        then Layers := Layers + 'OnSilkscreen or ';

    if CheckBoxSolder.Checked         then Layers := Layers + 'OnSolderMask or ';

    if CheckBoxPaste.Checked          then Layers := Layers + 'OnPaste or ';

    if CheckBoxDrill.Checked          then Layers := Layers + '(Layer = ''DrillGuide'') or (Layer = ''DrillDrawing'') or ';

    if CheckBoxFreePrims.Checked      then Parents := Parents + 'Not InAnyComponent or ';

    if CheckBoxComponentPrims.Checked then Parents := Parents + 'InAnyComponent or ';

    if Objects <> '' then
    begin
       SetLength(Objects, Length(Objects) - 4);
       Parameters := Parameters + '(' + Objects + ') and ';
    end;

    if Layers <> '' then
    begin
       SetLength(Layers, Length(Layers) - 4);
       Parameters := Parameters + '(' + Layers + ') and ';
    end;

    if Parents <> '' then
    begin
       SetLength(Parents, Length(Parents) - 4);
       Parameters := Parameters + '(' + Parents + ') and ';
    end;

    If Parameters <> 'Apply=True|Expr=' then SetLength(Parameters, Length(Parameters) - 5);  

    Parameters := Parameters + '|Index=1|Zoom=False|Select=False|Mask=True';

    ProcessLauncher := Client;
    ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
    ProcessLauncher.PostMessage('PCB:RunQuery', Parameters, Length(Parameters), Client.CurrentView);
    WriteToIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\FilterObjectsScriptData');     // Saving condition of checkboxes
    close;                       
end;



procedure TFormFilterObjects.CheckBoxAllClick(Sender: TObject);
begin
   if CheckBoxAll.Checked then
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
      CheckBoxSignal.Checked         := True;
      CheckBoxPlane.Checked          := True;
      CheckBoxMech.Checked           := True;
      CheckBoxOverlay.Checked        := True;
      CheckBoxSolder.Checked         := True;
      CheckBoxPaste.Checked          := True;
      CheckBoxDrill.Checked          := True;
      CheckBoxFreePrims.Checked      := True;
      CheckBoxComponentPrims.Checked := True;
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
      CheckBoxSignal.Checked         := False;
      CheckBoxPlane.Checked          := False;
      CheckBoxMech.Checked           := False;
      CheckBoxOverlay.Checked        := False;
      CheckBoxSolder.Checked         := False;
      CheckBoxPaste.Checked          := False;
      CheckBoxDrill.Checked          := False;
      CheckBoxFreePrims.Checked      := False;
      CheckBoxComponentPrims.Checked := False;
   end;
end;



Procedure Start;
begin
   If PCBServer.GetCurrentPCBBoard = nil then exit;

   ReadFromIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\FilterObjectsScriptData');

   FormFilterObjects.ShowModal;
end;


