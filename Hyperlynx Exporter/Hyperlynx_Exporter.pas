Procedure Hyperlynx;
var
    FileName      : TPCBString;
    SaveAs        : String;
    Flag          : Bool;
    Board         : IPCB_Board;
    Doc           : IServerDocument;
    SaveDialog    : TSaveDialog;
    PCBObject     : IPCB_Primitive;
    NewObject     : IPCB_Primitive;

    hypFile       : TStringList;
    TheLayerStack : IPCB_LayerStack;
    LayerObj      : IPCB_LayerObject;
    LayerStackup  : TStringList;
    NamePos       : Integer;
    SpacePos      : Integer;
    pcbLayerName  : String;
    hypLayerName  : String;
    LayerName     : String;

    BoardIterator : IPCB_BoardIterator;
    ID            : Integer;
    Region        : IPCB_Region;
    NetName       : String;
    i             : Integer;
    Line          : String;
    Contour       : IPCB_Contour;
    j             : Integer;
    k             : Integer;
    dimX          : String;
    dimY          : String;
    Dim1X         : String;
    Dim1Y         : String;


    Poligon       : IPCB_Polygon;
    PolyIterator  : IPCB_GroupIterator;
    Track         : IPCB_Track;
    X1            : String;
    Y1            : String;
    X2            : String;
    Y2            : String;
    W             : String;

    Arc           : IPCB_Arc;
    XC            : String;
    YC            : String;
    R             : String;


    Fill          : IPCB_Fill;
    phiin         : Double;
    rotation      : Double;
    angle         : Double;
    Distance      : Integer;

    XLength       : Integer;
    YLength       : Integer;

    X1Point       : Integer;
    Y1Point       : Integer;
    X2point       : Integer;
    Y2point       : Integer;
    X3Point       : Integer;
    Y3Point       : Integer;
    X4point       : Integer;
    Y4point       : Integer;
    X5Point       : Integer;
    Y5Point       : Integer;
    X6point       : Integer;
    Y6point       : Integer;
    X7Point       : Integer;
    Y7Point       : Integer;
    X8point       : Integer;
    Y8point       : Integer;

    X3            : String;
    Y3            : String;
    X4            : String;
    Y4            : String;

    Split         : IPCB_SplitPlane;
    Iterator      : IPCB_BoardIterator;
    Pad           : IPCB_Pad2;
    X             : Integer;
    Y             : Integer;
    SizeX         : Integer;
    SizeY         : Integer;
    Line1         : Integer;
    Line2         : Integer;
    Line3         : Integer;
    Line4         : Integer;
    Line5         : Integer;
    Line6         : Integer;
    Line7         : Integer;
    radLength     : Integer;
    angle1        : Double;
    angle2        : Double;
    angle3        : Double;
    angle4        : Double;
    angle5        : Double;
    angle6        : Double;
    angle7        : Double;

    X5            : String;
    Y5            : String;
    X6            : String;
    Y6            : String;
    X7            : String;
    Y7            : String;
    X8            : String;
    Y8            : String;
    XC1           : String;
    YC1           : String;
    XC2           : String;
    YC2           : String;
    XC3           : String;
    YC3           : String;
    XC4           : String;
    YC4           : String;


    CornerRadius  : Integer;
    Via           : IPCB_Via;

Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Doc := Client.GetDocumentByPath(GetWorkspace.DM_FocusedDocument.DM_FullPath);

    if Doc = nil then exit;

    If (Doc <> Nil) And Doc.Modified Then
    begin
       ShowMessage('Please Save PCB Document First');
       Exit;
    end;

    // First part - open and set Save dialog
    SaveDialog        := TSaveDialog.Create(Application);
    SaveDialog.Title  := 'Export HyperLynx';
    SaveDialog.Filter := 'Hyperlynx file (*.hyp)|*.hyp';

    Flag := SaveDialog.Execute;
    if (not Flag) then exit;

    SaveAs := SaveDialog.FileName;

    if (SaveAs[Length(SaveAs)-3] <> '.') then
       SaveAs := SaveAs + '.hyp';

    // Second part - export regular altium's *.hyp file
    FileName := Board.FileName;
    Doc := Client.GetDocumentByPath(FileName);

    ShowMessage('Wait for export to finish. (it might take a while)'#13#10 +
                'Export is done when document temporarily closes and re-opens.'#13#10 +
                'Until then your PCB file will have *.hyp extension in Altium'#13#10 +
                'Click OK to continue.');

    if Doc <> nil then
    begin
      Doc.SetFileName(SaveAs);
      Doc.DoFileSave('HyperLynx');
    end;

    {
    // other way of exporting as hyperlynx, not used
    ResetParameters;
    AddStringParameter('SaveMode', 'SaveAs');
    AddStringParameter('ObjectKind', 'Document');
    AddStringParameter('FileFormat', 'Export HyperLynx');
    RunProcess('WorkspaceManager:SaveObject');
    }

    // Third part - Load exported *.hyp file to a string list
    hypFile := TStringList.Create;
    hypFile.LoadFromFile(SaveAs);

    // we need to connect layer stackup info from PCB to hyp file.
    // In order to do this we will use TStringlist that will look like this:
    // .........................................................................
    // PCB_Layer_Name;HYP_Layer_Name
    // PCB_Layer_Name;HYP_Layer_Name
    // .............................

    // We will get layer stackup from hyp file
    i := 0;
    Line := hypFile[i];

    while (Pos('{STACKUP', Line) <> 1) do
    begin
       Inc(i);
       Line := hypFile[i];
    end;
    Inc(i);   // now "i" contains line with first signal layer

    // We need to get layer stackup from PCB, and imidiatley implement
    TheLayerStack := Board.LayerStack;
    If TheLayerStack = Nil Then Exit;

    LayerStackup := TstringList.Create;
    LayerObj := TheLayerStack.FirstLayer;
    Repeat
       Line := hypFile[i]; // This line contains layer name. We need to extract it
       NamePos := Pos('L=',Line) + 2;
       SpacePos := Pos(' ',Line);
       while NamePos > SpacePos do
       begin
           Delete(Line,SpacePos,1);
           NamePos := Pos('L=',Line) + 2;
           SpacePos := Pos(' ',Line);
       end;
       hypLayerName := Copy(Line, NamePos, SpacePos - NamePos);
       pcbLayerName := Layer2String(LayerObj.LayerID);
       LayerStackup.Add(pcbLayerName + ';' + hypLayerName);
       LayerObj := TheLayerStack.NextLayer(LayerObj);
       i := i + 2;
    Until LayerObj = Nil;

    // Update - we need to put regions, fills, tracks and arcs that are on multi
    // layer to appropriate layers in layer stack.
    (*
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eRegionObject, eFillObject, eArcObject, eTrackObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    PCBObject := BoardIterator.FirstPCBObject;
    While (PCBObject <> Nil) do
    begin
       if Layer2String(PCBObject.Layer) = 'Multi Layer' then
       begin
          LayerObj := TheLayerStack.FirstLayer;
          repeat
             if ((LayerObj.LayerID = eTopLayer) or (LayerObj.LayerID = eMidLayer1) or (LayerObj.LayerID = eMidLayer2) or (LayerObj.LayerID = eMidLayer3) or (LayerObj.LayerID = eMidLayer4) or
                (LayerObj.LayerID = eMidLayer5) or (LayerObj.LayerID = eMidLayer6) or (LayerObj.LayerID = eMidLayer7) or (LayerObj.LayerID = eMidLayer8) or (LayerObj.LayerID = eMidLayer9) or
                 (LayerObj.LayerID = eMidLayer10) or (LayerObj.LayerID = eMidLayer11) or (LayerObj.LayerID = eMidLayer12) or (LayerObj.LayerID = eMidLayer13) or (LayerObj.LayerID = eMidLayer14) or
                 (LayerObj.LayerID = eMidLayer15) or (LayerObj.LayerID = eMidLayer16) or (LayerObj.LayerID = eMidLayer17) or (LayerObj.LayerID = eMidLayer18) or (LayerObj.LayerID = eMidLayer19) or
                 (LayerObj.LayerID = eMidLayer20) or (LayerObj.LayerID = eMidLayer21) or (LayerObj.LayerID = eMidLayer22) or (LayerObj.LayerID = eMidLayer23) or (LayerObj.LayerID = eMidLayer24) or
                 (LayerObj.LayerID = eMidLayer25) or (LayerObj.LayerID = eMidLayer26) or (LayerObj.LayerID = eMidLayer27) or (LayerObj.LayerID = eMidLayer28) or (LayerObj.LayerID = eMidLayer29) or
                 (LayerObj.LayerID = eMidLayer30) or (LayerObj.LayerID = eBottomLayer)) then
                  begin
                     NewObject := PCBObject.Replicate;
                     NewObject.Layer := String2Layer(LayerObj.Name);
                     Board.AddPCBObject(NewObject);
                  end;
             LayerObj  := TheLayerStack.NextLayer(LayerObj);
          until LayerObj = Nil;
       end;
       PCBObject := BoardIterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(BoardIterator);
    *)

    // Fourth part - the fun begins :) - read all regions on top-mid-bot layers:
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eRegionObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    // Start iterating through regions
    ID := 0;
    Region := BoardIterator.FirstPCBObject;
    While (Region <> Nil) Do
    begin
       // Ignore regions that are polygon cutout, keepout or board cutout.
       if ((Region.Kind <> eRegionkind_Cutout) and (not(Region.IsKeepout)) and (Region.Kind <> eRegionkind_BoardCutout)) and (
       (Region.Layer = eTopLayer) or (Region.Layer = eMidLayer1) or (Region.Layer = eMidLayer2) or (Region.Layer = eMidLayer3) or (Region.Layer = eMidLayer4) or
       (Region.Layer = eMidLayer5) or (Region.Layer = eMidLayer6) or (Region.Layer = eMidLayer7) or (Region.Layer = eMidLayer8) or (Region.Layer = eMidLayer9) or
       (Region.Layer = eMidLayer10) or (Region.Layer = eMidLayer11) or (Region.Layer = eMidLayer12) or (Region.Layer = eMidLayer13) or (Region.Layer = eMidLayer14) or
       (Region.Layer = eMidLayer15) or (Region.Layer = eMidLayer16) or (Region.Layer = eMidLayer17) or (Region.Layer = eMidLayer18) or (Region.Layer = eMidLayer19) or
       (Region.Layer = eMidLayer20) or (Region.Layer = eMidLayer21) or (Region.Layer = eMidLayer22) or (Region.Layer = eMidLayer23) or (Region.Layer = eMidLayer24) or
       (Region.Layer = eMidLayer25) or (Region.Layer = eMidLayer26) or (Region.Layer = eMidLayer27) or (Region.Layer = eMidLayer28) or (Region.Layer = eMidLayer29) or
       (Region.Layer = eMidLayer30) or (Region.Layer = eBottomLayer)) then
       Begin
          // This is where we got regions on copper layers. For each region we need to
          // fiogure out it's net to put it in appropriate position in *.hyp file

          // we need to figure out in which net region is. it is different
          // for free regions and for regions inside polygon
          NetName := 'UNUSED_PINS_PADS00';

          // Net name of free regions
          if Region.InNet then
             NetName := Region.Net.Name;

          // net name of poly regions
          if Region.InPolygon then
          begin
             if Region.Polygon.Net <> Nil then
             begin
                NetName :=  Region.Polygon.Net.Name;
             end;
          end;

          // We will increase ID of this region
          Inc(ID);

          // We need to figure out what layer it is on, and what is layer name in hyp file
          i := 0;
          pcbLayerName := Layer2String(Region.Layer);
          while i < LayerStackup.count do
          begin
             hypLayerName := LayerStackup[i];
             NamePos := Pos(';',hypLayerName);
             if (pcbLayerName = Copy(hypLayerName,1,NamePos - 1))then
             begin
                LayerName := Copy(hyplayerName, NamePos + 1, Length(hypLayerName) - NamePos);
                Break;
             end;
             Inc(i);
          end;
          // Now LayerName contains name of the layer this region is on

          // We need to find position of this net in a hyp file
          i := 0;
          while i < hypFile.Count do
          begin
             Line := hypFile[i];
             if (Pos('{NET=' + AnsiUpperCase(NetName),AnsiUpperCase(Line)) = 1) then Break;
             Inc(i);
          end;

          // Now i has line number where net name is. we need co cycle to
          // the end of net definition, because here we will place region
          if i <> hypFile.Count then
          begin
             while Line[1] <> '}' do
             begin
                Inc(i);
                Line := hypFile[i];
             end;
          end
          else
          begin
             Dec(i);
             Line := hypFile[i];
             while Line[1] <> '}'do
             begin
                Dec(i);
                Line := hypFile[i];
             end;
             Inc(i);
             hypFile.Insert(i,'');
             Inc(i);
             hypFile.Insert(i,'{NET=UNUSED_PINS_PADS00');
             Inc(i);
             hypFile.Insert(i,'}');
          end;

          // Now "i" contains a place in a hyp file where we will put region.


          // We will start preparing region for hyp file.
          // to do this we will first add main contour to report,
          // and after that all holes

          Contour := Region.MainContour;

          for j := 1 to Contour.Count do
          begin
             // Setting up dimensions
             dimX := FormatFloat('0.00000',Contour.X[j]/10000000);
             dimY := FormatFloat('0.00000',Contour.Y[j]/10000000);

             // if decimal symbol is ',' - change it to '.'
             if  (LastDelimiter(',',dimX) <> 0) then dimX[LastDelimiter(',',dimX)] := '.';
             if  (LastDelimiter(',',dimY) <> 0) then dimY[LastDelimiter(',',dimY)] := '.';

             // modify line for hypFile
             if j = 1 then
             begin
                // saving first points for closing loop
                dim1X := dimX;
                dim1Y := dimY;
                Line := '    {POLYGON T=POUR L=' + LayerName + ' W=0.00000 ID=' + IntToStr(ID) + ' X=' + dimx + ' Y=' + dimY;
             end
             else
             begin
                Line := '    (LINE X=' + dimX + ' Y=' + dimY + ')';
             end;

             // add line to hyp file and increment
             hypFile.Insert(i,Line);
             Inc(i);
          end;

          // close main contour
          hypFile.Insert(i,'    (LINE X=' + dim1X + ' Y=' + dim1Y + ')');
          Inc(i);
          hypFile.Insert(i,'    }');
          Inc(i);

          // now we need to put all holes inside
          for k := 0 To Region.HoleCount - 1 Do
          begin
             Contour := Region.Holes[k];
             for j := 1 to Contour.Count do
             begin
                // Setting up dimensions
                dimX := FormatFloat('0.00000',Contour.X[j]/10000000);
                dimY := FormatFloat('0.00000',Contour.Y[j]/10000000);

                // if decimal symbol is ',' - change it to '.'
                if  (LastDelimiter(',',dimX) <> 0) then dimX[LastDelimiter(',',dimX)] := '.';
                if  (LastDelimiter(',',dimY) <> 0) then dimY[LastDelimiter(',',dimY)] := '.';

                // modify line for hypFile
                if j = 1 then
                begin
                   // saving first points for closing loop
                   dim1X := dimX;
                   dim1Y := dimY;
                   Line := '    {POLYVOID ID=' + IntToStr(ID) + ' X=' + dimx + ' Y=' + dimY;
                end
                else
                begin
                   Line := '    (LINE X=' + dimX + ' Y=' + dimY + ')';
                end;

                // add line to hyp file and increment
                hypFile.Insert(i,Line);
                Inc(i);
             end;
             // close contour
             hypFile.Insert(i,'    (LINE X=' + dim1X + ' Y=' + dim1Y + ')');
             Inc(i);
             hypFile.Insert(i,'    }');
             Inc(i);
          end;
       end;
       Region := BoardIterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(BoardIterator);

    // Right now we will transfer all hatched and outlines only polygons
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Poligon := BoardIterator.FirstPCBObject;
    While (Poligon <> Nil) Do
    begin
       // Ignore solid poligons
       if (Poligon.PolyHatchStyle <> ePolySolid) and(
       (Poligon.Layer = eTopLayer) or (Poligon.Layer = eMidLayer1) or (Poligon.Layer = eMidLayer2) or (Poligon.Layer = eMidLayer3) or (Poligon.Layer = eMidLayer4) or
       (Poligon.Layer = eMidLayer5) or (Poligon.Layer = eMidLayer6) or (Poligon.Layer = eMidLayer7) or (Poligon.Layer = eMidLayer8) or (Poligon.Layer = eMidLayer9) or
       (Poligon.Layer = eMidLayer10) or (Poligon.Layer = eMidLayer11) or (Poligon.Layer = eMidLayer12) or (Poligon.Layer = eMidLayer13) or (Poligon.Layer = eMidLayer14) or
       (Poligon.Layer = eMidLayer15) or (Poligon.Layer = eMidLayer16) or (Poligon.Layer = eMidLayer17) or (Poligon.Layer = eMidLayer18) or (Poligon.Layer = eMidLayer19) or
       (Poligon.Layer = eMidLayer20) or (Poligon.Layer = eMidLayer21) or (Poligon.Layer = eMidLayer22) or (Poligon.Layer = eMidLayer23) or (Poligon.Layer = eMidLayer24) or
       (Poligon.Layer = eMidLayer25) or (Poligon.Layer = eMidLayer26) or (Poligon.Layer = eMidLayer27) or (Poligon.Layer = eMidLayer28) or (Poligon.Layer = eMidLayer29) or
       (Poligon.Layer = eMidLayer30) or (Poligon.Layer = eBottomLayer)) then

       Begin
          // We need to set net name here, and find the place where net is
          NetName := 'UNUSED_PINS_PADS00';

          if Poligon.Net <> Nil then
             NetName :=  Poligon.Net.Name;

          // We need to figure out what layer it is on, and what is layer name in hyp file
          i := 0;
          pcbLayerName := Layer2String(Poligon.Layer);
          while i < LayerStackup.count do
          begin
             hypLayerName := LayerStackup[i];
             NamePos := Pos(';',hypLayerName);
             if (pcbLayerName = Copy(hypLayerName,1,NamePos - 1))then
             begin
                LayerName := Copy(hyplayerName, NamePos + 1, Length(hypLayerName) - NamePos);
                Break;
             end;
             Inc(i);
          end;
          // Now LayerName contains name of the layer this polygon is on

          // We need to find position of this net in a hyp file
          i := 0;
          while i < hypFile.Count do
          begin
             Line := hypFile[i];
             if (Pos('{NET=' + AnsiUpperCase(NetName),AnsiUpperCase(Line)) = 1) then Break;
             Inc(i);
          end;

          // We need to do is make sure i is not at the end of file
          if i = hypFile.Count then
          begin
             Dec(i);
             Line := hypFile[i];
             while Line[1] <> '}'do
             begin
                Dec(i);
                Line := hypFile[i];
             end;
             Inc(i);
             hypFile.Insert(i,'');
             Inc(i);
             hypFile.Insert(i,'{NET=UNUSED_PINS_PADS00');
             Inc(i);
             hypFile.Insert(i,'}');
          end
          else
          begin
             Inc(i);
          end;

          // Now "i+1" is the place we will be adding segments in a hyp file
          // Here i will not get incremented. all segments will be placed here.


          // Iterate through tracks
          PolyIterator := Poligon.GroupIterator_Create;
          PolyIterator.AddFilter_ObjectSet(MkSet(eTrackObject));

          Track := PolyIterator.FirstPCBObject;
          while (Track <> nil) do
          begin
             // Adding track to hyp file
             X1 := FormatFloat('0.00000',Track.x1/10000000);
             X2 := FormatFloat('0.00000',Track.x2/10000000);
             Y1 := FormatFloat('0.00000',Track.y1/10000000);
             Y2 := FormatFloat('0.00000',Track.y2/10000000);
             W  := FormatFloat('0.00000',Track.Width/10000000);

             if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
             if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
             if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
             if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
             if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

             hypFile.Insert(i,'  (SEG X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X2 + ' Y2=' + Y2 + ' W=' + W + ' L=' + LayerName + ')');

             Track := PolyIterator.NextPCBObject;
          end;
          Poligon.GroupIterator_Destroy(PolyIterator);

          // Iterate through arcs
          PolyIterator := Poligon.GroupIterator_Create;
          PolyIterator.AddFilter_ObjectSet(MkSet(eArcObject));

          Arc := PolyIterator.FirstPCBObject;
          while (Arc <> nil) do
          begin
             // adding Arc to hyp file

             // If it is not full circle
             if ((Arc.StartX <> Arc.EndX) or (Arc.StartY <> Arc.EndY)) then
             begin
                X2 := FormatFloat('0.00000',Arc.StartX/10000000);
                X1 := FormatFloat('0.00000',Arc.EndX/10000000);
                Y2 := FormatFloat('0.00000',Arc.StartY/10000000);
                Y1 := FormatFloat('0.00000',Arc.EndY/10000000);
                XC := FormatFloat('0.00000',Arc.XCenter/10000000);
                YC := FormatFloat('0.00000',Arc.YCenter/10000000);
                R  := FormatFloat('0.00000',Arc.Radius/10000000);
                W  := FormatFloat('0.00000',Arc.LineWidth/10000000);

                if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                hypFile.Insert(i,'  (ARC X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ' W=' + W + ' L=' + LayerName + ')');
             end
             else
             begin
                // I was having problems with importing full circle, so I will split it in two
                X2 := FormatFloat('0.00000',Arc.StartX/10000000);
                X1 := FormatFloat('0.00000',Arc.XCenter/10000000);
                Y2 := FormatFloat('0.00000',Arc.StartY/10000000);
                Y1 := FormatFloat('0.00000',(Arc.YCenter + Arc.Radius)/10000000);
                XC := FormatFloat('0.00000',Arc.XCenter/10000000);
                YC := FormatFloat('0.00000',Arc.YCenter/10000000);
                R  := FormatFloat('0.00000',Arc.Radius/10000000);
                W  := FormatFloat('0.00000',Arc.LineWidth/10000000);

                if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                hypFile.Insert(i,'  (ARC X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ' W=' + W + ' L=' + LayerName + ')');

                X2 := FormatFloat('0.00000',Arc.XCenter/10000000);
                X1 := FormatFloat('0.00000',Arc.EndX/10000000);
                Y2 := FormatFloat('0.00000',(Arc.YCenter + Arc.Radius)/10000000);
                Y1 := FormatFloat('0.00000',Arc.EndY/10000000);
                XC := FormatFloat('0.00000',Arc.XCenter/10000000);
                YC := FormatFloat('0.00000',Arc.YCenter/10000000);
                R  := FormatFloat('0.00000',Arc.Radius/10000000);
                W  := FormatFloat('0.00000',Arc.LineWidth/10000000);

                if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                hypFile.Insert(i,'  (ARC X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ' W=' + W + ' L=' + LayerName + ')');
             end;


             Arc := PolyIterator.NextPCBObject;
          end;
          Poligon.GroupIterator_Destroy(PolyIterator);
       end;
       Poligon := BoardIterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(BoardIterator);


    // Transfering fills to hyp file
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eFillObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Fill := BoardIterator.FirstPCBObject;

    While (Fill <> nil) do
    begin
       if (not (Fill.IsKeepout)) and (
       (Fill.Layer = eTopLayer) or (Fill.Layer = eMidLayer1) or (Fill.Layer = eMidLayer2) or (Fill.Layer = eMidLayer3) or (Fill.Layer = eMidLayer4) or
       (Fill.Layer = eMidLayer5) or (Fill.Layer = eMidLayer6) or (Fill.Layer = eMidLayer7) or (Fill.Layer = eMidLayer8) or (Fill.Layer = eMidLayer9) or
       (Fill.Layer = eMidLayer10) or (Fill.Layer = eMidLayer11) or (Fill.Layer = eMidLayer12) or (Fill.Layer = eMidLayer13) or (Fill.Layer = eMidLayer14) or
       (Fill.Layer = eMidLayer15) or (Fill.Layer = eMidLayer16) or (Fill.Layer = eMidLayer17) or (Fill.Layer = eMidLayer18) or (Fill.Layer = eMidLayer19) or
       (Fill.Layer = eMidLayer20) or (Fill.Layer = eMidLayer21) or (Fill.Layer = eMidLayer22) or (Fill.Layer = eMidLayer23) or (Fill.Layer = eMidLayer24) or
       (Fill.Layer = eMidLayer25) or (Fill.Layer = eMidLayer26) or (Fill.Layer = eMidLayer27) or (Fill.Layer = eMidLayer28) or (Fill.Layer = eMidLayer29) or
       (Fill.Layer = eMidLayer30) or (Fill.Layer = eBottomLayer)) then
       begin

          // We will figure out net of this region
          NetName := 'UNUSED_PINS_PADS00';
          if Fill.Net <> Nil then NetName := Fill.Net.Name;

          // We will increase ID of this region
          Inc(ID);

          // We need to figure out what layer it is on, and what is layer name in hyp file
          i := 0;
          pcbLayerName := Layer2String(Fill.Layer);
          while i < LayerStackup.count do
          begin
             hypLayerName := LayerStackup[i];
             NamePos := Pos(';',hypLayerName);
             if (pcbLayerName = Copy(hypLayerName,1,NamePos - 1))then
             begin
                LayerName := Copy(hyplayerName, NamePos + 1, Length(hypLayerName) - NamePos);
                Break;
             end;
             Inc(i);
          end;
          // Now LayerName contains name of the layer this region is on

          // We need to find position of this net in a hyp file
          i := 0;
          while i < hypFile.Count do
          begin
             Line := hypFile[i];
             if (Pos('{NET=' + AnsiUpperCase(NetName),AnsiUpperCase(Line)) = 1) then Break;
             Inc(i);
          end;

          // Now i has line number where net name is. we need co cycle to
          // the end of net definition, because here we will place region
          if i <> hypFile.Count then
          begin
             while Line[1] <> '}' do
             begin
                Inc(i);
                Line := hypFile[i];
             end;
          end
          else
          begin
             Dec(i);
             Line := hypFile[i];
             while Line[1] <> '}'do
             begin
                Dec(i);
                Line := hypFile[i];
             end;
             Inc(i);
             hypFile.Insert(i,'');
             Inc(i);
             hypFile.Insert(i,'{NET=UNUSED_PINS_PADS00');
             Inc(i);
             hypFile.Insert(i,'}');
          end;

          // The hard part is getting other two coordinates. Rotation Complicates things
          phiin := ArcTan((Fill.Y2Location - Fill.Y1Location)/(Fill.X2Location - Fill.X1Location));
          rotation := Fill.Rotation * PI / 180;
          angle := rotation + phiin;

          XLength := abs(Fill.X1Location - Fill.X2Location);
          YLength := abs(Fill.Y1Location - Fill.Y2Location);

          Distance := sqrt(sqr(XLength) + sqr(YLength));

          X2point := Fill.XLocation + XLength * cos(rotation);
          Y2point := Fill.YLocation + XLength * sin(rotation);
          X4point := Fill.XLocation + YLength * cos(rotation + PI / 2);
          Y4point := Fill.YLocation + YLength * sin(rotation + PI / 2);


          // Modifying points for hyp file
          X1 := FormatFloat('0.00000',Fill.XLocation/10000000);
          Y1 := FormatFloat('0.00000',Fill.YLocation/10000000);
          X2 := FormatFloat('0.00000',X2point/10000000);
          Y2 := FormatFloat('0.00000',Y2point/10000000);
          X3 := FormatFloat('0.00000',(Fill.XLocation + (Distance * cos(angle)))/10000000);
          Y3 := FormatFloat('0.00000',(Fill.YLocation + (Distance * sin(angle)))/10000000);
          X4 := FormatFloat('0.00000',X4point/10000000);
          Y4 := FormatFloat('0.00000',Y4point/10000000);


          if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
          if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
          if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
          if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
          if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
          if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
          if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
          if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

          // Now we need to write this inside
          hypFile.Insert(i  ,'    {POLYGON T=POUR L=' + LayerName + ' W=0.00000 ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
          hypFile.Insert(i+1,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
          hypFile.Insert(i+2,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
          hypFile.Insert(i+3,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
          hypFile.Insert(i+4,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
          hypFile.Insert(i+5,'    }');
       end;
       Fill := BoardIterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(BoardIterator);


    // We need to transfer split planes to hyp file. This will be little harder to do,
    // and since Steve helped, We can go

    // Iterator for split planes
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eSplitPlaneObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    // Start iterating through regions
    Split := BoardIterator.FirstPCBObject;
    
    while (Split <> Nil) do
    begin

       // We will get net name of a split
       NetName := 'UNUSED_PINS_PADS00';
       if Split.Net <> Nil then NetName := Split.Net.Name;

       // We will increase ID of this region
       Inc(ID);

       // We need to figure out what layer it is on, and what is layer name in hyp file
       i := 0;
       pcbLayerName := Layer2String(Split.Layer);
       while i < LayerStackup.count do
       begin
          hypLayerName := LayerStackup[i];
          NamePos := Pos(';',hypLayerName);
          if (pcbLayerName = Copy(hypLayerName,1,NamePos - 1))then
          begin
             LayerName := Copy(hyplayerName, NamePos + 1, Length(hypLayerName) - NamePos);
             Break;
          end;
          Inc(i);
       end;
       // Now LayerName contains name of the layer this region is on

       // We need to find position of this net in a hyp file
       i := 0;
       while i < hypFile.Count do
       begin
          Line := hypFile[i];
          if (Pos('{NET=' + AnsiUpperCase(NetName),AnsiUpperCase(Line)) = 1) then Break;
          Inc(i);
       end;

       // Now i has line number where net name is. we need co cycle to
       // the end of net definition, because here we will place region
       if i <> hypFile.Count then
       begin
          while Line[1] <> '}' do
          begin
             Inc(i);
             Line := hypFile[i];
          end;
       end
       else
       begin
          Dec(i);
          Line := hypFile[i];
          while Line[1] <> '}'do
          begin
             Dec(i);
             Line := hypFile[i];
          end;
          Inc(i);
          hypFile.Insert(i,'');
          Inc(i);
          hypFile.Insert(i,'{NET=UNUSED_PINS_PADS00');
          Inc(i);
          hypFile.Insert(i,'}');
       end;




       // Iterate through regions in a split (there is only one anyway)
       PolyIterator := Split.GroupIterator_Create;
       PolyIterator.AddFilter_ObjectSet(MkSet(eRegionObject));

       Region := PolyIterator.FirstPCBObject;
       while (Region <> nil) do
       begin

          Contour := Region.MainContour;

          for j := 1 to Contour.Count do
          begin
             // Setting up dimensions
             dimX := FormatFloat('0.00000',Contour.X[j]/10000000);
             dimY := FormatFloat('0.00000',Contour.Y[j]/10000000);

             // if decimal symbol is ',' - change it to '.'
             if  (LastDelimiter(',',dimX) <> 0) then dimX[LastDelimiter(',',dimX)] := '.';
             if  (LastDelimiter(',',dimY) <> 0) then dimY[LastDelimiter(',',dimY)] := '.';

             // modify line for hypFile
             if j = 1 then
             begin
                // saving first points for closing loop
                dim1X := dimX;
                dim1Y := dimY;
                Line := '    {POLYGON T=POUR L=' + LayerName + ' W=0.00000 ID=' + IntToStr(ID) + ' X=' + dimx + ' Y=' + dimY;
             end
             else
             begin
                Line := '    (LINE X=' + dimX + ' Y=' + dimY + ')';
             end;

             // add line to hyp file and increment
             hypFile.Insert(i,Line);
             Inc(i);
          end;

          // close main contour
          hypFile.Insert(i,'    (LINE X=' + dim1X + ' Y=' + dim1Y + ')');
          Inc(i);
          hypFile.Insert(i,'    }');
          Inc(i);

          // now we need to put all holes inside
          for k := 0 To Region.HoleCount - 1 Do
          begin
             Contour := Region.Holes[k];
             for j := 1 to Contour.Count do
             begin
                // Setting up dimensions
                dimX := FormatFloat('0.00000',Contour.X[j]/10000000);
                dimY := FormatFloat('0.00000',Contour.Y[j]/10000000);

                // if decimal symbol is ',' - change it to '.'
                if  (LastDelimiter(',',dimX) <> 0) then dimX[LastDelimiter(',',dimX)] := '.';
                if  (LastDelimiter(',',dimY) <> 0) then dimY[LastDelimiter(',',dimY)] := '.';

                // modify line for hypFile
                if j = 1 then
                begin
                   // saving first points for closing loop
                   dim1X := dimX;
                   dim1Y := dimY;
                   Line := '    {POLYVOID ID=' + IntToStr(ID) + ' X=' + dimx + ' Y=' + dimY;
                end
                else
                begin
                   Line := '    (LINE X=' + dimX + ' Y=' + dimY + ')';
                end;

                // add line to hyp file and increment
                hypFile.Insert(i,Line);
                Inc(i);
             end;
             // close contour
             hypFile.Insert(i,'    (LINE X=' + dim1X + ' Y=' + dim1Y + ')');
             Inc(i);
             hypFile.Insert(i,'    }');
             Inc(i);
          end;

          // Up to here, with splits was practically the same like with regions
          // but here is where it gets interesting because now we have to make
          // holes for every thermal / unconnected via or pad.


          // Pads

          Iterator := Board.BoardIterator_Create;
          Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
          Iterator.AddFilter_LayerSet(AllLayers);
          Iterator.AddFilter_Method(eProcessAll);

          Pad := Iterator.FirstPCBObject;

          while (Pad <> nil) do
          begin
             If Layer2String(Pad.Layer) = 'Multi Layer' then
             begin
                // This is through-hole pad. This will be very hard to do, because
                // holes can be slotted and rectangular, and can have rotation

                if Pad.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneReliefConnect then
                begin
                   if Pad.HoleType = eRoundHole then
                   begin
                      // We need to make 4 (or 2) holes here because these are thermals
                      phiin := arccos(Pad.ReliefConductorWidth / (2 * Pad.PowerPlaneReliefExpansion + Pad.HoleSize));
                      phiin := phiin - (PI / 4);

                      // We will be using variables X2pount, and Y2point for X,Y for relative points of nearer arc
                      // and X4Point and Y4point for relative points of further arc. This is bit hard to explain,
                      // so you are gonna have to trust me.

                      X2Point := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * cos(phiin);
                      Y2Point := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sin(phiin);

                      phiin := arccos(Pad.ReliefConductorWidth / (2 * (Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) + Pad.HoleSize));
                      phiin := phiin - (PI / 4);

                      X4Point := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * cos(phiin);
                      Y4Point := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sin(phiin);


                      if Pad.ReliefEntries = 4 then
                      begin

                         // Modifying points for hyp file - first hole
                         X1 := FormatFloat('0.00000',(Pad.x + X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + X2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - Y2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - Second hole
                         X1 := FormatFloat('0.00000',(Pad.x - Y2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + X2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - Y4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + X4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - third hole
                         X1 := FormatFloat('0.00000',(Pad.x - X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - X2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Y2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - fourth hole
                         X1 := FormatFloat('0.00000',(Pad.x + Y2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - X2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Y4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - X4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                      end
                      else if Pad.ReliefEntries = 2 then
                      begin

                         // Modifying points for hyp file - first hole
                         X1 := FormatFloat('0.00000',(Pad.x + X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // Modifying points for hyp file - second hole
                         X1 := FormatFloat('0.00000',(Pad.x - X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                      end;
                   end
                   else if Pad.HoleType = eSquareHole then
                   begin
                      // We need to make 4 holes here because these are thermals

                      // first angle
                      phiin := arctan((((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) / (Pad.ReliefConductorWidth / 2));
                      phiin := phiin + ((Pad.HoleRotation + Pad.Rotation) * PI / 180) - (PI / 4);

                      // first point
                      X1Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * cos(phiin);
                      Y1Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * sin(phiin);

                      // second angle
                      phiin := arctan((((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) / (Pad.ReliefConductorWidth / 2));
                      phiin := phiin + ((Pad.HoleRotation + Pad.Rotation) * PI / 180) - (PI / 4);

                      // second point
                      X2Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * cos(phiin);
                      Y2Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * sin(phiin);

                      // third angle
                      phiin := arctan((((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) / (Pad.ReliefConductorWidth / 2));
                      phiin := PI - phiin + ((Pad.HoleRotation + Pad.Rotation) * PI / 180) - (PI / 4);

                      // third point
                      X3Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * cos(phiin);
                      Y3Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * sin(phiin);

                      // fourth angle
                      phiin := arctan((((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) / (Pad.ReliefConductorWidth / 2));
                      phiin := PI - phiin + ((Pad.HoleRotation + Pad.Rotation) * PI / 180) - (PI / 4);

                      // fourth point
                      X4Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * cos(phiin);
                      Y4Point := (sqrt(sqr(((Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2)) - (Pad.ReliefConductorWidth / 2)) + sqr(Pad.ReliefConductorWidth / 2))) * sin(phiin);

                      if Pad.ReliefEntries = 4 then
                      begin

                         // Modifying points for hyp file - first hole
                         X1 := FormatFloat('0.00000',(Pad.x + X1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Y1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + X2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Y2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Y3Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - X3Point)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - second hole
                         X1 := FormatFloat('0.00000',(Pad.x - Y1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + X1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - Y2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + X2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + X3Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Y3Point)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - third hole
                         X1 := FormatFloat('0.00000',(Pad.x - X1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - Y1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - X2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - Y2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - Y3Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + X3Point)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - fourth hole
                         X1 := FormatFloat('0.00000',(Pad.x + Y1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - X1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Y2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - X2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - X3Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - Y3Point)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if  Pad.ReliefEntries = 2 then
                      begin
                         // we also need two more coordinates
                         Line1 := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion) * sqrt(2);
                         Line2 := (Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap) * sqrt(2);

                         X := Line1 * cos((Pad.Rotation + Pad.HoleRotation) * PI / 180 + (PI / 4));   // smaller point
                         Y := Line1 * sin((Pad.Rotation + Pad.HoleRotation) * PI / 180 + (PI / 4));

                         SizeX := Line2 * cos((Pad.Rotation + Pad.HoleRotation) * PI / 180 + (PI / 4)); // bigger point
                         SizeY := Line2 * sin((Pad.Rotation + Pad.HoleRotation) * PI / 180 + (PI / 4));

                         // Modifying points for hyp file - First hole
                         X1 := FormatFloat('0.00000',(Pad.x + X1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Y1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + X2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Y2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + SizeY)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y - SizeX)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x - X4Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y - Y4Point)/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x - X3Point)/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y - Y3Point)/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x + Y)/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y - X)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',X5) <> 0) then X5[LastDelimiter(',',X5)] := '.';
                         if  (LastDelimiter(',',Y5) <> 0) then Y5[LastDelimiter(',',Y5)] := '.';
                         if  (LastDelimiter(',',X6) <> 0) then X6[LastDelimiter(',',X6)] := '.';
                         if  (LastDelimiter(',',Y6) <> 0) then Y6[LastDelimiter(',',Y6)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X5 + ' Y=' + Y5 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - second hole
                         X1 := FormatFloat('0.00000',(Pad.x - X1Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - Y1Point)/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x - X2Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y - Y2Point)/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x - SizeY)/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + SizeX)/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + X4Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Y4Point)/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x + X3Point)/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y + Y3Point)/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x - Y)/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y + X)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',X5) <> 0) then X5[LastDelimiter(',',X5)] := '.';
                         if  (LastDelimiter(',',Y5) <> 0) then Y5[LastDelimiter(',',Y5)] := '.';
                         if  (LastDelimiter(',',X6) <> 0) then X6[LastDelimiter(',',X6)] := '.';
                         if  (LastDelimiter(',',Y6) <> 0) then Y6[LastDelimiter(',',Y6)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X5 + ' Y=' + Y5 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end;
                   end
                   else if Pad.HoleType = eSlotHole then
                   begin
                      // Slot hole will be defined by 7 distances, and 7 angles
                      // Like on the paper. I will do it on wednesday

                      X := Pad.x;
                      Y := Pad.y;
                      rotation := (Pad.Rotation + Pad.HoleRotation) * PI / 180;

                      angle1 := arccos((Pad.ReliefConductorWidth / 2) / (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2))) - (PI / 4);
                      angle2 := arccos((Pad.ReliefConductorWidth / 2) / (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2) + Pad.ReliefAirGap)) - (PI / 4);

                      angle3 := (PI / 2) - angle1;
                      angle4 := (PI / 2) - angle2;

                      X1Point := (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) * cos(angle1) + ((Pad.HoleWidth - Pad.HoleSize) / 2);
                      Y1Point := (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) * sin(angle1);
                      angle1  := arctan(Y1Point / X1Point);
                      Line1   := sqrt(sqr(X1Point) + sqr(Y1Point));

                      X2Point := ((Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) + Pad.ReliefAirGap) * cos(angle2) + ((Pad.HoleWidth - Pad.HoleSize) / 2);
                      Y2Point := ((Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) + Pad.ReliefAirGap) * sin(angle2);
                      angle2  := arctan(Y2Point / X2Point);
                      Line2   := sqrt(sqr(X2Point) + sqr(Y2Point));

                      X3Point := (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) * cos(angle3) + ((Pad.HoleWidth - Pad.HoleSize) / 2);
                      Y3Point := (Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) * sin(angle3);
                      angle3  := arctan(Y3Point / X3Point);
                      Line3   := sqrt(sqr(X3Point) + sqr(Y3Point));

                      X4Point := ((Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) + Pad.ReliefAirGap) * cos(angle4) + ((Pad.HoleWidth - Pad.HoleSize) / 2);
                      Y4Point := ((Pad.PowerPlaneReliefExpansion + (Pad.HoleSize / 2)) + Pad.ReliefAirGap) * sin(angle4);
                      angle4  := arctan(Y4Point / X4Point);
                      Line4   := sqrt(sqr(X4Point) + sqr(Y4Point));

                      angle5  := arctan((Pad.PowerPlaneReliefExpansion  + (Pad.HoleSize / 2)) / ((Pad.HoleWidth - Pad.HoleSize) / 2));
                      Line5   := sqrt(sqr(Pad.PowerPlaneReliefExpansion  + (Pad.HoleSize / 2)) + sqr((Pad.HoleWidth - Pad.HoleSize) / 2));

                      angle6  := arctan((Pad.PowerPlaneReliefExpansion  + (Pad.HoleSize / 2) + Pad.ReliefAirGap) / ((Pad.HoleWidth - Pad.HoleSize) / 2));
                      Line6   := sqrt(sqr(Pad.PowerPlaneReliefExpansion  + (Pad.HoleSize / 2) + Pad.ReliefAirGap) + sqr((Pad.HoleWidth - Pad.HoleSize) / 2));

                      angle7  := 0;
                      Line7   := (Pad.HoleWidth - Pad.HoleSize) / 2;

                      // Now comes the fun part - modifying points and setting up hole info
                      if Pad.ReliefEntries = 4 then
                      begin
                         // First hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation + angle1))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation + angle1))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation + angle2))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation + angle2))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation - angle2))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation - angle2))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation - angle1))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation - angle1))/10000000);
                         XC := FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation))/10000000);
                         YC := FormatFloat('0.00000',(Pad.Y + Line7 * sin(rotation))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',', R) <> 0) then  R[LastDelimiter(',', R)] := '.';
                         if  (LastDelimiter(',', W) <> 0) then  W[LastDelimiter(',', W)] := '.';


                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // second hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation + angle1 + PI))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation + angle1 + PI))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation + angle2 + PI))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation + angle2 + PI))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation - angle2 + PI))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation - angle2 + PI))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation - angle1 + PI))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation - angle1 + PI))/10000000);
                         XC := FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation + PI))/10000000);
                         YC := FormatFloat('0.00000',(Pad.Y + Line7 * sin(rotation + PI))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',', R) <> 0) then  R[LastDelimiter(',', R)] := '.';
                         if  (LastDelimiter(',', W) <> 0) then  W[LastDelimiter(',', W)] := '.';


                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // Second two holes are bit harder because they are made out of 8 points

                         // third hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation + PI - angle3))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation + PI - angle3))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation + PI - angle4))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation + PI - angle4))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + PI - angle6))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + PI - angle6))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + angle6))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + angle6))/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation + angle4))/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation + angle4))/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation + angle3))/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation + angle3))/10000000);
                         X7 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + angle5))/10000000);
                         Y7 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + angle5))/10000000);
                         X8 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + PI - angle5))/10000000);
                         Y8 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + PI - angle5))/10000000);
                         XC1:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation + PI))/10000000);
                         YC1:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation + PI))/10000000);
                         XC2:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation))/10000000);
                         YC2:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);

                         if  (LastDelimiter(',', X1) <> 0) then  X1[LastDelimiter(',', X1)] := '.';
                         if  (LastDelimiter(',', Y1) <> 0) then  Y1[LastDelimiter(',', Y1)] := '.';
                         if  (LastDelimiter(',', X2) <> 0) then  X2[LastDelimiter(',', X2)] := '.';
                         if  (LastDelimiter(',', Y2) <> 0) then  Y2[LastDelimiter(',', Y2)] := '.';
                         if  (LastDelimiter(',', X3) <> 0) then  X3[LastDelimiter(',', X3)] := '.';
                         if  (LastDelimiter(',', Y3) <> 0) then  Y3[LastDelimiter(',', Y3)] := '.';
                         if  (LastDelimiter(',', X4) <> 0) then  X4[LastDelimiter(',', X4)] := '.';
                         if  (LastDelimiter(',', Y4) <> 0) then  Y4[LastDelimiter(',', Y4)] := '.';
                         if  (LastDelimiter(',', X5) <> 0) then  X5[LastDelimiter(',', X5)] := '.';
                         if  (LastDelimiter(',', Y5) <> 0) then  Y5[LastDelimiter(',', Y5)] := '.';
                         if  (LastDelimiter(',', X6) <> 0) then  X6[LastDelimiter(',', X6)] := '.';
                         if  (LastDelimiter(',', Y6) <> 0) then  Y6[LastDelimiter(',', Y6)] := '.';
                         if  (LastDelimiter(',', X7) <> 0) then  X7[LastDelimiter(',', X7)] := '.';
                         if  (LastDelimiter(',', Y7) <> 0) then  Y7[LastDelimiter(',', Y7)] := '.';
                         if  (LastDelimiter(',', X8) <> 0) then  X8[LastDelimiter(',', X8)] := '.';
                         if  (LastDelimiter(',', Y8) <> 0) then  Y8[LastDelimiter(',', Y8)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',  R) <> 0) then   R[LastDelimiter(',',  R)] := '.';
                         if  (LastDelimiter(',',  W) <> 0) then   W[LastDelimiter(',',  W)] := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X5 + ' Y1=' + Y5 + ' X2=' + X4 + ' Y2=' + Y4 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X6 + ' Y1=' + Y6 + ' X2=' + X7 + ' Y2=' + Y7 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X8 + ' Y=' + Y8 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X8 + ' Y1=' + Y8 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // fourth hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation - angle3))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation - angle3))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation - angle4))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation - angle4))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation - angle6))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation - angle6))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + PI + angle6))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + PI + angle6))/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation + PI + angle4))/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation + PI + angle4))/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation + PI + angle3))/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation + PI + angle3))/10000000);
                         X7 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + PI + angle5))/10000000);
                         Y7 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + PI + angle5))/10000000);
                         X8 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation - angle5))/10000000);
                         Y8 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation - angle5))/10000000);
                         XC1:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation))/10000000);
                         YC1:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation))/10000000);
                         XC2:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation + PI))/10000000);
                         YC2:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation + PI))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);

                         if  (LastDelimiter(',', X1) <> 0) then  X1[LastDelimiter(',', X1)] := '.';
                         if  (LastDelimiter(',', Y1) <> 0) then  Y1[LastDelimiter(',', Y1)] := '.';
                         if  (LastDelimiter(',', X2) <> 0) then  X2[LastDelimiter(',', X2)] := '.';
                         if  (LastDelimiter(',', Y2) <> 0) then  Y2[LastDelimiter(',', Y2)] := '.';
                         if  (LastDelimiter(',', X3) <> 0) then  X3[LastDelimiter(',', X3)] := '.';
                         if  (LastDelimiter(',', Y3) <> 0) then  Y3[LastDelimiter(',', Y3)] := '.';
                         if  (LastDelimiter(',', X4) <> 0) then  X4[LastDelimiter(',', X4)] := '.';
                         if  (LastDelimiter(',', Y4) <> 0) then  Y4[LastDelimiter(',', Y4)] := '.';
                         if  (LastDelimiter(',', X5) <> 0) then  X5[LastDelimiter(',', X5)] := '.';
                         if  (LastDelimiter(',', Y5) <> 0) then  Y5[LastDelimiter(',', Y5)] := '.';
                         if  (LastDelimiter(',', X6) <> 0) then  X6[LastDelimiter(',', X6)] := '.';
                         if  (LastDelimiter(',', Y6) <> 0) then  Y6[LastDelimiter(',', Y6)] := '.';
                         if  (LastDelimiter(',', X7) <> 0) then  X7[LastDelimiter(',', X7)] := '.';
                         if  (LastDelimiter(',', Y7) <> 0) then  Y7[LastDelimiter(',', Y7)] := '.';
                         if  (LastDelimiter(',', X8) <> 0) then  X8[LastDelimiter(',', X8)] := '.';
                         if  (LastDelimiter(',', Y8) <> 0) then  Y8[LastDelimiter(',', Y8)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',  R) <> 0) then   R[LastDelimiter(',',  R)] := '.';
                         if  (LastDelimiter(',',  W) <> 0) then   W[LastDelimiter(',',  W)] := '.';


                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X5 + ' Y1=' + Y5 + ' X2=' + X4 + ' Y2=' + Y4 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X6 + ' Y1=' + Y6 + ' X2=' + X7 + ' Y2=' + Y7 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X8 + ' Y=' + Y8 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X8 + ' Y1=' + Y8 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if Pad.ReliefEntries = 2 then
                      begin

                         // first hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation + PI + angle1))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation + PI + angle1))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation + PI + angle2))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation + PI + angle2))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + PI - angle6))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + PI - angle6))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + angle6))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + angle6))/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation + angle4))/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation + angle4))/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation + angle3))/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation + angle3))/10000000);
                         X7 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + angle5))/10000000);
                         Y7 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + angle5))/10000000);
                         X8 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + PI - angle5))/10000000);
                         Y8 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + PI - angle5))/10000000);
                         XC1:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation + PI))/10000000);
                         YC1:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation + PI))/10000000);
                         XC2:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation))/10000000);
                         YC2:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);

                         if  (LastDelimiter(',', X1) <> 0) then  X1[LastDelimiter(',', X1)] := '.';
                         if  (LastDelimiter(',', Y1) <> 0) then  Y1[LastDelimiter(',', Y1)] := '.';
                         if  (LastDelimiter(',', X2) <> 0) then  X2[LastDelimiter(',', X2)] := '.';
                         if  (LastDelimiter(',', Y2) <> 0) then  Y2[LastDelimiter(',', Y2)] := '.';
                         if  (LastDelimiter(',', X3) <> 0) then  X3[LastDelimiter(',', X3)] := '.';
                         if  (LastDelimiter(',', Y3) <> 0) then  Y3[LastDelimiter(',', Y3)] := '.';
                         if  (LastDelimiter(',', X4) <> 0) then  X4[LastDelimiter(',', X4)] := '.';
                         if  (LastDelimiter(',', Y4) <> 0) then  Y4[LastDelimiter(',', Y4)] := '.';
                         if  (LastDelimiter(',', X5) <> 0) then  X5[LastDelimiter(',', X5)] := '.';
                         if  (LastDelimiter(',', Y5) <> 0) then  Y5[LastDelimiter(',', Y5)] := '.';
                         if  (LastDelimiter(',', X6) <> 0) then  X6[LastDelimiter(',', X6)] := '.';
                         if  (LastDelimiter(',', Y6) <> 0) then  Y6[LastDelimiter(',', Y6)] := '.';
                         if  (LastDelimiter(',', X7) <> 0) then  X7[LastDelimiter(',', X7)] := '.';
                         if  (LastDelimiter(',', Y7) <> 0) then  Y7[LastDelimiter(',', Y7)] := '.';
                         if  (LastDelimiter(',', X8) <> 0) then  X8[LastDelimiter(',', X8)] := '.';
                         if  (LastDelimiter(',', Y8) <> 0) then  Y8[LastDelimiter(',', Y8)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',  R) <> 0) then   R[LastDelimiter(',',  R)] := '.';
                         if  (LastDelimiter(',',  W) <> 0) then   W[LastDelimiter(',',  W)] := '.';


                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X5 + ' Y1=' + Y5 + ' X2=' + X4 + ' Y2=' + Y4 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X6 + ' Y1=' + Y6 + ' X2=' + X7 + ' Y2=' + Y7 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X8 + ' Y=' + Y8 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X8 + ' Y1=' + Y8 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // second hole
                         X1 := FormatFloat('0.00000',(Pad.x + Line1 * cos(rotation + angle1))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + Line1 * sin(rotation + angle1))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + Line2 * cos(rotation + angle2))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + Line2 * sin(rotation + angle2))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation - angle6))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation - angle6))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + Line6 * cos(rotation + PI + angle6))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + Line6 * sin(rotation + PI + angle6))/10000000);
                         X5 := FormatFloat('0.00000',(Pad.x + Line4 * cos(rotation + PI + angle4))/10000000);
                         Y5 := FormatFloat('0.00000',(Pad.y + Line4 * sin(rotation + PI + angle4))/10000000);
                         X6 := FormatFloat('0.00000',(Pad.x + Line3 * cos(rotation + PI + angle3))/10000000);
                         Y6 := FormatFloat('0.00000',(Pad.y + Line3 * sin(rotation + PI + angle3))/10000000);
                         X7 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation + PI + angle5))/10000000);
                         Y7 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation + PI + angle5))/10000000);
                         X8 := FormatFloat('0.00000',(Pad.x + Line5 * cos(rotation - angle5))/10000000);
                         Y8 := FormatFloat('0.00000',(Pad.y + Line5 * sin(rotation - angle5))/10000000);
                         XC1:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation))/10000000);
                         YC1:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation))/10000000);
                         XC2:= FormatFloat('0.00000',(Pad.x + Line7 * cos(rotation + PI))/10000000);
                         YC2:= FormatFloat('0.00000',(Pad.y + Line7 * sin(rotation + PI))/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Pad.HoleSize / 2 + Pad.PowerPlaneReliefExpansion + Pad.ReliefAirGap)/10000000);

                         if  (LastDelimiter(',', X1) <> 0) then  X1[LastDelimiter(',', X1)] := '.';
                         if  (LastDelimiter(',', Y1) <> 0) then  Y1[LastDelimiter(',', Y1)] := '.';
                         if  (LastDelimiter(',', X2) <> 0) then  X2[LastDelimiter(',', X2)] := '.';
                         if  (LastDelimiter(',', Y2) <> 0) then  Y2[LastDelimiter(',', Y2)] := '.';
                         if  (LastDelimiter(',', X3) <> 0) then  X3[LastDelimiter(',', X3)] := '.';
                         if  (LastDelimiter(',', Y3) <> 0) then  Y3[LastDelimiter(',', Y3)] := '.';
                         if  (LastDelimiter(',', X4) <> 0) then  X4[LastDelimiter(',', X4)] := '.';
                         if  (LastDelimiter(',', Y4) <> 0) then  Y4[LastDelimiter(',', Y4)] := '.';
                         if  (LastDelimiter(',', X5) <> 0) then  X5[LastDelimiter(',', X5)] := '.';
                         if  (LastDelimiter(',', Y5) <> 0) then  Y5[LastDelimiter(',', Y5)] := '.';
                         if  (LastDelimiter(',', X6) <> 0) then  X6[LastDelimiter(',', X6)] := '.';
                         if  (LastDelimiter(',', Y6) <> 0) then  Y6[LastDelimiter(',', Y6)] := '.';
                         if  (LastDelimiter(',', X7) <> 0) then  X7[LastDelimiter(',', X7)] := '.';
                         if  (LastDelimiter(',', Y7) <> 0) then  Y7[LastDelimiter(',', Y7)] := '.';
                         if  (LastDelimiter(',', X8) <> 0) then  X8[LastDelimiter(',', X8)] := '.';
                         if  (LastDelimiter(',', Y8) <> 0) then  Y8[LastDelimiter(',', Y8)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',  R) <> 0) then   R[LastDelimiter(',',  R)] := '.';
                         if  (LastDelimiter(',',  W) <> 0) then   W[LastDelimiter(',',  W)] := '.';


                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X5 + ' Y1=' + Y5 + ' X2=' + X4 + ' Y2=' + Y4 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X6 + ' Y1=' + Y6 + ' X2=' + X7 + ' Y2=' + Y7 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X8 + ' Y=' + Y8 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X8 + ' Y1=' + Y8 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                      end;
                   end;
                end;

                // Direct
                if Pad.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneDirectConnect then
                begin
                      // This Pad directly connected to this plane
                      // We need to make hole for it
                      // We need to make one hole here, and it is hole size
                      if Pad.HoleType = eRoundHole then
                      begin
                         X1 := FormatFloat('0.00000',(Pad.x)/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - (Pad.HoleSize / 2)) /10000000);
                         XC := FormatFloat('0.00000',(Pad.x)/10000000);
                         YC := FormatFloat('0.00000',(Pad.y)/10000000);
                         R  := FormatFloat('0.00000',(Pad.HoleSize / 2)/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if Pad.HoleType = eSquareHole then
                      begin
                         // since this hole can have rotation
                         angle := (Pad.HoleRotation + Pad.Rotation) * PI / 180 + (PI / 4);

                         X1 := FormatFloat('0.00000',(Pad.x + (Pad.HoleSize / 2) * Sqrt(2) * cos(angle))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + (Pad.HoleSize / 2) * Sqrt(2) * sin(angle))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + (Pad.HoleSize / 2) * Sqrt(2) * cos(angle + (PI / 2)))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + (Pad.HoleSize / 2) * Sqrt(2) * sin(angle + (PI / 2)))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + (Pad.HoleSize / 2) * Sqrt(2) * cos(angle + PI))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + (Pad.HoleSize / 2) * Sqrt(2) * sin(angle + PI))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + (Pad.HoleSize / 2) * Sqrt(2) * cos(angle - (PI / 2)))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + (Pad.HoleSize / 2) * Sqrt(2) * sin(angle - (PI / 2)))/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if (Pad.HoleType = eSlotHole) and (Pad.HoleWidth >  Pad.HoleSize) then
                      begin
                         // since this hole can have rotation
                         angle := arctan(Pad.HoleSize / (Pad.HoleWidth - Pad.HoleSize));
                         radLength := sqrt(sqr(Pad.HoleSize / 2) + sqr((Pad.HoleWidth - Pad.HoleSize) / 2));

                         X1  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + angle))/10000000);
                         Y1  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + angle))/10000000);
                         X2  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI - angle))/10000000);
                         Y2  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI - angle))/10000000);
                         X3  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI + angle))/10000000);
                         Y3  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI + angle))/10000000);
                         X4  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 - angle))/10000000);
                         Y4  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 - angle))/10000000);
                         XC1 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleWidth - Pad.HoleSize) / 2) * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180))/10000000);
                         YC1 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleWidth - Pad.HoleSize) / 2) * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180))/10000000);
                         XC2 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleWidth - Pad.HoleSize) / 2) * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI))/10000000);
                         YC2 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleWidth - Pad.HoleSize) / 2) * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI))/10000000);
                         R   := FormatFloat('0.00000',(Pad.HoleSize / 2)/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',R  ) <> 0) then   R[LastDelimiter(',',R  )] := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X2 + ' Y1=' + Y2 + ' X2=' + X3 + ' Y2=' + Y3 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end;
                end;

                // No connect - handle it few lines of code latter, in next "if j = 0 then"
                if Pad.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneNoConnect then
                   begin
                      // This Pad is not connected to this plane
                      // We need to make hole for it
                      // We need to make one hole here, and it is hole size
                      if Pad.HoleType = eRoundHole then
                      begin
                         X1 := FormatFloat('0.00000',Pad.x/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y - (Pad.HoleSize / 2) - Pad.PowerPlaneClearance) /10000000);
                         XC := FormatFloat('0.00000',Pad.x/10000000);
                         YC := FormatFloat('0.00000',Pad.y/10000000);
                         R  := FormatFloat('0.00000',((Pad.HoleSize / 2) + Pad.PowerPlaneClearance)/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if Pad.HoleType = eSquareHole then
                      begin
                         // since this hole can have rotation
                         angle := (Pad.HoleRotation + Pad.Rotation) * PI / 180 + (PI / 4);

                         X1 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * cos(angle))/10000000);
                         Y1 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * sin(angle))/10000000);
                         X2 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * cos(angle + (PI / 2)))/10000000);
                         Y2 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * sin(angle + (PI / 2)))/10000000);
                         X3 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * cos(angle + PI))/10000000);
                         Y3 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * sin(angle + PI))/10000000);
                         X4 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * cos(angle - (PI / 2)))/10000000);
                         Y4 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) * Sqrt(2) * sin(angle - (PI / 2)))/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end
                      else if (Pad.HoleType = eSlotHole) and (Pad.HoleWidth > Pad.HoleSize) then
                      begin
                         // since this hole can have rotation
                         angle := arctan((Pad.HoleSize +  2 * Pad.PowerPlaneClearance) / (Pad.HoleWidth - Pad.HoleSize));
                         radLength := sqrt(sqr((Pad.HoleSize / 2) + Pad.PowerPlaneClearance) + sqr((Pad.HoleWidth - Pad.HoleSize) / 2));

                         X1  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + angle))/10000000);
                         Y1  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + angle))/10000000);
                         X2  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI - angle))/10000000);
                         Y2  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI - angle))/10000000);
                         X3  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI + angle))/10000000);
                         Y3  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI + angle))/10000000);
                         X4  := FormatFloat('0.00000',(Pad.x + radLength * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 - angle))/10000000);
                         Y4  := FormatFloat('0.00000',(Pad.y + radLength * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 - angle))/10000000);
                         XC1 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleWidth - Pad.HoleSize) / 2) * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180))/10000000);
                         YC1 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleWidth - Pad.HoleSize) / 2) * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180))/10000000);
                         XC2 := FormatFloat('0.00000',(Pad.x + ((Pad.HoleWidth - Pad.HoleSize) / 2) * cos((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI))/10000000);
                         YC2 := FormatFloat('0.00000',(Pad.y + ((Pad.HoleWidth - Pad.HoleSize) / 2) * sin((Pad.HoleRotation + Pad.Rotation) * PI / 180 + PI))/10000000);
                         R   := FormatFloat('0.00000',((Pad.HoleSize / 2) + Pad.PowerPlaneClearance)/10000000);

                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                         if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                         if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                         if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                         if  (LastDelimiter(',',R  ) <> 0) then   R[LastDelimiter(',',R  )] := '.';

                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X2 + ' Y1=' + Y2 + ' X2=' + X3 + ' Y2=' + Y3 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);
                      end;
                end;
             end
             else if Layer2String(Pad.Layer) = pcbLayerName then
             begin
                // This is pad on plane only. It's size is a hole
                // Modifying points for hyp file

                // Now, since pad can be Round, octagonal, rectangle or rounded rectangle
                // we will be handling all situations at once.

                X := Pad.x;
                Y := Pad.y;

                SizeX := Pad.TopXSize;
                SizeY := Pad.TopYSize;
                rotation := Pad.Rotation * PI / 180;

                if Pad.TopShape = eRounded then
                begin
                   if SizeX < SizeY then
                      CornerRadius := SizeX / 2
                   else
                      CornerRadius := SizeY / 2;
                end;

                if Pad.TopShape = eRoundedRectangular then
                begin
                   // we need to check Altium version for this
                   W := Client.GetServerRecordByName('CLIENT').GetVersion;
                   if W[9] = '1' then
                   begin
                      if SizeX < SizeY then
                         CornerRadius := SizeX * Pad.CRPercentage[Split.Layer] / 200
                      else
                         CornerRadius := SizeY * Pad.CRPercentage[Split.Layer] / 200;
                   end
                   else // if W[9] = '9' then   // if it is before release 10
                   begin
                      if SizeX < SizeY then
                         CornerRadius := SizeX * Pad.CRPercentage[Split.V7_Layer] / 200
                      else
                         CornerRadius := SizeY * Pad.CRPercentage[Split.V7_Layer] / 200; 
                   end;
                end;

                if Pad.TopShape = eOctagonal then
                begin
                   if SizeX < SizeY then
                      CornerRadius := SizeX / 4
                   else
                      CornerRadius := SizeY / 4;
                end;

                if Pad.TopShape = eRectangular then
                   CornerRadius := 0;

                Line1 :=  sqrt(sqr(SizeX / 2) + sqr((SizeY / 2) - CornerRadius));
                Line2 :=  sqrt(sqr((SizeX / 2) - CornerRadius) + sqr(SizeY / 2));

                angle1 := arccos((sizeX / 2) / Line1);
                angle2 := arcsin((SizeY / 2) / Line2);

                radLength := sqrt(sqr((sizeX / 2) - CornerRadius) + sqr((sizeY / 2) - CornerRadius));

                if radLength = 0 then   phiin := 0
                else                    phiin := arccos(((SizeX / 2) - CornerRadius) / radLength);



                X1 := FormatFloat('0.00000',(X + Line1 * cos(rotation + angle1))/10000000);
                Y1 := FormatFloat('0.00000',(Y + Line1 * sin(rotation + angle1))/10000000);
                X2 := FormatFloat('0.00000',(X + Line2 * cos(rotation + angle2))/10000000);
                Y2 := FormatFloat('0.00000',(Y + Line2 * sin(rotation + angle2))/10000000);
                X3 := FormatFloat('0.00000',(X + Line2 * cos(rotation + PI - angle2))/10000000);
                Y3 := FormatFloat('0.00000',(Y + Line2 * sin(rotation + PI - angle2))/10000000);
                X4 := FormatFloat('0.00000',(X + Line1 * cos(rotation + PI - angle1))/10000000);
                Y4 := FormatFloat('0.00000',(Y + Line1 * sin(rotation + PI - angle1))/10000000);
                X5 := FormatFloat('0.00000',(X + Line1 * cos(rotation + PI + angle1))/10000000);
                Y5 := FormatFloat('0.00000',(Y + Line1 * sin(rotation + PI + angle1))/10000000);
                X6 := FormatFloat('0.00000',(X + Line2 * cos(rotation + PI + angle2))/10000000);
                Y6 := FormatFloat('0.00000',(Y + Line2 * sin(rotation + PI + angle2))/10000000);
                X7 := FormatFloat('0.00000',(X + Line2 * cos(rotation - angle2))/10000000);
                Y7 := FormatFloat('0.00000',(Y + Line2 * sin(rotation - angle2))/10000000);
                X8 := FormatFloat('0.00000',(X + Line1 * cos(rotation - angle1))/10000000);
                Y8 := FormatFloat('0.00000',(Y + Line1 * sin(rotation - angle1))/10000000);
                XC1:= FormatFloat('0.00000',(X + radLength * cos(rotation + phiin))/10000000);
                YC1:= FormatFloat('0.00000',(Y + radLength * sin(rotation + phiin))/10000000);
                XC2:= FormatFloat('0.00000',(X + radLength * cos(rotation + PI - phiin))/10000000);
                YC2:= FormatFloat('0.00000',(Y + radLength * sin(rotation + PI - phiin))/10000000);
                XC3:= FormatFloat('0.00000',(X + radLength * cos(rotation + PI + phiin))/10000000);
                YC3:= FormatFloat('0.00000',(Y + radLength * sin(rotation + PI + phiin))/10000000);
                XC4:= FormatFloat('0.00000',(X + radLength * cos(rotation - phiin))/10000000);
                YC4:= FormatFloat('0.00000',(Y + radLength * sin(rotation - phiin))/10000000);
                R  := FormatFloat('0.00000',CornerRadius/10000000);



                if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                if  (LastDelimiter(',',X5) <> 0) then X5[LastDelimiter(',',X5)] := '.';
                if  (LastDelimiter(',',Y5) <> 0) then Y5[LastDelimiter(',',Y5)] := '.';
                if  (LastDelimiter(',',X6) <> 0) then X6[LastDelimiter(',',X6)] := '.';
                if  (LastDelimiter(',',Y6) <> 0) then Y6[LastDelimiter(',',Y6)] := '.';
                if  (LastDelimiter(',',X7) <> 0) then X7[LastDelimiter(',',X7)] := '.';
                if  (LastDelimiter(',',Y7) <> 0) then Y7[LastDelimiter(',',Y7)] := '.';
                if  (LastDelimiter(',',X8) <> 0) then X8[LastDelimiter(',',X8)] := '.';
                if  (LastDelimiter(',',Y8) <> 0) then Y8[LastDelimiter(',',Y8)] := '.';
                if  (LastDelimiter(',',XC1) <> 0) then XC1[LastDelimiter(',',XC1)] := '.';
                if  (LastDelimiter(',',YC1) <> 0) then YC1[LastDelimiter(',',YC1)] := '.';
                if  (LastDelimiter(',',XC2) <> 0) then XC2[LastDelimiter(',',XC2)] := '.';
                if  (LastDelimiter(',',YC2) <> 0) then YC2[LastDelimiter(',',YC2)] := '.';
                if  (LastDelimiter(',',XC3) <> 0) then XC3[LastDelimiter(',',XC3)] := '.';
                if  (LastDelimiter(',',YC3) <> 0) then YC3[LastDelimiter(',',YC3)] := '.';
                if  (LastDelimiter(',',XC4) <> 0) then XC4[LastDelimiter(',',XC4)] := '.';
                if  (LastDelimiter(',',YC4) <> 0) then YC4[LastDelimiter(',',YC4)] := '.';
                if  (LastDelimiter(',',R  ) <> 0) then   R[LastDelimiter(',',R  )] := '.';

                // Now we need to write this inside
                hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                Inc(i);

                if not ((X1 = X2) and (Y1 = Y2)) then
                begin
                   if Pad.TopShape = eOctagonal then
                   begin
                      hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                      Inc(i);
                   end
                   else if (Pad.TopShape = eRoundedRectangular) or (Pad.TopShape = eRounded) then
                   begin
                      hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC1 + ' YC=' + YC1 + ' R=' + R + ')');
                      Inc(i);
                   end;
                end;


                if not ((X2 = X3) and (Y2 = Y3)) then
                begin
                   hypFile.Insert(i,'    (LINE X=' + X3 + ' Y=' + Y3 + ')');
                   Inc(i);
                end;


                if not ((X3 = X4) and (Y3 = Y4)) then
                begin
                   if Pad.TopShape = eOctagonal then
                   begin
                      hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                      Inc(i);
                   end
                   else if (Pad.TopShape = eRoundedRectangular) or (Pad.TopShape = eRounded) then
                   begin
                      hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X4 + ' Y2=' + Y4 + ' XC=' + XC2 + ' YC=' + YC2 + ' R=' + R + ')');
                      Inc(i);
                   end;
                end;


                if not ((X4 = X5) and (Y4 = Y5)) then
                begin
                   hypFile.Insert(i,'    (LINE X=' + X5 + ' Y=' + Y5 + ')');
                   Inc(i);
                end;


                if not ((X5 = X6) and (Y5 = Y6)) then
                   begin
                   if Pad.TopShape = eOctagonal then
                   begin
                      hypFile.Insert(i,'    (LINE X=' + X6 + ' Y=' + Y6 + ')');
                      Inc(i);
                   end
                   else if (Pad.TopShape = eRoundedRectangular) or (Pad.TopShape = eRounded) then
                   begin
                      hypFile.Insert(i,'    (CURVE X1=' + X5 + ' Y1=' + Y5 + ' X2=' + X6 + ' Y2=' + Y6 + ' XC=' + XC3 + ' YC=' + YC3 + ' R=' + R + ')');
                      Inc(i);
                   end;
                end;


                if not ((X6 = X7) and (Y6 = Y7)) then
                begin
                   hypFile.Insert(i,'    (LINE X=' + X7 + ' Y=' + Y7 + ')');
                   Inc(i);
                end;


                if not ((X7 = X8) and (Y7 = Y8)) then
                begin
                   if Pad.TopShape = eOctagonal then
                   begin
                      hypFile.Insert(i,'    (LINE X=' + X8 + ' Y=' + Y8 + ')');
                      Inc(i);
                   end
                   else if (Pad.TopShape = eRoundedRectangular) or (Pad.TopShape = eRounded) then
                   begin
                      hypFile.Insert(i,'    (CURVE X1=' + X7 + ' Y1=' + Y7 + ' X2=' + X8 + ' Y2=' + Y8 + ' XC=' + XC4 + ' YC=' + YC4 + ' R=' + R + ')');
                      Inc(i);
                   end;
                end;


                if not ((X1 = X8) and (Y1 = Y8)) then
                begin
                   hypFile.Insert(i,'    (LINE X=' + X1 + ' Y=' + Y1 + ')');
                   Inc(i);
                end;


                hypFile.Insert(i,'    }');
                Inc(i);
             end;
             Pad := Iterator.NextPCBObject;
          end;
          Board.BoardIterator_Destroy(Iterator);


          // Vias

          Iterator := Board.BoardIterator_Create;
          Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
          Iterator.AddFilter_LayerSet(AllLayers);
          Iterator.AddFilter_Method(eProcessAll);

          Via := Iterator.FirstPCBObject;

          while (Via <> nil) do
          begin
             // we need to check weather via goes through current layer
             j := 0;
             LayerObj := TheLayerStack.FirstLayer;
             Repeat
                if Layer2String(LayerObj.LayerID) = Layer2String(Via.StartLayer.LayerID) then j := 1;
                if Layer2String(LayerObj.LayerID) = Layer2String(Split.Layer) then break;
                if Layer2String(LayerObj.LayerID) = Layer2String(Via.StopLayer.LayerID) then j := 0;

                LayerObj := TheLayerStack.NextLayer(LayerObj);
             Until LayerObj = Nil;

             if j = 1 then
             begin
                // This is where we know that this via intersects current layer

                // we will check weather it is connected to plane, so that we
                // know weather we have to make connection or clearence
                if Via.IsConnectedToPlane(Split.Layer) then
                begin
                   // This via is connected to this plane
                   // we need to check weather it is on thermals or direct

                   // Thermals
                   if Via.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneReliefConnect then
                   begin
                      // We need to make 4 (or 2) holes here because these are thermals
                      phiin := arccos(Via.ReliefConductorWidth / (2 * Via.PowerPlaneReliefExpansion + Via.HoleSize));
                      phiin := phiin - (PI / 4);

                      // We will be using variables X2pount, and Y2point for X,Y for relative points of nearer arc
                      // and X4Point and Y4point for relative points of further arc. This is bit hard to explain,
                      // so you are gonna have to trust me.

                      X2Point := (Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion) * cos(phiin);
                      Y2Point := (Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion) * sin(phiin);

                      phiin := arccos(Via.ReliefConductorWidth / (2 * (Via.PowerPlaneReliefExpansion + Via.ReliefAirGap) + Via.HoleSize));
                      phiin := phiin - (PI / 4);

                      X4Point := (Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap) * cos(phiin);
                      Y4Point := (Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap) * sin(phiin);


                      if Via.ReliefEntries = 4 then
                      begin

                         // Modifying points for hyp file - first hole
                         X1 := FormatFloat('0.00000',(Via.x + X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y + Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x + X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y + Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x + X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y - Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x + X2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y - Y2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - Second hole
                         X1 := FormatFloat('0.00000',(Via.x - Y2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y + X2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x - Y4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y + X4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x + Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y + X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x + Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y + X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - third hole
                         X1 := FormatFloat('0.00000',(Via.x - X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y - Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x - X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y - Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x - X4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y + Y4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x - X2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y + Y2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                         // Modifying points for hyp file - fourth hole
                         X1 := FormatFloat('0.00000',(Via.x + Y2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y - X2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x + Y4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y - X4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x - Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y - X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x - Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y - X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                      end
                      else if Via.ReliefEntries = 2 then
                      begin

                         // Modifying points for hyp file - first hole
                         X1 := FormatFloat('0.00000',(Via.x + X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y + Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x + X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y + Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x - Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y - X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x - Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y - X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);


                         // Modifying points for hyp file - second hole
                         X1 := FormatFloat('0.00000',(Via.x - X2Point)/10000000);
                         Y1 := FormatFloat('0.00000',(Via.y - Y2Point)/10000000);
                         X2 := FormatFloat('0.00000',(Via.x - X4Point)/10000000);
                         Y2 := FormatFloat('0.00000',(Via.y - Y4Point)/10000000);
                         X3 := FormatFloat('0.00000',(Via.x + Y4Point)/10000000);
                         Y3 := FormatFloat('0.00000',(Via.y + X4Point)/10000000);
                         X4 := FormatFloat('0.00000',(Via.x + Y2Point)/10000000);
                         Y4 := FormatFloat('0.00000',(Via.y + X2Point)/10000000);
                         XC := FormatFloat('0.00000',(Via.x)/10000000);
                         YC := FormatFloat('0.00000',(Via.y)/10000000);
                         R  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion)/10000000);
                         W  := FormatFloat('0.00000',(Via.HoleSize / 2 + Via.PowerPlaneReliefExpansion + Via.ReliefAirGap)/10000000);


                         if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                         if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                         if  (LastDelimiter(',',X2) <> 0) then X2[LastDelimiter(',',X2)] := '.';
                         if  (LastDelimiter(',',Y2) <> 0) then Y2[LastDelimiter(',',Y2)] := '.';
                         if  (LastDelimiter(',',X3) <> 0) then X3[LastDelimiter(',',X3)] := '.';
                         if  (LastDelimiter(',',Y3) <> 0) then Y3[LastDelimiter(',',Y3)] := '.';
                         if  (LastDelimiter(',',X4) <> 0) then X4[LastDelimiter(',',X4)] := '.';
                         if  (LastDelimiter(',',Y4) <> 0) then Y4[LastDelimiter(',',Y4)] := '.';
                         if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                         if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                         if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';
                         if  (LastDelimiter(',',W)  <> 0) then  W[LastDelimiter(',',W)]  := '.';

                         // Now we need to write this inside
                         hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X2 + ' Y=' + Y2 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X3 + ' Y1=' + Y3 + ' X2=' + X2 + ' Y2=' + Y2 + ' XC=' + XC + ' YC=' + YC + ' R=' + W + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (LINE X=' + X4 + ' Y=' + Y4 + ')');
                         Inc(i);
                         hypFile.Insert(i,'    (CURVE X1=' + X4 + ' Y1=' + Y4 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                         Inc(i);
                         hypFile.Insert(i,'    }');
                         Inc(i);

                      end;
                   end;

                   // Direct
                   if Via.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneDirectConnect then
                   begin
                      // We need to make one hole here, and it is hole size
                      X1 := FormatFloat('0.00000',Via.x/10000000);
                      Y1 := FormatFloat('0.00000',(Via.y - (Via.HoleSize / 2)) /10000000);
                      XC := FormatFloat('0.00000',Via.x/10000000);
                      YC := FormatFloat('0.00000',Via.y/10000000);
                      R  := FormatFloat('0.00000',(Via.HoleSize / 2)/10000000);

                      if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                      if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                      if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                      if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                      if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';

                      hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                      Inc(i);
                      hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                      Inc(i);
                      hypFile.Insert(i,'    }');
                      Inc(i);
                   end;

                   // No connect - handle it few lines of code latter, in next "if j = 0 then"
                   if Via.PlaneConnectionStyleForLayer(Split.Layer) = ePlaneNoConnect then
                   begin
                      // This via is not connected to this plane
                      // We need to make hole for it
                      // We need to make one hole here, and it is hole size
                      X1 := FormatFloat('0.00000',Via.x/10000000);
                      Y1 := FormatFloat('0.00000',(Via.y - Via.PowerPlaneClearance - (Via.HoleSize / 2) )/10000000);
                      XC := FormatFloat('0.00000',Via.x/10000000);
                      YC := FormatFloat('0.00000',Via.y/10000000);
                      R  := FormatFloat('0.00000',(Via.PowerPlaneClearance + (Via.HoleSize / 2))/10000000);

                      if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                      if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                      if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                      if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                      if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';

                      hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                      Inc(i);
                      hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                      Inc(i);
                      hypFile.Insert(i,'    }');
                      Inc(i);
                   end;
                end
                else
                begin
                   // This via is not connected to this plane
                   // We need to make hole for it
                   // We need to make one hole here, and it is hole size
                   X1 := FormatFloat('0.00000',Via.x/10000000);
                   Y1 := FormatFloat('0.00000',(Via.y - (Via.HoleSize / 2) - Via.PowerPlaneClearance) /10000000);
                   XC := FormatFloat('0.00000',Via.x/10000000);
                   YC := FormatFloat('0.00000',Via.y/10000000);
                   R  := FormatFloat('0.00000',((Via.HoleSize / 2) + Via.PowerPlaneClearance)/10000000);

                   if  (LastDelimiter(',',X1) <> 0) then X1[LastDelimiter(',',X1)] := '.';
                   if  (LastDelimiter(',',Y1) <> 0) then Y1[LastDelimiter(',',Y1)] := '.';
                   if  (LastDelimiter(',',XC) <> 0) then XC[LastDelimiter(',',XC)] := '.';
                   if  (LastDelimiter(',',YC) <> 0) then YC[LastDelimiter(',',YC)] := '.';
                   if  (LastDelimiter(',',R)  <> 0) then  R[LastDelimiter(',',R)]  := '.';

                   hypFile.Insert(i,'    {POLYVOID ID=' + IntToStr(ID) + ' X=' + X1 + ' Y=' + Y1);
                   Inc(i);
                   hypFile.Insert(i,'    (CURVE X1=' + X1 + ' Y1=' + Y1 + ' X2=' + X1 + ' Y2=' + Y1 + ' XC=' + XC + ' YC=' + YC + ' R=' + R + ')');
                   Inc(i);
                   hypFile.Insert(i,'    }');
                   Inc(i);
                end;
             end;
             Via := Iterator.NextPCBObject;
          end;
          Board.BoardIterator_Destroy(Iterator);


          Region := PolyIterator.NextPCBObject;
       end;
       Split.GroupIterator_Destroy(PolyIterator);

       Split := BoardIterator.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(BoardIterator);


    // Saving "hypFile" StringList to file
    hypFile.SaveToFile(SaveAs);
    if Doc <> nil then Doc.SetFileName(FileName);
end;
