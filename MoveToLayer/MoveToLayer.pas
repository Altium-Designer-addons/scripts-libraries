{..............................................................................}
{ Summary   This script moves selected copper tracks to another signal layer,  }
{           while maintaining connectivity with Vias that are automatically    }
{           placed.                                                            }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
    Board         : IPCB_Board;
    TheLayerStack : IPCB_LayerStack;
    LayerObj      : IPCB_LayerObject;
    LastLayerNum  : Integer;
    MaxWidth      : Integer;


{..............................................................................}
{                                                                              }
{   function that places new via                                               }
{                                                                              }
{..............................................................................}
Procedure PlaceVia(Layer : TLayer; X : Integer; Y : Integer; Layer2 : String; Net : IPCB_Net);
var
    BoardIterator : IPCB_SpatialIterator;
    PCBLayerPair  : IPCB_DrillLayerPair;
    LowLayerObj   : IPCB_LayerObject;
    HighLayerObj  : IPCB_LayerObject;
    LowPos        : Integer;
    HighPos       : Integer;
    LowDrillPos   : Integer;
    HighDrillPos  : Integer;
    ViaHighLayer  : Integer;
    ViaLowlayer   : Integer;

    i, j          : Integer;

    FinalLayer    : TLayer;
    Rule          : IPCB_Rule;
    Via           : IPCB_Via;
    Primitive     : IPCB_Primitive;
    Rectangle     : TCoordRect;
begin
   FinalLayer := String2Layer(Layer2);

   // We will create via
   Via := PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);

   Via.x := X;
   Via.y := Y;
   Via.Net := Net;

   // We need to get highLayer and lowLayer. Tough
   i := 0;
   HighPos := 0;
   LowPos := 0;

   LayerObj := TheLayerStack.FirstLayer;
   Repeat
       Inc(i);

       If LayerObj.LayerID = Layer then
       begin
          if HighPos = 0 then
             HighPos := i
          else
             LowPos  := i;
       end
       else if Layerobj.LayerID = FinalLayer then
       begin
          if HighPos = 0 then
             HighPos := i
          else
             LowPos  := i;
       end;

       LayerObj := TheLayerStack.NextLayer(LayerObj);
   Until LayerObj = Nil;


   ViaHighLayer := 1;
   ViaLowLayer  := LastLayerNum;

   For j := 0 To Board.DrillLayerPairsCount - 1 Do
   Begin
       PCBLayerPair := Board.LayerPair[j];

       LowLayerObj  := Board.LayerStack.LayerObject[PCBLayerPair.LowLayer];
       HighLayerObj := Board.LayerStack.LayerObject[PCBLayerPair.HighLayer];

       // sort layer objects
       LayerObj := TheLayerStack.FirstLayer;
       HighDrillPos := 0;
       LowDrillPos  := 0;
       i := 0;
       Repeat
          Inc(i);
          if HighLayerObj = LayerObj then
          begin
             if HighDrillPos = 0 then
                HighDrillPos := i
             else
                LowDrillPos := i;
          end
          else if LowLayerObj = LayerObj then
          begin
             if HighDrillPos = 0 then
                HighDrillPos := i
             else
                LowDrillPos := i;
          end;
          LayerObj := TheLayerStack.NextLayer(LayerObj);
       Until LayerObj = Nil;

       if ((HighDrillPos <= HighPos) and (LowDrillPos >= LowPos) and (HighDrillPos >= ViaHighLayer) and (LowDrillPos <= ViaLowLayer)) then
       begin
          ViaHighLayer := HighDrillPos;
          ViaLowLayer  := LowDrillPos;
       end;
   End;

   i := 0;
   LayerObj := TheLayerStack.FirstLayer;
   Repeat
       Inc(i);

       If i = ViaHighLayer then
          Via.HighLayer := LayerObj.LayerID
       else if i = ViaLowLayer then
          Via.LowLayer  := LayerObj.LayerID;

       LayerObj := TheLayerStack.NextLayer(LayerObj);
   Until LayerObj = Nil;


   Via.Size      := MilsToCoord(50);
   Via.HoleSize  := MilsToCoord(20);

   Board.AddPCBObject(Via);

    // now we need to get the rule, to figure out via size and hole size
   Rule := Board.FindDominantRuleForObject(Via, eRule_RoutingViaStyle);

   Via.Size     := Rule.PreferedWidth;
   Via.HoleSize := Rule.PreferedHoleWidth;

   Rectangle := Via.BoundingRectangle;

   BoardIterator := Board.SpatialIterator_Create;
   BoardIterator.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);
   BoardIterator.AddFilter_ObjectSet(AllObjects);
   BoardIterator.AddFilter_Area(Rectangle.Left - MaxWidth, Rectangle.Bottom - MaxWidth, Rectangle.Right + MaxWidth, Rectangle.Top + MaxWidth);

   Primitive := BoardIterator.FirstPCBObject;

   while Primitive <> Nil do
   begin
      if Via.IntersectLayer(Primitive.Layer) then
      Begin
         Rule := Board.FindDominantRuleForObjectPair(Via, Primitive, eRule_Clearance);

         if Rule <> nil then
         begin
            Board.AddPCBObject(Rule.ActualCheck(Via, Primitive));
            Primitive.GraphicallyInvalidate;
         end;
      end;
      Primitive := BoardIterator.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(BoardIterator);
   Via.GraphicallyInvalidate;

end;


{..............................................................................}
{                                                                              }
{   function that modifies via                                                 }
{                                                                              }
{..............................................................................}
Procedure ModifyVia(Via : IPCB_Via);
var
    BoardIterator : IPCB_SpatialIterator;
    Primitive     : IPCB_Primitive;
    Rule          : IPCB_Rule;

    PCBLayerPair  : IPCB_DrillLayerPair;
    LowLayerObj   : IPCB_LayerObject;
    HighLayerObj  : IPCB_LayerObject;
    LowPos        : Integer;
    HighPos       : Integer;
    LowDrillPos   : Integer;
    HighDrillPos  : Integer;
    ViaHighLayer  : Integer;
    ViaLowlayer   : Integer;

    i, j          : integer;
    Rectangle     : TCoordRect;
begin
   // modify existing via or delete via


   // now we need to find all tracks or arcs on the same place and in the same net
   BoardIterator := Board.SpatialIterator_Create;
   BoardIterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
   BoardIterator.AddFilter_LayerSet(AllLayers);
   BoardIterator.AddFilter_Area(Via.x - 1, Via.y - 1, Via.x + 1, Via.y + 1);

   Primitive := BoardIterator.FirstPCBObject;

   HighPos := LastLayerNum;
   LowPos  := 0;

   While (Primitive <> Nil) Do
   Begin
      if (Primitive.InNet) then
         if Primitive.Net.Name = Via.Net.Name then
         begin
            LayerObj := TheLayerStack.FirstLayer;
            i := 0;
            Repeat
                Inc(i);

                If Primitive.Layer = LayerObj.LayerID then
                begin
                   if i < HighPos then HighPos := i;
                   if i > LowPos  then LowPos := i;
                end;

                LayerObj := TheLayerStack.NextLayer(LayerObj);
            Until LayerObj = Nil;

         end;
      Primitive := BoardIterator.NextPCBObject;
   End;
   Board.SpatialIterator_Destroy(BoardIterator);

   if HighPos = LowPos then
      Board.RemovePCBObject(Via)
   else
   begin
      ViaHighLayer := 1;
      ViaLowLayer  := LastLayerNum;

      For j := 0 To Board.DrillLayerPairsCount - 1 Do
      Begin
          PCBLayerPair := Board.LayerPair[j];

          LowLayerObj  := Board.LayerStack.LayerObject[PCBLayerPair.LowLayer];
          HighLayerObj := Board.LayerStack.LayerObject[PCBLayerPair.HighLayer];

          // sort layer objects
          LayerObj := TheLayerStack.FirstLayer;
          HighDrillPos := 0;
          LowDrillPos  := 0;
          i := 0;
          Repeat
             Inc(i);
             if HighLayerObj = LayerObj then
             begin
                if HighDrillPos = 0 then
                   HighDrillPos := i
                else
                   LowDrillPos := i;
             end
             else if LowLayerObj = LayerObj then
             begin
                if HighDrillPos = 0 then
                   HighDrillPos := i
                else
                   LowDrillPos := i;
             end;
             LayerObj := TheLayerStack.NextLayer(LayerObj);
          Until LayerObj = Nil;

          if ((HighDrillPos <= HighPos) and (LowDrillPos >= LowPos) and (HighDrillPos >= ViaHighLayer) and (LowDrillPos <= ViaLowLayer)) then
          begin
             ViaHighLayer := HighDrillPos;
             ViaLowLayer  := LowDrillPos;
          end;
      End;

      i := 0;
      LayerObj := TheLayerStack.FirstLayer;
      Repeat
          Inc(i);

          If i = ViaHighLayer then
             Via.HighLayer := LayerObj.LayerID
          else if i = ViaLowLayer then
             Via.LowLayer  := LayerObj.LayerID;

          LayerObj := TheLayerStack.NextLayer(LayerObj);
      Until LayerObj = Nil;

      Rule := Board.FindDominantRuleForObject(Via, eRule_RoutingViaStyle);

      Via.Size     := Rule.PreferedWidth;
      Via.HoleSize := Rule.PreferedHoleWidth;


      (*
      if (Via.Size < Rule.MinWidth) then
         Via.Size     := Rule.MinWidth;

      if (Via.Size > Rule.MaxWidth) then
         Via.Size     := Rule.MaxWidth;

      If (Via.HoleSize < Rule.MinHoleWidth) then
         Via.HoleSize := Rule.MinHoleWidth;

      if (Via.HoleSize > Rule.MaxHoleWidth) then
         Via.HoleSize := Rule.MaxHoleWidth;
      *)

      Via.GraphicallyInvalidate;

      Rectangle := Via.BoundingRectangle;

      BoardIterator := Board.SpatialIterator_Create;
      BoardIterator.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);
      BoardIterator.AddFilter_ObjectSet(AllObjects);
      BoardIterator.AddFilter_Area(Rectangle.Left - MaxWidth, Rectangle.Bottom - MaxWidth, Rectangle.Right + MaxWidth, Rectangle.Top + MaxWidth);

      Primitive := BoardIterator.FirstPCBObject;

      while Primitive <> Nil do
      begin
         if Via.IntersectLayer(Primitive.Layer) then
         Begin
            Rule := Board.FindDominantRuleForObjectPair(Via, Primitive, eRule_Clearance);

            if Rule <> nil then
            begin
               Board.AddPCBObject(Rule.ActualCheck(Via, Primitive));
               Primitive.GraphicallyInvalidate;
            end;
         end;
         Primitive := BoardIterator.NextPCBObject;
      end;
      Board.SpatialIterator_Destroy(BoardIterator);

      Via.GraphicallyInvalidate;

   end;
end;


{..............................................................................}
{                                                                              }
{   Main procedure that is executed on "OK" click                              }
{                                                                              }
{..............................................................................}
procedure TMoveToLayer.ButtonOKClick(Sender: TObject);
Var

    Iterator      : IPCB_BoardIterator;
    Rule          : IPCB_Rule;
    BoardIterator : IPCB_SpatialIterator;
    FinalLayer    : String;
    Prim          : IPCB_Primitive;
    Prim1         : IPCB_Primitive;
    Prim2         : IPCB_Primitive;
    Layer1        : String;
    Layer2        : String;
    NetName1      : String;
    NetName2      : String;
    X11           : Integer;
    X12           : Integer;
    Y11           : Integer;
    Y12           : Integer;
    X21           : Integer;
    X22           : Integer;
    Y21           : Integer;
    Y22           : Integer;
    i, j          : Integer;

    Flag1         : Integer;
    Flag2         : Integer;
    Rectangle     : TCoordRect;
    Violation     : IPCB_Violation;

Begin
    // There will be two big steps:
    // 1 - move tracks/arcs to layer
    // 2 - place/remove/modify via
    // second step is a lot more complicated, and I will need to think it through.


    if Board.SelectecObjectCount = 0 then
    begin
       ShowMessage('No selected objects');
       close;
    end;    

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
       if ((Board.SelectecObject[i].ObjectId <> eTrackObject) and (Board.SelectecObject[i].ObjectId <> eArcObject)) then
       begin
          ShowMessage('Please select only tracks or/and arcs');
          close;
       end;
       if (not Board.SelectecObject[i].InNet) then
       begin
          ShowMessage('All selected objects must be in a net');
          close;
       end;
    end;

    FinalLayer := '';

    LayerObj := TheLayerStack.FirstLayer;
    Repeat
        If LayerObj.Name = ComboBoxLayer.Text Then
           FinalLayer := Layer2String(LayerObj.V7_LayerID);


        LayerObj := TheLayerStack.NextLayer(LayerObj);
    Until LayerObj = Nil;


    // Now LayerObj contains Final Layer
    if FinalLayer = '' then close;

    // now we have info about selected primitives. We need to cycle through it because
    // I need to figure out potential places where Via ahould be Added/Deleted/Modified


    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eRuleObject));

    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    MaxWidth := 0;

    Rule := Iterator.FirstPCBObject;
    While (Rule <> Nil) Do
    Begin
        if (Rule.RuleKind = eRule_Clearance) and Rule.Enabled then
           if MaxWidth < Rule.Gap then
              MaxWidth := Rule.Gap;

        Rule := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
       Prim1 := Board.SelectecObject[i];

       NetName1   := Prim1.Net.Name;
       Layer1     := Prim1.Layer;
       If Prim1.ObjectID = eTrackObject then
       begin
          X11        := Prim1.x1;
          Y11        := Prim1.y1;
          X12        := Prim1.x2;
          Y12        := Prim1.y2;
       end
       else if Prim1.ObjectID = eArcObject then
       begin
          X11        := Prim1.StartX;
          Y11        := Prim1.StartY1;
          X12        := Prim1.EndX;
          Y12        := Prim1.EndY;
       end;

       Flag1 := 0;
       Flag2 := 0;

       Prim1.BeginModify;
       Prim1.Layer := String2Layer(FinalLayer);
       Prim1.EndModify;
       Prim1.GraphicallyInvalidate;

       // first thing we need to look is does via already exist on edge

       BoardIterator := Board.SpatialIterator_Create;
       BoardIterator.AddFilter_ObjectSet(MkSet(eViaObject, ePadObject));
       BoardIterator.AddFilter_LayerSet(AllLayers);
       BoardIterator.AddFilter_Area(X11 - 1, Y11 - 1, X11 + 1, Y11 + 1);

       Prim2 := BoardIterator.FirstPCBObject;
       While (Prim2 <> Nil) Do
       Begin
          if Prim2.InNet then
             if (Prim2.net.Name = NetName1) then
                if ((X11 = Prim2.x) and (Y11 = Prim2.y)) then
                   if Prim2.ObjectID = eViaobject then
                   begin
                      ModifyVia(Prim2);
                      Flag1 := 1;
                   end
                   else
                   begin
                      // if there is pad on multi layer - do nothing
                      if Layer2String(Prim2.Layer) = 'Multi Layer' then
                         Flag2 := 1;
                   end;
          Prim2 := BoardIterator.NextPCBObject;
       End;
       Board.SpatialIterator_Destroy(BoardIterator);

       // Now we need to see weather we have unselected tracks/Arcs on
       // source layer, and at which place. this happens only if there was no via
       // or multi-layer pad at the place

       if Flag1 = 0 then
       begin
          BoardIterator := Board.SpatialIterator_Create;
          BoardIterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
          BoardIterator.AddFilter_LayerSet(MkSet(Layer1));
          BoardIterator.AddFilter_Area(X11 - 1, Y11 - 1, X11 + 1, Y11 + 1);

          Prim2 := BoardIterator.FirstPCBObject;
          While (Prim2 <> Nil) Do
          Begin
             if ((not Prim2.Selected) and (Prim2.net.Name = NetName1)) then
             begin
                If Prim2.ObjectID = eTrackObject then
                begin
                   X21        := Prim2.x1;
                   Y21        := Prim2.y1;
                   X22        := Prim2.x2;
                   Y22        := Prim2.y2;
                end
                else if Prim2.ObjectID = eArcObject then
                begin
                   X21        := Prim2.StartX;
                   Y21        := Prim2.StartY1;
                   X22        := Prim2.EndX;
                   Y22        := Prim2.EndY;
                end;

                if ((X11 = X21) and (Y11 = Y21)) or ((X11 = X22) and (Y11 = Y22)) then PlaceVia(Layer1, X11, Y11, FinalLayer, Prim2.Net);
             end;

             Prim2 := BoardIterator.NextPCBObject;
          End;
          Board.SpatialIterator_Destroy(BoardIterator);
       end;

       // Another point

       BoardIterator := Board.SpatialIterator_Create;
       BoardIterator.AddFilter_ObjectSet(MkSet(eViaObject, ePadObject));
       BoardIterator.AddFilter_LayerSet(AllLayers);
       BoardIterator.AddFilter_Area(X12 - 1, Y12 - 1, X12 + 1, Y12 + 1);

       Prim2 := BoardIterator.FirstPCBObject;
       While (Prim2 <> Nil) Do
       Begin
          if Prim2.InNet then
             if (Prim2.net.Name = NetName1) then
                if ((X12 = Prim2.x) and (Y12 = Prim2.y)) then
                   if Prim2.ObjectID = eViaobject then
                   begin
                      ModifyVia(Prim2);
                      Flag2 := 1;
                   end
                   else
                   begin
                      // if there is pad on multi layer - do nothing
                      if Layer2String(Prim2.Layer) = 'Multi Layer' then
                         Flag2 := 1;
                   end;
          Prim2 := BoardIterator.NextPCBObject;
       End;
       Board.SpatialIterator_Destroy(BoardIterator);

       // Now we need to see weather we have unselected tracks/Arcs on
       // source layer, and at which place. this happens only if there was no via

       if Flag2 = 0 then
       begin
          BoardIterator := Board.SpatialIterator_Create;
          BoardIterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
          BoardIterator.AddFilter_LayerSet(MkSet(Layer1));
          BoardIterator.AddFilter_Area(X12 - 1, Y12 - 1, X12 + 1, Y12 + 1);

          Prim2 := BoardIterator.FirstPCBObject;
          While (Prim2 <> Nil) Do
          Begin
             if ((not Prim2.Selected) and (Prim2.net.Name = NetName1)) then
             begin
                If Prim2.ObjectID = eTrackObject then
                begin
                   X21        := Prim2.x1;
                   Y21        := Prim2.y1;
                   X22        := Prim2.x2;
                   Y22        := Prim2.y2;
                end
                else if Prim2.ObjectID = eArcObject then
                begin
                   X21        := Prim2.StartX;
                   Y21        := Prim2.StartY1;
                   X22        := Prim2.EndX;
                   Y22        := Prim2.EndY;
                end;

                if ((X12 = X21) and (Y12 = Y21)) or ((X12 = X22) and (Y12 = Y22)) then PlaceVia(Layer1, X12, Y12, FinalLayer, Prim2.Net);

             end;

             Prim2 := BoardIterator.NextPCBObject;
          End;
          Board.SpatialIterator_Destroy(BoardIterator);
       end;



       Rectangle := Prim1.BoundingRectangle;

       // We will remove all violations that are connected to the Prim1, using a simple trick :-)

       Board.RemovePCBObject(Prim1);
       Board.AddPCBObject(Prim1);

       // Over here we will test this track and all other primitives on current layer for clearence violation

       BoardIterator := Board.SpatialIterator_Create;
       BoardIterator.AddFilter_IPCB_LayerSet(String2Layer(FinalLayer));
       BoardIterator.AddFilter_ObjectSet(AllObjects);
       BoardIterator.AddFilter_Area(Rectangle.Left - MaxWidth, Rectangle.Bottom - MaxWidth, Rectangle.Right + MaxWidth, Rectangle.Top + MaxWidth);

       Prim2 := BoardIterator.FirstPCBObject;

       while Prim2 <> Nil do
       begin
          Rule := Board.FindDominantRuleForObjectPair(Prim1, Prim2, eRule_Clearance);

          if Rule <> nil then
          begin
             Board.AddPCBObject(Rule.ActualCheck(Prim1, Prim2));
             Prim2.GraphicallyInvalidate;
          end;

          Prim2 := BoardIterator.NextPCBObject;
       end;
       Board.SpatialIterator_Destroy(BoardIterator);
       Prim1.GraphicallyInvalidate;
    end;
    Board.ViewManager_FullUpdate;
    close;
 End;


{..............................................................................}
{                                                                              }
{   Click on "Cancel" button                                                   }
{                                                                              }
{..............................................................................}
procedure TMoveToLayer.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;


{..............................................................................}
{                                                                              }
{   Procedure that runs on create                                              }
{                                                                              }
{..............................................................................}
procedure TMoveToLayer.MoveToLayerShow(Sender: TObject);
begin
    LayerObj := TheLayerStack.FirstLayer;
    LastLayerNum := 0;
    Repeat
        inc(LastLayerNum);
        If (LayerObj.LayerID = eTopLayer)   or (LayerObj.LayerID = eMidLayer1)  or (LayerObj.LayerID = eMidLayer2)  or
           (LayerObj.LayerID = eMidLayer3)  or (LayerObj.LayerID = eMidLayer4)  or (LayerObj.LayerID = eMidLayer5)  or (LayerObj.LayerID = eMidLayer6)  or (LayerObj.LayerID = eMidLayer7)  or
           (LayerObj.LayerID = eMidLayer8)  or (LayerObj.LayerID = eMidLayer9)  or (LayerObj.LayerID = eMidLayer10) or (LayerObj.LayerID = eMidLayer11) or (LayerObj.LayerID = eMidLayer12) or
           (LayerObj.LayerID = eMidLayer13) or (LayerObj.LayerID = eMidLayer14) or (LayerObj.LayerID = eMidLayer15) or (LayerObj.LayerID = eMidLayer16) or (LayerObj.LayerID = eMidLayer17) or
           (LayerObj.LayerID = eMidLayer18) or (LayerObj.LayerID = eMidLayer19) or (LayerObj.LayerID = eMidLayer20) or (LayerObj.LayerID = eMidLayer21) or (LayerObj.LayerID = eMidLayer22) or
           (LayerObj.LayerID = eMidLayer23) or (LayerObj.LayerID = eMidLayer24) or (LayerObj.LayerID = eMidLayer25) or (LayerObj.LayerID = eMidLayer26) or (LayerObj.LayerID = eMidLayer27) or
           (LayerObj.LayerID = eMidLayer28) or (LayerObj.LayerID = eMidLayer29) or (LayerObj.LayerID = eMidLayer30) or (LayerObj.LayerID = eBottomLayer) Then
           begin
              ComboBoxLayer.Items.Add(LayerObj.Name);
           end;


        LayerObj := TheLayerStack.NextLayer(LayerObj);
    Until LayerObj = Nil;

    ComboBoxLayer.Text := ComboBoxLayer.Items.Get(0);
end;


Procedure Start;
begin  // Get the board
   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then Exit;

   // Now we need to get the signal layers and populate comboBox
   TheLayerStack := Board.LayerStack;
   If TheLayerStack = Nil Then Exit;

   if Board.SelectecObjectCount = 0 then exit;

   MoveToLayer.ShowModal;
end;    
