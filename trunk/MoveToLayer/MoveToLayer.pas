{..............................................................................}
{ Summary   This script moves selected copper tracks to another signal layer,  }
{           while maintaining connectivity.                                    }
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


{..............................................................................}
{                                                                              }
{   function that places new via                                               }
{                                                                              }
{..............................................................................}
Procedure PlaceVia(Layer : TLayer; X : Integer; Y : Integer; Layer2 : String; Net : IPCB_Net);
var
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
end;


{..............................................................................}
{                                                                              }
{   function that modifies via                                                 }
{                                                                              }
{..............................................................................}
Procedure ModifyVia(Via : IPCB_Via);
var
    BoardIterator : IPCB_BoardIterator;
    Primitive     : IPCB_Primitive;

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

    BoardIterator : IPCB_SpatialIterator;
    FinalLayer    : String;
    Primitive1    : IPCB_Primitive;
    Primitive2    : IPCB_Primitive;
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


    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
       Primitive1 := Board.SelectecObject[i];

       NetName1   := Primitive1.Net.Name;
       Layer1     := Primitive1.Layer;
       If Primitive1.ObjectID = eTrackObject then
       begin
          X11        := Primitive1.x1;
          Y11        := Primitive1.y1;
          X12        := Primitive1.x2;
          Y12        := Primitive1.y2;
       end
       else if Primitive1.ObjectID = eArcObject then
       begin
          X11        := Primitive1.StartX;
          Y11        := Primitive1.StartY1;
          X12        := Primitive1.EndX;
          Y12        := Primitive1.EndY;
       end;

       Flag1 := 0;
       Flag2 := 0;

       Primitive1.BeginModify;
       Primitive1.Layer := String2Layer(FinalLayer);
       Primitive1.EndModify;
       Primitive1.GraphicallyInvalidate;

       // first thing we need to look is does via already exist on edge

       BoardIterator := Board.SpatialIterator_Create;
       BoardIterator.AddFilter_ObjectSet(MkSet(eViaObject, ePadObject));
       BoardIterator.AddFilter_LayerSet(AllLayers);
       BoardIterator.AddFilter_Area(X11 - 1, Y11 - 1, X11 + 1, Y11 + 1);

       Primitive2 := BoardIterator.FirstPCBObject;
       While (Primitive2 <> Nil) Do
       Begin
          if Primitive2.InNet then
             if (Primitive2.net.Name = NetName1) then
                if ((X11 = Primitive2.x) and (Y11 = Primitive2.y)) then
                   if Primitive2.ObjectID = eViaobject then
                   begin
                      ModifyVia(Primitive2);
                      Flag1 := 1;
                   end
                   else
                   begin
                      // if there is pad on multi layer - do nothing
                      if Layer2String(Primitive2.Layer) = 'Multi Layer' then
                         Flag2 := 1;
                   end;
          Primitive2 := BoardIterator.NextPCBObject;
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

          Primitive2 := BoardIterator.FirstPCBObject;
          While (Primitive2 <> Nil) Do
          Begin
             if ((not Primitive2.Selected) and (Primitive2.net.Name = NetName1)) then
             begin
                If Primitive2.ObjectID = eTrackObject then
                begin
                   X21        := Primitive2.x1;
                   Y21        := Primitive2.y1;
                   X22        := Primitive2.x2;
                   Y22        := Primitive2.y2;
                end
                else if Primitive2.ObjectID = eArcObject then
                begin
                   X21        := Primitive2.StartX;
                   Y21        := Primitive2.StartY1;
                   X22        := Primitive2.EndX;
                   Y22        := Primitive2.EndY;
                end;

                if ((X11 = X21) and (Y11 = Y21)) or ((X11 = X22) and (Y11 = Y22)) then PlaceVia(Layer1, X11, Y11, FinalLayer, Primitive2.Net);
             end;

             Primitive2 := BoardIterator.NextPCBObject;
          End;
          Board.SpatialIterator_Destroy(BoardIterator);
       end;

       // Another point

       BoardIterator := Board.SpatialIterator_Create;
       BoardIterator.AddFilter_ObjectSet(MkSet(eViaObject, ePadObject));
       BoardIterator.AddFilter_LayerSet(AllLayers);
       BoardIterator.AddFilter_Area(X12 - 1, Y12 - 1, X12 + 1, Y12 + 1);

       Primitive2 := BoardIterator.FirstPCBObject;
       While (Primitive2 <> Nil) Do
       Begin
          if Primitive2.InNet then
             if (Primitive2.net.Name = NetName1) then
                if ((X12 = Primitive2.x) and (Y12 = Primitive2.y)) then
                   if Primitive2.ObjectID = eViaobject then
                   begin
                      ModifyVia(Primitive2);
                      Flag2 := 1;
                   end
                   else
                   begin
                      // if there is pad on multi layer - do nothing
                      if Layer2String(Primitive2.Layer) = 'Multi Layer' then
                         Flag2 := 1;
                   end;
          Primitive2 := BoardIterator.NextPCBObject;
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

          Primitive2 := BoardIterator.FirstPCBObject;
          While (Primitive2 <> Nil) Do
          Begin
             if ((not Primitive2.Selected) and (Primitive2.net.Name = NetName1)) then
             begin
                If Primitive2.ObjectID = eTrackObject then
                begin
                   X21        := Primitive2.x1;
                   Y21        := Primitive2.y1;
                   X22        := Primitive2.x2;
                   Y22        := Primitive2.y2;
                end
                else if Primitive2.ObjectID = eArcObject then
                begin
                   X21        := Primitive2.StartX;
                   Y21        := Primitive2.StartY1;
                   X22        := Primitive2.EndX;
                   Y22        := Primitive2.EndY;
                end;

                if ((X12 = X21) and (Y12 = Y21)) or ((X12 = X22) and (Y12 = Y22)) then PlaceVia(Layer1, X12, Y12, FinalLayer, Primitive2.Net);

             end;

             Primitive2 := BoardIterator.NextPCBObject;
          End;
          Board.SpatialIterator_Destroy(BoardIterator);
       end;
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
procedure TMoveToLayer.MoveToLayerCreate(Sender: TObject);
begin
   // Get the board
   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then Exit;

   // Now we need to get the signal layers and populate comboBox
   TheLayerStack := Board.LayerStack;
    If TheLayerStack = Nil Then Exit;

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

