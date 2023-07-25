{.................................................................................
 Summary   This script copies (duplicates) component designators to mech
           layer or mech layer pair.
                  
           Designators will display the same text as the parent component, but will
           have '.Designator' special text, they are combined as part of component footprint.
                                                                              
                                                                              
 Author/Created by:    Petar Perisin  
 circa 2014 v3.1  Modified by Randy Clemmons for AD14 and Higher
                  Changed Board.LayerStack. to Board.LayerStack_V7.
 09/04/2021 v4.0  BLM  Added support for AD19+ mech layers & some refactoring.
 25/04/2021 v4.1  BLM  Minor tweaks to UI text & stringlist handling.
 2023-07-12 v4.2  BLM eliminate V7 stack interfaces for AD19+ 
 .................................................................................}

const
    AD19VersionMajor  = 19;
    AD17MaxMechLayers = 32;       // scripting API has broken consts from TV6_Layer
    AD19MaxMechLayers = 1024;

var
    VerMajor        : WideString;
    LegacyMLS       : boolean;
    Board           : IPCB_Board;
    LayerStack      : IPCB_MasterLayerStack;
    LayerObj        : IPCB_LayerObject;
    MechLayer1      : IPCB_MechanicalLayer;
    MechLayer2      : IPCB_MechanicalLayer;
    MechPair        : TMechanicalPair;       // IPCB_MechanicalLayerPairs.LayerPair(MechPairIdx)
    MaxMechLayers   : integer;
    ML1, ML2        : integer;
    slMechPairs     : TStringList;
    slMechSingles   : TStringList;

function Version(const dummy : boolean) : TStringList; forward;
function GetMechLayerObject(LS: IPCB_MasterLayerStack, i : integer, var MLID : TLayer) : IPCB_MechanicalLayer; forward;

procedure TFormMechLayerDesignators.ButtonCancelClick(Sender: TObject);
begin
    Close;
    slMechPairs.Clear;
    slMechSingles.Clear;
end;

procedure TFormMechLayerDesignators.FormMechLayerDesignatorsShow(Sender: TObject);
var
    MechLayerPairs  : IPCB_MechanicalLayerPairs;
    i, j            : Integer;

begin
    LayerStack := Board.MasterLayerStack;
    MechLayerPairs  := Board.MechanicalPairs;

// are any layer pairs defined ?..
    if MechLayerPairs.Count = 0 then
    begin
        RadioButtonSingle.Checked := True;
        RadioButtonPair.Enabled := False;
        GroupBoxLayer.Caption := 'Choose Mech Layer:';

        RadioButtonLayer1.Enabled := False;
        RadioButtonLayer2.Enabled := False;
    end;

    for i := 1 to MaxMechLayers do
    begin
        MechLayer1 := GetMechLayerObject(LayerStack, i, ML1);

        if MechLayer1.MechanicalLayerEnabled then
        begin
            slMechSingles.Add(Board.LayerName(ML1));

            if (RadioButtonPair.Checked) then
            begin
                for j := (i + 1) to MaxMechLayers do
                begin
                    MechLayer2 := GetMechLayerObject(LayerStack, j, ML2);

                    if MechLayer2.MechanicalLayerEnabled then
                    if MechLayerPairs.PairDefined(ML1, ML2) then
                    begin
                        slMechPairs.Add(Board.LayerName(ML1) + '=' + Board.LayerName(ML2));
                        ComboBoxLayers.Items.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        if ComboBoxLayers.Items.Count = 1 then
                        begin
                            ComboBoxLayers.SetItemIndex(0);
                            RadioButtonLayer1.caption := Board.LayerName(ML1);
                            RadioButtonLayer2.caption := Board.LayerName(ML2);
                        end;
                    end;
                end;  // j
            end else
            begin
// single layer radio button ticked/checked.
                ComboBoxLayers.Items.Add(Board.LayerName(ML1));
                if ComboBoxLayers.Items.Count = 1 then
                    ComboBoxLayers.SetItemIndex(0);
            end;
        end;
    end;
end;

procedure TFormMechLayerDesignators.RadioButtonSingleClick(Sender: TObject);
var
   i : Integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer:' then
   begin
      RadioButtonLayer1.Enabled := True;
      RadioButtonLayer2.Enabled := False;

      RadioButtonLayer2.Caption := 'Single Layer';
      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      ComboBoxLayers.Clear;

      for i := 0 to (slMechSingles.Count - 1) do
      begin
         ComboBoxLayers.Items.Add(slMechSingles[i]);
      end;
      if slMechSingles.Count > 0 then
      begin
         ComboBoxLayers.SetItemIndex(0);
         RadioButtonLayer1.Caption := slMechSingles.Strings(0);
      end;
   end;
end;

procedure TFormMechLayerDesignators.RadioButtonPairClick(Sender: TObject);
var
   i : integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer Pair:' then
   begin
      RadioButtonLayer1.Enabled := True;
      RadioButtonLayer2.Enabled := True;
      GroupBoxLayer.Caption := 'Choose Mech Layer Pair:';

      ComboBoxLayers.Clear;

      for i := 0 to (slMechPairs.Count - 1) do
      begin
         ComboBoxLayers.Items.Add(slMechPairs.Names(i) + ' <----> ' + slMechPairs.ValueFromIndex(i) );
      end;
      if slMechPairs.Count > 0 then
      begin
          ComboBoxLayers.SetItemIndex(0);
          RadioButtonLayer1.Caption := slMechPairs.Names(0);
          RadioButtonLayer2.Caption := slMechPairs.ValueFromIndex(0);
      end;
   end;
end;

procedure TFormMechLayerDesignators.ComboBoxLayersChange(Sender: TObject);
begin
   if RadioButtonPair.Checked then
   begin
      if slMechPairs.Count > 0 then
      begin
         RadioButtonLayer1.Caption := slMechPairs.Names(ComboBoxLayers.GetItemIndex);
         RadioButtonLayer2.Caption := slMechPairs.ValueFromIndex(ComboBoxLayers.GetItemIndex);
      end;
   end;
   if RadioButtonSingle.Checked then
   begin
      if slMechSingles.Count > 0 then
      begin
         RadioButtonLayer1.Caption := slMechSingles.Strings(ComboBoxLayers.GetItemIndex);
         RadioButtonLayer2.Caption := 'Single Layer';
      end;
   end;
end;

procedure TFormMechLayerDesignators.ButtonOKClick(Sender: TObject);
var
   MechTop         : TLayer;
   MechBot         : TLayer;
   i, flag         : Integer;
   Primitive       : IPCB_Primitive;
   NewPrim         : IPCB_Primitive;
   CompIterator    : IPCB_BoardIterator;
   OverlayIterator : IPCB_GroupIterator;
   Component       : IPCB_Component;
   ASetOfLayers    : IPCB_LayerSet;
   NewPrims        : TObjectList;
begin
   // This is the main one. This was hard to set up.
    for i := 1 to MaxMechLayers do
    begin
        MechLayer1 := GetMechLayerObject(LayerStack, i, ML1);

        if RadioButtonPair.Checked then
        begin
            if (MechLayer1.Name = RadioButtonLayer1.Caption) then
                if RadioButtonLayer1.Checked then MechTop := ML1
                else                              MechBot := ML1;
             if (MechLayer1.Name = RadioButtonLayer2.Caption) then
                 if RadioButtonLayer2.Checked then MechTop := ML1
                 else                              MechBot := ML1;
        end else
        begin
            if (MechLayer1.Name = ComboBoxLayers.Text) then
            begin
                MechTop := ML1;
                MechBot := ML1;
                break;
            end;
        end;
   end;

   // Cycle through all components, or only selected ones, and
   // copy designators etc to the mech layers defined.
    ASetOfLayers := LayerSetUtils.CreateLayerSet;
    ASetOfLayers.IncludeSignalLayers;
    CompIterator := Board.BoardIterator_Create;
    CompIterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    CompIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);
    CompIterator.AddFilter_Method(eProcessAll);

    Component := CompIterator.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
//   No selected components - make it for all components
        if (RadioButtonAll.Checked) or (RadioButtonSelected.Checked and Component.Selected) then
        begin

            NewPrim := Component.Name.Replicate;

            if Component.Layer = eTopLayer then NewPrim.Layer := MechTop
            else                                NewPrim.Layer := MechBot;

            NewPrim.Text := '.Designator';

            Board.AddPCBObject(NewPrim);
            Component.AddPCBObject(NewPrim);

            if CheckBoxOverlayPrims.Checked then
            begin
                // Copy comment first
                if Component.CommentOn then
                begin
                    NewPrim := Component.Comment.Replicate;

                    if Component.Layer = eTopLayer then NewPrim.Layer := MechTop
                    else                                NewPrim.Layer := MechBot;

                    NewPrim.Text := '.Comment';

                    Board.AddPCBObject(NewPrim);
                    Component.AddPCBObject(NewPrim);
                end;

//   copy all other overlay primitives to mechLayer -
                ASetOfLayers := LayerSetUtils.EmptySet;
                AsetOfLayers.Include(eTopOverlay);
                AsetOfLayers.Include(eBottomOverlay);
                OverlayIterator := Component.GroupIterator_Create;
                OverlayIterator.SetState_FilterAll;
                OverlayIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);   // DNW on Group ??

                NewPrims := TObjectList.Create;

                Primitive := OverlayIterator.FirstPCBObject;
                while (Primitive <> nil) do
                begin
                    if ASetOfLayers.Contains(Primitive.Layer) then
                    begin
                        NewPrim := Primitive.Replicate;

                        if Primitive.Layer = eTopOverlay then NewPrim.Layer := MechTop
                        else                                  NewPrim.Layer := MechBot;

                        NewPrims.Add(NewPrim);
                    end;
                    Primitive := OverlayIterator.NextPCBObject;
                end;
                Component.GroupIterator_Destroy(OverlayIterator);

// should NOT add or delete from inside an iterated collection.
                for i := 0 to (NewPrims.Count -1) do
                begin
                    NewPrim := NewPrims.Items(i);
                    Board.AddPCBObject(NewPrim);
                    Component.AddPCBObject(NewPrim);
                end;
                NewPrims.Destroy;

            end;  // OverlayPrims..
        end;      // if Selected..
        Component := CompIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(CompIterator);

    Close;
end;

Procedure Start;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowMessage('Focused Doc Not a .PcbDoc ');
        exit;
    end;

    LayerStack := Board.MasterLayerStack;
    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        LegacyMLS     := false;
        MaxMechLayers := AD19MaxMechLayers;
    end;

    slMechPairs   := TStringList.Create;
    slMechPairs.StrictDelimiter := true;
    slMechPairs.NameValueSeparator := '=';

    slMechSingles := TStringList.Create;

    FormMechLayerDesignators.ShowModal;
end;

{.......................................................................................}
                                                            // cardinal      V7 LayerID
function GetMechLayerObject(LS: IPCB_MasterLayerStack, i : integer, var MLID : TLayer) : IPCB_MechanicalLayer;
begin
    if LegacyMLS then
    begin
        MLID := LayerUtils.MechanicalLayer(i);
        Result := LS.LayerObject_V7(MLID)
    end else
    begin
        Result := LS.GetMechanicalLayer(i);
        MLID := Result.V7_LayerID.ID;       // .LayerID returns zero for dielectric
    end;
end;

function Version(const dummy : boolean) : TStringList;
begin
    Result               := TStringList.Create;
    Result.Delimiter     := '.';
    Result.Duplicates    := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;
