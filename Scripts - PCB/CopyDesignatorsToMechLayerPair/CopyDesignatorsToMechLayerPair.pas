{.................................................................................
 Summary   This script copies (duplicates) component designators to mech
           layer or mech layer pair.
                  
           Designators will display the same text as the parent component, but will
           have '.Designator' special text, they are combined as part of component footprint.
                                                                              
                                                                              
 Author/Created by:    Petar Perisin  
 circa 2014 v3.1  Modified by Randy Clemmons for AD14 and Higher
                  Changed Board.LayerStack. to Board.LayerStack_V7.
 09/04/2021 v4.0  BLM  Added support for AD19+ mech layers & some refactoring.
.................................................................................}

const
    AD19VersionMajor  = 19;
    AD17MaxMechLayers = 32;       // scripting API has broken consts from TV6_Layer
    AD19MaxMechLayers = 1024;

var
    VerMajor        : WideString;
    LegacyMLS       : boolean;
    Board           : IPCB_Board;
    LayerStack      : IPCB_LayerStack_V7;
    LayerObj        : IPCB_LayerObject_V7;
    MechLayer1      : IPCB_MechanicalLayer;
    MechLayer2      : IPCB_MechanicalLayer;
    MechLayerPairs  : IPCB_MechanicalLayerPairs;
    MechPair        : TMechanicalPair;       // IPCB_MechanicalLayerPairs.LayerPair(MechPairIdx)
    MaxMechLayers   : integer;
    ML1, ML2        : integer;
    slMechPairs     : TStringList;
    slMechSingles   : TStringList;

function GetFirstLayerName(Pair : String) : String; forward;
function GetSecondLayerName(Pair : String) : String; forward;
function Version(const dummy : boolean) : TStringList; forward;

procedure TFormMechLayerDesignators.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFormMechLayerDesignators.FormMechLayerDesignatorsShow(Sender: TObject);
var
   i, j      : Integer;

begin
    LayerStack := Board.LayerStack_V7;
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
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer1 := LayerStack.LayerObject_V7(ML1);

        if MechLayer1.MechanicalLayerEnabled then
        begin
            slMechSingles.Add(Board.LayerName(ML1));

            if (RadioButtonPair.Checked) then
            begin
                for j := (i + 1) to MaxMechLayers do
                begin
                    ML2 := LayerUtils.MechanicalLayer(j);
                    MechLayer2 := LayerStack.LayerObject_V7(ML2);
                    if MechLayer2.MechanicalLayerEnabled then
                    if MechLayerPairs.PairDefined(ML1, ML2) then
                    begin
                        slMechPairs.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        ComboBoxLayers.Items.Add(Board.LayerName(ML1) + ' <----> ' + Board.LayerName(ML2));
                        if ComboBoxLayers.Items.Count = 1 then
                        begin
                            ComboBoxLayers.Text := ComboBoxLayers.Items[0];
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
                    ComboBoxLayers.Text := ComboBoxLayers.Items[0];
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
      RadioButtonLayer1.Enabled := False;
      RadioButtonLayer2.Enabled := False;

      RadioButtonLayer1.Caption := 'Single Layer';
      RadioButtonLayer2.Caption := 'Single Layer';

      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      ComboBoxLayers.Clear;

      for i := 0 to (slMechSingles.Count - 1) do
      begin
         ComboBoxLayers.Items.Add(slMechSingles[i]);
      end;

      ComboBoxLayers.Text := ComboBoxLayers.Items[0];
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
         ComboBoxLayers.Items.Add(slMechPairs[i]);
      end;

      ComboBoxLayers.Text := ComboBoxLayers.Items[0];
      RadioButtonLayer1.Caption := GetFirstLayerName(ComboBoxLayers.Text);
      RadioButtonLayer2.Caption := GetSecondLayerName(ComboBoxLayers.Text);

   end;
end;

procedure TFormMechLayerDesignators.ComboBoxLayersChange(Sender: TObject);
begin
   if GroupBoxLayer.Caption = 'Choose Mech Layer Pair:' then
   begin
      RadioButtonLayer1.Caption := GetFirstLayerName(ComboBoxLayers.Text);
      RadioButtonLayer2.Caption := GetSecondLayerName(ComboBoxLayers.Text);
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
    LayerStack := Board.LayerStack_V7;

    for i := 1 to MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer1 := LayerStack.LayerObject_V7[ML1];

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
    ASetOfLayers := LayerSetUtils.SignalLayers;
    CompIterator := Board.BoardIterator_Create;
    CompIterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    CompIterator.AddFilter_IPCB_LayerSet(ASetOfLayers);
    CompIterator.AddFilter_Method(eProcessAll);

    Component := CompIterator.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
//   No selected components - make it for all components
        if (not RadioButtonSelected.Checked) or (RadioButtonSelected.Checked and Component.Selected) then
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

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        LegacyMLS     := false;
        MaxMechLayers := AD19MaxMechLayers;
    end;

    slMechPairs   := TStringList.Create;
    slMechSingles := TStringList.Create;

    FormMechLayerDesignators.ShowModal;
end;

{.......................................................................................}
function GetFirstLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   SetLength(Pair, Pos - 1);

   Result := Pair;
end;

function GetSecondLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   Delete(Pair, 1, Pos  + 7);

   Result := Pair;
end;

function Version(const dummy : boolean) : TStringList;
begin
    Result               := TStringList.Create;
    Result.Delimiter     := '.';
    Result.Duplicates    := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;
