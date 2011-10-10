{..............................................................................}
{ Summary   This script creates Stitching Vias on a PCB Document.              }
{           Vias are added to dummy component, for easier manipulation.        }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board : IPCB_Board;



function IsStringANum(Tekst : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Tekst) do
      if not(((ord(Tekst[i]) > 47) and (ord(Tekst[i]) < 58)) or (ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Tekst) do
      if ((ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
      begin
         Inc(dotCount);
         if (i = 1) or (i = Length(Tekst)) then Result := False;
      end;

   if dotCount > 1 then Result := False;
end;



procedure TStitchingVias.EditOutlineChange(Sender: TObject);
begin
   if not IsStringANum(EditOutline.Text) then
   begin
      ButtonOK.Enabled := False;
      EditOutline.Font.Color := clRed;
   end
   else
   begin
      EditOutline.Font.Color := clWindowText;
      if (IsStringANum(EditBetween.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditSize.Text) and IsStringANum(EditHoleSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TStitchingVias.EditBetweenChange(Sender: TObject);
begin
   if not IsStringANum(EditBetween.Text) then
   begin
      ButtonOK.Enabled := False;
      EditBetween.Font.Color := clRed;
   end
   else
   begin
      EditBetween.Font.Color := clWindowText;
      if (IsStringANum(EditBetween.Text) and IsStringANum(EditOutline.Text) and IsStringANum(EditSize.Text) and IsStringANum(EditHoleSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TStitchingVias.EditElectricalChange(Sender: TObject);
begin
   if not IsStringANum(EditElectrical.Text) then
   begin
      ButtonOK.Enabled := False;
      EditElectrical.Font.Color := clRed;
   end
   else
   begin
      EditElectrical.Font.Color := clWindowText;
      if (IsStringANum(EditOutline.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditSize.Text) and IsStringANum(EditHoleSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TStitchingVias.EditSizeChange(Sender: TObject);
begin
   if not IsStringANum(EditSize.Text) then
   begin
      ButtonOK.Enabled := False;
      EditSize.Font.Color := clRed;
   end
   else
   begin
      EditSize.Font.Color := clWindowText;
      if (IsStringANum(EditOutline.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditBetween.Text) and IsStringANum(EditHoleSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TStitchingVias.EditHoleSizeChange(Sender: TObject);
begin
   if not IsStringANum(EditHoleSize.Text) then
   begin
      ButtonOK.Enabled := False;
      EditSize.Font.Color := clRed;
   end
   else
   begin
      EditSize.Font.Color := clWindowText;
      if (IsStringANum(EditOutline.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditBetween.Text) and IsStringANum(EditHoleSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TStitchingVias.ButtonCancelClick(Sender: TObject);
begin
   close;
end;



procedure TStitchingVias.ButtonOKClick(Sender: TObject);
var

   BoardShapeRect : TCoordRect;
   NewVia         : IPCB_Via;
   PosX, PosY     : Integer;
   TheLayerStack  : IPCB_LayerStack;
   LayerObj       : IPCB_LayerObject;
   LayerNum       : integer;
   Comp           : IPCB_Component;
   Distance       : Float;
   Net            : IPCB_Net;
   i              : Integer;

   Iterator       : IPCB_BoardIterator;
   Rule           : IPCB_Rule;
   RuleOutline    : IPCB_ClearanceConstraint;
   RuleElectrical : IPCB_ClearanceConstraint;
   RuleBetween    : IPCB_ClearanceConstraint;

   RuleViaStyle   : IPCB_RoutingViaStyleRule;
   RulePlaneConn  : IPCB_PowerPlaneConnectStyleRule;
   RulePolyConn   : IPCB_PolygonConnectStyleRule;
   RuleCompClear  : IPCB_ComponentClearanceConstraint;

   MaxGap         : Integer;
   PadRect        : TCoordRect;
   Spatial        : IPCB_SpatialIterator;
   Primitive      : IPCB_Primitive;
   Violation      : IPCB_Violation;
   ViolationFlag  : Integer;
   SetOfLayers    : IPCB_LayerSet;

begin

   // Distance
   Distance := StrToFloat(EditSize.Text) + StrToFloat(EditBetween.Text);

   if RadioButtonMM.Checked then Distance := MMsToCoord(Distance)
   else                          Distance := MilsToCoord(Distance);


   // We will get net from ComboBoxNets
   For i := 0 To ComboBoxNets.Items.Count - 1 Do
     If ComboBoxNets.Items[i] = ComboBoxNets.Text Then
         Net := ComboBoxNets.Items.Objects[i];

   // First I neeed to get Board shape bounding rectangel - we start from this
   BoardShapeRect := Board.BoardOutline.BoundingRectangle;

   // now we will create new component because we will add all this objects to
   // dummy component
   PCBServer.PreProcess;
   Comp := PCBServer.PCBObjectFactory(eComponentObject, eNoDimension, eCreate_Default);
   If Comp = Nil Then Exit;

   // Set the reference point of the Component
   if (Board.XOrigin > BoardShapeRect.Left) and (Board.YOrigin > BoardShapeRect.Bottom) then
   begin
      Comp.X := Board.XOrigin;
      Comp.Y := Board.YOrigin;
   end
   else
   begin
      Comp.X := BoardShapeRect.Left;
      Comp.Y := BoardShapeRect.Bottom;
   end;

   Comp.Layer := eTopLayer;
   Comp.PrimitiveLock := False;
   Comp.Moveable := False;

   // Make the designator text visible;
   Comp.NameOn       := False;
   Comp.Name.Text    := 'StitchingVias';

   // Make the comment text visible;
   Comp.CommentOn    := False;
   Comp.Comment.Text := 'Component That Holds Stitching Vias';

   Comp.ComponentKind := eComponentKind_Graphical;

   PCBServer.SendMessageToRobots(Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,Comp.I_ObjectAddress);
   Board.AddPCBObject(Comp);
   PCBServer.PostProcess;

   // Now we need to set up rules for this objects.
   // We need to check if they exist

   Iterator        := Board.BoardIterator_Create;
   Iterator.AddFilter_ObjectSet(MkSet(eRuleObject));
   Iterator.AddFilter_LayerSet(AllLayers);
   Iterator.AddFilter_Method(eProcessAll);

   // Search for Rules and mark if they are found
   // If not, create them
   Rule := Iterator.FirstPCBObject;

   RuleOutline    := Nil;
   RuleElectrical := Nil;
   RuleBetween    := Nil;
   RuleViaStyle   := Nil;
   RulePlaneConn  := Nil;
   RulePolyConn   := Nil;
   RuleCompClear  := Nil;

   While (Rule <> Nil) Do
   Begin
       if Rule.RuleKind = eRule_Clearance then
       begin

          if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') and (Rule.Scope2Expression = 'OnLayer(''Keep-Out Layer'')') then
             RuleOutline := Rule
          else if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') and (Rule.Scope2Expression = 'OnSignal') then
             RuleElectrical := Rule
          else if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') and (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') then
             RuleBetween := Rule;
       end;

       if Rule.RuleKind = eRule_RoutingViaStyle then
          if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') then
             RuleViaStyle := Rule;

       if Rule.RuleKind = eRule_PowerPlaneConnectStyle then
          if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') then
             RulePlaneConn := Rule;

       if Rule.RuleKind = eRule_PolygonConnectStyle then
          if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') then
             RulePolyConn := Rule;

       if Rule.RuleKind = eRule_ComponentClearance then
          if (Rule.Scope1Expression = 'InComponent(''StitchingVias'')') and (Rule.Scope2Expression = 'All') then
             RuleCompClear := Rule;

       Rule :=  Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

   // Check rule from via to outline
   if RuleOutline = Nil then
   begin
      RuleOutline := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleOutline.Scope1Expression := 'InComponent(''StitchingVias'')';
      RuleOutline.Scope2Expression := 'OnLayer(''Keep-Out Layer'')';

      RuleOutline.NetScope  := eNetScope_AnyNet;

      if RadioButtonMM.Checked then RuleOutline.Gap := MMsToCoord(StrToFloat(EditOutline.Text))
      else                          RuleOutline.Gap := MilsToCoord(StrToFloat(EditOutline.Text));

      RuleOutline.Name    := 'StitchingVias-Board';
      RuleOutline.Comment := 'Clearance between Stitching Vias and Board Outline';

      // Add the rule into the Board
      Board.AddPCBObject(RuleOutline);

   end
   else
   begin
      if RadioButtonMM.Checked then RuleOutline.Gap := MMsToCoord(StrToFloat(EditOutline.Text))
      else                          RuleOutline.Gap := MilsToCoord(StrToFloat(EditOutline.Text));
   end;

   // Check rule from via to electrical primitive
   if RuleElectrical = Nil then
   begin
      RuleElectrical := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleElectrical.Scope1Expression := 'InComponent(''StitchingVias'')';
      RuleElectrical.Scope2Expression := 'OnSignal';

      RuleElectrical.NetScope  := eNetScope_AnyNet;

      if RadioButtonMM.Checked then RuleElectrical.Gap := MMsToCoord(StrToFloat(EditElectrical.Text))
      else                          RuleElectrical.Gap := MilsToCoord(StrToFloat(EditElectrical.Text));

      RuleElectrical.Name    := 'StitchingVias-Electrical';
      RuleElectrical.Comment := 'Clearance between Stitching Vias and Electrical Objects';

      // Add the rule into the Board
      Board.AddPCBObject(RuleElectrical);

   end
   else
   begin
      if RadioButtonMM.Checked then RuleElectrical.Gap := MMsToCoord(StrToFloat(EditElectrical.Text))
      else                          RuleElectrical.Gap := MilsToCoord(StrToFloat(EditElectrical.Text));
   end;

   // Check Rule Between vias
   if RuleBetween = Nil then
   begin
      RuleBetween := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleBetween.Scope1Expression := 'InComponent(''StitchingVias'')';
      RuleBetween.Scope2Expression := 'InComponent(''StitchingVias'')';

      RuleBetween.NetScope  := eNetScope_AnyNet;

      if RadioButtonMM.Checked then RuleBetween.Gap := MMsToCoord(StrToFloat(EditBetween.Text))
      else                          RuleBetween.Gap := MilsToCoord(StrToFloat(EditBetween.Text));

      RuleBetween.Name    := 'StitchingVias-Internal';
      RuleBetween.Comment := 'Clearance between Stitching Vias';

      // Add the rule into the Board
      Board.AddPCBObject(RuleBetween);

   end
   else
   begin
      if RadioButtonMM.Checked then RuleBetween.Gap := MMsToCoord(StrToFloat(EditBetween.Text))
      else                          RuleBetween.Gap := MilsToCoord(StrToFloat(EditBetween.Text));
   end;

   // Check Via style rule
   if CheckBoxViaStyleRule.Checked then
      if RuleViaStyle = Nil then
      begin
         RuleViaStyle := PCBServer.PCBRuleFactory(eRule_RoutingViaStyle);

         // Set values
         RuleViaStyle.Scope1Expression := 'InComponent(''StitchingVias'')';

         RuleViaStyle.NetScope  := eNetScope_AnyNet;

         if RadioButtonMM.Checked then
         begin
            RuleViaStyle.MinHoleWidth      := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MaxHoleWidth      := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.PreferedHoleWidth := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MinWidth          := MMsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.MaxWidth          := MMsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.PreferedWidth     := MMsToCoord(StrToFloat(EditSize.Text));
         end
         else
         begin
            RuleViaStyle.MinHoleWidth      := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MaxHoleWidth      := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.PreferedHoleWidth := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MinWidth          := MilsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.MaxWidth          := MilsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.PreferedWidth     := MilsToCoord(StrToFloat(EditSize.Text));
         end;

         RuleViaStyle.Name    := 'StitchingVias-ViaStyle';
         RuleViaStyle.Comment := 'Via style rule for Stitching Vias';

         // Add the rule into the Board
         Board.AddPCBObject(RuleViaStyle);
      end
      else
      begin
         if RadioButtonMM.Checked then
         begin
            RuleViaStyle.MinHoleWidth      := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MaxHoleWidth      := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.PreferedHoleWidth := MMsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MinWidth          := MMsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.MaxWidth          := MMsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.PreferedWidth     := MMsToCoord(StrToFloat(EditSize.Text));
         end
         else
         begin
            RuleViaStyle.MinHoleWidth      := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MaxHoleWidth      := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.PreferedHoleWidth := MilsToCoord(StrToFloat(EditHoleSize.Text));
            RuleViaStyle.MinWidth          := MilsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.MaxWidth          := MilsToCoord(StrToFloat(EditSize.Text));
            RuleViaStyle.PreferedWidth     := MilsToCoord(StrToFloat(EditSize.Text));
         end;
      end;

   // Check plane connect style rule
   if CheckBoxPlaneConnRule.Checked then
      if RulePlaneConn = Nil then
      begin
         RulePlaneConn := PCBServer.PCBRuleFactory(eRule_PowerPlaneConnectStyle);

         // Set values
         RulePlaneConn.Scope1Expression := 'InComponent(''StitchingVias'')';

         RulePlaneConn.NetScope  := eNetScope_AnyNet;

         RulePlaneConn.PlaneConnectStyle := eDirectConnectToPlane;

         RulePlaneConn.Name    := 'StitchingVias-Plane';
         RulePlaneConn.Comment := 'Power plane connect style rule for Stitching Vias';

         // Add the rule into the Board
         Board.AddPCBObject(RulePlaneConn);

      end
      else
      begin
         RulePlaneConn.PlaneConnectStyle := eDirectConnectToPlane;
      end;

   // Check polygon connect style rule
   if CheckBoxPolyConnRule.Checked then
      if RulePolyConn = Nil then
      begin
         RulePolyConn := PCBServer.PCBRuleFactory(eRule_PolygonConnectStyle);

         // Set values
         RulePolyConn.Scope1Expression := 'InComponent(''StitchingVias'')';

         RulePolyConn.NetScope  := eNetScope_AnyNet;

         RulePolyConn.ConnectStyle := eDirectConnectToPlane;

         RulePolyConn.Name    := 'StitchingVias-Poly';
         RulePolyConn.Comment := 'Polygon connect style rule for Stitching Vias';

         // Add the rule into the Board
         Board.AddPCBObject(RulePolyConn);

      end
      else
      begin
         RulePolyConn.ConnectStyle := eDirectConnectToPlane;
      end;

   // Check component clearence rule
   if CheckBoxCompClearRule.Checked then
      if RuleCompClear = Nil then
      begin
         RuleCompClear := PCBServer.PCBRuleFactory(eRule_ComponentClearance);

         // Set values
         RuleCompClear.Scope1Expression := 'InComponent(''StitchingVias'')';
         RuleCompClear.Scope2Expression := 'All';

         RuleCompClear.NetScope := eNetScope_AnyNet;

         RuleCompClear.Gap         := 0;
         RuleCompClear.VerticalGap := 0;

         RuleCompClear.Name     := 'StitchingVias-Comp';
         RuleCompClear.Comment  := 'Clearance between "StitchingVias" Component and other Components';

         // Add the rule into the Board
         Board.AddPCBObject(RuleCompClear);

      end
      else
      begin
         RuleCompClear.Gap         := 0;
         RuleCompClear.VerticalGap := 0;
      end;


   // We will save MaxGap value for further testing
   if RuleElectrical.Gap > RuleOutline.Gap then MaxGap := RuleElectrical.Gap
   else                                         MaxGap := RuleOutline.Gap;

   PosX := BoardShapeRect.Left;
   PosY := BoardShapeRect.Top;

   SetOfLayers := LayerSet.CreateLayerSet;
   SetOfLayers.IncludeSignalLayers;
   SetOfLayers.Include(String2Layer('Multi Layer'));
   SetOfLayers.Include(String2Layer('Keep Out Layer'));

   while ((PosY > BoardShapeRect.Bottom)) do
   begin

      if Board.BoardOutline.PointInPolygon(PosX, PosY) then
      Begin
         Try
            PCBServer.PreProcess;
            NewVia := PcbServer.PCBObjectFactory(eViaObject,eNoDimension,eCreate_Default);
            If NewVia = Nil Then Exit;

            NewVia.BeginModify;
            NewVia.Mode := ePadMode_Simple;
            NewVia.X    := PosX;
            NewVia.Y    := PosY;

            if RadioButtonMM.Checked then NewVia.Size := MMsToCoord(StrToFloat(EditSize.Text))
            else                          NewVia.Size := MilsToCoord(StrToFloat(EditSize.Text));

            if RadioButtonMM.Checked then NewVia.HoleSize := MMsToCoord(StrToFloat(EditHoleSize.Text))
            else                          NewVia.HoleSize := MilsToCoord(StrToFloat(EditHoleSize.Text));

            NewVia.LowLayer  := eTopLayer;
            NewVia.HighLayer := eBottomLayer;

            NewVia.IsTenting := True;

            NewVia.Net := Net;

         Finally
            PCBServer.PostProcess;
         End;

         NewVia.EndModify;
         Comp.AddPCBObject(NewVia);
         PCBServer.SendMessageToRobots(Comp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewVia.I_ObjectAddress);
         Board.AddPCBObject(NewVia);

         // here we need to set up spatial iterator that will
         // Check if we have clearence issues on pads
         PadRect := NewVia.BoundingRectangle;

         Spatial := Board.SpatialIterator_Create;
         Spatial.AddFilter_ObjectSet(AllObjects);
         Spatial.AddFilter_IPCB_LayerSet(SetOfLayers);
         Spatial.AddFilter_Area(PadRect.Left - MaxGap, PadRect.Bottom - MaxGap, PadRect.Right + MaxGap, PadRect.Top + MaxGap);

         ViolationFlag := 0;
         Primitive := Spatial.FirstPCBObject;

         while (Primitive <> nil) do
         begin
            Violation := RuleElectrical.ActualCheck(Primitive, NewVia);
            if Violation <> nil then ViolationFlag := 1;

            Violation := RuleOutline.ActualCheck(Primitive, NewVia);
            if Violation <> nil then ViolationFlag := 1;

            Primitive := Spatial.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(Spatial);


         If ViolationFlag = 1 then
            Board.RemovePCBObject(NewVia);

      end;

      // finally we need to move one point right
      PosX := PosX + Distance;

      if PosX > BoardShapeRect.Right then
      begin
         PosX := BoardShapeRect.Left;

         PosY := PosY - Distance;
      end;
   end;

   if not CheckBoxBetween.Checked    then Board.RemovePCBObject(RuleBetween);
   if not CheckBoxElectrical.Checked then Board.RemovePCBObject(RuleElectrical);
   if not CheckBoxOutline.Checked    then Board.RemovePCBObject(RuleOutline);

   Comp.PrimitiveLock := False;
   Comp.Moveable := False;
   Board.ViewManager_GraphicallyInvalidatePrimitive(Comp);
                                                        
   ResetParameters;
   AddStringParameter('Action', 'Redraw');
   RunProcess('PCB:Zoom');

   close;
end;



procedure TStitchingVias.StitchingViasShow(Sender: TObject);
var
   Iterator : IPCB_BoardIterator;
   Net      : IPCB_Net;
begin
   // Create the iterator that will look for Net objects only
   Iterator        := Board.BoardIterator_Create;
   Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
   Iterator.AddFilter_LayerSet(AllLayers);
   Iterator.AddFilter_Method(eProcessAll);

   // Search for Net object that has the 'GND' name.
   Net := Iterator.FirstPCBObject;
   While (Net <> Nil) Do
   Begin
      If Net.Name = 'GND' Then
         ComboBoxNets.Text := Net.Name;
      ComboBoxNets.Items.AddObject(Net.Name, Net);
      Net := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   StitchingVias.ShowModal;
end;





