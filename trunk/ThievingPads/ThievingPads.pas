{..............................................................................}
{ Summary   This script creates Thieving Pads to a PCB Document.               }
{           Pads are added to dummy component, for easier manipulation.        }
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



procedure TThievingPads.EditOutlineChange(Sender: TObject);
begin
   if not IsStringANum(EditOutline.Text) then
   begin
      ButtonOK.Enabled := False;
      EditOutline.Font.Color := clRed;
   end
   else
   begin
      EditOutline.Font.Color := clWindowText;
      if (IsStringANum(EditBetween.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TThievingPads.EditBetweenChange(Sender: TObject);
begin
   if not IsStringANum(EditBetween.Text) then
   begin
      ButtonOK.Enabled := False;
      EditBetween.Font.Color := clRed;
   end
   else
   begin
      EditBetween.Font.Color := clWindowText;
      if (IsStringANum(EditElectrical.Text) and IsStringANum(EditOutline.Text) and IsStringANum(EditSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TThievingPads.EditElectricalChange(Sender: TObject);
begin
   if not IsStringANum(EditElectrical.Text) then
   begin
      ButtonOK.Enabled := False;
      EditElectrical.Font.Color := clRed;
   end
   else
   begin
      EditElectrical.Font.Color := clWindowText;
      if (IsStringANum(EditOutline.Text) and IsStringANum(EditBetween.Text) and IsStringANum(EditSize.Text)) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TThievingPads.EditSizeChange(Sender: TObject);
begin
   if not IsStringANum(EditSize.Text) then
   begin
      ButtonOK.Enabled := False;
      EditSize.Font.Color := clRed;
   end
   else
   begin
      EditSize.Font.Color := clWindowText;
      if (IsStringANum(EditOutline.Text) and IsStringANum(EditElectrical.Text) and IsStringANum(EditBetween.Text)) then
         ButtonOK.Enabled := True;
   end;
end;


procedure TThievingPads.CheckBoxCutoutsClick(Sender: TObject);
begin
   if CheckBoxCutouts.Checked then
      CheckBoxShortCircuitRule.Enabled := True
   else
      CheckBoxShortCircuitRule.Enabled := False;
end;


procedure TThievingPads.ButtonCancelClick(Sender: TObject);
begin
   close;
end;



procedure TThievingPads.CheckBoxObjectsInsideClick(Sender: TObject);
begin
   if CheckBoxObjectsInside.Checked then
   begin
      EditElectrical.Enabled  := True;
      CheckBoxElectrical.Enabled := True;
   end
   else
   begin
      EditElectrical.Enabled  := False;
      CheckBoxElectrical.Enabled := False;
   end;
end;



procedure TThievingPads.ButtonOKClick(Sender: TObject);
var

   BoardShapeRect : TCoordRect;
   NewPad         : IPCB_Arc;
   PosX, PosY     : Integer;
   TheLayerStack  : IPCB_LayerStack;
   LayerObj       : IPCB_LayerObject;
   LayerNum       : integer;
   Comp           : IPCB_Component;
   Distance       : Float;

   Iterator       : IPCB_BoardIterator;
   Rule           : IPCB_Rule;
   RuleOutline    : IPCB_ClearanceConstraint;
   RuleElectrical : IPCB_ClearanceConstraint;
   RuleBetween    : IPCB_ClearanceConstraint;
   RuleCompClear  : IPCB_ComponentClearanceConstraint;
   RuleWidth      : IPCB_MaxMinWidthConstraint;
   RuleCutout     : IPCB_ShortCircuitConstraint;

   MaxGap         : Integer;
   PadRect        : TCoordRect;
   Spatial        : IPCB_SpatialIterator;
   Primitive      : IPCB_Primitive;
   Violation      : IPCB_Violation;
   ViolationFlag  : Integer;
   CutoutFlag     : Integer;
   SetOfLayers    : IPCB_LayerSet;
   TempString     : String;
   TempInt        : Integer;

begin

   // Fix numbers based on local settings
   TempString := EditSize.Text;
   if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
   EditSize.Text := TempString;

   TempString := EditBetween.Text;
   if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
   EditBetween.Text := TempString;

   TempString := EditElectrical.Text;
   if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator; 
   EditElectrical.Text := TempString;

   TempString := EditOutline.Text;
   if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;
   EditOutline.Text := TempString;


   // Distance
   Distance := StrToFloat(EditSize.Text) + StrToFloat(EditBetween.Text);

   if RadioButtonMM.Checked then Distance := MMsToCoord(Distance)
   else                          Distance := MilsToCoord(Distance);

   // First I neeed to get Board shape bounding rectangel - we start from this
   BoardShapeRect := Board.BoardOutline.BoundingRectangle;
   TheLayerStack := Board.LayerStack;

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
   Comp.Name.Text    := 'Venting';

   // Make the comment text visible;
   Comp.CommentOn    := False;
   Comp.Comment.Text := 'Component That Holds Thieving Pads';

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
   RuleCompClear  := Nil;
   RuleWidth      := Nil;
   RuleCutout     := Nil;

   While (Rule <> Nil) Do
   Begin
       if Rule.RuleKind = eRule_Clearance then
       begin

          if (Rule.Scope1Expression = 'InComponent(''Venting'')') and (Rule.Scope2Expression = 'OnLayer(''Keep-Out Layer'')') then
             RuleOutline := Rule
          else if (Rule.Scope1Expression = 'InComponent(''Venting'')') and (Rule.Scope2Expression = 'OnSignal') then
             RuleElectrical := Rule
          else if (Rule.Scope1Expression = 'InComponent(''Venting'')') and (Rule.Scope1Expression = 'InComponent(''Venting'')') then
             RuleBetween := Rule;

       end;

       if Rule.RuleKind = eRule_ComponentClearance then
          if (Rule.Scope1Expression = 'InComponent(''Venting'')') and (Rule.Scope2Expression = 'All') then
             RuleCompClear := Rule;

       if Rule.RuleKind = eRule_MaxMinWidth then
          if (Rule.Scope1Expression = 'InComponent(''Venting'')') then
             RuleWidth := Rule;

       if Rule.RuleKind = eRule_ShortCircuit then
          if (Rule.Scope1Expression = 'InComponent(''Venting'')') and (Rule.Scope2Expression = 'IsBoardCutoutRegion') then
             RuleCutout := Rule;

       Rule :=  Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

   // Check rule to outline
   if RuleOutline = Nil then
   begin
      RuleOutline := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleOutline.Scope1Expression := 'InComponent(''Venting'')';
      RuleOutline.Scope2Expression := 'OnLayer(''Keep-Out Layer'')';

      RuleOutline.NetScope  := eNetScope_AnyNet;

      RuleOutline.Name    := 'Venting-Board';
      RuleOutline.Comment := 'Clearance between Venting Pads and Board Outline';

      // Add the rule into the Board
      Board.AddPCBObject(RuleOutline);

   end;

   if RadioButtonMM.Checked then RuleOutline.Gap := MMsToCoord(StrToFloat(EditOutline.Text))
   else                          RuleOutline.Gap := MilsToCoord(StrToFloat(EditOutline.Text));


   // Check rule to other electrical objects
   if RuleElectrical = Nil then
   begin
      RuleElectrical := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleElectrical.Scope1Expression := 'InComponent(''Venting'')';
      RuleElectrical.Scope2Expression := 'OnSignal';

      RuleElectrical.NetScope  := eNetScope_AnyNet;

      if RadioButtonMM.Checked then RuleElectrical.Gap := MMsToCoord(StrToFloat(EditElectrical.Text))
      else                          RuleElectrical.Gap := MilsToCoord(StrToFloat(EditElectrical.Text));

      RuleElectrical.Name    := 'Venting-Electrical';
      RuleElectrical.Comment := 'Clearance between Venting Pads and Electrical Objects';

      // Add the rule into the Board
      Board.AddPCBObject(RuleElectrical);

   end;

   if RadioButtonMM.Checked then RuleElectrical.Gap := MMsToCoord(StrToFloat(EditElectrical.Text))
   else                          RuleElectrical.Gap := MilsToCoord(StrToFloat(EditElectrical.Text));

   // Check rule between pads
   if RuleBetween = Nil then
   begin
      RuleBetween := PCBServer.PCBRuleFactory(eRule_Clearance);

      // Set values
      RuleBetween.Scope1Expression := 'InComponent(''Venting'')';
      RuleBetween.Scope2Expression := 'InComponent(''Venting'')';

      RuleBetween.NetScope  := eNetScope_AnyNet;

      RuleBetween.Name    := 'Venting-Internal';
      RuleBetween.Comment := 'Clearance between Venting Pads';

      // Add the rule into the Board
      Board.AddPCBObject(RuleBetween);

   end;

   if RadioButtonMM.Checked then RuleBetween.Gap := MMsToCoord(StrToFloat(EditBetween.Text))
   else                          RuleBetween.Gap := MilsToCoord(StrToFloat(EditBetween.Text));

   // Check component clearence rule
   if CheckBoxCompClearRule.Checked then
   begin
      if RuleCompClear = Nil then
      begin
         RuleCompClear := PCBServer.PCBRuleFactory(eRule_ComponentClearance);

         // Set values
         RuleCompClear.Scope1Expression := 'InComponent(''Venting'')';
         RuleCompClear.Scope2Expression := 'All';

         RuleCompClear.NetScope := eNetScope_AnyNet;

         RuleCompClear.Name     := 'Venting-Comp';
         RuleCompClear.Comment  := 'Clearance between "Venting" Component and other Components';

         // Add the rule into the Board
         Board.AddPCBObject(RuleCompClear);

      end;

      RuleCompClear.Gap         := 0;
      RuleCompClear.VerticalGap := 0;
   end;

   If TheLayerStack = Nil Then Exit;

   // Check Routing Width rule
   if CheckBoxRoutingWidthRule.Checked then
   begin
      if RuleWidth = Nil then
      begin
         RuleWidth := PCBServer.PCBRuleFactory(eRule_MaxMinWidth);

         // Set values
         RuleWidth.Scope1Expression := 'InComponent(''Venting'')';

         RuleWidth.NetScope := eNetScope_AnyNet;

         RuleWidth.Name     := 'Venting-Width';
         RuleWidth.Comment  := 'Width Constraint for Arcs in "Venting" Component';

         // Add the rule into the Board
         Board.AddPCBObject(RuleWidth);

      end;

      // Use MaxGap to temporary store Width
      if RadioButtonMM.Checked then MaxGap := MMsToCoord(StrToFloat(EditSize.Text)) / 2
      else                          MaxGap := MilsToCoord(StrToFloat(EditSize.Text)) / 2;

      LayerObj := TheLayerStack.FirstLayer;
      Repeat
         // we check if this is a signal layer
         if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
         begin
            RuleWidth.MinWidth[LayerObj.V7_LayerID]     := MaxGap;
            RuleWidth.MaxWidth[LayerObj.V7_LayerID]     := MaxGap;
            RuleWidth.FavoredWidth[LayerObj.V7_LayerID] := MaxGap;
         end;
         LayerObj := TheLayerStack.NextLayer(LayerObj); ;
      Until LayerObj = Nil;
   end;

   // Check short circuit rule (needed for board cutout)
   if (CheckBoxShortCircuitRule.Checked and CheckBoxCutouts.Checked) then
   begin
      if RuleCutout = Nil then
      begin
         RuleCutout := PCBServer.PCBRuleFactory(eRule_ShortCircuit);

         // Set values
         RuleCutout.Scope1Expression := 'InComponent(''Venting'')';
         RuleCutout.Scope2Expression := 'IsBoardCutoutRegion';

         RuleCutout.NetScope := eNetScope_AnyNet;

         RuleCutout.Name    := 'Venting-Cutout';
         RuleCutout.Comment := 'Allow short between Board Cutout and Venting Pads';

         // Add the rule into the Board
         Board.AddPCBObject(RuleCutout);

      end;

      if CheckBoxCutouts.Checked then
         RuleCutout.Allowed := True
      else
         RuleCutout.Allowed := False;
   end;

   if (not CheckBoxObjectsInside.Checked) then
      RuleElectrical.Gap := RuleOutline.Gap;

   // We will save MaxGap value for further testing
   if RuleElectrical.Gap > RuleOutline.Gap then MaxGap := RuleElectrical.Gap
   else                                         MaxGap := RuleOutline.Gap;

   LayerNum := 1;

   LayerObj := TheLayerStack.FirstLayer;
   Repeat
      // we check if this is a signal layer
      if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
      begin
         if (((LayerNum = 1) and (CheckBoxTop.Checked)) or ((CheckBoxMid.Checked) and (LayerNum <> 1) and (LayerNum <> TheLayerStack.SignalLayerCount))
         or ((LayerNum = TheLayerStack.SignalLayerCount) and (CheckBoxBottom.Checked))) then
         Begin
            // We start from top left
            PosX := BoardShapeRect.Left;
            PosY := BoardShapeRect.Top;

            while ((PosY > BoardShapeRect.Bottom)) do
            begin

               if ((Board.BoardOutline.PointInPolygon(PosX, PosY) and (CheckBoxObjectsInside.Checked or CheckBoxCutouts.Checked)) or
                  ((not(Board.BoardOutline.PointInPolygon(PosX, PosY))) and (CheckBoxObjectsOutside.Checked))) then
                  Begin
                     Try
                        PCBServer.PreProcess;
                        NewPad := PcbServer.PCBObjectFactory(eArcObject,eNoDimension,eCreate_Default);
                        If NewPad = Nil Then Exit;

                        NewPad.BeginModify;
                        NewPad.StartAngle := 0;
                        NewPad.EndAngle   := 360;
                        NewPad.XCenter    := PosX;
                        NewPad.YCenter    := PosY;

                        if RadioButtonMM.Checked then NewPad.LineWidth := MMsToCoord(StrToFloat(EditSize.Text)) / 2
                        else                          NewPad.LineWidth := MilsToCoord(StrToFloat(EditSize.Text)) / 2;

                        NewPad.Radius    := NewPad.LineWidth / 2;
                        NewPad.Layer     := LayerObj.V7_LayerID;

                     Finally
                        PCBServer.PostProcess;
                     End;

                     NewPad.EndModify;
                     Board.AddPCBObject(NewPad);
                     Comp.AddPCBObject(NewPad);
                     PCBServer.SendMessageToRobots(Comp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);

                     // here we need to set up spatial iterator that will
                     // Check if we have clearence issues on pads
                     PadRect := NewPad.BoundingRectangle;

                     SetOfLayers := LayerSet.CreateLayerSet;
                     SetOfLayers.Include(LayerObj.V7_LayerID);
                     SetOfLayers.Include(String2Layer('Multi Layer'));
                     SetOfLayers.Include(String2Layer('Keep Out Layer'));

                     Spatial := Board.SpatialIterator_Create;
                     Spatial.AddFilter_ObjectSet(AllObjects);
                     Spatial.AddFilter_IPCB_LayerSet(SetOfLayers);
                     Spatial.AddFilter_Area(PadRect.Left - MaxGap, PadRect.Bottom - MaxGap, PadRect.Right + MaxGap, PadRect.Top + MaxGap);

                     ViolationFlag := 0;
                     CutoutFlag    := 0;
                     Primitive := Spatial.FirstPCBObject;   

                     while (Primitive <> nil) do
                     begin
                        if (Primitive.ObjectId = eRegionObject) and (Primitive.Kind = eRegionkind_BoardCutout) and (CheckBoxCutouts.Checked) then
                        begin
                           // Here we will do trick to see if this one is inside board cutout
                           TempInt := RuleElectrical.Gap;
                           RuleElectrical.Gap := 0;

                           Violation := RuleElectrical.ActualCheck(Primitive, NewPad);
                           if Violation <> nil then
                              CutOutFlag := 1;

                           RuleElectrical.Gap := TempInt;
                        end
                        else
                        begin
                           Violation := RuleElectrical.ActualCheck(Primitive, NewPad);
                           if Violation <> nil then
                              ViolationFlag := 1;

                           Violation := RuleOutline.ActualCheck(Primitive, NewPad);
                           if Violation <> nil then
                              ViolationFlag := 1;
                        end;

                        Primitive := Spatial.NextPCBObject;
                     end;
                     Board.SpatialIterator_Destroy(Spatial);

                     SetOfLayers := nil;

                     If (ViolationFlag = 1) or (Board.BoardOutline.PointInPolygon(PosX, PosY) and ((not CheckBoxObjectsInside.Checked) and (CutOutflag = 0))) then
                        Board.RemovePCBObject(NewPad);

                  end;

               // finally we need to move one point right
               PosX := PosX + Distance;

               if PosX > BoardShapeRect.Right then
               begin
                  PosX := BoardShapeRect.Left;

                  PosY := PosY - Distance;
               end;
            end;
         end;
         Inc(LayerNum);
      end;

      LayerObj := TheLayerStack.NextLayer(LayerObj);
   Until LayerObj = Nil;

   if not CheckBoxBetween.Checked       then Board.RemovePCBObject(RuleBetween);
   if not CheckBoxOutline.Checked       then Board.RemovePCBObject(RuleOutline);
   if not (CheckBoxObjectsInside.Checked and CheckBoxElectrical.Checked) then Board.RemovePCBObject(RuleElectrical);

   Comp.PrimitiveLock := False;
   Comp.Moveable := False;
   Board.ViewManager_GraphicallyInvalidatePrimitive(Comp);

   ResetParameters;
   AddStringParameter('Action', 'Redraw');
   RunProcess('PCB:Zoom');

   close;
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   ThievingPads.ShowModal;
end;



