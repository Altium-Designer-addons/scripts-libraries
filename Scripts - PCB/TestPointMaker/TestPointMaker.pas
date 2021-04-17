{..............................................................................}
{ Summary   This script creates Test points for a Net class. they are added    }
{           above the PCB, ready to be assigned.                               }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board        : IPCB_Board;



function ReturnString(Tekst : String) : String;
begin
   Result := Tekst;
end;



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



procedure TTestPointMakerForm.RadioButtonSMClick(Sender: TObject);
begin
   if RadioButtonSM.Checked then
   begin
      EditHoleSize.Enabled       := False;
      CheckBoxFabBottom.Enabled  := False;
      CheckBoxAssyBottom.Enabled := False;
   end
   else
   begin
      EditholeSize.Enabled       := True;
      CheckBoxFabBottom.Enabled  := True;
      CheckBoxAssyBottom.Enabled := True;
   end;
end;



procedure TTestPointMakerForm.RadioButtonTHClick(Sender: TObject);
begin
   if RadioButtonSM.Checked then
   begin
      EditHoleSize.Enabled       := False;
      CheckBoxFabBottom.Enabled  := False;
      CheckBoxAssyBottom.Enabled := False;
   end
   else
   begin
      EditholeSize.Enabled       := True;
      CheckBoxFabBottom.Enabled  := True;
      CheckBoxAssyBottom.Enabled := True;
   end;
end;



procedure TTestPointMakerForm.EditSizeChange(Sender: TObject);
begin
   if not IsStringANum(EditSize.Text) then
   begin
      ButtonOK.Enabled := False;
      EditSize.Font.Color := clRed;
   end
   else
   begin
      EditSize.Font.Color := clWindowText;
      if IsStringANum(EditHoleSize.Text) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TTestPointMakerForm.EditHoleSizeChange(Sender: TObject);
begin
   if not IsStringANum(EditHoleSize.Text) then
   begin
      ButtonOK.Enabled := False;
      EditSize.Font.Color := clRed;
   end
   else
   begin
      EditSize.Font.Color := clWindowText;
      if IsStringANum(EditSize.Text) then
         ButtonOK.Enabled := True;
   end;
end;



procedure TTestPointMakerForm.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;



procedure TTestPointMakerForm.ButtonOKClick(Sender: TObject);
var

   Iterator     : IPCB_BoardIterator;
   GrIter       : IPCB_GroupIterator;
   NetClass     : IPCB_ObjectClass;
   Comp         : IPCB_Component;
   Primitive    : IPCB_Primitive;
   NewTestPoint : IPCB_Pad2;
   TestPoint    : IPCB_Pad2;
   Net          : IPCB_Net;
   i, j         : Integer;
   FabFlag      : Integer;
   AssyFlag     : Integer;
   Rectangle    : TCoordRect;
   PosX         : Integer;
   PosY         : Integer;
   SizeX        : Integer;
   SizeY        : Integer;
   Left         : Integer;
   Mid          : Integer;

begin
   Try
      PCBServer.PreProcess;
      TestPoint := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);
      If TestPoint = Nil Then Exit;

      TestPoint.BeginModify;
      TestPoint.Mode := ePadMode_Simple;
      TestPoint.X    := PosX;
      TestPoint.Y    := PosY;

      if RadioButtonMM.Checked then TestPoint.TopXSize := MMsToCoord(StrToFloat(EditSize.Text))
      else                          TestPoint.TopXSize := MilsToCoord(StrToFloat(EditSize.Text));

      if RadioButtonMM.Checked then TestPoint.TopYSize := MMsToCoord(StrToFloat(EditSize.Text))
      else                          TestPoint.TopYSize := MilsToCoord(StrToFloat(EditSize.Text));

      TestPoint.TopShape  := eRounded;
      TestPoint.HoleSize  := 0;

      if RadioButtonSM.Checked then
         TestPoint.Layer := eTopLayer
      else
      begin
         TestPoint.Layer := eMultiLayer;

         if RadioButtonMM.Checked then TestPoint.HoleSize := MMsToCoord(StrToFloat(EditHoleSize.Text))
         else                          TestPoint.HoleSize := MilsToCoord(StrToFloat(EditHoleSize.Text));
      end;

      TestPoint.Name      := 'TestPoint';

   Finally
      PCBServer.PostProcess;
   End;

   TestPoint.EndModify;

   Rectangle := TestPoint.BoundingRectangle;
   SizeX := Rectangle.Right - Rectangle.Left;
   SizeY := Rectangle.Top - Rectangle.Bottom;

   Rectangle := Board.BoardOutline.BoundingRectangle;
   Mid  := (Rectangle.Left + Rectangle.Right) / 2;

   PosX := Rectangle.Right;
   PosY := Rectangle.Top + MilsToCoord(50) + TestPoint.TopXSize/2;

   // We will get net class
   Iterator := Board.BoardIterator_Create;

   Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
   Iterator.AddFilter_LayerSet(AllLayers);
   Iterator.AddFilter_Method(eProcessAll);

   NetClass := Iterator.FirstPCBObject;
   While NetClass <> NIl Do
   Begin
      If NetClass.MemberKind = eClassMemberKind_Net Then
         if NetClass.Name = ReturnString(ComboBoxNets.Text) then
            break;

      NetClass := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);
               
   // now CLS contains net class. We need to find a way to cycle through all nets
   // In that net class.

   Iterator := Board.BoardIterator_Create;

   Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
   Iterator.AddFilter_LayerSet(AllLayers);
   Iterator.AddFilter_Method(eProcessAll);

   Net := Iterator.FirstPCBObject;
   while Net <> nil do
   begin
      if NetClass.IsMember(Net) then
      begin
         FabFlag := 1;
         AssyFlag := 1;

         if (CheckBoxFabTop.Checked  or CheckBoxFabBottom.Checked)  then FabFlag  := 0;
         if (CheckBoxAssyTop.Checked or CheckBoxAssyBottom.Checked) then AssyFlag := 0;


         if not CheckBoxForce.Checked then
         begin
            // Test pads to see weather we have testpoint on this net

            GrIter := Net.GroupIterator_Create;
            GrIter.AddFilter_ObjectSet(Mkset(ePadObject, eViaObject));
            GrIter.AddFilter_AllLayers;

            Primitive := GrIter.firstPCBObject;

            While (Primitive <> nil) do
            begin
               if ((Primitive.IsTestpoint_Top or Primitive.IsTestpoint_Bottom) and (CheckBoxFabTop.Checked or CheckBoxFabBottom.Checked)) then
                  FabFlag := 1;

               if ((Primitive.IsAssyTestpoint_Top or Primitive.IsAssyTestpoint_Bottom) and (CheckBoxAssyTop.Checked or CheckBoxAssyBottom.Checked)) then
                  AssyFlag := 1;

               Primitive := GrIter.NextPCBObject;
            end;
            Net.GroupIterator_Destroy(GrIter);
         end;


         // Now we have to test weather to add TestPoints or not
         if ((FabFlag = 0) or (AssyFlag = 0)) then
         begin

            PCBServer.PreProcess;

            NewTestPoint := TestPoint.Replicate;

            NewTestPoint.BeginModify;
            PCBServer.SendMessageToRobots(
               NewTestPoint.I_ObjectAddress,
               c_Broadcast,
               PCBM_BeginModify,
               c_NoEventData);

            NewTestPoint.Net := Net;
            NewTestPoint.x := PosX;
            NewTestPoint.y := PosY;

            NewTestPoint.IsTestpoint_Top        := CheckBoxFabTop.Checked;
            NewTestPoint.IsTestpoint_Bottom     := CheckBoxFabBottom.Checked;
            NewTestPoint.IsAssyTestpoint_Top    := CheckBoxAssyTop.Checked;
            NewTestPoint.IsAssyTestpoint_Bottom := CheckBoxAssyBottom.Checked;
            NewTestPoint.Moveable := True;

            NewTestPoint.EndModify;
            PCBServer.SendMessageToRobots(
               NewTestPoint.I_ObjectAddress,
               c_Broadcast,
               PCBM_EndModify,
               c_NoEventData);

            Board.AddPCBObject(NewTestPoint);
            Net.AddPCBObject(NewTestPoint);
            PCBServer.SendMessageToRobots(
               NewTestPoint.I_ObjectAddress,
               c_Broadcast,
               PCBM_BoardRegisteration,
               c_NoEventData);


            PCBServer.PostProcess;

            PosX := PosX - SizeX - MilsToCoord(50);

            if PosX < Mid then
            begin
               PosX := Rectangle.Right;
               PosY := PosY + SizeY + MilsToCoord(50);
            end;
         end;
      end;

      Net := Iterator.NextPCBObject;
   end;

   Board.BoardIterator_Destroy(Iterator);

   Board.ViewManager_FullUpdate;

   // Refresh PCB screen
   Client.CommandLauncher.LaunchCommand('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

   close;
end;



Procedure Start;
var
   flag     : Integer;
   Iterator : IPCB_BoardIterator;
   cls      : IPCB_ObjectClass;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   Iterator := Board.BoardIterator_Create;
   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
   cls := Iterator.FirstPCBObject;
   While cls <> NIl Do
   Begin
      If cls.MemberKind = eClassMemberKind_Net Then
         ComboBoxNets.Items.Add(cls.Name);

      cls := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

   TestPointMakerForm.ShowModal;
end;

