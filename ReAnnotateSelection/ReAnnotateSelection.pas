
var
   Board   : IPCB_Board;
   UsedDes : TStringList;


procedure TFormReAnnotate.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;


Procedure ReAnnotateLayer(Layer: TLayer, LeftRight : String, UpDown : String);
var
   i, j       : Integer;
   Comp       : IPCB_Component;
   Components : TStringList;
   X, Y       : TCoord;
   Designator : String;

   UsedPrefix : String;
   UsedIndex  : String;
   UsedIndexes: TStringList;
begin

   Components := TStringList.Create;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Comp := Board.SelectecObject[i];
      if Comp.Layer = Layer then
         Components.AddObject('i',Comp);
   end;

   UsedIndexes := TStringList.Create;

   While Components.Count > 0 do
   begin
      if LeftRight = 'Left2Right' then X := Board.BoundingRectangleChildren.Right
      else                             X := Board.BoundingRectangleChildren.Left;

      if UpDown = 'Up2Down'       then Y := Board.BoundingRectangleChildren.Bottom
      else                             Y := Board.BoundingRectangleChildren.Top;

      for i := 0 to Components.Count - 1 do
      begin
         Comp := Components.GetObject(i);

         if RadioButtonHorizontal.Checked then
         begin
           if ((UpDown = 'Up2Down') and (Comp.y > Y)) or ((UpDown = 'Up2Down') and (Comp.y = Y) and
              (((LeftRight = 'Left2Right') and (Comp.x < X)) or ((LeftRight = 'Right2Left') and (Comp.x > X)))) or
              ((UpDown = 'Down2Up') and (Comp.y < Y)) or ((UpDown = 'Down2Up') and (Comp.y = Y) and
              (((LeftRight = 'Left2Right') and (Comp.x < X)) or ((LeftRight = 'Right2Left') and (Comp.x > X)))) then
              begin
                 X := Comp.x;
                 Y := Comp.y;
                 j := i;
              end;
         end
         else
         begin
            if ((LeftRight = 'Left2Right') and (Comp.x < X)) or ((LeftRight = 'Left2Right') and (Comp.x = X) and
               (((UpDown = 'Up2Down') and (Comp.y > Y)) or ((UpDown = 'Down2Up') and (Comp.y < Y)))) or
               ((LeftRight = 'Right2Left') and (Comp.x > X)) or ((LeftRight = 'Right2Left') and (Comp.x = X) and
               (((UpDown = 'Up2Down') and (Comp.y > Y)) or ((UpDown = 'Down2Up') and (Comp.y < Y)))) then
               begin
                  X := Comp.x;
                  Y := Comp.y;
                  j := i;
               end;
         end;
      end;
      Comp := Components.GetObject(j);
      Components.Delete(j);
      Designator := Comp.Name.Text;

      While Length(Designator) > 0 do
         if (Ord(Designator[Length(Designator)]) > 47) and (Ord(Designator[Length(Designator)]) < 58) then
            SetLength(Designator, Length(Designator) - 1)
         else
            break;

      for i := 0 to UsedDes.Count - 1 do
      begin
         UsedPrefix := UsedDes[i];

         While Length(UsedPrefix) > 0 do
            if (Ord(UsedPrefix[Length(UsedPrefix)]) > 47) and (Ord(UsedPrefix[Length(UsedPrefix)]) < 58) then
               SetLength(UsedPrefix, Length(UsedPrefix) - 1)
            else
               break;

         if UsedPrefix = Designator then
         begin
            UsedIndex := UsedDes[i];
            Delete(UsedIndex,1,Length(UsedPrefix));

            UsedIndexes.Add(UsedIndex);
         end;
      end;

      i := 1;
      While (UsedIndexes.IndexOf(IntToStr(i)) >= 0) do Inc(i);

      Designator := Designator + IntToStr(i);

      Comp.Name.Text := Designator;
      Comp.Name.GraphicallyInvalidate;

      UsedDes.Add(Designator);
      UsedIndexes.Clear;
   end;
end;


procedure TFormReAnnotate.ButtonOKClick(Sender: TObject);
var
   i       : Integer;
   Iter    : IPCB_BoardIterator;
   Comp    : IPCB_Component;
   First   : TLayer;
begin

   UsedDes := TStringList.Create;

   // Unselect all but components

   For i := 0 to Board.SelectecObjectCount - 1 do
      if Board.SelectecObject[i].ObjectId <> eComponentObject then
         UsedDes.AddObject(IntToStr(i),Board.SelectecObject[i]);

   for i := 0 to UsedDes.Count - 1 do
   begin
      UsedDes.GetObject(i).Selected := False;
   end;

   UsedDes.Clear;

   // Fill in UsedDes list with used designators
   Iter := Board.BoardIterator_Create;
   Iter.AddFilter_AllLayers;
   Iter.AddFilter_ObjectSet(mkSet(eComponentObject));

   Comp := Iter.FirstPCBObject;

   while (Comp <> nil) do
   begin
      if not Comp.Selected then
         UsedDes.Add(Comp.Name.Text);

      Comp := Iter.NextPCBObject
   end;
   Board.BoardIterator_Destroy(Iter);

   if RadioButtonTop.Checked then
   begin
      if RadioButton1.Checked then
      begin
         ReAnnotateLayer(eTopLayer,'Left2Right','Up2Down');
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Right2Left','Up2Down')
         else
            ReAnnotateLayer(eBottomLayer,'Left2Right','Up2Down');
      end
      else if RadioButton2.Checked then
      begin
         ReAnnotateLayer(eTopLayer,'Right2Left','Up2Down');
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Left2Right','Up2Down')
         else
            ReAnnotateLayer(eBottomLayer,'Right2Left','Up2Down');
      end
      else if RadioButton3.Checked then
      begin
         ReAnnotateLayer(eTopLayer,'Left2Right','Down2Up');
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Right2Left','Down2Up')
         else
            ReAnnotateLayer(eBottomLayer,'Left2Right','Down2Up');
      end
      else if RadioButton4.Checked then
      begin
         ReAnnotateLayer(eTopLayer,'Right2Left','Down2Up');
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Left2Right','Down2Up')
         else
            ReAnnotateLayer(eBottomLayer,'Right2Left','Down2Up');
      end;
   end
   else
   begin
      if RadioButton1.Checked then
      begin
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Right2Left','Up2Down')
         else
            ReAnnotateLayer(eBottomLayer,'Left2Right','Up2Down');
         ReAnnotateLayer(eTopLayer,'Left2Right','Up2Down');
      end
      else if RadioButton2.Checked then
      begin
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Left2Right','Up2Down')
         else
            ReAnnotateLayer(eBottomLayer,'Right2Left','Up2Down');
         ReAnnotateLayer(eTopLayer,'Right2Left','Up2Down');
      end
      else if RadioButton3.Checked then
      begin
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Right2Left','Down2Up')
         else
            ReAnnotateLayer(eBottomLayer,'Left2Right','Down2Up');
         ReAnnotateLayer(eTopLayer,'Left2Right','Down2Up');
      end
      else if RadioButton4.Checked then
      begin
         if CheckBoxMirror.Checked then
            ReAnnotateLayer(eBottomLayer,'Left2Right','Down2Up')
         else
            ReAnnotateLayer(eBottomLayer,'Right2Left','Down2Up');
         ReAnnotateLayer(eTopLayer,'Right2Left','Down2Up');
      end;
   end;

   close;
end;


Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   if Board.SelectecObjectCount = 0 then exit;

   FormReAnnotate.ShowModal;
end;
