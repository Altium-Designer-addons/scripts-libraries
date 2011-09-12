procedure FlipOnClick;
var
   Board         : IPCB_Board;
   Component     : IPCB_Component;
   CompIterator  : IPCB_GroupIterator;
   Primitive     : IPCB_Primitive;
   Pad           : IPCB_Pad2;
   OldRotation   : Float;
   DictionaryX   : TStringList;
   DictionaryY   : TStringList;
   Line          : String;
   Location      : String;
   Number        : String;
   i             : Integer;
   Indeks        : Integer;
   Num           : Integer;
   MaxX          : Integer;
   MaxY          : Integer;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   Component := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Component');

   OldRotation := Component.Rotation;

   Component.BeginModify;
   Component.Rotation := 0;
   Component.EndModify;

   CompIterator := Component.GroupIterator_Create;
   CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

   DictionaryX := TStringList.Create;
   DictionaryY := TStringList.Create;

   DictionaryX.NameValueSeparator := '=';
   DictionaryY.NameValueSeparator := '=';

   MaxX := 1;
   MaxY := 1;

   Pad := CompIterator.FirstPCBObject;


   While (Pad <> Nil) Do
   Begin
      Indeks := DictionaryX.IndexOfName(IntToStr(Pad.x));

      if Indeks = -1 then
         DictionaryX.Add(IntToStr(Pad.x) + '=1')
      else
      begin
         Number := DictionaryX.ValueFromIndex[Indeks];
         Num    := StrToInt(Number) + 1;

         if Num > MaxX then MaxX := Num;

         Number := IntToStr(Num);
         DictionaryX.Put(Indeks, IntToStr(Pad.x) + '=' + Number);
      end;

      Indeks := DictionaryY.IndexOfName(IntToStr(Pad.y));

      if Indeks = -1 then
         DictionaryY.Add(IntToStr(Pad.y) + '=1')
      else
      begin
         Number := DictionaryY.ValueFromIndex[Indeks];
         Num    := StrToInt(Number) + 1;

         if Num > MaxY then MaxY := Num;

         Number := IntToStr(Num);
         DictionaryY.Put(Indeks, IntToStr(Pad.y) + '=' + Number);
      end;

      Pad := CompIterator.NextPCBObject;
   End;
   Component.GroupIterator_Destroy(CompIterator);


   Component.BeginModify;
   if MaxY >= MaxX then
   begin
      // This is vertical component
      Component.Rotation := Oldrotation + 180;
      Component.Name.RotateBy(180);
      Component.Name.MoveByXY(2 * (Component.x - Component.Name.XLocation), 2 * (Component.y - Component.Name.YLocation));

      if Component.Layer = eTopLayer then Component.Layer := eBottomLayer
      else                                Component.Layer := eTopLayer;

   end
   else
   begin
      // This is horizontal component
      Component.Rotation := Oldrotation;

      if Component.Layer = eTopLayer then Component.Layer := eBottomLayer
      else                                Component.Layer := eTopLayer;

   end;
   Component.EndModify;
end;
