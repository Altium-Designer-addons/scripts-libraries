{..............................................................................}
{ Summary   This script works in 2 ways:                                       }
{           - If there are selected objects, it flips selected components to   }
{             the opposite side of the board.                                  }
{           - If there are no selected objects it asks user to click on        }
{             components, and it flips them.                                   }
{                                                                              }
{           Script uses smart way to figure out weather component is vertical  }
{           or horizontal, and it flips according to this info. This means     }
{           that for simple components like resistors pads will not change     }
{           position, and even if flipped they will be flipped in a smart way. }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
procedure Calculate(Component : IPCB_Component);
var
   CompIterator   : IPCB_GroupIterator;
   Primitive      : IPCB_Primitive;
   Pad            : IPCB_Pad2;
   Tekst          : IPCB_Text;
   OldRotation    : Float;
   DictionaryX    : TStringList;
   DictionaryY    : TStringList;
   Line           : String;
   Location       : String;
   Number         : String;
   i              : Integer;
   Indeks         : Integer;
   Num            : Integer;
   MaxX           : Integer;
   MaxY           : Integer;
   Rectangle      : TCoordRect;
   BoundRect      : TCoordRect;
   X1, Y1, X2, Y2 : Integer;
   X, Y           : Integer;
   Temp           : Integer;
begin

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



   if MaxY >= MaxX then
   begin


      // This is Horizontal component
      Component.Rotation := Oldrotation;

      CompIterator := Component.GroupIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

      Pad := CompIterator.FirstPCBObject;

      BoundRect := TCoordRect;

      if Pad <> nil then
      begin
         BoundRect.Left   := Pad.X;
         BoundRect.Right  := Pad.X;
         BoundRect.Top    := Pad.Y;
         BoundRect.Bottom := Pad.Y;
      end;

      Temp := 0;

      While (Pad <> Nil) Do
      Begin

         Inc(Temp);

         Rectangle := Pad.BoundingRectangle;

         if Rectangle.Left   < BoundRect.Left   then BoundRect.Left   := Rectangle.Left;
         if Rectangle.Right  > BoundRect.Right  then BoundRect.Right  := Rectangle.Right;
         if Rectangle.Top    > BoundRect.Top    then BoundRect.Top    := Rectangle.Top;
         if Rectangle.Bottom < BoundRect.Bottom then BoundRect.Bottom := Rectangle.Bottom;

         Pad := CompIterator.NextPCBObject;
      End;
      Component.GroupIterator_Destroy(CompIterator);

      X1 := BoundRect.Left;
      Y1 := BoundRect.Bottom;

      X := (BoundRect.Left + BoundRect.Right) / 2;
      Y := (BoundRect.Top + BoundRect.Bottom) / 2;

      // This is Horizontal component
      Component.Rotation := Oldrotation + 180;

      Component.BeginModify;
      if Component.Layer = eTopLayer then Component.Layer := eBottomLayer
      else                                Component.Layer := eTopLayer;
      Component.EndModify;

      CompIterator := Component.GroupIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

      Pad := CompIterator.FirstPCBObject;

      if Pad <> nil then
      begin
         BoundRect.Left   := Pad.X;
         BoundRect.Right  := Pad.X;
         BoundRect.Top    := Pad.Y;
         BoundRect.Bottom := Pad.Y;
      end;

      While (Pad <> Nil) Do
      Begin

         Rectangle := Pad.BoundingRectangle;

         if Rectangle.Left   < BoundRect.Left   then BoundRect.Left   := Rectangle.Left;
         if Rectangle.Right  > BoundRect.Right  then BoundRect.Right  := Rectangle.Right;
         if Rectangle.Top    > BoundRect.Top    then BoundRect.Top    := Rectangle.Top;
         if Rectangle.Bottom < BoundRect.Bottom then BoundRect.Bottom := Rectangle.Bottom;

         Pad := CompIterator.NextPCBObject;
      End;
      Component.GroupIterator_Destroy(CompIterator);

      X2 := BoundRect.Left;
      Y2 := BoundRect.Bottom;

      Component.MoveByXY(X1 - X2, Y1 - Y2);

      // we will modify tekst placement for this kind of rotation
      CompIterator := Component.GroupIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(eTextObject));

      Tekst := CompIterator.FirstPCBObject;

      While (Tekst <> Nil) Do
      Begin
         Tekst.RotateBy(180);
         Tekst.MoveByXY(2 * (X - Tekst.XLocation), 2 * (Y - Tekst.YLocation));

         Tekst := CompIterator.NextPCBObject;
      End;
      Component.GroupIterator_Destroy(CompIterator);

      Component.Name.RotateBy(180);
      Component.Name.MoveByXY(2 * (X - Component.Name.XLocation), 2 * (Y - Component.Name.YLocation));

      Component.Comment.RotateBy(180);
      Component.Comment.MoveByXY(2 * (X - Component.Comment.XLocation), 2 * (Y - Component.Comment.YLocation));


   end
   else
   begin
      // This is Vertical component
      Component.Rotation := Oldrotation;

      CompIterator := Component.GroupIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

      Pad := CompIterator.FirstPCBObject;

      BoundRect := TCoordRect;

      if Pad <> nil then
      begin
         BoundRect.Left   := Pad.X;
         BoundRect.Right  := Pad.X;
         BoundRect.Top    := Pad.Y;
         BoundRect.Bottom := Pad.Y;
      end;

      Temp := 0;

      While (Pad <> Nil) Do
      Begin

         Inc(Temp);

         Rectangle := Pad.BoundingRectangle;

         if Rectangle.Left   < BoundRect.Left   then BoundRect.Left   := Rectangle.Left;
         if Rectangle.Right  > BoundRect.Right  then BoundRect.Right  := Rectangle.Right;
         if Rectangle.Top    > BoundRect.Top    then BoundRect.Top    := Rectangle.Top;
         if Rectangle.Bottom < BoundRect.Bottom then BoundRect.Bottom := Rectangle.Bottom;

         Pad := CompIterator.NextPCBObject;
      End;
      Component.GroupIterator_Destroy(CompIterator);

      X1 := BoundRect.Left;
      Y1 := BoundRect.Bottom;

      Component.BeginModify;
      if Component.Layer = eTopLayer then Component.Layer := eBottomLayer
      else                                Component.Layer := eTopLayer;
      Component.EndModify;

      CompIterator := Component.GroupIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(ePadObject));

      Pad := CompIterator.FirstPCBObject;

      if Pad <> nil then
      begin
         BoundRect.Left   := Pad.X;
         BoundRect.Right  := Pad.X;
         BoundRect.Top    := Pad.Y;
         BoundRect.Bottom := Pad.Y;
      end;

      While (Pad <> Nil) Do
      Begin

         Rectangle := Pad.BoundingRectangle;

         if Rectangle.Left   < BoundRect.Left   then BoundRect.Left   := Rectangle.Left;
         if Rectangle.Right  > BoundRect.Right  then BoundRect.Right  := Rectangle.Right;
         if Rectangle.Top    > BoundRect.Top    then BoundRect.Top    := Rectangle.Top;
         if Rectangle.Bottom < BoundRect.Bottom then BoundRect.Bottom := Rectangle.Bottom;

         Pad := CompIterator.NextPCBObject;
      End;
      Component.GroupIterator_Destroy(CompIterator);

      X2 := BoundRect.Left;
      Y2 := BoundRect.Bottom;

      Component.MoveByXY(X1 - X2, Y1 - Y2);
   end;
end;


Procedure FlipComponents;
var
   Board      : IPCB_Board;
   Component  : IPCB_Component;
   Primitive  : IPCB_Primitive;
   i          : Integer;
   flag       : Integer;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   flag := 0;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Primitive := Board.SelectecObject[i];
      if Primitive.ObjectId = eComponentObject then
      begin
         Calculate(Primitive);
         flag := 1;
      end;
   end;

   if flag = 0 then
   begin
      Component := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Component');
      While Component <> nil do
      begin
         Calculate(Component);
         Component := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Component');
      end;
   end;
end;
