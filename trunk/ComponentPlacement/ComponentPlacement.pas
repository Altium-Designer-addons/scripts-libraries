{..............................................................................}
{ Summary   This scripts is made to ease component placement. It allows user   }
{           to easily copy destination component placement based on their      }
{           relative placement of orientational component (I know this does    }
{           not explain anything, but read on).                                }
{                                                                              }
{           This script asks user to click on components a lot. First he needs }
{           to select source orientational component and destination           }
{           orientational component. This two components are main ones, since  }
{           all other component placement will be based on this two.           }
{                                                                              }
{           After this user needs to click on source component, and after that }
{           on destination component. Destination component placement will be  }
{           changed so that relative placement of source component and source  }
{           orientational component =  relative placement of destination       }
{           component and destination orientational component                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

 Procedure RepositionComponents;
var

   Board       : IPCB_Board;
   Source      : IPCB_Component;
   Destin      : IPCB_Component;
   SourceX     : TCoord;
   SourceY     : TCoord;
   SourceRot   : TAngle;
   DestinX     : TCoord;
   DestinY     : TCoord;
   DestinRot   : TAngle;

   SameLayer   : Boolean;

   Distance    : TCoord;
   Angle       : TAngle;

   X, Y        : Integer;
   Rotation    : TAngle;

begin

   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   // Get reference source and destination component

   Destin := nil;

   While Destin = nil do
   begin
      Source := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Source Orientational Component');
      if Source = nil then exit;

      Destin := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Destination Orientational Component');
   end;

   SourceX     := Source.x;
   SourceY     := Source.y;
   SourceRot   := Source.Rotation;

   DestinX     := Destin.x;
   DestinY     := Destin.y;
   DestinRot   := Destin.Rotation;

   if Source.Layer = Destin.Layer then SameLayer := True
   else                                SameLayer := False;

   while True do
   begin

      Source := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Source Component');
      if Source = nil then exit;

      Distance := sqrt(sqr(SourceX - Source.x) + sqr(SourceY - Source.y));

      if SourceX = source.x then
         Angle := 90.0 - SourceRot
      else
         Angle := RadToDeg(ArcTan((Source.y - SourceY) / (Source.x - SourceX))) - SourceRot;

      if Source.X < SourceX then Angle := Angle + 180;
      
      Destin := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers, 'Choose Destination Component');

      if Destin <> nil then
      begin
         // Modify Layer
         if SameLayer = True then
         begin
            // Here I need to relatively place destination component

            X := Distance * cos(DegToRad(DestinRot + Angle));
            Y := Distance * sin(DegToRad(DestinRot + Angle));

            Destin.MoveToXY(DestinX + X, DestinY + Y);
            Destin.Layer := Source.Layer;

            // Modify rotation
            Destin.Rotation := DestinRot - SourceRot + Source.Rotation;



            // Designator
            Distance := sqrt(sqr(Source.Name.XLocation - Source.x) + sqr(Source.Name.YLocation - Source.y));

            if Source.Name.XLocation = source.x then
               Angle := 90.0 - Source.Rotation
            else
               Angle := RadToDeg(ArcTan((Source.Name.YLocation - Source.y) / (Source.Name.XLocation - Source.x))) - Source.Rotation;

            if Source.Name.XLocation < Source.x then Angle := Angle + 180;

            X := Distance * cos(DegToRad(Destin.Rotation + Angle));
            Y := Distance * sin(DegToRad(Destin.Rotation + Angle));


            Destin.Name.MoveToXY(Destin.X + X, Destin.Y + Y);

            Destin.Name.Rotation := Destin.Rotation - Source.Rotation + Source.Name.Rotation;


            // Comment
            Distance := sqrt(sqr(Source.Comment.XLocation - Source.x) + sqr(Source.Comment.YLocation - Source.y));

            if Source.Comment.XLocation = source.x then
               Angle := 90.0 - Source.Rotation
            else
               Angle := RadToDeg(ArcTan((Source.Comment.YLocation - Source.y) / (Source.Comment.XLocation - Source.x))) - Source.Rotation;

            if Source.Comment.XLocation < Source.x then Angle := Angle + 180;

            X := Distance * cos(DegToRad(Destin.Rotation + Angle));
            Y := Distance * sin(DegToRad(Destin.Rotation + Angle));


            Destin.Comment.MoveToXY(Destin.X + X, Destin.Y + Y);

            Destin.Comment.Rotation := Destin.Rotation - Source.Rotation + Source.Comment.Rotation;
         end
         else
         begin
            // Here I need to relatively place destination component

            X := Distance * cos(DegToRad(180 + DestinRot - Angle));
            Y := Distance * sin(DegToRad(180 + DestinRot - Angle));

            Destin.MoveToXY(DestinX - X, DestinY - Y);

            if Source.Layer = eTopLayer then
               Destin.Layer := eBottomLayer
            else
               Destin.Layer := eTopLayer;

            Destin.Rotation := DestinRot + SourceRot - Source.Rotation;



            // Designator
            Distance := sqrt(sqr(Source.Name.XLocation - Source.x) + sqr(Source.Name.YLocation - Source.y));

            if Source.Name.XLocation = source.x then
               Angle := 90.0 - Source.Rotation
            else
               Angle := RadToDeg(ArcTan((Source.Name.YLocation - Source.y) / (Source.Name.XLocation - Source.x))) - Source.Rotation;

            if Source.Name.XLocation < Source.x then Angle := Angle + 180;


            X := Distance * cos(DegToRad(180 + Destin.Rotation - Angle));
            Y := Distance * sin(DegToRad(180 + Destin.Rotation - Angle));


            Destin.Name.MoveToXY(Destin.X - X, Destin.Y - Y);

            Destin.Name.Rotation := Destin.Rotation + Source.Rotation - Source.Name.Rotation;


            // Comment
            Distance := sqrt(sqr(Source.Comment.XLocation - Source.x) + sqr(Source.Comment.YLocation - Source.y));

            if Source.Comment.XLocation = source.x then
               Angle := 90.0 - Source.Rotation
            else
               Angle := RadToDeg(ArcTan((Source.Comment.YLocation - Source.y) / (Source.Comment.XLocation - Source.x))) - Source.Rotation;

            if Source.Comment.XLocation < Source.x then Angle := Angle + 180;

            X := Distance * cos(DegToRad(180 + Destin.Rotation + Angle));
            Y := Distance * sin(DegToRad(180 + Destin.Rotation + Angle));


            Destin.Comment.MoveToXY(Destin.X - X, Destin.Y - Y);

            Destin.Comment.Rotation := Destin.Rotation + Source.Rotation - Source.Comment.Rotation;
         end;

         Destin.GraphicallyInvalidate;
      end;
   end;
end;
