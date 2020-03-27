Procedure StartScript ();
var
   Board              : IPCB_Board;         //Circuit Board Object Variable
   X                  : Tcoord;             //X coordinate - component selection point
   Y                  : Tcoord;             //Y coordinate - component selection point

   ComponentIterator  : IPCB_BoardIterator; //Component iterator
   Comp               : IPCB_Component;     //Component obtained through iterator
   CompItog           : IPCB_Component;     //Component found
   X1                 : Tcoord;             //X coordinate - The current component in the iterator
   Y1                 : Tcoord;             //Y coordinate - The current component in the iterator
   d                  : real;               //The distance to the component.
   dOld               : real;               //The previous distance to the component.

   lyrMehPairs        : IPCB_MechanicalLayerPairs; // Variable containing pairs of mechanical layers
   LayerM1            : Tlayer;                    // First mechanical layer
   LayerM2            : Tlayer;                    // Second mechanical layer
   CurrentLayer       : integer;            //Current layer index

   Area               : Tcoord;


Begin  // StartScript
       CurrentLayer  := eTopLayer;  //Initialization of the current layer variable by default

       Board := PCBServer.GetCurrentPCBBoard;

       if Board = nil then // check for open circuit board
          Begin
               ShowError('Open Board!');
               Exit;
          end;

       Area := Board.SnapGridSize*2.1;
       Board.ChooseLocation(X,Y,'Choose Component'); //Select a component on the board

       //*******Determination of the working side of the board*****
       if (board.CurrentLayer = eTopLayer) | (board.CurrentLayer = eTopPaste) | (board.CurrentLayer =eTopOverlay)
       then CurrentLayer  := eTopLayer;
       if (board.CurrentLayer = eBottomLayer) | (board.CurrentLayer = eBottomPaste) | (board.CurrentLayer =eBottomOverlay)
       then CurrentLayer  := eBottomLayer;
       lyrMehPairs := Board.MechanicalPairs;
       for LayerM1 := 1 to 32 do
           for LayerM2 := LayerM1 to 32 do
           begin
                if lyrMehPairs.PairDefined(PCBServer.LayerUtils.MechanicalLayer(LayerM1),PCBServer.LayerUtils.MechanicalLayer(LayerM2)) then
                begin
                     if Board.CurrentLayer =  PCBServer.LayerUtils.MechanicalLayer(LayerM1) then CurrentLayer  := eTopLayer;
                     if Board.CurrentLayer =  PCBServer.LayerUtils.MechanicalLayer(LayerM2) then CurrentLayer  := eBottomLayer;
                end;
           end;
       //******The end of the definition of the working side of the board*****

       ComponentIterator := Board.BoardIterator_Create; // Enumeration of all objects on the board
       ComponentIterator.AddFilter_ObjectSet(MkSet(eComponentObject));  //filter filtering only components.
       ComponentIterator.AddFilter_LayerSet(MkSet(CurrentLayer));        //Filter for selecting components on a specific layer.
       ComponentIterator.AddFilter_Method(eProcessAll);                 //Brute force method.

       Comp := ComponentIterator.FirstPCBObject;                        //Getting the first component.
       CompItog := Comp;                                                //Initialization of the first value.
       X1 := Comp.x;
       Y1 := Comp.y;
       dOld := sqrt(sqr(X1-X)+sqr(Y1-Y));
       While (Comp <> Nil) Do  //Component Iteration Cycle
       Begin
            X1:=Comp.x;
            Y1:=Comp.y;
            d := sqrt(sqr(X1-X)+sqr(Y1-Y));
            if d <= Area then   // Optimization of the script with the exact indication of the component.
            begin
               CompItog := Comp;
               Break;
            end;

            if d < dOld then   // Component search by principle - closest to point
            begin
                 CompItog := Comp;
                 dOld := d;
            end;
        Comp := ComponentIterator.NextPCBObject;
       end;
       Board.BoardIterator_Destroy(ComponentIterator);

      If CompItog <> Nil then CompItog.Selected := true;
      Board.ViewManager_FullUpdate;
End;
