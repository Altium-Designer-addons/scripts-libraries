{..............................................................................}
{ Summary   This scripts Shows/Hides connection lines on selected objects.     }
{           It works on selected primitives, components and polygons           }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


 Procedure Show;
 var
    Board : IPCB_Board;
    Prim  : IPCB_Primitive;
    Pad   : IPCB_Pad2;
    i     : Integer;
    Iter  : IPCB_GroupIterator;
 begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
       Prim := Board.SelectecObject[i];

       if Prim.InNet then
          Prim.Net.ShowNetConnects;

       if Prim.ObjectId = ePolyObject then
          if Prim.Net <> nil then
             Prim.Net.ShowNetConnects;

       if Prim.ObjectId = eComponentObject then
       begin
          Iter := Prim.GroupIterator_Create;
          Iter.AddFilter_ObjectSet(MkSet(ePadObject));

          Pad := Iter.FirstPCBObject;
          while (Pad <> nil) do
          begin
             // Adding track to hyp file
             if Pad.InNet then
                Pad.Net.ShowNetConnects;

             Pad := Iter.NextPCBObject;
          end;
          Prim.GroupIterator_Destroy(Iter);
       end;
    end;
 end;



 Procedure Hide;
 var
    Board : IPCB_Board;
    Prim  : IPCB_Primitive;
    Pad   : IPCB_Pad2;
    i     : Integer;
    iter  : IPCB_GroupIterator;
 begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
       Prim := Board.SelectecObject[i];

       if Prim.InNet then
          Prim.Net.HideNetConnects;

       if Prim.ObjectId = ePolyObject then
          if Prim.Net <> nil then
             Prim.Net.HideNetConnects;

       if Prim.ObjectId = eComponentObject then
       begin

          Iter := Prim.GroupIterator_Create;
          Iter.AddFilter_ObjectSet(MkSet(ePadObject));

          Pad := Iter.FirstPCBObject;
          while (Pad <> nil) do
          begin
             // Adding track to hyp file
             if Pad.InNet then
                Pad.Net.HideNetConnects;

             Pad := Iter.NextPCBObject;
          end;
          Prim.GroupIterator_Destroy(Iter);
       end;
    end;
 end;
