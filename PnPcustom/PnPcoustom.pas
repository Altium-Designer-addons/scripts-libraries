{==============================================================================}
{ --------------------- Pick & Place custom output ----------------------------}
{------------------------------------------------------------------------------}
{-                                                                            -}
{-  Pick & Place data are exported only for SMD components (components        -}
{-  without hole in any pad).                                                 -}
{-  Units - mm                                                                -}
{-  Data separator - horizontal TAB                                           -}
{-  Out file - Filename of processed PCB with .pic extension                  -}
{-                                                                            -}
{------------------------------------------------------------------------------}
{==============================================================================}

Procedure PickAndPlaceOutput;
var
    Board                          : IPCB_Board; // document board object
    Component                      : IPCB_Component; // component object
    Iterator                       : IPCB_BoardIterator;
    ComponentIterator              : IPCB_GroupIterator;
    Pad                            : IPCB_Pad;
    SMDcomponent                   : Boolean;
    BoardUnits                     : String;
                                     // Current unit string mm/mils
    PnPout                         : TStringList;
    Count                          : Integer;
    FileName                       : TString;
    Document                       : IServerDocument;
    X, Y, Rotation, Layer          : TString;

Begin
 Board := PCBServer.GetCurrentPCBBoard;
    If Not Assigned(Board) Then  // check of active document
       Begin
          ShowMessage('The Current Document is not a PCB Document.');
       Exit;
    End;


    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Count := 0;
    PnPout   := TStringList.Create;
    Component := Iterator.FirstPCBObject;
    PnPout.Add('Designator'  + #9 + 'Footprint' + #9 + 'Xref' + #9 + 'Yref'  + #9 + 'Rotation' + #9 + 'Layer' + #9 + 'Description');

    While (Component <> Nil) Do
    Begin
        // Test if the component is SMD (all pads without hole)
        ComponentIterator := Component.GroupIterator_Create;
        ComponentIterator.AddFilter_ObjectSet(MkSet(ePadObject));
        Pad := ComponentIterator.FirstPCBObject;
        While (Pad <> Nil) Do
        Begin
          //
            SMDcomponent := True;
            If Pad.Layer = eMultiLayer Then
               Begin
                    SMDcomponent := False;
                    Break;
               End;
            Pad := ComponentIterator.NextPCBObject;
        End;
        // Print Pick&Place data of SMD components to file
        If (SMDcomponent = True) Then
           Begin
                Inc(Count);
                X := FloatToStr(CoordToMMs(Component.X - Board.XOrigin));
                Y := FloatToStr(CoordToMMs(Component.Y - Board.YOrigin));
                Rotation := IntToStr(Component.Rotation);
                If (Component.Layer = eTopLayer) Then
                    Layer := 'TopLayer'
                Else
                    Layer := 'BottomLayer';
                PnPout.Add(Component.SourceDesignator  + #9 + Component.SourceLibReference + #9 + X + #9 + Y  + #9 + Rotation + #9 + Layer + #9 + Component.FootprintDescription);
           End;
        Component := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // Display the Pick&Place report
    FileName := ChangeFileExt(Board.FileName,'.pic');
    PnPout.SaveToFile(Filename);
    PnPout.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);

    ShowMessage(IntToStr(Count) + ' were exported to Pick and Place file:' + #13 + Board.FileName + '.pic');
End;



