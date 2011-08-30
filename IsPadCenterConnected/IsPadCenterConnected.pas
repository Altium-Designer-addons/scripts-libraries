Procedure IsPadCenterConnected;
Var
    Track                   : IPCB_Primitive;
    Pad                     : IPCB_Primitive;
    TrackIteratorHandle     : IPCB_BoardIterator;
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    PadIteratorHandle       : IPCB_GroupIterator;
Begin

    If PCBServer.GetCurrentPCBBoard = Nil Then Exit;

    ResetParameters;
    AddStringParameter('Scope', 'All');
    RunProcess('PCB:DeSelect');

    ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIteratorHandle.AddFilter_LayerSet(AllLayers);
    ComponentIteratorHandle.AddFilter_Method(eProcessAll);


    Component := ComponentIteratorHandle.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
        PadIteratorHandle := Component.GroupIterator_Create;
        PadIteratorHandle.AddFilter_ObjectSet(MkSet(ePadObject));

        Pad := PadIteratorHandle.FirstPCBObject;
        While (Pad <> Nil) Do
        Begin
            Pad.Selected := True;
            TrackIteratorHandle        := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
            TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));

            if Layer2String(Pad.Layer) = 'Multi Layer' then
               TrackIteratorHandle.AddFilter_LayerSet(MkSet(eTopLayer,  eMidLayer1,  eMidLayer2,
                                 eMidLayer3,  eMidLayer4,  eMidLayer5,  eMidLayer6,  eMidLayer7,
                                 eMidLayer8,  eMidLayer9,  eMidLayer10, eMidLayer11, eMidLayer12,
                                 eMidLayer13, eMidLayer14, eMidLayer15, eMidLayer16, eMidLayer17,
                                 eMidLayer18, eMidLayer19, eMidLayer20, eMidLayer21, eMidLayer22,
                                 eMidLayer23, eMidLayer24, eMidLayer25, eMidLayer26, eMidLayer27,
                                 eMidLayer28, eMidLayer29, eMidLayer30, eBottomLayer))
            else
               TrackIteratorHandle.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));

            TrackIteratorHandle.AddFilter_Method(eProcessAll);

            Track := TrackIteratorHandle.FirstPCBObject;
            While (Track <> Nil) Do
            Begin
                if (((Track.x1 = Pad.x) and (Track.y1 = Pad.y)) or ((Track.x2 = Pad.x) and (Track.y2 = Pad.y))) then
                   Pad.Selected := False;


                Track := TrackIteratorHandle.NextPCBObject;
            End;

            PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(TrackIteratorHandle);

            Pad := PadIteratorHandle.NextPCBObject;
        End;
        // fetch source designator of component
        Component.GroupIterator_Destroy(PadIteratorHandle);

        Component := ComponentIteratorHandle.NextPCBObject;
    End;
    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
End;
{..............................................................................}

{..............................................................................}



