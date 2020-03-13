{..............................................................................}
Function IsPolyRegionInvalid (P : IPCB_Primitive);
Var
    Region : IPCB_Region;
Begin
    Region := P;

    If Region.GeometricPolygon <> Nil Then
       Result := Region.GeometricPolygon.GetState_Area < 1
    Else
       Result := True;
End;
{..............................................................................}

{..............................................................................}
Function IsPrimitiveInvalid (P : IPCB_Primitive) : Boolean;
Begin
    Result := False;

    If P = Nil Then Exit;

    //do not touch primitives belonging to physical group objects
    If P.InComponent Or P.InDimension Then Exit;

    //optional - logical group objects
    //If P.InNet Then Exit
    //If P.InPolygon Then Exit;



    Case P.ObjectId Of
        eRegionObject : Result := IsPolyRegionInvalid (P);
        Else Result := False;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure DeleteInvalidObjects;
var
    CurrentPCBBoard : IPCB_Board;
    Iterator        : IPCB_BoardIterator;
    Prim            : IPCB_Primitive;
    PrimList        : IInterfaceList;
    I               : Integer;
Begin
    CurrentPCBBoard := PCBServer.GetCurrentPCBBoard;
    If CurrentPCBBoard = Nil Then Exit;

    Iterator := CurrentPCBBoard.BoardIterator_Create;
    If Iterator = Nil Then Exit;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eRegionObject));



    PrimList := TInterfaceList.Create;
    Try
       InitStatus('Searching for invalid objects on the board ...', 50000 {guesstimate value});
       Prim := Iterator.FirstPCBObject;
        While Prim <> Nil Do
        Begin
            If IsPrimitiveInvalid (Prim) Then
               PrimList.Add(Prim);
            Prim := Iterator.NextPCBObject;

            UpdateStatus(0);
        End;
    Finally
        CurrentPCBBoard.BoardIterator_Destroy(Iterator);
        FinishStatus(0);
    End;

    If PrimList.Count = 0 Then
    Begin
        ShowInfo ('Objects on the board appear valid');
    End
    Else
    Begin
        If ConfirmNoYes ('Delete ' + IntToStr(PrimList.Count) + ' invalid objects') Then
        Begin
            Try
                //make this deletion undoable;
                PCBServer.PreProcess;

                For I := 0 to PrimList.Count - 1 Do
                Begin
                    Prim := PrimList.items[i];
                    CurrentPCBBoard.RemovePCBObject(Prim);
                End;
            Finally
                PCBServer.PostProcess;
            End;

            // Refresh the PCB document.
            CurrentPCBBoard.ViewManager_FullUpdate;
            Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
        End;
    End;
End;
{..................................................................................................}
