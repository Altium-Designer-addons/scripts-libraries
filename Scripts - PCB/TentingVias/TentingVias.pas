{------------------------------------------------------------------------------}
{ Script with GUI to make all vias tenting or                                  }
{ remove tenting from all vias.                                                }
{ It does also work on selected vias only.                                     }
{                                                                              }
{ Author: cyril@andreatta.ch                                                   }
{ Version 1.1: Improved tenting function for when only some vias are selected.
          1.2: BLM Fixed display update in any view & set soldermask
                   expansion = manual to allow tenting to be set.
-------------------------------------------------------------------------------}

Var
    Board     : IPCB_Board;

{-- Function to count all vias already tented --}
Procedure CountVias(rw : Boolean = false);
Var
    Via         : IPCB_Primitive;
    PadCache    : TPadCache;
    Iterator    : IPCB_BoardIterator;
    ViaCount    : Integer;
    TopTenting  : Integer;
    BotTenting  : Integer;
    ViaSelected : Integer;

Begin
    ViaCount      := 0;
    TopTenting    := 0;
    BotTenting    := 0;
    ViaSelected   := 0;

    // retrieve the iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);
    // Search and count vias
    Via := Iterator.FirstPCBObject;
    While (Via <> Nil) Do
    Begin
        If rw = true Then
        Begin
            // make vias tented or not depending on checkbox states
            If ((chkSelected.Checked = false) or ((chkselected.Checked = true) and (Via.Selected))) Then
            Begin
                Via.BeginModify;
                PadCache := Via.Cache;
                PadCache.SolderMaskExpansionValid := eCacheManual;
                Via.SetState_Cache := PadCache;
//                Via.Setstate_PadCacheRobotFlag(true);
                Via.SetState_IsTenting_Top(chkTop.Checked);
                Via.SetState_IsTenting_Bottom(chkBottom.Checked);
                Via.EndModify;
            End;
        End
        Else
        Begin
            If Via.Selected Then
                Inc(ViaSelected);

            If Via.GetState_IsTenting_Top() Then
               Inc(TopTenting);

            If Via.GetState_IsTenting_Bottom() Then
               Inc(BotTenting);
        End;
        Inc(ViaCount);

        Board.ViewManager_GraphicallyInvalidatePrimitive(Via);

        Via := Iterator.NextPCBObject;
    End;

    Board.BoardIterator_Destroy(Iterator);

    // Display the count results on the form
    lblViaCount.Caption   := IntToStr(ViaCount);
    lblTopTenting.Caption := IntToStr(TopTenting);
    lblBotTenting.Caption := IntToStr(BotTenting);
    lblSelected.Caption   := IntToStr(ViaSelected);

    If ViaSelected = 0 Then
        chkSelected.Enabled := false;

    // Tick the checkbox if more than half of the vias are already tenting
    If TopTenting > ViaCount/2 Then chkTop.Checked := true;
    If BotTenting > ViaCount/2 Then chkBottom.Checked := true;

    // Refresh PCB screen
    Board.ViewManager_FullUpdate;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;

procedure TfrmTenting.btnCancelClick(Sender: TObject);
begin
     Close;
end;

procedure TfrmTenting.btnOKClick(Sender: TObject);
begin
    CountVias(true);
    Close;              
end;

procedure TfrmTenting.frmTentingCreate(Sender: TObject);
begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowWarning('Focused document is not a PCB !' + #13 +
                    'Tenting script will not execute.');
        Exit;
    End;

    CountVias;
end;
