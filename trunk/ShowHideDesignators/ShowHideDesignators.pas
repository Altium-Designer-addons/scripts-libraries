{..............................................................................}
{ Summary   This script allows user to easily show or hide Designators.        }
{                                                                              }
{ Created by:    Ran Shah  ar                                                  }
{..............................................................................}

{..............................................................................}

procedure TFormShowHideDesignators.ButtonCloseClick(Sender: TObject);
begin
   close;
end;



procedure TFormShowHideDesignators.ButtonShowClick(Sender: TObject);
Var
    ComponentIteratorHandle : IPCB_BoardIterator;
    Component               : IPCB_Component;
begin
    ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIteratorHandle.AddFilter_LayerSet(AllLayers);
    ComponentIteratorHandle.AddFilter_Method(eProcessAll);

    Component := ComponentIteratorHandle.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
       Component.NameOn := True;

       Component := ComponentIteratorHandle.NextPCBObject;
    end;

    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');

    close;
end;



procedure TFormShowHideDesignators.ButtonHideClick(Sender: TObject);
Var
    ComponentIteratorHandle : IPCB_BoardIterator;
    Component               : IPCB_Component;
begin
    ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIteratorHandle.AddFilter_LayerSet(AllLayers);
    ComponentIteratorHandle.AddFilter_Method(eProcessAll);

    Component := ComponentIteratorHandle.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
       Component.NameOn := False;

       Component := ComponentIteratorHandle.NextPCBObject;
    end;

    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');

    close;
end;



Procedure Start;
begin
   if PCBServer.GetCurrentPCBBoard = nil then exit;

   FormShowHideDesignators.ShowModal;
end;
