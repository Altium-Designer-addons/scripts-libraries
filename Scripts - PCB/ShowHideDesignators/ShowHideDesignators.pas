{..............................................................................}
{ Summary   This script allows user to easily show or hide Designators and     }
{           Comments.                                                          }
{                                                                              }
{ Created by:    Ran Shah  ar                                                  }
{ Changelog:                                                                   }
{   2013-02-14   Cyril Andreatta                                               }
{                Added support to show/hide comments                           }
{..............................................................................}

{***
 * global variables for iterating over components
}
var
    ComponentIteratorHandle : IPCB_BoardIterator;
    Component               : IPCB_Component;

{***
 * procedure to show or hide names and comments for all components
}
procedure ShowHide (nameon : boolean, commenton : boolean);
begin
    Component := ComponentIteratorHandle.FirstPCBObject;
    While (Component <> Nil) Do
    Begin
        Component.NameOn := nameon;
        Component.CommentOn := commenton;

       Component := ComponentIteratorHandle.NextPCBObject;
    end;

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
end;

{***
 * actions performed on button press
}
procedure TFormShowHideDesignators.ButtonApplyClick(Sender: TObject);
begin
   ShowHide (chkDesignator.Checked, chkComment.Checked);
end;

procedure TFormShowHideDesignators.ButtonCloseClick(Sender: TObject);
begin
    close;
end;

{***
 * Create BoardIterator when form is created. BoardIterator is active as long
 * as the script is running.
}
procedure TFormShowHideDesignators.FormShowHideDesignatorsCreate(Sender: TObject);
begin
    ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
    ComponentIteratorHandle.AddFilter_LayerSet(AllLayers);
    ComponentIteratorHandle.AddFilter_Method(eProcessAll);

    // take first found component as reference and display it's state in checkbox
    Component := ComponentIteratorHandle.FirstPCBObject;
    chkComment.Checked := Component.CommentOn;
    chkDesignator.Checked := Component.NameOn;
end;

{***
 * destroy global BoardIterator on FormClose
}
procedure TFormShowHideDesignators.FormShowHideClose(Sender: TObject; var Action: TCloseAction);
begin
    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);
end;
