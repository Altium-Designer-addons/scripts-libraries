{..............................................................................}
{                                                                              }
{ Iterate and find Selected Objects for all footprints within the current      }
{ library, one footprint at a time. Delete selected objects for each footprint,}
{ and move to next footprint in library.                                       }
{                                                                              }
{ Created by: Colby Siemer                                                     }
{                                                                              }
{..............................................................................}
Procedure DeleteSelectedItemsFromAllFootprints;
Var
    CurrentLib        : IPCB_Library;
    TempPCBLibComp    : IPCB_LibComponent;

    FootprintIterator : IPCB_LibraryIterator;
    Iterator          : IPCB_GroupIterator;

    Footprint         : IPCB_LibComponent;
    Text              : IPCB_Text;
    DeleteList        : TInterfaceList;
    I                 : Integer;
    FP                : String;
    MyModel           : IPCB_ComponentBody;
    HowMany           : String;
    HowManyInt        : Integer;
    ButtonSelected    : Integer;

Begin
     HowManyInt :=   0;
     CurrentLib := PCBServer.GetCurrentPCBLibrary;
     If CurrentLib = Nil Then
        Begin
             ShowMessage('This is not a PCB Library document');
             Exit;
        End;
        buttonSelected := MessageDlg('!!!This operation CANNOT be undone, proceed with caution!!!', mtwarning, mbOKCancel, 0);

  // Verify user wants to continue, if cancel pressed, exit script.  If OK, continue
  if buttonSelected = mrCancel then
     Begin
     ShowMessage('Cancel pressed Exiting');
     Exit;
     End;


    // For each page of library is a footprint
    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;
    FP := '___TemporaryComponent__DeleteMeWhenDone___';
    TempPCBLibComp := PCBServer.CreatePCBLibComp;
    TempPcbLibComp.Name := FP;
    // Create a temporary component to hold focus while we delete items
    CurrentLib.RegisterComponent(TempPCBLibComp);
    CurrentLib.CurrentComponent := TempPcbLibComp;
    CurrentLib.Board.ViewManager_FullUpdate;


       Try
        // Step through footprints until end of library is reached
        // A footprint is a IPCB_LibComponent inherited from
        // IPCB_Group which is a container object that stores primitives.
        Footprint := FootprintIterator.FirstPCBObject;
                  While Footprint <> Nil Do
                  Begin //Iterate through each footprint finding Selected objects and building a list
                       Iterator := Footprint.GroupIterator_Create;
                       // Use a line such as the following if you would like to limit the type of items you are allowed to delete, in the example line below,
                       // this would limit the script to Component Body Objects
                       // Iterator.Addfilter_ObjectSet(MkSet(eComponentBodyObject));
                       MyModel := Iterator.FirstPCBObject;
                       DeleteList := TInterfaceList.Create;
                       While MyModel <> Nil Do
                             Begin
                             If MyModel.Selected = true then DeleteList.Add(MyModel);
                             MyModel := Iterator.NextPCBObject;
                             End;
                       If DeleteList.Count <> Nil Then
                       Begin //Process list and delete items from created list
                       PCBServer.PreProcess;
                       For I := 0 to DeleteList.Count - 1 Do
                           Begin
                                MyModel := DeleteList.items[i];
                                Footprint.RemovePCBObject(MyModel);
                           End;
                       PCBServer.PostProcess;
                       HowmanyInt := HowManyInt + DeleteList.Count;
                       DeleteList.Free;
                       End;
                       Footprint.GroupIterator_Destroy(Iterator);
                       Footprint := FootprintIterator.NextPCBObject;
                  End;



    Finally


      HowMany := IntToStr(HowManyInt);
      Showmessage('Deleted ' + HowMany + ' Items');
      CurrentLib.SetBoardToComponentByName(FP);
      Client.SendMessage('PCB:Zoom', 'Action=All' , 255, Client.CurrentView);
      Client.SendMessage('PCB:DeleteComponentFromLibrary',FP,255,client.CurrentView);
      //Delete Temporary Footprint
      Footprint := FootprintIterator.FirstPCBObject;
      CurrentLib.CurrentComponent := Footprint;
      CurrentLib.Board.ViewManager_FullUpdate;
      CurrentLib.LibraryIterator_Destroy(FootprintIterator);
      //Set to first component, refresh screen and destroy final iterators

    End;





{..............................................................................}

{..............................................................................}
End;
