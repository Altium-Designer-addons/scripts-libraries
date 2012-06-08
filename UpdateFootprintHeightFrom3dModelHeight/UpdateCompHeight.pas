{..............................................................................}
{                                                                              }
{ Iterate and find Selected Objects for all footprints within the current      }
{ library, one footprint at a time. Get the overall height from 3d bodies if   }
{ they exist and update the footprint height if it is less than                }
{ the body height.                                                             }
{ Created by: Colby Siemer                                                     }
{ Modified by Tony Chilco to update comp height.                               }
{..............................................................................}
Procedure GetCompHeightFromStepHeight;
Var
    CurrentLib        : IPCB_Library;

    FootprintIterator : IPCB_LibraryIterator;
    Iterator          : IPCB_GroupIterator;

    Footprint,CurrentFootprint         : IPCB_LibComponent;
    Text              : IPCB_Text;
    I                 : Integer;
    FP                : String;
    MyModel           : IPCB_ComponentBody;
    HowMany           : String;
    HowManyInt        : Integer;
    ButtonSelected    : Integer;
    AView           : IServerDocumentView;
    AServerDocument : IServerDocument;

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
    CurrentFootprint:= CurrentLib.CurrentComponent;

    // For each page of library is a footprint
    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;


       Try
        // Step through footprints until end of library is reached
        // A footprint is a IPCB_LibComponent inherited from
        // IPCB_Group which is a container object that stores primitives.
        Footprint := FootprintIterator.FirstPCBObject;

                  While Footprint <> Nil Do
                  Begin //Iterate through each footprint finding 3d Bodies
                       Iterator := Footprint.GroupIterator_Create;

                       // Use a line such as the following if you would like to limit the type of items you are allowed to delete, in the example line below,
                       // this would limit the script to Component Body Objects
                       Iterator.Addfilter_ObjectSet(MkSet(eComponentBodyObject));
                       MyModel := Iterator.FirstPCBObject;
                       While MyModel <> Nil Do
                             Begin
                             If MyModel.OverallHeight<>0 Then
                               If  MyModel.OverallHeight > Footprint.Height Then Begin
                                 Footprint.Height:= MyModel.OverallHeight;
                                 Inc(HowManyInt);
                               End;
                             MyModel := Iterator.NextPCBObject;
                             End;

                       Footprint.GroupIterator_Destroy(Iterator);
                       Footprint := FootprintIterator.NextPCBObject;
                  End;


    Finally

    If HowManyInt>0 Then Begin
      // Grab the current document view using the Client's Interface.
      AView := Client.GetCurrentView;

      // Grab the server document which stores views by extracting the ownerdocument field.
      AServerDocument := AView.OwnerDocument;

      // Set the document dirty.
      AServerDocument.Modified := True;
    End;

      HowMany := IntToStr(HowManyInt);
      Showmessage('Updated ' + HowMany + ' Comp Heights');
      CurrentLib.CurrentComponent := CurrentFootprint;
      CurrentLib.Board.ViewManager_FullUpdate;
      CurrentLib.LibraryIterator_Destroy(FootprintIterator);
      //Set to first component, refresh screen and destroy final iterators

    End;





{..............................................................................}

{..............................................................................}
End;
