{..............................................................................}
{ Summary Pin Package Lengths Importer script.
{         The ImportPinPackLenForm is the main form.                           }
{         You need a Pin Package Length Data CSV file to import                }
{         onto a Component symbol                                              }
{                                                                              }
{ To use the script:                                                           }
{  1/ Select the component in schematic that is going to be updated            }
{  2/ Execute the ImportPins procedure and the Pins Importer dialog appears    }
{  3/ Click on browse button to load in the CSV file of schematic pins data.   }
{  4/ Click Run Button                                                         }
{  5/ Check lengths in schematic:                                              }
{            - Right click on schematic symbol and Select Properties           }
{            - In the new window click 'Edit Pins...' in the botttom left      }
{            - This should allow you to review the pin package lengths         }
{  Note: Built on Altium 16.1 so menu options and locations may have changed   }
{..............................................................................}

{..............................................................................}
Interface
Type
  TImportPinsForm = class(TForm)
    ButtonBrowse        : TButton;
    ButtonRun           : TButton;
    OpenDialog          : TOpenDialog;
    Edit                : TEdit;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
  End;

Var
    ImportPinsForm : TImportPinsForm;
    SchDoc         : ISch_Document;
{..............................................................................}

{..............................................................................}
Procedure TImportPinsForm.ButtonBrowseClick(Sender: TObject);
Begin
    If OpenDialog.Execute Then Edit.Text := OpenDialog.FileName;
End;
{..............................................................................}

{..............................................................................}
// Iterate through pins on the selected schematic symbol, check if they match
// the selected pin and if they do update the length
Function UpdatePinLength(CSVBall: TPCBString, CSVLenStr: String): Boolean;
Var
     CurrentSch       : ISch_Sheet             ;
     Iterator         : ISch_Iterator          ;
     PIterator        : ISch_Iterator          ;
     AComponent       : ISch_Component         ;
     Pin              : ISch_Pin               ;
     CompDes          : TPCBString             ;
     CompBall         : TPCBString             ;
     CSVLenCoord      : Integer                ;
Begin
     Result := False;

     // Check if schematic server exists or not.
     If SchServer = Nil Then Exit;

     // Obtain the current schematic document interface.
     CurrentSch := SchServer.GetCurrentSchDocument;
     If CurrentSch = Nil Then Exit;

     // Look for components only
     Iterator := CurrentSch.SchIterator_Create;
     Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

     Try
         AComponent := Iterator.FirstSchObject;
         While AComponent <> Nil Do
         Begin
             CompDes := AComponent.Designator.Text;
             If AComponent.Selection Then
                 Try
                     PIterator := AComponent.SchIterator_Create;
                     PIterator.AddFilter_ObjectSet(MkSet(ePin));

                     Pin := PIterator.FirstSchObject;
                     While Pin <> Nil Do
                     Begin
                         CompBall := Pin.Designator;

                         If CompBall = CSVBall Then
                            Begin
                                 StringToCoordUnit(CSVLenStr, CSVLenCoord, eImperial);

                                 pin.PinPackageLength := CSVLenCoord; // Set Pin Length
                                 Result := True;
                            End;

                         Pin := PIterator.NextSchObject;
                     End;
                 Finally
                     AComponent.SchIterator_Destroy(PIterator);
                 End;

             AComponent := Iterator.NextSchObject;
         End;
     Finally
         CurrentSch.SchIterator_Destroy(Iterator);
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure TImportPinsForm.ButtonRunClick(Sender: TObject);
Const
    COL_CNT = 2;
    DESIGNATOR_COL = 0;
    LENGTH_COL = 1;
Var
    ValuesCount      : Integer       ;
    i, k             : Integer       ;
    csv_row, char_cnt : Integer;
    TxtFieldValue    : String        ;
    StrList          : TStringList   ;
    Location         : TLocation     ;
    PinLocX, PinLocY : Integer       ;
    PinLocMapped     : Boolean       ;
    CSVBall          : String        ;
    CSVLenStr        : String        ;
Begin
    // check if file exists or not
    If Not(FileExists(Edit.Text)) or (Edit.Text = '') Then
    Begin
        ShowWarning('The Pin Data CSV format file doesnt exist!');
        Exit;
    End;

    StrList := TStringList.Create;
    Try
        StrList.LoadFromFile(Edit.Text); // CSV with pin/package lengths

        // Iterate CSV Rows
        For csv_row := 1 To StrList.Count-1 Do
        Begin
            For i := 0 To COL_CNT-1 Do
            Begin
                TxtFieldValue := '';
                ValuesCount   := 1 ;
                k             := 1 ;
                // For each character in csv row
                For char_cnt := 1 To Length(StrList[csv_row]) Do
                    // If delimeter character reached, else skip
                    If (Copy(StrList[csv_row], char_cnt, 1) = ',') Then
                    Begin
                        If ValuesCount = i+1 Then
                        Begin
                            TxtFieldValue := Copy(StrList[csv_row], k, char_cnt-k);
                            k := char_cnt+1;
                            Inc(ValuesCount);
                            Break;
                        End;
                        k := char_cnt+1;
                        Inc(ValuesCount);
                    End;
                If ValuesCount = i+1 Then TxtFieldValue := Copy(StrList[csv_row], k, Length(StrList[csv_row])+1-k);

                If i = DESIGNATOR_COL Then
                    Begin
                        CSVBall := TxtFieldValue;
                    End
                Else If i = LENGTH_COL Then
                    Begin
                        CSVLenStr := TxtFieldValue;
                    End
            End;
            // Check csv against component pin length
            UpdatePinLength(CSVBall, CSVLenStr);
        End;
    Finally
        StrList.Free;
    End;

    SchDoc.GraphicallyInvalidate;

    ResetParameters;
    AddStringParameter('Action', 'All');
    RunProcess('Sch:Zoom');

    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure RunImportPins;
Begin
    If SchServer = Nil Then Exit;
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    // check if it is a schematic library document
    If Not SchDoc.IsLibrary Then Exit;

    ImportPinsForm.ShowModal;
End;
{..............................................................................}

{..............................................................................}
