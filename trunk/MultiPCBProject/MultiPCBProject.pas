{..............................................................................}
{ Summary   This Script enables Project to be used with multiple PCB           }
{           documents, so that project is split among multiple PCB Documents.  }
{                                                                              }
{           It uses Balnkets + Parameter Sets (Place -> Directive -> Parameter }
{           Set) which have parameter "PcbDoc", and it's value is name of the  }
{           PCB Document.                                                      }
{                                                                              }
{           If the above is satisfied, this script replaces normal "Update PCB"}
{           menu entries. This script will call "Update PCB", but before that  }
{           it will Place Compile mask over all blankets that do not point to  }
{           the PCB that is currently being updated. After "Update PCB" is     }
{           done, it will remove compile masks and re-compile the project.     }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


{..............................................................................}
{                                                                              }
{                    UpdateOther - MAIN PROCEDURE                              }
{                                                                              }
{                                                                              }
{                                                                              }
{     This procedure creates compile mask directives over blankets that point  }
{     to other PCB Documents, then calls "Update PCB", and then removes this   }
{     compile masks.                                                           }
{                                                                              }
{                                                                              }
{     for inputs it takes:                                                     }
{                                                                              }
{     Index - when updating from SCH to PCB, this is the reference number of   }
{             PCB document that is currently being updated. If there is update }
{             from PCB, this is 0.                                             }
{                                                                              }
{     PcbName - The name of PCB Document that is being updated.                }
{                                                                              }
{     Destination - This is used to fill parameter used while "Update PCB". It }
{                   is "UpdateOther" anways excepr on PCB import changes. Then }
{                   it is "UpdateMe".                                          }
{                                                                              }
{..............................................................................}
Procedure UpdateOther(indeks : Integer, PCBName : String, Destination : String);
var

   PcbProject       : IProject;
   DocNum           : Integer;
   Document         : IDocument;
   Sheet            : ISCH_Document;
   BlanketIterator  : ISCH_Iterator;
   Blanket          : ISCH_Blanket;
   ParamSetIterator : ISCH_Iterator;
   ParamSet         : ISCH_ParameterSet;
   ParamIterator    : ISCH_Iterator;
   Parameter        : ISCH_Parameter;
   Rectangle        : TCoordRect;
   CompileMask      : ISCH_CompileMask;
   ObjectsToDelete  : TStringList;
   i, flag          : Integer;
   OldMask          : Integer;
   MaskIterator     : ISCH_Iterator;
   ParameterName    : String;
   ParameterText    : String;
   IsPCBDocBlanket  : Integer;
   UnMaskedBlanket  : Integer;

begin
   PcbProject := GetWorkspace.DM_FocusedProject;

   ObjectsToDelete := TStringList.Create;
   i := 0;

   for DocNum := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(DocNum);

      // If this is SCH document
      if Document.DM_DocumentKind = 'SCH' then
      begin

         Sheet := SCHServer.GetSchDocumentByPath(Document.DM_FullPath);
         if Sheet = nil then exit;

         try
            // we will create iterator
            BlanketIterator := Sheet.SchIterator_Create;
            // next line is stupid, but it is iterator for blanket objects
            BlanketIterator.AddFilter_ObjectSet(MkSet('59'));

            Blanket := BlanketIterator.FirstSchObject;

            While (Blanket <> nil) do
            begin
               If not Blanket.Collapsed then
               begin
                  try
                     // we will create iterator
                     ParamSetIterator := Sheet.SchIterator_Create;
                     // iterator for parameter sets
                     ParamSetIterator.AddFilter_ObjectSet(MkSet(eParameterSet));

                     ParamSet := ParamSetIterator.FirstSchObject;

                     Rectangle := Blanket.BoundingRectangle;

                     IsPCBDocBlanket := 0;
                     UnMaskedBlanket := 0;

                     While (ParamSet <> nil) do
                     begin

                        if (((ParamSet.Location.X = Rectangle.Left) or (ParamSet.Location.X = Rectangle.Right))  and ((ParamSet.Location.Y <= Rectangle.Top)   and (ParamSet.Location.Y >= Rectangle.Bottom)) or
                            ((ParamSet.Location.Y = Rectangle.Top)  or (ParamSet.Location.Y = Rectangle.Bottom)) and ((ParamSet.Location.X <= Rectangle.Right) and (ParamSet.Location.X >= Rectangle.Left))) then
                            begin

                               Try
                                  // Iterate parameters within this parameter set
                                  ParamIterator := ParamSet.SchIterator_Create;
                                  ParamIterator.AddFilter_ObjectSet(MkSet(eParameter));

                                  Parameter := ParamIterator.FirstSchObject;
                                  While Parameter <> Nil Do
                                  Begin

                                     ParameterName := Parameter.Name;
                                     ParameterText := Parameter.Text;

                                     if length(ParameterName) > 6 then
                                        SetLength(ParameterName, 6);


                                     // We need to check weather this is "PcbDoc" Parameter
                                     if (AnsiUpperCase(ParameterName) = 'PCBDOC') then
                                        IsPCBDocBlanket := 1;

                                     // We need to check weather it contains
                                     if (IsPCBDocBlanket = 1) and ((AnsiUpperCase(ParameterText) = AnsiUpperCase(PCBName)) or
                                                                  ((AnsiUpperCase(ParameterText) + '.PCBDOC') = AnsiUpperCase(PCBName))) then
                                        UnMaskedBlanket := 1;

                                     Parameter := ParamIterator.NextSchObject;
                                  End;
                               Finally
                                  ParamSet.SchIterator_Destroy(ParamIterator);
                               End;
                           end;
                        ParamSet := ParamSetIterator.NextSchObject;
                     end;
                  finally
                     Sheet.SchIterator_Destroy(ParamSetIterator);
                  end;

                  // We place mask only if this is blanket with PcbDoc parameter
                  // and if it does not point to Pcb Document that is currently being updated
                  if ((IsPCBDocBlanket = 1) and (UnMaskedBlanket = 0)) then
                  begin

                     // Here we need to place compile mask over a blanket

                     // Initialize the robots in Schematic editor.
                     SchServer.ProcessControl.PreProcess(Sheet, '');

                     // First new port created.
                     CompileMask := SchServer.SchObjectFactory(eCompileMask, eCreate_GlobalCopy);

                     CompileMask.Location := Point(Rectangle.Left, Rectangle.Bottom);
                     CompileMask.Corner   := Point(Rectangle.Right, Rectangle.Top);

                     CompileMask.Collapsed := False;

                     // Add compile mask to Schematic document.
                     Sheet.RegisterSchObjectInContainer(CompileMask);
                     SchServer.RobotManager.SendMessage(Sheet.I_ObjectAddress,c_BroadCast,
                                                        SCHM_PrimitiveRegistration,CompileMask.I_ObjectAddress);

                     // Clean up the robots in Schematic editor
                     SchServer.ProcessControl.PostProcess(Sheet, '');
                     Inc(i);

                     ObjectsToDelete.AddObject(IntToStr(i),CompileMask);
                  end;
               end;

               Blanket := BlanketIterator.NextSchObject;
            end;
         finally
            Sheet.SchIterator_Destroy(BlanketIterator);
         end;
      end;
   end;

   // Recompile
   ResetParameters;
   AddStringParameter('Action','Compile');
   AddStringParameter('ObjectKind','Project');
   RunProcess('WorkspaceManager:Compile');

   // So now we have Compile mask over everithing. We just need to update document
   ResetParameters;
   AddStringParameter('ObjectKind','Project');
   AddStringParameter('Action',Destination);
   if (indeks <> 0) then
      AddStringParameter('Index',IntToStr(Indeks));
   RunProcess('WorkspaceManager:Compare');

   // Now delete all compile masks
   for DocNum := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(DocNum);

      // If this is SCH document
      if Document.DM_DocumentKind = 'SCH' then
      begin

         Sheet := SCHServer.GetSchDocumentByPath(Document.DM_FullPath);
         if Sheet = nil then exit;

         // Initialize the robots in Schematic editor.
         SchServer.ProcessControl.PreProcess(Sheet, '');

         try
            // we will create iterator
            MaskIterator := Sheet.SchIterator_Create;
            // next line is stupid, but it is iterator for blanket objects
            MaskIterator.AddFilter_ObjectSet(MkSet(eCompileMask));

            CompileMask := MaskIterator.FirstSchObject;
            While (CompileMask <> nil) do
            begin
               flag := 0;

               for i := 0 to ObjectsToDelete.Count - 1 do
               begin
                  OldMask := ObjectsToDelete.GetObject(i);

                  if OldMask = CompileMask then
                  begin

                     flag := 1;
                     CompileMask := MaskIterator.NextSchObject;

                     Sheet.RemoveSchObject(OldMask);

                     SchServer.RobotManager.SendMessage(Sheet.I_ObjectAddress,c_BroadCast,
                                                        SCHM_PrimitiveRegistration,OldMask.I_ObjectAddress);

                  end;
               end;

               if flag = 0 then
                  CompileMask := MaskIterator.NextSchObject;
            end;
         finally
            Sheet.SchIterator_Destroy(MaskIterator);
         end;
         // Clean up robots in Schematic editor.
         SchServer.ProcessControl.PostProcess(Sheet, '');

         Sheet.GraphicallyInvalidate;
      end;
   end;

   // Recompile
   ResetParameters;
   AddStringParameter('Action','Compile');
   AddStringParameter('ObjectKind','Project');
   RunProcess('WorkspaceManager:Compile');
end;



{..............................................................................}
{                                                                              }
{     TestsOnCreate - This procedure is checked on start of every script       }
{                     execution to test initial cases.                         }
{                                                                              }
{..............................................................................}
Function TestsOnCreate(CurrentDoc : String) : Boolean;
Var
   PcbProject : IProject;
   ThisDoc    : String;
begin

   Result := True;
   PcbProject := GetWorkspace.DM_FocusedProject;

   If (PcbProject = nil) then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      Result := False;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      Result := False;
   end;
  {
   if PcbProject.DM_HierarchyMode = '0' then
   Begin
      ShowMessage('Automatic Net identifier scope is not supported');
      Result := False;
   end;
  }
   if (GetWorkspace.DM_FocusedDocument.DM_DocumentKind <> CurrentDoc) then
   begin
      ShowMessage('Current Document is not a ' + CurrentDoc + ' Document');
      Result := False;
   end;

   // If we couldn't get the flattened sheet, then most likely the project has
   // not been compiled recently
   If (PCBProject.DM_DocumentFlattened = Nil) Then
   Begin
       // First try compiling the project
       ResetParameters;
       AddStringParameter( 'Action', 'Compile' );
       AddStringParameter( 'ObjectKind', 'Project' );
       RunProcess( 'WorkspaceManager:Compile' );

       // Try Again to open the flattened document
       If (PCBProject.DM_DocumentFlattened = Nil) Then
       Begin
           ShowMessage('NOTICE: Compile the Project before Running this script.');
           Result := False;;
       End;
   End;
end;



{..............................................................................}
{                                                                              }
{  SCHUpdateSinglePcbDocument - you call this procedure if you want to update  }
{                               Single PCB document from SCH.                  }
{                               This procedure opens up a form in which you    }
{                               choose PCB Document to update.                 }
{                                                                              }
{..............................................................................}
Procedure SCHUpdateSinglePcbDocument;
Var
   PcbProject : IProject;
   Document   : IDocument;
   i          : Integer;
begin

   if (TestsOnCreate('SCH') = False) then exit;

   PcbProject := GetWorkspace.DM_FocusedProject;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);

      if Document.DM_DocumentKind = 'PCB' then
         ComboBoxDocuments.Items.Add(Document.DM_FileName);
   end;

   ComboBoxDocuments.ItemIndex := 0;
   FormMultiPCBProject.ShowModal;
end;



{..............................................................................}
{                                                                              }
{  SCHUpdateAllPcbDocuments - you call this procedure if you want to update    }
{                             All PCB documents from SCH.                      }
{                                                                              }
{..............................................................................}
Procedure SCHUpdateAllPcbDocuments;
Var
   PcbProject : IProject;
   Document   : IDocument;
   i          : Integer;
   PcbDocs    : TStringList;
begin

   if (TestsOnCreate('SCH') = False) then exit;

   PcbProject := GetWorkspace.DM_FocusedProject;
   PcbDocs    := TStringList.Create;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);

      if Document.DM_DocumentKind = 'PCB' then
         PcbDocs.Add(Document.DM_FileName);
   end;

   for i := 0 to PcbDocs.Count - 1 do
      UpdateOther(i + 1, PcbDocs.Get(i), 'UpdateOther');

end;



{..............................................................................}
{                                                                              }
{  PCBUpdateSchematic - you call this procedure if you want to update          }
{                       schematic from PCB document.                           }
{                                                                              }
{..............................................................................}
Procedure PCBUpdateSchematic;
begin
   if (TestsOnCreate('PCB') = False) then exit;
   UpdateOther(0, GetWorkspace.DM_FocusedDocument.DM_FileName, 'UpdateOther');
end;



{..............................................................................}
{                                                                              }
{  PCBImportChangesFromSch - you call this procedure if you want to update     }
{                            current (focused) PCB document.                   }
{                                                                              }
{..............................................................................}
Procedure PCBImportChangesFromSch;
begin
   if (TestsOnCreate('PCB') = False) then exit;
   UpdateOther(0, GetWorkspace.DM_FocusedDocument.DM_FileName, 'UpdateMe');
end;



{..............................................................................}
{                                                                              }
{                        Some Form Procedures                                  }
{                                                                              }
{..............................................................................}
procedure TFormMultiPCBProject.RadioButtonAllClick(Sender: TObject);
begin
   if RadioButtonAll.Checked then ComboBoxDocuments.Enabled := False
   else                           ComboBoxDocuments.Enabled := True;
end;

procedure TFormMultiPCBProject.RadioButtonSingleClick(Sender: TObject);
begin
   if RadioButtonAll.Checked then ComboBoxDocuments.Enabled := False
   else                           ComboBoxDocuments.Enabled := True;
end;

procedure TFormMultiPCBProject.ButtonUpdateClick(Sender: TObject);
begin
   if RadioButtonSingle.Checked then
      UpdateOther(ComboBoxDocuments.ItemIndex + 1, ComboBoxDocuments.Text, 'UpdateOther')
   else
      SCHUpdateAllPcbDocuments;

   Close;
end;

procedure TFormMultiPCBProject.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;


Procedure Test;
var
   str1 : String;
   str2 : String;
begin
   str1 := 'Test';
   str2 := 'Test';

   if str1 = 'Test' Then ShowMessage('equal')
   else                  ShowMessage('not equal');
end;
