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
{           There are extra functions you can use for pin swapping, and by     }
{           and by doing it connector setup like in examples above you can     }
{           have automated pin swapping on connectors on one PCB automaically  }
{           update to second PCB.                                              }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}






{..............................................................................}
{                                                                              }
{     RecompileProject - This procedure recompiles the project. Used a lot.    }
{                                                                              }
{..............................................................................}
Procedure RecompileProject(Confirm : Bool);
begin
   // Recompile
   if Confirm then
   begin
      ResetParameters;
      AddStringParameter('Action','Compile');
      AddStringParameter('ObjectKind','Project');
      RunProcess('WorkspaceManager:Compile');
   end;
end;


Procedure UnMasksAll(Confirm : bool);
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

begin
   if Confirm = False then exit;

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
                  if (IsPCBDocBlanket = 1) then
                  begin
                     try
                        // we will create iterator
                        MaskIterator := Sheet.SchIterator_Create;
                        // next line is stupid, but it is iterator for blanket objects
                        MaskIterator.AddFilter_ObjectSet(MkSet(eCompileMask));

                        CompileMask := MaskIterator.FirstSchObject;
                        While (CompileMask <> nil) do
                        begin

                           if (not CompileMask.Collapsed) then
                              if (CompileMask.Location.X = Rectangle.Left)  and (CompileMask.Location.Y = Rectangle.Bottom) and
                                 (CompileMask.Corner.X   = Rectangle.Right) and (CompileMask.Corner.Y = Rectangle.Top) then
                                 begin
                                    // We need to delete this compile mask

                                    Inc(i);
                                    ObjectsToDelete.AddObject(IntToStr(i),CompileMask);
                                 end;
                           CompileMask := MaskIterator.NextSchObject;
                        end;
                     finally
                        Sheet.SchIterator_Destroy(MaskIterator);
                     end;
               end;

               end;
               Blanket := BlanketIterator.NextSchObject;
            end;
         finally
            Sheet.SchIterator_Destroy(BlanketIterator);
         end;
      end;
   end;

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
   RecompileProject(True);
end;




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
{     Index - When updating from SCH to PCB, this is the reference number of   }
{             PCB document that is currently being updated.                    }
{           - 0 - If there is update from PCB (Update SCH or Import Changes    }
{             from SCH), this is 0.                                            }
{           - -1 - If we do Component Links from PCB, this is -1.              }
{           - -2 - If we do Pin Swapping    from PCB, this is -2.              }
{           - -3 - If we want to Mask PCB, this is -3.                         }
{                                                                              }
{                                                                              }
{     PcbName - The name of PCB Document that is being updated.                }
{                                                                              }
{     Destination - This is used to fill parameter used while "Update PCB". It }
{                   is "UpdateOther" anways except:                            }
{                   - on PCB import changes: it is "UpdateMe"                  }
{                   - on Pin swapping and MaskPCB it is '' (empty string)      }
{                                                                              }
{                                                                              }
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

                                     if (IsPCBDocBlanket = 1) and ((AnsiUpperCase(ParameterText) = AnsiUpperCase(PCBName)) or
                                        ((AnsiUpperCase(ParameterText) + '.PCBDOC') = AnsiUpperCase(PCBName))) then
                                        UnMaskedBlanket := 1;

                                     // We need to check weather it contains

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
   RecompileProject(True);

   // So now we have Compile mask over everithing. We execute function.
   ResetParameters;

   // If index is -2 -> do pin swapping
   if indeks = -2 then
   begin
      RunProcess('PinSwapper:RunManualPinSwapper');
   end

   // If index is -1 -> do "Project >> Component links" from PCB
   else if indeks = -1 then
   begin
      // in this case Destination = 'ComponentLinking'
      AddStringParameter('ObjectKind','Project');
      AddStringParameter('Action',Destination);
      RunProcess('WorkspaceManager:DocumentOptions');
   end

   // If index is 0 -> do update on PCB
   else if indeks = 0 then
   begin
      // Destination can be:
      //    - 'UpdateOther' - used in PCB -> Update Schematic
      //    - 'UpdateMe'    - used in PCB -> Import changes from SCH
      AddStringParameter('ObjectKind','Project');
      AddStringParameter('Action',Destination);
      RunProcess('WorkspaceManager:Compare');
   end

   // if indeks > 0 ->do Update PCB from schematic
   else if indeks > 0 then
   begin
      // in this case Destination = 'UpdateOther'
      // and indeks represents ordinal number of PCB document that is being updated
      AddStringParameter('ObjectKind','Project');
      AddStringParameter('Action',Destination);
      AddStringParameter('Index',IntToStr(Indeks));
      RunProcess('WorkspaceManager:Compare');
   end;

   // Now delete all compile masks
   if Indeks <> -3 then
   begin
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
   end;
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
       RecompileProject(True);

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
{  SCH_UpdateSinglePcbDocument - you call this procedure if you want to update }
{                                Single PCB document from current (focused) SCH}
{                                This procedure opens up a form in which you   }
{                                choose PCB Document to update.                }
{                                                                              }
{..............................................................................}
Procedure SCH_UpdateSinglePcbDocument;
Var
   PcbProject : IProject;
   Document   : IDocument;
   FileName   : String;
   i          : Integer;
begin

   if (TestsOnCreate('SCH') = False) then exit;

   PcbProject := GetWorkspace.DM_FocusedProject;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);


      if Document.DM_DocumentKind = 'PCB' then
      begin
         FileName := Document.DM_FileName;
         FileName := ChangeFileExt(FileName,'.PcbDoc');

         if Length(FileName) >= 12 then
            Delete(FileName, 1, Length(FileName) - 12);

         if (AnsiUpperCase(FileName) <> 'PANEL.PCBDOC') then
            ComboBoxDocuments.Items.Add(Document.DM_FileName);
      end;
   end;

   ComboBoxDocuments.ItemIndex := 0;
   FormMultiPCBProject.ShowModal;
end;



{..............................................................................}
{                                                                              }
{  SCH_UpdateAllPcbDocuments - you call this procedure if you want to update   }
{                              All PCB documents from current (focused) SCH.   }
{                                                                              }
{..............................................................................}
Procedure SCH_UpdateAllPcbDocuments;
Var
   PcbProject : IProject;
   Document   : IDocument;
   FileName   : String;
   CurrentSCH : IServerDocument;
   i          : Integer;
   PcbDocs    : TStringList;
begin

   if (TestsOnCreate('SCH') = False) then exit;

   UnMasksAll(True);

   PcbProject := GetWorkspace.DM_FocusedProject;
   PcbDocs    := TStringList.Create;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);

      if Document.DM_DocumentKind = 'PCB' then
      begin
         FileName := Document.DM_FileName;
         FileName := ChangeFileExt(FileName,'.PcbDoc');

         if Length(FileName) >= 12 then
            Delete(FileName, 1, Length(FileName) - 12);

         if (AnsiUpperCase(FileName) <> 'PANEL.PCBDOC') then
            PcbDocs.Add(Document.DM_FileName);
      end;
   end;

   CurrentSCH := Client.GetDocumentByPath(GetWorkspace.DM_FocusedDocument.DM_FullPath);

   for i := 0 to PcbDocs.Count - 1 do
   begin
      UpdateOther(i + 1, PcbDocs.Get(i), 'UpdateOther');
      Client.ShowDocument(CurrentSCH);
   end;

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_UpdateSchematic - you call this procedure if you want to update         }
{                        schematic from current (focused) PCB document.        }
{                                                                              }
{..............................................................................}
Procedure PCB_UpdateSCH;
var
   PinSwapByPin : Boolean;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   UnMasksAll(True);

   // Main function call - never comment next line
   UpdateOther(0, GetWorkspace.DM_FocusedDocument.DM_FileName, 'UpdateOther');

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_ImportChangesFromSch - you call this procedure if you want to update    }
{                             current (focused) PCB document.                  }
{                                                                              }
{..............................................................................}
Procedure PCB_ImportChangesFromSch;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   UnMasksAll(True);

   UpdateOther(0, GetWorkspace.DM_FocusedDocument.DM_FileName, 'UpdateMe');

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_ComponentLinks - you call this procedure if you want to set "Component  }
{                       Links" for current (focused) PCB document.             }
{                                                                              }
{..............................................................................}
Procedure PCB_ComponentLinks;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   UnMasksAll(True);

   UpdateOther(-1, GetWorkspace.DM_FocusedDocument.DM_FileName, 'ComponentLinking');

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_PinSwapping - you call this to do manual pin swapping in case you want  }
{                    to have "swap schematic pins" always disabled.            }
{                                                                              }
{..............................................................................}
Procedure PCB_PinSwapping;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   UnMasksAll(True);

   UpdateOther(-2, GetWorkspace.DM_FocusedDocument.DM_FileName, '');

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_UpdateSchematic - you call this procedure if you want to update         }
{                        schematic from current (focused) PCB document.        }
{                                                                              }
{..............................................................................}
Procedure PCB_UpdateSCHandOtherPCBs;
var
   PcbProject  : IProject;
   Document    : IDocument;
   FileName    : String;
   CurrentSCH  : IServerDocument;
   i           : Integer;
   PcbDocs     : TStringList;
   CurrPCB     : String;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   PCB_UpdateSCH;

   PcbProject  := GetWorkspace.DM_FocusedProject;
   PcbDocs     := TStringList.Create;
   CurrPCB     := GetWorkspace.DM_FocusedDocument.DM_FileName;

   CurrentSCH := nil;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);

      if Document.DM_DocumentKind = 'PCB' then
      begin
         FileName := Document.DM_FileName;
         FileName := ChangeFileExt(FileName,'.PcbDoc');

         if Length(FileName) >= 12 then
            Delete(FileName, 1, Length(FileName) - 12);

         if (AnsiUpperCase(FileName) <> 'PANEL.PCBDOC') then
            PcbDocs.Add(Document.DM_FileName);
      end;

      if Document.DM_DocumentKind = 'SCH' then
      begin
         if CurrentSCH = nil then
            CurrentSCH := Client.GetDocumentByPath(Document.DM_FullPath);

         if Document.DM_FullPath = PcbProject.DM_TopLevelLogicalDocument.DM_FullPath then
            CurrentSCH := Client.GetDocumentByPath(Document.DM_FullPath);
      end;
   end;

   Client.ShowDocument(CurrentSCH);

   for i := 0 to PcbDocs.Count - 1 do
   begin
      if CurrPCB <> PCBDocs.Get(i) then
      begin
         UpdateOther(i + 1, PcbDocs.Get(i), 'UpdateOther');
         Client.ShowDocument(CurrentSCH);
      end;
   end;

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  PCB_MaskThisPCB - you call this procedure if you want to mask out in SCH    }
{                    everything that does not belong to currently focused PCB. }
{                                                                              }
{..............................................................................}
Procedure PCB_MaskThisPCB;
begin
   if (TestsOnCreate('PCB') = False) then exit;

   UnMasksAll(True);

   UpdateOther(-3, GetWorkspace.DM_FocusedDocument.DM_FileName, '');

   // Recompile
   RecompileProject(True);
end;



{..............................................................................}
{                                                                              }
{  SCH_MaskPCB - you call this procedure if you want to mask out in SCH        }
{                everything that does not belong to PCB selected in the form.  }
{                                                                              }
{..............................................................................}
Procedure SCH_MaskPCB;
Var
   PcbProject : IProject;
   Document   : IDocument;
   FileName   : String;
   i          : Integer;
begin

   if (TestsOnCreate('SCH') = False) then exit;

   RadioButtonSingle.Caption := 'Mask PCB';
   RadioButtonAll.Caption    := 'Unmask All';
   ButtonUpdate.Caption      := 'Mask';

   PcbProject := GetWorkspace.DM_FocusedProject;

   for i := 0 to PcbProject.DM_LogicalDocumentCount - 1 do
   begin
      Document := PcbProject.DM_LogicalDocuments(i);

      if Document.DM_DocumentKind = 'PCB' then
      begin
         FileName := Document.DM_FileName;
         FileName := ChangeFileExt(FileName,'.PcbDoc');

         if Length(FileName) >= 12 then
            Delete(FileName, 1, Length(FileName) - 12);

         if (AnsiUpperCase(FileName) <> 'PANEL.PCBDOC') then
            ComboBoxDocuments.Items.Add(Document.DM_FileName);
      end;
   end;

   ComboBoxDocuments.ItemIndex := 0;
   FormMultiPCBProject.ShowModal;
end;



{..............................................................................}
{                                                                              }
{  UnMaskAll - you call this procedure if you want to unmask everything in     }
{              Project.                                                        }
{                                                                              }
{..............................................................................}
Procedure UnMaskAll;
begin
   if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'PCB' then
   begin
      if (TestsOnCreate('PCB') = False) then exit;
   end
   else if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCH' then
   begin
      if (TestsOnCreate('SCH') = False) then exit;
   end
   else
   begin
      ShowMessage('Current document type not supported');
      exit;
   end;

   UnMasksAll(True);

   // Recompile
   RecompileProject(True);
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
   // Since the form is called in "SCH_UpdateSinglePcbDocument" and "SCH_MaskPCB"
   // we need to figure wout which is this case

   UnMasksAll(True);

   if RadioButtonSingle.Caption = 'Mask PCB' then
   begin
      if RadioButtonSingle.Checked then
         UpdateOther(-3, ComboBoxDocuments.Text, '');
   end
   else
   begin
      if RadioButtonSingle.Checked then
         UpdateOther(ComboBoxDocuments.ItemIndex + 1, ComboBoxDocuments.Text, 'UpdateOther')
      else
         SCH_UpdateAllPcbDocuments;
   end;

   // Recompile
   RecompileProject(True);

   Close;
end;

procedure TFormMultiPCBProject.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;
