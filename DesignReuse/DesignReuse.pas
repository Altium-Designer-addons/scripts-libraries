{..............................................................................}
{ Summary   This Script enables true design reuse in Altium Designer           }
{                                                                              }
{           It uses parameter Sets (Place -> Directive -> Parameter Set) which }
{           have parameter name "Snippet", and their value is name of the PCB  }
{           snippet.                                                           }
{                                                                              }
{           User should have this directives placed on schematic, and he       }
{           should also have PCB snippets with the same name.                  }
{                                                                              }
{           If the above is satisfied, user should do regular PCB update first,}
{           and after that run this script. This script will then place PCB    }
{           snippet(s), remove duplicate components, update net info in        }
{           snippet and put all snippet objects in a union, for easier         }
{           movement.                                                          }
{                                                                              }
{           NOTE - since I can not access where snippets are stored from a     }
{           script, You need to fill in folders of your snippets by modifying  }
{           the script. Cntinue reading on line 35, in "WriteSnippetsFolders"  }
{           procedure.                                                         }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

// I need to get snippets folders
var
   SnippetsFolders  : TStringList;


Procedure WriteSnippetsFolders;
begin
   SnippetsFolders := TStringList.Create;

   SnippetsFolders.Add('C:\Users\Public\Documents\Altium\AD 10\Examples\Snippets Examples');     

   // I have used default Snippets folder in previous line:
   // If you use some other folders for snippets, please uncomment
   // following lines and add those folders:

// SnippetsFolders.Add('Folder-Path-1-Here');
// SnippetsFolders.Add('Folder-Path-2-Here');


end;

// I can not use following procedure to get snippets folders because I can not
// seem to access registry with Altium Scripting - probably safety precousion
(*
Procedure GetSnippetsfolders;
var
   Reg           : TRegistry;
   GUID          : String;
   TempString    : String;
   count, i      : Integer;
begin
   GUID := ClientAPI_SpecialFolder_AltiumApplicationData;
   Delete(GUID, 1, AnsiPos('Altium\AD 10', GUID) - 1);
   ShowMessage(GUID);

   SnippetsFolders := TStringList.Create;

   Reg := TRegistry.Create;
   Reg.RootKey := HKEY_CURRENT_USER;
   if Reg.OpenKeyReadOnly('Software\' + GUID + '\DesignExplorer\Preferences\Client\InstalledCollections') then
   begin
      count := Reg.ReadInteger('Count');
      for i:= 0 to count-1 do
         SnippetsFolders.Add(Reg.ReadString('Path' + IntToStr(i)));
   end;

   for i:= 0 to SnippetsFolders.Count do
      Showmessage(SnippetsFolders[i]);
end;
*)



procedure DesignReuse;
var

    Workspace         : IWorkspace;
    PCBProject        : IProject;
    ProjectName       : String;
    DocNum            : Integer;
    Document          : IDocument;
    DocCompNum        : Integer;
    DocComponent      : IComponent;
    DocPartNum        : Integer;
    DocPart           : IPart;
    DocPartFlag       : Integer;
    PCBDoc            : IServerDocument;
    ModeFlag          : Integer;


    Board             : IPCB_Board;
    PcbPosX           : Integer;
    PcbPosY           : Integer;
    BoardIterator     : IPCB_BoardIterator;
    PcbObj            : IPCB_Primitive;
    PcbComp           : IPCB_Component;
    LogicalComp       : IPCB_Component;
    PhysicalComp      : IPCB_Component;
    PCBGroupIterator  : IPCB_GroupIterator;
    SnippetDocument   : IServerDocument;
    SnippetName       : String;
    SnippetPCB        : IPCB_Board;
    SnippetPosX       : Integer;
    SnippetPosY       : Integer;
    SnippetIterator   : IPCB_BoardIterator;
    SnippetObj        : IPCB_Primitive;

    Sheet             : ISCH_Document;
    BlanketIterator   : ISCH_Iterator;
    Blanket           : ISCH_Blanket;
    ParamSetIterator  : ISCH_Iterator;
    ParamSet          : ISCH_ParameterSet;
    ParamIterator     : ISCH_Iterator;
    Parameter         : ISCH_Parameter;
    ComponentIterator : ISCH_Iterator;
    Component         : ISCH_Component;

    i                 : Integer;
    Rectangle         : TCoordRect;

begin

   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then
   Begin
      ShowMessage('Current Document is not a PCB Document');
      close;
   end;

   PCBDoc := Client.GetDocumentByPath(GetWorkspace.DM_FocusedDocument.DM_FullPath);

   if PcbDoc = nil then
   begin
      ShowMessage('Save PCB Document First');
      close;
   end;

   Workspace := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   If (PcbProject = nil) then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      close;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      close;
   end;

   // Compile project
   Document := PCBProject.DM_DocumentFlattened;

    // If we couldn't get the flattened sheet, then most likely the project has
    // not been compiled recently
    If (Document = Nil) Then
    Begin
        // First try compiling the project
        AddStringParameter( 'Action', 'Compile' );
        AddStringParameter( 'ObjectKind', 'Project' );
        RunProcess( 'WorkspaceManager:Compile' );

        // Try Again to open the flattened document
        Document := PCBProject.DM_DocumentFlattened;
        If (Document = Nil) Then
        Begin
            ShowMessage('NOTICE: Compile the Project before Running this script.');
            Close;
            Exit;
        End; // If (FlattenedDoc = Nil) Then
    End;

   // I can not access registry with Altium Scripting system, so I can not use next function
   // GetSnippetsfolders;

   // Instead, I have to use this one:
   WriteSnippetsFolders;

   Rectangle    := Board.BoardOutline.BoundingRectangle;
   PcbPosX      := Rectangle.Left;
   PcbPosY      := Rectangle.Top;

   // Need to start up Schematic Server first.
   Client.StartServer('SCH');

   // Cyckle through all documents
   for DocNum := 0 to PcbProject.DM_PhysicalDocumentCount - 1 do
   begin
      Document := PCBProject.DM_PhysicalDocuments(DocNum);

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

               try
                  // we will create iterator
                  ParamSetIterator := Sheet.SchIterator_Create;
                  // iterator for parameter sets
                  ParamSetIterator.AddFilter_ObjectSet(MkSet(eParameterSet));

                  ParamSet := ParamSetIterator.FirstSchObject;

                  Rectangle := Blanket.BoundingRectangle;

                  While (ParamSet <> nil) do
                  begin

                     if (((ParamSet.Location.X = Rectangle.Left) or (ParamSet.Location.X = Rectangle.Right))  and ((ParamSet.Location.Y < Rectangle.Top)   and (ParamSet.Location.Y > Rectangle.Bottom)) or
                         ((ParamSet.Location.Y = Rectangle.Top)  or (ParamSet.Location.Y = Rectangle.Bottom)) and ((ParamSet.Location.X < Rectangle.Right) and (ParamSet.Location.X > Rectangle.Left))) then
                         begin

                            Try
                               ParamIterator := ParamSet.SchIterator_Create;
                               ParamIterator.AddFilter_ObjectSet(MkSet(eParameter));

                               Parameter := ParamIterator.FirstSchObject;
                               While Parameter <> Nil Do
                               Begin

                                  // We need to check weather this is "Snippet" Parameter
                                  if (AnsiUpperCase(Parameter.Name) = 'SNIPPET') then
                                  begin

                                     // We will mark that we have placed snippet for this blanket

                                     // Now we need to place snippet on the PCB.
                                     // It will be placed on an empty area above the Board

                                     SnippetName := '\' + Parameter.Text;

                                     if (AnsiPos('.PCBDOC', ansiUpperCase(SnippetName)) = 0) then
                                        SnippetName := SnippetName + '.PcbDoc';

                                     SnippetPCB := nil;

                                     for i := 0 to SnippetsFolders.Count-1 do
                                     begin
                                        SnippetDocument := Client.OpenDocument('PCB', Snippetsfolders[i] + SnippetName);
                                        if SnippetDocument <> nil then break;
                                     end;

                                     if SnippetDocument = nil then
                                     begin
                                        SetLength(SnippetName, AnsiPos('.PCBDOC', ansiUpperCase(SnippetName)) - 1);
                                        Delete(SnippetName, 1, 1);
                                        ShowMessage('Could not find snippet "' + SnippetName + '"');
                                        break;
                                     end;

                                     SnippetPCB := PCBServer.GetPCBBoardByPath(Snippetsfolders[i] + SnippetName);

                                     // Next part is figuring out weather we have enough empty space above the PCB, and where
                                     Rectangle := SnippetPCB.BoundingRectangle;

                                     while (1) do
                                     begin
                                        PcbPosY := PcbPosY + MilsToCoord(500);

                                        try
                                           BoardIterator := Board.BoardIterator_Create;
                                           BoardIterator.AddFilter_Area(PcbPosX, PcbPosY, PcbPosX + (Rectangle.Right - Rectangle.Left), PcbPosY + (Rectangle.Top - Rectangle.Bottom));
                                           BoardIterator.AddFilter_ObjectSet(AllObjects);
                                           BoardIterator.AddFilter_LayerSet(AllLayers);
                                           BoardIterator.AddFilter_Method(eProcessAll);

                                           PcbObj := BoardIterator.FirstPCBObject;

                                        finally
                                           Board.BoardIterator_Destroy(BoardIterator);
                                        end;

                                        if PcbObj = nil then break;
                                     end;

                                     // Now we need to place snippet on the PCB.
                                     // Since interactive placement does not work for now, I need to place it manually.
                                     // It will work better this way, anyway

                                     // First we will run process to unselect everything
                                     ResetParameters;
                                     AddStringParameter('Scope', 'All');
                                     RunProcess('PCB:DeSelect');

                                     Try
                                        SnippetIterator := SnippetPCB.BoardIterator_Create;
                                        SnippetIterator.AddFilter_ObjectSet(AllObjects);
                                        SnippetIterator.AddFilter_LayerSet(AllLayers);
                                        SnippetIterator.AddFilter_Method(eProcessAll);

                                        SnippetObj := SnippetIterator.FirstPCBObject;

                                        While SnippetObj <> nil do
                                        begin
                                           PCBObj := nil;

                                           if SnippetObj.ObjectId = eComponentobject then
                                              PcbObj := SnippetObj.ReplicateWithChildren
                                           else if not SnippetObj.InComponent then
                                              PcbObj := SnippetObj.Replicate;

                                           if PCBObj <> nil then
                                           begin
                                              Board.BeginModify;
                                              Board.AddPCBObject(PcbObj);
                                              Board.EndModify;

                                              PcbObj.BeginModify;
                                              PcbObj.MoveByXY(PcbPosX - Rectangle.Left, PcbPosY - Rectangle.Bottom);
                                              PcbObj.Selected := True;
                                              PcbObj.EndModify;
                                           end;
                                           SnippetObj := SnippetIterator.NextPCBObject;
                                        end;
                                     finally
                                        SnippetPCB.BoardIterator_Destroy(SnippetIterator);
                                     end;

                                     PcbPosY := PcbPosY + (Rectangle.Top - Rectangle.Bottom);
                                     Client.CloseDocument(SnippetDocument);


                                     // This is where data for transfer needs to be created.

                                     // There are two modes of work - one with blankets that have
                                     // only nets and one with blankets that have components

                                     Rectangle := Blanket.BoundingRectangle;

                                     // There are two modes of work, and they are determined by the fact if there are
                                     // components inside blanket
                                     ModeFlag  := 0;


                                     for DocCompNum := 0 to Document.DM_ComponentCount - 1 do
                                     begin
                                        DocPartFlag := 0;
                                        for DocPartNum := 0 to Document.DM_Components(DocCompNum).DM_SubPartCount - 1 do
                                        begin
                                            if (DocPartFlag = 0) then
                                            begin
                                               DocPart := Document.DM_Components(DocCompNum).DM_SubParts(DocPartNum);
                                               if ((DocPart.DM_LocationX >= Rectangle.Left)   and (DocPart.DM_LocationX <= Rectangle.Right) and
                                                   (DocPart.DM_LocationY >= Rectangle.Bottom) and (DocPart.DM_LocationY <= Rectangle.Top)) then
                                                   begin
                                                      // OK, so here is the part on sch. I need it's logical designator to find snippet component,
                                                      // and I need it's UniqueID to find it's matching component on the PCB. Wish me luck.

                                                      Try
                                                         // I need to iterate through PCB components
                                                         BoardIterator := Board.BoardIterator_Create;
                                                         BoardIterator.AddFilter_ObjectSet(MkSet(eComponentObject));
                                                         BoardIterator.AddFilter_LayerSet(AllLayers);
                                                         BoardIterator.AddFilter_Method(eProcessAll);
                                                        
                                                         PcbComp := BoardIterator.FirstPCBObject;
                                                         PhysicalComp := nil;
                                                         LogicalComp  := nil;

                                                         While (PcbComp <> nil) do
                                                         begin
                                                            if PcbComp.Selected then
                                                            begin
                                                               // this is a snippet component
                                                               if PcbComp.Name.Text = DocPart.DM_LogicalDesignator then
                                                                  LogicalComp := PcbComp;
                                                            end
                                                            else
                                                            begin
                                                               // this is a PCB component;
                                                               // ShowMessage(PCBComp.SourceUniqueId + ' ' + DocPart.DM_UniqueIdPath + ' ' + DocPart.DM_UniqueId);
                                                               if PcbComp.SourceUniqueId = (DocPart.DM_UniqueIdPath + '\' + DocPart.DM_UniqueId) then
                                                                  PhysicalComp := PcbComp;
                                                            end;
                                                            PcbComp := BoardIterator.NextPCBObject;
                                                         end;
                                                      finally
                                                         Board.BoardIterator_Destroy(BoardIterator);
                                                      end;

                                                      // now we have snippet component and doc component. let us modify some ot their parameters
                                                      if ((PhysicalComp <> nil) and (LogicalComp <> nil)) then
                                                      begin
                                                         PhysicalComp.BeginModify;

                                                         PhysicalComp.x := LogicalComp.x;
                                                         PhysicalComp.y := LogicalComp.y;

                                                         PhysicalComp.Rotation := LogicalComp.Rotation;
                                                         PhysicalComp.Layer := LogicalComp.Layer;

                                                         PhysicalComp.Selected := True;

                                                         // Set some options of designator and comment
                                                         Physicalcomp.NameAutoPosition             :=LogicalComp.NameAutoPosition;
                                                         Physicalcomp.NameOn                       :=LogicalComp.NameOn;

                                                         Physicalcomp.CommentAutoPosition          :=LogicalComp.CommentAutoPosition;
                                                         Physicalcomp.CommentOn                    :=LogicalComp.CommentOn;

                                                         PhysicalComp.GraphicallyInvalidate;

                                                         PhysicalComp.EndModify;

                                                         // Set Designator
                                                         Physicalcomp.Name.BeginModify;
                                                         Physicalcomp.Name.XLocation            :=LogicalComp.Name.XLocation;
                                                         Physicalcomp.Name.YLocation            :=LogicalComp.Name.YLocation;
                                                         Physicalcomp.Name.Size                 :=LogicalComp.Name.Size;
                                                         Physicalcomp.Name.Width                :=LogicalComp.Name.Width;
                                                         Physicalcomp.Name.Layer                :=LogicalComp.Name.Layer;
                                                         Physicalcomp.Name.Rotation             :=LogicalComp.Name.Rotation;
                                                         Physicalcomp.Name.MirrorFlag           :=LogicalComp.Name.MirrorFlag;
                                                         Physicalcomp.Name.UseTTFonts           :=LogicalComp.Name.UseTTFonts;
                                                         Physicalcomp.Name.FontID               :=LogicalComp.Name.FontID;
                                                         Physicalcomp.Name.FontName             :=LogicalComp.Name.FontName;
                                                         Physicalcomp.Name.Bold                 :=LogicalComp.Name.Bold;
                                                         Physicalcomp.Name.Italic               :=LogicalComp.Name.Italic;
                                                         Physicalcomp.Name.Inverted             :=LogicalComp.Name.Inverted;
                                                         Physicalcomp.Name.InvertedTTTextBorder :=LogicalComp.Name.InvertedTTTextBorder;
                                                         Physicalcomp.Name.EndModify;

                                                         // Set Comment
                                                         Physicalcomp.Comment.BeginModify;
                                                         Physicalcomp.Comment.XLocation            :=LogicalComp.Comment.XLocation;
                                                         Physicalcomp.Comment.YLocation            :=LogicalComp.Comment.YLocation;
                                                         Physicalcomp.Comment.Size                 :=LogicalComp.Comment.Size;
                                                         Physicalcomp.Comment.Width                :=LogicalComp.Comment.Width;
                                                         Physicalcomp.Comment.Layer                :=LogicalComp.Comment.Layer;
                                                         Physicalcomp.Comment.Rotation             :=LogicalComp.Comment.Rotation;
                                                         Physicalcomp.Comment.MirrorFlag           :=LogicalComp.Comment.MirrorFlag;
                                                         Physicalcomp.Comment.UseTTFonts           :=LogicalComp.Comment.UseTTFonts;
                                                         Physicalcomp.Comment.FontID               :=LogicalComp.Comment.FontID;
                                                         Physicalcomp.Comment.FontName             :=LogicalComp.Comment.FontName;
                                                         Physicalcomp.Comment.Bold                 :=LogicalComp.Comment.Bold;
                                                         Physicalcomp.Comment.Italic               :=LogicalComp.Comment.Italic;
                                                         Physicalcomp.Comment.Inverted             :=LogicalComp.Comment.Inverted;
                                                         Physicalcomp.Comment.InvertedTTTextBorder :=LogicalComp.Comment.InvertedTTTextBorder;
                                                         Physicalcomp.Comment.EndModify;


                                                         // Over here we will need to delete component.
                                                         // I will need to cycling through footprint

                                                         PCBServer.PreProcess;
                                                         Board.RemovePCBObject(LogicalComp);
                                                         PCBServer.PostProcess;
                                                      end
                                                      else
                                                      begin
                                                         ModeFlag := 1;
                                                      end;

                                                      // This sets ModeFlag to say that there are components in this blanket
                                                      ModeFlag := 1;

                                                      // DocPartFlag - I need only one part of multi-part component to be in a blanket
                                                      DocPartFlag := 1;
                                                   end;
                                            end;
                                        end;
                                     end;

                                     // We will add all selected objects to union
                                     ResetParameters;
                                     AddStringParameter('Action', 'CreateUnion');
                                     RunProcess('PCB:ManageUnions');

                                     if ModeFlag = 0 then
                                     begin
                                        // This mode is without any components - if blanket does not contain components.
                                        // It will be implemented later
                                        ModeFlag := 1;

                                     end;
                                  end;

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
               Blanket := BlanketIterator.NextSchObject;
            end;
         finally
            Sheet.SchIterator_Destroy(BlanketIterator);
         end;
      end;
   end;

   // Clear selection
   ResetParameters;
   AddStringParameter('Scope', 'All');
   RunProcess('PCB:DeSelect');

   // Update free primitives from component pads
   ResetParameters;
   AddStringParameter('Action', 'UpdateFreePrimitiveNets');
   RunProcess('PCB:Netlist');
end;
