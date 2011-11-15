{..............................................................................}
{ Summary   This Script enables true design reuse in Altium Designer           }
{                                                                              }
{           Read "How to use this script.odt" document that comes with this    }
{           script for more info.                                              }
{                                                                              }
{           It uses Balnkets + Parameter Sets (Place -> Directive -> Parameter }
{           Set) which have parameter "Snippet", and it's value is name of the }
{           PCB snippet.                                                       }
{                                                                              }
{           User should have this directives placed on schematic, and he       }
{           should also have PCB snippets with the same name.                  }
{                                                                              }
{           If the above is satisfied, user should do regular PCB update first,}
{           and after that run this script. This script will then place PCB    }
{           snippet(s), modify component placement, update net info in         }
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

    SnippetsFolders   : TStringList;
    Board             : IPCB_Board;
    FlatHierarchy     : IDocument;
    PcbPosX, PcbPosY  : Integer;


{..............................................................................}
{                                                                              }
{   WriteSnippetsFolders - Procedure used to get snippets folders.             }
{                                                                              }
{..............................................................................}
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


{..............................................................................}
{                                                                              }
{   TestSnippet - Procedure used to test if selected componants have different }
{                 visible designators. Function can be used for testing before }
{                 creating snippet, since all it's components need to have     }
{                 different designators                                        }
{                                                                              }
{..............................................................................}
Procedure TestSnippet;
var

    BoardIterator1    : IPCB_BoardIterator;
    BoardIterator2    : IPCB_BoardIterator;
    Comp1             : IPCB_Component;
    Comp2             : IPCB_Component;
    Designatori       : TStringList;
    i, flag           : Integer;
    S                 : String;

begin

    try
       BoardIterator1 := Board.BoardIterator_Create;
       BoardIterator1.AddFilter_ObjectSet(MkSet(eComponentObject));
       BoardIterator1.AddFilter_LayerSet(AllLayers);
       BoardIterator1.AddFilter_Method(eProcessAll);

       Designatori := TStringList.Create;

       Comp1 := BoardIterator1.FirstPCBObject;
       While (Comp1 <> Nil) Do
       Begin
          if Comp1.Selected then
          begin
             try
                BoardIterator2 := Board.BoardIterator_Create;
                BoardIterator2.AddFilter_ObjectSet(MkSet(eComponentObject));
                BoardIterator2.AddFilter_LayerSet(AllLayers);
                BoardIterator2.AddFilter_Method(eProcessAll);

                Comp2 := BoardIterator2.FirstPCBObject;
                While (Comp2 <> Nil) Do
                Begin
                   if (Comp2.Selected) and (Comp2.Name.GetDesignatorDisplayString = Comp1.Name.GetDesignatorDisplayString) and not (Comp1.I_ObjectAddress = Comp2.I_ObjectAddress) then
                   begin
                      if Designatori.Count = 0 then
                         Designatori.Add(Comp2.Name.GetDesignatorDisplayString)
                      else
                      begin
                         flag := 0;
                         for i := 0 to Designatori.Count - 1 do
                            if Designatori[i] = Comp2.Name.GetDesignatorDisplayString then
                               flag := 1;

                         if flag = 0 then
                            Designatori.Add(Comp2.Name.GetDesignatorDisplayString);
                      end;
                   end;
                   Comp2 := BoardIterator2.NextPCBObject;
                end;
             finally
                Board.BoardIterator_Destroy(BoardIterator2);
             end;
          end;
          Comp1 := BoardIterator1.NextPCBObject;
       end;
    finally
       Board.BoardIterator_Destroy(BoardIterator1);
    end;

    if Designatori.Count = 0 then
       S := 'All OK'
    else
    begin
       S := 'Duplicates are:' + #13;
       for i := 0 to Designatori.Count - 1 do
           S := S + Designatori[i] + #13;
    end;

    ShowInfo(S);
end;


{..............................................................................}
{                                                                              }
{   PlaceSnippet - Procedure used to place snippet to the right place on PCB.  }
{                                                                              }
{..............................................................................}
function PlaceSnippet(SnippetName : String) : Boolean;
var

    SnippetDocument   : IServerDocument;
    SnippetPCB        : IPCB_Board;
    BoardIterator     : IPCB_BoardIterator;
    Rectangle         : TCoordRect;

    PcbObj            : IPCB_Primitive;
    SnippetIterator   : IPCB_BoardIterator;
    SnippetObj        : IPCB_Primitive;

    i                 : Integer;
    fileFlag          : Integer;
    ASetOfObjects     : TObjectSet;

begin
   SnippetName := '\' + SnippetName;

   if (AnsiPos('.PCBDOC', ansiUpperCase(SnippetName)) = 0) then
      SnippetName := SnippetName + '.PcbDoc';

   SnippetDocument := nil;

   fileFlag := 0;

   for i := 0 to SnippetsFolders.Count-1 do
   begin
      if FileExists(Snippetsfolders[i] + SnippetName) then
      begin
         SnippetDocument := Client.OpenDocument('PCB', Snippetsfolders[i] + SnippetName);
         fileFlag := 1;
         break;
      end;
   end;

   if fileFlag = 0 then
   begin
      SetLength(SnippetName, AnsiPos('.PCBDOC', ansiUpperCase(SnippetName)) - 1);
      Delete(SnippetName, 1, 1);
      ShowMessage('Could not find snippet "' + SnippetName + '"');
      Result := False;
      Exit;
   end;


   SnippetPCB := PCBServer.GetPCBBoardByPath(Snippetsfolders[i] + SnippetName);

   // Next part is figuring out weather we have enough empty space above the PCB, and where
   Rectangle := SnippetPCB.BoundingRectangle;

   PcbPosY := PcbPosY + MilsToCoord(500);


   // Now we need to place snippet on the PCB.
   // Since interactive placement does not work for now, I need to place it manually.
   // It will work better this way, anyway

   // First we will run process to unselect everything
   ResetParameters;
   AddStringParameter('Scope', 'All');
   RunProcess('PCB:DeSelect');

   ASetOfObjects := MkSet(ePadObject, eViaObject, eTrackObject, eArcObject, eTextObject, eFillObject, eComponentObject, ePolyObject,
                          eRegionObject, eComponentBodyObject, eDimensionObject, eCoordinateObject);

   Try
      SnippetIterator := SnippetPCB.BoardIterator_Create;
      SnippetIterator.AddFilter_ObjectSet(ASetOfObjects);
      SnippetIterator.AddFilter_LayerSet(AllLayers);
      SnippetIterator.AddFilter_Method(eProcessAll);

      SnippetObj := SnippetIterator.FirstPCBObject;

      While SnippetObj <> nil do
      begin
         PCBObj := nil;

         if (SnippetObj.ObjectId = eComponentobject) or (SnippetObj.ObjectId = ePolyObject) or
            (SnippetObj.ObjectId = eDimensionObject) or (SnippetObj.ObjectId = eCoordinateObject) then
               PcbObj := SnippetObj.ReplicateWithChildren
         else if SnippetObj.IsFreePrimitive then
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

   Result := True;
end;


{..............................................................................}
{                                                                              }
{   SwitchPCBComponents - Procedure used to replace snippet component with     }
{                         Project Component.                                   }
{                                                                              }
{..............................................................................}
Procedure SwitchPCBComponents(LogicalDesignator : String; PhysicalDesignator : String);
var

    BoardIterator     : IPCB_BoardIterator;
    PcbComp           : IPCB_Component;
    PhysicalComp      : IPCB_Component;
    LogicalComp       : IPCB_Component;

begin
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
            if PcbComp.Name.Text = LogicalDesignator then
               LogicalComp := PcbComp;
         end
         else
         begin
            // this is a PCB component
            if PcbComp.Name.Text = PhysicalDesignator then
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

      PhysicalComp.Layer := LogicalComp.Layer;
      PhysicalComp.Rotation := LogicalComp.Rotation;

      PhysicalComp.Selected := True;

      // Set some options of designator and comment
      PhysicalComp.NameAutoPosition             :=LogicalComp.NameAutoPosition;
      PhysicalComp.NameOn                       :=LogicalComp.NameOn;

      PhysicalComp.CommentAutoPosition          :=LogicalComp.CommentAutoPosition;
      PhysicalComp.CommentOn                    :=LogicalComp.CommentOn;

      PhysicalComp.GraphicallyInvalidate;

      PhysicalComp.EndModify;

      // Set Designator
      PhysicalComp.Name.BeginModify;
      PhysicalComp.Name.XLocation            :=LogicalComp.Name.XLocation;
      PhysicalComp.Name.YLocation            :=LogicalComp.Name.YLocation;
      PhysicalComp.Name.Size                 :=LogicalComp.Name.Size;
      PhysicalComp.Name.Width                :=LogicalComp.Name.Width;
      PhysicalComp.Name.Layer                :=LogicalComp.Name.Layer;
      PhysicalComp.Name.Rotation             :=LogicalComp.Name.Rotation;
      PhysicalComp.Name.MirrorFlag           :=LogicalComp.Name.MirrorFlag;
      PhysicalComp.Name.UseTTFonts           :=LogicalComp.Name.UseTTFonts;
      PhysicalComp.Name.FontID               :=LogicalComp.Name.FontID;
      PhysicalComp.Name.FontName             :=LogicalComp.Name.FontName;
      PhysicalComp.Name.Bold                 :=LogicalComp.Name.Bold;
      PhysicalComp.Name.Italic               :=LogicalComp.Name.Italic;
      PhysicalComp.Name.Inverted             :=LogicalComp.Name.Inverted;
      PhysicalComp.Name.InvertedTTTextBorder :=LogicalComp.Name.InvertedTTTextBorder;
      PhysicalComp.Name.EndModify;

      // Set Comment
      PhysicalComp.Comment.BeginModify;
      PhysicalComp.Comment.XLocation            :=LogicalComp.Comment.XLocation;
      PhysicalComp.Comment.YLocation            :=LogicalComp.Comment.YLocation;
      PhysicalComp.Comment.Size                 :=LogicalComp.Comment.Size;
      PhysicalComp.Comment.Width                :=LogicalComp.Comment.Width;
      PhysicalComp.Comment.Layer                :=LogicalComp.Comment.Layer;
      PhysicalComp.Comment.Rotation             :=LogicalComp.Comment.Rotation;
      PhysicalComp.Comment.MirrorFlag           :=LogicalComp.Comment.MirrorFlag;
      PhysicalComp.Comment.UseTTFonts           :=LogicalComp.Comment.UseTTFonts;
      PhysicalComp.Comment.FontID               :=LogicalComp.Comment.FontID;
      PhysicalComp.Comment.FontName             :=LogicalComp.Comment.FontName;
      PhysicalComp.Comment.Bold                 :=LogicalComp.Comment.Bold;
      PhysicalComp.Comment.Italic               :=LogicalComp.Comment.Italic;
      PhysicalComp.Comment.Inverted             :=LogicalComp.Comment.Inverted;
      PhysicalComp.Comment.InvertedTTTextBorder :=LogicalComp.Comment.InvertedTTTextBorder;
      PhysicalComp.Comment.EndModify;


      // Over here we will need to delete snippet component.

      PCBServer.PreProcess;
      Board.RemovePCBObject(LogicalComp);
      PCBServer.PostProcess;
   end;
end;


{..............................................................................}
{                                                                              }
{   ConnectSnippet - Procedure used to connect components in snippet and       }
{                    components in project                                     }
{                                                                              }
{..............................................................................}
function ConnectSnippet(Document : IDocument; Rectangle : TCoordRect): Integer;
var

    DocCompNum        : Integer;
    DocPartFlag       : Integer;
    DocPartNum        : Integer;
    DocPart           : IPart;
    ModeFlag          : Integer;

begin

   ModeFlag := 0;

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
                    // and I need it's UniqueID to find it's matching component on the PCB.

                    SwitchPCBComponents(DocPart.DM_LogicalDesignator, DocPart.DM_PhysicalDesignator);

                    // This sets ModeFlag to say that there are components in this blanket
                    ModeFlag := 1;

                    // DocPartFlag - I need only one part of multi-part component to be in a blanket
                    DocPartFlag := 1;
                 end;
          end;
      end;
   end;
   Result := ModeFlag;
end;


{..............................................................................}
{                                                                              }
{   AddSheetSymbolParts - Procedure used to connect sub-shet components and    }
{                         snippet components.                                  }
{                                                                              }
{..............................................................................}
Procedure AddSheetSymbolParts(Document : IDocument);
var

    Sheet             : ISCH_Sheet;
    DocCompNum        : Integer;
    DocComponent      : IComponent;
    DocSheetSymbolNum : Integer;
    DocSheetSymbol    : ISheetSymbol;
    DocPartNum        : Integer;
    DocPart           : IPart;
    DocPartFlag       : Integer;

begin
   Sheet := SCHServer.GetSchDocumentByPath(Document.DM_FullPath);
   if Sheet = nil then exit;

   for DocCompNum := 0 to Document.DM_ComponentCount - 1 do
   begin
      DocPartFlag := 0;
      for DocPartNum := 0 to Document.DM_Components(DocCompNum).DM_SubPartCount - 1 do
      begin
          if (DocPartFlag = 0) then
          begin
             DocPart := Document.DM_Components(DocCompNum).DM_SubParts(DocPartNum);

             // OK, so here is the part on sch. I need it's logical designator to find snippet component,
             // and I need it's UniqueID to find it's matching component on the PCB.

             SwitchPCBComponents(DocPart.DM_LogicalDesignator, DocPart.DM_PhysicalDesignator);

             DocPartFlag := 1;
          end;
      end;
   end;

   // Now we need to add other sheet symbols to this recursively
   // there will be drawbacks with this - a lot of them


   for DocSheetSymbolNum := 0 to Document.DM_SheetSymbolCount - 1 do
      AddSheetSymbolParts(Document.DM_SheetSymbols(DocSheetSymbolNum).DM_ChildSheet(0));
end;


{..............................................................................}
{                                                                              }
{   AddSheetSymbolParts - Procedure used to connect sub-shet components and    }
{                         snippet components.                                  }
{                                                                              }
{..............................................................................}
Procedure InterfaceMode(Document : IDocument, Rectangle : TCoordRect);
var

    Designators       : TStringList;
    i                 : Integer;
    Line              : String;
    DesignatorFlag    : Integer;

    NetNum            : Integer;
    DocNet            : INet;
    FlatNetNum        : Integer;
    FlatNet           : INet;
    LabelNum          : Integer;
    NetLabel          : INetItem;
    PinNum            : Integer;
    Part              : IPart;
    ParamNum          : Integer;
    Parameter         : IParameter;

    SnippetDes        : String;
    PhysicalDes       : String;
    Delimiter         : Integer;

begin

   // We need Designators String List to write snippet designators
   // and physical designators of parts
   // They will be stored like this:
   //
   // SnippetDesignator1;PhysicalDesignator1
   // SnippetDesignator2;PhysicalDesignator2
   // SnippetDesignator3;PhysicalDesignator3
   // SnippetDesignator4;PhysicalDesignator4

   Designators := TStringList.Create;

   // We will cycle through nets in this sheet,
   // and see weather we have some net labels in rectangle (blanket)
   for NetNum := 0 to Document.DM_NetCount - 1 do
   begin
      DocNet := Document.DM_Nets(NetNum);

      // cycle through net labels of this net
      for LabelNum := 0 to DocNet.DM_NetLabelCount - 1 do
      begin
         NetLabel := DocNet.DM_NetLabels(LabelNum);

         // is net label inside rectangle
         if ((NetLabel.DM_LocationX >= Rectangle.Left)   and (NetLabel.DM_LocationX <= Rectangle.Right) and
             (NetLabel.DM_LocationY >= Rectangle.Bottom) and (NetLabel.DM_LocationY <= Rectangle.Top)) then
             begin

                // now we know that this NetLabel is inside rectangle,
                // First we need to get this net from flat hierarchy
                for FlatNetNum := 0 to FlatHierarchy.DM_NetCount - 1 do
                   if DocNet.DM_NetName = FlatHierarchy.DM_Nets(FlatNetNum).DM_NetName then
                      FlatNet := FlatHierarchy.DM_Nets(FlatNetNum);

                // now we need to check all pins on this net,
                // to get all components in this interface
                for PinNum := 0 to FlatNet.DM_PinCount - 1 do
                begin

                   // Part that this pin belongs to
                   Part := FlatNet.DM_Pins(PinNum).DM_Part;

                   // now we need to get the parameter of this part, to find
                   // "SnippetDesignator" Parameter
                   for ParamNum := 0 to Part.DM_ParameterCount - 1 do
                   begin
                      Parameter := Part.DM_Parameters(ParamNum);

                      // Check the parameter
                      if ((AnsiUpperCase(Parameter.DM_Name) = 'SNIPPET DESIGNATOR')) then
                      begin
                         // this is the line that will be placed in string list
                         Line := Parameter.DM_Value + ';' + Part.DM_PhysicalDesignator;

                         // we need to see weather this line already exists in string list
                         DesignatorFlag := 0;
                         for i := 0 to Designators.Count - 1 do
                            if Designators[i] = Line then DesignatorFlag := 1;

                         // if not, add it to string list
                         if DesignatorFlag = 0 then Designators.Add(Line);
                      end;
                   end;
                end;
             end;
      end;
   end;

   // now we have stringList populated with necessary designators
   // we will need to connect those to the PCB components
   // similar like in "ConnectSnippet" and "AddSheetSymbolParts" procedures

   for i := 0 to Designators.Count - 1 do
   begin
      Line := Designators[i];

      // Get designators
      Delimiter   := LastDelimiter(';', Line);
      SnippetDes  := Line;
      PhysicalDes := Line;
      
      Delete(SnippetDes,  Delimiter, Length(Line) - Delimiter + 1);
      Delete(PhysicalDes, 1, Delimiter);

      // Now we need to iterate PCB to get components with this designators
      SwitchPCBComponents(SnippetDes, PhysicalDes);
   end;
end;


{..............................................................................}
{                                                                              }
{   AnalyzeDocument - Procedure used to analyze documents iteratively, to see  }
{                     weather they have necessary blanket                      }
{                                                                              }
{..............................................................................}
Procedure AnalyzeDocument(Document : IDocument);
var

    Sheet             : ISCH_Document;
    BlanketIterator   : ISCH_Iterator;
    Blanket           : ISCH_Blanket;
    ParamSetIterator  : ISCH_Iterator;
    ParamSet          : ISCH_ParameterSet;
    ParamIterator     : ISCH_Iterator;
    Parameter         : ISCH_Parameter;
    ComponentIterator : ISCH_Iterator;
    Component         : ISCH_Component;
    ModeFlag          : Integer;
    SnippetPlaced     : Boolean;
    Rectangle         : TCoordRect;

    DocSheetSymbolNum : Integer;
    DocSheetSymbol    : ISheetSymbol;
    UsedSheetSymbols  : TStringList;
    UsedSCHSymbolsNum : Integer;
    SheetFlag         : Integer;
    ChildNum          : Integer;

begin
   ModeFlag := 0;

   UsedSheetSymbols := TStringList.Create;

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
                               if ((AnsiUpperCase(Parameter.Name) = 'SNIPPET') and (Parameter.Text <> '') and (Parameter.Text <> '*')) then
                               begin

                                  // Now we need to place snippet on the PCB.
                                  // It will be placed on an empty area above the Board

                                  SnippetPlaced := PlaceSnippet(Parameter.Text);
                                  if  SnippetPlaced = False then break;

                                  // There are two modes of work, and they are determined by the fact if there are
                                  // components inside blanket
                                  ModeFlag := ConnectSnippet(Document, Rectangle);


                                  // Now we need to add sheet symbols to this
                                  // there will be drawbacks wiht this - a lot of them
                                  DocSheetSymbolNum := Document.DM_SheetSymbolCount;

                                  for DocSheetSymbolNum := 0 to Document.DM_SheetSymbolCount - 1 do
                                  begin
                                     DocSheetSymbol := Document.DM_SheetSymbols(DocSheetSymbolNum);

                                     if ((DocSheetSymbol.DM_LocationX >= Rectangle.Left)   and (DocSheetSymbol.DM_LocationX <= Rectangle.Right) and
                                         (DocSheetSymbol.DM_LocationY >= Rectangle.Bottom) and (DocSheetSymbol.DM_LocationY <= Rectangle.Top)) then
                                         begin
                                            AddSheetSymbolParts(DocSheetSymbol.DM_ChildSheet(0));
                                            UsedSheetSymbols.Add(IntToStr(DocSheetSymbolNum));
                                            ModeFlag := 1;
                                         end;
                                  end;


                                  // This mode is without any components or sheet symbols - nets only
                                  // It is used to place interface between any number of components
                                  if ModeFlag = 0 then
                                     InterfaceMode(Document, Rectangle);


                                  // We will add all selected objects to union
                                  ResetParameters;
                                  AddStringParameter('Action', 'CreateUnion');
                                  RunProcess('PCB:ManageUnions');
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

   // Now analyze other sub-documents

   for DocSheetSymbolNum := 0 to Document.DM_SheetSymbolCount - 1 do
   begin
      SheetFlag := 0;
      for UsedSCHSymbolsNum := 0 to UsedSheetSymbols.Count - 1 do
      begin
         if (DocSheetSymbolNum = StrToInt(UsedSheetSymbols[UsedSCHSymbolsNum])) then SheetFlag := 1;
      end;

      if SheetFlag = 0 then
      begin
         DocSheetSymbol := Document.DM_SheetSymbols(DocSheetSymbolNum);

         for ChildNum := 0 to DocSheetSymbol.DM_ChildSheetCount - 1 do
            AnalyzeDocument(DocSheetSymbol.DM_ChildSheet(ChildNum));
      end;
   end;
end;


{..............................................................................}
{                                                                              }
{   DesignReuse - Main Procedure, that needs to be run. It calls all other     }
{                 procedures.                                                  }
{                                                                              }
{..............................................................................}
procedure DesignReuse;
var

    Workspace         : IWorkspace;
    PCBProject        : IProject;
    ProjectName       : String;
    Document          : IDocument;
    DocNum            : Integer;
    Rectangle         : TCoordRect;

begin

   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then
   Begin
      ShowMessage('Current Document is not a PCB Document');
      exit;
   end;

   Workspace := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   If (PcbProject = nil) then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   // Compile project
   FlatHierarchy := PCBProject.DM_DocumentFlattened;

   // If we couldn't get the flattened sheet, then most likely the project has
   // not been compiled recently
   If (FlatHierarchy = Nil) Then
   Begin
       // First try compiling the project
       ResetParameters;
       AddStringParameter( 'Action', 'Compile' );
       AddStringParameter( 'ObjectKind', 'Project' );
       RunProcess( 'WorkspaceManager:Compile' );

       // Try Again to open the flattened document
       FlatHierarchy := PCBProject.DM_DocumentFlattened;
       If (FlatHierarchy = Nil) Then
       Begin
           ShowMessage('NOTICE: Compile the Project before Running this script.');
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

   // We need to get top level physical document
   // We will find it because he is the only one that has no parents
   for DocNum := 0 to PcbProject.DM_PhysicalDocumentCount - 1 do
   begin
      Document := PCBProject.DM_PhysicalDocuments(DocNum);

      // Remember top level document
      if Document.DM_ParentDocumentCount = 0 then break;
   end;

   // --------------------------------------------------------------------------
   //
   //                     This is where calculation starts
   //
   // --------------------------------------------------------------------------

   AnalyzeDocument(Document);

   // Clear selection
   ResetParameters;
   AddStringParameter('Scope', 'All');
   RunProcess('PCB:DeSelect');

   // Update free primitives from component pads
   ResetParameters;
   AddStringParameter('Action', 'UpdateFreePrimitiveNets');
   RunProcess('PCB:Netlist');

   // Reset error markers
   ResetParameters;
   RunProcess('PCB:ResetAllErrorMarkers');
end;
