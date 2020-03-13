{..............................................................................}
{ Summary   This scripts can be used to Auto load STEP files in PcbLib file.   }
{                                                                              }
{           STEP files need to have the same name as footprints. If names are  }
{           not 100% same, you can use "Smart Name Detection" that will try to }
{           find corrent 3D model by removing last characters in footprint     }
{           name.                                                              }
{                                                                              }
{           "Include Subfolders" option will include subfolders into detection.}
{           if 3D model is found, script will remove existing 3D models from   }
{           footprint if "Delete Existing models is checked".                  }
{           Most 3D CAD tools have Z axis that points up - except SolidWorks,  }
{           which has Y pointing up. You can choose up axis in popup menu.     }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}



procedure TFormAutoSTEPplacer.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;


procedure TFormAutoSTEPplacer.ButtonOKClick(Sender: TObject);
var
   SourceFolder   : String;
   Subfolders     : Boolean;
   STEPList       : TStringList;
   PCBLibList     : TStringList;
   PcbLibDoc      : IServerDocument;
   PCBLibrary     : IPCB_Library;
   Component      : IPCB_LibComponent;
   CIter          : IPCB_GroupIterator;
   CompName       : String;
   ExistingModels : TStringList;
   STEPFileName   : String;
   STEPmodel      : IPCB_ComponentBody;
   Model          : IPCB_Model;
   i, j, k        : Integer;
   CHRremoved     : Integer;
   ModelFound     : Boolean;
begin
   SourceFolder := EditFolderName.Text;
   Subfolders   := CheckBoxSubfolders.Checked;

   If (DirectoryExists(SourceFolder)) Then
   Begin
      STEPList  := TStringList.Create;
      PCBLibList := TStringList.Create;
      ExistingModels := TStringList.Create;
      FindFiles(SourceFolder,'*.step',faAnyFile,Subfolders,STEPList);
      FindFiles(SourceFolder,'*.stp',faAnyFile,Subfolders,PCBLibList);

      STEPList.AddStrings(PCBLibList);
      STEPList.Sort;

      FindFiles(SourceFolder,'*.PcbLib',faAnyFile,Subfolders,PCBLibList);

      for i := 0 to PCBLibList.Count - 1 do
      begin
         PcbLibDoc := Client.OpenDocument('PCBLIB',PCBLibList.Strings(i));
         Client.ShowDocument(PcbLibDoc);

         PCBLibrary := PCBServer.GetCurrentPCBLibrary;
         if PCBLibrary <> nil then
            for j := 0 to PCBLibrary.ComponentCount - 1 do
            begin
               Component := PCBLibrary.GetComponent(j);
               CompName := Component.Name;
               ModelFound := False;

               if CheckBoxDelete.Checked then
               begin
                  CIter := Component.GroupIterator_Create;
                  CIter.AddFilter_ObjectSet(MkSet(eComponentBodyObject));
                  ExistingModels.Clear;
                  // Using STEPmodel for 3D body temporarely
                  // And CHRremoved for counter
                  STEPmodel := CIter.FirstPCBObject;
                  CHRremoved := 0;
                  While (STEPmodel <> nil) do
                  begin
                     ExistingModels.AddObject(IntToStr(CHRremoved),STEPmodel);
                     Inc(CHRremoved);
                     STEPmodel := CIter.NextPCBObject;
                  end;
               end;

               CHRremoved := 0;

               repeat
                  for k := 0 to STEPList.Count - 1 do
                  begin
                     STEPFileName := STEPList[k];
                     Delete(STEPFileName, 1, LastDelimiter('\',STEPFileName));

                     if Length(STEPFileName) > Length(CompName) then
                     begin
                        SetLength(STEPFileName, Length(CompName));

                        if AnsiCompareText(STEPFileName, CompName) = 0 then
                        begin
                           STEPFileName := STEPList[k];
                           // Delete existing models if CB is checked
                           if CheckBoxDelete.Checked then
                              for k := 0 to ExistingModels.Count - 1 do
                              begin
                                 STEPmodel := ExistingModels.GetObject(k);
                                 Component.RemovePCBObject(STEPmodel);
                              end;
                           // So now I do have the STEP file and component it needs to go to
                           STEPmodel := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);
                           Model := STEPmodel.ModelFactory_FromFilename(STEPFileName, false);
                           STEPmodel.SetState_FromModel;
                           if ComboBoxTop.Text = 'Y' then
                              Model.SetState(90,0,0,0);
                           STEPmodel.Model := Model;
                           Component.AddPCBObject(STEPmodel);
                           ModelFound := True;
                           break;
                        end;
                     end;
                  end;
                  Inc(CHRremoved);
                  SetLength(CompName, Length(CompName) - 1);
               until ModelFound or (CHRRemoved = 5) or (CheckBoxSmart.Checked = False) or (CompName = '');
            end;
         PcbLibDoc.DoFileSave('PcbLib');
         Client.CloseDocument(PcbLibDoc);
      end;
   end;
   close;
end;

