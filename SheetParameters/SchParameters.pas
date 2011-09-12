{..............................................................................}
{ Summary   This script can be used to set schematic document parameters.      }
{           If you leave parameter to be '*', it will not be updated. Other    }
{           than that, all documents will be updated with new value that is    }
{           set in the text Box                                                }
{                                                                              }
{                                                                              }
{                                                                              }
{ Created by:    Rob Sterling                                                  }
{..............................................................................}

{..............................................................................}
Var
  SchParm       : TSchParm;
  OurProject    : IProject;
  numbered      : boolean;
  SchCount      : integer;
{..............................................................................}

Function FindAndChangeParam(param : String, newText : string, CurDoc : ISch_Sheet, change : boolean) : String;
Var
    CurrentSch  : Isch_Sheet;
    Iterator    : Isch_Iterator;
    ReportList  : TStringList;
    Parameter   : ISch_Parameter;
    Document    : IServerDocument;
    found       : boolean;
Begin
    If SchServer = Nil then Exit;
    If CurDoc = Nil then Exit;
    //we first look for an existing parameter and change that. if we can't
    //find one we have to create one.
    //create the iterator
    Iterator := CurDoc.SchIterator_Create;
    //look only for stand-alone parameters
    Iterator.SetState_IterationDepth(eIterateFirstLevel);
    //only look for schematic params
    Iterator.AddFilter_ObjectSet(MkSet(eParameter));

                                                  
    found := false;
    Result := '*';
    Try
       Parameter := Iterator.FirstSchObject;
       While Parameter <> Nil Do
         Begin
            if Parameter.Name = param then
               Begin
                 found := true;
                 if change then
                   Begin
                     Parameter.Text := newText;
                   End
                 else
                   Begin
                     If Parameter.Text <> '*' then
                        If Parameter.Text <> '' then
                           Result := Parameter.Text;
                   End;
               End;
            Parameter := Iterator.NextSchObject;
          End;
    Finally
       CurDoc.SchIterator_Destroy(Iterator);
    End;
    if not found then
       if change then
       Begin //we need to create the parameter instead
         Parameter := SchServer.SchObjectFactory (eParameter, eCreate_Default);
         Parameter.Name := param;
         Parameter.Text := newText;
         CurDoc.AddSchObject(Parameter);
       End;

End;

{..............................................................................}

function TextUpdate( inputstr : string, schstr : string) : string;
begin
  if ((inputstr = '*') or (inputstr = '')) then
       if ((schstr <> '') and (schstr <> '*')) then
          Result := schstr
       else Result := inputstr
  else
      Result := inputstr
end;

{..............................................................................}

Procedure IterateAllSchematicsOfAProject(Project : IProject, setParam : boolean);
Var
    I           : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Sheet;
    S           : string;
    SchDocument : IServerDocument;
Begin
    SchCount := 0;
    if (setParam = false) then //assume sheets aren't numbered, we look as we iterate
       numbered := false;
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
             SchCount := SchCount + 1;
             //Make sure it is open
             SchDocument := Client.OpenDocument('SCH',Doc.DM_FullPath);
             //and visible
             Client.ShowDocumentDontFocus(SchDocument);
             CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

             if setParam then
                Begin
                  FindAndChangeParam('Address1', Address1.Text, CurrentSch, true);
                  FindAndChangeParam('Address2', Address2.Text, CurrentSch, true);
                  FindAndChangeParam('Address3', Address3.Text, CurrentSch, true);
                  FindAndChangeParam('Address4', Address4.Text, CurrentSch, true);
                  FindAndChangeParam('ApprovedBy', ApprovedBy.Text, CurrentSch, true);
                  FindAndChangeParam('Author', Author.Text, CurrentSch, true);
                  FindAndChangeParam('CheckedBy', CheckedBy.Text, CurrentSch, true);
                  FindAndChangeParam('CompanyName', CompanyName.Text, CurrentSch, true);
                  FindAndChangeParam('Date', DocDate.Text, CurrentSch, true);
                  FindAndChangeParam('DocumentNumber', DocumentNumber.Text, CurrentSch, true);
                  FindAndChangeParam('DrawnBy', DrawnBy.Text, CurrentSch, true);
                  FindAndChangeParam('Engineer', Engineer.Text, CurrentSch, true);
                  //FindAndChangeParam('ImagePath', ImagePath.Text, CurrentSch, true);
                  FindAndChangeParam('ModifiedDate', ModifiedDate.Text, CurrentSch, true);
                  FindAndChangeParam('Organization', Organization.Text, CurrentSch, true);
                  FindAndChangeParam('Revision', PCBrevision.Text, CurrentSch, true);
                  FindAndChangeParam('Time', DocTime.Text, CurrentSch, true);
                  FindAndChangeParam('Title', PCBTitle.Text, CurrentSch, true);
                  FindAndChangeParam('SheetTotal', Totalsheets.Text, CurrentSch, true);
                  //if our sheets aren't numbered, we do it here
                  SchDocument.Modified := True;
                End
             else
                Begin
                  Address1.Text := TextUpdate(Address1.Text,FindAndChangeParam('Address1', '', CurrentSch, false));
                  Address2.Text := TextUpdate(Address2.Text,FindAndChangeParam('Address2', '', CurrentSch, false));
                  Address3.Text := TextUpdate(Address3.Text,FindAndChangeParam('Address3', '', CurrentSch, false));
                  Address4.Text := TextUpdate(Address4.Text,FindAndChangeParam('Address4', '', CurrentSch, false));
                  ApprovedBy.Text := TextUpdate(ApprovedBy.Text, FindAndChangeParam('ApprovedBy', '', CurrentSch, false));
                  Author.Text := TextUpdate(Author.Text, FindAndChangeParam('Author', '', CurrentSch, false));
                  CheckedBy.Text := TextUpdate(CheckedBy.Text,FindAndChangeParam('CheckedBy', '', CurrentSch, false));
                  CompanyName.Text := TextUpdate(CompanyName.Text,FindAndChangeParam('CompanyName', '', CurrentSch, false));
                  DocDate.Text := TextUpdate(DocDate.Text,FindAndChangeParam('Date', '', CurrentSch, false));
                  DocumentNumber.Text := TextUpdate(DocumentNumber.Text,FindAndChangeParam('DocumentNumber', '', CurrentSch, false));
                  DrawnBy.Text := TextUpdate(DrawnBy.Text, FindAndChangeParam('DrawnBy', '', CurrentSch, false));
                  Engineer.Text := TextUpdate(Engineer.Text, FindAndChangeParam('Engineer', '', CurrentSch, false));
                  //ImagePath.Text := TextUpdate(ImagePath.Text,FindAndChangeParam('ImagePath', '', CurrentSch, false));
                  ModifiedDate.Text := TextUpdate(ModifiedDate.Text,FindAndChangeParam('ModifiedDate', '', CurrentSch, false));
                  Organization.Text := TextUpdate(Organization.Text,FindAndChangeParam('Organization', '', CurrentSch, false));
                  PCBrevision.Text := TextUpdate(PCBrevision.Text,FindAndChangeParam('Revision', '', CurrentSch, false));
                  DocTime.Text := TextUpdate(DocTime.Text, FindAndChangeParam('Time', '', CurrentSch, false));
                  PCBTitle.Text := TextUpdate(PCBTitle.Text, FindAndChangeParam('Title', '', CurrentSch, false));
                End;
        End
    End;
    Totalsheets.Text := IntToStr(SchCount);
End;


Procedure NumberSchematicsOfAProject(Project : IProject, CurDoc : IDocument);
Var
    I           : Integer;
    Children    : integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Sheet;
    S           : string;
    SchDocument : IServerDocument;
Begin
  if (CurDoc = nil) then
     begin
       Doc := Project.DM_TopLevelLogicalDocument;
       If Doc.DM_DocumentKind = 'SCH' Then
        Begin
          SchCount := 0;
          NumberSchematicsOfAProject(Project, Doc);
        end
    end
  else
    begin
      Doc := CurDoc;
      If Doc.DM_DocumentKind = 'SCH' Then
         begin
           SchDocument := Client.OpenDocument('SCH',Doc.DM_FullPath);
           CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
           SchCount := SchCount + 1;
           FindAndChangeParam('SheetNumber', IntToStr(SchCount), CurrentSch, true);
           CurrentSch.GraphicallyInvalidate;
           SchDocument.Modified := True;
         end;
      if (Doc.DM_ChildDocumentCount > 0) then
         Begin
           Children := Doc.DM_ChildDocumentCount;
           //recurse into the children
           For i:= 0 to (children - 1) do
             NumberSchematicsOfAProject(Project, Doc.DM_ChildDocuments[i]);
         End
    end;
End;


procedure TSchParm.OkButtonClick(Sender: TObject);
begin
  OurProject := GetWorkspace.DM_FocusedProject;
  If OurProject = Nil Then
     Begin
       ShowMessage('Could not find current project.');
       Close;
     End
  else
     Begin
       IterateAllSchematicsOfAProject(OurProject, true);
       if (OurProject.DM_Compile = True) then
          begin
            NumberSchematicsOfAProject(OurProject, nil);
          end
       else
           ShowMessage('Unable to compile project - no sheet numbers updated.');
       Close;
     End;

end;


procedure TSchParm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;


Procedure UpdateSchematicParameters;
Begin
  OurProject := GetWorkspace.DM_FocusedProject;
  If OurProject = Nil Then
     Begin
       ShowMessage('Could not find current project.');
       Close;
     End
  else
     Begin
       ProjName.Caption := OurProject.DM_ProjectFullPath;
       IterateAllSchematicsOfAProject(OurProject, false);
       SchParm.ShowModal;
     End;
End;


procedure TSchParm.SchParmActivate(Sender: TObject);
begin
  OurProject := GetWorkspace.DM_FocusedProject;
  If OurProject = Nil Then
     Begin
       ShowMessage('Could not find current project.');
       Close;
     End
  else
     Begin
       ProjName.Caption := OurProject.DM_ProjectFullPath;
       IterateAllSchematicsOfAProject(OurProject, false);
     end;
end; 
