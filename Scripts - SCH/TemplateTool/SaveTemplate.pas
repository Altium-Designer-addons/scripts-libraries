{..............................................................................}
function GetWorkingSheet : ISch_Document;
begin
    Result := nil;
    if (SchServer <> nil) then
        Result := SchServer.GetCurrentSchDocument;
end;
{..............................................................................}

{..............................................................................}
function GetNewSheet: ISch_Document;
var
    new_doc : IServerDocument;
begin
    new_doc := CreateNewDocumentFromDocumentKind('SCH');
    if new_doc <> nil then
    begin
        Result := SchServer.GetCurrentSchDocument;
    end;
end;
{..............................................................................}

{..............................................................................}
function GetTemplate(const Sheet : ISch_Document) : ISch_Template;
var
    iter : ISch_Iterator;
    obj  : ISch_BasicContainer;
begin
    Result := nil;
    if Sheet <> nil then
    begin
        iter := Sheet.SchIterator_Create;
        obj  := iter.FirstSchObject;
        while (obj <> nil) do
        begin
            if obj.ObjectId = eTemplate then
            begin
                Result := obj;
                break;
            end;
            obj := iter.NextSchObject;
        end;
        Sheet.SchIterator_Destroy(iter);
    end;
end;
{..............................................................................}

{..............................................................................}
procedure GetContainedObjects(Container   : ISch_BasicContainer;
                              ObjList     : TList;
                              Recursively : Bool);
var
    iter   : ISch_Iterator;
    child  : ISch_BasicContainer;
begin
    iter := Container.SchIterator_Create;
    child  := iter.FirstSchObject;
    while (child <> nil) do
    begin
        if (Recursively) and (Container.ObjectId = child.ObjectId) then
            GetContainedObjects(child, obj_list, true)
        else
            ObjList.Add(child);
        child := iter.NextSchObject;
    end;
    Container.SchIterator_Destroy(iter);
end;
{..............................................................................}

{..............................................................................}
procedure MoveContainedObjects(FromContainer : ISch_BasicContainer;
                               ToContainer   : ISch_BasicContainer);
var
    obj_list : TList;
    obj      : ISch_BasicContainer;
    i        : Integer;
begin
    obj_list := TList.Create;
    GetContainedObjects(FromContainer, obj_list, true);
    for i := 0 to obj_list.count - 1 do
    begin
        obj := obj_list.Items[i];
        ToContainer.AddSchObject(obj);
        FromContainer.RemoveSchObject(obj);
    end;
    obj_list.Free;
end;
{..............................................................................}

{..............................................................................}
procedure CopyContainedObjects(FromContainer : ISch_BasicContainer;
                               ToContainer   : ISch_BasicContainer);
var
    obj_list : TList;
    obj      : ISch_BasicContainer;
    new_obj  : ISch_BasicContainer;
    i        : Integer;
begin
    obj_list := TList.Create;
    GetContainedObjects(FromContainer, obj_list, true);
    for i := 0 to obj_list.count - 1 do
    begin
        obj := obj_list.Items[i];
        new_obj := obj.Replicate;
        ToContainer.AddSchObject(new_obj);
    end;
    obj_list.Free;
end;
{..............................................................................}

{..............................................................................}
procedure ExplodeTemplate;
var
    template  : ISch_Template;
    sheet     : ISch_Document;
    i         : Integer;
begin
    sheet := GetWorkingSheet;
    if (sheet <> nil) then
    begin
        template := GetTemplate(sheet);
        if template <> nil then
        begin
            MoveContainedObjects(template, sheet);
            sheet.RemoveSchObject(template);
            SchServer.DestroySchObject(template);
        end;
        sheet.GraphicallyInvalidate;
    end;
End;
{..............................................................................}

{..............................................................................}
procedure SaveTemplate;
var
    template      : ISch_Template;
    sheet         : ISch_Document;
    new_sheet     : ISch_Document;
    i             : Integer;
begin
    sheet := GetWorkingSheet;
    if (sheet <> nil) then
    begin
        // Get the top level template in sheet
        template := GetTemplate(sheet);
        if template <> nil then
        begin
            new_sheet := GetNewSheet;
            if new_sheet <> nil then
            begin
                // Set up the new sheet size
                new_sheet.TitleBlockOn     := False;
                new_sheet.ReferenceZonesOn := False;
                if sheet.UseCustomSheet then
                begin
                    new_sheet.UseCustomSheet := true;
                    new_sheet.CustomX := sheet.CustomX;
                    new_sheet.CustomY := sheet.CustomY;
                end
                else
                begin
                    new_sheet.SheetXSize := sheet.SheetXSize;
                    new_sheet.SheetYSize := sheet.SheetYSize;
                end;

                CopyContainedObjects(template, new_sheet);

                new_sheet.UpdateDocumentProperties;
                new_sheet.GraphicallyInvalidate
            end;
        end;
    end;
End;
{..............................................................................}

