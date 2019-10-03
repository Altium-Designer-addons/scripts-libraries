var

Board   :   IPCB_Board;

{   Gets the Board from the PCBServer called from the form create function     }
Procedure InitBoard;
begin

    board := PCBServer.GetCurrentPCBBoard;

end;

procedure AddText;
var

    LayerI  :   IPCB_LayerIterator;
    Layer   :   IPCB_LayerObject;
    TToAdd  :   string;
    Text    :   IPCB_Primitive;
    n       :   Double;

begin



    if board=Nil then
    begin
        showmessage('Board not found');
    end
    else
    begin

        TToAdd := BoardText.Text;// Get text string from form

        Text := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);//Create text primitive

        PCBServer.PreProcess;// ¯\_(?)_/¯

        if CoordUnits.Text='Mils' then// Set text location in mils
        begin
            Text.XLocation := board.XOrigin+MilsToCoord(XPos.Text);
            Text.YLocation := board.YOrigin+MilsToCoord(YPos.Text);
        end
        else// Set text location in mm
        begin
            Text.XLocation := board.XOrigin+MilsToCoord((XPos.Text)/0.0254);
            Text.YLocation := board.YOrigin+MilsToCoord((YPos.Text)/0.0254);
        end;

        Text.RotateBy(Rotation.Text);// Set the rotation of text

        Text.Text := TToAdd;// Set the text of the primitive

        if FontUnits.Text='Mils' then// Set text size in mils
        begin
            Text.Size := MilsToCoord(TextHeight.Text);
            Text.Width := MilsToCoord(StrokeWidth.Text);
        end
        else// Set text size in mm
        begin
            Text.Size := MilsToCoord(TextHeight.Text/0.0254);
            Text.Width := MilsToCoord(StrokeWidth.Text/0.0254);
        end;

        LayerI := Board.LayerIterator;// Set up a layer iterator

        LayerI.First;

        Layer := LayerI.LayerObject;

        n := 0;

        while Layer <> Nil do// loop until found layer selected
        begin
//IPCB_LayerOBJECT.LayerStack.StateID

            //ShowMessage(Layer.GetState_LayerDisplayName(e));

            //ShowMessage(Layer.Name);
            
            if Layer.Name = LayerSelect.Text then// if is correct layer set text to be on it
            begin
                {
                when the right layer object is found the coresponding integer must also be found.
                Where is no method to do this.  A work around was found by which the short display
                name is retrieved from the layer object and then using the layerUtil converted to
                the integer.  As far as I could tell, there is no documentation available for the
                GetState_LayerDisplayName method. It returns a string of the layer display name.
                depending on the value it id given it can be up to three (probably) different types
                of display name.  0 will give the short displayname, 1 will give a longer version
                (the regular displayname?), 2 gives the name as can be set by the user
                }
                Text.Layer := LayerUtils.FromShortDisplayString(Layer.GetState_LayerDisplayName(0));

                break;


            end;

            LayerI.Next;
            Layer := LayerI.LayerObject;

        end;

        Board.AddPCBObject(Text);// Add primitive to PCB

        PCBServer.PostProcess;// ¯\_(?)_/¯

    end;
end;

procedure TForm1.SubmitClick(Sender: TObject);
begin
    AddText;
    if CloseReady.Checked = True then
        self.Close;
end;


procedure TForm1.Form1Create(Sender: TObject);
var

   // LayerSet    :   IPCB_LayerObjectIterator;

    LayerI  :   IPCB_LayerObjectIterator;
    Layer   :   IPCB_LayerObject;

begin

//////////////Add layerselect

    InitBoard;

    //CloseReady.Checked := True;

    CoordUnits.AddItem('mm', CoordUnits);
    CoordUnits.AddItem('Mils', CoordUnits);

    CoordUnits.ItemIndex := 0;

    FontUnits.AddItem('mm', FontUnits);
    FontUnits.AddItem('Mils', FontUnits);

    FontUnits.ItemIndex := 0;

    //get layers and add them to layer select combo box


    LayerI := Board.LayerIterator;

    LayerI.First;

    Layer := LayerI.LayerObject;

    While Layer <> Nil do
    begin

        //stuff
        //ShowMessage(Layer.Name);

        LayerSelect.AddItem(Layer.Name, LayerSelect);

        LayerI.Next;
        Layer := LayerI.LayerObject;
    end;

    LayerSelect.ItemIndex := 0;

end;


procedure EndModify;//for ending failed scripts to allow the project to be saved
begin
    PCBServer.PostProcess;
end;

procedure TForm1.Form1KeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then AddText;
end;
