{..............................................................................}
{ Summary   This scripts can be used to filter objects that will be selected.  }
{           It was made when I was trying to make "SelectTouchingRectangle"    }
{           script.                                                            }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Uses
    IniFiles;
Var
    IniFileName : String;
    modeofwork  : Integer;

procedure ReadFromIniFile(AFileName : String);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    if (IniFile.ReadBool('CheckBoxes','Part',True)) then
        CheckBoxPart.Checked := 1
    else
        CheckBoxPart.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','Wire',True)) then
        CheckBoxWire.Checked := 1
    else
        CheckBoxWire.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','Port',True)) then
        CheckBoxPort.Checked := 1
    else
        CheckBoxPort.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','SheetSymbol',True)) then
        CheckBoxSheetSymbol.Checked := 1
    else
        CheckBoxSheetSymbol.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','HarnessConnector',True)) then
        CheckBoxHarnessConnector.Checked := 1
    else
        CheckBoxHarnessConnector.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','NetLabel',True)) then
        CheckBoxNetLabel.Checked := 1
    else
        CheckBoxNetLabel.Checked := 0;

    if (IniFile.ReadBool('CheckBoxes','PowerPort',True)) then
        CheckBoxPowerPort.Checked := 1
    else
        CheckBoxPowerPort.Checked := 0;

    IniFile.Free;
end;
{..............................................................................}

{..............................................................................}
Procedure WriteToIniFile(AFileName : String);
Var
    IniFile : TIniFile;
Begin
    IniFile := TIniFile.Create(AFileName);

    if (CheckBoxPart.Checked) then
       IniFile.WriteBool('CheckBoxes','Part',True)
    else
       IniFile.WriteBool('CheckBoxes','Part',False);

    if (CheckBoxWire.Checked) then
       IniFile.WriteBool('CheckBoxes','Wire',True)
    else
       IniFile.WriteBool('CheckBoxes','Wire',False);

    if (CheckBoxPort.Checked) then
       IniFile.WriteBool('CheckBoxes','Port',True)
    else
       IniFile.WriteBool('CheckBoxes','Port',False);

    if (CheckBoxSheetSymbol.Checked) then
       IniFile.WriteBool('CheckBoxes','SheetSymbol',True)
    else
       IniFile.WriteBool('CheckBoxes','SheetSymbol',False);

    if (CheckBoxHarnessConnector.Checked) then
       IniFile.WriteBool('CheckBoxes','HarnessConnector',True)
    else
       IniFile.WriteBool('CheckBoxes','HarnessConnector',False);

    if (CheckBoxNetLabel.Checked) then
       IniFile.WriteBool('CheckBoxes','NetLabel',True)
    else
       IniFile.WriteBool('CheckBoxes','NetLabel',False);

    if (CheckBoxPowerPort.Checked) then
       IniFile.WriteBool('CheckBoxes','PowerPort',True)
    else
       IniFile.WriteBool('CheckBoxes','PowerPort',False);

    IniFile.Free;
End;
{..............................................................................}

{..............................................................................}
// This script is used to select touching rectangle. In form you can set what objects to select.
Function Start;
var
   CurrentSheet    : ISch_Document;
   SpatialIterator : ISch_Iterator;
   Component       : ISch_Component;
   Port            : ISch_Port;
   Wire            : ISch_Wire;
   Bus             : IBus;
   BusEntry        : IBusEntry;
   SignalHarness   : ISignalHarness;
   HarnessConnector: IHarnessConnector;
   SheetSymbol     : ISheetSymbol;
   PowerPort       : IPowerObject;
   NetLabel        : INetLabel;
   Rect            : TCoordRect;
   boolRect        : bool;
begin

   Result := 0;

   If SchServer = Nil Then Exit;
   CurrentSheet := SchServer.GetCurrentSchDocument;
   If CurrentSheet = Nil Then Exit;

   Rect := TCoordRect;

   boolRect := CurrentSheet.ChooseRectangleInteractively(Rect,
                                                     'Please select the first corner',
                                                     'Please select the second corner');
   Result := 1;

   If Not boolRect Then Exit;

   Result := 2;

   SchServer.ProcessControl.PreProcess(CurrentSheet, '');

   // Check components for selection
   If (CheckBoxPart.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         Component := SpatialIterator.FirstSchObject;
         While Component <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                Component.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            Component := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check wires for selection
   If (CheckBoxWire.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eWire));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         Wire := SpatialIterator.FirstSchObject;
         While Wire <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(Wire.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                Wire.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(Wire.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            Wire := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check Busses for selection
   If (CheckBoxWire.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eBus));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         Bus := SpatialIterator.FirstSchObject;
         While Bus <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(Bus.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                Bus.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(Bus.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            Bus := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check Busses for selection
   If (CheckBoxWire.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eSignalHarness));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         SignalHarness := SpatialIterator.FirstSchObject;
         While SignalHarness <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(SignalHarness.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                SignalHarness.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(SignalHarness.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            SignalHarness := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Chack Bus entries for selection
   If (CheckBoxWire.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eBusEntry));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         BusEntry := SpatialIterator.FirstSchObject;
         While BusEntry <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(BusEntry.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                BusEntry.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(BusEntry.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            BusEntry := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;


   // Check Ports for selection
   If (CheckBoxPort.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(ePort));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         Port := SpatialIterator.FirstSchObject;
         While Port <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(Port.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                Port.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(Port.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            Port := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check sheet symbols for selection
   If (CheckBoxSheetSymbol.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eSheetSymbol));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         SheetSymbol := SpatialIterator.FirstSchObject;
         While SheetSymbol <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(SheetSymbol.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                SheetSymbol.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(SheetSymbol.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            SheetSymbol := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check Harness Connectors for selection
   If (CheckBoxSheetSymbol.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eHarnessConnector));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         HarnessConnector := SpatialIterator.FirstSchObject;
         While HarnessConnector <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(HarnessConnector.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                HarnessConnector.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(HarnessConnector.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            HarnessConnector := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check Power Ports for selection
   If (CheckBoxPowerPort.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(ePowerObject));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         PowerPort := SpatialIterator.FirstSchObject;
         While PowerPort <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(PowerPort.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                PowerPort.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(PowerPort.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            PowerPort := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Check Net Labels for selection
   If (CheckBoxNetLabel.Checked) then
   begin
      SpatialIterator := CurrentSheet.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(eNetLabel));
         SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
         //SpatialIterator.addfilt

         NetLabel := SpatialIterator.FirstSchObject;
         While NetLabel <> Nil Do
         Begin
            SchServer.RobotManager.SendMessage(NetLabel.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                NetLabel.Selection := True;    // write new designator to the component
            SchServer.RobotManager.SendMessage(NetLabel.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            NetLabel := SpatialIterator.NextSchObject;
         End;
      Finally
         CurrentSheet.SchIterator_Destroy(SpatialIterator);
      End;
   end;

   // Post Process:
   SchServer.ProcessControl.PostProcess(CurrentSheet, '');

   WriteToIniFile(IniFileName);

   RectangleForm.Close;
end;

procedure TRectangleForm.RectangleFormCreate(Sender: TObject);
begin
   IniFileName := 'C:\Users\Petar\AppData\Local\Altium\SelectTouchingRectangle.ini';
   ReadFromIniFile(IniFileName);   // Reading condition of checkboxes

   modeofwork := Start;
end;

procedure TRectangleForm.ButtonCloseClick(Sender: TObject);
begin
   RectangleForm.Close;
end;

procedure TRectangleForm.ButtonOKClick(Sender: TObject);
begin
   RectangleForm.Hide;
   modeofwork := Start;
end;
