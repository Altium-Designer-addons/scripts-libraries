//toggle DRC online ON-OFF
//must be associated to a key
//(C) Juan Martinezs /captura electronica 2011

Procedure DRC_ON;
Var
PCBSystemOptions : IPCB_SystemOptions;
Begin

PCBSystemOptions := PCBServer.SystemOptions;
If PCBSystemOptions = Nil Then Exit;

If (PcbSystemOptions.DoOnlineDRC = False) Then
PcbSystemOptions.DoOnlineDRC := True
Else PcbSystemOptions.DoOnlineDRC := False ;

End;
