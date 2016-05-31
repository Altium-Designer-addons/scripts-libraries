uses libMessage, libUtils, OpenLibSubroutines, SchLibOpenFootprintInteractive;

(*
  OpenLib.pas
   
  summary:
    Opens the library of the selected component either in Schematic, PCB or SchLib environment.
      PCB:    Open PCBLib and show footprint
      Sch:    Open SchLib and show component
      SchLib: Show selector for currently present footprints and open PCBLib and footprint for selection
    
  usage:
    call OpenLib procedure with while the component is selected.
    
  known limitations:
    currently only tested with IntLibs. Might have issues with SVNLibs, DBLibs and SVNDBlibs as well as vault components.
    Is a bit verbose on the messages panel, to be adjustable in future releases
    
  changelog:
    2016-04-18 v1.0: Initial release

*)
Procedure OpenLib;
var
        MM          : IMessagesManager;
        WS          : IWorkspace;

        Doc             : IDocument;
        Sch             : ISch_Document;
        PCB             : IPCB_Board;
        
begin
        WS := GetWorkSpace;
        MM := WS.DM_MessagesManager;
        
        AddMessage(MM, 'Script Start','OpenLib');

    // determine environment
        if (WS.DM_FocusedDocument <> Nil) then
        begin
            Doc := WS.DM_FocusedDocument;

            AddMessage(MM, 'Script Info','Found Document kind = '+Doc.DM_DocumentKind);
            
            // call sub procedure

            If ((AnsiCompareStr(UpperCase(Doc.DM_DocumentKind), 'PCB') = 0)) then
            begin
                AddMessage(MM, 'Script Info','PCB subcall');

                OpenLib_PCBComponent(WS,MM);

            end
            else if ((AnsiCompareStr(UpperCase(Doc.DM_DocumentKind), 'PCBLIB') = 0)) then
            begin
                AddMessage(MM, 'Script Info','PCBLIB subcall');
            end
            else if ((AnsiCompareStr(UpperCase(Doc.DM_DocumentKind), 'SCH') = 0)) then
            begin
                AddMessage(MM, 'Script Info','SCH subcall');

                OpenLib_SchComponent(WS,MM);
            end
            else if ((AnsiCompareStr(UpperCase(Doc.DM_DocumentKind), 'SCHLIB') = 0)) then
            begin
                AddMessage(MM, 'Script Info','SCHLIB subcall');
                
                FrmModelList.Show;
            end
            else
            begin
                AddWarning(MM, 'Script Exit','Unknown or unsupported document type.');
                exit;
            end;
            
            
        end;

        AddMessage(MM, 'Script End','OpenLib');


end;
