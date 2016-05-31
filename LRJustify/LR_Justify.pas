//LR_Justify Script: by John M. Go-Soco
//  Note:  This script is intended for use in the Schematic editor. It is best used when assigned to a shortcut key. 
//  Assign it first to a button/icon:
//   Process: ScriptingSystem:RunScriptFile
//   Parameters: Filename = <FULL FILE PATH HERE> | Procname = LR_Justify
//  then assign it a shortcut key.
//  USAGE: Select some text, activate the script, and it will swap it from left justification to right justification.

Function Modify_Begin(TObject);
begin
    SchServer.RobotManager.SendMessage(TObject.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
end;

Function Modify_End(TObject);
begin
    SchServer.RobotManager.SendMessage(TObject.I_ObjectAddress, c_BroadCast, SCHM_EndModify, c_NoEventData);
end;

procedure LR_Justify;
  Var
    sk       :  ISCH_Document;
    txt      :  ISch_GraphicalObject;
    iterator :  ISch_Iterator;
    firstloc :  IDispatch;
  Begin
    //Check we're in a schematic
      sk := SCHServer.GetCurrentSchDocument;
      If sk = Nil then Exit;
      SchServer.ProcessControl.PreProcess(sk, '');

    //Find the bloody selections
      iterator := sk.schIterator_Create;
      iterator.SetState_FilterAll;
      iterator.AddFilter_ObjectSet(mkSet(eDesignator,eParameter,eLabel,eNetLabel));
      txt := iterator.FirstSCHObject;

    //Choose Text Object
      While txt <> Nil Do
            begin
                 if txt.selection = true then
                    begin
                        Modify_Begin(txt);  					
                      case txt.justification of
						eJustify_BottomLeft     : txt.Justification := eJustify_BottomRight;
						eJustify_BottomRight    : txt.Justification := eJustify_BottomLeft;
						eJustify_CenterLeft     : txt.Justification := eJustify_CenterRight;
						eJustify_CenterRight    : txt.Justification := eJustify_CenterLeft;
						eJustify_TopLeft        : txt.Justification := eJustify_TopRight;
						eJustify_TopRight       : txt.Justification := eJustify_TopLeft;
                      end;
						//txt.Autoposition := False;
						Modify_End(txt);					  
                    end;
              txt := iterator.NextSchObject;
            end;
      sk.SchIterator_Destroy(iterator);
      SchServer.ProcessControl.PostProcess(sk, '');
      ResetParameters;
      AddStringParameter('Action','Redraw');
      RunProcess('SCH:Zoom');
  End;
