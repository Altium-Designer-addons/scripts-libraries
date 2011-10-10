//LR_Justify Script: by John M. Go-Soco v1.0
//  Note:  This script is intended for use in the Schematic editor. It is best used when assigned to a shortcut key. 
//  Assign it first to a button/icon:
//   Process: ScriptingSystem:RunScriptFile
//   Parameters: Filename = <FULL FILE PATH HERE> | Procname = LR_Justify
//  then assign it a shortcut key.
//  USAGE: Select some text, activate the script, and it will swap it from left justification to right justification.

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
      iterator.AddFilter_ObjectSet(mkSet(eDesignator,eParameter));
      txt := iterator.FirstSCHObject;

    //Choose Text Object
      While txt <> Nil Do
            begin
                 if txt.selection = true then
                    begin
                      case txt.justification of
                        akLeft  : txt.justification := akRight;
                        akRight : txt.justification := akLeft;
                      end;
                      //txt.Autoposition := False;
                    end;
              txt := iterator.NextSchObject;
            end;

      sk.SchIterator_Destroy(iterator);
      SchServer.ProcessControl.PostProcess(sk, '');
      ResetParameters;
      AddStringParameter('Action','Redraw');
      RunProcess('SCH:Zoom');
  End;
