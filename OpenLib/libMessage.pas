

procedure AddMessage(MM : IMessagesManager; MClass : WideString; MText : WideString);
var
   ImageIndex   : Integer;
   F           : Boolean;
begin
        MM.BeginUpdate;

        F := False;
        ImageIndex := 164;
        If MM = Nil Then Exit;
        MM.AddMessage({MessageClass             } MClass,
                      {MessageText              } MText,
                      {MessageSource            } '',
                      {MessageDocument          } '',
                      {MessageCallBackProcess   } '',
                      {MessageCallBackParameters} '',
                      ImageIndex,
                      F);
        MM.EndUpdate;
end;


procedure AddError(MM : IMessagesManager; MClass : WideString; MText : WideString);
var
   ImageIndex   : Integer;
   F           : Boolean;
begin
        MM.BeginUpdate;

        F := False;
        ImageIndex := 165;
        If MM = Nil Then Exit;
        MM.AddMessage({MessageClass             } MClass,
                      {MessageText              } MText,
                      {MessageSource            } '',
                      {MessageDocument          } '',
                      {MessageCallBackProcess   } '',
                      {MessageCallBackParameters} '',
                      ImageIndex,
                      F);
        MM.EndUpdate;
end;


procedure AddWarning(MM : IMessagesManager; MClass : WideString; MText : WideString);
var
   ImageIndex   : Integer;
   F           : Boolean;
begin
        MM.BeginUpdate;

        F := False;
        ImageIndex := 163;
        If MM = Nil Then Exit;
        MM.AddMessage({MessageClass             } MClass,
                      {MessageText              } MText,
                      {MessageSource            } '',
                      {MessageDocument          } '',
                      {MessageCallBackProcess   } '',
                      {MessageCallBackParameters} '',
                      ImageIndex,
                      F);
        MM.EndUpdate;
end;
