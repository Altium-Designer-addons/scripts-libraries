{..............................................................................}
{ Summary   This scripts can be used to run any programm from altium           }
{                                                                              }
{                                                                              }
{                                                                              }
{ Created by:     Ran Shahar                                                   }
{                                                                              }
{..............................................................................}



procedure TForm1.Button1Click(Sender: TObject);
var
 ErrorCode : Integer;
begin
 ErrorCode := RunApplication('C:\Program Files (x86)\Saturn PCB Design\PCB Toolkit V5\PCB Toolkit V5.65.exe');
  If ErrorCode <> 0 Then
   ShowError('System cannot start PCB Toolkit ' + GetErrorMessage(ErrorCode));
  end;

procedure TForm1.Button2Click(Sender: TObject);
var
 ErrorCode1 : Integer;
begin
 ErrorCode1 := RunApplication('C:\apps\traceSim\TraceSimNew.exe');
  If ErrorCode1 <> 0 Then
   ShowError('System cannot start TraceSimNew ' + GetErrorMessage(ErrorCode1));
end; 

procedure TForm1.Button3Click(Sender: TObject);
var
 ErrorCode2 : Integer;
begin
 ErrorCode2 := RunApplication('C:\work\applications\PCB assistance program\txline\txline.exe');
  If ErrorCode2 <> 0 Then
   ShowError('System cannot start txline ' + GetErrorMessage(ErrorCode2));
end;
