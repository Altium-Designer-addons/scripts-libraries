Function isMidLayerFunction(LiD : integer):Boolean;
    Begin
         If LiD <> 1 then
            If LiD <> 32 then Result := True
            Else Result := False
         Else Result := False;
    End;


Function f2u(number,u: double):string;
    Begin
         case u of
              0 : result := stripTrailingZeroes((FloatToStrF(number/10000,ffFixed,20,2))) + 'mil';
              1 : result := stripTrailingZeroes((FloatToStrF(number/10000*0.0254,ffFixed,20,4))) + 'mm';
         end;
    end;

// Simple Power function for Delphi Script by SMARTBEAR
Function pow(base,exponent: double):double;
    Begin
        result := Exp(Exponent*Ln(Base));
    end;


	
Procedure CurrentCalc;
//Current Calculator Script
//by John Michael Go-Soco
//Installation: 
//  In PCB Editor, create a button which uses ScriptingSystem:RunScriptFile process
//  Parameters: Filename = <insert file name/path here> | Procname = CurrentCalc
//Usage:
//  Simply click the button and select a track or arc on the PCB. A dialog box will appear and tell you what that track's
//  current capacity is, for 1°C rise, 5°C rise, and 10°C rise.
// v1.0: 21/07/2011
// v1.2: 22/07/2011 Removed 0.5oz plating section

    Var
       h : TCoord; //Assign to Layer Thickness
       z : Double; //Area
       I1,I2,I3,I4,I5,I6 : Double; //Current per given temperature range
       w,wz : double; //Width of Track
       k,c,b : double; //IPC-2221 Constants
       tk1,tk2,tk3 : double; //Temperature Rise times Constant
       Board : IPCB_Board; //Current PCB
       LStack : IPCB_Layerstack; //Layer Stack
       MyTrack : IPCB_Track; //Track you want to query
       L1 : IPCB_LayerObject; //Use as Current Layer
       isMidLayer : Boolean; //Use LayerUtils to determine if it is a midlayer
       isMidString : string;
       str1,str2,str3 : string; //messagestrings
       u : integer; //Board Unit

    Begin
       //<Declarations>
        Board := PCBServer.GetCurrentPCBBoard;
        LSTack := Board.LayerStack;
        u := Board.Displayunit;
        case u of
             1 : u := 0;
             0 : u := 1;
        end;

        //assign some constants
         c := 0.725;
         b := 0.44;


        MyTrack := Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject),AllLayers,'Select track');
        while Mytrack <> Null do
        begin

                L1 := Lstack.LayerObject(Mytrack.Layer); //Get layer object for Track
                h := coordToMils(L1.CopperThickness); //Get Layer Thickness (for h) in mils
                Case MyTrack.ObjectID of
                     eTrackObject : wz := MyTrack.Width; //Get track width in Mils
                     eArcObject :  wz := MyTrack.LineWidth;
                end;

                w := coordToMils(wz);

                isMidlayer := isMidLayerFunction(MyTrack.Layer);
                If isMidlayer = True
                   then isMidString := ' (Internal Layer)'
                   else isMidString := ' (External Layer)';

                //Assign value for k; 0.024 if internal, 0.048 if external
                if (isMidlayer = True)
                   then k := 0.024
                   else k := 0.048;

                //Get tk constant:
                    tk1 := k*pow(1,b);
                    tk2 := k*pow(5,b);
                    tk3 := k*pow(10,b);

                //get track area section
                    z := pow((h * w),c);

                //get currents
                    I1 := z * tk1;
                    I2 := z * tk2;
                    I3 := z * tk3;

                //Parse Results:
                str1 := 'Track Width:=' + f2u(wz,u) + #13#10 + 'Copper Thickness:=' + f2u(L1.CopperThickness,u);
                str1 := str1 + ' (' + FloatToStrF(L1.Copperthickness/(27600/2),ffgeneral,2,1) + 'oz/ft^2)';
                str1 := str1 + #13#10 + 'Track Layer:=' + L1.Name + isMidString;

                str2 := '      1 Degree Rise : ' + FloatToStrF(I1,0,5,4) + 'A';
                str2 := str2 + #13#10 + '      5 Degree Rise : ' + FloatToStrF(I2,0,5,4) + 'A';
                str2 := str2 + #13#10 + '     10 Degree Rise : ' + FloatToStrF(I3,0,5,4) + 'A';

                z := MessageDlg(str1+#13#10+str2,mtinformation,4,0);

        MyTrack := Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject),AllLayers,'Select track'); //while loop
        end;

    End;