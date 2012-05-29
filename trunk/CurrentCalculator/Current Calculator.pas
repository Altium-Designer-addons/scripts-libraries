Function isMidLayerFunc(LiD : integer):Boolean;
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
        if base = 0 then result := 0;
    end;



Procedure CurrentCalc;
//Current Calculator Script
//by John Michael Go-Soco (ETL Systems Ltd)
//Installation: 
//  In PCB Editor, create a button which uses ScriptingSystem:RunScriptFile process
//  Parameters: Filename = <insert file name/path here> | Procname = CurrentCalc
//Usage:
//  Click the button and select a track or arc on the PCB. A dialog box will appear and tell you what that track's
//  current capacity is, for 1°C rise, 5°C rise, and 10°C rise.
//Standards:
//  IPC-2221 (from CircuitCalculator blog)
// v1.0: 21/07/11
// v1.2: 22/07/11 Removed 0.5oz plating section
// v2.0: 25/05/12 Added 0.5oz Plating Section back in for clarity, made it removable via a switch
// v2.1: 25/05/12 Added resistance calculations!

    Var
       h : TCoord; //Assign to Layer Thickness
       z : Double; //Area
       I_1,I_5,I_10,I_20 : Double; //Vanilla Currents per temp range
       I4,I5,I6 : Double; //Current per given temperature range
       w,wz : double; //Width of Track
       k,c,b : double; //IPC-2221 Constants
       tk_1,tk_5,tk_10,tk_20 : double; //Temperature Rise times Constant
       Board : IPCB_Board; //Current PCB
       LStack : IPCB_Layerstack; //Layer Stack
       MyTrack : IPCB_Track; //Track you want to query
       L1 : IPCB_LayerObject; //Use as Current Layer
       isMidLayer : Boolean; //Use LayerUtils to determine if it is a midlayer
       isMidString : string;
       header_string,vanilla_string,unplated_string : string; //messagestrings;
       u : integer; //Board Unit
       Remove_Plating : Boolean; //Use this to add plating removal calculation
       Wire_Length : Double ; // Track/Arc Length Calculation
       Wire_Length_String : String ; //Wire Length container
       OutputString : string;
       Cu_rho : double; //Electrical Resistivity
       Cu_Temp_Co : double; //Temperature Coefficient
       Res : string; // Resistance


    Begin
       //<Declarations>
        Board := PCBServer.GetCurrentPCBBoard;
        LSTack := Board.LayerStack;
        u := Board.Displayunit;
        case u of
             1 : u := 0;
             0 : u := 1;
        end;

        //NOTE: If you use final plated thickness in the Layer Stack Manager, and want to show the extra plating removed in thge
        Remove_Plating := False;

        //assign some constants
         c := 0.725;
         b := 0.44;
         Cu_Rho := 67 * 0.00001;

         Cu_Temp_Co := 0.039; //ohm/ohm/C



        MyTrack := Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject),AllLayers,'Select track');
        while Mytrack <> Null do
        begin

                L1 := Lstack.LayerObject(Mytrack.Layer); //Get layer object for Track
                h := coordToMils(L1.CopperThickness); //Get Layer Thickness (for h) in mils
                Case MyTrack.ObjectID of

                     eTrackObject :
                      begin
                           wz := MyTrack.Width; //Get track width in Mils
                             Wire_Length := sqrt(pow(MyTrack.x2 - MyTrack.x1,2) + pow(myTrack.y2 - MyTrack.Y1,2)); //GET WIRE LENGTH
                      end;

                     eArcObject :
                      begin
                           wz := MyTrack.LineWidth;
                              Wire_Length := ((MyTrack.StartAngle - MyTrack.EndAngle)/360)* pi * MyTrack.Radius;
                       end;
                end;

                Wire_Length_String := f2u(Wire_Length,u);
                Wire_Length := coordToMils(Wire_Length);
                w := coordToMils(wz);

                isMidlayer := isMidLayerFunc(MyTrack.Layer);
                If isMidlayer = True
                   then isMidString := ' (Internal Layer)'
                   else isMidString := ' (External Layer)';

                //Assign value for k; 0.024 if internal, 0.048 if external
                if (isMidlayer = True)
                   then k := 0.024
                   else k := 0.048;

                //Get tk constant:
                    tk_1  := k*pow(1,b);
                    tk_5  := k*pow(5,b);
                    tk_10 := k*pow(10,b);
                    tk_20 := k*pow(20,b);

                //get track area section
                    z := pow((h * w),c);

                //get currents
                    I_1 := z * tk_1;
                    I_5 := z * tk_5;
                    I_10 := z * tk_10;
                    I_20 := z * tk_20;

                //0.5 Oz PLATING REMOVAL SECTION:
                //Note - This section REDUCES the layer thickness by 0.5 oz. This is if you add the layer plating already to the copper thickness in the stack manager
                unplated_string := '';
                If Remove_Plating = true then
                Begin
                  If isMidlayer = False then //Add calc/string for 1/2oz plating removal from thickness)
                    begin
                        z := pow(((h - 0.689) * w),c);
                        I4 := z * tk_1;
                        I5 := z * tk_5;
                        I6 := z * tk_10;

                         unplated_string := '0.5oz Plating REMOVED:';
                         unplated_string := unplated_string + #13#10 + '      1 Degree Rise : ' + FloatToStrF(I4,0,5,4) + 'A';
                         unplated_string := unplated_string + #13#10 + '      5 Degree Rise : ' + FloatToStrF(I5,0,5,4) + 'A';
                         unplated_string := unplated_string + #13#10 + '     10 Degree Rise : ' + FloatToStrF(I6,0,5,4) + 'A';
                    end;
                END;

        //RESISTANCE CALCULATIONS (for 25°C)
                    Res := Cu_Rho * Wire_Length/(h*w) * 1000;

                //Parse Results:
                header_string := 'Track Width:=' + f2u(wz,u) + #13#10 + 'Copper Thickness:=' + f2u(L1.CopperThickness,u);
                header_string := header_string + ' (' + FloatToStrF(L1.Copperthickness/(27600/2),ffgeneral,2,1) + 'oz/ft^2)';
                header_string := header_string + #13#10 + 'Selected Wire Length:=' + Wire_Length_String;
                header_string := header_string + #13#10 + 'Track Layer:=' + L1.Name + isMidString;
                header_string := header_string + #13#10 + 'Resistance:=' + FloatToStrF(Res,0,4,5) + ' milli-ohm';

                vanilla_string := 'Calculated Current Capacity:';
                vanilla_string := vanilla_string + #13#10 + '      1 Degree Rise : ' + FloatToStrF(I_1,0,5,4) + 'A';
                vanilla_string := vanilla_string + #13#10 + '      5 Degree Rise : ' + FloatToStrF(I_5,0,5,4) + 'A';
                vanilla_string := vanilla_string + #13#10 + '     10 Degree Rise : ' + FloatToStrF(I_10,0,5,4) + 'A';
                vanilla_string := vanilla_string + #13#10 + '     20 Degree Rise : ' + FloatToStrF(I_20,0,5,4) + 'A';
                vanilla_string := vanilla_string + #13#10 + #13#10;

     //Show Message Dialog:
               OutputString := header_string +#13#10+#13#10+ vanilla_string + unplated_string ;
                 z := MessageDlg(OutputString,mtinformation,4,0);

        MyTrack := Board.GetObjectAtCursor(MkSet(eTrackObject,eArcObject),AllLayers,'Select track'); //while loop
        end;

    End;
