{-----------------------------------------------}
{ Easy to use calculator for converting units.  }
{                                               }
{ Enter a numeric value including unit...       }
{ [mm, in, mil, um] -> e.g. 3.1415 mm           }
{ ...then press enter (or the 'Convert' button).}
{ Converted values will be shown in all units.  }
{ Spaces between number and unit are ignored.   }
{ '.' and ',' are accepted as decimal separator.}
{ If unit is missing, last unit is used. This   }
{ makes it easy to calculate multiple values.   }
{                                               }
{ Author: cyril@andreatta.ch                    }
{ Version: 2.0                                  }
{ Improved by:                                  }
{   Justin MASSIOT,                             }
{   m [dot] just1 !at! free [dot] fr            }
{-----------------------------------------------}
{                                               }
{ Changelog:                                    }
{                                               }
{ -----                                         }
{ 2.0                                           }
{ Improvements by Justin MASSIOT                }
{ * New unit 'in' (inch) available              }
{ * Copy to clipboard automatically             }
{ * Parameters for user configuration           }
{ * New GUI, InitCreate() and InitShow()        }
{                                               }
{ -----                                         }
{ 1.0                                           }
{ Original version by Cyril ANDREATTA           }
{                                               }
{-----------------------------------------------}

//uses ClipBrd;

const
    floatformat = '0.####'; // configuration: define the format of the decimal results; default is '0.####' (round to the fourth decimal, no trailing zeros but with a leading zero before the separator), leave this empty if you do not ant to round the results
    copybydefault = true; // configuration: choose to copy the result to clipboard by default at startup; default is 'true' = copy by default
    copymetric = 'mm'; // configuration: choose the result to copy to clipboard after a conversion from imperial to metric; could be 'mm' or 'um', default is 'mm'
    copyimperial = 'mil'; // configuration: choose the result to copy to clipboard after a conversion from metric to imperial; could be 'mil' or 'in', default is 'mil'
    copywithunit = true; // configuration: choose to copy the raw value, or the value followed by the unit; default is 'false' = no unit
    inputcolor = clBlue; // configuration: define the font color of the value which is the input
    copycolor = clSkyBlue; // configuration: define the background color of the result which has been copied to the clipboard; default is clSkyBlue, other proposals are clActiveCaption, clHighlight

var
    lastunit:String; // global variable to store last unit

procedure TfrmCalc.btnCalcClick(Sender: TObject);
var
    i       :Integer;
    value   :String;
    numstr  :String;
    unitstr :String;
    num     :Float;

begin
    i:=1;
    numstr:='';
    unitstr:='';
    
    // reset the TEdit font colours
    edum.Font.Color := clWindowText;
    edmm.Font.Color := clWindowText;
    edinch.Font.Color := clWindowText;
    edmil.Font.Color := clWindowText;
    // reset the TEdit box background colours
    edum.Color := clMenu;
    edmm.Color := clMenu;
    edinch.Color := clMenu;
    edmil.Color := clMenu;
    // reset the clipboard help text visibility
    lblClipBrd.Visible := false;
    //chbClipBrd.Checked := true;
    
    // ---------- ---------- ---------- ----------

    // read value from textbox
    value := edInput.Text;
    if value = '' then exit;

    // extract numeric value
    while (((value[i] >= '0') and
          (value[i] <= '9')) or
          (value[i] = '.') or
          (value[i] = ',')) and
          (i <= length(value)) do
    begin
        numstr:=numstr + value[i];
        i:=i + 1;
    end;
    // check if it is a real number
    try
        if ( (value[1] >= '0') and (value[1] <= '9') ) then num:=StrToFloat(numstr);
    except
        exit
    end;

    // ignore spaces between number and unit
    while (value[i] = ' ') do
        i:=i+1;

    // extract unit
    while (value[i] >= 'a') and (value[i] <= 'z') do
    begin
        unitstr:=unitstr + value[i];
        i:=i+1;
    end;

    // if unit is missing try the same as for last calculation
    if unitstr = '' then
        unitstr:=lastunit;
    
    // ---------- ---------- ---------- ----------

    case unitstr of
    'in':
        begin
        edinch.Font.Color := inputcolor; // set a colour to the input value
        edum.Text:=FormatFloat(floatformat, num * 25400);
        edmm.Text:=FormatFloat(floatformat, num * 25.4);
        edinch.Text:=FormatFloat(floatformat, num);
        edmil.Text:=FormatFloat(floatformat, num * 1000);
        end;
    'mil':
        begin
        edmil.Font.Color := inputcolor; // set a colour to the input value
        edum.Text:=FormatFloat(floatformat, num * 25.4);
        edmm.Text:=FormatFloat(floatformat, num * 0.0254);
        edinch.Text:=FormatFloat(floatformat, num / 1000);
        edmil.Text:=FormatFloat(floatformat, num);
        end;
    'mm':
        begin
        edmm.Font.Color := inputcolor; // set a colour to the input value
        edum.Text:=FormatFloat(floatformat, num * 1000);
        edmm.Text:=FormatFloat(floatformat, num);
        edinch.Text:=FormatFloat(floatformat, num / 25.4);
        edmil.Text:=FormatFloat(floatformat, num / 0.0254);
        end;
    'um':
        begin
        edum.Font.Color := inputcolor; // set a colour to the input value
        edum.Text:=FormatFloat(floatformat, num);
        edmm.Text:=FormatFloat(floatformat, num / 1000);
        edinch.Text:=FormatFloat(floatformat, num / 25400);
        edmil.Text:=FormatFloat(floatformat, num / 25.4);
        end;
    else
        // unknown value
        ShowMessage('I can''t understand what you sayin...' + #10 +
                    'Enter a numeric value followed by either ''in'', ''mil'', ''mm'' or ''um''' + #10 +
                    'e.g: 12.3 mil');
    end;
    
    // ---------- ---------- ---------- ----------
    
    // copy the result to the clipboard (if the user wants to)
    //ShowMessage(chbClipBrd.Checked);
    if (chbClipBrd.Checked = true) then
    begin
        if (unitstr = 'mil') or (unitstr = 'in') then // imperial -> metric conversion
            begin
            if (copymetric = 'um') then // copy the 'um' result
                begin
                edum.Color := copycolor; // set a background colour to the value that will be copied to the clipboard
                lblClipBrd.Visible := true; // set the clipboard help text visible
                if (copywithunit = true) then
                    Clipboard.AsText := edum.Text + 'µm'
                else
                    begin
                    edum.SelectAll;
                    edum.CopyToClipboard;
                    end;
                end
            else // copy the 'mm' result by default
                begin
                edmm.Color := copycolor; // set a background colour to the value that will be copied to the clipboard
                lblClipBrd.Visible := true; // set the clipboard help text visible
                if (copywithunit = true) then
                    Clipboard.AsText := edmm.Text + 'mm'
                else
                    begin
                    edmm.SelectAll;
                    edmm.CopyToClipboard;
                    end;
                end;
            end
        else if (unitstr = 'mm') or (unitstr = 'um') then // metric -> imperial conversion
            begin
            if (copyimperial = 'in') then // copy the 'in' result
                begin
                edinch.Color := copycolor; // set a background colour to the value that will be copied to the clipboard
                lblClipBrd.Visible := true; // set the clipboard help text visible
                if (copywithunit = true) then
                    Clipboard.AsText := edinch.Text + 'inch'
                else
                    begin
                    edinch.SelectAll;
                    edinch.CopyToClipboard;
                    end;
                end
            else // copy the 'mil' result by default
                begin
                edmil.Color := copycolor; // set a background colour to the value that will be copied to the clipboard
                lblClipBrd.Visible := true; // set the clipboard help text visible
                if (copywithunit = true) then
                    Clipboard.AsText := edmil.Text + 'mil'
                else
                    begin
                    edmil.SelectAll;
                    edmil.CopyToClipboard;
                    end;
                end;
            end;
        // else: the specified unit is not within the valid list of units
        
        // now set the "clipboard label" to show the text contained in the clipboard
        lblClipBrd.Caption := 'Clipboard = ' + Clipboard.AsText;
    end;
    // else: the user does not want to copy the result to the clipboard
    
    // ---------- ---------- ---------- ----------

    lastunit:=unitstr;

    // clear input text
    edInput.Text:='';
    // show the unit in the GUI
    lblUnit.Caption:=lastunit;
end;

procedure TfrmCalc.btnCloseClick(Sender: TObject);
begin
    Close;
end;
  
procedure TfrmCalc.InitCreate(Sender: TObject); // OnCreate event triggered when the TForm is created (http://delphi.about.com/od/formsdialogs/a/delphiformlife.htm)
                                                // see the *.dfm file for OnCreate -> InitCreate function call association
begin
    // set default states for the clipboard controls and info
    lblClipBrd.Visible := false;
    chbClipBrd.Checked := copybydefault;
end;
  
procedure TfrmCalc.InitShow(Sender: TObject); // OnShow event triggered when the ShowModal procedure is called (http://delphi.about.com/od/formsdialogs/a/delphiformlife.htm)
                                              // see the *.dfm file for OnShow -> InitShow function call association
begin
    //
end;

procedure ShowCalc;
begin
    frmCalc.ShowModal();
end;


