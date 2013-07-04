{-----------------------------------------------}
{ Easy to use calculator for converting units.  }
{                                               }
{ Enter a numeric value including unit...       }
{ [mm, mil, um] -> e.g. 3.1415 mm               }
{ ...then press enter (or the small 'c' button).}
{ Converted values will be shown in all units.  }
{ Spaces between number and unit are ignored.   }
{ '.' and ',' are accepted as decimal separator.}
{ If unit is missing, last unit is used. This   }
{ makes it easy to calculate multiple values.   }
{                                               }
{ Author: cyril@andreatta.ch                    }
{ Version: 1.0                                  }
{-----------------------------------------------}

var
	lastunit:String; // global variable to store last unit

procedure TfrmCalc.btnCalcClick(Sender: TObject);
var
	i		:Integer;
	value   :String;
    numstr	:String;
    unitstr :String;
    num		:Float;

begin
	i:=1;
    numstr:='';
    unitstr:='';

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
    	num:=StrToFloat(numstr);
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

    case unitstr of
    'mm':
    	begin
        edum.Text:=FloatToStr(num * 1000);
        edmm.Text:=FloatToStr(num);
        edmil.Text:=FloatToStr(num / 0.0254);
        end;
    'mil':
    	begin
        edum.Text:=FloatToStr(num * 25.4);
        edmm.Text:=FloatToStr(num * 0.0254);
        edmil.Text:=FloatToStr(num);
        end;
    'um':
    	begin
        edum.Text:=FloatToStr(num);
        edmm.Text:=FloatToStr(num / 1000);
        edmil.Text:=FloatToStr(num / 25.4);
        end;
    else
    	// unknown value
    	ShowMessage('I can''t understand what you sayin...' + #10 +
        			'Enter a numeric value followed by either ''mil'', ''mm'' or ''um''' + #10 +
                    'e.g:' + #10 +
                    '12 mil');
    end;

    lastunit:=unitstr;

    // clear input text
	edInput.Text:='';
end;

procedure TfrmCalc.btnCloseClick(Sender: TObject);
begin
    Close;
end;
