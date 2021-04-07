# Easy to use calculator for converting units from/to the metric/imperial systems.

Originally developped by Cyril Andreattta

Improved by Justin MASSIOT

## How to use it
  1. Enter a numeric value including its unit [in, mil, mm, um], e.g. 3.1415 mm
  1. Check the "copy to clipboard" checkbox if you want to directly paste the result somewhere.
  1. Press enter or click the small 'Convert' button.

![Image capture of mil_mm_conv](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Scripts%20-%20Misc/mil-mm-conv/mil-mm-conv.png)

## Cool features

### Since version 1.0:
* Converted values will be shown in all 4 units (inch support added by version 2.0).
* Spaces between number and unit are ignored.
* '.' and ',' are accepted as decimal separator.
* If unit is missing, last unit is used. This makes it easy to calculate multiple values.

### And more with version 2.0:
* The 'inch' unit is supported.
* The result can be automatically copied to the Windows clipboard, in order to paste it wherever you want.
* The input value is shown with a foreground color, and the copied value with a background color.
* The converter is fully customizable for your own needs, see [the dedicated section](#customizing-the-converter).

## Customizing the converter

Inside the file mil-mm-conv.pas you will find a series of constants that you can modify to fit your needs:
* floatformat: defines the format of the decimal results; default is '0.####' (round to the fourth decimal, no trailing zeros but with a leading zero before the separator), leave this empty if you do not want to round the results
* copybydefault: choose to copy the result to clipboard by default at startup; default is 'true' = copy by default
* copymetric: choose the result to copy to clipboard after a conversion from imperial to metric; could be 'mm' or 'um', default is 'mm'
* copyimperial: choose the result to copy to clipboard after a conversion from metric to imperial; could be 'mil' or 'in', default is 'mil'
* copywithunit: choose to copy the raw value, or the value followed by the unit; default is 'false' = no unit
* inputcolor: define the font color of the value which is the input
* copycolor: define the background color of the result which has been copied to the clipboard; default is clSkyBlue, other proposals are clActiveCaption, clHighlight

```delphi
const
    floatformat = '0.####'; // configuration: define the format of the decimal results; default is '0.####' (round to the fourth decimal, no trailing zeros but with a leading zero before the separator), leave this empty if you do not want to round the results
    copybydefault = true; // configuration: choose to copy the result to clipboard by default at startup; default is 'true' = copy by default
    copymetric = 'mm'; // configuration: choose the result to copy to clipboard after a conversion from imperial to metric; could be 'mm' or 'um', default is 'mm'
    copyimperial = 'mil'; // configuration: choose the result to copy to clipboard after a conversion from metric to imperial; could be 'mil' or 'in', default is 'mil'
    copywithunit = true; // configuration: choose to copy the raw value, or the value followed by the unit; default is 'false' = no unit
    inputcolor = clBlue; // configuration: define the font color of the value which is the input
    copycolor = clSkyBlue; // configuration: define the background color of the result which has been copied to the clipboard; default is clSkyBlue, other proposals are clActiveCaption, clHighlight
```