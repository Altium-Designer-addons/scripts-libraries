
' Copyright (C) 2015 Justin MASSIOT
' Author: Justin MASSIOT ( m [dot] just1 !at! free [dot] fr )

' ============================================================================ '
'                                                                              '
'    This program is free software: you can redistribute it and/or modify      '
'    it under the terms of the GNU General Public License as published by      '
'    the Free Software Foundation, either version 3 of the License, or         '
'    (at your option) any later version.                                       '
'                                                                              '
'    This program is distributed in the hope that it will be useful,           '
'    but WITHOUT ANY WARRANTY; without even the implied warranty of            '
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the              '
'    GNU General Public License for more details.                              '
'                                                                              '
'    You should have received a copy of the GNU General Public License         '
'    along with this program. If not, see <http://www.gnu.org/licenses/>.      '
'                                                                              '
' ============================================================================ '

' Altium®, Altium Designer®, DXP™ and their respective logos are trademarks or registered trademarks of Altium Limited or its subsidiaries. See the full Copyright at http://www.altium.com/copyrights-and-trademarks and the EULA at http://www.altium.com/eula .
' All other registered or unregistered trademarks referenced herein are the property of their respective owners.

' ------------------------------------------------------------------------------



' VBScript workarounds and feature enhancements
' To use them, you need to include this file in your script, or simply copy/paste the functions in the content of your file.



' determine if a file exists
Function DoesFileExist (path)
  If TypeName(path) <> "String" Or path = "" Then
    DoesFileExist = False
    Exit Function
  End If
  
  Set fso = CreateObject("Scripting.FileSystemObject")
  If (fso.FileExists(path)) Then
    DoesFileExist = True
  Else
    DoesFileExist = False
  End If
End Function



' inline 'if' statement, replacement of a ternary operator
Function iIf (expression, valIfTrue, valIfFalse)
  If (expression) Then
    iIf = valIfTrue
  Else
    iIf = valIfFalse
  End If
End Function



' this is a workaround to replace the Try - Catch statement that does not exist in VBScript
Function TryCmd (StrCmd)
  If TypeName(StrCmd) <> "String" Or StrCmd = "" Then
    TryCmd = False
    Exit Function
  End If
  
  On Error Resume Next ' if an error occurs, discard it and go to the next instruction (= enable errors handling)
  Err.Clear ' clear previous errors
  ExecuteGlobal StrCmd ' execute (evaluate) the user command, which is a string
  If Err.Number <> 0 Then ' there is an error
    TryCmd = False
    'ShowWarning "Error: " & Err.Number & vbCrLf &_
    '            "Error (Hex): " & Hex(Err.Number) & vbCrLf &_
    '            "Source: " &  Err.Source & vbCrLf &_
    '            "Description: " &  Err.Description
  Else
    TryCmd = True
  End If
  On Error Goto 0 ' disables the 'On Error Resume Next' statement (= disable errors handling)
End Function
