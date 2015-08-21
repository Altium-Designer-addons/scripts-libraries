
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



' Library: File management functions
' To use them, you need to include this file in your script, or simply copy/paste the functions in the content of your file.



' check if a file exists and return True if it does; return False and show an error if it does not
Function FileExistsOrShowError (FilePath)
  If TypeName(FilePath) <> "String" Or FilePath = "" Then
    FileExistsOrShowError = False
    Exit Function
  End If
  
  If Not FileExists(FilePath) Then
    ShowError "Please save the file '" & FilePath & "' on your disk before trying to operate on it."
    FileExistsOrShowError = False
  Else
    FileExistsOrShowError = True
  End If
End Function



' extract the file name from a file path (relative or absolute path)
Function GetFileNameFromPath (FilePath)
  If TypeName(FilePath) <> "String" Or FilePath = "" Then
    GetFileNameFromPath = ""
    Exit Function
  End If
  
  If FileExists(FilePath) Then
    Set objFSO = CreateObject("Scripting.FileSystemObject")
    Set objFile = objFSO.GetFile(FilePath)
    GetFileNameFromPath = objFSO.GetFileName(objFile)
  Else
    GetFileNameFromPath = ""
  End If
End Function



' extract the parent path from a file path (relative or absolute path)
Function GetParentFullPathFromFilePath (FilePath)
  If TypeName(FilePath) <> "String" Or FilePath = "" Then
    GetParentFullPathFromFilePath = ""
    Exit Function
  End If
  
  If FileExists(FilePath) Then
    Set objFSO = CreateObject("Scripting.FileSystemObject")
    Set objFile = objFSO.GetFile(FilePath)
    GetParentFullPathFromFilePath = objFile.ParentFolder
  Else
    GetParentFullPathFromFilePath = ""
  End If
End Function
