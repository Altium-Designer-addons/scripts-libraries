
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

' Altium®, Altium Designer®, Altium Vault®, Autotrax®, Camtastic®, CircuitMaker®, CircuitStudio®, Codemaker™, DXP™, Easytrax®, LiveDesign®, NanoBoard®, PCBWORKS™, P-CAD®, Protel®, TASKING® and their respective logos are trademarks or registered trademarks of Altium Limited or its subsidiaries. See the full Copyright at http://www.altium.com/copyrights-and-trademarks and the EULA at http://www.altium.com/eula .
' All other registered or unregistered trademarks referenced herein are the property of their respective owners.

' ------------------------------------------------------------------------------
' Setup requirements
' ------------------
'  * You must create a directory "C:\AltiumDesigner_Config\Scripts" on your
'    system where you would store the "Libraries" folder containing the script
'    dependencies.
'
' ------------------------------------------------------------------------------
' Compatibility concerns
' ----------------------
'  * The current version of this script is tested and works well with:
'     -> Altium Designer 15.0.15 build 41991
'     -> Altium Designer 14.3.17 build 42447
'  * The current version of this script is known to work under Windows 7 64-bit.
'
' ------------------------------------------------------------------------------
' How to use this script
' ----------------------
'  1) From Altium Designer, open a file that is under version control.
'  2) Modify the document, then save it.
'  3) Run this script by the "DXP > Run Script" command and select
'     BetterRevert, or use a dedicated menu button.
'     Be sure that the visible document at the time you run the script
'     is a document that you wish to revert.



' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------



' Function to include an external file
Sub IncludeAbsolute (FilePathAbsolute)
  Dim oFSO, File, FileContent
  Set oFSO = CreateObject("Scripting.FileSystemObject")
  On Error Resume Next
  If oFSO.FileExists(FilePathAbsolute) Then
    Set File = oFSO.OpenTextFile(FilePathAbsolute)
    FileContent = File.ReadAll
    File.Close
    ExecuteGlobal FileContent ' this line executes the VBScript code of the included file
  End If
  On Error Goto 0
  Set File = Nothing
  Set oFSO = Nothing
End Sub

IncludeAbsolute "C:\AltiumDesigner_Config\Scripts\Libraries\Lib_AltiumFunctions.vbs"



' --------------------------------------------------------------------------------



' Function to add a "close then re-open" feature to the Revert function
Sub BetterRevert
  Dim CurDocKind
  Dim CurDocPath
  
  ' Check if Client is available
  If Client Is Nothing Then Exit Sub
  ' Check if GetWorkspace is available
  If GetWorkspace Is Nothing Then Exit Sub

  ' do the revert on the file (this function unfortunately does not immediately show the reverted modifications)
  Call ServerRunProcessSend("VersionControl:VersionControl", "ObjectKind=FocusedDocument|Action=Revert")

  ' save the document information
  CurDocKind = GetWorkspace.DM_FocusedDocument.DM_DocumentKind
  CurDocPath = GetWorkspace.DM_FocusedDocument.DM_FullPath
  ' close the document
  Client.CloseDocument( Client.OpenDocument(CurDocKind, CurDocPath) )
  ' re-open the document
  Client.ShowDocument( Client.OpenDocument(CurDocKind, CurDocPath) )
End Sub
