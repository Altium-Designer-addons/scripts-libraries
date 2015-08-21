
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
' What does this script do
' -------------------
'  1) The script will try to VCS Revert the currently focused (visible) file.
'     (Unfortunately, Altium Designer does not refresh the file content after
'     a Revert.)
'  2) (Even if the previous command fails,) the script saves the current
'     document path, then closes the document.
'  3) It will then re-open the document without local modifications (reverted).
'  Of course, this script CANNOT work with a file that does not exist on
'    your hard drive because we need an existing file path to re-open it.
'  See the following web page to learn more about VCS in ALtium Designer:
'    http://techdocs.altium.com/display/ADOH/Version+Control+and+Altium+Designer
'
' ------------------------------------------------------------------------------
' How to use this script
' ----------------------
'  1) From Altium Designer, open a file that is under version control.
'  2) Modify the document, then save it.
'  3) With your document currently visible, run this script by one of
'     this two means:
'     a) Opening the PrjScr project, then click on "DXP > Run Script" and select
'        BetterRevert.
'     b) Creating a custom menu button with Process set to
'          ScriptingSystem:RunScript
'        and Parameters set to something like
'          ProjectName=[*PathToPrjScr*]|ProcName=[*PathToTheCurrentFile*]>[*NameOfTheFunction*]
'
' ------------------------------------------------------------------------------
' Setup requirements
' ------------------
'  * The project file (*.PrjScr) associated with your script must include
'    the following dependencies contained in the "Libraries" folder:
'    - Lib_FileManagement.vbs
'    - Lib_AltiumFunctions.vbs
'
' ------------------------------------------------------------------------------
' Compatibility concerns
' ----------------------
'  * The current version of this script is tested and works well with:
'     -> Altium Designer 15.1.14 build 47215
'     -> Altium Designer 14.3.18 build 45973
'  * The current version of this script is known to work under Windows 7 64-bit.



' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------



' Function to add a "close then re-open" feature to the Revert function
Sub BetterRevert
  Dim CurDocKind
  Dim CurDocPath
  
  ' Check if Client is available
  If Client Is Nothing Then Exit Sub
  ' Check if GetWorkspace is available
  If GetWorkspace Is Nothing Then Exit Sub
  ' Verify that the file path really exists
  If Not FileExistsOrShowError(GetWorkspace.DM_FocusedDocument.DM_FullPath) Then Exit Sub
  ' Verify that there are no unsaved modifications
  If DocObj_IsModified(Client, GetWorkspace.DM_FocusedDocument) Then
    If Not ConfirmNoYesWithCaption("Modifications found", "Your document contains un-saved modifications." & vbCrLf & "Do you agree to lose them and revert the file anyway?") Then
      Exit Sub
    End If
  End If

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
