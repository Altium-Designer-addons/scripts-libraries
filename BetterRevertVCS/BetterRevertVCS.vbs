
' Original author: Justin MASSIOT

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

' For a description of this script and some help on how to use it, please see the attached ReadMe file.



' Function to add a "close then re-open" feature to the Revert function
Sub BetterRevertVCS
  Dim CurDocKind
  Dim CurDocPath
  
  ' Check if Client is available
  If Client Is Nothing Then Exit Sub
  ' Check if GetWorkspace is available
  If GetWorkspace Is Nothing Then Exit Sub
  ' Verify that the file path really exists
  If Not FileExists(GetWorkspace.DM_FocusedDocument.DM_FullPath) Then
    ShowError "You cannot revert a file that is not saved on your hard drive."
    Exit Sub
  End If
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
