
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

' For a description of this script and some help on how to use it, please see the attached ReadMe file.



' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------



' TortoiseSVN binary path
Const TortoiseProc = """C:\\Program Files\\TortoiseSVN\\bin\\TortoiseProc.exe""" ' (since Altium Designer version 15, we need to precise the full path of the program)



' --------------------------------------------------------------------------------



' between two given paths, extract the path that is at the highest level in the directory tree
Function GetTopMostFolderPath (Path1, Path2)
  If TypeName(Path1) <> "String" Then Exit Function
  If TypeName(Path2) <> "String" Then Exit Function
  
  ' we determine which of the two paths is at the toppest level in the directory tree
  TreeLevelPath1 = Len(Path1) - Len(Replace(Path1, "\", "")) ' get the level by counting the number of backslashes
  TreeLevelPath2 = Len(Path2) - Len(Replace(Path2, "\", "")) ' get the level by counting the number of backslashes
  'ShowMessage Path1 & " - " & Path2
  If (TreeLevelPath2 < TreeLevelPath1 and Path2 <> "") or Path1 = "" Then
    GetTopMostFolderPath = Path2
  Else
    GetTopMostFolderPath = Path1
  End If
End Function



' --------------------------------------------------------------------------------



' TortoiseSVN: commit the folder containing the current file
Sub TSVN_CommitFolder
  Dim objShell
  Dim commitResult
  Dim commitFromPath
  
  ' Check if Client is available
  If Client Is Nothing Then Exit Sub
  ' Check if GetWorkspace is available
  If GetWorkspace Is Nothing Then Exit Sub
  
  ' Check if the command is launched from an opened file
  If Not TryCmd("GetWorkspace.DM_FocusedDocument.DM_FileName") Then
    ShowError "This script must be loaded from an open file."
    Exit Sub ' no filename = no focused document = no document open
  End If
    
  ' Check if the file exists (if this is a new file that has not been saved yet, the result is False)
  If Not FileExists(GetWorkspace.DM_FocusedDocument.DM_FullPath) Then
    ShowError "Please save the file before trying to commit it."
    Exit Sub
  End If
  
  ' ----------
  ' Before committing the documents, let's check that there is no unsaved document in the current project
  Dim DocCounter
  Dim Project
  Dim IsThereUnsavedDocs
  Dim isProjectFreeDocuments
  
  Project = GetWorkspace.DM_FocusedProject
  IsThereUnsavedDocs = False
  isProjectFreeDocuments = True

  ' If the user is currently working from inside a project (not from "Free Documents"), we also perform the check on the project file (which does not exist in the case of "Free Documents")
  ' start by checking the project file itself
  If IsProjectPathValid(Project, "properties") Then
    'ShowMessage Project.DM_ProjectFullPath
    isProjectFreeDocuments = False
    IsThereUnsavedDocs = DocObj_IsModified(Client, Project)
  End If
  ' and then check the files inside the project (or the "Free Documents" files)
  For DocCounter = 0 to (Project.DM_LogicalDocumentCount - 1) ' loop on all the documents of the project
    'ShowMessage Project.DM_LogicalDocuments(DocCounter).DM_FullPath
    If DocObj_IsModified( Client, Project.DM_LogicalDocuments(DocCounter) ) Then
      IsThereUnsavedDocs = True ' True if *at least* one file has the Modified attribute
    End If
  Next
  
  ' warning: some documents contain unsaved modifications
  If IsThereUnsavedDocs = True Then
    ShowWarning "There are some unsaved modifications in the document(s) of the current project. If you continue with committing, the unsaved modifications will not be committed." & vbCrLf & "To exit and save the modifications, click Cancel in the following commit dialog box."
  End If
  ' ----------
  
  ' let's commit the whole folder
  Set objShell = CreateObject( "WScript.Shell" )
  If TortoiseProc <> "" Then
    If isProjectFreeDocuments Then
      ' the user works from free documents -> the commit is done on the folder that contains the current document
      commitFromPath = GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedDocument.DM_FullPath)
    Else
      ' the user works in a project -> the commit is done either on the project parent folder or on the document parent folder (we always take the highest level in the tree hierarchy)
      commitFromPath = GetTopMostFolderPath( GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedProject.DM_ProjectFullPath) , GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedDocument.DM_FullPath) )
    End If
    'ShowMessage commitFromPath
    commitResult = objShell.Run( TortoiseProc & "/command:commit /path:""" & commitFromPath & """" , 10 , True ) ' blocking call
  Else
    ShowError "The path to the TortoiseSVN program is not properly defined. Impossible to commit!"
  End If
  Set objShell = Nothing
  
  ' then refresh the SVN status of the project files
  If commitResult >= 0 Then
    Call ServerRunProcessSend("VersionControl:VersionControl", "ObjectKind=FocusedProject | Action=RefreshProject")
  End If
End Sub



' TortoiseSVN: update the folder containing the current file
Sub TSVN_UpdateFolder
  Dim objShell
  Dim updateResult
  Dim updateFromPath
  Dim Project
  Dim isProjectFreeDocuments
  
  ' Check if Client is available
  If Client Is Nothing Then Exit Sub
  ' Check if GetWorkspace is available
  If GetWorkspace Is Nothing Then Exit Sub
  
  ' Check if the command is launched from an opened file
  If Not TryCmd("GetWorkspace.DM_FocusedDocument.DM_FileName") Then
    ShowError "This script must be loaded from an open file."
    Exit Sub ' no filename = no focused document = no document open
  End If
    
  ' Check if the file exists (if this is a new file that has not been saved yet, the result is False)
  If Not FileExists(GetWorkspace.DM_FocusedDocument.DM_FullPath) Then
    ShowError "Please save the file before trying to update it."
    Exit Sub
  End If
  
  ' Check if the user is working in a real project or from "Free Documents"
  Project = GetWorkspace.DM_FocusedProject
  If IsProjectPathValid(Project, "properties") Then
    isProjectFreeDocuments = False
  Else
    isProjectFreeDocuments = True
  End If
  
  ' If the project has some unsaved modifications, warn the user and stop the Update process to avoid conflicts
  If DocObj_IsModified(Client, Project) Then
    ShowError "Your currently active project contains some unsaved modifications (" & Project.DM_ProjectFileName & ")." & vbCrLf &_
              "Please save or discard those modifications before trying to update in order not to lose them."
    Exit Sub
  End If
  
  ' get the folder path which has to be updated
  If TortoiseProc <> "" Then
    If isProjectFreeDocuments Then
      ' Free Documents -> let's work on the current file only, since the project is virtual (not a file)
      updateFromPath = GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedDocument.DM_FullPath)
    Else
      updateFromPath = GetTopMostFolderPath( GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedProject.DM_ProjectFullPath) , GetParentFullPathFromFilePath(GetWorkspace.DM_FocusedDocument.DM_FullPath) )
    End If
    'ShowMessage updateFromPath
  Else
    ShowError "The path to the TortoiseSVN program is not properly defined. Impossible to commit!"
  End If
  
  ' ----------
  
  If isProjectFreeDocuments Then ' working from free documents
  
    Dim CurDoc
    Dim CurDocKind
    Dim CurDocPath
    
    If DocObj_IsModified(Client, GetWorkspace.DM_FocusedDocument) Then ' we do not close the document in order not to lose the unsaved modifications
      ShowError "Your currently active document contains some unsaved modifications (" & GetWorkspace.DM_FocusedDocument.DM_FileName & ")." & vbCrLf &_
                "Please save or discard those modifications before trying to update in order not to lose them."
      Exit Sub
    Else ' the document has no unsaved modification
      CurDocKind = GetWorkspace.DM_FocusedDocument.DM_DocumentKind
      CurDocPath = GetWorkspace.DM_FocusedDocument.DM_FullPath
      Client.CloseDocument( Client.OpenDocument(CurDocKind, CurDocPath) ) ' close the document before updating it
    End If
    
    ' ----------
    ' let's update the whole folder with its externals
    Set objShell = CreateObject( "WScript.Shell" )
    updateResult = objShell.Run( TortoiseProc & "/command:update /includeexternals /path:""" & updateFromPath & """" , 10 , True ) ' blocking call
    Set objShell = Nothing
    ' refresh the SVN status inside Altium Designer
    Call ServerRunProcessSend("VersionControl:VersionControl", "ObjectKind=FocusedDocument | Action=RefreshProject")
    ' ----------
    
    Client.ShowDocument( Client.OpenDocument(CurDocKind, CurDocPath) ) ' re-open the document after the update
    
  Else ' not inside Free Documents = working with a real project
    
    Dim arrayPleaseReOpen
    Dim arrayCounter
    Dim ProjectPath
    
    arrayCounter = 0
    arrayPleaseReOpen = Array(0, 0) ' creation of a two-dimensional array (the array is not empty, it contains 1 element!)
    ReDim arrayPleaseReOpen(Project.DM_LogicalDocumentCount-1, 1) ' expansion of the array to a size corresponding to the number of files in the project, multiplied by 2 because we have to store a DocumentKind and a DocumentPath
    
    ' close the open AND non-modified documents ...
    ' (all the modified files should not be updated!)
    For DocCounter = 0 to (Project.DM_LogicalDocumentCount - 1) ' loop on all the documents of the project
      If DocObj_IsModified( Client, Project.DM_LogicalDocuments(DocCounter) ) Then
        ShowError "At least one document in your project contains some unsaved modifications (" & Project.DM_LogicalDocuments(DocCounter).DM_FileName & ")." & vbCrLf &_
                  "Please save or discard those modifications before trying to update in order not to lose them."
        Exit Sub
      Else
        ' list each document that is currently visible in order to re-open them after the update
        If DocObj_IsVisible( Client, Project.DM_LogicalDocuments(DocCounter) ) Then
          arrayPleaseReOpen(arrayCounter,0) = Project.DM_LogicalDocuments(DocCounter).DM_DocumentKind
          arrayPleaseReOpen(arrayCounter,1) = Project.DM_LogicalDocuments(DocCounter).DM_FullPath
          'ShowMessage "[" & arrayPleaseReOpen(arrayCounter,0) & "] " & arrayPleaseReOpen(arrayCounter,1)
          arrayCounter = arrayCounter + 1 ' increment the array counter at each loop iteration
        ' Else: the document is not visible -> no need to re-open it
        End If
        If Client.IsDocumentOpen(Project.DM_LogicalDocuments(DocCounter).DM_FullPath) Then
          ' open and non-modified documents are closed in order to be refreshed (even the documents that are open but not visible)
          Client.CloseDocument( Client.OpenDocument(Project.DM_LogicalDocuments(DocCounter).DM_DocumentKind , Project.DM_LogicalDocuments(DocCounter).DM_FullPath) )
        End If
      End If
    Next
    
    ' close the project if not modified
    ' if the project is modified, the function has already stopped just after the line " If DocObj_IsModified(Client, Project) "
    ProjectPath = Project.DM_ProjectFullPath
    'Call ServerRunProcessSend("WorkspaceManager:CloseObject", "ObjectKind=FocusedProject")
    Client.CloseDocument( Client.OpenDocument("", ProjectPath) )
    
    ' ----------
    ' let's update the whole folder with its externals
    Set objShell = CreateObject( "WScript.Shell" )
    updateResult = objShell.Run( TortoiseProc & "/command:update /includeexternals /path:""" & updateFromPath & """" , 10 , True ) ' blocking call
    Set objShell = Nothing
    ' refresh the SVN status inside Altium Designer
    Call ServerRunProcessSend("VersionControl:VersionControl", "ObjectKind=FocusedProject | Action=RefreshProject")
    ' ----------
    
    ' re-open the project
    Call ServerRunProcessSend("WorkspaceManager:OpenObject", "ObjectKind=Project | FileName=" & ProjectPath)
    'Call Client.OpenDocument("", ProjectPath) ' does not work if the project is not already open...
    
    ' and re-open all the documents that were open before the update
    For DocCounter = 0 to (arrayCounter - 1)
      'ShowMessage arrayPleaseReOpen(DocCounter,1)
      If DoesFileExist( arrayPleaseReOpen(DocCounter,1) ) Then ' the file may not exist any more after the update!
        Client.ShowDocument( Client.OpenDocument( arrayPleaseReOpen(DocCounter,0) , arrayPleaseReOpen(DocCounter,1) ) )
      End If
    Next
    
  End If
  
End Sub

