
Copyright (C) 2015 Justin MASSIOT
Author: Justin MASSIOT ( m [dot] just1 !at! free [dot] fr )

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

TortoiseSVN is an Open Source and free software licensed under the GNU General Public License (GPL). It is free to download and free to use, either personally or commercially, on any number of PCs. http://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-preface-source.html
Altium®, Altium Designer®, DXP™ and their respective logos are trademarks or registered trademarks of Altium Limited or its subsidiaries. See the full Copyright at http://www.altium.com/copyrights-and-trademarks and the EULA at http://www.altium.com/eula .
All other registered or unregistered trademarks referenced herein are the property of their respective owners.

------------------------------------------------------------------------------
What does this script do
-------------------
 * TSVN_CommitFolder is a function to commit a project, with extended features compared to the built-in function of Altium:
   -> it works on all the content of the project folder, not only the files contained in the Altium project
   -> it warns the user before committing if unsaved modifications are detected (everyone hates to forget to save just before a commit)
   -> it automatically refreshes the SVN icons at the end of the operation
 * TSVN_UpdateFolder is a function to update a project, with extended features compared to the built-in function of Altium:
   -> it works on all the content of the project folder, not only the files contained in the Altium project
   -> it does not update if unsaved modifications are detected on any file
   -> it closes and re-open all the project files that were previously open to be sure to work on the all fresh files

------------------------------------------------------------------------------
How to use this script
----------------------
 1) From Altium Designer, open a file or a project that is/are under version
    control with SVN.
 2) If your files contain unsaved modifications, please save them all.
 3) With your document currently visible, run this script by one of
    this three means:
    a) Using a custom menu dedicated to SVN actions described at
         https://github.com/Altium-Designer-addons/scripts-libraries/wiki/Menu_SVN
    b) Creating a custom menu button with Process set to
         ScriptingSystem:RunScript
       and Parameters set to something like
         ProjectName=[*PathToPrjScr*]|ProcName=[*PathToTheCurrentFile*]>[*NameOfTheFunction*]
    c) Opening the PrjScr project, then click on "DXP > Run Script" and select
       TSVN_CommitFolder or TSVN_UpdateFolder.
    Be sure that the visible document at the time you run the script
    is a document that you wish to update/commit.
 Note: In Update mode, all the open documents will be closed and re-opened
   after the update.

------------------------------------------------------------------------------
Setup requirements
------------------
 * TortoiseSVN must be properly installed on your system. In particular,
   the Windows path must contain the TortoiseSVN binaries directory.
 * The project file (*.PrjScr) associated with your script must include
   the following dependencies contained in the "Libraries" folder:
   - Lib_FileManagement.vbs
   - Lib_AltiumFunctions.vbs
   - Lib_VBScriptAdditions.vbs
 * It is strongly advised to install TortoiseSVN with its command-line
   binaries and configure Altium Designer to use them instead of the
   built-in Subversion client.

------------------------------------------------------------------------------
Compatibility concerns
----------------------
 * The current version of this script is tested and works well with:
    -> Altium Designer 17.1.5 build 472
    -> Altium Designer 15.0.15 build 41991
    -> Altium Designer 14.3.17 build 42447
 * The current version of this script is tested and works well with: 
    -> TortoiseSVN 1.9.5 build 27581
    -> TortoiseSVN 1.8.11 build 26392
 * The current version of this script is known to work under Windows 7 64-bit.

TortoiseSVN command line help: http://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-automation.html
