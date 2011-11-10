@ECHO OFF

rem ***************************************************************************
rem svn_cmd.bat
rem    DOS batch file to run a generic svn command, called from an Altium Delphi script.
rem ***************************************************************************
rem 
rem ***************************************************************************
rem * Copyright (c) 2011 XIA LLC.
rem *  Author:        Jeff Collins, jcollins@xia.com
rem *  Author:        $Author: jcollins $
rem *  Check-in Date: $Date: 2011-09-12 19:16:21 -0700 (Mon, 12 Sep 2011) $ 
rem *  Version #:     $Revision: 20198 $
rem *  
rem * Redistribution and use in source and binary forms, 
rem * with or without modification, are permitted provided 
rem * that the following conditions are met:
rem *
rem *   * Redistributions of source code must retain the above 
rem *     copyright notice, this list of conditions and the 
rem *     following disclaimer.
rem *   * Redistributions in binary form must reproduce the 
rem *     above copyright notice, this list of conditions and the 
rem *     following disclaimer in the documentation and/or other 
rem *     materials provided with the distribution.
rem *   * Neither the name of XIA LLC nor the names of its
rem *     contributors may be used to endorse or promote
rem *     products derived from this software without 
rem *     specific prior written permission.
rem *
rem * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
rem * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
rem * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
rem * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
rem * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE 
rem * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
rem * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
rem * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
rem * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
rem * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
rem * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
rem * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
rem * SUCH DAMAGE.
rem ***************************************************************************

rem ***************************************************************************
rem NOTES:
rem  %1 : Drive on which Altium working directory is located.
rem  %2 : Path to Altium working directory.
rem  %3 : The file to which svn_cmd.bat should write its return code.
rem  %4 : The file to which svn_cmd.bat should write its output.
rem  %5 : The svn command we wish to run.
rem  %6 .. : Parameters to supply to svn.
rem ***************************************************************************

rem /* Retrieve all command line parameters */
SET SCRNAME=%0
SET DRIVE=%1
SET WD=%2
SET RCFILE=%3
SET OUTFILE=%4
SET SVNCMD=%5

rem /* Shift 6 times so that what was in %6 is now in %0, etc. */
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT

rem /* Loop over all remaining command line parameters.  Iteratively add them to SVNPARMS. */
rem /* Some of this code was copied from http://www.robvanderwoude.com/parameters.php */
SET SVNPARMS=
:Loop
IF [%0]==[] GOTO Continue

SET SVNPARMS=%SVNPARMS% %0
SHIFT

GOTO Loop
:Continue

rem /* We've extracted all the command line parameters.  Proceed. */
rem ECHO In script %SCRNAME%
rem ECHO  running generic svn command.
rem ECHO.
 
rem /* Change to appropriate drive letter */
%DRIVE%

rem /* Change to appropriate working directory */
cd %WD%

rem /* Issue svn command. */
rem /*  Redirect stdout and stderr from svn.exe to %OUTFILE% */
rem @ECHO ON
rem ECHO svn --non-interactive %SVNCMD% %SVNPARMS% > %OUTFILE%
svn --non-interactive %SVNCMD% %SVNPARMS% > %OUTFILE% 2>&1
@ECHO OFF

rem /* Store return code given to us by svn command. */
SET SVNRC=%ERRORLEVEL%
rem ECHO.
rem ECHO Return code from svn was %SVNRC%.

rem /* Write svn return code to desired output file. */
ECHO %SVNRC% > %RCFILE%

rem rem /* Wrap up and get ready to exit. */
rem rem ECHO.
rem rem ECHO Delaying 10 seconds to allow you to read this output (if you like), 
rem rem ECHO  before exiting....
rem rem TIMEOUT 10

rem /* Exit with return code from svn. */
EXIT /b %SVNRC%


