@ECHO OFF

rem ***************************************************************************
rem sed_cmd.bat
rem    DOS batch file to run a generic sed command, called from an Altium Delphi script.
rem ***************************************************************************
rem 
rem ***************************************************************************
rem * Copyright (c) 2011 XIA LLC.
rem *  Author:        Jeff Collins, jcollins@xia.com
rem *  Author:        $Author: jcollins $
rem *  Check-in Date: $Date: 2011-09-13 17:56:54 -0700 (Tue, 13 Sep 2011) $ 
rem *  Version #:     $Revision: 20209 $
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
rem  %1 : Path to Altium_scripts directory.
rem  %2 : The file to pipe into sed.
rem  %3 : The file to which sed_cmd.bat should write its return code.
rem  %4 : The file to which sed_cmd.bat should write its output.
rem  %5 : Sed command(s).
rem ***************************************************************************

rem /* Retrieve all command line parameters */
SET SCRNAME=%0
SET TEMPLATE=%1
SET INFILE=%2
SET RCFILE=%3
SET OUTFILE=%4
SET COMMAND=%5

rem ECHO In script %SCRNAME%
rem ECHO  running generic sed command.
rem ECHO.
 
rem /* Issue sed command. */
rem /*  Redirect stdout and stderr from sed.exe to %OUTFILE% */
rem @ECHO ON
%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\sed.exe %COMMAND% %INFILE% > %OUTFILE% 2>&1
@ECHO OFF

rem /* Store return code given to us by sed command. */
SET SEDRC=%ERRORLEVEL%
rem ECHO.
rem ECHO Return code from sed was %SEDRC%.

rem /* Write sed return code to desired output file. */
ECHO %SEDRC% > %RCFILE%

rem rem /* Wrap up and get ready to exit. */
rem ECHO.
rem ECHO Delaying 10 seconds to allow you to read this output (if you like), 
rem ECHO  before exiting....
rem TIMEOUT 10

rem /* Exit with return code from sed. */
EXIT /b %SEDRC%


