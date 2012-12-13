@ECHO OFF

rem ***************************************************************************
rem generic_cmd.bat
rem    DOS batch file to run a generic unix utils command (eg, find, grep, etc.), 
rem called from an Altium Delphi script.
rem ***************************************************************************

rem {***************************************************************************
rem  * Sierra Photonics Inc. has derived this file from svn_cmd.bat.
rem  *  Original / modified / updated code is subject to:
rem  *
rem  * The Sierra Photonics, Inc. Software License, Version 1.0:
rem  *  
rem  * Copyright (c) 2012 by Sierra Photonics Inc.  All rights reserved.
rem  *  Author:        Jeff Collins, jcollins@sierraphotonics.com
rem  *  Author:        $Author$
rem  *  Check-in Date: $Date$ 
rem  *  Version #:     $Revision$
rem  *  
rem  * Redistribution and use in source and binary forms, with or without
rem  * modification, are permitted provided that the following conditions
rem  * are met and the person seeking to use or redistribute such software hereby
rem  * agrees to and abides by the terms and conditions below:
rem  *
rem  * 1. Redistributions of source code must retain the above copyright
rem  * notice, this list of conditions and the following disclaimer.
rem  *
rem  * 2. Redistributions in binary form must reproduce the above copyright
rem  * notice, this list of conditions and the following disclaimer in
rem  * the documentation and/or other materials provided with the
rem  * distribution.
rem  *
rem  * 3. The end-user documentation included with the redistribution,
rem  * if any, must include the following acknowledgment:
rem  * "This product includes software developed by Sierra Photonics Inc." 
rem  * Alternately, this acknowledgment may appear in the software itself,
rem  * if and wherever such third-party acknowledgments normally appear.
rem  *
rem  * 4. The Sierra Photonics Inc. names or marks must
rem  * not be used to endorse or promote products derived from this
rem  * software without prior written permission. For written
rem  * permission, please contact:
rem  *  
rem  *  Sierra Photonics Inc.
rem  *  attn:  Legal Department
rem  *  7563 Southfront Rd.
rem  *  Livermore, CA  94551  USA
rem  * 
rem  * IN ALL CASES AND TO THE FULLEST EXTENT PERMITTED UNDER APPLICABLE LAW,
rem  * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
rem  * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
rem  * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
rem  * DISCLAIMED.  IN NO EVENT SHALL SIERRA PHOTONICS INC. OR 
rem  * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
rem  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
rem  * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
rem  * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
rem  * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
rem  * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
rem  * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
rem  * SUCH DAMAGE.
rem  *
rem  * This software consists of voluntary contributions made by many
rem  * individuals on behalf of the Altium Community Software.
rem  *
rem  * See also included file SPI_License.txt.
rem  ***************************************************************************}

rem {***************************************************************************
rem  * Derived from svn_cmd.bat downloaded from:
rem  *  http://code.google.com/p/altium-designer-addons/wiki/Release_Manager
rem * Copyright (c) 2011 XIA LLC.
rem *  Author:        Jeff Collins, jcollins@xia.com
rem *  Author:        Author: jcollins 
rem *  Check-in Date: $Date$ 
rem *  Version #:     Revision: 20198 
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
rem  %2 : Drive on which Altium working directory is located.
rem  %3 : Path to Altium working directory.
rem  %4 : The file to which generic_cmd.bat should write its return code.
rem  %5 : The file to which generic_cmd.bat should write its output.
rem  %6 : The command we wish to run (eg. find).
rem  %7 .. : Parameters to supply to said command.
rem ***************************************************************************

rem /* Retrieve all command line parameters */
SET SCRNAME=%0
SET TEMPLATE=%1
SET DRIVE=%2
SET WD=%3
SET RCFILE=%4
SET OUTFILE=%5
SET CMD=%6

rem /* Shift 7 times so that what was in %7 is now in %0, etc. */
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT
SHIFT

rem /* Loop over all remaining command line parameters.  Iteratively add them to PARMS. */
rem /* Some of this code was copied from http://www.robvanderwoude.com/parameters.php */
SET PARMS=
:Loop
IF [%0]==[] GOTO Continue

SET PARMS=%PARMS% %0
SHIFT

GOTO Loop
:Continue

rem /* We've extracted all the command line parameters.  Proceed. */
rem ECHO In script %SCRNAME%
rem ECHO  running generic  command.
rem ECHO.
 
rem /* Change to appropriate drive letter */
%DRIVE%

rem /* Change to appropriate working directory */
cd %WD%

rem /* Issue specified command. */
rem /*  Redirect stdout and stderr from %CMD%.exe to %OUTFILE% */
rem @ECHO ON
rem ECHO %TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\%CMD%.exe %PARMS% > %OUTFILE% 2>&1
%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\%CMD%.exe %PARMS% > %OUTFILE% 2>&1
@ECHO OFF

rem /* Store return code given to us by specified command. */
SET RC=%ERRORLEVEL%
rem ECHO.
rem ECHO Return code from  was %RC%.

rem /* Write specified command's return code to desired output file. */
ECHO %RC% > %RCFILE%

rem rem /* Wrap up and get ready to exit. */
rem rem ECHO.
rem rem ECHO Delaying 10 seconds to allow you to read this output (if you like), 
rem rem ECHO  before exiting....
rem rem TIMEOUT 10

rem /* Exit with return code from . */
EXIT /b %RC%


