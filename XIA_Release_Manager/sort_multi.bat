@ECHO OFF

rem ***************************************************************************
rem sort_multi.bat
rem    DOS batch file to sort an Altium-generated Multiwire netlist.
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
rem  %2 : The file to operate upon.
rem  %3 : The file to which sort_multi.bat should write its output.
rem  %4 : The file to which sort_multi.bat should write its return code.
rem ***************************************************************************

rem /* Retrieve all command line parameters */
SET SCRNAME=%0
SET TEMPLATE=%1
SET INFILE=%2
SET OUTFILE=%3
SET RCFILE=%4

rem /* Assign paths to all the Unix utilities we need to use. */
SET CAT=%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\cat.exe
SET AWK=%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\gawk.exe
SET SED=%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\sed.exe
SET SORT=%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\sort.exe
SET GREP=%TEMPLATE%\dosutils\UnxUtils\usr\local\wbin\grep.exe

rem /* This was my unix bash script that did the same thing. */
rem 
rem # Script to sort a "Multiwire" netlist produce an output that is
rem # sorted first by refdes and then by pin # within each refdes.
rem 
rem # First parameter is the name of the project (ie. MICROCOMU).
rem # Input is from $PROJECT.NET
rem # Output is to $PROJECT_1.NET
rem 
rem # Remove any existing sorted version of this netlist
rem rm -f $1_1.NET
rem 
rem # Alter the raw version of the "Multiwire" netlist.
rem cat $1.NET | 
rem 
rem 	# Reorder the file to be "refdes"."pin":"netname"
rem 	awk '{print $2"."$3":"$1}' | 
rem 
rem 	# Add leading 0's to refdes and pin number so that we may sort them with "sort".
rem 	# Creates 3 numeric digits for refdes.  Thus, assumes refdes numbers <=999.
rem 	# Creates 3 numeric digits for pin #.  Thus, assumes pins per part <=999.
rem 	sed '
rem 		s/^\([A-Z][A-Z]*\)\([0-9]\)\./\100\2\./; 
rem 		s/^\([A-Z][A-Z]*\)\([0-9][0-9]\)\./\10\2\./; 
rem 		s/\.\([A-Z]*\)\([0-9]\):/\.\100\2:/; 
rem 		s/\.\([A-Z]*\)\([0-9][0-9]\):/\.\10\2:/; 
rem 	' | 
rem 
rem 	# Sort the file first by refdes, then by pin within refdes.
rem 	sort |
rem 
rem 	# Now that refdes'es are sorted and pin numbers within each refdes are sorted, remove
rem 	# leading 0's so as not to confuse us.
rem 	sed '
rem 		s/^\([A-Z][A-Z]*\)0*\([1-9][0-9]*\)\./\1\2\./;
rem 		s/\.\([A-Z]*\)0*\([1-9][0-9]*\):/\.\1\2\:/;
rem 
rem 	# Remove EOF marker from original netlist file.
rem 	' | grep -v ":-1" > $1_1.NET
rem 	
rem echo "sort_multi completed successfully"
rem echo " Input file was $1.NET".
rem echo " Output file is $1_1.NET.  Output is sorted Multiwire netlist."

rem /* Store the 3 sed commands (the above 2 plus 1 new one) in variables to make the command line easier to read. */
SET SEDCMD0="s/:/\./; "
SET SEDCMD1="s/^\([A-Z][A-Z]*\)\([0-9]\)\./\100\2\./; s/^\([A-Z][A-Z]*\)\([0-9][0-9]\)\./\10\2\./; s/\.\([A-Z]*\)\([0-9]\):/\.\100\2:/; s/\.\([A-Z]*\)\([0-9][0-9]\):/\.\10\2:/; "
SET SEDCMD2="s/^\([A-Z][A-Z]*\)0*\([1-9][0-9]*\)\./\1\2\./;	s/\.\([A-Z]*\)0*\([1-9][0-9]*\):/\.\1\2\:/; "

rem @ECHO ON

rem /* Delete old version of output file. */
rem /* Rely on Altium script to have performed this step. */
rem DEL /Q "%OUTFILE%"

rem /* Run all the above commands together in one line to keep things simple for DOS. */
rem /* Note that the awk command now generates : as the output separator and requires an additional sed to sort things out. */
%CAT% %INFILE% | %AWK% -v OFS=: "{print $2,$3,$1}" | %SED% %SEDCMD0% | %SED% %SEDCMD1% | %SORT% | %SED% %SEDCMD2% | %GREP% -v ":-1" > %OUTFILE%

rem /* Store return code given to us by the last command (grep). */
SET GREPRC=%ERRORLEVEL%
rem ECHO.
rem ECHO Return code from grep was %GREPRC%.

rem /* Write sed return code to desired output file. */
ECHO %GREPRC% > %RCFILE%

rem rem /* Wrap up and get ready to exit. */
rem ECHO.
rem ECHO Delaying 10 seconds to allow you to read this output (if you like), 
rem ECHO  before exiting....
rem TIMEOUT 10

rem /* Exit with return code from sed. */
EXIT /b %GREPRC%
