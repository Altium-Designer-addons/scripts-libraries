rem {***************************************************************************
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

@rem Warning:  Because the gbak call contacts a windows service that doesn't know our current working directory, we MUST specify full paths to all files!
@rem Warning:  The Firebird service does not recognize paths on my H: drive (which is really a part of my C: drive, having run the SUBST DOS command)!

@rem "c:\Program Files (x86)\Firebird\Firebird_2_5\bin\gbak.exe" -backup -v "h:\projects\altium_libraries\trunk\database\altium_vault_files\Data\EmbFirebirdDb.fdb" "h:\projects\altium_libraries\trunk\database\altium_vault_files\Data\EmbFirebirdDb.fbk" -y "h:\projects\altium_libraries\trunk\database\altium_vault_files\Data\backup.log" -user SYSDBA -password masterkey

"c:\Program Files (x86)\Firebird\Firebird_2_5\bin\gbak.exe" -backup -v "c:\cygwin\home\jcollins\projects\altium_libraries\trunk\database\altium_vault_files\Data\EmbFirebirdDb.fdb" "c:\cygwin\home\jcollins\projects\altium_libraries\trunk\database\altium_vault_files\Data\EmbFirebirdDb.fbk" -y "c:\cygwin\home\jcollins\projects\altium_libraries\trunk\database\altium_vault_files\Data\backup.log" -user SYSDBA -password masterkey



