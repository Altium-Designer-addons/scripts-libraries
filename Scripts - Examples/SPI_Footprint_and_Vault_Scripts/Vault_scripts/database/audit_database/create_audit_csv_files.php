<?php 
/*!================================================================================================

	@file			create_audit_csv_files.php

	@brief			PHP script to create audit trail files to track changes to Altium Vault database.

	@details		Various reports will be created in .csv file format.
					The Altium Vault librarian is responsible for tracking changes to Altium Vault
					components, aided by the various audit .csv files that this script generates.

	@version		1.14.5
					   $Rev::                                                                        $:
	@date			  $Date::                                                                        $:
	@author			$Author::                                                                        $:
					    $Id::                                                                        $:

	@copyright      Copyright (c) 2012 by Sierra Photonics, Inc.  All rights reserved.
	
   The Sierra Photonics, Inc. Software License, Version 1.0:
    
   Copyright (c) 2012 by Sierra Photonics Inc.  All rights reserved.
    Author:        Jeff Collins, jcollins@sierraphotonics.com
    Author:        $Author$
    Check-in Date: $Date$ 
    Version #:     $Revision$
    
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met and the person seeking to use or redistribute such software hereby
   agrees to and abides by the terms and conditions below:
  
   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
  
   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.
  
   3. The end-user documentation included with the redistribution,
   if any, must include the following acknowledgment:
   "This product includes software developed by Sierra Photonics Inc." 
   Alternately, this acknowledgment may appear in the software itself,
   if and wherever such third-party acknowledgments normally appear.
  
   4. The Sierra Photonics Inc. names or marks must
   not be used to endorse or promote products derived from this
   software without prior written permission. For written
   permission, please contact:
    
    Sierra Photonics Inc.
    attn:  Legal Department
    7563 Southfront Rd.
    Livermore, CA  94551  USA
   
   IN ALL CASES AND TO THE FULLEST EXTENT PERMITTED UNDER APPLICABLE LAW,
   THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED.  IN NO EVENT SHALL SIERRA PHOTONICS INC. OR 
   ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
   OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
   SUCH DAMAGE.
  
   This software consists of voluntary contributions made by many
   individuals on behalf of the Altium Community Software.
  
   See also included file SPI_License.txt.
					
=================================================================================================*/
?>

<?php

/****************************************************************
 *
 * Altium Vault database structure (interesting parts only)
 *
 * ALU_ACLENTRY
 *		Holds access control lists for all folders, items, and itemRevs 
 *		(when Vault is in Enterprise sharing mode).
 *
 *		Foreign keys (field in this table -> foreign table.field):
 *		OBJECTID -> ALU_ITEMREVISION.GUID *or*
 *		OBJECTID -> ALU_ITEM.GUID *or*
 *		OBJECTID -> ALU_FOLDER.GUID
 *
 * ALU_LIFECYCLESTATECHANGE (top-most level)
 *		Holds change log describing all changes to models and components.
 *		GUID = Globally Unique ID (primary key).
 *		HRID = Description of action performed.
 *
 *		Foreign keys (field in this table -> foreign table.field):
 *		ITEMREVISIONGUID -> ALU_ITEMREVISION.GUID
 *		LIFECYCLESTATETRANSITIONGUID -> ALU_LIFECYCLESTATETRANSITION.GUID
 *		LIFECYCLESTATEAFTERGUID	-> ALU_LIFECYCLESTATE.GUID
 *
 * ALU_ITEMREVISION
 *		Holds models (symbol / footprint) and components at a specific revision.
 *		GUID = Globally Unique ID for item at a specific revision. (primary key)
 *		HRID = Human Readable ID for item at a specific revision.
 *		REVISIONID = Just the revision suffix from HRID.
 *		DESCRIPTION = Altium schematic Description field.
 *		COMMENT = Altium schematic Comment field.
 *	
 *		Foreign keys (field in this table -> foreign table.field):
 *		GUID -> ALU_ITEMREVISIONLINK.PARENTITEMREVISIONGUID (components only!)
 *		LIFECYCLESTATEGUID -> ALU_LIFECYCLESTATE.GUID
 *		ITEMGUID -> ALU_ITEM.GUID
 *  
 * ALU_ITEM
 *		Holds models (symbol / footprint) and components, no revision #.
 *		GUID = Globally Unique ID for item, no revision #. (primary key)
 *		HRID = Human Readable ID for a item, no revision #.
 *	
 *		Foreign keys (field in this table -> foreign table.field):
 *		FOLDERGUID -> ALU_FOLDER.GUID
 *		LIFECYCLEDEFINITIONGUID -> ALU_LIFECYCLEDEFINITION.GUID
 *		REVISIONNAMINGSCHEMEGUID -> ALU_REVISIONNAMINGSCHEME.GUID
 *		CONTENTTYPEGUID -> ALU_CONTENTTYPE.GUID
 *
 * ALU_ITEMREVISIONLINK
 *		Holds linking and type information re/ models (symbol / footprint).
 *		GUID = Globally Unique ID (primary key).
 *		HRID = Type of model (eg. "SCHLIB").
 *
 *		Foreign keys (field in this table -> foreign table.field):
 *		CHILDITEMREVISIONGUID -> DATAFILE.OWNERGUID
 *		CHILDITEMREVISIONGUID -> ALU_ITEMREVISION.GUID (of the model used in original ITEMREVISION object)
 *
 * ALU_DATAFILE
 *		Holds filename information re/ models (symbol / footprint).
 *		GUID = Globally Unique ID (primary key).
 *		HRID = Name of model (eg. "Resistor_single.SchLib").
 *
 *		Foreign keys (field in this table -> foreign table.field):
 *		OWNERGUID -> ALU_ITEMREVISION.GUID
 *		DATAFOLDERGUID -> ALU_DATAFOLDER.GUID
 *
 * ALU_DATAFOLDER
 *		Holds release information (?) re/ models (symbol / footprint).
 *		HRID = Status of model (eg. "Released").
 *
 * ALU_FOLDER
 *		Holds name of a particular folder (eg. subdir) within the Altium Vault database filesystem.
 *		GUID = Globally Unique ID (primary key).
 *		HRID = Name of particular folder.
 *	
 *		Foreign keys (field in this table -> foreign table.field):
 *		FOLDERTYPEGUID -> ALU_FOLDERTYPE.GUID
 *		PARENTFOLDERGUID -> ALU_FOLDER.GUID
 *		
 * ALU_FOLDERTYPE
 *		Holds list of folder types.
 *		GUID = Globally Unique ID (primary key).
 *		HRID = Human readable folder type.
 *	
 ****************************************************************/


/****************************************************************
 * my_die()
 *		Function to print a final message before dying.
 ****************************************************************/
function my_die($msg) 
{
  echo "\n\nError!  Aborting script!  Message was: \n$msg\n\n";

  die(-1);
}


/****************************************************************
 * my_fopen()
 *		Function to open a specified file for writing.
 * Die a horrible death if unable to do so.
 ****************************************************************/
function my_fopen($fileName) 
{
  /** Open specified fileName for writing. **/
  $fileHandle = fopen($fileName, "w") 
    or my_die("Unable to open output file \"$fileName.\"");

  /* Return file handle to caller. */
  return($fileHandle);

} /* end my_fopen()


/****************************************************************
 * CACF_RemapReservedChars()
 *		Function to open a specified file for writing.
 * Die a horrible death if unable to do so.
 ****************************************************************/
function CACF_RemapReservedChars($remapMe) 
{
  /* Remap any ',' chars in cell to '|' chars! */
  return(preg_replace('/,/', '|', $remapMe));

} /* end CACF_RemapReservedChars()


/****************************************************************
 * CACF_InitUsernames()
 *		Initialize known usernames at SPI.
 *
 * Outputs:  $altiumUserNamesByGuid
 ****************************************************************/
function CACF_InitUsernames(&$altiumUserNamesByGuid) 
{
  /* Init to an empty array. */
  $altiumUserNamesByGuid = array();

  /* Store the number of unknown usernames. */
  $altiumUserNamesByGuid["numUnknowns"] = 0;

  /* Create entries for all known Altium users at SPI.
   As far as I can tell, this data is not stored locally.
   Thus, it must be stored on Altium's authentication server.
   I am basically reverse engineering each user's GUID as he/she
   performs some action on the Vault database. */
  $altiumUserNamesByGuid["DF8C1AC3-FCED-4CEF-A217-753A257A452B"] = "jcollins";
  $altiumUserNamesByGuid["EFCE8226-63F8-4159-8DD7-CF38C6159BCB"] = "dwheeler";
  $altiumUserNamesByGuid["0F1C40E1-F033-40A3-A5CA-743089A57EBD"] = "kulander";
  $altiumUserNamesByGuid["DFDBE10B-A72F-46B8-86E8-4138BBDA1710"] = "akuo";


} /* end CACF_InitUsernames() */


/****************************************************************
 * CACF_InitAclUserPermissions()
 *		Initialize ACL user permissions at SPI.
 *
 * Outputs:  $altiumUserNamesByGuid
 ****************************************************************/
function CACF_InitAclUserPermissions(&$altiumAclUserPermissions)
{
  /* Create array to hold user permission data (reverse engineered). */
  /* FIXME:  Use user name GUIDs from above function rather than redeclaring them here! */
  $altiumAclUserPermissions = array();
  $altiumAclUserPermissions[2]["C8EF4489-F3CC-46BC-B212-F538A59C1C36"][1][0] = "All SPI users:  Allow read access";
  $altiumAclUserPermissions[2]["C8EF4489-F3CC-46BC-B212-F538A59C1C36"][15][0] = "All SPI users:  Allow read/write access";
  $altiumAclUserPermissions[0]["EFCE8226-63F8-4159-8DD7-CF38C6159BCB"][15][0] = "Don Wheeler:  Allow read/write access";
  $altiumAclUserPermissions[0]["0F1C40E1-F033-40A3-A5CA-743089A57EBD"][15][0] = "Klaus Ulander:  Allow read/write access";
  $altiumAclUserPermissions[0]["DFDBE10B-A72F-46B8-86E8-4138BBDA1710"][15][0] = "Alice Kuo:  Allow read/write access";

} /* end CACF_InitAclUserPermissions() */


/****************************************************************
 * CACF_LookupUsername()
 *		Lookup a username from a given GUID.
 *
 * Outputs:  String return value from function (username corresponding to given GUID).
 ****************************************************************/
function CACF_LookupUsername(&$altiumUserNamesByGuid, $guid) 
{
  /* See if there is an entry for this user stored in the array. */
  if (isset($altiumUserNamesByGuid[$guid]))
    {
      /* So just extract the name and prepare to return it to caller. */
      $username = $altiumUserNamesByGuid[$guid];
    }

  /** Else we don't know who this user is. **/
  else
    {
      /* Retrieve number of unknown users. */
      $numUnknowns = $altiumUserNamesByGuid["numUnknowns"];

      /* Increment number of unknown users. */
      $numUnknowns++;

      /* Assign this user as "UNKNOWN%d". */
      $username = "UNKNOWN" . $numUnknowns;

      /* Store this for future reference. */
      $altiumUserNamesByGuid[$guid] = $username;

    } /* endelse */
  
  /* Return username to caller. */
  return($username);

} /* end CACF_LookupUsername() */


/****************************************************************
 * CACF_TraceFolderPath()
 *		Function to trace the full path for a given leaf folder.
 *
 * Outputs:  String return value from function (full path to given leaf folder).
 ****************************************************************/
function CACF_TraceFolderPath(&$CACFconstants, &$altiumFoldersByGuid, $leafFolderGuid)
{
  /* Retrieve necessary global constants. */
  $pathSep = 										$CACFconstants["pathSep"];

  //  echo "In CACF_TraceFolderPath(), leafFolderGuid is $leafFolderGuid.\n";

  /* Remember the name of this leaf folder. */
  $leafFolder = $altiumFoldersByGuid[$leafFolderGuid]["HRID"];

  /* See if we've already done this lookup and cached the results. */
  if (isset($altiumFoldersByGuid[$leafFolderGuid]["FullPath"]))
    {
      /* Retrieve cached result. */
      $path = $altiumFoldersByGuid[$leafFolderGuid]["FullPath"];
      //      echo "Found cached lookup for \"$leafFolderGuid\" as \"$path\".\n";
    }
  
  /* Else we actually have to go to the effort of walking the linked list. */
  else
    {
      /* Initialize path to null. */
      $path = "";

      /* Initialize GUID we're looking for to be the one given to us. */
      $currGuid = $leafFolderGuid;

      $i = 0;
      do
        {
          //      echo "i is $i.\n";
          //      echo "currGuid is \"$currGuid\".\n";

          /* Get the name of the current folder.  Pre-pend it to running path. */
          $path = $altiumFoldersByGuid[$currGuid]["HRID"] . $pathSep . $path;
          //      echo "Path is now \"$path\".\n";

          /* Trace one more level deeper. */
          $parentFolderGuid = $altiumFoldersByGuid[$currGuid]["PARENTFOLDERGUID"];
          //      echo "parentFolderGuid is \"$parentFolderGuid\".\n";
          $currGuid = $parentFolderGuid;

          $i++;
      
          /* See if we've hit our breakout condition. */
        } while ($currGuid != "");

      //      echo "Traced full path for leaf folder \"$leafFolder\" as \"$path\".\n";

      /* Cache this information for next time we're asked for the full path
       for this same leaf folder. */
      $altiumFoldersByGuid[$leafFolderGuid]["FullPath"] = $path;

    } /* endelse */
  
  /* Return full path to caller function. */
  return ($path);

} /* end CACF_TraceFolderPath() */


/****************************************************************
 * CACF_AlterAltiumUserParmName()
 *		Function to alter Altium user parameter names to facilitate auditing.
 *
 * Outputs:  String return value from function (altered user parameter name).
 ****************************************************************/
function CACF_AlterAltiumUserParmName($PARAMETERNAME)
{
  
  /* Return altered user parameter name to caller. */
  /* Do a regex substiution on one particular parameter name to transform 'Supplier Part Number 1' to 'Supplier 1 Part Number', etc. */
  return(preg_replace('/Supplier Part Number ([0-9]+)/', 'Supplier $1 Part Number', $PARAMETERNAME));

} /* end CACF_AlterAltiumUserParmName() */


/****************************************************************
 * CACF_UnAlterAltiumUserParmName()
 *		Function to un-alter Altium user parameter names.  This restoration
 * to Altium standard user parameter names is needed when re-writing .CmpLib file.
 *
 * Outputs:  String return value from function (altered user parameter name).
 ****************************************************************/
function CACF_UnAlterAltiumUserParmName($PARAMETERNAME)
{
  
  /* Return un-altered user parameter name to caller. */
  /* Do a regex substiution on one particular parameter name to transform 'Supplier 1 Part Number' back to 'Supplier Part Number 1', etc. */
  return(preg_replace('/Supplier ([0-9]+) Part Number/', 'Supplier Part Number $1', $PARAMETERNAME));

} /* end CACF_UnAlterAltiumUserParmName() */


/****************************************************************
 * CACF_AlterAltiumSysParmName()
 *		Function to alter Altium sys parameter names to facilitate auditing.
 *
 * Outputs:  String return value from function (altered sys parameter name).
 ****************************************************************/
function CACF_AlterAltiumSysParmName($PARAMETERNAME)
{
  /* Examine given parameter name. */
  switch($PARAMETERNAME)
    {

    case "ItemHRID":
    case "ITEMHRID":
      {
        $PARAMETERNAME = "00-ITEMHRID";
        break;
      }

    case "REVISIONID":
      {
        $PARAMETERNAME = "01-REVISIONID";
        break;
      }
      
    case "ANCESTORITEMREVISIONHRID":
      {
        $PARAMETERNAME = "02-ANCESTORITEMREVISIONHRID";
        break;
      }

    case "Comment":
    case "COMMENT":
      {
        $PARAMETERNAME = "03-COMMENT";
        break;
      }

    case "Description":
    case "DESCRIPTION":
      {
        $PARAMETERNAME = "04-DESCRIPTION";
        break;
      }

    case "COMPONENTPATH":
      {
        $PARAMETERNAME = "05-COMPONENTPATH";
        break;
      }

    case "CREATEDBY":
      {
        $PARAMETERNAME = "06-CREATEDBY";
        break;
      }

    case "CREATEDAT":
      {
        $PARAMETERNAME = "07-CREATEDAT";
        break;
      }

    case "LASTMODIFIEDBY":
      {
        $PARAMETERNAME = "08-LASTMODIFIEDBY";
        break;
      }

    case "LASTMODIFIEDAT":
      {
        $PARAMETERNAME = "09-LASTMODIFIEDAT";
        break;
      }

    case "LIFECYCLESTATEHRID":
      {
        $PARAMETERNAME = "11-LIFECYCLESTATEHRID";
        break;
      }

    case "RELEASEDATE":
      {
        $PARAMETERNAME = "12-RELEASEDATE";
        break;
      }

    case "LIFECYCLEDEFINITIONHRID":
      {
        $PARAMETERNAME = "13-LIFECYCLEDEFINITIONHRID";
        break;
      }

    case "REVISIONNAMINGSCHEMEHRID":
      {
        $PARAMETERNAME = "14-REVISIONNAMINGSCHEMEHRID";
        break;
      }

    case "SHARINGCONTROL":
      {
        $PARAMETERNAME = "15-SHARINGCONTROL";
        break;
      }

    default:
      {
        /* If it starts with "PERMISSIONS, pre-pend a "19" and call it good. */
        if (substr_compare($PARAMETERNAME, "PERMISSIONS", 0) == 1)
          {
            $PARAMETERNAME = "19-" . $PARAMETERNAME;
          }
        
        else
          my_die('Unrecognized system parameter name "' . $PARAMETERNAME . '"!');

        break;
      }


    } /* endswitch */


  /* Return altered name to caller. */
  return($PARAMETERNAME);

} /* end CACF_AlterAltiumSysParmName() */


/****************************************************************
 * function CACF_AddPrefixedModelInfoToSysParms()
 *		Add information for a single model (that already has a prefix
 * assigned) to system parameters array.
 ****************************************************************/
function CACF_AddPrefixedModelInfoToSysParms(&$CACFconstants, 
                                             &$sysParms, 
                                             $modelNum, $modelTypeWithNum, $modelHRID, $modelPath, $modelLib)
{

  /* Retrieve necessary global constants. */
  $reserveForThisManyNonModels = 					$CACFconstants["reserveForThisManyNonModels"];

  /* Configure the per-entry prefix. */
  /* FIXME:  Magic number! */
  $modelPrefixNum = ($reserveForThisManyNonModels + (4*$modelNum));

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Store the ModelKind (aka. ModelType). */
  $sysParms[$modelPrefix."MODELTYPE"] = $modelTypeWithNum;

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Store the Model HRID. */
  $sysParms[$modelPrefix."MODELHRID"] = $modelHRID;

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Store the Model PATH. */
  $sysParms[$modelPrefix."MODELPATH"] = $modelPath;

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Store the Model LIB. */
  $sysParms[$modelPrefix."MODELLIB"] = $modelLib;

} /* end CACF_AddPrefixedModelInfoToSysParms() */


/****************************************************************
 * function CACF_ReserveForModelsInSysParms()
 *		Add information for a single model (that already has a prefix
 * assigned) to system parameters array.
 ****************************************************************/
function CACF_ReserveForModelsInSysParms(&$CACFconstants, 
                                         &$sysParms)
{

  /* Retrieve necessary global constants. */
  $reserveForThisManyModels = 						$CACFconstants["reserveForThisManyModels"];

  /* Setup to store null strings for all fields. */
  $modelTypeWithNum = "";
  $modelHRID = "";
  $modelPath = "";
  $modelLib = "";

  /* Loop over all the models we wish to reserve space for. */
  for ($modelNum = 0; $modelNum < $reserveForThisManyModels; $modelNum++)
    {

      /* Call CACF_AddPrefixedModelInfoToSysParms() to reserve for this model number. */
      CACF_AddPrefixedModelInfoToSysParms(&$CACFconstants, 
                                          &$sysParms, 
                                          $modelNum, $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);

    } /* endfor */

} /* end CACF_ReserveForModelsInSysParms() */


/****************************************************************
 * function CACF_AddModelInfoToSysParms()
 *		Add information for a single model to system parameters array.
 ****************************************************************/
function CACF_AddModelInfoToSysParms(&$CACFconstants, 
                                     &$sysParms, 
                                     $modelTypeWithNum, $modelHRID, $modelPath, $modelLib)
{

  /* Retrieve necessary global constants. */
  $maxPcbLibModels = 								$CACFconstants["maxPcbLibModels"];

  /** Construct a lookup table to convert modelTypeWithNum into an absolute model number. **/
  /* TODO:  If any other function ever needs this lookup table, rename it and move it to CACF_Init(). */
  $lookupTable = array();
  $modelNum = 0;

  /* Base entries for SCHLIB and PCBLIB (0). */
  $lookupTable["SCHLIB"] = $modelNum++;
  $lookupTable["PCBLIB"] = $modelNum++;

  /* Loop over all alternate (1, 2, 3, ...) allowed PCBLIB models. */
  for ($i = 1; $i < $maxPcbLibModels; $i++)
    {
      $lookupTable["PCBLIB $i"] = $modelNum++;
    
    } /* endfor */
      

  /** Lookup the given modelTypeWithNum in our lookup table. **/
  if (!isset($lookupTable[$modelTypeWithNum]))
    my_die("Unknown/unsupported model type $modelTypeWithNum!");

  else
    $modelNum = $lookupTable[$modelTypeWithNum];

  /* Call CACF_AddPrefixedModelInfoToSysParms() to add this model to sysParms array. */
  CACF_AddPrefixedModelInfoToSysParms(&$CACFconstants, 
                                      &$sysParms, 
                                      $modelNum, $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);

} /* end CACF_AddModelInfoToSysParms() */


/****************************************************************
 * function CACF_GetModelInfoFromSysParms()
 *		Get information for a single model from system parameters array.
 *
 * Outputs:  &$modelType, &$modelHRID, &$modelPath, &$modelLib
 ****************************************************************/
function CACF_GetModelInfoFromSysParms(&$CACFconstants, 
                                       &$sysParms, 
                                       $modelNum, 
                                       &$modelType, &$modelHRID, &$modelPath, &$modelLib)
{
  /* Retrieve necessary global constants. */
  $reserveForThisManyNonModels = 					$CACFconstants["reserveForThisManyNonModels"];

  /* Setup to return null strings by default. */
  $modelType = "";
  $modelHRID = "";
  $modelPath = "";
  $modelLib = "";

  /* Configure the per-entry prefix. */
  /* FIXME:  Magic number! */
  $modelPrefixNum = ($reserveForThisManyNonModels + (4*$modelNum));

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Fetch the ModelKind (aka. ModelType). */
  if (isset($sysParms[$modelPrefix."MODELTYPE"]))
    $modelType = $sysParms[$modelPrefix."MODELTYPE"];

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Fetch the Model HRID. */
  if (isset($sysParms[$modelPrefix."MODELHRID"]))
    $modelHRID = $sysParms[$modelPrefix."MODELHRID"];

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Fetch the Model PATH. */
  if (isset($sysParms[$modelPrefix."MODELPATH"]))
    $modelPath = $sysParms[$modelPrefix."MODELPATH"];

  /* Create a sortable numeric prefix for model information. */
  $modelPrefix = sprintf("%02d-", $modelPrefixNum++);

  /* Fetch the Model LIB. */
  if (isset($sysParms[$modelPrefix."MODELLIB"]))
    $modelLib = $sysParms[$modelPrefix."MODELLIB"];

} /* end CACF_GetModelInfoFromSysParms() */


/****************************************************************
 * CACF_DumpRawDbTableToCsv()
 *		Function to dump a specified raw database table to csv file.
 *
 * Outputs:  (output to csv file)
 ****************************************************************/
function CACF_DumpRawDbTableToCsv($db, &$CACFconstants, $tableName, $csvFileName)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];

  /** Open raw database table audit file for writing. **/
  $csvFile = my_fopen($csvFileName);

  /* Setup query SQL commands. */
  $queryText = "SELECT * FROM $tableName;";

  echo date('H:i:s') . " Begin query to read in $tableName table...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Flag that we need to omit column header row. */
  $needColHeader = 1;

  while( $row = odbc_fetch_array($resultHandle) ) 
    {
      /* See if we need to emit a column header. */
      if ($needColHeader == 1)
        {
          $line = "";
          foreach ($row as $colName => $cellVal)
            {
              /* Start or continue a line as needed. */
              if ($line == "")
                $line = $colName;
              else
                $line = $line.$ofs.$colName;
            }
        
          /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
          fputs($csvFile, $line . "\r\n");
    
          /* Flag that we're done with column header. */
          $needColHeader = 0;
        } /* endif */

      /* Write this row's values. */
      $line = "";
      foreach ($row as $colName => $cellVal)
        {
          /* Start or continue a line as needed. */
          if ($line == "")
            $line = $cellVal;
          else
            $line = $line.$ofs.CACF_RemapReservedChars($cellVal);	/* Remap any ',' chars in cell to '|' chars! */
        }
        
      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($csvFile, $line . "\r\n");

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /** Close the audit output file. **/
  fclose($csvFile);
  echo date('H:i:s') . " Done writing said raw database table to \"$csvFileName\".\n";
} /* end CACF_DumpRawDbTableToCsv() */


/****************************************************************
 * CACF_Init()
 *		Function to perform all necessary initialization.
 *
 * Outputs:  (ALL of the parameters listed are outputs!)
 ****************************************************************/
function CACF_Init(&$db, &$CACFconstants,
                   &$altiumParmsByComponentLines, &$auditComponentsByType, &$auditComponentsByTypeUnmatched, &$altiumUserNamesByGuid, &$altiumAclUserPermissions)
{

  /* Setup ODBC connection info */
  $db_dsn = "Altium Vault database";	// ODBC Data Source Name
  $db_user = "SYSDBA";			// ODBC User Name 
  $db_pass = "masterkey";			// ODBC Password 

  /* Setup some constants for use by this script. */
  $CACFconstants = array();

  /* Setup global constants for use by this script and other scripts. */
  $CACFconstants["ofs"] = 							",";				// Output file separator
  $CACFconstants["pathSep"] = 						"\\";				// Path separator (Note:  This is actually a single backslash!)
  $CACFconstants["revSep"] = 						"-";				// Character separating part number from revision number
  $CACFconstants["constNamingScheme"] = 			'$$!NAMING_SCHEME!$$';	// Special string used in ALU_FOLDERPARAMETER to indicate naming scheme.
  $CACFconstants["constPresent"] = 					'$NODEFAULT';		// Special string that we are inventing to indicate the presence of a particular folder parameter (with no value).
  $CACFconstants["constAbsent"] = 					'*';				// Special string that we are inventing to indicate the absence of a particular folder parameter (with no value).
  $CACFconstants["cInitialRev"] = 					"01";				// Initial revision number/code for a newly-created component.
  $CACFconstants["cRevFormatString"] =				"%02d";				// Printf() format string for our revision number/code scheme.

  /* Flag that we wish to keep 0 length output files that we generate. */
  $CACFconstants["doKeepZeroLengthOutputFiles"] = 	true;

  /* Setup the number of certain system parameters that we will reserve space for in our audit csv output files. */
  $CACFconstants["numPerms"] = 						10;					// The number of permissions that we will allow for each folder/item/itemRev.
  $CACFconstants["reserveForThisManyNonModels"] = 	20;					// Reserve space in .csv files for 20 non-models (ITEMHRID, COMMENT, DESCRIPTION, permissions, etc.) per component.
  $CACFconstants["reserveForThisManyModels"] = 		12;					// Reserve space in .csv files for 12 models per component.
  $CACFconstants["maxPcbLibModels"] =		 		8;					// Maximum number of PcbLib models we allow per component.

  /* Define the file extension of our various output files. */
  $CACFconstants["auditFileExt"] = 	                ".csv";
  $auditFileExt = 									$CACFconstants["auditFileExt"];

  /* Define the suffix that will be appended for the version of certain files describing the obsolete models/components/etc. */
  $CACFconstants["auditObsoleteSuffix"] =          	"_OBSOLETE";

  /* Define the names of our various output files. */
  $CACFconstants["auditComponentsFileName"] =      	"audit_vault_database_components".$auditFileExt;
  $CACFconstants["auditFoldersFileName"] =         	"audit_vault_database_folders".$auditFileExt;
  $CACFconstants["auditModelsFileName"] =          	"audit_vault_database_models".$auditFileExt;
  $CACFconstants["auditModelsWhereUsedFileName"] = 	"audit_vault_database_models_where_used".$auditFileExt;
  $CACFconstants["auditPcbsFileName"] =            	"audit_vault_database_PCBs".$auditFileExt;
  $CACFconstants["auditChangeLogFileName"] =       	"audit_vault_database_change_log".$auditFileExt;
  $CACFconstants["auditAclsFileName"] = 			"audit_vault_database_ACLs".$auditFileExt;
  
  /* Define sub-reports for the various types of components. */
  /* Note:  "FOOBAR" will be replaced by the type of component as defined below! */
  $CACFconstants["auditComponentsByTypeFileName"] =	"audit_vault_database_components_FOOBAR".$auditFileExt;

  /* Define an array to hold per-component audit data. */
  $altiumParmsByComponentLines = array();

  /* Define the name of each type of component for which we want a component type report. */
  $auditComponentsByType = array();
  $auditComponentsByType["instructions"] =			"/0051.-...-.....|1161.-...-...../";	// Regex match for all instructions according to our part numbering scheme.
  $auditComponentsByType["hardware"] =	 			"/37...-...-.....|13100-...-...../";	// Regex match for all hardware according to our part numbering scheme.
  $auditComponentsByType["sockets"] = 				"/423..-...-...../";	// Regex match for all sockets according to our part numbering scheme.
  $auditComponentsByType["connectors"] = 			"/42[45]..-...-...../";	// Regex match for all connectors according to our part numbering scheme.
  $auditComponentsByType["ferrite_beads"] = 		"/431..-...-...../";	// Regex match for all ferrite_beads according to our part numbering scheme.
  $auditComponentsByType["inductors"] = 			"/43[234]..-...-...../";// Regex match for all inductors according to our part numbering scheme.
  $auditComponentsByType["fuses"] = 				"/436..-...-...../";	// Regex match for all fuses according to our part numbering scheme.
  $auditComponentsByType["resistors"] = 			"/44...-...-...../";	// Regex match for all resistors according to our part numbering scheme.
  $auditComponentsByType["capacitors"] = 			"/45...-...-...../";	// Regex match for all capacitors according to our part numbering scheme.
  $auditComponentsByType["diodes"] = 				"/461..-...-...../";	// Regex match for all diodes according to our part numbering scheme.
  $auditComponentsByType["transistors"] = 			"/462..-...-...../";	// Regex match for all transistors according to our part numbering scheme.
  $auditComponentsByType["integrated_circuits"] = 	"/46[3456789]..-...-.....|41630-...-...../";	// Regex match for all integrated_circuits according to our part numbering scheme.
  $auditComponentsByType["modules"] = 				"/47...-...-...../";	// Regex match for all modules according to our part numbering scheme.

  /* Define the unmatched, catchall type.  Note:  This MUST be the last entry! */
  $auditComponentsByTypeUnmatched = "unmatched";
  $auditComponentsByType[$auditComponentsByTypeUnmatched] = "/.....-...-...../";	// Regex match for anything according to our part numbering scheme.

  /* Initialize array holding known SPI usernames. */
  $altiumUserNamesByGuid = array();
  CACF_InitUsernames(&$altiumUserNamesByGuid);

  /* Initialize array holding known SPI ACL permissions. */
  CACF_InitAclUserPermissions(&$altiumAclUserPermissions);

  /* Report to user the start time of this script run. */
  echo date('H:i:s') . " Script begins...\n";

  /* Connect to ODBC server and open specified data source. */
  if ($db = odbc_connect($db_dsn, $db_user, $db_pass))
    {
      echo "Connected to the database.\n";
      //    odbc_close($db);
    } else
    {
      my_die('Connection failed.');
    }

} /* end CACF_Init() */


/****************************************************************
 * CACF_DumpAllRawDatabaseTablesToCsv()
 *		Function to dump all raw database tables to individual .csv files.
 *
 * Outputs:  (output to multiple csv files)
 ****************************************************************/
function CACF_DumpAllRawDatabaseTablesToCsv(&$db, &$CACFconstants)
{
  /* Retrieve necessary global constants. */
  $auditFileExt = 									$CACFconstants["auditFileExt"];

  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_VAULT", "raw_ALU_VAULT".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LANGUAGE", "raw_ALU_LANGUAGE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_CONTENTTYPE", "raw_ALU_CONTENTTYPE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_FOLDERTYPE", "raw_ALU_FOLDERTYPE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMREVISIONLINK", "raw_ALU_ITEMREVISIONLINK".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMREVISIONLINKPARAMETER", "raw_ALU_ITEMREVISIONLINKPARAMETER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMLINK", "raw_ALU_ITEMLINK".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMLINKPARAMETER", "raw_ALU_ITEMLINKPARAMETER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_FOLDERPARAMETER", "raw_ALU_FOLDERPARAMETER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_FOLDER", "raw_ALU_FOLDER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEM", "raw_ALU_ITEM".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMREVISION", "raw_ALU_ITEMREVISION".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMREVISIONPARAMETER", "raw_ALU_ITEMREVISIONPARAMETER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LIFECYCLESTATETRANSITION", "raw_ALU_LIFECYCLESTATETRANSITION".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LIFECYCLESTATE", "raw_ALU_LIFECYCLESTATE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LIFECYCLESTAGE", "raw_ALU_LIFECYCLESTAGE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LIFECYCLEDEFINITION", "raw_ALU_LIFECYCLEDEFINITION".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_LIFECYCLESTATECHANGE", "raw_ALU_LIFECYCLESTATECHANGE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_REVISIONNAMINGSCHEME", "raw_ALU_REVISIONNAMINGSCHEME".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_REVISIONNAMELEVEL", "raw_ALU_REVISIONNAMELEVEL".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_DATAFILE", "raw_ALU_DATAFILE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_DATAFOLDER", "raw_ALU_DATAFOLDER".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_USERROLE", "raw_ALU_USERROLE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_USERINROLE", "raw_ALU_USERINROLE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ACCESSRULE", "raw_ALU_ACCESSRULE".$auditFileExt);
  CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ACLENTRY", "raw_ALU_ACLENTRY".$auditFileExt);
  //CACF_DumpRawDbTableToCsv($db, &$CACFconstants, "ALU_ITEMREVISIONEX", "raw_ALU_ITEMREVISIONEX".$auditFileExt);

} /* end CACF_DumpAllRawDatabaseTablesToCsv() */


/****************************************************************
 * CACF_ReservePerCompTypeUserParmNames()
 *		Function to reserve certain user parameter names that must exist for every component type.
 *
 * Outputs:  $altiumUserParmNames
 ****************************************************************/
function CACF_ReservePerCompTypeUserParmNames(&$altiumUserParmNames, $compType)
{
  /* These are user parameter names that are forced to exist in every component type. */

  /* Note:  Be aware that supplier p/n's in the files and database actually look like: "Supplier Part Number 1".
  But this is insane.  So instead, for auditing purposes, we will make them look like: "Supplier 1 Part Number".
  This way, an alphabetically ordered list gives us "Supplier 1", "Supplier 1 Part Number", etc. pairwise, 
  rather than having all suppliers and then all supplier part numbers. */

  /* Note:  There should be no "," characters here! */
  $altiumUserParmNames["Case-EIA"] = 1;
  $altiumUserParmNames["Case-Metric"] = 1;
  $altiumUserParmNames["Case-Mfg"] = 1;
  $altiumUserParmNames["Case-Package"] = 1;
  $altiumUserParmNames["ComponentLink1Description"] = 1;
  $altiumUserParmNames["ComponentLink1URL"] = 1;
  $altiumUserParmNames["ComponentLink2Description"] = 1;
  $altiumUserParmNames["ComponentLink2URL"] = 1;
  $altiumUserParmNames["ComponentLink3Description"] = 1;
  $altiumUserParmNames["ComponentLink3URL"] = 1;
  $altiumUserParmNames["ComponentLink4Description"] = 1;
  $altiumUserParmNames["ComponentLink4URL"] = 1;
  $altiumUserParmNames["ComponentLink5Description"] = 1;
  $altiumUserParmNames["ComponentLink5URL"] = 1;
  $altiumUserParmNames["ComponentLink6Description"] = 1;
  $altiumUserParmNames["ComponentLink6URL"] = 1;
  $altiumUserParmNames["Date Created"] = 1;
  $altiumUserParmNames["Elements"] = 1;
  $altiumUserParmNames["Failure Rate"] = 1;
  $altiumUserParmNames["Lifecycle Status Date"] = 1;
  $altiumUserParmNames["Lifecycle Status"] = 1;
  $altiumUserParmNames["Manufacturer Part Num1"] = 1;
  $altiumUserParmNames["Manufacturer Part Num2"] = 1;
  $altiumUserParmNames["Manufacturer Part Num3"] = 1;
  $altiumUserParmNames["Manufacturer"] = 1;
  $altiumUserParmNames["Mass"] = 1;
  $altiumUserParmNames["Moisture Sensitivity Level"] = 1;
  $altiumUserParmNames["Operating Temperature"] = 1;
  $altiumUserParmNames["Part Height max mm"] = 1;
  $altiumUserParmNames["Part Length max mm"] = 1;
  $altiumUserParmNames["Part Width max mm"] = 1;
  $altiumUserParmNames["RoHS Status"] = 1;
  $altiumUserParmNames["Soldering--Max reflow temp"] = 1;
  $altiumUserParmNames["Soldering--Max time at max temp"] = 1;
  $altiumUserParmNames["SPI Approved for"] = 1;
  $altiumUserParmNames["SPI Cost (qty 10)"] = 1;
  $altiumUserParmNames["SPI Leadtime/Notes"] = 1;
  $altiumUserParmNames["SPI Preference"] = 1;
  $altiumUserParmNames["SPI Part Number"] = 1;
  $altiumUserParmNames["SPI Path"] = 1;
  $altiumUserParmNames["SPI Visible on BOMs"] = 1;		/* TODO:  Should we keep this around forever???? */
  $altiumUserParmNames["00SPI Visible on BOMs"] = 1;
  $altiumUserParmNames["Supplier 1"] = 1;
  $altiumUserParmNames["Supplier 2"] = 1;
  $altiumUserParmNames["Supplier 3"] = 1;
  $altiumUserParmNames["Supplier 4"] = 1;
  $altiumUserParmNames["Supplier 5"] = 1;
  $altiumUserParmNames["Supplier 6"] = 1;
  $altiumUserParmNames["Supplier 1 Part Number"] = 1;
  $altiumUserParmNames["Supplier 2 Part Number"] = 1;
  $altiumUserParmNames["Supplier 3 Part Number"] = 1;
  $altiumUserParmNames["Supplier 4 Part Number"] = 1;
  $altiumUserParmNames["Supplier 5 Part Number"] = 1;
  $altiumUserParmNames["Supplier 6 Part Number"] = 1;
  $altiumUserParmNames["Technology"] = 1;
  $altiumUserParmNames["Terminations"] = 1;
  $altiumUserParmNames["Tolerance"] = 1;
  $altiumUserParmNames["Topside Marking"] = 1;
  $altiumUserParmNames["Type"] = 1;
  $altiumUserParmNames["Value"] = 1;
  $altiumUserParmNames["Tape Pitch"] = 1;

  /* See if there are any new parameters that we wish to reserve for a particular compType. */
  switch ($compType)
    {
      /* Reserve any new parameters for integrated circuits type. */
      /* Note:  Be sure to also include these in the function below us! */
    case "integrated_circuits":
      {
        $altiumUserParmNames["Accuracy"] = 1;
        $altiumUserParmNames["Function"] = 1;
        $altiumUserParmNames["Frequency"] = 1;
        $altiumUserParmNames["Reset Voltage 1"] = 1;
        $altiumUserParmNames["Reset Voltage 2"] = 1;
        $altiumUserParmNames["Reset Voltage 3"] = 1;
        $altiumUserParmNames["Reset Voltage 4"] = 1;
        $altiumUserParmNames["Reset Voltage 5"] = 1;
        $altiumUserParmNames["Reset Voltage 6"] = 1;
        $altiumUserParmNames["Memory Density"] = 1;
        $altiumUserParmNames["Memory Organization"] = 1;
        $altiumUserParmNames["Interface Type"] = 1;
        $altiumUserParmNames["Delay Propagation"] = 1;
        $altiumUserParmNames["Bits"] = 1;
        $altiumUserParmNames["Logic Function"] = 1;
        $altiumUserParmNames["Temp. Coefficent"] = 1;
        $altiumUserParmNames["Update Rate"] = 1;
        $altiumUserParmNames["Slew Rate"] = 1;
        $altiumUserParmNames["Sample Rate"] = 1;
        $altiumUserParmNames["INL"] = 1;
        $altiumUserParmNames["DNL"] = 1;
        $altiumUserParmNames["Dynamic Range"] = 1;
        $altiumUserParmNames["Total Harmonic Distortion"] = 1;
        $altiumUserParmNames["Signal to Noise Ratio"] = 1;
        $altiumUserParmNames["Settling Time"] = 1;
        $altiumUserParmNames["Resistance"] = 1;
        $altiumUserParmNames["Taps"] = 1;

        break;
      }

    case "capacitors":
      {
        $altiumUserParmNames["Features"] = 1;
        $altiumUserParmNames["Lifetime"] = 1;
        $altiumUserParmNames["Current Leakage"] = 1;
        $altiumUserParmNames["Current Ripple"] = 1;

        break;
      }

    case "connectors":
      {
        $altiumUserParmNames["Thickness PCB"] = 1;
        $altiumUserParmNames["Thickness Panel"] = 1;
        $altiumUserParmNames["Frequency"] = 1;

        break;
      }

    case "diodes":
      {
        $altiumUserParmNames["Color"] = 1;
        $altiumUserParmNames["Lens Color"] = 1;
        $altiumUserParmNames["Wavelength"] = 1;
        $altiumUserParmNames["Composition"] = 1;
        $altiumUserParmNames["Intensity"] = 1;
        $altiumUserParmNames["Viewing Angle"] = 1;
        $altiumUserParmNames["Vz"] = 1;
        $altiumUserParmNames["Resistance"] = 1;

        break;
      }

    case "hardware":
      {
        $altiumUserParmNames["Thickness PCB"] = 1;
        $altiumUserParmNames["Thickness Panel"] = 1;
        $altiumUserParmNames["Diameter"] = 1;
        $altiumUserParmNames["Head Diameter"] = 1;
        $altiumUserParmNames["Hole Diameter"] = 1;
        $altiumUserParmNames["Material"] = 1;
        $altiumUserParmNames["Threads"] = 1;
        $altiumUserParmNames["Color"] = 1;

        break;
      }

    case "inductors":
      {
        $altiumUserParmNames["Frequency"] = 1;

        break;
      }

    case "modules":
      {
        $altiumUserParmNames["Impedance"] = 1;
        $altiumUserParmNames["Distance Rating"] = 1;
        $altiumUserParmNames["Resistance"] = 1;
        $altiumUserParmNames["Vf"] = 1;
        $altiumUserParmNames["Data Rate"] = 1;
        $altiumUserParmNames["Output per chan"] = 1;
        $altiumUserParmNames["Wavelength"] = 1;
        $altiumUserParmNames["Bandwidth -3dB"] = 1;
        $altiumUserParmNames["Output Type"] = 1;
        $altiumUserParmNames["Thickness PCB"] = 1;
        $altiumUserParmNames["Thickness Panel"] = 1;
        $altiumUserParmNames["Current Supply"] = 1;
        $altiumUserParmNames["Efficiency"] = 1;

        break;
      }

    case "resistors":
      {
        $altiumUserParmNames["Adjustment Type"] = 1;
        $altiumUserParmNames["Features"] = 1;
        $altiumUserParmNames["Thickness PCB"] = 1;
        $altiumUserParmNames["Taper"] = 1;
        $altiumUserParmNames["Turns"] = 1;
        $altiumUserParmNames["Lifetime"] = 1;

        break;
      }

    } /* endswitch */

} /* end CACF_ReservePerCompTypeUserParmNames() */


/****************************************************************
 * CACF_ReserveGlobalUserParmNames()
 *		Function to reserve certain user parameter names that may not exist in the Vault just yet.
 *
 * Outputs:  $altiumUserParmNames
 ****************************************************************/
function CACF_ReserveGlobalUserParmNames(&$altiumUserParmNames)
{

  /* Call CACF_ReservePerCompTypeUserParmNames() to reserve most of the user parameter names. */
  CACF_ReservePerCompTypeUserParmNames(&$altiumUserParmNames, "foo");

  /* These are user parameter names that are forced to exist globally, but not in every component type. */
  $altiumUserParmNames["Circuits"] = 1;
  $altiumUserParmNames["Composition"] = 1;
  $altiumUserParmNames["Contact Plating"] = 1;
  $altiumUserParmNames["Core Material"] = 1;
  $altiumUserParmNames["Current Collector"] = 1;
  $altiumUserParmNames["Current Rating"] = 1;
  $altiumUserParmNames["Current Saturation"] = 1;
  $altiumUserParmNames["DC Current Gain"] = 1;
  $altiumUserParmNames["Impedance"] = 1;
  $altiumUserParmNames["Pitch"] = 1;
  $altiumUserParmNames["Power max"] = 1;
  $altiumUserParmNames["Power per Element"] = 1;
  $altiumUserParmNames["Power Total"] = 1;
  $altiumUserParmNames["Q"] = 1;
  $altiumUserParmNames["Resistance"] = 1;
  $altiumUserParmNames["Shielding"] = 1;
  $altiumUserParmNames["Temp. Coefficent"] = 1;
  $altiumUserParmNames["Value"] = 1;
  $altiumUserParmNames["Voltage ce max"] = 1;
  $altiumUserParmNames["Voltage Rating"] = 1;
  $altiumUserParmNames["Voltage vcesat"] = 1;
  $altiumUserParmNames["SPI Path"] = 1;
  $altiumUserParmNames["Tape Pitch"] = 1;

  /* These are integrated-circuit specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Accuracy"] = 1;
  $altiumUserParmNames["Function"] = 1;
  $altiumUserParmNames["Frequency"] = 1;
  $altiumUserParmNames["Reset Voltage 1"] = 1;
  $altiumUserParmNames["Reset Voltage 2"] = 1;
  $altiumUserParmNames["Reset Voltage 3"] = 1;
  $altiumUserParmNames["Reset Voltage 4"] = 1;
  $altiumUserParmNames["Reset Voltage 5"] = 1;
  $altiumUserParmNames["Reset Voltage 6"] = 1;
  $altiumUserParmNames["Memory Density"] = 1;
  $altiumUserParmNames["Memory Organization"] = 1;
  $altiumUserParmNames["Interface Type"] = 1;
  $altiumUserParmNames["Delay Propagation"] = 1;
  $altiumUserParmNames["Bits"] = 1;
  $altiumUserParmNames["Logic Function"] = 1;
  $altiumUserParmNames["Temp. Coefficent"] = 1;
  $altiumUserParmNames["Update Rate"] = 1;
  $altiumUserParmNames["Slew Rate"] = 1;
  $altiumUserParmNames["Sample Rate"] = 1;
  $altiumUserParmNames["INL"] = 1;
  $altiumUserParmNames["DNL"] = 1;
  $altiumUserParmNames["Dynamic Range"] = 1;
  $altiumUserParmNames["Total Harmonic Distortion"] = 1;
  $altiumUserParmNames["Signal to Noise Ratio"] = 1;
  $altiumUserParmNames["Settling Time"] = 1;
  $altiumUserParmNames["Resistance"] = 1;
  $altiumUserParmNames["Taps"] = 1;

  /* These are capacitor specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Features"] = 1;
  $altiumUserParmNames["Lifetime"] = 1;
  $altiumUserParmNames["Current Leakage"] = 1;
  $altiumUserParmNames["Current Ripple"] = 1;

  /* These are connector specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Thickness PCB"] = 1;
  $altiumUserParmNames["Thickness Panel"] = 1;
  $altiumUserParmNames["Frequency"] = 1;

  /* These are diode specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Color"] = 1;
  $altiumUserParmNames["Lens Color"] = 1;
  $altiumUserParmNames["Wavelength"] = 1;
  $altiumUserParmNames["Composition"] = 1;
  $altiumUserParmNames["Intensity"] = 1;
  $altiumUserParmNames["Viewing Angle"] = 1;
  $altiumUserParmNames["Vz"] = 1;
  $altiumUserParmNames["Resistance"] = 1;

  /* These are hardware specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Diameter"] = 1;
  $altiumUserParmNames["Hole Diameter"] = 1;
  $altiumUserParmNames["Head Diameter"] = 1;
  $altiumUserParmNames["Material"] = 1;
  $altiumUserParmNames["Threads"] = 1;
  $altiumUserParmNames["Color"] = 1;

  /* These are module specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Distance Rating"] = 1;
  $altiumUserParmNames["Data Rate"] = 1;
  $altiumUserParmNames["Output per chan"] = 1;
  $altiumUserParmNames["Efficiency"] = 1;

  /* These are resistor specific parameters. */
  /* Note:  Be sure to also include these in the function above us! */
  $altiumUserParmNames["Adjustment Type"] = 1;
  $altiumUserParmNames["Taper"] = 1;
  $altiumUserParmNames["Turns"] = 1;

  /* TODO:  Should we be reserving these as user parameter names?  These are system parameters! */
  $altiumUserParmNames["Comment"] = 1;			/* TODO: ???? */
  $altiumUserParmNames["Description"] = 1;		/* TODO: ???? */

} /* end CACF_ReserveGlobalUserParmNames() */


/****************************************************************
 * CACF_AnalyzeVaultFolders()
 *		Function to analyze all Vault folders and extract all linkages so that we understand folder trees.
 *
 * Outputs:  $altiumFoldersByGuid
 ****************************************************************/
function CACF_AnalyzeVaultFolders(&$db, &$CACFconstants,
                                  &$altiumFoldersByGuid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT FOLDER.GUID, FOLDER.HRID, FOLDER.PARENTFOLDERGUID
FROM ALU_FOLDER FOLDER
;
';

  echo date('H:i:s') . " Begin query to read in folder tree from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache info for Altium Vault folders. */
  $altiumFoldersByGuid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the fields of interest from this query result. */
      $GUID = odbc_result($resultHandle, "GUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $PARENTFOLDERGUID = odbc_result($resultHandle, "PARENTFOLDERGUID");

      /* At this GUID, create an array to store the HRID and PARENTFOLDERGUID fields. */
      $altiumFoldersByGuid[$GUID] = array("HRID" => $HRID, 
                                          "PARENTFOLDERGUID" => $PARENTFOLDERGUID);

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Examine all $altiumFoldersByGuid */
  foreach ($altiumFoldersByGuid as $GUID => $value)
    {
      foreach ($value as $key2 => $val2)
        {
          //		print "GUID is $GUID, key2 is $key2, val2 is $val2\n";

        } /* end foreach */

    } /* end foreach */

} /* end CACF_AnalyzeVaultFolders() */


/****************************************************************
 * CACF_AnalyzeVaultItems()
 *		Function to analyze all Vault items and cache certain fields, indexed by GUID.
 *
 * Outputs:  $altiumItemsByGuid
 ****************************************************************/
function CACF_AnalyzeVaultItems(&$db, &$CACFconstants,
                                &$altiumItemsByGuid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEM.GUID, ITEM.HRID, ITEM.DESCRIPTION, ITEM.FOLDERGUID, CONTENTTYPE.HRID AS CONTENTTYPEHRID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
;
';

  echo date('H:i:s') . " Begin query to read in all items from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium items, indexed by item GUID. */
  $altiumItemsByGuid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the "GUID" and "HRID" fields. */
      $GUID = odbc_result($resultHandle, "GUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $CONTENTTYPEHRID = odbc_result($resultHandle, "CONTENTTYPEHRID");

      //    echo "Storing info for item GUID $GUID\n";

      /* At this GUID, create an array to store the HRID and a few other fields. */
      $altiumItemsByGuid[$GUID] = array("HRID" => $HRID, 
                                        "FOLDERGUID" => $FOLDERGUID,
                                        "CONTENTTYPEHRID" => $CONTENTTYPEHRID
                                        );

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_AnalyzeVaultItems() */


/****************************************************************
 * CACF_AnalyzeVaultItemRevisions()
 *		Function to analyze all Vault item revisions and cache certain fields, indexed by GUID.
 *
 * Outputs:  $altiumItemRevsByGuid
 ****************************************************************/
function CACF_AnalyzeVaultItemRevisions(&$db, &$CACFconstants,
                                        &$altiumItemRevsByGuid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, ITEMREV.HRID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, ITEMREV.ANCESTORITEMREVISIONGUID, ITEMREV.ITEMGUID, ITEMREV.REVISIONID
FROM ALU_ITEMREVISION ITEMREV
;
';

  echo date('H:i:s') . " Begin query to read in all item revisions from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium user parameter names and values, indexed by item revision GUID. */
  $altiumItemRevsByGuid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the "GUID" and "HRID" fields. */
      $GUID = odbc_result($resultHandle, "GUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $REVISIONID = odbc_result($resultHandle, "REVISIONID");
      $COMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "COMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $DESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "DESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $ANCESTORITEMREVISIONGUID = odbc_result($resultHandle, "ANCESTORITEMREVISIONGUID");
      $ITEMGUID = odbc_result($resultHandle, "ITEMGUID");

      //    echo "Storing info for itemRev GUID $GUID\n";

      /* At this GUID, create an array to store the HRID and a few other fields. */
      /* Create placeholders for MODELPATH and OBSOLETE. */
      $altiumItemRevsByGuid[$GUID] = array("ITEMGUID" => $ITEMGUID, 
                                           "HRID" => $HRID, 
                                           "REVISIONID" => $REVISIONID, 
                                           "COMMENT" => $COMMENT, 
                                           "DESCRIPTION" => $DESCRIPTION, 
                                           "MODELPATH" => "TBD",
                                           "OBSOLETE" => 0
                                           );

      /* See if this ItemRev has an ancestor ItemRev.  If so, go back and mark it as obsolete. */
      /* NOTE:  Here we assume that we will always see ancestors before their children! */
      if ($ANCESTORITEMREVISIONGUID != "")
        {
          //          echo "          Marking GUID \"$ANCESTORITEMREVISIONGUID\" as obsolete!\n";

          /* Mark the ancestor ItemRev as obsolete. */
          $altiumItemRevsByGuid[$ANCESTORITEMREVISIONGUID]["OBSOLETE"] = 1;

        }

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_AnalyzeVaultItemRevisions() */


/****************************************************************
 * CACF_AnalyzeVaultVaults()
 *		Function to analyze all Vault Vaults and cache certain fields, 
 * indexed by HRID.
 *
 * Outputs:  $altiumVaultsByHrid
 ****************************************************************/
function CACF_AnalyzeVaultVaults(&$db, &$CACFconstants,
                                 &$altiumVaultsByHrid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT AV.HRID, AV.GUID

FROM ALU_VAULT AV
;
';

  echo date('H:i:s') . " Begin query to read in all defined Vaults from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium user parameter names and values, indexed by item revision GUID. */
  $altiumVaultsByHrid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the "GUID" and "HRID" fields. */
      $HRID = odbc_result($resultHandle, "HRID");
      $GUID = odbc_result($resultHandle, "GUID");

      /* At this HRID, create an array to store the GUID and a few other fields. */
      $altiumVaultsByHrid[$HRID] = array("GUID" => $GUID
                                         );

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_AnalyzeVaultVaults() */


/****************************************************************
 * CACF_AnalyzeVaultRevisionNamingSchemes()
 *		Function to analyze all Vault Revision Naming Schemes and 
 * cache certain fields, indexed by HRID.
 *
 * Outputs:  $altiumRevisionNamingSchemesByHrid
 ****************************************************************/
function CACF_AnalyzeVaultRevisionNamingSchemes(&$db, &$CACFconstants,
                                                &$altiumRevisionNamingSchemesByHrid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT RNS.HRID, RNS.GUID

FROM ALU_REVISIONNAMINGSCHEME RNS
;
';

  echo date('H:i:s') . " Begin query to read in all defined Revision Naming Schemes from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium user parameter names and values, indexed by item revision GUID. */
  $altiumRevisionNamingSchemesByHrid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the "GUID" and "HRID" fields. */
      $HRID = odbc_result($resultHandle, "HRID");
      $GUID = odbc_result($resultHandle, "GUID");

      /* At this HRID, create an array to store the GUID and a few other fields. */
      $altiumRevisionNamingSchemesByHrid[$HRID] = array("GUID" => $GUID
                                                        );

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_AnalyzeVaultRevisionNamingSchemes() */


/****************************************************************
 * CACF_AnalyzeVaultLifeCycleDefinitions()
 *		Function to analyze all Vault LifeCycle Definitions and 
 * cache certain fields, indexed by HRID.
 *
 * Outputs:  $altiumLifeCycleDefinitionsByHrid
 ****************************************************************/
function CACF_AnalyzeVaultLifeCycleDefinitions(&$db, &$CACFconstants,
                                               &$altiumLifeCycleDefinitionsByHrid)
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT LCD.HRID, LCD.GUID

FROM ALU_LIFECYCLEDEFINITION LCD
;
';

  echo date('H:i:s') . " Begin query to read in all defined Revision Naming Schemes from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium user parameter names and values, indexed by item revision GUID. */
  $altiumLifeCycleDefinitionsByHrid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the "GUID" and "HRID" fields. */
      $HRID = odbc_result($resultHandle, "HRID");
      $GUID = odbc_result($resultHandle, "GUID");

      /* At this HRID, create an array to store the GUID and a few other fields. */
      $altiumLifeCycleDefinitionsByHrid[$HRID] = array("GUID" => $GUID
                                                        );

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_AnalyzeVaultLifeCycleDefinitions() */


/****************************************************************
 * CACF_AnalyzeVaultFolderUserParameters()
 *		Function to analyze all Vault folder user parameters and store for later use.
 *
 * Outputs:  $altiumUserParmNames, $altiumFolderUserParmValuesByGuid
 ****************************************************************/
function CACF_AnalyzeVaultFolderUserParameters(&$db, &$CACFconstants,
                                               &$altiumUserParmNames, &$altiumFolderUserParmValuesByGuid)
{
  /* Retrieve necessary global constants. */
  $constNamingScheme = 								$CACFconstants["constNamingScheme"];
  $constPresent = 									$CACFconstants["constPresent"];

  /* Setup query SQL commands. */
  $queryText = '
SELECT FP.FOLDERGUID, FP.HRID AS PARAMETERNAME, FP.DEFAULTVALUE AS PARAMETERVALUE
FROM ALU_FOLDERPARAMETER FP
';

  echo date('H:i:s') . " Begin query to read in all folder user parameters from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will hold the universe of Altium user parameter names. */
  $altiumUserParmNames = array();

  /* Clear array that will cache all Altium folder user parameter names and values, indexed by FOLDERGUID. */
  $altiumFolderUserParmValuesByGuid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the field named "PARAMETERNAME" from query results. */
      /* Do a regex substiution on one particular parameter name to transform 'Supplier Part Number 1' to 'Supplier 1 Part Number', etc. */
      $PARAMETERNAME = CACF_AlterAltiumUserParmName(odbc_result($resultHandle, "PARAMETERNAME"));

      /* Explicitly exclude "$$!NAMING_SCHEME!$$" special string. */
      if ($PARAMETERNAME != $constNamingScheme)
        {
          /* Store this Altium user parameter name in array. */
          $altiumUserParmNames[$PARAMETERNAME] = 1;
        }

      /* Extract the "FOLDERGUID" and "PARAMETERVALUE" fields as well. */
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $PARAMETERVALUE = odbc_result($resultHandle, "PARAMETERVALUE");

      /* Replace a null PARAMETERVALUE with a special string. */
      if ($PARAMETERVALUE == "")
        {
          $PARAMETERVALUE = $constPresent;
        }

      /* See if there is already an array setup for this FOLDERGUID. */
      if (isset($altiumFolderUserParmValuesByGuid[$FOLDERGUID]))
        {
          //		echo "Already have an entry at FOLDERGUID $FOLDERGUID.\n";
          $altiumFolderUserParmValuesByGuid[$FOLDERGUID][$PARAMETERNAME] = $PARAMETERVALUE;
        }

      else
        {
          //		echo "Need to make an entry at FOLDERGUID $FOLDERGUID.\n";
          $altiumFolderUserParmValuesByGuid[$FOLDERGUID] = array($PARAMETERNAME => $PARAMETERVALUE);
        }

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Examine all $altiumFolderUserParmValuesByGuid */
  foreach ($altiumFolderUserParmValuesByGuid as $FOLDERGUID => $value)
    {
      foreach ($value as $PARAMETERNAME => $PARAMETERVALUE)
        {
          //		print "FOLDERGUID is $FOLDERGUID, PARAMETERNAME is $PARAMETERNAME, PARAMETERVALUE is $PARAMETERVALUE\n";

        }

    }
  //print "\n";

} /* end CACF_AnalyzeVaultFolderUserParameters() */


/****************************************************************
 * CACF_AnalyzeVaultItemUserParameters()
 *		Function to analyze all Vault item user parameters and store for later use.
 *
 * Outputs:  $altiumUserParmNames, $altiumItemUserParmValuesByGuid
 ****************************************************************/
function CACF_AnalyzeVaultItemUserParameters(&$db, &$CACFconstants,
                                             &$altiumUserParmNames, &$altiumItemUserParmValuesByGuid) 
{

  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, IRP.HRID AS PARAMETERNAME, IRP.PARAMETERVALUE
FROM ALU_ITEMREVISION ITEMREV

LEFT JOIN 
(SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, ITEM.HRID AS FOLDERHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

LEFT JOIN ALU_ITEMREVISIONPARAMETER IRP ON ITEMREV.GUID = IRP.ITEMREVISIONGUID
WHERE ITEMINFO.CONTENTTYPEHRID = \'altium-component\'
;
';

  echo date('H:i:s') . " Begin query to read in all item user parameters from Vault database...\n";

  /* Execute SQL query. */
     $resultHandle = odbc_exec($db, $queryText);

  /* Clear array that will cache all Altium item user parameter names and values, indexed by item revision GUID. */
  $altiumItemUserParmValuesByGuid = array();

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the field named "PARAMETERNAME" from query results. */
      /* Do a regex substiution on one particular parameter name to transform 'Supplier Part Number 1' to 'Supplier 1 Part Number', etc. */
      $PARAMETERNAME = CACF_AlterAltiumUserParmName(odbc_result($resultHandle, "PARAMETERNAME"));

      /* Store this Altium user parameter name in array. */
      $altiumUserParmNames[$PARAMETERNAME] = 1;

      /* Extract the "GUID" and "PARAMETERVALUE" fields as well. */
      $GUID = odbc_result($resultHandle, "GUID");
      $PARAMETERVALUE = odbc_result($resultHandle, "PARAMETERVALUE");

      /* See if there is already an array setup for this GUID. */
      if (isset($altiumItemUserParmValuesByGuid[$GUID]))
        {
          //		echo "Already have an entry at GUID $GUID.\n";
          $altiumItemUserParmValuesByGuid[$GUID][$PARAMETERNAME] = $PARAMETERVALUE;
        }

      else
        {
          //		echo "Need to make an entry at GUID $GUID.\n";
          $altiumItemUserParmValuesByGuid[$GUID] = array($PARAMETERNAME => $PARAMETERVALUE);
        }

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Examine all $altiumItemUserParmValuesByGuid */
  foreach ($altiumItemUserParmValuesByGuid as $GUID => $value)
    {
      foreach ($value as $PARAMETERNAME => $PARAMETERVALUE)
        {
          //		print "GUID is $GUID, PARAMETERNAME is $PARAMETERNAME, PARAMETERVALUE is $PARAMETERVALUE\n";

        }

    }
  //print "\n";


  /* Add placeholders for some user parameters that may not exist just yet. */
  CACF_ReserveGlobalUserParmNames(&$altiumUserParmNames);


  /* Sort all Altium user parameter names stored in the array, by array key. */
  $rc = ksort($altiumUserParmNames);
  if ($rc == FALSE) my_die("ksort() failed!");
  
  /* Loop over all the defined Altium user parameter names. */
  //echo "About to list all defined Altium user parameter names:\n";
  foreach ($altiumUserParmNames as $key => $value)
    {
      //	print "$key\n";
    }
  //print "\n";
  
} /* end CACF_AnalyzeVaultItemUserParameters() */


/****************************************************************
 * CACF_CreateModelAuditData()
 *		Function to create model audit data.
 *
 * Outputs:  $altiumItemRevsByGuid, $altiumModelDataByItemRevHrid
 ****************************************************************/
function CACF_CreateModelAuditData(&$db, &$CACFconstants,
                                   &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                   &$altiumItemRevsByGuid, &$altiumModelDataByItemRevHrid)
{
  /* Retrieve necessary global constants. */
  $revSep = 										$CACFconstants["revSep"];

  //  echo "In CACF_CreateModelAuditData().\n";

  //  echo "altiumAclDataByObjectGuid is:\n";
  //  print_r($altiumAclDataByObjectGuid);

  /* Create array to hold model data. */
  $altiumModelDataByItemRevHrid = array();

  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, ITEMINFO.HRID AS ITEMHRID, ITEMINFO.GUID AS ITEMGUID, ITEMREV.HRID, ITEMREV.CREATEDBYGUID, ITEMREV.LASTMODIFIEDBYGUID, ITEMREV.CREATEDAT, ITEMREV.LASTMODIFIEDAT, ITEMREV.REVISIONID, ITEMREV.ANCESTORITEMREVISIONGUID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, LCS.HRID AS LIFECYCLESTATEHRID, ITEMREV.RELEASEDATE, ITEMINFO.SHARINGCONTROL, ITEMINFO.FOLDERHRID, ITEMINFO.LIFECYCLEDEFINITIONHRID, ITEMINFO.REVISIONNAMINGSCHEMEHRID, ITEMINFO.FOLDERGUID, DATAFILE.HRID AS MODELLIB, DATAFILE.DATAFOLDERGUID, IRL.HRID AS MODELTYPE
FROM ALU_ITEMREVISION ITEMREV

LEFT JOIN 
  (SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, FOLDER.HRID AS FOLDERHRID, LIFECYCLEDEF.HRID AS LIFECYCLEDEFINITIONHRID, REVNAMESCHEME.HRID AS REVISIONNAMINGSCHEMEHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID, ITEM.FOLDERGUID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
LEFT JOIN ALU_FOLDER FOLDER ON ITEM.FOLDERGUID = FOLDER.GUID
LEFT JOIN ALU_LIFECYCLEDEFINITION LIFECYCLEDEF ON ITEM.LIFECYCLEDEFINITIONGUID = LIFECYCLEDEF.GUID
LEFT JOIN ALU_REVISIONNAMINGSCHEME REVNAMESCHEME ON ITEM.REVISIONNAMINGSCHEMEGUID = REVNAMESCHEME.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

LEFT JOIN ALU_DATAFILE DATAFILE ON ITEMREV.GUID = DATAFILE.OWNERGUID
LEFT JOIN ALU_LIFECYCLESTATE LCS ON ITEMREV.LIFECYCLESTATEGUID = LCS.GUID
LEFT JOIN ALU_ITEMREVISIONLINK IRL ON IRL.CHILDITEMREVISIONGUID = ITEMREV.GUID
WHERE ( (ITEMINFO.CONTENTTYPEHRID = \'altium-symbol\') OR (ITEMINFO.CONTENTTYPEHRID = \'altium-pcb-component\') )
ORDER BY ITEMREV.HRID
;
';

  echo date('H:i:s') . " Begin query to read in all model info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $ITEMHRID = odbc_result($resultHandle, "ITEMHRID");
      $GUID = odbc_result($resultHandle, "GUID");
      $ITEMGUID = odbc_result($resultHandle, "ITEMGUID");
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $REVISIONID = $revSep . odbc_result($resultHandle, "REVISIONID");
      $ANCESTORITEMREVISIONGUID = odbc_result($resultHandle, "ANCESTORITEMREVISIONGUID");
      $COMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "COMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $DESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "DESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $LIFECYCLESTATEHRID = odbc_result($resultHandle, "LIFECYCLESTATEHRID");
      $RELEASEDATE = odbc_result($resultHandle, "RELEASEDATE");
      $SHARINGCONTROL = odbc_result($resultHandle, "SHARINGCONTROL");
      $FOLDERHRID = odbc_result($resultHandle, "FOLDERHRID");
      $LIFECYCLEDEFINITIONHRID = odbc_result($resultHandle, "LIFECYCLEDEFINITIONHRID");
      $REVISIONNAMINGSCHEMEHRID = odbc_result($resultHandle, "REVISIONNAMINGSCHEMEHRID");
      $MODELTYPE = odbc_result($resultHandle, "MODELTYPE");
      $MODELLIB = CACF_RemapReservedChars(odbc_result($resultHandle, "MODELLIB"));		/* Re-map ',' chars in MODELLIB field to '|'! */
      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");

      /* Lookup the usernames of the person to create and last modify this folder. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      /* Lookup the ANCESTORITEMREVISIONHRID from ANCESTORITEMREVISIONGUID. */
      $ANCESTORITEMREVISIONHRID = "";
      if ($ANCESTORITEMREVISIONGUID != "")
        $ANCESTORITEMREVISIONHRID = $altiumItemRevsByGuid[$ANCESTORITEMREVISIONGUID]["HRID"];

      /* Lookup the full path of the leaf folder we were just given. */
      $MODELPATH = CACF_TraceFolderPath(&$CACFconstants, $altiumFoldersByGuid, $FOLDERGUID);

      /* Cache the full path to this model for later use when generating the component audit report. */
      $altiumItemRevsByGuid[$GUID]["MODELPATH"] = $MODELPATH;
      //    echo "Cached MODELPATH for GUID $GUID as \"$MODELPATH\".\n";

      /* The MODELTYPE is only valid for models that have been linked to a component.
       For those that haven't yet been linked, there will not be an entry in the ALU_ITEMREVISIONLINK
       table, and the MODELTYPE will be set to "".  In this case, do the best we can and extract 
       the file extension from the end of the MODELLIB. */
      if ($MODELTYPE == "")
        {
          $MODELTYPE = strtoupper(preg_replace("/^.*\./", "", $MODELLIB));
        }

      /* Create a new line in the array that stores all our data. */
      $altiumModelDataByItemRevHrid[$HRID] = array();
      
      /* Store all the fields that are needed for writing to csv file, etc. */
      $altiumModelDataByItemRevHrid[$HRID]["GUID"] = $GUID;
      $altiumModelDataByItemRevHrid[$HRID]["ITEMGUID"] = $ITEMGUID;
      $altiumModelDataByItemRevHrid[$HRID]["ITEMHRID"] = $ITEMHRID;
      $altiumModelDataByItemRevHrid[$HRID]["REVISIONID"] = $REVISIONID;
      $altiumModelDataByItemRevHrid[$HRID]["ANCESTORITEMREVISIONHRID"] = $ANCESTORITEMREVISIONHRID;
      $altiumModelDataByItemRevHrid[$HRID]["COMMENT"] = $COMMENT;
      $altiumModelDataByItemRevHrid[$HRID]["DESCRIPTION"] = $DESCRIPTION;
      $altiumModelDataByItemRevHrid[$HRID]["MODELTYPE"] = $MODELTYPE;
      $altiumModelDataByItemRevHrid[$HRID]["MODELPATH"] = $MODELPATH;
      $altiumModelDataByItemRevHrid[$HRID]["MODELLIB"] = $MODELLIB;
      $altiumModelDataByItemRevHrid[$HRID]["CREATEDBY"] = $CREATEDBY;
      $altiumModelDataByItemRevHrid[$HRID]["CREATEDAT"] = $CREATEDAT;
      $altiumModelDataByItemRevHrid[$HRID]["LASTMODIFIEDBY"] = $LASTMODIFIEDBY;
      $altiumModelDataByItemRevHrid[$HRID]["LASTMODIFIEDAT"] = $LASTMODIFIEDAT;
      $altiumModelDataByItemRevHrid[$HRID]["LIFECYCLESTATEHRID"] = $LIFECYCLESTATEHRID;
      $altiumModelDataByItemRevHrid[$HRID]["RELEASEDATE"] = $RELEASEDATE;
      $altiumModelDataByItemRevHrid[$HRID]["LIFECYCLEDEFINITIONHRID"] = $LIFECYCLEDEFINITIONHRID;
      $altiumModelDataByItemRevHrid[$HRID]["REVISIONNAMINGSCHEMEHRID"] = $REVISIONNAMINGSCHEMEHRID;
      $altiumModelDataByItemRevHrid[$HRID]["SHARINGCONTROL"] = $SHARINGCONTROL;

      /* Get all the permission fields that exist for this model. */
      foreach ($altiumAclDataByObjectGuid[$GUID] as $PERMNAME => $PERMVALUE)
        {
          /* Copy permission field name and permission field value to target data structure. */
          $altiumModelDataByItemRevHrid[$HRID][$PERMNAME] = $PERMVALUE;
          
        } /* end foreach */

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_CreateModelAuditData() */


/****************************************************************
 * CACF_WriteSpecifiedAuditDataToCsv()
 *		Function to create specified audit data and write to csv file.
 * Currently works for models, PCBs, and ACLs.
 *
 * Outputs:  (text written to csv file)
 ****************************************************************/
function CACF_WriteSpecifiedAuditDataToCsv(&$db, &$CACFconstants,
                                           $auditSpecifiedsFileName, &$altiumUserNamesByGuid, &$altiumFoldersByGuid, 
                                           $auditType, &$altiumItemRevsByGuid,
                                           &$altiumSpecifiedDataByItemRevHrid)
{
//  echo "In CACF_WriteSpecifiedAuditDataToCsv(), auditSpecifiedsFileName is \"$auditSpecifiedsFileName\".\n";

  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $auditFileExt = 									$CACFconstants["auditFileExt"];
  $auditObsoleteSuffix = 							$CACFconstants["auditObsoleteSuffix"];

  
  /** Open audit specifieds file for writing. **/
  $auditSpecifiedsFile = my_fopen($auditSpecifiedsFileName);
  
  /** See if we have the concept of "OBSOLETE" for this audit type. **/
  if ( ($auditType == "models") || ($auditType == "PCBs") )
    {

      /** Open another version of this file to describe obsolete specifieds. **/
      $auditSpecifiedsObsoleteFileName = preg_replace("/".$auditFileExt."/", $auditObsoleteSuffix.$auditFileExt, $auditSpecifiedsFileName);
      $auditSpecifiedsObsoleteFile = my_fopen($auditSpecifiedsObsoleteFileName);
      
    } /* endif */

  /* Flag that we need to output column headers. */
  $doColumnHeaders = 1;

  /* Loop over all the lines in the $altiumSpecifiedDataByItemRevHrid array holding specified data. */
  foreach ($altiumSpecifiedDataByItemRevHrid as $ItemRevHrid => $val)
    {

      /** Handle column headers **/
      /* See if we need to output column headers. */
      if ($doColumnHeaders == 1)
        {
          /* Clear output line. */
          $line = "";

          /* Loop over all the fields. */
          foreach ($altiumSpecifiedDataByItemRevHrid[$ItemRevHrid] as $key => $value)
            {
              /* Exclude a few fields from being written to csv for models and PCBs only. */
              if  (!(  ( ($auditType == "models") || ($auditType == "PCBs") ) && ( ($key == "GUID") || ($key == "ITEMGUID") )  ))
                {
                  /* Add field separator if needed. */
                  if ($line != "")
                    $line = $line . $ofs;

                  /* Add the name of this field to output line. */
                  $line = $line . $key;

                } /* endif */

            } /* end foreach field */

          /* Write line to both files.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
          fputs($auditSpecifiedsFile, $line . "\r\n");

          if ( ($auditType == "models") || ($auditType == "PCBs") )
            fputs($auditSpecifiedsObsoleteFile, $line . "\r\n");

          /* Flag that we're done with column headers. */
          $doColumnHeaders = 0;

        } /* endif need to do column headers */


      /** Handle field data **/
      /* Extract GUID that we need for other purposes. */
      if (isset($altiumSpecifiedDataByItemRevHrid[$ItemRevHrid]["GUID"]))
        $GUID = $altiumSpecifiedDataByItemRevHrid[$ItemRevHrid]["GUID"];
      else
        $GUID = '';

      /* Clear output line. */
      $line = "";

      /* Loop over all the fields. */
      foreach ($altiumSpecifiedDataByItemRevHrid[$ItemRevHrid] as $key => $value)
        {
          /* Exclude a few fields from being written to csv for models and PCBs only. */
          if  (!(  ( ($auditType == "models") || ($auditType == "PCBs") ) && ( ($key == "GUID") || ($key == "ITEMGUID") )  ))
            {
              /* Add field separator if needed. */
              if ($line != "")
                $line = $line . $ofs;

              /* Add the value of this field to output line. */
              $line = $line . $value;

            } /* endif */

        } /* end foreach field */

      /** See if we have the concept of "OBSOLETE" for this audit type. **/
      if ( ($auditType == "models") || ($auditType == "PCBs") )
        {

          /* Figure out if this specified is obsolete or not and write this line to the appropriate output file. */
          if ($altiumItemRevsByGuid[$GUID]["OBSOLETE"] == 0)
            {
              /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
              fputs($auditSpecifiedsFile, $line . "\r\n");
            }
    
          else
            {
              /* Write line to obsolete file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
              fputs($auditSpecifiedsObsoleteFile, $line . "\r\n");
            }

        } /* endif we have the concept of OBSOLETE for this audit type */
      
      /* Else just write the data to file already. */
      else
        {
          /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
          fputs($auditSpecifiedsFile, $line . "\r\n");
        }
      
    } /* end foreach line in array */

  /** Close the audit output files. **/
  fclose($auditSpecifiedsFile);

  if ( ($auditType == "models") || ($auditType == "PCBs") )
    fclose($auditSpecifiedsObsoleteFile);

  echo date('H:i:s') . " Done writing audit $auditType file \"$auditSpecifiedsFileName.\"\n";

  /* Clear array that held specified data. */
  $altiumSpecifiedDataByItemRevHrid = array();

} /* end CACF_WriteSpecifiedAuditDataToCsv() */


/****************************************************************
 * CACF_CreateModelAuditDataAndWriteToCsv()
 *		Function to create model audit data and write to csv file.
 *
 * Outputs:  $altiumItemRevsByGuid, (text written to csv file)
 ****************************************************************/
function CACF_CreateModelAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                                &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                                &$altiumItemRevsByGuid)
{
//  echo "In CACF_CreateModelAuditDataAndWriteToCsv(), auditModelsFileName is \"$auditModelsFileName\".\n";

  /* Retrieve necessary global constants. */
  $auditModelsFileName = 							$CACFconstants["auditModelsFileName"];

  /* Create model audit data. */
  CACF_CreateModelAuditData(&$db, &$CACFconstants,
                            &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                            &$altiumItemRevsByGuid, &$altiumModelDataByItemRevHrid);
    
  /* Write audit data to csv file. */
  $auditSpecifiedsFileName = $auditModelsFileName;
  $auditType = "models";
  $altiumSpecifiedDataByItemRevHrid = &$altiumModelDataByItemRevHrid;
  CACF_WriteSpecifiedAuditDataToCsv(&$db, &$CACFconstants,
                                    $auditSpecifiedsFileName, &$altiumUserNamesByGuid, &$altiumFoldersByGuid, 
                                    $auditType, &$altiumItemRevsByGuid,
                                    &$altiumSpecifiedDataByItemRevHrid);
  
} /* end CACF_CreateModelAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreatePcbAuditData()
 *		Function to create Pcb audit data.
 *
 * Outputs:  $altiumItemRevsByGuid, $altiumPcbDataByItemRevHrid
 ****************************************************************/
function CACF_CreatePcbAuditData(&$db, &$CACFconstants,
                                   &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                   &$altiumItemRevsByGuid, &$altiumPcbDataByItemRevHrid)
{
  /* Retrieve necessary global constants. */
  $revSep = 										$CACFconstants["revSep"];

//  echo "In CACF_CreatePcbAuditData().\n";

  /* Create array to hold Pcb data. */
  $altiumPcbDataByItemRevHrid = array();

  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, ITEMINFO.HRID AS ITEMHRID, ITEMINFO.GUID AS ITEMGUID, ITEMREV.HRID, ITEMREV.CREATEDBYGUID, ITEMREV.LASTMODIFIEDBYGUID, ITEMREV.CREATEDAT, ITEMREV.LASTMODIFIEDAT, ITEMREV.REVISIONID, ITEMREV.ANCESTORITEMREVISIONGUID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, LCS.HRID AS LIFECYCLESTATEHRID, ITEMREV.RELEASEDATE, ITEMINFO.SHARINGCONTROL, ITEMINFO.FOLDERHRID, ITEMINFO.LIFECYCLEDEFINITIONHRID, ITEMINFO.REVISIONNAMINGSCHEMEHRID, ITEMINFO.FOLDERGUID, ITEMINFO.CONTENTTYPEHRID AS PCBTYPE
FROM ALU_ITEMREVISION ITEMREV

LEFT JOIN 
  (SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, FOLDER.HRID AS FOLDERHRID, LIFECYCLEDEF.HRID AS LIFECYCLEDEFINITIONHRID, REVNAMESCHEME.HRID AS REVISIONNAMINGSCHEMEHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID, ITEM.FOLDERGUID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
LEFT JOIN ALU_FOLDER FOLDER ON ITEM.FOLDERGUID = FOLDER.GUID
LEFT JOIN ALU_LIFECYCLEDEFINITION LIFECYCLEDEF ON ITEM.LIFECYCLEDEFINITIONGUID = LIFECYCLEDEF.GUID
LEFT JOIN ALU_REVISIONNAMINGSCHEME REVNAMESCHEME ON ITEM.REVISIONNAMINGSCHEMEGUID = REVNAMESCHEME.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

LEFT JOIN ALU_LIFECYCLESTATE LCS ON ITEMREV.LIFECYCLESTATEGUID = LCS.GUID
LEFT JOIN ALU_ITEMREVISIONLINK IRL ON IRL.CHILDITEMREVISIONGUID = ITEMREV.GUID
WHERE ( (ITEMINFO.CONTENTTYPEHRID = \'altium-pcb-blank\') OR (ITEMINFO.CONTENTTYPEHRID = \'altium-pcb-assembly\') )
ORDER BY ITEMREV.HRID
;
';

  echo date('H:i:s') . " Begin query to read in all Pcb info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $ITEMHRID = odbc_result($resultHandle, "ITEMHRID");
      $GUID = odbc_result($resultHandle, "GUID");
      $ITEMGUID = odbc_result($resultHandle, "ITEMGUID");
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $REVISIONID = $revSep . odbc_result($resultHandle, "REVISIONID");
      $ANCESTORITEMREVISIONGUID = odbc_result($resultHandle, "ANCESTORITEMREVISIONGUID");
      $COMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "COMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $DESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "DESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $LIFECYCLESTATEHRID = odbc_result($resultHandle, "LIFECYCLESTATEHRID");
      $RELEASEDATE = odbc_result($resultHandle, "RELEASEDATE");
      $SHARINGCONTROL = odbc_result($resultHandle, "SHARINGCONTROL");
      $FOLDERHRID = odbc_result($resultHandle, "FOLDERHRID");
      $LIFECYCLEDEFINITIONHRID = odbc_result($resultHandle, "LIFECYCLEDEFINITIONHRID");
      $REVISIONNAMINGSCHEMEHRID = odbc_result($resultHandle, "REVISIONNAMINGSCHEMEHRID");
      $PCBTYPE = odbc_result($resultHandle, "PCBTYPE");
      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");

      /* Lookup the usernames of the person to create and last modify this folder. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      /* Lookup the ANCESTORITEMREVISIONHRID from ANCESTORITEMREVISIONGUID. */
      $ANCESTORITEMREVISIONHRID = "";
      if ($ANCESTORITEMREVISIONGUID != "")
        $ANCESTORITEMREVISIONHRID = $altiumItemRevsByGuid[$ANCESTORITEMREVISIONGUID]["HRID"];

      /* Lookup the full path of the leaf folder we were just given. */
      $PCBPATH = CACF_TraceFolderPath(&$CACFconstants, &$altiumFoldersByGuid, $FOLDERGUID);

      /* Cache the full path to this model for later use when generating the component audit report. */
      $altiumItemRevsByGuid[$GUID]["PCBPATH"] = $PCBPATH;
      //    echo "Cached PCBPATH for GUID $GUID as \"$PCBPATH\".\n";

      /* Create a new line in the array that stores all our data. */
      $altiumPcbDataByItemRevHrid[$HRID] = array();
      
      /* Store all the fields that are needed for writing to csv file, etc. */
      $altiumPcbDataByItemRevHrid[$HRID]["GUID"] = $GUID;
      $altiumPcbDataByItemRevHrid[$HRID]["ITEMGUID"] = $ITEMGUID;
      $altiumPcbDataByItemRevHrid[$HRID]["ITEMHRID"] = $ITEMHRID;
      $altiumPcbDataByItemRevHrid[$HRID]["REVISIONID"] = $REVISIONID;
      $altiumPcbDataByItemRevHrid[$HRID]["ANCESTORITEMREVISIONHRID"] = $ANCESTORITEMREVISIONHRID;
      $altiumPcbDataByItemRevHrid[$HRID]["COMMENT"] = $COMMENT;
      $altiumPcbDataByItemRevHrid[$HRID]["DESCRIPTION"] = $DESCRIPTION;
      $altiumPcbDataByItemRevHrid[$HRID]["PCBTYPE"] = $PCBTYPE;
      $altiumPcbDataByItemRevHrid[$HRID]["PCBPATH"] = $PCBPATH;
      $altiumPcbDataByItemRevHrid[$HRID]["CREATEDBY"] = $CREATEDBY;
      $altiumPcbDataByItemRevHrid[$HRID]["CREATEDAT"] = $CREATEDAT;
      $altiumPcbDataByItemRevHrid[$HRID]["LASTMODIFIEDBY"] = $LASTMODIFIEDBY;
      $altiumPcbDataByItemRevHrid[$HRID]["LASTMODIFIEDAT"] = $LASTMODIFIEDAT;
      $altiumPcbDataByItemRevHrid[$HRID]["LIFECYCLESTATEHRID"] = $LIFECYCLESTATEHRID;
      $altiumPcbDataByItemRevHrid[$HRID]["RELEASEDATE"] = $RELEASEDATE;
      $altiumPcbDataByItemRevHrid[$HRID]["LIFECYCLEDEFINITIONHRID"] = $LIFECYCLEDEFINITIONHRID;
      $altiumPcbDataByItemRevHrid[$HRID]["REVISIONNAMINGSCHEMEHRID"] = $REVISIONNAMINGSCHEMEHRID;
      $altiumPcbDataByItemRevHrid[$HRID]["SHARINGCONTROL"] = $SHARINGCONTROL;

      /* Get all the permission fields that exist for this model. */
      foreach ($altiumAclDataByObjectGuid[$GUID] as $PERMNAME => $PERMVALUE)
        {
          /* Copy permission field name and permission field value to target data structure. */
          $altiumPcbDataByItemRevHrid[$HRID][$PERMNAME] = $PERMVALUE;
          
        } /* end foreach */

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  print_r($altiumPcbDataByItemRevHrid);

} /* end CACF_CreatePcbAuditData() */


/****************************************************************
 * CACF_CreatePcbAuditDataAndWriteToCsv()
 *		Function to create PCB audit data and write to csv file.
 *
 * Outputs:  $altiumItemRevsByGuid, (text written to csv file)
 ****************************************************************/
function CACF_CreatePcbAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                              &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                              &$altiumItemRevsByGuid)
{
//  echo "In CACF_CreatePcbAuditDataAndWriteToCsv(), auditPcbsFileName is \"$auditPcbsFileName\".\n";

  /* Retrieve necessary global constants. */
  $auditPcbsFileName = 								$CACFconstants["auditPcbsFileName"];

  /* Create PCB audit data. */
  CACF_CreatePcbAuditData(&$db, &$CACFconstants,
                          &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                          &$altiumItemRevsByGuid, &$altiumPcbDataByItemRevHrid);

  /* Write audit data to csv file. */
  $auditSpecifiedsFileName = $auditPcbsFileName;
  $auditType = "PCBs";
  $altiumSpecifiedDataByItemRevHrid = &$altiumPcbDataByItemRevHrid;
  CACF_WriteSpecifiedAuditDataToCsv(&$db, &$CACFconstants,
                                    $auditSpecifiedsFileName, &$altiumUserNamesByGuid, &$altiumFoldersByGuid, 
                                    $auditType, &$altiumItemRevsByGuid,
                                    &$altiumSpecifiedDataByItemRevHrid);

} /* end CACF_CreatePcbAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreateAclAuditData()
 *		Function to create Acl audit data.
 *
 * Outputs:  $altiumAclDataByObjectHrid, &$altiumAclDataByObjectGuid
 ****************************************************************/
function CACF_CreateAclAuditData(&$db, &$CACFconstants,
                                 &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumItemsByGuid, &$altiumItemRevsByGuid, &$altiumAclUserPermissions, 
                                 &$altiumAclDataByObjectHrid, &$altiumAclDataByObjectGuid)
{
//  echo "In CACF_CreateAclAuditData().\n";
  
//  echo "altiumItemsByGuid is:\n";
//  print_r($altiumItemsByGuid);

  /* Retrieve necessary global constants. */
  $numPerms = 										$CACFconstants["numPerms"];

  /* Create arrays to hold Acl data. */
  $altiumAclDataByObjectHrid = array();
  $altiumAclDataByObjectGuid = array();

  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  /* Note that we wish do to left joins so that we make sure we examine every line in the ACLENTRY table.
   There may be more types of ACLs that we discover in the future, besides folders, items, and itemRevs. */
  $queryText = '
SELECT ACL.GUID, FOLDER.GUID AS FOLDERGUID, FOLDER.HRID AS FOLDERHRID, FOLDER.DESCRIPTION AS FOLDERDESCRIPTION, ITEMREV.GUID AS ITEMREVGUID, ITEMREV.HRID AS ITEMREVHRID, ITEMREV.COMMENT AS ITEMREVCOMMENT, ITEMREV.DESCRIPTION AS ITEMREVDESCRIPTION, ITEM.GUID AS ITEMGUID, ITEM.HRID AS ITEMHRID, ITEM.DESCRIPTION AS ITEMDESCRIPTION, ACL.OBJECTID AS OBJECTGUID, ACL.CREATEDBYGUID, ACL.LASTMODIFIEDBYGUID, ACL.CREATEDAT, ACL.LASTMODIFIEDAT, ACL.USERSCOPEMODE, ACL.USERSCOPE, ACL.ALLOWPERMISSIONS, ACL.DENYPERMISSIONS 
FROM ALU_ACLENTRY ACL

LEFT JOIN ALU_FOLDER FOLDER ON FOLDER.GUID = ACL.OBJECTID
LEFT JOIN ALU_ITEM ITEM ON ITEM.GUID = ACL.OBJECTID
LEFT JOIN ALU_ITEMREVISION ITEMREV ON ITEMREV.GUID = ACL.OBJECTID

ORDER BY ACL.USERSCOPE
;
';

  echo date('H:i:s') . " Begin query to read in all ACL info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $OBJECTGUID = odbc_result($resultHandle, "OBJECTGUID");
      $ITEMHRID = odbc_result($resultHandle, "ITEMHRID");
      $ITEMGUID = odbc_result($resultHandle, "ITEMGUID");
      $ITEMDESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "ITEMDESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $ITEMREVHRID = odbc_result($resultHandle, "ITEMREVHRID");
      $ITEMREVGUID = odbc_result($resultHandle, "ITEMREVGUID");
      $ITEMREVCOMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "ITEMREVCOMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $ITEMREVDESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "ITEMREVDESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $FOLDERHRID = odbc_result($resultHandle, "FOLDERHRID");
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $FOLDERDESCRIPTION = odbc_result($resultHandle, "FOLDERDESCRIPTION");
      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");

      $USERSCOPEMODE = odbc_result($resultHandle, "USERSCOPEMODE");
      $USERSCOPE = odbc_result($resultHandle, "USERSCOPE");
      $ALLOWPERMISSIONS = odbc_result($resultHandle, "ALLOWPERMISSIONS");
      $DENYPERMISSIONS  = odbc_result($resultHandle, "DENYPERMISSIONS");

      /* Lookup the usernames of the person to create and last modify this ACL. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      //      echo "GUID is $GUID, ITEMGUID is $ITEMGUID, ITEMREVGUID is $ITEMREVGUID, FOLDERGUID is $FOLDERGUID.\n";

      /* Lookup the user(s) and associated permissions. */
      if (isset($altiumAclUserPermissions[$USERSCOPEMODE][$USERSCOPE][$ALLOWPERMISSIONS][$DENYPERMISSIONS]))
        {
          $PERMISSIONS = $altiumAclUserPermissions[$USERSCOPEMODE][$USERSCOPE][$ALLOWPERMISSIONS][$DENYPERMISSIONS];
        }
      else
        {
          $PERMISSIONS = "**UNKNOWN** $USERSCOPEMODE $USERSCOPE $ALLOWPERMISSIONS $DENYPERMISSIONS";
        }


      /** Try to find a valid folder, item, or itemrev pointed to by the ACL entry. **/
      if ($FOLDERHRID != "")
        {
          //          echo "\nFound valid folder!\n";

          $OBJECTHRID = $FOLDERHRID;
          $COMMENT = '';
          $DESCRIPTION = $FOLDERDESCRIPTION;

          /* Invent a content type for a folder. */
          $CONTENTTYPE = 'vault-folder';

          /* Lookup the full path of the leaf folder we were just given. */
          $OBJECTPATH = CACF_TraceFolderPath(&$CACFconstants, &$altiumFoldersByGuid, $FOLDERGUID);

          /* Create sortable key that includes the OBJECTPATH. */
          $key = '01-FOLDER-' . $OBJECTPATH;

        }

      else if ($ITEMHRID != "")
        {
          //          echo "\nFound valid ITEM!\n";
          //          print_r($altiumItemsByGuid[$ITEMGUID]);

          $OBJECTHRID = $ITEMHRID;
          $COMMENT = '';
          $DESCRIPTION = $ITEMDESCRIPTION;

          /* Lookup the FOLDERGUID of this ITEMGUID (previously cached). */
          $FOLDERGUID = $altiumItemsByGuid[$ITEMGUID]["FOLDERGUID"];

          /* Lookup the CONTENTTYPEHRID of this ITEM (previously cached). */
          $CONTENTTYPE = $altiumItemsByGuid[$ITEMGUID]["CONTENTTYPEHRID"];

          /* Lookup the full path of the leaf folder we were just given. */
          $OBJECTPATH = CACF_TraceFolderPath(&$CACFconstants, &$altiumFoldersByGuid, $FOLDERGUID);

          /* Create sortable key that includes the ITEMHRID. */
          $key = '02-CONTENT-' . $CONTENTTYPE . '-' . $ITEMHRID;

        }

      else if ($ITEMREVHRID != "")
        {
          //          echo "\nFound valid ITEMREV!\n";
          //          print_r($altiumItemRevsByGuid[$ITEMREVGUID]);

          $OBJECTHRID = $ITEMREVHRID;
          $COMMENT = $ITEMREVCOMMENT;
          $DESCRIPTION = $ITEMREVDESCRIPTION;

          /* Lookup the corresponding ITEM (previously cached). */
          $ITEMGUID = $altiumItemRevsByGuid[$ITEMREVGUID]["ITEMGUID"];
          //          echo "ITEMGUID is $ITEMGUID.\n";
          //          print_r($altiumItemsByGuid[$ITEMGUID]);

          /* Lookup the FOLDERGUID of this ITEMGUID (previously cached). */
          $FOLDERGUID = $altiumItemsByGuid[$ITEMGUID]["FOLDERGUID"];

          /* Lookup the CONTENTTYPEHRID of this ITEM (previously cached). */
          $CONTENTTYPE = $altiumItemsByGuid[$ITEMGUID]["CONTENTTYPEHRID"];

          /* Lookup the full path of the leaf folder we were just given. */
          $OBJECTPATH = CACF_TraceFolderPath(&$CACFconstants, &$altiumFoldersByGuid, $FOLDERGUID);

          /* Create sortable key that includes the ITEMREVHRID. */
          $key = '02-CONTENT-' . $CONTENTTYPE . '-' . $ITEMREVHRID;

        }

      else
        {
          echo "Error!! Could not find match for GUID $GUID!\n";
        }


      /* See if we need to create a new entry in the array that stores ACL data. */
      if (!isset($altiumAclDataByObjectHrid[$key]))
        {

          /* Create a new line in the array that stores all our data. */
          $altiumAclDataByObjectHrid[$key] = array();
      
          /* Store all the fields that are a function of the object, not the ACL. */
          $altiumAclDataByObjectHrid[$key]["OBJECTGUID"] = $OBJECTGUID;
          $altiumAclDataByObjectHrid[$key]["CONTENTTYPE"] = $CONTENTTYPE;
          $altiumAclDataByObjectHrid[$key]["OBJECTHRID"] = $OBJECTHRID;
          $altiumAclDataByObjectHrid[$key]["OBJECTPATH"] = $OBJECTPATH;
          $altiumAclDataByObjectHrid[$key]["COMMENT"] = $COMMENT;
          $altiumAclDataByObjectHrid[$key]["DESCRIPTION"] = $DESCRIPTION;

          /* Store 0th set of permissions for this object. */
          $altiumAclDataByObjectHrid[$key]["PERMISSIONS0"] = $PERMISSIONS;
          $altiumAclDataByObjectHrid[$key]["CREATEDBY0"] = $CREATEDBY;
          $altiumAclDataByObjectHrid[$key]["CREATEDAT0"] = $CREATEDAT;
          $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDBY0"] = $LASTMODIFIEDBY;
          $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDAT0"] = $LASTMODIFIEDAT;
          
          /* Store 0th permission summary in $altiumAclDataByObjectGuid array. */
          $altiumAclDataByObjectGuid[$OBJECTGUID] = array();
          $altiumAclDataByObjectGuid[$OBJECTGUID]["PERMISSIONS0"] = $PERMISSIONS;

          /* Reserve columns for up to 9 more permissions (eg. write permissions) that may be given 
           to specific folders, items, or itemRevs. */
          for ($i = 1; ($i < $numPerms); $i++)
            {
              /* Store null strings at ith set of permissions for this object. */
              $altiumAclDataByObjectHrid[$key]["PERMISSIONS" . $i] = '';
              $altiumAclDataByObjectHrid[$key]["CREATEDBY" . $i] = '';
              $altiumAclDataByObjectHrid[$key]["CREATEDAT" . $i] = '';
              $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDBY" . $i] = '';
              $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDAT" . $i] = '';

              /* Store null strings at ith set of permissions for this object. */
              $altiumAclDataByObjectGuid[$OBJECTGUID]["PERMISSIONS" . $i] = '';
            } /* endfor */

        } /* endif */

      /* Else we already have an entry for this object.
       Proceed to add 1st, 2nd, etc. permission. */
      else
        {
          /* Flag that we have not yet stored this new permission. */
          $wrotePerm = 0;

          /* Loop over all possible remaining permission slots for this object. */
          for ($i = 1; ( ($i < $numPerms) && (!$wrotePerm) ); $i++)
            {
              /* Construct name of this permission slot. */
              $permSlotName = "PERMISSIONS" . $i;
              //              echo "permSlotName is $permSlotName\n";

              /* See if this slot is empty. */
              if ($altiumAclDataByObjectHrid[$key][$permSlotName] == "")
                {
                  /* Store ith set of permissions for this object. */
                  $altiumAclDataByObjectHrid[$key]["PERMISSIONS" . $i] = $PERMISSIONS;
                  $altiumAclDataByObjectHrid[$key]["CREATEDBY" . $i] = $CREATEDBY;
                  $altiumAclDataByObjectHrid[$key]["CREATEDAT" . $i] = $CREATEDAT;
                  $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDBY" . $i] = $LASTMODIFIEDBY;
                  $altiumAclDataByObjectHrid[$key]["LASTMODIFIEDAT" . $i] = $LASTMODIFIEDAT;

                  /* Store ith permission summary in $altiumAclDataByObjectGuid array. */
                  $altiumAclDataByObjectGuid[$OBJECTGUID]["PERMISSIONS" . $i] = $PERMISSIONS;

                  /* Flag that we've stored this permission. */
                  $wrotePerm = 1;

                } /* endif */

            } /* endfor */

          /* Make sure we succeeded. */
          if (!$wrotePerm)
            {
              echo "ERROR!  Was not able to store this permission in data structure!\n";

            } /* endif */

        } /* endelse */

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Sort data structure by artificial key that we previously invented. */
  $rc = ksort($altiumAclDataByObjectHrid);
  if ($rc == FALSE) my_die("ksort() failed!");

  //  print_r($altiumAclDataByObjectHrid);

} /* end CACF_CreateAclAuditData() */


/****************************************************************
 * CACF_CreateAclAuditDataAndWriteToCsv()
 *		Function to create ACL audit data and write to csv file.
 *
 * Outputs:  &$altiumAclDataByObjectGuid, (text written to csv file)
 ****************************************************************/
function CACF_CreateAclAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                              &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumItemsByGuid, &$altiumItemRevsByGuid, &$altiumAclUserPermissions, 
                                              &$altiumAclDataByObjectGuid)

{
  /* Retrieve necessary global constants. */
  $auditAclsFileName = 								$CACFconstants["auditAclsFileName"];

  echo "In CACF_CreateAclAuditDataAndWriteToCsv(), auditAclsFileName is \"$auditAclsFileName\".\n";

  /* Create ACL audit data. */
  CACF_CreateAclAuditData(&$db, &$CACFconstants,
                          &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumItemsByGuid, &$altiumItemRevsByGuid, &$altiumAclUserPermissions, 
                          &$altiumAclDataByObjectHrid, &$altiumAclDataByObjectGuid);

  /* Write audit data to csv file. */
  $auditSpecifiedsFileName = $auditAclsFileName;
  $auditType = "ACLs";
  $altiumSpecifiedDataByItemRevHrid = &$altiumAclDataByObjectHrid;
  CACF_WriteSpecifiedAuditDataToCsv(&$db, &$CACFconstants,
                                    $auditSpecifiedsFileName, &$altiumUserNamesByGuid, &$altiumFoldersByGuid, 
                                    $auditType, &$altiumItemRevsByGuid,
                                    &$altiumSpecifiedDataByItemRevHrid);

} /* end CACF_CreateAclAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreateModelWhereUsedDataAndWriteToCsv()
 *		Function to create model where-used data and write to csv file.
 *
 * Outputs:  (text written to csv file)
 ****************************************************************/
function CACF_CreateModelWhereUsedDataAndWriteToCsv(&$db, &$CACFconstants,
                                                    &$altiumItemRevsByGuid)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $revSep = 										$CACFconstants["revSep"];
  $auditFileExt = 									$CACFconstants["auditFileExt"];
  $auditObsoleteSuffix = 							$CACFconstants["auditObsoleteSuffix"];
  $auditModelsWhereUsedFileName = 					$CACFconstants["auditModelsWhereUsedFileName"];

  /** Open audit models file for writing. **/
  $auditModelsWhereUsedFile = my_fopen($auditModelsWhereUsedFileName);

  /** Open another version of this file to describe obsolete models. **/
  $auditModelsWhereUsedObsoleteFileName = preg_replace("/".$auditFileExt."/", $auditObsoleteSuffix.$auditFileExt, $auditModelsWhereUsedFileName);
  $auditModelsWhereUsedObsoleteFile = my_fopen($auditModelsWhereUsedObsoleteFileName);

  /** Output column headers. **/
  $line = "";
  $line = $line . "COMBHRID".$ofs."ITEMHRID".$ofs."REVISIONID".$ofs."ANCESTORITEMREVISIONHRID".$ofs."COMMENT".$ofs."DESCRIPTION".$ofs."MODELTYPE".$ofs."COMPHRID".$ofs."COMPCOMMENT".$ofs."COMPDESCRIPTION";

  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
  fputs($auditModelsWhereUsedFile, $line . "\r\n");
  fputs($auditModelsWhereUsedObsoleteFile, $line . "\r\n");

  /** Create array to temporarily hold all lines that we generate, since we need to sort before writing to file. **/
  $auditModelsWhereUsedLines = array();
  $auditModelsWhereUsedObsoleteLines = array();

  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, ITEMREV.HRID AS ITEMREVHRID, ITEMINFO.HRID AS ITEMHRID, ITEMREV.HRID, ITEMREV.REVISIONID, ITEMREV.ANCESTORITEMREVISIONGUID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, IRL.HRID AS MODELTYPE, IRL.PARENTITEMREVISIONGUID
FROM ALU_ITEMREVISION ITEMREV

LEFT JOIN 
  (SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, FOLDER.HRID AS FOLDERHRID, LIFECYCLEDEF.HRID AS LIFECYCLEDEFINITIONHRID, REVNAMESCHEME.HRID AS REVISIONNAMINGSCHEMEHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID, ITEM.FOLDERGUID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
LEFT JOIN ALU_FOLDER FOLDER ON ITEM.FOLDERGUID = FOLDER.GUID
LEFT JOIN ALU_LIFECYCLEDEFINITION LIFECYCLEDEF ON ITEM.LIFECYCLEDEFINITIONGUID = LIFECYCLEDEF.GUID
LEFT JOIN ALU_REVISIONNAMINGSCHEME REVNAMESCHEME ON ITEM.REVISIONNAMINGSCHEMEGUID = REVNAMESCHEME.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

LEFT JOIN ALU_ITEMREVISIONLINK IRL ON IRL.CHILDITEMREVISIONGUID = ITEMREV.GUID
WHERE ( (ITEMINFO.CONTENTTYPEHRID = \'altium-symbol\') OR (ITEMINFO.CONTENTTYPEHRID = \'altium-pcb-component\') )
ORDER BY ITEMREV.HRID
;
';


  echo date('H:i:s') . " Begin query to generate model where-used info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $ITEMHRID = odbc_result($resultHandle, "ITEMHRID");
      $ITEMREVHRID = odbc_result($resultHandle, "ITEMREVHRID");
      $GUID = odbc_result($resultHandle, "GUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $REVISIONID = $revSep . odbc_result($resultHandle, "REVISIONID");
      $ANCESTORITEMREVISIONGUID = odbc_result($resultHandle, "ANCESTORITEMREVISIONGUID");
      $COMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "COMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $DESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "DESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $MODELTYPE = odbc_result($resultHandle, "MODELTYPE");
      $PARENTITEMREVISIONGUID = odbc_result($resultHandle, "PARENTITEMREVISIONGUID");

      /* Lookup the ANCESTORITEMREVISIONHRID from ANCESTORITEMREVISIONGUID. */
      $ANCESTORITEMREVISIONHRID = "";
      if ($ANCESTORITEMREVISIONGUID != "")
        $ANCESTORITEMREVISIONHRID = $altiumItemRevsByGuid[$ANCESTORITEMREVISIONGUID]["HRID"];

      /* Lookup certain fields of the component using this model, using PARENTITEMREVISIONGUID. */
      if ($PARENTITEMREVISIONGUID != "")
        {
          $COMPHRID = $altiumItemRevsByGuid[$PARENTITEMREVISIONGUID]["HRID"];
          $COMPCOMMENT = $altiumItemRevsByGuid[$PARENTITEMREVISIONGUID]["COMMENT"];
          $COMPDESCRIPTION = $altiumItemRevsByGuid[$PARENTITEMREVISIONGUID]["DESCRIPTION"];
        }

      else
        {
          $COMPHRID = "";
          $COMPCOMMENT = "";
          $COMPDESCRIPTION = "";
        }

      /* Create a unique HRID for this audit output by combining the HRID of the model with the 
       HRID of the component that uses it.  This is used:
       1. For giving BCompare a key column when comparing multiple revs of this output file,
       2. As the sort key for sorting all the data in this audit file. */
      $COMBHRID = $ITEMREVHRID.".".$COMPHRID;

      /* Output the actual fields retrieved from SQL query. */
      $line = "";
      $line = $line . "$COMBHRID".$ofs."$ITEMHRID".$ofs."$REVISIONID".$ofs."$ANCESTORITEMREVISIONHRID".$ofs."$COMMENT".$ofs."$DESCRIPTION".$ofs."$MODELTYPE".$ofs."$COMPHRID".$ofs."$COMPCOMMENT".$ofs."$COMPDESCRIPTION";

      /* Figure out if this model is obsolete or not and write this line to the appropriate array. */
      if ($altiumItemRevsByGuid[$GUID]["OBSOLETE"] == 0)
        {
          /* Store this line in an in-memory array, so we can sort it all just before writing to disk. */
          $auditModelsWhereUsedLines[$COMBHRID] = $line;
        }

      else
        {
          /* Store this line in an in-memory array, so we can sort it all just before writing to disk. */
          $auditModelsWhereUsedObsoleteLines[$COMBHRID] = $line;
        }

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Sort all output lines here by concatenation of the model HRID and the component-where-used HRID (key). */
  $rc = ksort($auditModelsWhereUsedLines);
  if ($rc == FALSE) my_die("ksort() failed!");

  $rc = ksort($auditModelsWhereUsedObsoleteLines);
  if ($rc == FALSE) my_die("ksort() failed!");

  /** Write all output to $auditModelsWhereUsedFile **/
  /* Loop over all lines in what will be our output file. */
  foreach ($auditModelsWhereUsedLines as $key => $line)
    {
      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($auditModelsWhereUsedFile, $line . "\r\n");

    } /* endforeach */

  /** Write all output to $auditModelsWhereUsedObsoleteFile **/
  /* Loop over all lines in what will be our output file. */
  foreach ($auditModelsWhereUsedObsoleteLines as $key => $line)
    {
      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($auditModelsWhereUsedObsoleteFile, $line . "\r\n");

    } /* endforeach */

  /** Clear array that held all lines prior to writing to file. **/
  $auditModelsWhereUsedLines = array();
  $auditModelsWhereUsedObsoleteLines = array();

  /** Close the audit output file. **/
  fclose($auditModelsWhereUsedFile);
  fclose($auditModelsWhereUsedObsoleteFile);
  echo date('H:i:s') . " Done writing audit models where-used file \"$auditModelsWhereUsedFileName.\"\n";

} /* end CACF_CreateModelWhereUsedDataAndWriteToCsv() */


/****************************************************************
 * CACF_WriteComponentAuditColumnHeaders()
 *		Function to create component audit data and output to csv file.
 *
 * Outputs:  $line, (text written to $file)
 ****************************************************************/
function CACF_WriteComponentAuditColumnHeaders(&$CACFconstants, &$itemSysParmValues, &$userParmNames, &$line, $doWriteToCsv, &$file)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];

  /* Clear $line variable. */
  $line = "";

  /** Output system parameter field names. **/
  /* Loop over all the standard system parameter names. */
  foreach ($itemSysParmValues as $PARAMETERNAME => $value)
    {
      /* Print out a field separator if needed. */
      if ($line != "")
        $line = $line.$ofs;
              
      /* Strip off leading digits and dash that were only there for sorting and uniqueness purposes. */
      $PARAMETERNAME_stripped = preg_replace('/^[0-9]+-/', '', $PARAMETERNAME);
              
      /* Output $PARAMETERNAME. */
      $line = $line.$PARAMETERNAME_stripped;
          
    } /* end foreach */
      

  /** Output user parameter field names. **/
  /* Output an ordered list of all the Altium user parameters that exist in our universe as columns in the csv file. */
  foreach ($userParmNames as $PARAMETERNAME => $value)
    {
      /* Output $PARAMETERNAME. */
      $line = $line.$ofs.$PARAMETERNAME;
          
    } /* end foreach */
      
  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
  if ($doWriteToCsv)
    fputs($file, $line . "\r\n");

} /* end CACF_WriteComponentAuditColumnHeaders() */


/****************************************************************
 * CACF_CreateComponentAuditSysParmValues()
 *		Function to create a line of text containing system parameter values for a given component.
 *
 * Outputs:  $line
 ****************************************************************/
function CACF_CreateComponentAuditSysParmValues(&$CACFconstants, &$itemSysParmValues, &$line)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];

  /* Clear $line variable. */
  $line = "";

  /** Output columns of Altium system parameter values. **/
  /* Loop over all the standard system parameter names. */
  foreach ($itemSysParmValues as $PARAMETERNAME => $value)
    {
      /* Print out a field separator if needed. */
      if ($line != "")
        $line = $line.$ofs;

      /* Retrieve the value of this parameter. */
      $PARAMETERVALUE = $itemSysParmValues[$PARAMETERNAME];
          
      /* Output the value of this component's system parameter. */
      $line = $line.CACF_RemapReservedChars($PARAMETERVALUE);	/* Remap any ',' chars in cell to '|' chars! */;
      
   } /* end foreach */

} /* end CACF_CreateComponentAuditSysParmValues() */


/****************************************************************
 * CACF_CreateComponentAuditData()
 *		Function to create component audit data.
 *
 * Outputs:  $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid
 ****************************************************************/
function CACF_CreateComponentAuditData(&$db, &$CACFconstants,
                                       &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, &$altiumItemRevsByGuid, 
                                       &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid)
{

  /* Retrieve necessary global constants. */
  $revSep = 										$CACFconstants["revSep"];
  $maxPcbLibModels = 								$CACFconstants["maxPcbLibModels"];

  /* Initialize array that will hold all system parameters for each item. */
  $altiumItemSysParmValuesByGuid = array();

  /* Initialize array that will record which components are obsolete. */
  $altiumObsoleteCompsByGuid = array();


  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  $queryText = '
SELECT ITEMREV.GUID, ITEMINFO.HRID AS ITEMHRID, ITEMREV.HRID, ITEMREV.CREATEDBYGUID, ITEMREV.LASTMODIFIEDBYGUID, ITEMREV.CREATEDAT, ITEMREV.LASTMODIFIEDAT, ITEMREV.REVISIONID, ITEMREV.ANCESTORITEMREVISIONGUID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, LCS.HRID AS LIFECYCLESTATEHRID, ITEMREV.RELEASEDATE, ITEMINFO.SHARINGCONTROL, ITEMINFO.FOLDERHRID, ITEMINFO.LIFECYCLEDEFINITIONHRID, ITEMINFO.REVISIONNAMINGSCHEMEHRID, ITEMINFO.FOLDERGUID, IRL_SCHLIB.HRID AS SCHLIB_MODELTYPE, IRL_SCHLIB.MODELLIB AS SCHLIB_MODELLIB, IRL_SCHLIB.OWNERGUID AS SCHLIB_OWNERGUID, IRL_SCHLIB.DATAFOLDERGUID';

  /* Add SQL query text related to each of the allowed PCBLIB models. */
  for ($i = 0; $i < $maxPcbLibModels; $i++)
    {
      $queryText = $queryText . ', IRL_PCBLIB'.$i.'.HRID AS PCBLIB'.$i.'_MODELTYPE, IRL_PCBLIB'.$i.'.MODELLIB AS PCBLIB'.$i.'_MODELLIB, IRL_PCBLIB'.$i.'.OWNERGUID AS PCBLIB'.$i.'_OWNERGUID, IRL_PCBLIB'.$i.'.DATAFOLDERGUID';

    } /* endfor */

  /* Add in more invariant SQL text. */
  $queryText = $queryText . '
FROM ALU_ITEMREVISION ITEMREV

LEFT JOIN 
  (SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, FOLDER.HRID AS FOLDERHRID, LIFECYCLEDEF.HRID AS LIFECYCLEDEFINITIONHRID, REVNAMESCHEME.HRID AS REVISIONNAMINGSCHEMEHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID, ITEM.FOLDERGUID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
LEFT JOIN ALU_FOLDER FOLDER ON ITEM.FOLDERGUID = FOLDER.GUID
LEFT JOIN ALU_LIFECYCLEDEFINITION LIFECYCLEDEF ON ITEM.LIFECYCLEDEFINITIONGUID = LIFECYCLEDEF.GUID
LEFT JOIN ALU_REVISIONNAMINGSCHEME REVNAMESCHEME ON ITEM.REVISIONNAMINGSCHEMEGUID = REVNAMESCHEME.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

LEFT JOIN 
  (SELECT ITEMREVLINK.GUID, ITEMREVLINK.PARENTITEMREVISIONGUID, ITEMREVLINK.CHILDITEMREVISIONGUID, ITEMREVLINK.HRID AS HRID, DATAFILE.HRID AS MODELLIB, DATAFILE.DATAFOLDERGUID, DATAFILE.OWNERGUID
FROM ALU_ITEMREVISIONLINK ITEMREVLINK
LEFT JOIN ALU_DATAFILE DATAFILE ON ITEMREVLINK.CHILDITEMREVISIONGUID = DATAFILE.OWNERGUID
WHERE ITEMREVLINK.HRID = \'SCHLIB\'
) IRL_SCHLIB ON ITEMREV.GUID = IRL_SCHLIB.PARENTITEMREVISIONGUID
';

  /* Add SQL query text (left join) related to each of the allowed PCBLIB models. */
  for ($i = 0; $i < $maxPcbLibModels; $i++)
    {
      /* The 0th PCBLIB doesn't need a suffix.  The rest do. */
      if ($i == 0)
        $suffix = "";
      else
        $suffix = " $i";

      $queryText = $queryText . '
LEFT JOIN 
  (SELECT ITEMREVLINK.GUID, ITEMREVLINK.PARENTITEMREVISIONGUID, ITEMREVLINK.CHILDITEMREVISIONGUID, ITEMREVLINK.HRID AS HRID, DATAFILE.HRID AS MODELLIB, DATAFILE.DATAFOLDERGUID, DATAFILE.OWNERGUID
FROM ALU_ITEMREVISIONLINK ITEMREVLINK
LEFT JOIN ALU_DATAFILE DATAFILE ON ITEMREVLINK.CHILDITEMREVISIONGUID = DATAFILE.OWNERGUID
WHERE ITEMREVLINK.HRID = \'PCBLIB'.$suffix.'\'
) IRL_PCBLIB'.$i.' ON ITEMREV.GUID = IRL_PCBLIB'.$i.'.PARENTITEMREVISIONGUID
';
    } /* endfor */

  /* Add in final invariant SQL text. */
  $queryText = $queryText . '
LEFT JOIN ALU_LIFECYCLESTATE LCS ON ITEMREV.LIFECYCLESTATEGUID = LCS.GUID
WHERE ITEMINFO.CONTENTTYPEHRID = \'altium-component\'
ORDER BY ITEMREV.HRID
;
';

  //  echo "queryText is: \n";
  //  echo $queryText;
  //  echo "\n";

  echo date('H:i:s') . " Begin query to read in all component info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $ITEMHRID = odbc_result($resultHandle, "ITEMHRID");
      $GUID = odbc_result($resultHandle, "GUID");
      $DATAFOLDERGUID = odbc_result($resultHandle, "DATAFOLDERGUID");
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $HRID = odbc_result($resultHandle, "HRID");
      $REVISIONID = $revSep . odbc_result($resultHandle, "REVISIONID");
      $ANCESTORITEMREVISIONGUID = odbc_result($resultHandle, "ANCESTORITEMREVISIONGUID");
      $COMMENT = odbc_result($resultHandle, "COMMENT");			/* Save re-mapping ',' chars in COMMENT field to '|' until output step! */
      $DESCRIPTION = odbc_result($resultHandle, "DESCRIPTION");	/* Save re-mapping ',' chars in DESCRIPTION field to '|' until output step! */
      $LIFECYCLESTATEHRID = odbc_result($resultHandle, "LIFECYCLESTATEHRID");
      $RELEASEDATE = odbc_result($resultHandle, "RELEASEDATE");
      $SHARINGCONTROL = odbc_result($resultHandle, "SHARINGCONTROL");
      $FOLDERHRID = odbc_result($resultHandle, "FOLDERHRID");
      $LIFECYCLEDEFINITIONHRID = odbc_result($resultHandle, "LIFECYCLEDEFINITIONHRID");
      $REVISIONNAMINGSCHEMEHRID = odbc_result($resultHandle, "REVISIONNAMINGSCHEMEHRID");
      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");
      $SCHLIB_OWNERGUID = odbc_result($resultHandle, "SCHLIB_OWNERGUID");
      $SCHLIB_MODELTYPE = odbc_result($resultHandle, "SCHLIB_MODELTYPE");
      $SCHLIB_MODELLIB = CACF_RemapReservedChars(odbc_result($resultHandle, "SCHLIB_MODELLIB"));	/* Re-map ',' chars in $SCHLIB_MODELLIB field to '|'! */

      /* Lookup the usernames of the person to create and last modify this folder. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      /* Handle ancestor item revisions. */
      $ANCESTORITEMREVISIONHRID = "";
      if ($ANCESTORITEMREVISIONGUID != "")
        {
          /* Lookup the ANCESTORITEMREVISIONHRID from ANCESTORITEMREVISIONGUID. */
          $ANCESTORITEMREVISIONHRID = $altiumItemRevsByGuid[$ANCESTORITEMREVISIONGUID]["HRID"];

          /* Mark ancestor ItemRev as obsolete. */
          $altiumObsoleteCompsByGuid[$ANCESTORITEMREVISIONGUID] = 1;

        } /* endif */

      /* Lookup the full path of the leaf folder we were just given. */
      $COMPONENTPATH = CACF_TraceFolderPath($CACFconstants, $altiumFoldersByGuid, $FOLDERGUID);

      /* Create entry in array that will hold all system parameters for each item, for this GUID. */
      $altiumItemSysParmValuesByGuid[$GUID] = array();

      /* Add all defined system parameters to the above-mentioned array. */
      /* Note:  Call CACF_AlterAltiumSysParmName() to add leading digits and dash (eg. "09-") to each parameter name.
       These will only exist for purposes of sorting and making entries unique.  They will be stripped off before writing to csv file! */
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("ITEMHRID")] = $ITEMHRID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("REVISIONID")] = $REVISIONID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("ANCESTORITEMREVISIONHRID")] = $ANCESTORITEMREVISIONHRID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("COMMENT")] = $COMMENT;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("DESCRIPTION")] = $DESCRIPTION;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("COMPONENTPATH")] = $COMPONENTPATH;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("CREATEDBY")] = $CREATEDBY;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("CREATEDAT")] = $CREATEDAT;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LASTMODIFIEDBY")] = $LASTMODIFIEDBY;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LASTMODIFIEDAT")] = $LASTMODIFIEDAT;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LIFECYCLESTATEHRID")] = $LIFECYCLESTATEHRID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("RELEASEDATE")] = $RELEASEDATE;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LIFECYCLEDEFINITIONHRID")] = $LIFECYCLEDEFINITIONHRID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("REVISIONNAMINGSCHEMEHRID")] = $REVISIONNAMINGSCHEMEHRID;
      $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("SHARINGCONTROL")] = $SHARINGCONTROL;

      /* Get all the permission fields that exist for this folder. */
      foreach ($altiumAclDataByObjectGuid[$GUID] as $PERMNAME => $PERMVALUE)
        {
          /* Store permission name / permission value. */
          $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName($PERMNAME)] = $PERMVALUE;
          
        } /* end foreach */

      /* Reserve for all models for which we allocate space in our csv file. */
      /* Those models actually defined in this component will override the placeholders shortly. */
      $sysParms = &$altiumItemSysParmValuesByGuid[$GUID];
      CACF_ReserveForModelsInSysParms(&$CACFconstants, 
                                      &$sysParms);


      /** Handle SCHLIB model. **/
      /* Lookup the HRID of the model. */
      $SCHLIB_MODELHRID = $altiumItemRevsByGuid[$SCHLIB_OWNERGUID]["HRID"];

      /* Lookup the datafile (model) path. */
      $SCHLIB_MODELPATH = $altiumItemRevsByGuid[$SCHLIB_OWNERGUID]["MODELPATH"];

      /* Add information describing the 1 model (SCHLIB) that we currently have to system parameters array. */
      $sysParms 		= &$altiumItemSysParmValuesByGuid[$GUID];
      $modelTypeWithNum = $SCHLIB_MODELTYPE;
      $modelHRID 		= $SCHLIB_MODELHRID;
      $modelPath 		= $SCHLIB_MODELPATH;
      $modelLib 		= $SCHLIB_MODELLIB;
      CACF_AddModelInfoToSysParms(&$CACFconstants, 
                                  &$sysParms, 
                                  $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);


      /** Loop over all possible PCBLIB models. **/
      for ($i = 0; $i < $maxPcbLibModels; $i++)
        {
          /* Retrieve results of SQL query for this PCBLIB model. */
          $PCBLIB_OWNERGUID = odbc_result($resultHandle, "PCBLIB$i"."_OWNERGUID");
          $PCBLIB_MODELTYPE = odbc_result($resultHandle, "PCBLIB$i"."_MODELTYPE");
          $PCBLIB_MODELLIB = CACF_RemapReservedChars(odbc_result($resultHandle, "PCBLIB$i"."_MODELLIB"));	/* Re-map ',' chars in $PCBLIB$i_MODELLIB field to '|'! */
          
          /* See if this model was defined. */
          if ($PCBLIB_OWNERGUID != "")
            {
              /* Lookup the HRID of the model. */
              $PCBLIB_MODELHRID = $altiumItemRevsByGuid[$PCBLIB_OWNERGUID]["HRID"];

              /* Lookup the datafile (model) path. */
              $PCBLIB_MODELPATH = $altiumItemRevsByGuid[$PCBLIB_OWNERGUID]["MODELPATH"];

              /* Add information describing this model (PCBLIB$i) to system parameters array. */
              $sysParms 		= &$altiumItemSysParmValuesByGuid[$GUID];
              $modelTypeWithNum = $PCBLIB_MODELTYPE;
              $modelHRID 		= $PCBLIB_MODELHRID;
              $modelPath 		= $PCBLIB_MODELPATH;
              $modelLib 		= $PCBLIB_MODELLIB;
              CACF_AddModelInfoToSysParms(&$CACFconstants, 
                                          &$sysParms, 
                                          $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);

            } /* endif */

        } /* endfor */

      /* Sort the list of system parms used by this component. */
      $rc = ksort($altiumItemSysParmValuesByGuid[$GUID]);
      if ($rc == FALSE) my_die("ksort() failed!");

    } /* endwhile */

  /* TODO:  For now, $altiumItemSysParmValuesByGuid ends up sorted by ITEMHRID by virtue of the ordering of the MySQL query.
   If that should change or we make changes to $altiumItemSysParmValuesByGuid, we would need to re-sort by ITEMHRID! */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

} /* end CACF_CreateComponentAuditData() */


/****************************************************************
 * CACF_WriteComponentAuditDataToCsv()
 *		Function to create component audit data and output to csv file.
 *
 * Outputs:  $altiumUserParmNamesByCompType, $altiumParmsByCompType
 ****************************************************************/
function CACF_WriteComponentAuditDataToCsv(&$CACFconstants,
                                           &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                           &$altiumUserParmNames, &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid, 
                                           &$altiumUserParmNamesByCompType, &$altiumParmsByCompType)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $auditFileExt = 									$CACFconstants["auditFileExt"];
  $auditObsoleteSuffix = 							$CACFconstants["auditObsoleteSuffix"];
  $auditComponentsFileName = 						$CACFconstants["auditComponentsFileName"];


  /** Initialize arrays describing which user parameters actually exist for each defined component type. **/
  foreach ($auditComponentsByType as $compType => $value)
    {
      //    echo "Setting up to audit component type \"".$compType."\"\r\n";

      /* Setup array to hold the names of all user parameters used in this component type. */
      $altiumUserParmNamesByCompType[$compType] = array();

      /* Setup array to hold the values of all user parameters used in this component type. */
      $altiumParmsByCompType[$compType] = array();

      /* Reserve some user parameter names that should exist (but may not just yet) in all component types. */
      CACF_ReservePerCompTypeUserParmNames(&$altiumUserParmNamesByCompType[$compType], $compType);
    
    } /* end foreach */


  /** Determine if we're even supposed to write to the csv file or if we're only doing this for the data arrays written by this function. **/
  $doWriteToCsv = ($auditComponentsFileName != "");

  /** Open audit components file for writing. **/
  if ($doWriteToCsv)
    {
      $auditComponentsFile = my_fopen($auditComponentsFileName);

      /** Open another version of this file to describe obsolete components. **/
      $auditComponentsObsoleteFileName = preg_replace("/".$auditFileExt."/", $auditObsoleteSuffix.$auditFileExt, $auditComponentsFileName);
      $auditComponentsObsoleteFile = my_fopen($auditComponentsObsoleteFileName);

    } /* endif */

  /* Flag that we need to output column headers. */
  $doColumnHeaders = 1;

  /* Clear line of text bound for csv file. */
  $line = "";

  /** Loop over all known components. **/
  foreach ($altiumItemSysParmValuesByGuid as $GUID => $value)
    {

      /** Output column headers if needed. **/
      /* See if we need to output column headers. */
      if ($doColumnHeaders == 1)
        {
          /* Flag that we are done with column headers. */
          $doColumnHeaders = 0;
          
          /* Call CACF_WriteComponentAuditColumnHeaders() to do all the real work. */
          CACF_WriteComponentAuditColumnHeaders(&$CACFconstants, $altiumItemSysParmValuesByGuid[$GUID], $altiumUserParmNames, $line, $doWriteToCsv, $auditComponentsFile);
          CACF_WriteComponentAuditColumnHeaders(&$CACFconstants, $altiumItemSysParmValuesByGuid[$GUID], $altiumUserParmNames, $line, $doWriteToCsv, $auditComponentsObsoleteFile);

        } /* endif need to output column headers. */


      /** Output columns of Altium system parameter values. **/
      CACF_CreateComponentAuditSysParmValues(&$CACFconstants, &$altiumItemSysParmValuesByGuid[$GUID], &$line);


      /** Output columns of Altium user parameter values. **/
      /* Flag that we have not yet matched this component to a defined type of component. */
      $matches = 0;

      /* Extract ItemHRID. */
      $ITEMHRID = $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("ITEMHRID")];

      /* Create ItemRevHrid. */
      $REVISIONID = $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("REVISIONID")];
      $ItemRevHrid = $ITEMHRID.$REVISIONID;

      /* Extract whether this component (as an ItemRev) is obsolete or not. */
      $isObsolete = isset($altiumObsoleteCompsByGuid[$GUID]);

      //      echo "ItemRevGuid is $GUID, ItemRevHrid is \"$ItemRevHrid\", isObsolete is $isObsolete.\n";
      
      /* Output an ordered list of all the Altium user parameters that exist in our universe as columns in the csv file.
       If this part has a given parameter, list its value. */

      /* Loop over all the defined Altium user parameter names. */
      foreach ($altiumUserParmNames as $PARAMETERNAME => $value)
        {
          /* Unconditionally print out a field separator. */
          $line = $line.$ofs;

          /* See if this component has a stored item user parameter named $PARAMETERNAME. */
          if (isset($altiumItemUserParmValuesByGuid[$GUID][$PARAMETERNAME]))
            {
              /* Retrieve the value of this parameter. */
              $PARAMETERVALUE = $altiumItemUserParmValuesByGuid[$GUID][$PARAMETERNAME];

              /* Output the value of this component's user parameter. */
              $line = $line.CACF_RemapReservedChars($PARAMETERVALUE);	/* Remap any ',' chars in cell to '|' chars! */;

              /** Also store the user parameters in arrays describing the various component types. **/
              /* Loop over all the component types that we are asked to report on. */
              /* TODO:  We should check for matching component types once per component, not once per user parameter.
               Consider moving this out of the outer foreach loop and creating a local array describing which
               component types match this component.  Be sure to continue supporting multiple matches per component. */
              foreach ($auditComponentsByType as $compType => $mask)
                {
                  /* See if this component's HRID is a regex match to the regex mask for this component type. */
                  /* It must also be a real component type, not the "unmatched" catchall.
                   If it is the "unmatched" catchall, we must have had no matches for defined component types. */
                  /* Note:  Here we assume/require that the "unmatched" catchall is the last defined component type! */
                  if (  (preg_match($mask, $ITEMHRID) > 0) &&
                        ( ($compType != $auditComponentsByTypeUnmatched) || 
                          ($matches == 0) )  )
                    {
                      //                      echo "compType is \"".$compType."\", matches is ".$matches."\n";
                      //                      echo "HRID is \"".$ITEMHRID."\", setting up component type \"".$compType."\" to have user parm \"".$PARAMETERNAME."\"\r\n";

                      /* Flag that we have matched this component to a defined type of component. */
                      if ($compType != $auditComponentsByTypeUnmatched)
                        $matches++;

                      /* Store this Altium user parameter name in array. */
                      $altiumUserParmNamesByCompType[$compType][$PARAMETERNAME] = 1;

                      /** Store this Altium user parameter in the array for the appropriate component type. **/
                      /* If needed, initialize array for this component type and HRID. */
                      if (!isset($altiumParmsByCompType[$compType][$ItemRevHrid]))
                        {
                          /* Initialize array for this HRID. */
                          $altiumParmsByCompType[$compType][$ItemRevHrid] = array();

                          /* Write the GUID for this component. */
                          $altiumParmsByCompType[$compType][$ItemRevHrid]["FooBarBinBatGUID"] = $GUID;

                        } /* endif */

                      /* Populate user parameter name=value pair. */
                      $altiumParmsByCompType[$compType][$ItemRevHrid][$PARAMETERNAME] = $PARAMETERVALUE;

                    } /* endif */

                } /* end foreach */

            } /* endif */

        } /* end foreach */

	  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      if ($doWriteToCsv)
        {
          /* If this is not an obsolete component, then write it to the normal component audit file. */
          if (!$isObsolete)
            fputs($auditComponentsFile, $line . "\r\n");

          /* Else write it to the obsolete component audit file. */
          else
            fputs($auditComponentsObsoleteFile, $line . "\r\n");

        } /* endif */

    } /* endwhile */

  /** Close the audit output file. **/
  if ($doWriteToCsv)
    {
      fclose($auditComponentsFile);
      fclose($auditComponentsObsoleteFile);
      echo date('H:i:s') . " Done writing audit components file \"$auditComponentsFileName.\"\n";
    }

} /* end CACF_WriteComponentAuditDataToCsv() */


/****************************************************************
 * CACF_CreatePerComponentTypeAuditDataAndWriteToCsv()
 *		Function to create per-component-type audit data and output to csv file.
 ****************************************************************/
function CACF_CreatePerComponentTypeAuditDataAndWriteToCsv(&$CACFconstants,
                                                           &$altiumParmsByComponentLines,
                                                           $compType, &$altiumUserParmNamesForThisCompType, &$altiumParmsForThisCompType, &$altiumItemSysParmValuesByGuid, &$altiumItemUserParmValuesByGuid, &$altiumObsoleteCompsByGuid)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $revSep = 										$CACFconstants["revSep"];
  $auditFileExt = 									$CACFconstants["auditFileExt"];
  $auditObsoleteSuffix = 							$CACFconstants["auditObsoleteSuffix"];
  $doKeepZeroLengthOutputFiles = 					$CACFconstants["doKeepZeroLengthOutputFiles"];
  $auditComponentsByTypeFileName = 					$CACFconstants["auditComponentsByTypeFileName"];


  /** Open audit components-by-type file for writing. **/
  /* Construct filename. */
  $auditComponentsByTypeThisFileName = preg_replace('/FOOBAR/', $compType, $auditComponentsByTypeFileName);

  echo date('H:i:s') . "  Begin creating audit per-component-type csv file \"".$auditComponentsByTypeThisFileName."\"...\n";

  /* Open file for writing. */
  $doWriteToCsv = 1;
  $auditComponentsByTypeFile = my_fopen($auditComponentsByTypeThisFileName);

  /* Open another version of this file to describe obsolete components. */
  $auditComponentsByTypeObsoleteFileName = preg_replace("/".$auditFileExt."/", $auditObsoleteSuffix.$auditFileExt, $auditComponentsByTypeThisFileName);
  $auditComponentsByTypeObsoleteFile = my_fopen($auditComponentsByTypeObsoleteFileName);

  /* Sort the list of user parms used by this component type. */
  $rc = ksort($altiumUserParmNamesForThisCompType);
  if ($rc == FALSE) my_die("ksort() failed!");


  /** Flag that we need to output column headers for this component type. **/
  $doColumnHeaders = 1;

  /* Loop over all the components belonging to this component type. */
  foreach ($altiumParmsForThisCompType as $ItemRevHrid => $val2)
    {
      //        echo "ItemRevHrid is ".$ItemRevHrid." val2 is ".$val2."\n";

      /* Retrieve the GUID for this component. */
      $GUID = $altiumParmsForThisCompType[$ItemRevHrid]["FooBarBinBatGUID"];

      /* Extract ItemHRID for this component. */
      $ITEMHRID = $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("ITEMHRID")];

      /* Extract RevisionId for this component. */
      $REVISIONID = $altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("REVISIONID")];

      /* Strip off the revSep separator to yield a RevisionKey. */
      $RevisionKey = preg_replace("/^$revSep/", "", $REVISIONID);

      /* Extract whether this component (as an ItemRev) is obsolete or not. */
      $isObsolete = isset($altiumObsoleteCompsByGuid[$GUID]);

      $ItemRevHrid = $ITEMHRID.$REVISIONID;
      //      echo "In CACF_CreatePerComponentTypeAuditDataAndWriteToCsv(), ItemRevGuid is $GUID, ItemRevHrid is \"$ItemRevHrid\", isObsolete is $isObsolete.\n";

      /** Output column headers if needed. **/
      /* See if we need to output column headers. */
      if ($doColumnHeaders == 1)
        {
          /* Flag that we are done with column headers. */
          $doColumnHeaders = 0;
          $colHeaderLine = "";
          
          /* Call CACF_WriteComponentAuditColumnHeaders() to do all the real work. */
          CACF_WriteComponentAuditColumnHeaders(&$CACFconstants, $altiumItemSysParmValuesByGuid[$GUID], $altiumUserParmNamesForThisCompType, $colHeaderLine, $doWriteToCsv, $auditComponentsByTypeFile);
          CACF_WriteComponentAuditColumnHeaders(&$CACFconstants, $altiumItemSysParmValuesByGuid[$GUID], $altiumUserParmNamesForThisCompType, $colHeaderLine, $doWriteToCsv, $auditComponentsByTypeObsoleteFile);

        } /* endif need to output column headers. */

      /* See if the per-component audit info needs the column header line. */
      if (!isset($altiumParmsByComponentLines[$ITEMHRID]))
        {

          /* Initialize array for this HRID. */
          $altiumParmsByComponentLines[$ITEMHRID] = array();

          /* Store filename for this HRID. */
          $altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatFilename"] = preg_replace('/FOOBAR/', ($compType."_".$ITEMHRID), $auditComponentsByTypeFileName);

          /* Store column headers for this HRID. */
          $altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatColHeaders"] = $colHeaderLine;

        } /* endif */

      
      /** Create columns of Altium system parameter values; return in $line variable. **/
      CACF_CreateComponentAuditSysParmValues(&$CACFconstants, &$altiumItemSysParmValuesByGuid[$GUID], &$line);

      /** Create columns of Altium user parameter values. **/
      /* Loop over all the defined Altium user parameter names for this component type. */
      foreach ($altiumUserParmNamesForThisCompType as $PARAMETERNAME => $value)
        {
          /* Unconditionally print out a field separator. */
          $line = $line.$ofs;
            
          /* See if this component has a stored item user parameter named $PARAMETERNAME. */
          if (isset($altiumItemUserParmValuesByGuid[$GUID][$PARAMETERNAME]))
            {

              /* Retrieve the value of this parameter. */
              $PARAMETERVALUE = $altiumItemUserParmValuesByGuid[$GUID][$PARAMETERNAME];
                
              /* Output the value of this component's user parameter. */
              $line = $line.CACF_RemapReservedChars($PARAMETERVALUE);	/* Remap any ',' chars in cell to '|' chars! */;

            } /* endif */

        } /* end foreach user parameters */

      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      /* If this is not an obsolete component, then write it to the normal component audit file. */
      if (!$isObsolete)
        fputs($auditComponentsByTypeFile, $line . "\r\n");
      
      /* Else write it to the obsolete component audit file. */
      else
        fputs($auditComponentsByTypeObsoleteFile, $line . "\r\n");

      /* Store line also for use in per-component audit file. */
      //      echo "In CACF_CreatePerComponentTypeAuditDataAndWriteToCsv(), stored line for ITEMHRID \"$ITEMHRID\" REVISIONID \"$REVISIONID\".\n";
      $altiumParmsByComponentLines[$ITEMHRID][$RevisionKey] = $line;

    } /* end foreach components of this component type */

  /* Close audit components-by-type file. */
  fclose($auditComponentsByTypeFile);
  fclose($auditComponentsByTypeObsoleteFile);
  echo date('H:i:s') . "  Done writing audit per-component-type file \"".$auditComponentsByTypeThisFileName."\"...\n";

  /* See if we have been instructed to delete any 0 length output files which result. */
  if (!$doKeepZeroLengthOutputFiles)
    {
      /* Attempt to delete the output file if needed. */
      if (filesize($auditComponentsByTypeThisFileName) == 0)
        unlink($auditComponentsByTypeThisFileName);

      /* Attempt to delete the output file if needed. */
      if (filesize($auditComponentsByTypeObsoleteFileName) == 0)
        unlink($auditComponentsByTypeObsoleteFileName);

    } /* endif */

} /* end CACF_CreatePerComponentTypeAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreateAllComponentTypeAuditDataAndWriteToCsv()
 *		Function to create all per-component-type audit data and output to csv files.
 ****************************************************************/
function CACF_CreateAllComponentTypeAuditDataAndWriteToCsv(&$CACFconstants,
                                                           &$altiumParmsByComponentLines,
                                                           &$altiumUserParmNamesByCompType, &$altiumParmsByCompType, &$altiumItemSysParmValuesByGuid, &$altiumItemUserParmValuesByGuid, &$altiumObsoleteCompsByGuid)
{
  echo date('H:i:s') . " Begin creating per-component-type audit csv files...\n";

  //  echo "altiumParmsByCompType is:\n";
  //  print_r($altiumParmsByCompType);

  /* Loop over all the defined component types. */
  foreach ($altiumParmsByCompType as $compType => $value)
    {
      /* Call CACF_CreatePerComponentTypeAuditDataAndWriteToCsv() to do all the real work for this component type. */
      CACF_CreatePerComponentTypeAuditDataAndWriteToCsv(&$CACFconstants,
                                                        &$altiumParmsByComponentLines,
                                                        $compType, &$altiumUserParmNamesByCompType[$compType], &$altiumParmsByCompType[$compType], &$altiumItemSysParmValuesByGuid, &$altiumItemUserParmValuesByGuid, &$altiumObsoleteCompsByGuid);

    } /* end foreach component-types */

} /* end CACF_CreateAllComponentTypeAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreatePerComponentAuditDataAndWriteToCsv()
 *		Function to create per-component audit data and output to csv files.
 ****************************************************************/
function CACF_CreatePerComponentAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                                       &$altiumParmsByComponentLines)
{

  echo date('H:i:s') . " Begin creating per-component audit csv files...\n";

  /* Sort array holding per-component audit information, by component HRID. */
  $rc = ksort($altiumParmsByComponentLines);
  if ($rc == FALSE) my_die("ksort() failed!");

  /** Create per-component audit files. **/
  /* Loop over all the individual components. */
  foreach ($altiumParmsByComponentLines as $ITEMHRID => $value)
    {
      /* Retrieve filename for this particular component's per-component audit file. */
      $auditComponentsByComponentFileName = $altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatFilename"];

      /* Retrieve column headers for this particular component's per-component audit file. */
      $auditComponentsByComponentColumnHeaders = $altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatColHeaders"];

      //  echo date('H:i:s') . "  Begin creating per-component audit csv files for ITEMHRID $ITEMHRID, filename is \"$auditComponentsByComponentFileName\"...\n";

      /* Open a new file for this component. */
      $auditComponentsByComponentFile = my_fopen($auditComponentsByComponentFileName);

      /* Write column headers to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($auditComponentsByComponentFile, $auditComponentsByComponentColumnHeaders . "\r\n");

      /* Remove the previous filename and column header entries from array. */
      unset($altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatFilename"]);
      unset($altiumParmsByComponentLines[$ITEMHRID]["FooBarBinBatColHeaders"]);

      /* Sort the revisions of this component. */
      $rc = ksort($altiumParmsByComponentLines[$ITEMHRID]);
      if ($rc == FALSE) my_die("ksort() failed!");

      /* Loop over all revisions of this component. */
      foreach ($altiumParmsByComponentLines[$ITEMHRID] as $RevisionKey => $line)
        {

          //          echo "In CACF_CreatePerComponentAuditDataAndWriteToCsv(), ITEMHRID is \"$ITEMHRID\", RevisionKey is \"$RevisionKey\".\n";

          /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
          fputs($auditComponentsByComponentFile, $line . "\r\n");

        } /* endforeach */

      /* Close audit components-by-component file. */
      fclose($auditComponentsByComponentFile);

    } /* endforeach */

  echo date('H:i:s') . " Done writing per-component audit files...\n";

} /* end CACF_CreatePerComponentAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreateFolderAuditDataAndWriteToCsv()
 *		Function to create folder audit data and output to csv file.
 ****************************************************************/
function CACF_CreateFolderAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                                 &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumFolderUserParmValuesByGuid, &$altiumUserParmNames, &$altiumAclDataByObjectGuid)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $constNamingScheme = 								$CACFconstants["constNamingScheme"];
  $constAbsent = 									$CACFconstants["constAbsent"];
  $auditFoldersFileName = 							$CACFconstants["auditFoldersFileName"];

  /** Modify $altiumUserParmNames to add one FOLDER level special parameter string. **/
  /* WARNING:  Do not move this code block above the audit components operation located above here! */
  $altiumUserParmNames[$constNamingScheme] = 1;

  /* Re-sort all Altium user parameter names stored in the array, by array key. */
  $rc = ksort($altiumUserParmNames);
  if ($rc == FALSE) my_die("ksort() failed!");

  /** Open audit folders file for writing. **/
  $auditFoldersFile = my_fopen($auditFoldersFileName);

  /** Output column headers. **/
  $line = "";
  $line = $line . "FOLDERGUID".$ofs."DESCRIPTION".$ofs."FOLDERTYPE".$ofs."FOLDERPATH".$ofs."CREATEDBY".$ofs."CREATEDAT".$ofs."LASTMODIFIEDBY".$ofs."LASTMODIFIEDAT";

  /* Get a random array slice to know what fields exist. */
  $slice = array_slice($altiumAclDataByObjectGuid, 0, 1);
  //echo "slice is:\n";
  //print_r($slice);

  /* Output an ordered list of all the permission fields that exist. */
  foreach ($slice as $key => $value)
    {
      foreach ($slice[$key] as $PERMNAME => $value2)
        {
          /* Output $PERMNAME. */
          $line = $line.$ofs.$PERMNAME;
          
        } /* end foreach */

    } /* end foreach */

  /* Output an ordered list of all the Altium user parameters that exist in our universe as columns in the csv file. */
  foreach ($altiumUserParmNames as $PARAMETERNAME => $value)
    {
      /* Output $PARAMETERNAME. */
      $line = $line.$ofs.$PARAMETERNAME;

    } /* end foreach */
  
  //  echo "altiumAclDataByObjectGuid is:\n";
  //  print_r($altiumAclDataByObjectGuid);

  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
  fputs($auditFoldersFile, $line . "\r\n");

  /** Create array to temporarily hold all lines that we generate, since we need to sort before writing to file. **/
  $auditFoldersLines = array();

  /* Setup query SQL commands. */
  $queryText = '
SELECT FOLDER.GUID AS FOLDERGUID, FOLDER.HRID, FOLDER.CREATEDBYGUID, FOLDER.LASTMODIFIEDBYGUID, FOLDER.CREATEDAT, FOLDER.LASTMODIFIEDAT, FOLDER.PARENTFOLDERGUID, FOLDER.DESCRIPTION, FT.HRID AS FOLDERTYPE
FROM ALU_FOLDER FOLDER
LEFT JOIN ALU_FOLDERTYPE FT ON FOLDER.FOLDERTYPEGUID = FT.GUID
;
';

  echo date('H:i:s') . " Begin query to read in all folder info from Vault database...\n";

  /* Execute SQL query. */
  $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Extract the fields of interest from this query result. */
      $FOLDERGUID = odbc_result($resultHandle, "FOLDERGUID");
      $DESCRIPTION = odbc_result($resultHandle, "DESCRIPTION");
      $FOLDERTYPE = odbc_result($resultHandle, "FOLDERTYPE");
      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");

      /* Lookup the usernames of the person to create and last modify this folder. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      /* Lookup the full path of this folder, based on cached data from having already read in this table once already. */
      $FOLDERPATH = CACF_TraceFolderPath($CACFconstants, $altiumFoldersByGuid, $FOLDERGUID);

      /** Output actual data. **/
      $line = "";
      $line = $line . "$FOLDERGUID".$ofs."$DESCRIPTION".$ofs."$FOLDERTYPE".$ofs."$FOLDERPATH".$ofs."$CREATEDBY".$ofs."$CREATEDAT".$ofs."$LASTMODIFIEDBY".$ofs."$LASTMODIFIEDAT";

      /** Output an ordered list of all the Altium user parameters that exist in our universe as columns in the csv file.
       If this part has a given parameter, list its value. **/

      /* Get all the permission fields that exist for this folder. */
      foreach ($altiumAclDataByObjectGuid[$FOLDERGUID] as $PERMNAME => $PERMVALUE)
        {
          /* Output $PERMVALUE. */
          $line = $line.$ofs.$PERMVALUE;
          
        } /* end foreach */

      /* Loop over all the defined Altium user parameter names. */
      foreach ($altiumUserParmNames as $PARAMETERNAME => $value)
        {
          /* Unconditionally print out a field separator. */
          $line = $line.$ofs;

          /* If this component has a stored folder user parameter named $PARAMETERNAME, then output it. */
          if (isset($altiumFolderUserParmValuesByGuid[$FOLDERGUID][$PARAMETERNAME]))
            {
              $line = $line.$altiumFolderUserParmValuesByGuid[$FOLDERGUID][$PARAMETERNAME];
            }

          /* Else emit a special string that indicates that this folder had no such parameter name defined. */
          else
            {
              $line = $line.$constAbsent;
            }

        } /* end foreach */

      /* Store this line in an in-memory array, so we can sort it all just before writing to disk. */
      $auditFoldersLines[$FOLDERPATH] = $line;

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  /* Sort all output lines here by folder path (key). */
  $rc = ksort($auditFoldersLines);
  if ($rc == FALSE) my_die("ksort() failed!");

  /* Loop over all lines in what will be our output file. */
  foreach ($auditFoldersLines as $key => $line)
    {
      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($auditFoldersFile, $line . "\r\n");

    } /* endforeach */

  /** Clear array that held all lines prior to writing to file. **/
  $auditFoldersLines = array();

  /** Close the audit output file. **/
  fclose($auditFoldersFile);
  echo date('H:i:s') . " Done writing audit folders file \"$auditFoldersFileName.\"\n";

} /* end CACF_CreateFolderAuditDataAndWriteToCsv() */


/****************************************************************
 * CACF_CreateChangeLogAuditDataAndWriteToCsv()
 *		Function to create change-log audit data and write to csv file.
 ****************************************************************/
function CACF_CreateChangeLogAuditDataAndWriteToCsv(&$db, $CACFconstants,
                                                    &$altiumUserNamesByGuid)
{
  /* Retrieve necessary global constants. */
  $ofs = 											$CACFconstants["ofs"];
  $auditChangeLogFileName = 						$CACFconstants["auditChangeLogFileName"];

  /** Open audit models file for writing. **/
  $auditChangeLogFile = my_fopen($auditChangeLogFileName);
  
  /** Output column headers. **/
  $line = "";
  $line = $line . "GUID".$ofs."CREATEDBY".$ofs."CREATEDAT".$ofs."LASTMODIFIEDBY".$ofs."LASTMODIFIEDAT".$ofs."LIFECYCLESTATETRANSITION".$ofs."LIFECYCLESTATEAFTER".$ofs."ITEMREVHRID".$ofs."CONTENTTYPEHRID".$ofs."COMMENT".$ofs."DESCRIPTION";

  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
  fputs($auditChangeLogFile, $line . "\r\n");

  /** Create array to temporarily hold all lines that we generate, since we need to sort before writing to file. **/
  $auditChangeLogLines = array();

  /** Execute SQL query and process each row returned by said query. **/
  /* Setup query SQL commands. */
  /* TODO:  Fix this to work with PCB footprints too! */
  $queryText = '
SELECT LCSC.GUID, ITEMREV.HRID AS ITEMREVHRID, ITEMREV."COMMENT", ITEMREV.DESCRIPTION, LCSC.CREATEDBYGUID, LCSC.LASTMODIFIEDBYGUID, LCSC.CREATEDAT, LCSC.LASTMODIFIEDAT, LCST.HRID AS LIFECYCLESTATETRANSITION, LCS.HRID AS LIFECYCLESTATEAFTER, ITEMINFO.CONTENTTYPEHRID
FROM ALU_LIFECYCLESTATECHANGE LCSC
LEFT JOIN ALU_ITEMREVISION ITEMREV ON ITEMREV.GUID = LCSC.ITEMREVISIONGUID
LEFT JOIN ALU_LIFECYCLESTATETRANSITION LCST ON LCST.GUID = LCSC.LIFECYCLESTATETRANSITIONGUID
LEFT JOIN ALU_LIFECYCLESTATE LCS ON LCS.GUID = LCSC.LIFECYCLESTATEAFTERGUID

LEFT JOIN 
(SELECT ITEM.GUID, ITEM.HRID, ITEM.SHARINGCONTROL, ITEM.HRID AS FOLDERHRID, CONTENTTYPE.HRID AS CONTENTTYPEHRID
FROM ALU_ITEM ITEM
LEFT JOIN ALU_CONTENTTYPE CONTENTTYPE ON ITEM.CONTENTTYPEGUID = CONTENTTYPE.GUID
) ITEMINFO ON ITEMREV.ITEMGUID = ITEMINFO.GUID

ORDER BY CONTENTTYPEHRID DESC, ITEMREV.HRID
;
';


  echo date('H:i:s') . " Begin query to generate change log info from Vault database...\n";

  /* Execute SQL query. */
     $resultHandle = odbc_exec($db, $queryText);

  /* Loop over all rows returned by SQL query. */
  while (odbc_fetch_row($resultHandle))
    {
      /* Retrieve specific fields from SQL query result row. */
      /* Note:  Prefix the revision number with the revision separator char (eg. "-"). */
      $GUID = odbc_result($resultHandle, "GUID");
      $ITEMREVHRID = odbc_result($resultHandle, "ITEMREVHRID");
      $COMMENT = CACF_RemapReservedChars(odbc_result($resultHandle, "COMMENT"));		/* Re-map ',' chars in COMMENT field to '|'! */
      $DESCRIPTION = CACF_RemapReservedChars(odbc_result($resultHandle, "DESCRIPTION"));	/* Re-map ',' chars in DESCRIPTION field to '|'! */
      $LIFECYCLESTATETRANSITION = odbc_result($resultHandle, "LIFECYCLESTATETRANSITION");
      $LIFECYCLESTATEAFTER = odbc_result($resultHandle, "LIFECYCLESTATEAFTER");
      $CONTENTTYPEHRID = odbc_result($resultHandle, "CONTENTTYPEHRID");

      $CREATEDBYGUID = odbc_result($resultHandle, "CREATEDBYGUID");
      $LASTMODIFIEDBYGUID = odbc_result($resultHandle, "LASTMODIFIEDBYGUID");
      $CREATEDAT = odbc_result($resultHandle, "CREATEDAT");
      $LASTMODIFIEDAT = odbc_result($resultHandle, "LASTMODIFIEDAT");

      /* Lookup the usernames of the person to create and last modify this folder. */
      $CREATEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $CREATEDBYGUID);
      $LASTMODIFIEDBY = CACF_LookupUsername($altiumUserNamesByGuid, $LASTMODIFIEDBYGUID);

      /* Output the actual fields retrieved from SQL query. */
      $line = "";
      $line = $line . "$GUID".$ofs."$CREATEDBY".$ofs."$CREATEDAT".$ofs."$LASTMODIFIEDBY".$ofs."$LASTMODIFIEDAT".$ofs."$LIFECYCLESTATETRANSITION".$ofs."$LIFECYCLESTATEAFTER".$ofs."$ITEMREVHRID".$ofs."$CONTENTTYPEHRID".$ofs."$COMMENT".$ofs."$DESCRIPTION";

      /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
      fputs($auditChangeLogFile, $line . "\r\n");

      /* Store this line in an in-memory array, so we can sort it all just before writing to disk. */
      /* Key by a concatenation of the model HRID and the component-where-used HRID. */
      //    $auditChangeLogLines[$ITEMREVHRID.$COMPHRID] = $line;

    } /* endwhile */

  /* Free memory that was holding query results. */
  odbc_free_result($resultHandle);

  ///* Sort all output lines here by concatenation of the model HRID and the component-where-used HRID (key). */
  //$rc = ksort($auditChangeLogLines);
  //if ($rc == FALSE) my_die("ksort() failed!");
  //
  ///* Loop over all lines in what will be our output file. */
  //foreach ($auditChangeLogLines as $key => $line)
  //{
  //  /* Write line to file.  Note:  explicitly use DOS (CR/LF) \r\n line endings! */
  //  fputs($auditChangeLogFile, $line . "\r\n");
  //
  //} /* endforeach */
  //
  ///** Clear array that held all lines prior to writing to file. **/
  //$auditChangeLogLines = array();

  /** Close the audit output file. **/
  fclose($auditChangeLogFile);
  echo date('H:i:s') . " Done writing change log file \"$auditChangeLogFileName.\"\n";

} /* end CACF_CreateChangeLogAuditDataAndWriteToCsv() */


/****************************************************************
 *	ENTRY POINT
 ****************************************************************/

/* Make sure we're not flagged that we should skip executing this main program code (eg. this script has been included by another .php script). */
if ($suppress_create_audit_csv_files_main_program != 1) 
{
  /** Configure error reporting **/
  error_reporting(E_ALL);

  /* Specify that we will need a lot of memory to run this. */
  ini_set("memory_limit","256M");

  /* Call CACF_Init() to perform all initialization. */
  /* Note:  All the parameters are outputs from this function! */
  CACF_Init(&$db, &$CACFconstants,
            &$altiumParmsByComponentLines, &$auditComponentsByType, &$auditComponentsByTypeUnmatched, &$altiumUserNamesByGuid, &$altiumAclUserPermissions);
  
  /* Dump all raw database tables to individual .csv files. */
  CACF_DumpAllRawDatabaseTablesToCsv(&$db, &$CACFconstants);

  /* Analyze all Vault folders and extract all linkages so that we understand folder trees. */
  CACF_AnalyzeVaultFolders(&$db, &$CACFconstants,
                           &$altiumFoldersByGuid);

  /* Analyze all Vault items and cache certain fields, indexed by GUID. */
  CACF_AnalyzeVaultItems(&$db, &$CACFconstants,
                         &$altiumItemsByGuid);

  /* Analyze all Vault item revisions and cache certain fields, indexed by GUID. */
  CACF_AnalyzeVaultItemRevisions(&$db, &$CACFconstants,
                                 &$altiumItemRevsByGuid);

  /* Create ACL audit data and write to csv file. */
  CACF_CreateAclAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                       &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumItemsByGuid, &$altiumItemRevsByGuid, &$altiumAclUserPermissions, 
                                       &$altiumAclDataByObjectGuid);
    
  /* Analyze all Vault folder user parameters and store for later use. */
  CACF_AnalyzeVaultFolderUserParameters(&$db, &$CACFconstants,
                                        &$altiumUserParmNames, &$altiumFolderUserParmValuesByGuid);

  /* Analyze all Vault item user parameters and store for later use. */
  CACF_AnalyzeVaultItemUserParameters(&$db, &$CACFconstants,
                                      &$altiumUserParmNames, &$altiumItemUserParmValuesByGuid);

  /* Create model audit data and write to csv file. */
  CACF_CreateModelAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                         &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                         &$altiumItemRevsByGuid);

  /* Create model where-used data and write to csv file. */
  CACF_CreateModelWhereUsedDataAndWriteToCsv(&$db, &$CACFconstants,
                                             &$altiumItemRevsByGuid);

  /* Create PCB audit data and write to csv file. */
  CACF_CreatePcbAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                       &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, 
                                       &$altiumItemRevsByGuid);

  /* Create component audit data. */
  CACF_CreateComponentAuditData(&$db, &$CACFconstants,
                                &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumAclDataByObjectGuid, &$altiumItemRevsByGuid, 
                                &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid);

  /* Write component audit data to csv file. */
  CACF_WriteComponentAuditDataToCsv(&$CACFconstants,
                                    &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                    &$altiumUserParmNames, &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid, 
                                    &$altiumUserParmNamesByCompType, &$altiumParmsByCompType);

  /* Create all per-component-type audit data and output to csv files. */
  CACF_CreateAllComponentTypeAuditDataAndWriteToCsv(&$CACFconstants,
                                                    &$altiumParmsByComponentLines,
                                                    &$altiumUserParmNamesByCompType, &$altiumParmsByCompType, &$altiumItemSysParmValuesByGuid, &$altiumItemUserParmValuesByGuid, &$altiumObsoleteCompsByGuid);

  /* Create per-component audit data and output to csv files. */
  CACF_CreatePerComponentAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                                &$altiumParmsByComponentLines);

  /* Create folder audit data and output to csv file. */
  CACF_CreateFolderAuditDataAndWriteToCsv(&$db, &$CACFconstants,
                                          &$altiumUserNamesByGuid, &$altiumFoldersByGuid, &$altiumFolderUserParmValuesByGuid, &$altiumUserParmNames, &$altiumAclDataByObjectGuid);

  /* Create change-log data and write to csv file. */
  CACF_CreateChangeLogAuditDataAndWriteToCsv(&$db, $CACFconstants,
                                             &$altiumUserNamesByGuid);


  /** Wrap things up. **/
  /** Report script done and peak memory usage. **/
  echo date('H:i:s') . " Script is ending successfully.\n";
  echo date('H:i:s') . " Peak memory usage: " . (memory_get_peak_usage(true) / 1024 / 1024) . " MiB.\n";

} /* endif */

?>

<?php /*_____________________END___OF___THE___CODE______________________

___________________________________________________________________*/ ?>
