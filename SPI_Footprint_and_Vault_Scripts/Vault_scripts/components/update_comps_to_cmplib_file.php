<?php 
/*!================================================================================================

	@file			update_comps_to_cmplib_file.php

	@brief			PHP script to update an Altium Vault .CmpLib file with respect to source data.

	@details		

    @version		0.11.0
					   $Rev::                                                                        $:
	@date			  $Date::                                                                        $:
	@author			$Author::                                                                        $:
					    $Id::                                                                        $:

	@copyright      Copyright (c) 2012 Sierra Photonics, Inc.  All rights reserved.
	
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

  /** Include various CACF_*() functions from create_audit_csv_files.php. **/
  /* Flag that we do not wish the main program in the included script to execute. */
$suppress_create_audit_csv_files_main_program = 1;
#include_once "../database/audit_database/create_audit_csv_files.php";
include_once "../Vaults/Altium Satellite Vault Server/audit_database/create_audit_csv_files.php";

/** Include PHPExcel functionality. **/
require_once '../drivers/php/PHPExcel/Classes/PHPExcel.php';

/****************************************************************
 * Assumptions:
 *  1.  HRID in CmpLib file must be the same as the component Comment field.
 *  2.  User must specify company part numbers ("ItemHRID") for all CmpLib & Excel components.
 *	
 * Notes:
 *  1.  Whatever you think about doing with this script, the one thing that
 *  you absolutely, positively, DO NOT WANT TO DO is to ever store a reference
 *  to some node in the XML tree.  This was tempting for me and earlier 
 *  versions of this script did so extensively.  The problem is that any
 *  time you make any slight change to the XML tree, that all those stored
 *  references are likely to become invalid.  The SimpleXML data structure,
 *  especially in conjunction with converting pieces of it to DOM and back
 *  (to insert nodes, etc.) is very much not static.  Individual nodes
 *  get moved around and reassigned in ways that are transparent to
 *  casual use when traversing the XML tree using SimpleXML children(),
 *  etc. calls.  But if you've stored a reference, you DO see these
 *  changes!  You have been warned.  Don't do it!  Use xpath queries
 *  instead.  Think of a reference to an XML node as being a pointer
 *  to some sector on your hard disk.  You really wanted it to point
 *  to a section of a file.  But as the OS grows and shrinks that file
 *  or defrags the disk, that sector won't contain what you think
 *  it contains.  Think of an xpath query, on the other hand,
 *  as being like a soft link to the file that you want.  You don't
 *  care if the OS defrags the disk, because any time you follow
 *  that soft link, you get the file that you want.
 *
 *  2.  Reserved user parameter names:
 *	"Comment"			(You MUST define this for each component)
 *  "Description"		(You MUST define this for each component)
 *  "Value"				(This will be silently created for you, copy of Comment)
 *  "ItemHRID"			(You MUST define this for each component, Vault Item #)
 *  "TRT Part Number"	(This will be silently created for you, copy of ItemHRID)
 *  "TRT Path"			(This will be silently created for you, copy of location in component tree)
 *  "SCHLIB"			(You MUST define this for each component)
 *  "PCBLIB"			(Optional)
 *  "PCBLIB x"			(Optional, where x ranges 0 to $maxPcbLibModels)
 ****************************************************************/

/* TODO when updating CmpLib file:
*/

/* General TODO list:
 1.  Flag and warn on user parameters or model links existing outside of a component!
 2.  Flag on any extended ASCII characters present in CmpLib file.
 3.  Flag if there are any user parameters in the Excel file that are new to this type of component!
 4.  Add code to insert <LifeCycleDefinitionGUID> and <RevisionNamingSchemeGUID> tags if needed, not just barf on their absence!
 5.  Check for disallowed characters in folder names (eg. "/").
 8.  Check ItemHrid of all new components against Vault.  If it already exists, make sure the path, etc. is the same!
 9.  Change code that does the initial parsing of CmpLib file to not collapse trees, such that when making emergency corrections
	to CmpLib file, this does not result in collapsing it also.
10.  Emergency changes to CmpLib file should not count as "updates needed" to Vault.  Add code to re-read CmpLib file after
	making such emergency changes.
11.  Detect and abort when Excel file is calling out user parameter names that aren't yet defined in this component type!
*/


/****************************************************************
 * function int_int_divide()
 *		Perform fast integer division.

 * Code borrowed from comment on http://php.net/manual/en/language.operators.arithmetic.php
 ****************************************************************/
function int_int_divide($x, $y) 
{
  return ($x - ($x % $y)) / $y;
}

/****************************************************************
 * function UCTCF_GetColLetterFromIndex()
 *		Convert a column index (starting at 0) into an Excel column letter (eg. "A", "J", "AB", "BL").
 ****************************************************************/
function UCTCF_GetColLetterFromIndex($index)
{
  /* If we cannot represent the given index in 2 alpha chars, then we're SOL. */
  if ($index > 676) //(26^2))
    my_die('Cannot represent this large of an index in Excel column letters!');
  
  /* Calculate base-26 digits. */
  $alpha1 = int_int_divide($index, 26);
  $alpha0 = $index % 26;

  echo "index = $index, alpha1 = $alpha1, alpha0 = $alpha0\n";

  echo "index = $index, intval = ".intval($index, 26)."\n";

  /* Handle the case where we don't need a 2 digit alpha char representation. */
  if ($alpha1 == 0)
    $alpha = "";
  else
    $alpha = chr(65 /* "A" */ - 1 + $alpha1);

  /* Add on the 0th char. */
  $alpha = $alpha . chr(65 /* "A" */  + $alpha0);

  echo " index = $index, alpha = \"$alpha\"\n";

  /* Return alpha code to caller. */
  return($alpha);

} /* end UCTCF_GetColLetterFromIndex() */


/****************************************************************
 * function UCTCF_ConfirmActionWithUser()
 *		Confirm an action with the user.
 *
 * Note:  No return values.  Script will abort if user doesn't like it.
 ****************************************************************/
function UCTCF_ConfirmActionWithUser($descriptionOfAction)
{
  $getResponse = false; //true;

  /** Make sure user really wants to do this. **/
  echo "\n\n*****************************************************************\n";
  echo "Script is about to $descriptionOfAction\n";
  echo " to CmpLib file.  Please make sure that you REALLY want to do this, \n";
  echo " and that this is not just some typo in the Excel file!\n\n";
  echo "Are you sure you wish to continue? (y/n) ";

  /* See if we're allowed to ask user for response. */
  if ($getResponse)
    {
      $response = fgets(STDIN);

      /* Check response. */
      if ( ($response != "y\n") && ($response != "Y\n") )
        my_die('Aborting script at user request!');    
      
    } /* endif */

  echo "Ok.  Proceeding to $descriptionOfAction....\n";
  echo "*****************************************************************\n\n";

} /* end UCTCF_ConfirmActionWithUser() */


/****************************************************************
 * function UCTCF_ConvertHrefToId()
 *		Convert an href number from .CmpLib into an ID number.
 ****************************************************************/
function UCTCF_ConvertHrefToId($href)
{
  /* Strip off leading '#' char. */
  return(preg_replace('/#/', '', $href));

} /* end UCTCF_ConvertHrefToId() */


/****************************************************************
 * function UCTCF_ConvertIdToHref()
 *		Convert an ID number from into a .CmpLib href.
 ****************************************************************/
function UCTCF_ConvertIdToHref($id)
{
  /* Add a leading '#' char. */
  return('#'.$id);

} /* end UCTCF_ConvertIdToHref() */


/****************************************************************
 * class SimpleXMLElementEx
 *
 * Attribution:  Code borrowed from http://stackoverflow.com/questions/3361036/php-simplexml-insert-node-at-certain-position/3361590#3361590
 *
 * Attribution:  Code borrowed from http://stackoverflow.com/questions/3361036/php-simplexml-insert-node-at-certain-position
 * Author:  salathe
 ****************************************************************/
class SimpleXMLElementEx extends SimpleXMLElement
{
  public function insertChildFirst($name, $value, $namespace)
  {
    // Convert ourselves to DOM.
    $targetDom = dom_import_simplexml($this);
    // Check for children
    $hasChildren = $targetDom->hasChildNodes();

    // Create the new childnode.
    $newNode = $this->addChild($name, htmlspecialchars($value), $namespace);

    // Put in the first position.
    if ($hasChildren)
      {
        $newNodeDom = $targetDom->ownerDocument->importNode(dom_import_simplexml($newNode), true);
        $targetDom->insertBefore($newNodeDom, $targetDom->firstChild);
      }

    // Return the new node.
    return $newNode;
  }

  public function insertNodeFirst(&$newNode)
  {
    /* Convert ourselves to DOM. */
    $targetDom = dom_import_simplexml($this);

    /* Check for existing children of the current node. */
    $hasChildren = $targetDom->hasChildNodes();

    /* Convert the new node to DOM. */
    $newNodeDom = $targetDom->ownerDocument->importNode(dom_import_simplexml($newNode), true);

    /* If we already have children, then put the new node in the first position. */
    if ($hasChildren)
      {
        $targetDom->insertBefore($newNodeDom, $targetDom->firstChild);
      }

    /* Else simply append the new node. */
    else
      {
        $targetDom->appendChild($newNodeDom);
      }

    /* Return the new node. */
    return $newNode;
  }

  public function insertNodeAfterSpecifiedNode(&$insertMe, &$insertAfter)
  {
    $insertAfter_dom = dom_import_simplexml($insertAfter);
    $insertMe_dom = $insertAfter_dom->ownerDocument->importNode(dom_import_simplexml($insertMe), true);
    if ($insertAfter_dom->nextSibling) {
      return $insertAfter_dom->parentNode->insertBefore($insertMe_dom, $insertAfter_dom->nextSibling);
    } else {
      return $insertAfter_dom->parentNode->appendChild($insertMe_dom);
    }
  }

  public function appendNode(&$newNode)
  {
    // Convert ourselves to DOM.
    $targetDom = dom_import_simplexml($this);

    $newNodeDom = $targetDom->ownerDocument->importNode(dom_import_simplexml($newNode), true);
    $targetDom->appendChild($newNodeDom);

    // Return a reference to the parent node.
    return $targetDom->ownerDocument;

  }

} /* end class SimpleXMLElementEx */


/****************************************************************
 * function UCTCF_ReadCmpLibFile()
 *		Read CmpLib XML file into in-memory XML data structure.
 ****************************************************************/
function UCTCF_ReadCmpLibFile(&$CmpLib, $CmpLibFileName)
{
  //  /* Read in text from file. */
  //  $data = implode("", file($CmpLibFileName));

  //  /* Interpret as an XML file. */
  //  $CmpLib = new SimpleXMLElement($data);

  /* Load using the derived class SimpleXMLElementEx. */
  $CmpLib = simplexml_load_file($CmpLibFileName, 'SimpleXMLElementEx');

} /* end UCTCF_ReadCmpLibFile() */


/****************************************************************
 * function UCTCF_WriteCmpLibFile()
 *		Write our in-memory XML data structure back to disk.
 ****************************************************************/
function UCTCF_WriteCmpLibFile(&$CmpLib, $CmpLibFileName)
{

  //  $CmpLib->asXML('new2.CmpLib');

  /* We have to convert to a DOMDocument at the last minute so that we can prettily format our output .xml. */
  $dom = new DOMDocument('1.0');
  $dom->preserveWhiteSpace = false;
  $dom->formatOutput = true;
  $dom->loadXML($CmpLib->asXML());
  
  /* Have the DOM class output to XML, but replace all LF (\n) with CRLF (\r\n) for DOS compatibility. */
  file_put_contents($CmpLibFileName, str_replace("\n", "\r\n", $dom->saveXML()));

} /* end UCTCF_WriteCmpLibFile() */


/****************************************************************
 * function UCTCF_ExtractRequiredParameters()
 *		Extract all RequiredParameters tags from a .CmpLib XML data structure.
 * 
 * Outputs:  &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName
 ****************************************************************/
function UCTCF_ExtractRequiredParameters(&$requiredParameters, 
                                         &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName) 
{
  /* Create 2 data structures to hold our user parameter declarations. */
  $cmpLibUserParmNamesById = array();
  $cmpLibUserParmIdsByName = array();

  /* Loop over all the (user) parameters that are children of the <Parameters> tag. */
  $requiredParameterNum = 0;
  foreach ($requiredParameters->children() as $TRequiredParameter)
    {
      //      echo "parm name is:". $TRequiredParameter->getName() ."\n";
      //                          print_r($TRequiredParameter);

      /* Extract the ID attribute, which should be the 0th attribute listed. */
      $id = (string) $TRequiredParameter->attributes();
      //      echo "id is $id\n";

      /* Loop over all the children of the <TRequiredParameter> tag. */
      foreach ($TRequiredParameter->children() as $parm)
        {
          /* We expect to see one child, named "HRID".  This represents the name of the user parameter (eg. "Voltage"). */
          if ($parm->getName() == "HRID")
            {
              //              echo "This is the HRID parm!\n";
                              
              /* Extract the name of the user parameter. */
              $name = (string) $parm;
              //              echo "HRID is $name\n";

              //              echo "Found user parameter declaration.  Name is \"" . $name . "\", Id is \"" . $id . "\".\n";

              /* Add an entry to the $cmpLibUserParmNamesById data structure. */
              /* Note:  This is storing the "unaltered" user parameter name! */
              $cmpLibUserParmNamesById[$id] = $name;

              /* Store the ID number in an array indexed by user parameter name. */
              /* Note:  This is storing the "unaltered" user parameter name! */
              $cmpLibUserParmIdsByName[$name]["id"] = $id;

            } /* endif */

        } /* end foreach $parm */

      /* Increment $requiredParameterNum */
      $requiredParameterNum++;

    } /* end foreach TRequiredParameter */

} /* end UCTCF_ExtractRequiredParameters() */


/****************************************************************
 * function UCTCF_ExtractRequiredModels()
 *		Extract all RequiredModels tags from a .CmpLib XML data structure.
 *
 * Outputs:  $cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 ****************************************************************/
function UCTCF_ExtractRequiredModels(&$requiredModels, 
                                     &$cmpLibModelTypesById, &$cmpLibModelTypesByType) 
{
  /* Create data structure to hold our model type declarations. */
  $cmpLibModelTypesById = array();
  $cmpLibModelTypesByType = array();

  /* Loop over all the model types that are children of the <RequiredModels> tag. */
  $requiredModelNum = 0;
  foreach ($requiredModels->children() as $TRequiredModel)
    {
      //      echo "parm name is:". $TRequiredModel->getName() ."\n";
      //                          print_r($TRequiredModel);

      /* Extract the ID attribute, which should be the 0th attribute listed. */
      $id = (string) $TRequiredModel->attributes();
      //      echo "id is $id\n";

      /* Loop over all the children of the <TRequiredModel> tag. */
      foreach ($TRequiredModel->children() as $parm)
        {
          /* We expect to see a child named "HRID".  This represents the type of the user model (eg. "PCBLIB"). */
          /* Note:  There is also a <ModelKind> tag, which is redundant.  So we are ignoring that one. */
          if ($parm->getName() == "HRID")
            {
              //              echo "This is the HRID parm!\n";
                              
              /* Extract the type of the user model. */
              $type = (string) $parm;
              //              echo "HRID is $type\n";

              //              echo "Found user model declaration.  Type is \"" . $type . "\", Id is \"" . $id . "\".\n";

              /* Add an entry to data structure. */
              $cmpLibModelTypesById[$id] = $type;

            } /* endif */

        } /* end foreach $parm */

      /** Re-index the $cmpLibModelLinksById information by type. **/
      $cmpLibModelTypesByType[$type] = array();

      /* Store the ID number. */
      $cmpLibModelTypesByType[$type]["id"] = (string) $id;


      /* Increment $requiredModelNum */
      $requiredModelNum++;

    } /* end foreach TRequiredModel */

} /* end UCTCF_ExtractRequiredModels() */


/****************************************************************
 * function UCTCF_ExtractModelLinks()
 *		Extract all ModelLinks tags from a .CmpLib XML data structure.
 *
 * Outputs:  $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid
 ****************************************************************/
function UCTCF_ExtractModelLinks(&$CACFconstants, 
                                 &$ModelLinksNode, 
                                 &$altiumModelDataByItemRevHrid, 
                                 &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid) 
{
  /* Create data structure to hold our model links. */
  $cmpLibModelLinksById = array();
  $cmpLibModelLinksByItemRevHrid = array();

  /* Loop over all the model links that are children of the <ModelLinks> tag. */
  $modelLinkNum = 0;
  foreach ($ModelLinksNode->children() as $TModelLink)
    {
      //      echo "parm name is:". $TModelLink->getName() ."\n";
      //                          print_r($TModelLink);

      /* Extract the ID attribute, which should be the 0th attribute listed. */
      $id = (string) $TModelLink->attributes();
      //      echo "id is $id\n";

      /* Create a new array entry to describe all fields of this ModelLink by ID. */
      $cmpLibModelLinksById[$id] = array();

      /* Clear the ItemGUID value. */
      $ItemGUID = "Unknown!";
      $ItemRevGUID = "Unknown!";
      $ItemRevHRID = "Unknown!";

      /* Loop over all the children of the <TModelLink> tag. */
      foreach ($TModelLink->children() as $tag)
        {
          /* Extract the value of this child tag. */
          $val = (string) $tag;

          /* Store the ItemGUID when we get it. */
          if ($tag->getName() == "ItemGUID")
            $ItemGUID = $val;

          /* Store the ItemRevGUID when we get it. */
          else if ($tag->getName() == "RevisionGUID")
            $ItemRevGUID = $val;

          /* Store the ItemRevHRID when we get it. */
          else if ($tag->getName() == "HRID")
            $ItemRevHRID = $val;

          /* Store this model link in array. */
          $cmpLibModelLinksById[$id][$tag->getName()] = $val;

        } /* end foreach */

      /* Sanity check. */
      if ( ($ItemGUID == "Unknown!") || ($ItemRevGUID == "Unknown!") || ($ItemRevHRID == "Unknown!") )
        my_die('Did not find ItemGUID or ItemRevGUID or ItemRevHRID info in ModelLink!!');

      /* Lookup some additional information, now that we have the ItemGUID for this model. */
      $cmpLibModelLinksById[$id]["MODELPATH"] = $altiumModelDataByItemRevHrid[$ItemRevHRID]["MODELPATH"];
      $cmpLibModelLinksById[$id]["MODELLIB"]  = $altiumModelDataByItemRevHrid[$ItemRevHRID]["MODELLIB"];

      /** Re-index the $cmpLibModelLinksById information by ItemRevHRID. **/
      $cmpLibModelLinksByItemRevHrid[$ItemRevHRID] = array();

      /* Store the ID number. */
      $cmpLibModelLinksByItemRevHrid[$ItemRevHRID]["id"] = (string) $id;

      /* Copy all key=value pairs to new array. */
      foreach ($cmpLibModelLinksById[$id] as $key => $value)
        $cmpLibModelLinksByItemRevHrid[$ItemRevHRID][$key] = $value;
        
      //      echo "modelLinksById[$id] is:\n";
      //      print_r($cmpLibModelLinksById[$id]);

      //      echo "modelLinksByItemRevHrid[$ItemRevHRID] is:\n";
      //      print_r($cmpLibModelLinksByItemRevHrid[$ItemRevHRID]);

      /* Increment $modelLinkNum */
      $modelLinkNum++;

    } /* end foreach */

  //  echo "In UCTCF_ExtractModelLinks(), modelLinksById is:\n";
  //  print_r($cmpLibModelLinksById);
  
  //  echo "In UCTCF_ExtractModelLinks(), modelLinksByItemRevHrid is:\n";
  //  print_r($cmpLibModelLinksByItemRevHrid);

} /* end UCTCF_ExtractModelLinks() */


/****************************************************************
 * function UCTCF_ExtractParameters()
 *		Extract all parameters that are children of a .CmpLib <Parameters> tag.
 *
 * Outputs:  $cmpLibTreeUserParmsCurrent, &$cmpLibTreeSysParmsCurrent
 ****************************************************************/
function UCTCF_ExtractParameters(&$CACFconstants, &$UCTCFconstants, 
                                 &$cmpLibUserParmNamesById,
                                 &$cmpLibTreeUserParmsInherited, &$cmpLibTreeSysParmsInherited, 
                                 $parametersNode, 
                                 &$cmpLibTreeUserParmsCurrent, &$cmpLibTreeSysParmsCurrent)
{
  /* Copy inherited sys parms and user parms to current. */
  /* Note:  Do not initialize the current arrays before doing this copy operation! */
  /* Note:  Do NOT make this a reference operation! */
  $cmpLibTreeSysParmsCurrent = $cmpLibTreeSysParmsInherited;
  $cmpLibTreeUserParmsCurrent = $cmpLibTreeUserParmsInherited;


  /* Attempt to loop over all <TParameter> tags that are children of the <Parameters> tag. */
  foreach ($parametersNode->children() as $TParameter)
    {
      /* See if we have a <TParameter> tag with children. */
      if ( ($TParameter->getName() == "TParameter") && ($TParameter->count() > 0) )
        {
          /* Loop over all the (user) parameters that are children of the <TParameter> tag. */
          foreach ($TParameter->children() as $parm)
            {
              /* Look for the <RequiredParameter> tag. */
              if ($parm->getName() == "RequiredParameter")
                {
                  /* Extract the "href" attribute value. */
                  $href = (string) $parm->attributes();

                  /* Strip off the leading "#" char to get the ID. */
                  $id = UCTCF_ConvertHrefToId($href);

                  /* Look up the name of the user parameter by ID. */
                  /* Call CACF_AlterAltiumUserParmName() to alter some user parameter names to make auditing easier. */
                  $parmName = CACF_AlterAltiumUserParmName($cmpLibUserParmNamesById[$id]);

                } /* endif */

              /* Look for the <Value> tag. */
              else if ($parm->getName() == "Value")
                {
                  /* Extract value of <Value> tag. */
                  $userParmValue = (string) $parm;

                  /* We need to handle the Comment and Description parameters as system parameters. */
                  if ( ($parmName == "Comment") || ($parmName == "Description") )
                    {

                      /* Alter the sys parameter name slightly to match column headers for our audit files. */
                      $parmName = CACF_AlterAltiumSysParmName($parmName);
                                      
                      /* Store system parameter. */
                      $cmpLibTreeSysParmsCurrent[$parmName] = $userParmValue;
                      
                    } /* endif */
                  
                  /* Else we can handle this as a user parameter. */
                  else
                    {                
                      /* Store user parameter for this component. */
                      $cmpLibTreeUserParmsCurrent[$parmName] = $userParmValue;
                                  
                    }
                              
                  /* Clear the $parmName variable in case for some inexplicable reason it never gets re-defined. */
                  $parmName = "Unknown!";

                } /* end elsif */

              /* Else we have some unknown tag.  Panic. */
              else
                {
                  my_die('In UCTCF_ExtractParameters(), unknown XML tag 1 "' . $parm->getName() . '"!');
                } /* endelse */

            } /* end foreach $TParameter->children() */

        } /* endif TParameter */

      /* Else we have some unknown tag.  Panic. */
      else
        {
          my_die('In UCTCF_ExtractParameters(), unknown XML tag 2 "' . $$TParameter->getName() . '", or known tag with no children!');
        } /* endelse */

    } /* end foreach $parametersNode->children() */

  //  echo "Leaving UCTCF_ExtractParameters(), cmpLibTreeUserParmsCurrent is:\n";
  //  print_r($cmpLibTreeUserParmsCurrent);

} /* end UCTCF_ExtractParameters() */


/****************************************************************
 * function UCTCF_ExtractModelChoices()
 *		Extract all parameters that are children of a .CmpLib <Parameters> tag.
 *
 * Note:  Be sure NOT to do pass-by-reference for $numModelsInherited!
 *
 * Outputs:  &$cmpLibTreeSysParmsCurrent, &$numModelsCurrent
 ****************************************************************/
function UCTCF_ExtractModelChoices(&$CACFconstants, &$UCTCFconstants, 
                                   &$cmpLibModelTypesById, &$cmpLibModelLinksById, 
                                   &$cmpLibTreeSysParmsInherited, $numModelsInherited,
                                   &$modelChoicesNode, 
                                   &$cmpLibTreeSysParmsCurrent, &$numModelsCurrent)
{
          
  //  echo "In UCTCF_ExtractModelChoices(), numModelsInherited is $numModelsInherited, cmpLibTreeSysParmsInherited is:\n";
  //  print_r($cmpLibTreeSysParmsInherited);

  /* Copy inherited sys parms to current. */
  /* Note:  Do not initialize the current array before doing this copy operation! */
  /* Note:  Do NOT make this a reference operation! */
  $cmpLibTreeSysParmsCurrent = $cmpLibTreeSysParmsInherited;
  
  //  echo "In UCTCF_ExtractModelChoices(), cmpLibTreeSysParmsCurrent is now:\n";
  //  print_r($cmpLibTreeSysParmsCurrent);


  /* Copy the number of models (inherited) to number of models (current). */
  $numModelsCurrent = $numModelsInherited;


  /* Attempt to loop over all <TModelChoice> tags that are children of the <ModelChoices> tag. */
  foreach ($modelChoicesNode->children() as $TModelChoice)
    {
      /* See if we have a <TModelChoice> tag with children. */
      if ( ($TModelChoice->getName() == "TModelChoice") && ($TModelChoice->count() > 0) )
        {
          /* Loop over all the model choices that are children of the <TModelChoice> tag. */
          foreach ($TModelChoice->children() as $model)
            {
              /* Look for the <RequiredModel> tag. */
              if ($model->getName() == "RequiredModel")
                {
                  /* Extract the "href" attribute value. */
                  $href = (string) $model->attributes();

                  /* Strip off the leading "#" char to get the ID. */
                  $id = UCTCF_ConvertHrefToId($href);

                  /** Retrieve modelTypeWithNum (eg. "SCHLIB", "PCBLIB", "PCBLIB 1", etc.) **/

                  /* Sanity check. */
                  if (!isset($cmpLibModelTypesById[$id]))
                    my_die('Unable to lookup ModelTypesById for model!');

                  /* Retrieve info. */
                  $modelTypeWithNum = $cmpLibModelTypesById[$id];                  

                } /* endif */

              /* Look for the <ModelLink> tag. */
              else if ($model->getName() == "ModelLink")
                {
                  /* Extract the "href" attribute value. */
                  $href = (string) $model->attributes();

                  /* Strip off the leading "#" char to get the ID. */
                  $id = UCTCF_ConvertHrefToId($href);
                              
                  //                              echo "Got <ModelLink> tag, id is $id\n";

                  /** Retrieve modelType, modelHRID, modelPath, and modelLib. **/
                  /* Sanity check. */
                  if (!isset($cmpLibModelLinksById[$id]["ModelKind"]))
                    my_die('Unable to lookup ModelKind for model!');

                  /* Retrieve info. */
                  $modelType = $cmpLibModelLinksById[$id]["ModelKind"];


                  /* Sanity check. */
                  if (!isset($cmpLibModelLinksById[$id]["HRID"]))
                    my_die('Unable to lookup ModelHRID for model!');

                  /* Retrieve info. */
                  $modelHRID = $cmpLibModelLinksById[$id]["HRID"];
                  //                              echo "Got <ModelLink> tag, modelHRID is $modelHRID\n";


                  /* Sanity check. */
                  if (!isset($cmpLibModelLinksById[$id]["MODELPATH"]))
                    my_die('Unable to lookup ModelPath for model!');

                  /* Retrieve info. */
                  $modelPath = $cmpLibModelLinksById[$id]["MODELPATH"];


                  /* Sanity check. */
                  if (!isset($cmpLibModelLinksById[$id]["MODELLIB"]))
                    my_die('Unable to lookup ModelLib for model!');

                  /* Retrieve info. */
                  $modelLib  = $cmpLibModelLinksById[$id]["MODELLIB"];

                  //                              echo "In UCTCF_ExtractModelChoices(), storing info for model ItemRevGuid $ItemRevGUID, ItemRevHRID \"".$cmpLibModelLinksById[$id]["HRID"]."\".\n";
        
                  /* Add information about this model to $cmpLibSysParmsByGuid[$ItemRevGUID]. */
                  $sysParms = &$cmpLibTreeSysParmsCurrent;
                  CACF_AddModelInfoToSysParms($CACFconstants, 
                                              $sysParms, 
                                              $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);

                  //                  echo "Found modelTypeWithNum \"$modelTypeWithNum\", modelType \"$modelType\", modelHRID \"$modelHRID\", modelPath \"$modelPath\", modelLib \"$modelLib\".\n";

                  /* Increment the number of models present in this component. */
                  $numModelsCurrent++;

                } /* end elsif */

              /* Else we have some unknown tag.  Panic. */
              else
                {
                  my_die('In UCTCF_ExtractModelChoices(), unknown XML tag 3 "' . $model->getName() . '"!');
                } /* endelse */

            } /* end foreach $TModelChoice->children() */

        } /* endif TModelChoice */

    } /* end foreach */

  //  echo "Leaving UCTCF_ExtractModelChoices(), cmpLibTreeSysParmsCurrent is:\n";
  //  print_r($cmpLibTreeSysParmsCurrent);

} /* end UCTCF_ExtractModelChoices() */


/****************************************************************
 * function UCTCF_ExtractComponentParmsFromComponentDefinitions()
 *		Extract all components that are children of a .CmpLib <ComponentDefinitions> tag.
 *
 * Note:  Be sure NOT to do pass-by-reference for $numModelsInherited!
 *
 * Outputs:  $&cmpLibUserParmsByGuid, $&cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection
 ****************************************************************/
function UCTCF_ExtractComponentParmsFromComponentDefinitions(&$CACFconstants, &$UCTCFconstants, 
                                                             &$altiumItemsByHrid, &$altiumItemRevsByHrid,
                                                             &$altiumItemSysParmValuesByGuid, 
                                                             &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName, &$cmpLibModelTypesById, &$cmpLibModelLinksById, 
                                                             &$cmpLibTreeUserParmsInherited, &$cmpLibTreeSysParmsInherited, $numModelsInherited, 
                                                             $path, $compDefNode, 
                                                             &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection)
{
  /* Retrieve necessary global constants. */
  $revSep = 										$CACFconstants["revSep"];
  $cInitialRev = 									$CACFconstants["cInitialRev"];

  /* Retrieve constants related to enabling and collapsing nodes in CmpLib XML file. */
  $cXmlStateName         = $UCTCFconstants["cXmlStateName"];
  $cXmlStateDisabled     = $UCTCFconstants["cXmlStateDisabled"];

  //  echo "In UCTCF_ExtractComponentParmsFromComponentDefinitions(), compDef is:\n";
  //  print_r($compDefNode);

  /* Attempt to loop over all components. */
  /* (eg. loop over all the TComponentDefinition tags that are children of the <ComponentDefinition> tag.) */
  foreach ($compDefNode->TComponentDefinition as $comp)
    {
      /* Make sure we have the component's HRID field. */
      if (!isset($comp->HRID))
        my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), could not find HRID for component!');
      $HRID = (string) $comp->HRID;
      
      /* Make sure we have the component's ItemHRID field. */
      if (!isset($comp->ItemHRID))
        my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), could not find ItemHRID for component!');
      $ItemHRID = (string) $comp->ItemHRID;
          
      /* See if we have a new component (no GUID in CmpLib file and no such ItemHrid in Vault). */
      if ( (!isset($comp->GUID)) && (!isset($altiumItemsByHrid[$ItemHRID])) )
        {
          //          my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), could not find GUID for component!');
          echo "Have new component \"$ItemHRID\"!\n";

          /* Fill in placeholder info for ItemGUID, ItemRevGUID, and RevisionId fields. */
          /* TODO:  We probably want to add this info to $altiumItemSysParmValuesByGuid as well! */
          $ItemGUID = (string) "ItemGUID_newComp".$cmpLibNumNewComps;
          $ItemRevGUID = (string) "ItemRevGUID_newComp".$cmpLibNumNewComps;
          $RevisionId = $cInitialRev;

          /* Increment the number of known new components. */
          $cmpLibNumNewComps++;

        } /* endif */

      /* Else we have a component that exists in some form... */
      else
        {
          /** Get Vault data for this ItemHRID. **/
          /* Extract VaultItemGUID from Vault data. */
          $VaultItemGUID = $altiumItemsByHrid[$ItemHRID]["GUID"];

          /* Lookup latest item revision number of this Item. */
          UCTCF_GetLatestRevisionNumberOfGivenItem($altiumItemsByHrid, $altiumItemRevsByHrid, 
                                                   $ItemHRID,
                                                   $VaultRevisionId);

          /* Construct VaultItemRevHrid. */
          $VaultItemRevHRID = $ItemHRID.$revSep.$VaultRevisionId;

          /* Lookup VaultItemRevGUID from VaultItemRevHrid. */
          $VaultItemRevGUID = $altiumItemRevsByHrid[$VaultItemRevHRID]["GUID"];

          /* Print debug info. */
          //          echo "VaultItemGUID is $VaultItemGUID\n";
          //          echo "VaultItemRevGUID is $VaultItemRevGUID\n";
          //          echo "VaultRevisionId is $VaultRevisionId\n";

          /* See if we have a component that got added to the Vault, but this info didn't get saved to CmpLib file. */
          if (!isset($comp->GUID))
            {
              echo "For ItemHRID $ItemHRID:\n";
              echo " Have newly added component, broken in CmpLib file!\n";
              
              /* Use the Vault information. */
              $ItemGUID = $VaultItemGUID;
              $ItemRevGUID = $VaultItemRevGUID;
              $RevisionId = $VaultRevisionId;
              
              /* Attempt to add new XML node to hold the ItemGUID number. */
              $newXmlNode = new SimpleXMLElement("<GUID>$ItemGUID</GUID>");
              $comp->insertNodeAfterSpecifiedNode($newXmlNode, $comp->HRID);
              
              /* Attempt to add a new XML node to hold the ItemRevGUID number. */
              $comp->addChild("RevisionGUID", $ItemRevGUID);
              
              /* Attempt to add a new XML node to hold the RevisionId number. */
              $comp->addChild("RevisionId", $RevisionId);
              
              /* Flag that we need to re-write the CmpLib file (even in audit mode) to add this piece of missing info to CmpLib file. */
              $cmpLibDidCorrection = true;

            } /* endif */

          /* Else we have a valid component. */
          else
            {
              /* Make sure we have the component's RevisionGUID field. */
              /* TODO:  Figure out how to handle new components that have not yet been released to Vault and thus have no RevisionGUIDs. */
              if (!isset($comp->RevisionGUID))
                my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), could not find RevisionGUID for component!');
      
              /* Extract ItemGUID and ItemRevGUID fields. */
              $ItemGUID = (string) $comp->GUID;
              $ItemRevGUID = (string) $comp->RevisionGUID;

              /* This is not working for the moment, so turn it off.  JWC 2012-10-03. */
              if (false)
                {
                  /* I haven't seen a case of the ItemGUID being wrong in the CmpLib data.  So omitting that check. */

                  /* Check the ItemRevGUID against the Vault data. */
                  if ($ItemRevGUID != $VaultItemRevGUID)
                    {
                      echo "For ItemHRID $ItemHRID:\n";
                      echo " Fixing ItemRevGUID with data from Vault!\n";

                      /* Override with Vault data. */
                      $ItemRevGUID = $VaultItemRevGUID;
                      $comp->RevisionGUID = $VaultItemRevGUID;

                      /* Flag that we need to re-write the CmpLib file (even in audit mode) to correct this info. */
                      $cmpLibDidCorrection = true;

                    } /* endif */

                } /* endif */

              /* Make sure we have the component's RevisionId field. */
              if (isset($comp->RevisionId))
                {
                  /* Retrieve info from CmpLib file XML node as normal. */
                  $RevisionId = (string) $comp->RevisionId;

                  /* This is not working for the moment, so turn it off.  JWC 2012-10-03. */
                  if (false)
                    {

                      /* Check the RevisionId against the Vault data. */
                      if ($RevisionId != $VaultRevisionId)
                        {
                          echo "For ItemHRID $ItemHRID:\n";
                          echo " Fixing RevisionId with data from Vault!\n";

                          /* Override with Vault data. */
                          $RevisionId = $VaultRevisionId;
                          $comp->RevisionId = $VaultRevisionId;
                      
                          /* Flag that we need to re-write the CmpLib file (even in audit mode) to correct this info. */
                          $cmpLibDidCorrection = true;
                      
                        } /* endif */

                    } /* endif */
                  
                } /* endif */

              /* Else the CmpLib file does not contain the RevisionId field.  I see this sometimes
               after adding new components to Vault via CmpLib file.  So we're going to do the next 
               best thing.  Try to retrieve this info from Vault. */
              else
                {
                  //              echo "Vault data is:\n";
                  //              print_r($altiumItemSysParmValuesByGuid[$ItemRevGUID]);

                  /* Make sure that this ItemRevGUID exists in Vault data. */
                  if (!isset($altiumItemSysParmValuesByGuid[$ItemRevGUID]))
                    {
                      echo "About to abort.\n";
                      echo "altiumItemSysParmValuesByGuid is:\n";
                      print_r($altiumItemSysParmValuesByGuid);

                      my_die("In UCTCF_ExtractComponentParmsFromComponentDefinitions(), could not find RevisionId for component \"$ItemHRID\", ItemRevGUID \"$ItemRevGUID\" (from Vault data)!");
                    }

                  /* Get RevisionId from Vault data. */
                  $RevisionId = preg_replace("/$revSep/", "", $altiumItemSysParmValuesByGuid[$ItemRevGUID][CACF_AlterAltiumSysParmName("REVISIONID")]);
                  echo "For component \"$ItemHRID\", looked up RevisionId in Vault as \"$RevisionId\".\n";

                  /* Attempt to add a new XML node to hold the RevisionId number. */
                  $comp->addChild("RevisionId", $RevisionId);

                  /* Flag that we need to re-write the CmpLib file (even in audit mode) to add this piece of missing info to CmpLib file. */
                  $cmpLibDidCorrection = true;

                } /* endelse */

            } /* endelse have valid component. */

        } /* endelse existing component. */
      
      /* See if we need to create an array at this location. */
      if (!isset($cmpLibUserParmsByGuid[$ItemRevGUID]))
        $cmpLibUserParmsByGuid[$ItemRevGUID] = array();
      
      /* See if we need to create an array at this location. */
      if (!isset($cmpLibSysParmsByGuid[$ItemRevGUID]))
        $cmpLibSysParmsByGuid[$ItemRevGUID] = array();

      /* Store ItemHRID and RevisionId fields in system parameters array. */
      $cmpLibSysParmsByGuid[$ItemRevGUID][CACF_AlterAltiumSysParmName("ITEMHRID")] = $ItemHRID;
      $cmpLibSysParmsByGuid[$ItemRevGUID][CACF_AlterAltiumSysParmName("REVISIONID")] = $revSep.$RevisionId;
      $cmpLibSysParmsByGuid[$ItemRevGUID][CACF_AlterAltiumSysParmName("COMPONENTPATH")] = $path;

      /* Reserve for all models for which we allocate space in our csv file. */
      /* Those models actually defined in this component will override the placeholders shortly. */
      $sysParms = &$cmpLibSysParmsByGuid[$ItemRevGUID];
      CACF_ReserveForModelsInSysParms($CACFconstants, 
                                      $sysParms);

      /* Disable this component by disabling updates to Vault. */
      $comp[$cXmlStateName] = $cXmlStateDisabled;
  
      /* Clear the $userParmName variable in case for some inexplicable reason it never gets defined. */
      $userParmName = "Unknown!";

      /* Attempt to loop over all children of the <TComponentDefinition> tag. */
      foreach ($comp->children() as $child)
        {
          /* See if we have a <Parameters> tag with children. */
          if ($child->getName() == "Parameters")
            {
              /* Sanity check. */
              if ($child->count() == 0)
                my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), found <Parameters> tag with no children!');

              /* Call UCTCF_ExtractParameters() to extract parameter name/value pairs from <Parameters> tag. */
              /* Outputs:  &$cmpLibTreeUserParmsCurrent, &$cmpLibTreeSysParmsCurrent */
              $parametersNode = &$child;
              UCTCF_ExtractParameters($CACFconstants, $UCTCFconstants, 
                                      $cmpLibUserParmNamesById,
                                      $cmpLibTreeUserParmsInherited, $cmpLibTreeSysParmsInherited, 
                                      $parametersNode, 
                                      $cmpLibTreeUserParmsCurrent, $cmpLibTreeSysParmsCurrent);
                
              /** Store all the user parameters for this component. **/
              /* Loop over all the user parameters defined for this component or for this component's groups (eg. paths). */
              foreach ($cmpLibTreeUserParmsCurrent as $userParmName => $userParmValue)
                {
                  
                  //                  echo "In UCTCF_ExtractComponentParmsFromComponentDefinitions(), copying current user parm \"$userParmName\"=\"$userParmValue\".\n";

                  /* Store user parameter for this component. */
                  $cmpLibUserParmsByGuid[$ItemRevGUID][$userParmName] = $userParmValue;

                } /* end foreach */

              /** Store all the sys parameters for this component. **/
              /* Loop over all the sys parameters defined for this component or for this component's groups (eg. paths). */
              foreach ($cmpLibTreeSysParmsCurrent as $sysParmName => $sysParmValue)
                {
                  
                  /* Store sys parameter for this component. */
                  $cmpLibSysParmsByGuid[$ItemRevGUID][$sysParmName] = $sysParmValue;

                  /* Special handling for Comment parameter. */
                  if (CACF_AlterAltiumSysParmName("Comment") == $sysParmName)
                    {
                      //                      echo "Checking Comment parameter!\n";

                      /* Sanity check. */
                      /* We currently require that the "HRID" (identifier) used in the CmpLib file be the same as the Comment parameter.
                       One of the reasons for this is that the HRID in CmpLib does NOT get stored to Vault! */
                      if ($sysParmValue != $HRID)
                        {
                          echo "Warning:  Found a component in the CmpLib file whose HRID does not match the Comment parameter!  \"$sysParmValue\" != \"$HRID\".  Correcting HRID and flagging to write corrected CmpLib file!\n";
                          
                          /* Overwrite the component's HRID field. */
                          $comp->HRID = $sysParmValue;
                          $HRID = $sysParmValue;

                          /* Flag that we need to re-write the CmpLib file (even in audit mode). */
                          $cmpLibDidCorrection = true;

                        } /* endif */
                      
                    } /* endif */

                } /* end foreach */

            } /* endif Parameters */

          /* See if we have a <ModelChoices> tag with children. */
          else if ($child->getName() == "ModelChoices")
            {
              /* Sanity check. */
              if ( ($child->count() == 0) && ($numModelsInherited == 0) )
                my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), found <ModelChoices> tag with no children, plus we have no inherited models!');

              //              echo "numModelsInherited is $numModelsInherited.\n";
              //              echo "Inherited sys parms are:\n";
              //              print_r($cmpLibTreeSysParmsInherited);

              /* Call UCTCF_ExtractModelChoices() to extract the model choices from this XML tag. */
              /* Outputs:  &$cmpLibTreeSysParmsCurrent, &$numModelsCurrent */
              /* Note:  Even if there are no model choices here (eg. $child->count() == 0) we will make
               this function call to copy inherited models to current models. */
              $modelChoicesNode = &$child;
              $numModelsCurrent = 0;			/* Initialize this to something. */
              UCTCF_ExtractModelChoices($CACFconstants, $UCTCFconstants, 
                                        $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                        $cmpLibTreeSysParmsInherited, $numModelsInherited,
                                        $modelChoicesNode, 
                                        $cmpLibTreeSysParmsCurrent, $numModelsCurrent);

              //              echo "In UCTCF_ExtractComponentParmsFromComponentDefinitions(), numModelsCurrent is $numModelsCurrent, cmpLibTreeSysParmsCurrent is:\n";
              //              print_r($cmpLibTreeSysParmsCurrent);

              /** Store all the sys parameters for this component. **/
              /* Loop over all the sys parameters defined for this component or for this component's groups (eg. paths). */
              foreach ($cmpLibTreeSysParmsCurrent as $sysParmName => $sysParmValue)
                {
                  
                  /* Store sys parameter for this component. */
                  $cmpLibSysParmsByGuid[$ItemRevGUID][$sysParmName] = $sysParmValue;
                  
                } /* end foreach */

            } /* end elsif ModelChoices */

          /* Look for other known tags that may exist at this level. */
          else if ( ($child->getName() == "HRID") || ($child->getName() == "GUID") || ($child->getName() == "ParentGroup") || 
                    ($child->getName() == "ItemHRID") || ($child->getName() == "RevisionGUID") || ($child->getName() == "RevisionId") )
            {
              /* Do nothing. */
            }

          /* Else we have some unknown tag.  Panic. */
          else
            {
              my_die('In UCTCF_ExtractComponentParmsFromComponentDefinitions(), unknown XML tag 4 "' . $child->getName() . '", or known tag with no children!');
            } /* endelse */

        } /* end foreach $comp->children() */

    } /* end foreach $compDefNode->TComponentDefinition */

} /* end UCTCF_ExtractComponentParmsFromComponentDefinitions() */


/****************************************************************
 * function UCTCF_ExtractComponentParmsFromTGroup()
 *		Extract all components that are children of a .CmpLib <TGroup> tag.
 *
 * Note:  Parameters &$cmpLibTreeUserParmsInherited, &$cmpLibTreeSysParmsInherited
 * may look like outputs from this function, but they are really only passed down the
 * recursion call list, not back up.
 *
 * Note:  Be sure NOT to do pass-by-reference for $numModelsInherited!
 *
 * Outputs:  &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection
 ****************************************************************/
function UCTCF_ExtractComponentParmsFromTGroup(&$CACFconstants, &$UCTCFconstants, 
                                               &$altiumItemsByHrid, &$altiumItemRevsByHrid,
                                               &$altiumFoldersByGuid, &$altiumItemSysParmValuesByGuid, 
                                               &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName, &$cmpLibModelTypesById, &$cmpLibModelLinksById, 
                                               &$cmpLibTreeUserParmsInherited, &$cmpLibTreeSysParmsInherited, $numModelsInherited, 
                                               $path, $tGroupNode, 
                                               &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection)
{
  /* Retrieve necessary global constants. */
  $pathSep = 										$CACFconstants["pathSep"];

  /* Retrieve constants related to enabling and collapsing nodes in CmpLib XML file. */
  $cXmlStateName         = $UCTCFconstants["cXmlStateName"];
  $cXmlStateDisabled     = $UCTCFconstants["cXmlStateDisabled"];
  $cXmlCollapsedName     = $UCTCFconstants["cXmlCollapsedName"];
  $cXmlCollapsedDisabled = $UCTCFconstants["cXmlCollapsedDisabled"];


  /* Handle the base case where we're given a path of "". */
  /* Look up the GUID of the initial path element in database to get starting path. */
  if ($path == "")
    {
      /* Make sure we have the root path element's GUID field. */
      if (!isset($tGroupNode->GUID))
        my_die('In UCTCF_ExtractComponentParmsFromTGroup(), could not find GUID for root path element!');
      
      /* Extract GUID field. */
      $GUID = (string) $tGroupNode->GUID;
      //      echo "GUID is \"$GUID\".\n";

      /* Lookup folder path for this starting point. */
      $path = CACF_TraceFolderPath($CACFconstants, $altiumFoldersByGuid, $GUID);

      //      echo "In UCTCF_ExtractComponentParmsFromTGroup(), base path is \"".$path."\".\n";
    } /* endif */

  /* Else we have the normal case where we already know part of the path and we have another 
   sub-folder to tack onto the running $path. */
  else
    {
      /* Make sure we have the group's HRID field. */
      if (!isset($tGroupNode->HRID))
        my_die('In UCTCF_ExtractComponentParmsFromTGroup(), could not find HRID for TGroup!');
      
      /* Attempt to extract new path element (TGroup's HRID field). */
      $HRID = (string) $tGroupNode->HRID;
      
      /* Add new element to path. */
      $path = $path.$HRID.$pathSep;
      
      //      echo "In UCTCF_ExtractComponentParmsFromTGroup(), path is \"".$path."\".\n";
    }


  //  echo "In UCTCF_ExtractComponentParmsFromTGroup(), numModelsInherited is $numModelsInherited.\n";
  //  echo "In UCTCF_ExtractComponentParmsFromTGroup(), cmpLibTreeSysParmsInherited is:\n";
  //  print_r($cmpLibTreeSysParmsInherited);

  /* Disable this group by (a) disabling updates to Vault, and (b) collapsing it from view. */
  $tGroupNode[$cXmlStateName] = $cXmlStateDisabled;
  $tGroupNode[$cXmlCollapsedName] = $cXmlCollapsedDisabled;
  
  /* Attempt to loop over all children of this <TGroup> or <TopGroup> tag. */
  foreach ($tGroupNode->children() as $child)
    {
      /* See if we have a <Parameters> tag. */
      if ($child->getName() == "Parameters")
        {
          
          /* Call UCTCF_ExtractParameters() to extract parameter name/value pairs from <Parameters> tag. */
          $parametersNode = &$child;
          UCTCF_ExtractParameters($CACFconstants, $UCTCFconstants, 
                                  $cmpLibUserParmNamesById,
                                  $cmpLibTreeUserParmsInherited, $cmpLibTreeSysParmsInherited, 
                                  $parametersNode, 
                                  $cmpLibTreeUserParmsCurrent, $cmpLibTreeSysParmsCurrent);

          /* Use the newly generated "Current" versions of these arrays from now on. */
          $cmpLibTreeUserParmsInherited = &$cmpLibTreeUserParmsCurrent;
          $cmpLibTreeSysParmsInherited = &$cmpLibTreeSysParmsCurrent;

        } /* endif */
                
      /* Else see if we have a <ModelChoices> tag. */
      else if ($child->getName() == "ModelChoices")
        {

          /* Call UCTCF_ExtractModelChoices() to extract the model choices from this XML tag. */
          $modelChoicesNode = &$child;
          $numModelsCurrent = 0;			/* Initialize this to something. */
          UCTCF_ExtractModelChoices($CACFconstants, $UCTCFconstants, 
                                    $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                    $cmpLibTreeSysParmsInherited, $numModelsInherited,
                                    $modelChoicesNode, 
                                    $cmpLibTreeSysParmsCurrent, $numModelsCurrent);
          
          //          echo "In UCTCF_ExtractComponentParmsFromTGroup(), back from UCTCF_ExtractModelChoices(), numModelsCurrent is $numModelsCurrent, cmpLibTreeSysParmsCurrent is:\n";
          //          print_r($cmpLibTreeSysParmsCurrent);

          /* Use the newly generated "Current" versions of these arrays from now on. */
          $cmpLibTreeUserParmsInherited = &$cmpLibTreeUserParmsCurrent;
          $cmpLibTreeSysParmsInherited = &$cmpLibTreeSysParmsCurrent;
          $numModelsInherited = $numModelsCurrent;

        } /* end elsif */

      /* Else see if we have a <ComponentDefinitions> tag, meaning that at this point in the hierarchy,
       we have components, rather than additional folders. */
      else if ($child->getName() == "ComponentDefinitions")
        {
          /* Make sure that this group has children. */
          if ($child->count() > 0)
            {
              /* Call UCTCF_ExtractComponentParmsFromComponentDefinitions() to extract all components and their parameters. */
              $compDefNode = &$child;
              UCTCF_ExtractComponentParmsFromComponentDefinitions($CACFconstants, $UCTCFconstants, 
                                                                  $altiumItemsByHrid, $altiumItemRevsByHrid,
                                                                  $altiumItemSysParmValuesByGuid, 
                                                                  $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName, $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                                                  $cmpLibTreeUserParmsInherited, $cmpLibTreeSysParmsInherited, $numModelsInherited, 
                                                                  $path, $compDefNode, 
                                                                  $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibNumNewComps, $cmpLibDidCorrection);
            } /* end elsif */

        } /* endif */

      /* Else see if we have a <Groups> tag, meaning that we have an additional folder at this
       point in the hierarchy.  Recursively call this function to handle it. */
      else if ($child->getName() == "Groups") 
        {
          /* Make sure that this group has children. */
          if ($child->count() > 0)
            {
              /** Recurse! **/
              /* Attempt to loop over <TGroup> children of the <Groups> tag. */
              foreach ($child->TGroup as $subTGroup)
                {

                  //                  echo "In UCTCF_ExtractComponentParmsFromTGroup(), about to recurse!  Path is \"".$path."\".\n";
                  //                  echo "In UCTCF_ExtractComponentParmsFromTGroup(), numModelsInherited is $numModelsInherited.\n";
                  //                  echo "In UCTCF_ExtractComponentParmsFromTGroup(), cmpLibTreeSysParmsInherited is:\n";

                  $tGroupNode = &$subTGroup;
                  UCTCF_ExtractComponentParmsFromTGroup($CACFconstants, $UCTCFconstants, 
                                                        $altiumItemsByHrid, $altiumItemRevsByHrid,
                                                        $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, 
                                                        $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName, $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                                        $cmpLibTreeUserParmsInherited, $cmpLibTreeSysParmsInherited, $numModelsInherited, 
                                                        $path, $tGroupNode, 
                                                        $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibNumNewComps, $cmpLibDidCorrection);

                } /* end foreach */

            } /* endif */          

          } /* end elsif */

      /* Look for other known tags that may exist at this level. */
      else if ( ($child->getName() == "HRID") || ($child->getName() == "GUID") || ($child->getName() == "ParentGroup") || 
                ($child->getName() == "ComponentSet") || ($child->getName() == "ItemNamingScheme") )
        {
          /* Do nothing. */
        }

      /* Else we have some unknown tag.  Panic. */
      else
        {
          my_die('In UCTCF_ExtractComponentParmsFromTGroup(), unknown XML tag "' . $child->getName() . '"!');
        } /* endelse */

    } /* end foreach loop over all subtGroups. */

} /* end UCTCF_ExtractComponentParmsFromTGroup() */


/****************************************************************
 * function UCTCF_ExtractAllComponentParmsFromCmpLib()
 *		Extract all components that are children of a .CmpLib <TGroup> tag.
 *
 * Note:  Be sure NOT to do pass-by-reference for $numModelsInherited!
 *
 * Outputs:  &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection)
 ****************************************************************/
function UCTCF_ExtractAllComponentParmsFromCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                                  &$altiumItemsByHrid, &$altiumItemRevsByHrid,
                                                  &$altiumFoldersByGuid, &$altiumItemSysParmValuesByGuid, 
                                                  &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName, &$cmpLibModelTypesById, &$cmpLibModelLinksById, 
                                                  &$CmpLib,
                                                  &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibNumNewComps, &$cmpLibDidCorrection)
{
  /* Initialize various things needed to start traversing the Groups tree in the CmpLib XML file. */
  $cmpLibNumNewComps = 0;
  $numModelsInherited = 0;
  $path = "";
  $tGroupNode = &$CmpLib->TopGroup;

  /* Initialize arrays to hold user and system parameters defined in the CmpLib XML tree. */
  $cmpLibTreeUserParmsInherited = array();
  $cmpLibTreeSysParmsInherited = array();
  $numModelsInherited = 0;

  /* Create arrays to hold all component system parameters and user parameters defined in CmpLib file. */
  $cmpLibUserParmsByGuid = array();
  $cmpLibSysParmsByGuid = array();

  //  echo "In UCTCF_ExtractAllComponentParmsFromCmpLib(), tGroupNode is:\n";
  //  print_r($tGroupNode);

  //  echo "In UCTCF_ExtractAllComponentParmsFromCmpLib(), altiumItemSysParmValuesByGuid is:\n";
  //  print_r($altiumItemSysParmValuesByGuid);

  /* Call UCTCF_ExtractComponentParmsFromTGroup() to do all the real work. */
  UCTCF_ExtractComponentParmsFromTGroup($CACFconstants, $UCTCFconstants, 
                                        $altiumItemsByHrid, $altiumItemRevsByHrid,
                                        $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, 
                                        $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName, $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                        $cmpLibTreeUserParmsInherited, $cmpLibTreeSysParmsInherited, $numModelsInherited, 
                                        $path, $tGroupNode, 
                                        $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibNumNewComps, $cmpLibDidCorrection);


  //  echo "Leaving UCTCF_ExtractAllComponentParmsFromCmpLib(), cmpLibSysParmsByGuid is:\n";
  //  print_r($cmpLibSysParmsByGuid);

  //  echo "Leaving UCTCF_ExtractAllComponentParmsFromCmpLib(), cmpLibUserParmsByGuid is:\n";
  //  print_r($cmpLibUserParmsByGuid);


} /* end UCTCF_ExtractAllComponentParmsFromCmpLib() */


/****************************************************************
 * function UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib()
 *		Traverse the remaining XML tree and build arrays describing all the
 * XML groups (aka database folders) by xpath and components by ItemHrid.
 *
 * Outputs: &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid
 ****************************************************************/
function UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                                           &$altiumFoldersByGuid, 
                                                           $path, $xpath, &$xmlNode,
                                                           &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid)
{
  /* Retrieve necessary global constants. */
  $pathSep = 										$CACFconstants["pathSep"];

  //  echo "Hello world from UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib().\n";

  //  echo "\nIn UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), xmlNode is:\n";
  //  print_r($xmlNode);
  
  /* Retrieve name of this node. */
  $name = $xmlNode->getName();

  /* Update xpath with name of this node. */
  $xpath = $xpath."/$name";
  //  echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), xpath is now \"$xpath\".\n";


  //  echo "In node \"$name\", count is " . $xmlNode->count() . "!\n";

  /* Handle the base case where we're given a path of "". */
  /* Look up the GUID of the initial path element in database to get starting path. */
  if ($path == "")
    {
      /* Make sure we have the root path element's GUID field. */
      if (!isset($xmlNode->GUID))
        my_die('In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), could not find GUID for root path element!');
      
      /* Extract GUID field. */
      $GUID = (string) $xmlNode->GUID;
      //      echo "GUID is \"$GUID\".\n";

      /* Lookup folder path for this starting point. */
      $path = CACF_TraceFolderPath($CACFconstants, $altiumFoldersByGuid, $GUID);

      //      echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), base path is \"".$path."\".\n";

      /* Store xpath query corresponding to this (database) path. */
      $cmpLibGroupXpathByPath[$path] = $xpath;
      //      echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), for group path \"$path\", stored xpath \"$xpath\".\n";
      
    } /* endif */

  /* Else see if we have a TGroup.  In this case, we already know part of the path and we have another 
   sub-folder to tack onto the running (database) $path. */
  else if ($name == "TGroup")
    {
      /* Make sure we have the group's HRID field. */
      if (!isset($xmlNode->HRID))
        my_die('In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), could not find HRID for TGroup!');
      
      /* Attempt to extract new path element (TGroup's HRID field). */
      $HRID = (string) $xmlNode->HRID;
      
      /* Add new element to path. */
      $path = $path.$HRID.$pathSep;
      
      //      echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), path is \"".$path."\".\n";

      /* Retrieve the ID of this group. */
      $groupId = (string) $xmlNode['id'];

      /* Add a query to our xpath to disambiguate it from other peer groups. */
      $xpath = $xpath."[@id=\"$groupId\"]";
      //      echo "xpath is \"$xpath\".\n";

      /* Store xpath query corresponding to this (database) path. */
      $cmpLibGroupXpathByPath[$path] = $xpath;
      //      echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), for group path \"$path\", stored xpath \"$xpath\".\n";
      
    } /* end elsif */

  /* Else see if we have a TComponentDefinition.  In this case, we simply need to store xpath
   information for this component. */
  else if ($name == "TComponentDefinition")
    {
      /* Make sure we have the component's ItemHRID field. */
      if (!isset($xmlNode->ItemHRID))
        my_die('In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), could not find ItemHRID for TComponentDefinition!');
      
      /* Attempt to extract component's ItemHRID field). */
      $ItemHRID = (string) $xmlNode->ItemHRID;
      
      /* Add a query to our xpath to disambiguate it from other peer components. */
      $xpath = $xpath."[ItemHRID=\"$ItemHRID\"]";
      //      echo "Component xpath is \"$xpath\".\n";

      /* Store path and xpath query corresponding to this component (by component ItemHrid). */
      $cmpLibCompByItemHrid[$ItemHRID] = array();
      $cmpLibCompByItemHrid[$ItemHRID]["path"] = $path;
      $cmpLibCompByItemHrid[$ItemHRID]["xpath"] = $xpath;
      //      echo "In UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib(), for component ItemHrid \"$ItemHRID\", stored xpath \"$xpath\".\n";

    } /* end elsif */


  /** Recurse into child nodes to find any TGroups or TComponentDefinitions. **/
  /* See if this XML node has any children. */
  if ($xmlNode->count() > 0)
    {
      /* Attempt to loop over all child nodes. */
      foreach ($xmlNode->children() as $childNode)
        {
          //          echo "Recursing from node \"$name\"!\n";

          /* Recursively call this function. */
          $xmlNode = &$childNode;
          UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                            $altiumFoldersByGuid, 
                                                            $path, $xpath, $xmlNode,
                                                            $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

        } /* end foreach */
            
    } /* endif node has children. */

} /* end UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib() */


/****************************************************************
 * function UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib()
 *		Traverse the remaining XML tree and build arrays describing all the
 * XML groups (aka database folders) and components by xpath.
 *
 * Outputs: &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid
 ****************************************************************/
function UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                                              &$altiumFoldersByGuid, 
                                                              &$CmpLib,
                                                              &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid)
{
  //  echo "Hello world from UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib().\n";

  /* Allocate an array to map folder path to group XML node xpath. */
  $cmpLibGroupXpathByPath = array();

  /* Allocate an array to map folder path + component comment to component XML node xpath. */
  $cmpLibCompByItemHrid = array();

  /* Initialize $path and $xpath. */
  $path = "";
  $xpath = "/TComponentSet";

  /* Specify that we will start with the <TopGroup> tag in XML file. */
  $xmlNode = &$CmpLib->TopGroup;

  /* Call UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib() to do all the real work. */
  UCTCF_ExtractGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                    $altiumFoldersByGuid, 
                                                    $path, $xpath, $xmlNode,
                                                    $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);


  //  echo "Leaving UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib(), cmpLibGroupXpathByPath is:\n";
  //  print_r($cmpLibGroupXpathByPath);

  //  echo "Leaving UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib(), $cmpLibCompByItemHrid is:\n";
  //  print_r($cmpLibCompByItemHrid);

} /* end UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib() */


/****************************************************************
 * UCTCF_CompareByItemHRID()
 *		Function to compare entries in *byGuid[] array by ITEMHRID value.
 * Used in conjunction with system uasort() function.
 *
 * Outputs:  Function returns -1, 0, or 1.
 ****************************************************************/
function UCTCF_CompareByItemHRID(&$a, &$b)
{
  /* Extract the 2 values we wish to compare by. */
  $aItemHRID = $a[CACF_AlterAltiumSysParmName("ITEMHRID")];
  $bItemHRID = $b[CACF_AlterAltiumSysParmName("ITEMHRID")];

  //  echo "Hello from UCTCF_CompareByItemHRID() $aItemHRID $bItemHRID.\n";
  //  print_r($a);
  //  print_r($b);

  /* Perform comparison and return to caller. */
  if ($aItemHRID == $bItemHRID) 
    return 0;
  else
    return ($aItemHRID < $bItemHRID) ? -1 : 1;

} /* end UCTCF_CompareByItemHRID() */


/****************************************************************
 * UCTCF_CompareByHRID()
 *		Function to compare entries in $cmpLibModelLinksByItemRevHrid array by HRID value.
 * Used in conjunction with system uasort() function.
 *
 * Outputs:  Function returns -1, 0, or 1.
 ****************************************************************/
function UCTCF_CompareByHRID(&$a, &$b)
{
  /* Extract the 2 values we wish to compare by. */
  $aHRID = (string) $a["HRID"];
  $bHRID = (string) $b["HRID"];

  //  echo "Hello from UCTCF_CompareByHRID() $aHRID $bHRID.\n";
  //  print_r($a);
  //  print_r($b);

  /* Perform comparison and return to caller. */
  return strcasecmp($aHRID, $bHRID);

} /* end UCTCF_CompareByHRID() */


/****************************************************************
 * UCTCF_CullCompsThatDontExistInCmpLib()
 *		Function to create component audit data.
 *
 * Outputs:  $altiumItemSysParmValuesByGuid
 ****************************************************************/
function UCTCF_CullCompsThatDontExistInCmpLib(&$CACFconstants, 
                                              &$altiumItemSysParmValuesByGuid, &$cmpLibSysParmsByGuid)
{

  /* Retrieve necessary global constants. */
  $numPerms = 										$CACFconstants["numPerms"];

  //  echo "Hello from UCTCF_CullCompsThatDontExistInCmpLib().\n";

  /** Loop over all known components (known to Altium Vault). **/
  foreach ($altiumItemSysParmValuesByGuid as $GUID => $value)
    {
      /* See if this component is defined in the CmpLib that we're processing. */
      if (isset($cmpLibSysParmsByGuid[$GUID]))
        {
          //          echo "Keeping GUID $GUID!\n";

          /* Proceed to delete a number of system parameters that do not and cannot exist at the CmpLib level. */
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("ANCESTORITEMREVISIONHRID")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("CREATEDBY")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("CREATEDAT")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LASTMODIFIEDBY")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LASTMODIFIEDAT")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("SHARINGCONTROL")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LIFECYCLESTATEHRID")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("RELEASEDATE")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("LIFECYCLEDEFINITIONHRID")]);
          unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("REVISIONNAMINGSCHEMEHRID")]);

          /* Delete the ACL related permissions. */
          for ($i = 0; ($i < $numPerms); $i++)
            {
              /* Remove entry for PERMISSIONSx. */
              unset($altiumItemSysParmValuesByGuid[$GUID][CACF_AlterAltiumSysParmName("PERMISSIONS" . $i)]);

            } /* endfor */


          /* Sort the list of CmpLib system parms used by this component. */
          $rc = ksort($cmpLibSysParmsByGuid[$GUID]);
          if ($rc == FALSE) my_die("ksort() failed!");
          
        } /* endif */

      /* Else this component is not known to the CmpLib that we're working on.
       Proceed to delete it completely from our data structures. */
      else
        {
          //          echo "Deleting GUID $GUID!\n";

          /* Delete this component from Vault-derived data structures. */
          /* Note:  Php seems to do a smart job of enumerating all elements of the foreach before
           actually executing code within the foreach.  Thus, it appears to be safe for us
           to delete things from $altiumItemSysParmValuesByGuid despite being in the middle
           of a foreach $altiumItemSysParmValuesByGuid. */
          unset($altiumItemSysParmValuesByGuid[$GUID]); 

        } /* endelse */


    } /* end foreach */

  //  print_r($altiumItemSysParmValuesByGuid);


  /** Sort the $cmpLibSysParmsByGuid array by ITEMHRID. **/
  uasort($cmpLibSysParmsByGuid, "UCTCF_CompareByItemHRID");

  //  echo "Leaving UCTCF_CullCompsThatDontExistInCmpLib(), altiumItemSysParmValuesByGuid is:\n";
  //  print_r($altiumItemSysParmValuesByGuid);

} /* end UCTCF_CullCompsThatDontExistInCmpLib() */


/****************************************************************
 * UCTCF_ReIndexByHrid()
 *		Function to create new arrays to describe items and itemRevs,
 * but indexed by their respective HRIDs. 
 *
 * Outputs:  $altiumItemsByHrid, $altiumItemRevsByHrid
 ****************************************************************/
function UCTCF_ReIndexByHrid(&$altiumItemsByGuid, &$altiumItemRevsByGuid,
                             &$altiumItemsByHrid, &$altiumItemRevsByHrid)
{

  /* Create our new output arrays. */
  $altiumItemsByHrid = array();
  $altiumItemRevsByHrid = array();

  /* Loop over all GUIDs in $altiumItemsByGuid. */
  foreach ($altiumItemsByGuid as $GUID => $val)
    {
      /* Extract HRID. */
      $HRID = $altiumItemsByGuid[$GUID]["HRID"];

      /* Store this entry in our new array. */
      $altiumItemsByHrid[$HRID] = $altiumItemsByGuid[$GUID];

      /* Add GUID entry. */
      $altiumItemsByHrid[$HRID]["GUID"] = $GUID;

    } /* end foreach */

  /* Loop over all GUIDs in $altiumItemRevsByGuid. */
  foreach ($altiumItemRevsByGuid as $GUID => $val)
    {
      /* Extract HRID. */
      $HRID = $altiumItemRevsByGuid[$GUID]["HRID"];

      /* Store this entry in our new array. */
      $altiumItemRevsByHrid[$HRID] = $altiumItemRevsByGuid[$GUID];

      /* Add GUID entry. */
      $altiumItemRevsByHrid[$HRID]["GUID"] = $GUID;

    } /* end foreach */

  //  echo "altiumItemsByHrid is:\n";
  //  print_r($altiumItemsByHrid);

} /* end UCTCF_ReIndexByHrid() */


/****************************************************************
 * UCTCF_GetLatestRevisionNumberOfGivenItem()
 *		Function to find the latest revision number for a given
 * Vault Item, by ItemHRID.
 *
 * Outputs:  $REVISIONID
 ****************************************************************/
function UCTCF_GetLatestRevisionNumberOfGivenItem(&$altiumItemsByHrid, &$altiumItemRevsByHrid, 
                                                  &$ItemHRID,
                                                  &$REVISIONID)
{
  /* Sanity check. */
  if (!isset($altiumItemsByHrid[$ItemHRID]["GUID"]))
    {
      //      my_die("In UCTCF_GetLatestRevisionNumberOfGivenItem(), ItemHRID \"$ItemHRID\" does not appear to exist in Vault!");

      /* Return 0 as the current RevisionId (since these numbers actually start at 1). */
      $REVISIONID = 0;

      /* Return to caller with error code. */
      return(-1);
    }

  /* First translate the ItemHRID into the ItemGUID. */
  $ItemGUID = $altiumItemsByHrid[$ItemHRID]["GUID"];

  /* Flag that we don't yet have any revision ID for this item. */
  $REVISIONID = "";

  /* Loop over all ItemRevs in the Vault database. */
  foreach ($altiumItemRevsByHrid as $ItemRevHRID => $val)
    {
      /* See if this ItemRev points to our given Item. */
      if ($altiumItemRevsByHrid[$ItemRevHRID]["ITEMGUID"] == $ItemGUID)
        {
          /* Extract the ItemRev's revision ID. */
          $ItemRevRevId = $altiumItemRevsByHrid[$ItemRevHRID]["REVISIONID"];

          //          echo "In UCTCF_GetLatestRevisionNumberOfGivenItem(), found match.  ItemRevHRID is \"$ItemRevHRID\", ItemRevRevId is \"$ItemRevRevId\".\n";
          //          echo "Obsolete flag for this itemRev is ".$altiumItemRevsByHrid[$ItemRevHRID]["OBSOLETE"] . "\n";

          /* Only store this if it is flagged as being not obsolete. */
          if ($altiumItemRevsByHrid[$ItemRevHRID]["OBSOLETE"] == 0)
            {
              $REVISIONID = $ItemRevRevId;
              //              echo "Storing REVISIONID \"$REVISIONID\".\n";

            } /* endif */

        } /* endif */

    } /* end foreach */

  /* Panic and die if we failed to find an ItemRev referencing our given Item. */
  if ($REVISIONID == "")
    my_die("Gack!  Failed to to find an ItemRev referencing our given ItemHrid \"$ItemHRID\"!");  

} /* end UCTCF_GetLatestRevisionNumberOfGivenItem() */


/****************************************************************
 * UCTCF_GetLatestItemRevOfGivenModelLib()
 *		Function to find the latest ItemRev of a given ModelLib.
 *
 * Outputs:  $ItemRevHrid
 ****************************************************************/
function UCTCF_GetLatestItemRevOfGivenModelLib(&$altiumModelDataByItemRevHrid, &$altiumItemRevsByHrid,
                                               &$ModelKind,
                                               &$ModelLib,
                                               &$ModelItemRevHrid)
{
  /* Flag that we don't yet have any revision ID for this item. */
  $REVISIONID = "";
  $ModelItemRevHrid = "";

  //  echo "In UCTCF_GetLatestItemRevOfGivenModelLib(), $altiumModelDataByItemRevHrid is:\n";
  //  print_r($altiumModelDataByItemRevHrid);

  /* Loop over all Models in the Vault database. */
  foreach ($altiumModelDataByItemRevHrid as $ItemRevHRID => $val)
    {
      /* Extract entry from $altiumModelDataByItemRevHrid array. */
      $entry = $altiumModelDataByItemRevHrid[$ItemRevHRID]["MODELLIB"];
      //      echo "Entry in altiumModelDataByItemRevHrid is \"" . $entry . "\"\n";
      
      /* Create version that has been stripped of any "_Svn_Rev123456" suffix. */
      $entryStripped = preg_replace('/_Svn_Rev[0-9]+/', '', $entry);
      //      echo "EntryStripped in altiumModelDataByItemRevHrid is \"" . $entryStripped . "\"\n";

      /* See if this ItemRev points to our given ModelLib. */
      /* We will also check if the user has specified a ModelLib name without the extension (eg. "SCHLIB"). */
      /* We will also allow a regex match to a specified ModelLib name that does not include the "_Svn_Rev123456" suffix. */
      if ( ($entry == $ModelLib) ||
           (strtoupper($entry) == strtoupper($ModelLib.".".$ModelKind)) ||
           (strtoupper($entryStripped) == strtoupper($ModelLib)) ||
           (strtoupper($entryStripped) == strtoupper($ModelLib.".".$ModelKind)) )
        {
          /* Extract the ItemRev's revision ID. */
          $ItemRevRevId = $altiumModelDataByItemRevHrid[$ItemRevHRID]["REVISIONID"];

          //          echo "In UCTCF_GetLatestItemRevOfGivenModelLib(), found match.  ItemRevHRID is \"$ItemRevHRID\", ItemRevRevId is \"$ItemRevRevId\".\n";
          //          echo "Obsolete flag for this itemRev is ".$altiumItemRevsByHrid[$ItemRevHRID]["OBSOLETE"] . "\n";

          /* Only store this if it is flagged as being not obsolete. */
          if ($altiumItemRevsByHrid[$ItemRevHRID]["OBSOLETE"] == 0)
            {
              $REVISIONID = $ItemRevRevId;
              $ModelItemRevHrid = $ItemRevHRID;

              //              echo "Storing REVISIONID \"$REVISIONID\".\n";
              //              echo "Storing ModelItemRevHrid \"$ModelItemRevHrid\".\n";

            } /* endif */

        } /* endif */      

    } /* end foreach */

  /* Panic and die if we failed to find an ItemRev referencing our given Item. */
  if ($REVISIONID == "")
    my_die("Gack!  Failed to to find an ItemRev referencing our given ModelLib \"$ModelLib\"!");  

  //  echo "Returning ItemRevHrid \"$ModelItemRevHrid\".\n";

} /* end UCTCF_GetLatestItemRevOfGivenModelLib() */


/****************************************************************
 * UCTCF_ExtractComponentsFromExcel()
 *		Function to extract component data from Excel source file.
 *
 * Outputs:  $excelUserParmsByItemHrid, $excelSysParmsByItemHrid
 ****************************************************************/
function UCTCF_ExtractComponentsFromExcel(&$CACFconstants, 
                                          &$altiumItemsByHrid, &$altiumItemRevsByHrid, 
                                          &$altiumModelDataByItemRevHrid, 
                                          &$objPHPExcel, 
                                          &$excelUserParmsByItemHrid, &$excelSysParmsByItemHrid)
{
  /* Retrieve necessary global constants. */
  $pathSep = 										$CACFconstants["pathSep"];
  $revSep = 										$CACFconstants["revSep"];
  $cRevFormatString = 								$CACFconstants["cRevFormatString"];

  /* Retrieve reference to current Excel worksheet. */
  $objWorksheet = $objPHPExcel->getActiveSheet();

  /* Allocate arrays to hold user and sys parameters for all components in excel file. */
  $excelUserParmsByItemHrid = array();
  $excelSysParmsByItemHrid = array();

  /* Set state as "search for command". */
  $state = "00_search_for_command";

  /* Declare $value. */
  $value = "";

  /* Clear ItemHRID */
  $ItemHRID = "";

  /* Clear flag that tells us to skip a given line. */
  $skipLine = false;
  
  /* Loop over all Excel worksheet rows that have data. */
  foreach ($objWorksheet->getRowIterator() as $row) 
    {
      //      echo "Row: " . $row->getRow() . "\n";

      /* Get an iterator for this row. */
      $cellIterator = $row->getCellIterator();
      $cellIterator->setIterateOnlyExistingCells(false); /* This loops all cells, even if it is not set.  By default, only cells that are set will be iterated. */

      /* Loop over all columns in this row. */
      foreach ($cellIterator as $cell) 
        {
          /* Get column letter for future use. */
          $rowNum = $cell->getRow();
          $colNum = $cell->getColumn();

          /* Get datatype of this cell. */
          $datatype = $cell->getDataType();

          //          echo "At row=" . $rowNum . ", col=" . $colNum . ", value=\"" . $cell->getValue() . "\", datatype=\"" . $datatype . "\".\n";

          /* If the data type is formula, then let's just go ahead and evaluate it now. */
          if ($cell->getDataType() == "f")
            $value = $cell->getCalculatedValue();

          /* Else it's just an ordinary number or string. */
          else
            $value = $cell->getValue();
          
          //          echo "At row=" . $rowNum . ", col=" . $colNum . ", value=\"" . $cell->getValue() . "\", datatype=\"" . $datatype . "\", value=\"" .$value ."\".\n";

          //          print_r(PHPExcel_Calculation::getInstance()->debugLog);


          /* Examine current state and act appropriately. */
          switch($state)
            {
              /* In state "00_search_for_command", search for a recognized command in column A. */
            case "00_search_for_command":
              {
                /* Look for a valid command in column A. */
                if ($colNum == "A")
                  {
                    /* Look for a "Define components" command. */
                    if ($value == "Define components")
                      {
                        echo "Found \"Define components\" command!\n";

                        /* Clear the column headers array, since we're starting a new component group. */
                        $colHeadersByCol = array();

                        /* Advance state. */
                        $state = "01_get_col_headers";
                        
                      }

                  } /* endif col A */

                break;
              } /* endcase */

              /* In state "01_get_col_headers", search for column headers in this row. */
            case "01_get_col_headers":
              {
                /* Make sure we have a non-null value in this cell. */
                if ($value != "")
                  {
                    /* Store this column header. */
                    $colHeadersByCol[$colNum] = $value;

                    echo "Found col header: \"" . $value . "\", at column " . $colNum . ".\n";

                  }
                
                break;
              } /* endcase */

              /* In state "02_get_component_parms", get component path from column A and user/sys parms from any other 
               column that had a defined column header. */
            case "02_get_component_parms":
              {
                /* Look for a valid path in column A. */
                if ($colNum == "A")
                  {

                    //                    echo "In column A, value is \"$value\".\n";

                    /* Look for an Excel string that tells us to skip this line. */
                    /* Also look for a string "#N/A" indicating that some part of the path couldn't be evaluated. */
                    $pos = strpos($value, "#N/A");
                    if ( ($value == "(skip)") || ( ($pos >= 0) && (!($pos === false)) ) )
                      {
                        /* Set flag that tells us to skip a given line. */
                        $skipLine = true;
                        echo "Skipping component with column A set to \"$value\".\n";
  
                      }

                    /* Else look for a valid path in column A. */
                    else if ($value != "")
                      {
                        echo "Found comp path: \"" . $value . "\", at column " . $colNum . ".\n";

                        /* Strip off any leading path separators for compatibility with Vault & CmpLib audit files. */
                        $value = preg_replace("/^".$pathSep."\+/", "", $value);

                        /* Store component path. */
                        $compProps["sysParms"][CACF_AlterAltiumSysParmName("COMPONENTPATH")] = $value;

                      } /* end elsif */

                    /* Else we have no valid path, so we figure we're done with this component group. */
                    else
                      {
                        echo "No comp path!  Transitioning back to idle state!\n";

                        /* Set state as "search for command". */
                        $state = "00_search_for_command";
                      }

                  } /* endif col A */

                /* Else look for user/sys parms in any column with a defined column header. */
                else if ( (isset($colHeadersByCol[$colNum])) && (!$skipLine) )
                  {

                    /* Extract column header name (typically user/sys parm name). */
                    $colHeader = $colHeadersByCol[$colNum];

                    //                    echo "Found user/sys parm value: \"" . $value . "\", for user/sys parm name \"" . $colHeader . "\", at column " . $colNum . ".\n";

                    /* Strip off any numeric suffixes from a "PCBLIB x" entry. */
                    $colHeaderStripped = preg_replace('/PCBLIB [0-9]+/', 'PCBLIB', $colHeader);	/* Strip out space and number suffix from PCBLIB. */

                    /* Store this user/sys parm in the $compProps array. */
                    switch($colHeaderStripped)
                      {

                        /* Handle special case of ItemHRID. */
                      case "ItemHRID":
                        {

                          /* Store ItemHRID. */
                          $ItemHRID = $value;
                          $compProps["ItemHRID"] = $value;

                          /* Reserve for all models for which we allocate space in our csv file. */
                          /* Those models actually defined in this component will override the placeholders shortly. */
                          $sysParms = &$compProps["sysParms"];
                          CACF_ReserveForModelsInSysParms($CACFconstants, 
                                                          $sysParms);

                          /* No break!  Handling for Comment and Description must immediately follow this code! */
                        }

                        /* Handle system parameters. */
                      case "Comment":
                      case "Description":
                        {
                          /* Alter sys parameter name as needed. */
                          $colHeader = CACF_AlterAltiumSysParmName($colHeader);

                          /* Store sys parameter name/value pair. */
                          $compProps["sysParms"][$colHeader] = $value;

                          break;
                        }

                        /* Handle models. */
                      case "SCHLIB":
                      case "PCBLIB":
                        {
                          /* Get a shortcut to ModelItemRevHrid. */
                          $ModelItemRevHrid = $value;
                          $ModelKind = $colHeaderStripped;	/* Strip out space and number suffix from PCBLIB. */
                          $modelTypeWithNum = $colHeader;	/* Leave the number suffixes on PCBLIB. */

                          /* PCBLIB is allowed to be null. */
                          if (!( ($ModelKind == "PCBLIB") && ( ($ModelItemRevHrid == "") || ($ModelItemRevHrid == "#N/A") ) ))
                            {
                              /* Make sure that this given model ModelItemRevHrid is valid. */
                              if ( (!isset($altiumItemRevsByHrid[$ModelItemRevHrid])) || (!isset($altiumModelDataByItemRevHrid[$ModelItemRevHrid])) )
                                {
                                  echo "Model \"$ModelItemRevHrid\" does not exist as an ItemRevHrid.  Proceeding to try to look it up by ItemHrid.\n";
                                  //                                  my_die("Specified model ItemRevHrid \"$ModelItemRevHrid\" does not exist in Vault!");

                                  /* Posit that the given value may be an ModelItemHrid. */
                                  $ModelItemHrid = $value;

                                  /* See if our given value is a plausibly-valid ModelItemHrid. */
                                  if (isset($altiumItemsByHrid[$ModelItemHrid]))
                                    {

                                      /* Lookup latest item revision number so that we can eventually get model info. */
                                      UCTCF_GetLatestRevisionNumberOfGivenItem($altiumItemsByHrid, $altiumItemRevsByHrid, 
                                                                               $ModelItemHrid,
                                                                               $REVISIONID);
                                      
                                      /* Now that we have an ModelItemHrid and a RevisionID, combine them to get an ItemRevHrid. */
                                      $ModelItemRevHrid = $ModelItemHrid.$revSep.$REVISIONID;
                                      echo "Model ItemHrid \"$ModelItemHrid\" translates into model ItemRevHrid \"$ModelItemRevHrid\".\n";
                                      
                                      /* Make sure that this constructed model ModelItemRevHrid is valid. */
                                      if ( (!isset($altiumItemRevsByHrid[$ModelItemRevHrid])) || (!isset($altiumModelDataByItemRevHrid[$ModelItemRevHrid])) )
                                        my_die("Constructed model ItemRevHrid \"$ModelItemRevHrid\" does not exist in Vault!");
                                      
                                    } /* endif */

                                  /* Else see if the given value is the MODELLIB name.  Attempt to translate this to an ItemRevHrid. */
                                  else
                                    {
                                      $ModelLib = CACF_RemapReservedChars($value);	/* Remap any ',' chars in cell to '|' chars! */
                                      echo "Model \"$value\" does not exist as an ItemHrid.  Proceeding to try to look it up as a ModelLib.\n";

                                      UCTCF_GetLatestItemRevOfGivenModelLib($altiumModelDataByItemRevHrid, $altiumItemRevsByHrid,
                                                                            $ModelKind,
                                                                            $ModelLib,
                                                                            $ModelItemRevHrid);

                                      echo "Got Model ModelItemRevHrid \"$ModelItemRevHrid\".\n";

                                    } /* endelse */
                                  
                                } /* endif is NULL */

                              
                              /* Lookup MODELPATH and MODELLIB for this model. */
                              $MODELPATH = $altiumModelDataByItemRevHrid[$ModelItemRevHrid]["MODELPATH"];
                              $MODELLIB  = $altiumModelDataByItemRevHrid[$ModelItemRevHrid]["MODELLIB"];
                              
                              /* Add information about this model to $compProps. **/
                              $sysParms = &$compProps["sysParms"];
                              $modelHRID = $ModelItemRevHrid;
                              $modelPath = $MODELPATH;
                              $modelLib = $MODELLIB;
                              CACF_AddModelInfoToSysParms($CACFconstants, 
                                                          $sysParms, 
                                                          $modelTypeWithNum, $modelHRID, $modelPath, $modelLib);
                          
                            } /* endif */

                          break;
                        }

                        /* Fix up certain EIA case codes that get mangled by PHPExcel (but not by Excel itself). */
                      case "Case-EIA":
                        {
                          /* Account for the inability of PHPExcel to properly treat a number as text.
                           Specifically, I cannot find a way to convince it to represent "0603" as "0603" and
                           not as "603".  So I'm kludging things here and tacking on the leading 0 that PHPExcel
                           is stripping off. */
                          /* TODO:  This is a huge kludge!  Find a better way to do this!! */
                          if ( ($value == "603") || ($value == "805") || ($value == "402") )
                            {
                              /* Prepend the leading 0. */
                              echo "Prepending leading 0!\n";
                              $value = "0" . $value;

                              //                              my_die('This should no longer be necessary!  Improve the .xlsx file by using TEXT(foo,"0000")!');

                            }

                          /* No break! */

                        } /* endcase */

                        /* Handle user parameters. */
                      default:
                        {

                          /* Alter user parameter name as needed. */
                          $colHeader = CACF_AlterAltiumUserParmName($colHeader);

                          /* Store user parameter name/value pair. */
                          $compProps["userParms"][$colHeader] = $value;

                          break;
                        }

                      } /* endswitch */
                    
                  } /* end elsif */

                break;
              } /* endcase */

            } /* endswitch */

        } /* end foreach cell in column */
          
      /** Perform any needed operations before starting a new row. **/
      switch($state)
        {
          /* Perform end-of-line operations for "01_get_col_headers". */
        case "01_get_col_headers":
          {
            /* Clear the $compProps array, since we're starting a new component. */
            $compProps = array();
            
            /* Advance state now that we're about to start a new row. */
            $state = "02_get_component_parms";
            
            break;
          }

          /* Perform end-of-line operations for "02_get_component_parms". */
        case "02_get_component_parms":
          {
            //            print_r($compProps);

//            echo "Processing ItemHRID " . $compProps["ItemHRID"] ."\n";

            /* Make sure we weren't flagged to skip this line. */
            if (!$skipLine)
              {
                /* Make sure we found an ItemHRID field. */
                if (!isset($compProps["ItemHRID"]))
                  my_die('Did not find ItemHRID defined for this component!');

                /* Make sure we found at least one userParm field. */
                if (!isset($compProps["userParms"]))
                  my_die('Did not find any userParms defined for this component!');

                /* Make sure we found at least one sysParm field. */
                if (!isset($compProps["sysParms"]))
                  my_die('Did not find any sysParms defined for this component!');            
            
                /* Lookup latest item revision number for compatibility with Vault & CmpLib audit files. */
                UCTCF_GetLatestRevisionNumberOfGivenItem($altiumItemsByHrid, $altiumItemRevsByHrid, 
                                                         $ItemHRID,
                                                         $REVISIONID);
            
                /* Store revision number. */
                $compProps["sysParms"][CACF_AlterAltiumSysParmName("REVISIONID")] = $revSep.$REVISIONID;
            
                /* Get component's ItemHRID. */
                $itemHRID = $compProps["ItemHRID"];

                /* Make sure that the itemHRID doesn't contain "#N/A" indicating that some part of the part number couldn't be evaluated. */
                $pos = strpos($itemHRID, "#N/A");
                if ( ($pos >= 0) && (!($pos === false)) )
                  {
                    /* Set flag that tells us to skip a given line. */
                    echo "Skipping line!\n";
                    $skipLine = true;
                  }

              } /* endif */

            /* Make sure we weren't flagged to skip this line. */
            if (!$skipLine)
              {
                /* Initialize supplier data that we will populate. */
                $supplierIndex = 1;

                /* Initialize supplier keys. */
                $supplierKeys = array("Supplier_Pn_Digi-Key_1", "Supplier_Pn_Digi-Key_2", "Supplier_Pn_Mouser_1", "Supplier_Pn_Mouser_2", "Supplier_Pn_Newark_1", "Supplier_Pn_Newark_2", "Supplier_Pn_Arrow_1", "Supplier_Pn_Arrow_2", "Supplier_Pn_Allied_1", "Supplier_Pn_Avnet_1", "Supplier_Name_Other_1", "Supplier_Pn_Other_1", "Supplier_Name_Other_2", "Supplier_Pn_Other_2");

                /* Iterate and look for all of our secret supplier keys in the Excel userParms. */
                foreach ($supplierKeys as $foo => $key)
                  {
                    //                    echo "Checking for secret supplier key $key!\n";

                    /* See if this key exists in our data. */
                    if (isset($compProps["userParms"][$key]))
                      {
                        //                        echo "Found secret supplier data as key $key!\n";

                        /* Extract supplier name from the key. */
                        $supplierName = preg_replace("/_[0-9]+/", "", $key);
                        $supplierName = preg_replace("/^Supplier_Pn_/", "", $supplierName);
                        //                        echo "supplierName is \"$supplierName\"\n";

                        /* Convert this supplier secret key into a standard "Supplier x" / "Supplier Part Number x" pair. */
                        $supplierPartNum = $compProps["userParms"][$key];
                        //                        echo "supplierPartNum is \"$supplierPartNum\"\n";

                        /* If this is an "other" supplier name, then store supplier name for next time. */
                        if ($supplierName == "Supplier_Name_Other")
                          {
                            $supplierNameOther = $compProps["userParms"][$key];
                          } /* endif */

                        /* Else this is a supplier part number field. */
                        else
                          {
                            /* Retrieve previously stored other supplier name, if needed. */
                            if ($supplierName == "Other")
                              {
                                $supplierName = $supplierNameOther;
                              }

                            /* Make sure we have a non-null supplier part number. */
                            if ($supplierPartNum != "") 
                              {

                                /* Convert this supplier secret key into a standard "Supplier x" / "Supplier Part Number x" pair. */
                                $compProps["userParms"]["Supplier $supplierIndex"] = $supplierName;
                                $compProps["userParms"]["Supplier $supplierIndex Part Number"] = $supplierPartNum;

                                /* Increment the index into suppliers. */
                                $supplierIndex++;

                              }

                          } /* endelse */

                      } /* endif */

                    /* Remove the secret key from user parameters. */
                    unset($compProps["userParms"][$key]);

                  } /* end foreach */

                //                print_r($compProps["userParms"]);


                /* Sanity check. */
                if ( isset($excelUserParmsByItemHrid[$itemHRID]) || (isset($excelSysParmsByItemHrid[$itemHRID])) )
                  {
                    echo "Before dying, excelSysParmsByItemHrid is:\n";
                    print_r($excelSysParmsByItemHrid);

                    echo "excelUserParmsByItemHrid is:\n";
                    print_r($excelUserParmsByItemHrid);

                    print_r($compProps);

                    my_die("Error in processing ItemHRID " . $compProps["ItemHRID"] .".  It seems to not be unique in this file!");

                  } /* endif */

                
                /* Add this component's info to arrays holding info for all components. */
                $excelUserParmsByItemHrid[$itemHRID] = $compProps["userParms"];
                $excelSysParmsByItemHrid[$itemHRID] = $compProps["sysParms"];

                /** Create "TRT Part Number" user parm. **/
                /* Add a hidden user parameter called "TRT Part Number" that will essentially
                 mirror the ItemHrid.  TRT Part Number will be what actually appears on
                 BOMs, etc.  It must be a user parameter because we cannot override
                 the LibRef (ItemRevHrid) with variant operations. */
                /* NOTE:  This operation is highly TRT-specific! */
                $nextRevId = sprintf($cRevFormatString, ($REVISIONID+1));
                // $excelUserParmsByItemHrid[$itemHRID]["TRT Part Number"] = $ItemHRID.$revSep.$nextRevId;
                $excelUserParmsByItemHrid[$itemHRID]["TRT Part Number"] = $ItemHRID;
            

                /** Create "TRT Path" user parm. **/
                /* Add a hidden user parameter called "TRT Path" that will essentially
                 mirror the Vault database path to this component.  The idea is to include the
                 information about where in the component tree this component came from within
                 the component itself. */
                /* NOTE:  This operation is highly TRT-specific! */
                $excelUserParmsByItemHrid[$itemHRID]["TRT Path"] = $compProps["sysParms"][CACF_AlterAltiumSysParmName("COMPONENTPATH")];


                /** Create "Value" user parm. **/
                /* Add a hidden user parameter called "Value" that will essentially
                 mirror the component Comment.  This may be helpful/necessary down the road
                 in dealing with varied or overridden component values. */
                /* NOTE:  This operation is highly TRT-specific! */
                $excelUserParmsByItemHrid[$itemHRID]["Value"] = $compProps["sysParms"][CACF_AlterAltiumSysParmName("Comment")];


                /* Sort the list of Excel system parms used by this component. */
                $rc = ksort($excelSysParmsByItemHrid[$itemHRID]);
                if ($rc == FALSE) my_die("ksort() failed!");
          
              } /* endif !skipline */

            /* Clear ItemHRID */
            $ItemHRID = "";
  
            /* Clear the $compProps array, since we're starting a new component. */
            $compProps = array();
            
            break;

          } /* endcase 02 */
      
        } /* endswitch */

      /* Clear flag that tells us to skip a given line. */
      $skipLine = false;
  
    } /* end foreach row */


  /* Sort the components in the Excel file by ITEMHRID. */
  $rc = ksort($excelSysParmsByItemHrid);
  if ($rc == FALSE) my_die("ksort() failed!");
          
  //  echo "excelSysParmsByItemHrid is:\n";
  //  print_r($excelSysParmsByItemHrid);

  //  echo "excelUserParmsByItemHrid is:\n";
  //  print_r($excelUserParmsByItemHrid);


} /* end UCTCF_ExtractComponentsFromExcel() */


/****************************************************************
 * function UCTCF_GetXmlNodeFromXpathQuery()
 *		Retrieve a single XML node from a valid xpath query. 
 *
 * Outputs: &$resultNode
 ****************************************************************/
function UCTCF_GetXmlNodeFromXpathQuery(&$CmpLib, &$xpathQuery, 
                                        &$resultNode)
{
  //  echo "In UCTCF_GetXmlNodeFromXpathQuery(), xpathQuery is \"$xpathQuery\".\n";
  
  /* Query the CmpLib XML data structure. */
  /* The result should be an array with exactly 1 element at index [0]. */
  $queryResult = $CmpLib->xpath($xpathQuery);

  //  echo "In UCTCF_GetXmlNodeFromXpathQuery(), queryResult is:\n";
  //  print_r($queryResult);
  
  /* Sanity check number of results. */
  if (count($queryResult) != 1)
    my_die("In UCTCF_GetXmlNodeFromXpathQuery(), query \"$xpathQuery\" results in " . count($queryResult) . " xml nodes from query, instead of just 1!");

  /* Sanity check existence of [0] node. */
  if (!isset($queryResult[0]))
    my_die("In UCTCF_GetXmlNodeFromXpathQuery(), query \"$xpathQuery\" results in no xml node at index 0 in the query result!");

  /* Now that we passed our sanity checks, return single result to caller. */
  /* Note:  This doesn't work if you make it a reference operation, so leave it as a copy op! */
  $resultNode = $queryResult[0];  

  //  echo "In UCTCF_GetXmlNodeFromXpathQuery(), resultNode is:\n";
  //  print_r($resultNode);
  
} /* end UCTCF_GetXmlNodeFromXpathQuery() */


/****************************************************************
 * function UCTCF_FixUpXmlIds()
 *		Fixup XML "id" and "ParentGroup" attributes following
 * insertion of new user parameter names, etc.
 *
 * Outputs:  (Changes to XML data structure at &$xmlNode)
 ****************************************************************/
function UCTCF_FixUpXmlIds(&$CACFconstants, &$UCTCFconstants, 
                           &$CmpLib,
                           &$cmpLibUserParmNamesById, &$cmpLibModelTypesById, &$cmpLibModelLinksById,
                           &$currIdNum, $parentIdNum, 
                           &$xmlNode)
{
  //  echo "Hello world from UCTCF_FixUpXmlIds().\n";

  //  echo "\nIn UCTCF_FixUpXmlIds(), xmlNode is:\n";
  //  print_r($xmlNode);
  
  /* Retrieve name of this node. */
  $name = $xmlNode->getName();

  //  echo "In node \"$name\", count is " . $xmlNode->count() . "!\n";

  /** Cleanup linkages between a component calling out a user parameter, a required model, 
   or a model link, and the possibly changed ID number of said referenced object. **/
  /* See if this node is named "RequiredParameter" and has an href attribute. */
  /* Note:  This tag is part of a component.  Components occur later in the XML file than
   the user parameter, model type, and model link declarations.  Thus, it is safe for
   us to assume that all these declarations have already had their ID numbers fixed up
   by the time we execute the code below. */
  if ($name == "RequiredParameter")
    {
      /* Sanity check. */
      if (!isset($xmlNode['href']))
        my_die('Missing href attribute in XML file!');

      /* Get existing value of href attribute. */
      $id = UCTCF_ConvertHrefToId( (string) $xmlNode['href']);

      /* Sanity check. */
      if (!isset($cmpLibUserParmNamesById[$id]))
        my_die('Unable to lookup user parm name by ID!');

      /* Lookup user parameter name in previously constructed data structure. */
      $userParmName = $cmpLibUserParmNamesById[$id];
      //      echo "In UCTCF_FixUpXmlIds(), UserParmName id is \"$id\", userParmName is \"$userParmName\".\n";

      /* Construct an xpath query so that we may find new ID for this user parm name. */
      $xpathQuery = "/TComponentSet/RequiredParameters/TRequiredParameter[HRID=\"$userParmName\"]";

      /* Run the xpath query to get the TRequiredParameter node in question. */
      UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                     $resultNode);

      /* Sanity check. */
      if (!isset($resultNode['id']))
        my_die('Unable to lookup new ID by user parm name!');

      /* Now lookup new ID number. */
      $id = (string) $resultNode['id'];
      //      echo "In UCTCF_FixUpXmlIds(), id is now \"$id\".\n";

      /* Overwrite href attribute at this XML node with correct pointer to user parameter name. */
      $xmlNode['href'] = UCTCF_ConvertIdToHref($id);

    } /* endif */

  /* See if this node is named "RequiredModel" and has an href attribute. */
  /* Note:  This tag is part of a component.  Components occur later in the XML file than
   the user parameter, model type, and model link declarations.  Thus, it is safe for
   us to assume that all these declarations have already had their ID numbers fixed up
   by the time we execute the code below. */
  else if ($name == "RequiredModel")
    {
      /* Sanity check. */
      if (!isset($xmlNode['href']))
        my_die('Missing href attribute in XML file!');

      /* Get existing value of href attribute. */
      $id = UCTCF_ConvertHrefToId( (string) $xmlNode['href']);

      /* Sanity check. */
      if (!isset($cmpLibModelTypesById[$id]))
        my_die('Unable to lookup model type by ID!');

      /* Lookup model type in previously constructed data structure. */
      $modelType = $cmpLibModelTypesById[$id];
      //      echo "In UCTCF_FixUpXmlIds(), ModelType id is \"$id\", modelType is \"$modelType\".\n";

      /* Construct an xpath query so that we may find new ID for this model type. */
      $xpathQuery = "/TComponentSet/RequiredModels/TRequiredModel[HRID=\"$modelType\"]";

      /* Run the xpath query to get the TRequiredParameter node in question. */
      UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                     $resultNode);

      /* Sanity check. */
      if (!isset($resultNode['id']))
        my_die('Unable to lookup new ID by model type!');

      /* Now lookup new ID number. */
      $id = (string) $resultNode['id'];
      //      echo "In UCTCF_FixUpXmlIds(), id is now \"$id\".\n";

      /* Overwrite href attribute at this XML node with correct pointer to model type. */
      $xmlNode['href'] = UCTCF_ConvertIdToHref($id);

    } /* endif */

  /* See if this node is named "ModelLink" and has an href attribute. */
  /* Note:  This tag is part of a component.  Components occur later in the XML file than
   the user parameter, model type, and model link declarations.  Thus, it is safe for
   us to assume that all these declarations have already had their ID numbers fixed up
   by the time we execute the code below. */
  else if ($name == "ModelLink")
    {
      /* Sanity check. */
      if (!isset($xmlNode['href']))
        my_die('Missing href attribute in XML file!');

      /* Get existing value of href attribute. */
      $id = UCTCF_ConvertHrefToId( (string) $xmlNode['href']);

      /* Sanity check. */
      if (!isset($cmpLibModelLinksById[$id]))
        my_die('Unable to lookup model link by ID!');

      /* Lookup model link in previously constructed data structure. */
      $modelLinkHrid = $cmpLibModelLinksById[$id]["HRID"];
      //      echo "In UCTCF_FixUpXmlIds(), ModelLink id is \"$id\", modelLinkHrid is \"$modelLinkHrid\".\n";

      /* Construct an xpath query so that we may find new ID for this model type. */
      $xpathQuery = "/TComponentSet/ModelLinks/TModelLink[HRID=\"$modelLinkHrid\"]";

      /* Run the xpath query to get the TRequiredParameter node in question. */
      UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                     $resultNode);

      /* Sanity check. */
      if (!isset($resultNode['id']))
        my_die('Unable to lookup new ID by modelLinkHrid!');

      /* Now lookup new ID number. */
      $id = (string) $resultNode['id'];
      //      echo "In UCTCF_FixUpXmlIds(), id is now \"$id\".\n";

      /* Overwrite href attribute at this XML node with correct pointer to user parameter name. */
      $xmlNode['href'] = UCTCF_ConvertIdToHref($id);

    } /* endif */


  /** Fix up child's ParentGroup ID number to match possibly changed actual Parent Group ID. **/
  /* See if this node is named "TGroup" or "TComponentDefinition", and has a child named 
   "ParentGroup" with an href attribute. */
  else if ( ($name == "TGroup") || ($name == "TComponentDefinition") )
    {
      //      echo "id is " . $xmlNode['id'] . "\n";
      //      echo "About to fixup ParentGroup href attribute!\n";

      /* Flag that we have not yet found "ParentGroup". */
      $haveParentGroup = false;
      
      /* Attempt to loop over all child nodes. */
      foreach ($xmlNode->children() as $childNode)
        {
          /* Look for a child node named 'ParentGroup'. */
          if ($childNode->getName() == 'ParentGroup')
            {
              /* Sanity check. */
              if (!isset($childNode['href']))
                my_die('Missing href attribute in XML file!');

              /* Flag that we have "ParentGroup". */
              $haveParentGroup = true;
      
              /* Overwrite this node's href # with what it should be, given insertions, etc. that
               have previously occurred. */
              $childNode['href'] = UCTCF_ConvertIdToHref($parentIdNum);

            } /* endif */

        } /* end foreach */

      /* Sanity check. */
      if (!$haveParentGroup)
        my_die('Missing <ParentGroup> child of $name in XML file!');

    } /* end elsif */


  /** Fix up top node's LifeCycleDefinition, RevisionNamingScheme, and Vault GUIDs. **/
  /* See if this node is named "TComponentSet", and has various children whose values
   we need to check. */
  else if ($name == "TComponentSet")
    {
      echo "About to fixup TComponentSet children values!\n";

      /* Sanity checks. */
      if (!isset($xmlNode->LifeCycleDefinitionGUID))
        my_die("In UCTCF_FixUpXmlIds(), failed to find <LifeCycleDefinitionGUID> tag under top level node!");

      if (!isset($xmlNode->RevisionNamingSchemeGUID))
        my_die("In UCTCF_FixUpXmlIds(), failed to find <RevisionNamingSchemeGUID> tag under top level node!");

      if (!isset($xmlNode->VaultGUID))
        my_die("In UCTCF_FixUpXmlIds(), failed to find <VaultGUID> tag under top level node!");

      if (!isset($xmlNode->VaultName))
        my_die("In UCTCF_FixUpXmlIds(), failed to find <VaultName> tag under top level node!");
      
      /* Retrieve default values for these tags. */
      $defaultLifeCycleDefinitionGuid =  $UCTCFconstants["defaultLifeCycleDefinitionGuid"];
      $defaultRevisionNamingSchemeGuid = $UCTCFconstants["defaultRevisionNamingSchemeGuid"];
      $defaultVaultGuid =                $UCTCFconstants["defaultVaultGuid"];
      $defaultVaultHrid =                $UCTCFconstants["defaultVaultHrid"];
      
      /* Retrieve current values for these tags. */
      $currentLifeCycleDefinitionGuid =  (string) $xmlNode->LifeCycleDefinitionGUID;
      $currentRevisionNamingSchemeGuid = (string) $xmlNode->RevisionNamingSchemeGUID;
      $currentVaultGuid =                (string) $xmlNode->VaultGUID; 
      $currentVaultHrid =                (string) $xmlNode->VaultName;  

      //      echo "currentLifeCycleDefinitionGuid is \"$currentLifeCycleDefinitionGuid\".\n";
      //      echo "currentRevisionNamingSchemeGuid is \"$currentRevisionNamingSchemeGuid\".\n";
      //      echo "currentVaultGuid is \"$currentVaultGuid\".\n";
      //      echo "currentVaultHrid is \"$currentVaultHrid\".\n";
      
      /* Check values. */
      if ($currentLifeCycleDefinitionGuid != $defaultLifeCycleDefinitionGuid)
        {
          echo "Setting new LifeCycleDefinitionGUID value to be \"$defaultLifeCycleDefinitionGuid\" instead of \"$currentLifeCycleDefinitionGuid\"!\n";
          $xmlNode->LifeCycleDefinitionGUID = $defaultLifeCycleDefinitionGuid;
        }

      if ($currentRevisionNamingSchemeGuid != $defaultRevisionNamingSchemeGuid)
        {
          echo "Setting new RevisionNamingSchemeGUID value to be \"$defaultRevisionNamingSchemeGuid\" instead of \"$currentRevisionNamingSchemeGuid\"!\n";
          $xmlNode->RevisionNamingSchemeGUID = $defaultRevisionNamingSchemeGuid;
        }

      if ($currentVaultGuid != $defaultVaultGuid)
        {
          echo "Setting new VaultGUID value to be \"$defaultVaultGuid\" instead of \"$currentVaultGuid\"!\n";
          $xmlNode->VaultGUID = $defaultVaultGuid;
        }

      if ($currentVaultHrid != $defaultVaultHrid)
        {
          echo "Setting new VaultName value to be \"$defaultVaultHrid\" instead of \"$currentVaultHrid\"!\n";
          $xmlNode->VaultName = $defaultVaultHrid;
        }

    } /* end elsif */


  /** Fix up this node's own ID number. **/
  /* See if this node has an id attribute. */
  if (isset($xmlNode['id']))
    {
      //      echo "Setting id of node \"$name\" to $currIdNum\n";

      /* Overwrite this node's ID number with what it should be, given insertions, etc. that
       have previously occurred. */
      $id = $currIdNum;
      $xmlNode['id'] = $id;

      /* Store that any children of this node may refer to their parent ID number above. */
      $parentIdNum = $currIdNum;
      
      /* Increment current ID number. */
      $currIdNum++;

    } /* endif */


  /** Recurse into child nodes to fix up any problems with them. **/
  /* See if this XML node has any children. */
  if ($xmlNode->count() > 0)
    {
      /* Attempt to loop over all child nodes. */
      foreach ($xmlNode->children() as $childNode)
        {
          //          echo "Recursing from node \"$name\"!\n";

          /* Recursively call this function. */
          $xmlNode = &$childNode;
          UCTCF_FixUpXmlIds($CACFconstants, $UCTCFconstants, 
                            $CmpLib,
                            $cmpLibUserParmNamesById, $cmpLibModelTypesById, $cmpLibModelLinksById,
                            $currIdNum, $parentIdNum, 
                            $xmlNode);

        } /* end foreach */
            
    } /* endif node has children. */

} /* end UCTCF_FixUpXmlIds() */


/****************************************************************
 * function UCTCF_FixUpAllXmlIds()
 *		Fixup all XML "id" and "ParentGroup" attributes following
 * insertion of new user parameter names, etc.  Then proceed
 * to re-create all data structures that store information about
 * CmpLib user parameter, model type, and model link declarations.
 *
 * Outputs: (Changes to XML data structure at &$CmpLib),
 *          &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *          &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *          &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid
 ****************************************************************/
function UCTCF_FixUpAllXmlIds(&$CACFconstants, &$UCTCFconstants, 
                              &$altiumModelDataByItemRevHrid, 
                              &$CmpLib,
                              &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                              &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                              &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid)
{

  /* Set the starting ID number to be 0. */
  $currIdNum = 0;
  $parentIdNum = 0;

  //  echo "In UCTCF_FixUpAllXmlIds(), cmpLibUserParmIdsByName is:\n";
  //  print_r($cmpLibUserParmIdsByName);

  //  echo "In UCTCF_FixUpAllXmlIds(), cmpLibModelLinksByItemRevHrid is:\n";
  //  print_r($cmpLibModelLinksByItemRevHrid);

  //  echo "In UCTCF_FixUpAllXmlIds(), xmlNode is:\n";
  //  print_r($xmlNode);
  
  /* Call UCTCF_FixUpXmlIds() to do most of the real work. */
  $xmlNode = &$CmpLib;
  UCTCF_FixUpXmlIds($CACFconstants, $UCTCFconstants, 
                    $CmpLib,
                    $cmpLibUserParmNamesById, $cmpLibModelTypesById, $cmpLibModelLinksById,
                    $currIdNum, $parentIdNum, 
                    $xmlNode);
  

  /* Now that we have called UCTCF_FixUpXmlIds() and had it re-index all the XML
   node ID numbers for us, we must re-create all data structures that were indexed
   by the previous set of ID numbers.  In addition, the ['id'] parts of the data
   structures indexed by name are now out of sync as well.  So we're just going
   to recreate all these data structures and call it good. */

  //  echo "In UCTCF_FixUpAllXmlIds(), about to recreate all *ById arrays!\n";

  /* Extract all required parameters (aka. declarations of user parameters) from XML file data structures. */
  UCTCF_ExtractRequiredParameters($CmpLib->RequiredParameters, 
                                  $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName);

  /* Extract all model types (aka. declarations of model types) from XML file data structures. */
  UCTCF_ExtractRequiredModels($CmpLib->RequiredModels, 
                              $cmpLibModelTypesById, $cmpLibModelTypesByType);

  /* Extract all model links (aka. pointers to an SCHLIB / PCBLIB object) from XML file data structures. */
  $ModelLinksNode = &$CmpLib->ModelLinks;
  UCTCF_ExtractModelLinks($CACFconstants, 
                          $ModelLinksNode, 
                          $altiumModelDataByItemRevHrid, 
                          $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid);


  //  echo "In UCTCF_FixUpAllXmlIds(), cmpLibUserParmNamesById is now:\n";
  //  print_r($cmpLibUserParmNamesById);
  
  //  echo "In UCTCF_FixUpAllXmlIds(), cmpLibModelLinksById is now:\n";
  //  print_r($cmpLibModelLinksById);
  
} /* end UCTCF_FixUpAllXmlIds() */


/****************************************************************
 * UCTCF_AddItemDeclarationToCmpLib()
 *		Function to add a declaration for a new item (eg. user parameter name)
 * to CmpLib file.
 *
 * Outputs:  $itemListById, $doFixupFromInsertBefore,
 *           (Changes to CmpLib XML data structure)
 ****************************************************************/
function UCTCF_AddItemDeclarationToCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                          &$CmpLib,
                                          &$itemListById, &$itemListByName, $itemType, 
                                          SimpleXMLElement &$newXmlNode, SimpleXMLElement &$parentXmlNode, 
                                          $newItemName, $newItemId)
{
  //  echo "Hello from UCTCF_AddItemDeclarationToCmpLib()\n";
  //  echo "In UCTCF_AddItemDeclarationToCmpLib(), newItemName is \"$newItemName\", newItemId is \"$newItemId\", itemType is \"$itemType\".\n";

  //  echo "In UCTCF_AddItemDeclarationToCmpLib(), itemListById is:\n";
  //  print_r($itemListById);

  /* Add new item to $itemListById array. */
  if ($itemType == "TModelLink")
    $itemListById[$newItemId] = array("HRID" => $newItemName);
  else
    $itemListById[$newItemId] = $newItemName;

  //  echo "In UCTCF_AddItemDeclarationToCmpLib(), after adding, itemListById is now:\n";
  //  print_r($itemListById);

  /* Choose a special comparison function when dealing with TModelLink. */
  if ($itemType == "TModelLink")
    $compareFunc = "UCTCF_CompareByHRID";
  else
    $compareFunc = "strcasecmp";

  /* Sort $itemListById array using non-case-sensitive sort. */
  /* Note:  Here we are using uasort() (with strcasecmp() as comparison func) to try to emulate what Altium does with ordering CmpLib href id references. */
  $rc = uasort($itemListById, $compareFunc);
  if ($rc == FALSE) my_die("uasort() failed!");
  

  /** Insert new XML node into parent XML tag. **/

  /* State the beginning of the xpath query. */
  $xpathQuery = "/TComponentSet";

  /* Construct the xpath prefix corresponding to our different item types. */
  if ($itemType == "TRequiredParameter")
    $xpathQuery = $xpathQuery."/RequiredParameters/TRequiredParameter";
  else if ($itemType == "TRequiredModel")
    $xpathQuery = $xpathQuery."/RequiredModels/TRequiredModel";
  else if ($itemType == "TModelLink")
    $xpathQuery = $xpathQuery."/ModelLinks/TModelLink";
  else
    my_die("Unsupported itemType \"$itemType\"!");

  /* Flag that we have not yet seen a valid node. */
  $insertAfter = false;

  /* Loop over all item names. */
  foreach ($itemListById as $id => $itemName)
    {
      /* Special handling for dealing with TModelLink. */
      if ($itemType == "TModelLink")
        $itemName = (string) $itemListById[$id]["HRID"];

      //      echo "In UCTCF_AddItemDeclarationToCmpLib(), id is $id, itemName is $itemName.\n";

      /* See if this user parameter name already exists. */
      if (isset($itemListByName[$itemName]))
        {
          /* Flag that we now have a valid insertAfter node for future reference. */
          $insertAfter = $itemName;

        } /* endif */

      /* Else this user parameter name does not yet exist.  Figure out how to add it. */
      else
        {
          echo "Need to insert item name \"$itemName\".\n";

          /* If we have already seen a valid XML node, then we can proceed to do the insert. */
          if (!is_bool($insertAfter))
            {

              /** Get the XML node corresponding to the itemName stored in $insertAfter. **/

              /* Finish constructing xpath query. */
              $xpathQuery = $xpathQuery."[HRID=\"$insertAfter\"]";
              //              echo "xpathQuery is \"$xpathQuery\".\n";

              /* Run the xpath query to get the TRequiredParameter node in question. */
              UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                             $resultNode);
              $insertAfterNode = &$resultNode;

//              echo "insertAfterNode is:\n";
//              print_r($insertAfterNode);

              echo "Attempting to do simplexml_insert_after() operation!\n";
              $parentXmlNode->insertNodeAfterSpecifiedNode($newXmlNode, $insertAfterNode);
            } /* endif */

          /* Else insert a new first node. */
          else
            {
              //              echo "Attempting to insert new first node!\n";
              $parentXmlNode->insertNodeFirst($newXmlNode);

            } /* endelse */

        } /* endelse */

    } /* end foreach */

  //  echo "parent node is now:\n";
  //  print_r($parentXmlNode);

  //  echo "var dump of parent node:\n";
  //  var_dump($parentXmlNode);
  
  //  echo "In UCTCF_AddItemDeclarationToCmpLib(), itemListByName is:\n";
  //  print_r($itemListByName);


  /* Add entry for new item to $itemListByName array. */
  $itemListByName[$newItemName] = array();
  $itemListByName[$newItemName]['id'] = $newItemId;

  //  echo "In UCTCF_AddItemDeclarationToCmpLib(), after adding, itemListByName is now:\n";
  //  print_r($itemListByName);

} /* end UCTCF_AddItemDeclarationToCmpLib() */


/****************************************************************
 * UCTCF_AddUserParmNameToCmpLib()
 *		Function to add a declaration for a new user parameter name
 * to CmpLib file.
 *
 * Outputs:  (Changes to CmpLib XML data structure)
 *           &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *           &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *           &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
 *           &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
 ****************************************************************/
function UCTCF_AddUserParmNameToCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                       &$altiumFoldersByGuid, &$altiumModelDataByItemRevHrid, 
                                       &$CmpLib, 
                                       &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                                       &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                                       &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
                                       &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
                                       &$newUserParmName)
{
  //  echo "Hello from UCTCF_AddUserParmNameToCmpLib()\n";

  //  echo "In UCTCF_AddUserParmNameToCmpLib(), cmpLibUserParmIdsByName is:\n";
  //  print_r($cmpLibUserParmIdsByName);

  /** Make sure user really wants to do this. **/
  UCTCF_ConfirmActionWithUser("add a new user parameter name (\"$newUserParmName\")");
  
  /* Create placeholder ID for new user parameter name. */
  $newItemId = "newGuy";

  /* Copy user parm name to new item name. */
  $newItemName = $newUserParmName;

  /* Retrieve local script constants. */
  $cNewUserParmColumnWidth = $UCTCFconstants["cNewUserParmColumnWidth"];

  /* Prepare a new XML node to be inserted. */
  $newXmlNode = new SimpleXMLElement("<TRequiredParameter/>");
  $newXmlNode->addChild('HRID', htmlspecialchars($newUserParmName));
  $newXmlNode->addAttribute('id', $newItemId);
  $newXmlNode->addAttribute('ColumnWidth', $cNewUserParmColumnWidth);

  //  echo "var_dump of newXmlNode is:\n";
  //  var_dump($newXmlNode);


  /* Configure item type. */
  $itemType = "TRequiredParameter";

  /* Call UCTCF_AddItemDeclarationToCmpLib() to add the new XML node to the data structures. */
  $itemListById = &$cmpLibUserParmNamesById;
  $itemListByName = &$cmpLibUserParmIdsByName;
  $parentXmlNode = &$CmpLib->RequiredParameters;
  UCTCF_AddItemDeclarationToCmpLib($CACFconstants, $UCTCFconstants, 
                                   $CmpLib,
                                   $itemListById, $itemListByName, $itemType,
                                   /*SimpleXMLElement*/ $newXmlNode, /*SimpleXMLElement*/ $parentXmlNode, 
                                   $newItemName, $newItemId);
  
  /* Call UCTCF_FixUpAllXmlIds() to walk the entire XML tree and fixup all ID numbers. 
   Then proceed to re-create all data structures describing user parm names, model types, and model links. */
  UCTCF_FixUpAllXmlIds($CACFconstants, $UCTCFconstants, 
                       $altiumModelDataByItemRevHrid, 
                       $CmpLib,
                       $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                       $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                       $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid);

  /* Re-extract xpath queries for all groups (database folders) and components so that we may find them later. */
  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                       $altiumFoldersByGuid, 
                                                       $CmpLib,
                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

  //  echo "RequiredParameters are now:\n";
  //  print_r($CmpLib->RequiredParameters);

  //  echo "In UCTCF_AddUserParmNameToCmpLib(), cmpLibUserParmIdsByName is now:\n";
  //  print_r($cmpLibUserParmIdsByName);
  
} /* end UCTCF_AddUserParmNameToCmpLib() */


/****************************************************************
 * UCTCF_AddModelTypeToCmpLib()
 *		Function to add a declaration for a new model type
 * to CmpLib file.
 *
 * Outputs:  (Changes to CmpLib XML data structure)
 *           &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *           &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *           &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
 *           &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
 ****************************************************************/
function UCTCF_AddModelTypeToCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                    &$altiumFoldersByGuid, &$altiumModelDataByItemRevHrid, 
                                    &$CmpLib, 
                                    &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                                    &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                                    &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
                                    &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
                                    &$newModelType)
{
  //  echo "Hello from UCTCF_AddModelTypeToCmpLib()\n";

  //  echo "In UCTCF_AddModelTypeToCmpLib(), modelTypesByType is:\n";
  //  print_r($cmpLibModelTypesByType);
  
  /** Make sure user really wants to do this. **/
  UCTCF_ConfirmActionWithUser("add a new model type (\"$newModelType\")");

  /* Create placeholder ID for new model type. */
  $newItemId = "newGuy";

  /* Copy model type to new item name. */
  $newItemName = $newModelType;

  /* Retrieve local script constants. */
  $cNewModelTypeColumnWidth = $UCTCFconstants["cNewModelTypeColumnWidth"];

  /* Prepare a new XML node to be inserted. */
  $newXmlNode = new SimpleXMLElement("<TRequiredModel/>");
  $newXmlNode->addChild('HRID', htmlspecialchars($newModelType));
  $newXmlNode->addChild('ModelKind', htmlspecialchars($newModelType));
  $newXmlNode->addAttribute('id', $newItemId);
  $newXmlNode->addAttribute('ColumnWidth', $cNewModelTypeColumnWidth);

  /* Configure item type. */
  $itemType = "TRequiredModel";

  /* Call UCTCF_AddItemDeclarationToCmpLib() to add the new XML node to the data structures. */
  $itemListById = &$cmpLibModelTypesById;
  $itemListByName = &$cmpLibModelTypesByType;
  $parentXmlNode = &$CmpLib->RequiredModels;
  UCTCF_AddItemDeclarationToCmpLib($CACFconstants, $UCTCFconstants, 
                                   $CmpLib,
                                   $itemListById, $itemListByName, $itemType,
                                   /*SimpleXMLElement*/ $newXmlNode, /*SimpleXMLElement*/ $parentXmlNode, 
                                   $newItemName, $newItemId);
  
  /* Call UCTCF_FixUpAllXmlIds() to walk the entire XML tree and fixup all ID numbers. 
   Then proceed to re-create all data structures describing user parm names, model types, and model links. */
  UCTCF_FixUpAllXmlIds($CACFconstants, $UCTCFconstants, 
                       $altiumModelDataByItemRevHrid, 
                       $CmpLib,
                       $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                       $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                       $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid);

  /* Re-extract xpath queries for all groups (database folders) and components so that we may find them later. */
  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                       $altiumFoldersByGuid, 
                                                       $CmpLib,
                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

} /* end UCTCF_AddModelTypeToCmpLib() */


/****************************************************************
 * UCTCF_AddModelLinkToCmpLib()
 *		Function to add a declaration for a new model link
 * to CmpLib file.
 *
 * Outputs:  (Changes to CmpLib XML data structure)
 *           &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *           &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *           &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
 *           &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
 ****************************************************************/
function UCTCF_AddModelLinkToCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                    &$altiumFoldersByGuid, &$altiumModelDataByItemRevHrid, 
                                    &$CmpLib, 
                                    &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                                    &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                                    &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
                                    &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
                                    $newModelLinkHrid)
{
  /* Retrieve necessary global constants. */
  $revSep = 										$CACFconstants["revSep"];

  //  echo "Hello from UCTCF_AddModelLinkToCmpLib()\n";

  //  echo "In UCTCF_AddModelLinkToCmpLib(), modelLinksByItemRevHrid is:\n";
  //  print_r($cmpLibModelLinksByItemRevHrid);

  /** Make sure user really wants to do this. **/
  UCTCF_ConfirmActionWithUser("add a new model link (\"$newModelLinkHrid\")");


  /* Lookup all the missing information, starting from $newModelLinkHrid. */
  $VaultGuid   = $UCTCFconstants["defaultVaultGuid"];
  $ItemGuid    = $altiumModelDataByItemRevHrid[$newModelLinkHrid]["ITEMGUID"];
  $ItemRevGuid = $altiumModelDataByItemRevHrid[$newModelLinkHrid]["GUID"];
  $RevisionID  = preg_replace("/$revSep/", "", $altiumModelDataByItemRevHrid[$newModelLinkHrid]["REVISIONID"]);
  $ModelType   = $altiumModelDataByItemRevHrid[$newModelLinkHrid]["MODELTYPE"];

  /* Create placeholder ID for new model type. */
  $newItemId = "newGuy";

  /* Copy model link to new item name. */
  $newItemName = $newModelLinkHrid;

  /* Prepare a new XML node to be inserted. */
  $newXmlNode = new SimpleXMLElement("<TModelLink/>");
  $newXmlNode->addChild('HRID',         htmlspecialchars($newModelLinkHrid));
  $newXmlNode->addChild('VaultGUID',    htmlspecialchars($VaultGuid));
  $newXmlNode->addChild('ItemGUID',     htmlspecialchars($ItemGuid));
  $newXmlNode->addChild('ModelKind',    htmlspecialchars($ModelType));
  $newXmlNode->addChild('RevisionGUID', htmlspecialchars($ItemRevGuid));
  $newXmlNode->addChild('RevisionId',   htmlspecialchars($RevisionID));
  $newXmlNode->addAttribute('id', $newItemId);

  /* Configure item type. */
  $itemType = "TModelLink";

  /* Call UCTCF_AddItemDeclarationToCmpLib() to add the new XML node to the data structures. */
  $itemListById = &$cmpLibModelLinksById;
  $itemListByName = &$cmpLibModelLinksByItemRevHrid;
  $parentXmlNode = &$CmpLib->ModelLinks;
  UCTCF_AddItemDeclarationToCmpLib($CACFconstants, $UCTCFconstants, 
                                   $CmpLib,
                                   $itemListById, $itemListByName, $itemType,
                                   /*SimpleXMLElement*/ $newXmlNode, /*SimpleXMLElement*/ $parentXmlNode, 
                                   $newItemName, $newItemId);

  /* Call UCTCF_FixUpAllXmlIds() to walk the entire XML tree and fixup all ID numbers. 
   Then proceed to re-create all data structures describing user parm names, model types, and model links. */
  UCTCF_FixUpAllXmlIds($CACFconstants, $UCTCFconstants, 
                       $altiumModelDataByItemRevHrid, 
                       $CmpLib,
                       $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                       $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                       $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid);

  /* Re-extract xpath queries for all groups (database folders) and components so that we may find them later. */
  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                       $altiumFoldersByGuid, 
                                                       $CmpLib,
                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

} /* end UCTCF_AddModelLinkToCmpLib() */


/****************************************************************
 * UCTCF_UpdateCmpLibFromExcelData()
 *		Function to update components described in CmpLib file
 * with respect to information from Excel file.
 *
 * Outputs:  (Changes to CmpLib XML data structure &$CmpLib),
 *           &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *           &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *           &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
 *           &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
 *           &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid,
 ****************************************************************/
function UCTCF_UpdateCmpLibFromExcelData(&$CACFconstants, &$UCTCFconstants, 
                                         &$altiumFoldersByGuid, &$altiumItemSysParmValuesByGuid, &$altiumModelDataByItemRevHrid,
                                         &$CmpLib, 
                                         &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                                         &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                                         &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
                                         &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
                                         &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid,
                                         &$excelUserParmsByItemHrid, &$excelSysParmsByItemHrid)
{
  /* Retrieve necessary global constants. */
  $reserveForThisManyModels = 						$CACFconstants["reserveForThisManyModels"];

  echo "Hello from UCTCF_UpdateCmpLibFromExcelData()\n";

  //  echo "cmpLibModelLinksByItemRevHrid is:\n";
  //  print_r($cmpLibModelLinksByItemRevHrid);

  //  echo "cmpLibCompByItemHrid is:\n";
  //  print_r($cmpLibCompByItemHrid);

  //  echo "cmpLibGroupXpathByPath is:\n";
  //  print_r($cmpLibGroupXpathByPath);

  /* Loop over all the components defined in the Excel file. */
  foreach ($excelSysParmsByItemHrid as $ItemHrid => $val)
    {
      //      echo "In UCTCF_UpdateCmpLibFromExcelData(), about to create CmpLib component for ItemHrid \"$ItemHrid\"\n";
      //      print_r($excelSysParmsByItemHrid[$ItemHrid]);

      /* Extract the path to this component. */
      $path = $excelSysParmsByItemHrid[$ItemHrid][CACF_AlterAltiumSysParmName("COMPONENTPATH")];
      //      echo "path is \"$path\"\n";

      /* Extract the comment of this component. */
      $comment = $excelSysParmsByItemHrid[$ItemHrid][CACF_AlterAltiumSysParmName("COMMENT")];
      //      echo "comment is \"$comment\"\n";

      /* Extract the description of this component. */
      $description = $excelSysParmsByItemHrid[$ItemHrid][CACF_AlterAltiumSysParmName("DESCRIPTION")];
      //      echo "description is \"$description\"\n";

      /* See if this is a new component (to the CmpLib file at least). */
      if (!isset($cmpLibCompByItemHrid[$ItemHrid]))
        {
          echo "Need to create new component at path \"$path\" and ItemHrid \"$ItemHrid\"!\n";

          /* See if we need to create the group (folder) for this component to live in. */
          if (!isset($cmpLibGroupXpathByPath[$path]))
            {

              echo "Need to create path \"$path\"!\n";

              /** Drill down until we find a subpath of the desired path that already exists. **/

              /* Initialize 2 variables:  1 for the subpath that already exists, one for the subpath we need to create. */
              $pathToCreate = "";
              $pathCurrent = $path;
              echo "pathCurrent is \"$pathCurrent\".\n";                  

              /* Loop until we find some subpath of our desired path to exist. */
              $subPathFound = false;
              while (!$subPathFound)
                {
                  /* Extract the leaf subdir from the working path. */
                  /* FIXME:  Hardcodes path separator! */
                  $pathLeaf    = preg_replace('/\\\\$/', "", $pathCurrent);
                  $pathLeaf    = preg_replace('/^.*\\\\/', "", $pathLeaf);
                  $pathLeaf    = $pathLeaf . '\\';

                  /* Strip off the leaf subdir from the working path. */
                  $pathCurrent = preg_replace('/[^\\\\]+\\\\$/', "", $pathCurrent);

                  /* Add leaf subdir to list of subdirs that we will need to create. */
                  $pathToCreate = $pathLeaf . $pathToCreate;

                  echo "pathLeaf is        \"$pathLeaf\".\n";
                  echo "pathCurrent is now \"$pathCurrent\".\n";
                  echo "pathToCreate is    \"$pathToCreate\".\n\n";

                  /* Sanity check. */
                  if ($pathCurrent == "")
                    my_die("Could not find any part of this path already defined!");

                  /* See if the new current path exists. */
                  $subPathFound = isset($cmpLibGroupXpathByPath[$pathCurrent]);

                } /* endwhile */


              /** Retrieve XML node for the leaf-most subdir of the path that already exists. **/
              /* Retrieve the xpath query that will get us to the XML node for this path. */
              $xpathQuery = $cmpLibGroupXpathByPath[$pathCurrent];
              
              /* Run xpath query to get XML node for this path. */
              UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                             $resultNode);


              /** Work our way back up until we've created all the leaf subdirs we need to. **/
              echo "Proceeding to create subdirs...\n\n";

              /* Loop until we've created all the leaf subdirs we need to create. */
              $doneCreating = false;
              while (!$doneCreating)
                {
                  /* Extract the next leaf subdir that we need to create. */
                  $pathLeaf    = preg_replace('/\\\\.*$/', "", $pathToCreate);

                  /* Strip off the leaf subdir from the working path. */
                  $pathToCreate = preg_replace('/^[^\\\\]+\\\\/', "", $pathToCreate);

                  /** Create new XML <TGroup> node. **/
                  $newXmlNode = new SimpleXMLElementEx("<TGroup/>");
                  $newXmlNode->addAttribute('id', 'foo');					// This will get fixed up later!
                  $newXmlNode->addAttribute('StateIndex', 0);				// This will get fixed up later!
                  $newXmlNode->addAttribute('Collapsed', "true");			// This will get fixed up later!

                  $newXmlNode->addChild('HRID', htmlspecialchars($pathLeaf));

                  $newXmlNode2 = new SimpleXMLElementEx("<ParentGroup/>");
                  $newXmlNode2->addAttribute('href', '#foo');				// This will get fixed up later!
                  $newXmlNode->appendNode($newXmlNode2);

                  $newXmlNode->addChild('Parameters', '');
                  $newXmlNode->addChild('ModelChoices', '');

                  $newXmlNode3 = new SimpleXMLElementEx("<ComponentSet/>");
                  $newXmlNode3->addAttribute('href', '#0');					// This will get fixed up later!
                  $newXmlNode->appendNode($newXmlNode3);

                  $newXmlNode->addChild('Groups', '');
                  $newXmlNode->addChild('ComponentDefinitions', '');

                  /* Add newXmlNode to exising XML tree. */
                  $resultNode->Groups->appendNode($newXmlNode);

                  /* Compute the new current path. */
                  $pathCurrent = $pathCurrent . $pathLeaf . '\\';

                  echo "pathLeaf is         \"$pathLeaf\".\n";               
                  echo "pathCurrent is now  \"$pathCurrent\".\n";
                  echo "pathToCreate is now \"$pathToCreate\".\n\n";

                  /** Re-index ID's and parent group ID's in XML file, now that we have added a new group. **/
                  /* Note that we don't have to re-index for user parameter names, model types, or model links! */
                  /* Set the starting ID number to be 0. */
                  $currIdNum = 0;
                  $parentIdNum = 0;
          
                  /* Call UCTCF_FixUpXmlIds() to do most of the real work. */
                  /* FIXME:  Should we use the FixUpAllXmlIds() function instead?? */
                  UCTCF_FixUpXmlIds($CACFconstants, $UCTCFconstants, 
                                    $CmpLib,
                                    $cmpLibUserParmNamesById, $cmpLibModelTypesById, $cmpLibModelLinksById,
                                    $currIdNum, $parentIdNum, 
                                    $CmpLib /*$xmlNode*/);

                  /* Re-extract xpath queries for all groups (database folders) and components so that we may find them later. */
                  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                                       $altiumFoldersByGuid, 
                                                                       $CmpLib,
                                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

                  /* Retrieve the xpath query that will get us to the XML node for this path. */
                  $xpathQuery = $cmpLibGroupXpathByPath[$pathCurrent];
                  
                  /* Run xpath query to get XML node for this path. */
                  UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                                 $resultNode);

                  /* Evaluate loop termination condition. */
                  if ($pathToCreate == "")
                    $doneCreating = true;

                } /* endwhile */

            } /* endif need to create path */


          /** Proceed to create component. **/

          /* Retrieve the xpath query that will get us to the XML node for this path. */
          $xpathQuery = $cmpLibGroupXpathByPath[$path];

          /* Run xpath query to get XML node for this path. */
          UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                         $resultNode);

          //              echo "After xpath query for group node, resultNode is:\n";
          //              print_r($resultNode);

          /* See if there is a <ComponentDefinitions> tag under this group. */
          if (isset($resultNode->ComponentDefinitions))
            {
              echo "Path \"$path\" already has a <ComponentDefinitions> tag under it.  Yay!\n";

              /* Create a new <TComponentDefinition StateIndex="0"> tag. */
              $newXmlNode = new SimpleXMLElementEx("<TComponentDefinition/>");
              $newXmlNode->addAttribute('StateIndex', 0);				// This will get fixed up later!
              $newXmlNode->addChild('HRID', htmlspecialchars($comment));

              $newXmlNode2 = new SimpleXMLElementEx("<ParentGroup/>");
              $newXmlNode2->addAttribute('href', '#foo');				// This will get fixed up later!
              $newXmlNode->appendNode($newXmlNode2);

              $newXmlNode->addChild('Parameters', '');					// This will get fixed up later!
              $newXmlNode->addChild('ModelChoices', '');				// This will get fixed up later!
              $newXmlNode->addChild('ItemHRID', htmlspecialchars($ItemHrid));

              /* No <GUID>, <RevisionGUID>, or <RevisionId> tags for a new component! */

              //                  echo "newXmlNode is:\n";
              //                  print_r($newXmlNode);

              /* Add newXmlNode to exising XML tree. */
              $resultNode->ComponentDefinitions->appendNode($newXmlNode);
                  
            } /* endif */

          /* Else we do not have a <ComponentDefinitions> tag under this group. */
          /* This should never actually happen. */
          else
            {
              echo "About to abort.  Node is:\n";
              print_r($resultNode);

              echo "cmpLibGroupXpathByPath is:\n";
              print_r($cmpLibGroupXpathByPath);

              my_die("CmpLib file does not have a <ComponentDefinitions> tag under path \"$path\"!");

            } /* endelse */


          /* Retrieve the xpath query that will get us to the XML node for this path. */
          $xpathQuery = $cmpLibGroupXpathByPath[$path];
          
          /* Run xpath query to get XML node for this path. */
          UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                         $resultNode);
          //          echo "In UCTCF_UpdateCmpLibFromExcelData(), resultNode is:\n";
          //          print_r($resultNode);


          /** Find our newly created component and get a reference to it. **/

          /* Add to our xpath query to find a component with the specified ItemHrid. */
          $xpathQuery = $xpathQuery."/ComponentDefinitions/TComponentDefinition[ItemHRID=\"$ItemHrid\"]";
          //          echo "xpathQuery is \"$xpathQuery\"\n";

          /* Run xpath query to get XML node for this new component. */
          UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                         $resultNode);
          //          echo "In UCTCF_UpdateCmpLibFromExcelData(), resultNode is:\n";
          //          print_r($resultNode);

          /* Store a reference to this node as $xmlNode for use in code below. */
          $xmlNode = &$resultNode;


          /** Re-index ID's and parent group ID's in XML file, now that we have added a new component. **/
          /* Note that we don't have to re-index for user parameter names, model types, or model links! */
          /* Set the starting ID number to be 0. */
          $currIdNum = 0;
          $parentIdNum = 0;
          
          /* Call UCTCF_FixUpXmlIds() to do most of the real work. */
          UCTCF_FixUpXmlIds($CACFconstants, $UCTCFconstants, 
                            $CmpLib,
                            $cmpLibUserParmNamesById, $cmpLibModelTypesById, $cmpLibModelLinksById,
                            $currIdNum, $parentIdNum, 
                            $CmpLib /*$xmlNode*/);

        } /* endif is this a new component */

      /* Else there is an existing Vault component with this path and comment. */
      else
        {
          /* Get the path that was stored for the CmpLib version of this component. */
          $cmpLibPath = $cmpLibCompByItemHrid[$ItemHrid]["path"];

          /* Make sure that the path that the Excel file wants is the same as the current
           path in the CmpLib file. */
          /* Note:  We don't currently have any mechanism in this script to move a Vault
           component from one location to another.  This would have to be done by manually
           moving it using the Vault Explorer GUI.  Then nuke that component(s) from
           CmpLib file.  Then manually (presumably through Altium GUI) update the CmpLib 
           file to contain that new path.  Then finally re-add these components to
           CmpLib file using this script.  And pray that it releases successfully to Vault! */
          if ($cmpLibPath != $path)
            my_die("Sorry, but component paths in Excel file must match component paths in CmpLib file.  \"$path\" != \"$cmpLibPath\"!\n");


          /* Retrieve the xpath query that will get us to the XML node for this component. */
          $xpathQuery = $cmpLibCompByItemHrid[$ItemHrid]["xpath"];
          
          /* Run xpath query to get XML node for this component. */
          UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                         $resultNode);
          $xmlNode = &$resultNode;
          //      echo "xmlNode is:\n";
          //      print_r($xmlNode);
          
        } /* endelse */

      /** Handle all (mostly "user") parameters described under the XML file <Parameters> tag. **/
      /* Create an array to hold all the parameters that will be re-described.
       This includes all the parameters we have stored as "user" parameters, plus
       the comment and desciptions fields, which were both stored as "system" paramters. */
      $parms = array();

      /* Copy two "system" paramters to $parms. */
      $parms["Comment"] = $comment;
      $parms["Description"] = $description;

      /* Loop over all Excel parameters that were stored as "user" parameters. */
      foreach ($excelUserParmsByItemHrid[$ItemHrid] as $userParmName => $val3)
        {
          /* Un-alter the name of a few user parameters before getting ready to re-write .CmpLib file. */
          $userParmNameUnaltered = CACF_UnAlterAltiumUserParmName($userParmName);
          
          /* Copy this user parameter name and value to $parms. */
          $parms[$userParmNameUnaltered] = $excelUserParmsByItemHrid[$ItemHrid][$userParmName];
          
        }

      /* Sort the list of parms attached to this component. */
      /* Note:  Here we are using uksort() (with strcasecmp() as comparison func) to try to emulate what Altium does with ordering CmpLib href id references. */
      $rc = uksort($parms, "strcasecmp");
      if ($rc == FALSE) my_die("uksort() failed!");

      //      echo "parms is:\n";
      //      print_r($parms);


      /** Handle all (mostly) user parameters described under the XML file <Parameters> tag. **/
      /* Replace the existing <Parameters> tag with a new XML node. */
      /* TODO:  This operation almost certainly results in a memory leak! */
      $xmlNode->Parameters = new SimpleXMLElement("<Parameters/>");

      /* Loop over all the parameters that we need to create in XML land. */
      $parmNum = 0;
      foreach ($parms as $parmName => $parmVal)
        {
          //          echo "parmName is \"$parmName\".\n";

          /* Special handing for "Comment" parameter. */
          if ($parmName == "Comment")
            {
              /* This script currently requires that the CmpLib component HRID field
               to always be the same as the "Comment" parameter.  In the case where
               the Excel version of "Comment" has changed from the CmpLib version 
               of "Comment", then the component HRID field (which was initially set to
               the CmpLib version of Comment, will now be wrong.  Fix it. */
              $xmlNode->HRID = $parmVal;

            } /* endif */

          /* See if we need to add a new user parameter name declaration to CmpLib file. */
          if (!isset($cmpLibUserParmIdsByName[$parmName]))
            {
              //              my_die('Unable to find desired user parameter name "' . $parmName . '" in list of defined user parameter names!');
          
              /* Call UCTCF_AddUserParmNameToCmpLib() to add a new user parameter name to CmpLib file. */
              $newUserParmName = $parmName;
              UCTCF_AddUserParmNameToCmpLib($CACFconstants, $UCTCFconstants, 
                                            $altiumFoldersByGuid, $altiumModelDataByItemRevHrid, 
                                            $CmpLib, 
                                            $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                                            $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                                            $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid,
                                            $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid,
                                            $newUserParmName);
            } /* endif */


          /** Proceed to create tags in XML data structure describing this user parameter. **/
          /* Create a new <TParameter> tag in XML data structure. */
          $xmlNode->Parameters->addChild('TParameter', '');

          /* Create a new <RequiredParameter> tag as a child of <TParameter>. */
          $xmlNode->Parameters->TParameter[$parmNum]->addChild('RequiredParameter', '');

          /* Sanity check. */
          if (!isset($cmpLibUserParmIdsByName[$parmName]['id']))
            my_die("In UCTCF_UpdateCmpLibFromExcelData(), didn't find ID for userParmName \"$parmName\"!");
              
          /* Lookup the parameter ID in the previously stored list of user parameter names. */
          $id = $cmpLibUserParmIdsByName[$parmName]['id'];
          //          echo "In add required parameter, id is \"$id\".\n";

          /* Add an attribute to the <RequiredParameter> tag specifying the ID number of the user parameter name. */
          $xmlNode->Parameters->TParameter[$parmNum]->RequiredParameter->addAttribute('href', UCTCF_ConvertIdToHref($id));

//          echo "parmVal is \"$parmVal\", type is " . gettype($parmVal) . "\n";

          /* If we have a non-null parameter value, then create a new <Value> tag as a child of <TParameter>. */
          if (! ( (is_null($parmVal)) || ( (gettype($parmVal) == "string") && ($parmVal == "") ) ))
            $xmlNode->Parameters->TParameter[$parmNum]->addChild('Value', htmlspecialchars($parmVal));
//          else
//            echo "Suppressing this value!\n";

          /* Increment parameter number counter. */
          $parmNum++;
        }
          
      //      echo "After redoing <Parameters>, xmlNode is now:\n";
      //      print_r($xmlNode);


      /** Handle all model linking described under the XML file <ModelChoices> tag. **/
      /* Replace the existing <ModelChoices> tag with a new XML node. */
      /* TODO:  This operation almost certainly results in a memory leak! */
      $xmlNode->ModelChoices = new SimpleXMLElement("<ModelChoices/>");

      /* Loop over all the models that could possibly be attached to this component in Excel land. */
      for ($modelNum = 0; $modelNum < $reserveForThisManyModels; $modelNum++)
        {
          /* Attempt to retrieve model information (if any exists). */
          $sysParms = &$excelSysParmsByItemHrid[$ItemHrid];
          CACF_GetModelInfoFromSysParms($CACFconstants, 
                                        $sysParms, 
                                        $modelNum, 
                                        $modelType, $modelHRID, $modelPath, $modelLib);

          /* See if we got model info. */
          if ($modelType != "") 
            {
              /* See if we need to add a new model type declaration to CmpLib file. */
              if (!isset($cmpLibModelTypesByType[$modelType]))
                {
                  /* Call UCTCF_AddModelTypeToCmpLib() to add a new model type declaration to CmpLib file. */
                  $newModelType = $modelType;
                  UCTCF_AddModelTypeToCmpLib($CACFconstants, $UCTCFconstants, 
                                             $altiumFoldersByGuid, $altiumModelDataByItemRevHrid, 
                                             $CmpLib, 
                                             $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                                             $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                                             $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid,
                                             $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid,
                                             $newModelType);
                } /* endif */


              /* See if we need to add a new model link declaration to CmpLib file. */
              if (!isset($cmpLibModelLinksByItemRevHrid[$modelHRID]))
                {
                  //                my_die('Unable to find desired model HRID "' . $modelHRID . '" in list of defined models!');
          
                  /* Call UCTCF_AddModelLinkToCmpLib() to add a new model link declaration to CmpLib file. */
                  $newModelLinkHrid = $modelHRID;
                  UCTCF_AddModelLinkToCmpLib($CACFconstants, $UCTCFconstants, 
                                             $altiumFoldersByGuid, $altiumModelDataByItemRevHrid, 
                                             $CmpLib, 
                                             $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                                             $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                                             $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid,
                                             $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid,
                                             $newModelLinkHrid);
                } /* endif */


              /** Proceed to create tags in XML data structure describing this model. **/
              /* Create a new <TModelChoice> tag in XML data structure. */
              $xmlNode->ModelChoices->addChild('TModelChoice', '');

              /* Create a new <RequiredModel> tag as a child of <TModel>. */
              $xmlNode->ModelChoices->TModelChoice[$modelNum]->addChild('RequiredModel', '');

              /* Sanity check. */
              if (!isset($cmpLibModelTypesByType[$modelType]['id']))
                my_die("In UCTCF_UpdateCmpLibFromExcelData(), didn't find ID for modelType \"$modelType\"!");

              /* Lookup the model type ID in the previously stored list of model types. */
              $id = $cmpLibModelTypesByType[$modelType]['id'];
              //              echo "In add model type, id is \"$id\".\n";
              
              /* Add an attribute to the <RequiredModel> tag specifying the ID number of the model type. */
              $xmlNode->ModelChoices->TModelChoice[$modelNum]->RequiredModel->addAttribute('href', UCTCF_ConvertIdToHref($id));
              
              /* Create a new <ModelLink> tag as a child of <TModel>. */
              $xmlNode->ModelChoices->TModelChoice[$modelNum]->addChild('ModelLink', '');

              /* Sanity check. */
              if (!isset($cmpLibModelLinksByItemRevHrid[$modelHRID]["id"]))
                my_die("In UCTCF_UpdateCmpLibFromExcelData(), didn't find ID for modelHRID \"$modelHRID\"!");
              
              /* Lookup the model link ID in the previously stored list of model links. */
              $id = $cmpLibModelLinksByItemRevHrid[$modelHRID]["id"];
              //              echo "Looking for modelHRID \"$modelHRID\".\n";
              //              echo "In add model link, id is \"$id\".\n";
              
              /* Add an attribute to the <ModelLink> tag specifying the ID number of the model link. */
              $xmlNode->ModelChoices->TModelChoice[$modelNum]->ModelLink->addAttribute('href', UCTCF_ConvertIdToHref($id));
              
            } /* endif */

        } /* endfor */

      //      echo "xmlNode is now:\n";
      //      print_r($xmlNode);

    } /* end foreach component */

  //  echo "In UCTCF_UpdateCmpLibFromExcelData(), cmpLibSysParmsByGuid is:\n";
  //  print_r($cmpLibSysParmsByGuid);

  //  echo "In UCTCF_UpdateCmpLibFromExcelData(), after adding and updating components, CmpLib is now:\n";
  //  print_r($CmpLib);


  /* Re-extract all components from XML file data structures. */
  UCTCF_ExtractAllComponentParmsFromCmpLib($CACFconstants, $UCTCFconstants, 
                                           $altiumItemsByHrid, $altiumItemRevsByHrid,
                                           $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, 
                                           $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName, $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                           $CmpLib,
                                           $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibNumNewComps, $cmpLibDidCorrection);
  
  /* Re-extract xpath queries for all groups (database folders) and components so that we may find them later. */
  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                       $altiumFoldersByGuid, 
                                                       $CmpLib,
                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

//  echo "In UCTCF_UpdateCmpLibFromExcelData(), cmpLibSysParmsByGuid is now:\n";
//  print_r($cmpLibSysParmsByGuid);

  //  echo "In UCTCF_UpdateCmpLibFromExcelData(), after adding and updating components, cmpLibCompByItemHrid is:\n";
  //  print_r($cmpLibCompByItemHrid);

  //  echo "Leaving UCTCF_UpdateCmpLibFromExcelData(), altiumItemSysParmValuesByGuid is:\n";
  //  print_r($altiumItemSysParmValuesByGuid);

} /* end UCTCF_UpdateCmpLibFromExcelData() */


/****************************************************************
 * UCTCF_CompareCmpLibComponentToVault()
 *		Function to compare CmpLib component data to Vault data.
 *
 * Outputs:  &$compNeedsUpdate
 ****************************************************************/
function UCTCF_CompareCmpLibComponentToVault(&$CACFconstants, &$UCTCFconstants, 
                                             &$altiumItemsByGuid, &$altiumItemRevsByGuid, 
                                             &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid,
                                             &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid,
                                             $ItemRevGuid, $doUserOrSys,
                                             &$compNeedsUpdate)
{
  /* See if we're supposed to handle user or system parameters this time around. */
  if ($doUserOrSys == "user")
    {
      $vaultSomeParms = &$altiumItemUserParmValuesByGuid;
      $cmpLibSomeParms = &$cmpLibUserParmsByGuid;
    }
  else if ($doUserOrSys == "sys")
    {
      $vaultSomeParms = &$altiumItemSysParmValuesByGuid;
      $cmpLibSomeParms = &$cmpLibSysParmsByGuid;
    }

  /* Look up the ItemHrid of this component. */
  $ItemGuid = $altiumItemRevsByGuid[$ItemRevGuid]["ITEMGUID"];
  $ItemHrid = $altiumItemsByGuid[$ItemGuid]["HRID"];

  /** Check for component parameters in the CmpLib version of the component that are new or different with respect to Vault. **/
  /* Loop over all parameters in the CmpLib component. */
  foreach ($cmpLibSomeParms[$ItemRevGuid] as $someParmName => $val2)
    {
      //      $vaultSomeParms[$ItemRevGuid]['foo'] = 'bar';

      /* Explicitly stringify cmpLibSomeParmValue for display purposes. */
      $cmpLibSomeParmValue = (string) $cmpLibSomeParms[$ItemRevGuid][$someParmName];

      //      echo "In loop over parms in CmpLib, $doUserOrSys"."ParmName is \"$someParmName\", cmpLibSomeParmValue is \"$cmpLibSomeParmValue\".\n";

      /* Check for missing parameters in Vault. */
      if (!isset($vaultSomeParms[$ItemRevGuid][$someParmName]))
        {
          echo "Component \"$ItemHrid\" in Vault is missing $doUserOrSys"."Parm \"$someParmName\"!  New value will be \"$cmpLibSomeParmValue\".\n";

          /* Flag that this component needs to be updated in Vault. */
          $compNeedsUpdate = true;

        } /* endif */

      /* Else both CmpLib component and Vault component have this parm. */
      else
        {
          /* Explicitly stringify vaultSomeParmValue for display purposes. */
          $vaultSomeParmValue = (string) $vaultSomeParms[$ItemRevGuid][$someParmName];

          /* Check for differing parameters in Vault. */
          if ($cmpLibSomeParmValue != $vaultSomeParms[$ItemRevGuid][$someParmName])
            {
                  
              /* See if this is a parameter that is allowed to differ between CmpLib and Vault data. */
              /* NOTE:  This is company specific! */
              /* TODO:  Revisit if we should still have this exception for a company part number that is set to ItemHRID, rather than ItemRevHrid! */
              if ($someParmName == "TRT Part Number")
                {
                  echo "Component \"$ItemHrid\" in Vault has outdated $doUserOrSys"."Parm name=value:  \"$someParmName\" = \"$vaultSomeParmValue\".  However, we will ignore this parameter and not update to \"$cmpLibSomeParmValue\".\n";

                } /* endif */
              
              /* Else the normal case where we care that this parameter value differs. */
              else
                {
                  echo "Component \"$ItemHrid\" in Vault has outdated $doUserOrSys"."Parm name=value:  \"$someParmName\" = \"$vaultSomeParmValue\".  Will be updated to \"$cmpLibSomeParmValue\"!\n";
                  
                  /* Flag that this component needs to be updated in Vault. */
                  $compNeedsUpdate = true;

                } /* endelse */
              
            } /* endif */

        } /* endelse */

    } /* end foreach parameters in CmpLib component */

      
  /** Check for component parameters in the Vault version of the component that no longer exist in the CmpLib version of the component. **/
  /* Loop over all parameters in the Vault component. */
  foreach ($vaultSomeParms[$ItemRevGuid] as $someParmName => $val2)
    {
      /* Explicitly stringify vaultSomeParmValue for display purposes. */
      $vaultSomeParmValue = (string) $vaultSomeParms[$ItemRevGuid][$someParmName];

      //      echo "In loop over parms in Vault, $doUserOrSys"."ParmName is \"$someParmName\", vaultSomeParmValue is \"$vaultSomeParmValue\".\n";

      /* Check for missing parameters in Vault. */
      /* Note:  We must also add a check against Vault component parm value being null.
       In this case, the CmpLib will not have an entry for that parm. */
      if ( ($vaultSomeParmValue != "") && (!isset($cmpLibSomeParms[$ItemRevGuid][$someParmName])) )
        {
          echo "Component \"$ItemHrid\" in Vault will have its $doUserOrSys"."Parm \"$someParmName\" deleted!  (Previous value = \"$vaultSomeParmValue\").\n";

          /* Flag that this component needs to be updated in Vault. */
          $compNeedsUpdate = true;
          
        } /* endif */

    } /* end foreach parameters in Vault component */

} /* end UCTCF_CompareCmpLibComponentToVault() */


/****************************************************************
 * UCTCF_CompareAllCmpLibComponentsToVault()
 *		Function to compare all CmpLib components to their counterparts in the Vault.
 *
 * Outputs:  $changedCompsByItemRevGuid
 ****************************************************************/
function UCTCF_CompareAllCmpLibComponentsToVault(&$CACFconstants, &$UCTCFconstants, 
                                                 &$altiumItemsByGuid, &$altiumItemRevsByGuid, 
                                                 &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid,
                                                 &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid,
                                                 &$changedCompsByItemRevGuid)
{
  echo "Hello from UCTCF_CompareAllCmpLibComponentsToVault()\n";
  //  echo "cmpLibUserParmsByGuid is:\n";
  //  print_r($cmpLibUserParmsByGuid);

  /* Allocate a new array to store all the components that need to be updated in Vault. */
  $changedCompsByItemRevGuid = array();

  /* Loop over all components defined in the CmpLib file. */
  foreach ($cmpLibSysParmsByGuid as $ItemRevGuid => $val)
    {
      /* See if this CmpLib component is new (eg. does not exist in the Vault). */
      if ( (!isset($altiumItemUserParmValuesByGuid[$ItemRevGuid])) || (!isset($altiumItemSysParmValuesByGuid[$ItemRevGuid])) )
        {
          //          my_die("ItemRevGuid \"$ItemRevGuid\" does not appear to exist in the Vault!");
          
          /* Get the ItemRevHrid for this CmpLib component. */
          $ItemHrid = $cmpLibSysParmsByGuid[$ItemRevGuid][CACF_AlterAltiumSysParmName("ITEMHRID")];

          echo "\nFlagging component \"$ItemHrid\" ($ItemRevGuid) for addition to Vault.\n";
          $changedCompsByItemRevGuid[$ItemRevGuid] = array();
          $changedCompsByItemRevGuid[$ItemRevGuid]["ITEMHRID"] = $ItemHrid;
          $changedCompsByItemRevGuid[$ItemRevGuid]["XMLNODE"] = $ItemHrid;

        } /* endif */

      /* Else this component already exists in the Vault. */
      else
        {
          /* Look up the ItemHrid of this component. */
          $ItemGuid = $altiumItemRevsByGuid[$ItemRevGuid]["ITEMGUID"];
          $ItemHrid = $altiumItemsByGuid[$ItemGuid]["HRID"];
      
          echo "\nComparing component \"$ItemHrid\"...\n";

          /* Flag that this component is not yet known to need updating in Vault. */
          $compNeedsUpdate = false;
      
          /* Compare all sys parameters for this CmpLib component to those in the Vault. */
          $doUserOrSys = "sys";
          UCTCF_CompareCmpLibComponentToVault($CACFconstants, $UCTCFconstants, 
                                              $altiumItemsByGuid, $altiumItemRevsByGuid, 
                                              $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid,
                                              $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid,
                                              $ItemRevGuid, $doUserOrSys,
                                              $compNeedsUpdate);

          /* Compare all user parameters for this CmpLib component to those in the Vault. */
          $doUserOrSys = "user";
          UCTCF_CompareCmpLibComponentToVault($CACFconstants, $UCTCFconstants, 
                                              $altiumItemsByGuid, $altiumItemRevsByGuid, 
                                              $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid,
                                              $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid,
                                              $ItemRevGuid, $doUserOrSys,
                                              $compNeedsUpdate);

          /* If needed, add this component to a list of components that need to be updated in Vault. */
          if ($compNeedsUpdate)
            {
              echo "Flagging component \"$ItemHrid\" ($ItemRevGuid) for update in Vault.\n";
              $changedCompsByItemRevGuid[$ItemRevGuid] = 1;
          
            } /* endif */

        } /* endelse */
      
    } /* end foreach */

  //  echo "Leaving UCTCF_CompareAllCmpLibComponentsToVault(), altiumItemSysParmValuesByGuid is:\n";
  //  print_r($altiumItemSysParmValuesByGuid);

} /* end UCTCF_CompareAllCmpLibComponentsToVault() */


/****************************************************************
 * UCTCF_EnableComponentsForUpdateInCmpLib()
 *		Enable all components whose CmpLib data differs from Vault
 * data for update.  This involves setting the XML "state" attribute
 * for relevant components to "2" and the state attribute for all 
 * relevant parent directories to "1".
 *
 * Outputs:  (Changes to CmpLib XML data structure &$CmpLib),
 ****************************************************************/
function UCTCF_EnableComponentsForUpdateInCmpLib(&$CACFconstants, &$UCTCFconstants, 
                                                 &$CmpLib,
                                                 &$altiumItemsByGuid, &$altiumItemRevsByGuid, 
                                                 &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid,
                                                 &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid,
                                                 &$changedCompsByItemRevGuid)
{
  /* Retrieve necessary global constants. */
  $pathSep = 										$CACFconstants["pathSep"];

  //  echo "Hello from UCTCF_EnableComponentsForUpdateInCmpLib()\n";
  //  echo "cmpLibCompByPathAndHrid is:\n";
  
  /* Retrieve constants related to enabling and collapsing nodes in CmpLib XML file. */
  $cXmlStateName         = $UCTCFconstants["cXmlStateName"];
  $cXmlStateEnabled      = $UCTCFconstants["cXmlStateEnabled"];
  $cXmlStateSemiEnabled  = $UCTCFconstants["cXmlStateSemiEnabled"];
  $cXmlStateDisabled     = $UCTCFconstants["cXmlStateDisabled"];
  $cXmlCollapsedName     = $UCTCFconstants["cXmlCollapsedName"];
  $cXmlCollapsedDisabled = $UCTCFconstants["cXmlCollapsedDisabled"];
  $cXmlCollapsedEnabled  = $UCTCFconstants["cXmlCollapsedEnabled"];


  //  print_r($cmpLibCompByItemHrid);

  /* Loop over all components that we need to enable. */
  foreach ($changedCompsByItemRevGuid as $ItemRevGuid => $val)
    {
      /* Get component item HRID. */
      $ItemHrid = $cmpLibSysParmsByGuid[$ItemRevGuid][CACF_AlterAltiumSysParmName("ITEMHRID")];

      /* Get component comment. */
      $comment = $cmpLibSysParmsByGuid[$ItemRevGuid][CACF_AlterAltiumSysParmName("COMMENT")];

      echo "Attempting to enable component ItemHrid \"$ItemHrid\".\n";

      /* Get path of this component. */
      $path = $cmpLibSysParmsByGuid[$ItemRevGuid][CACF_AlterAltiumSysParmName("COMPONENTPATH")];
      echo "path is $path\n";

      
      /** Attempt to enable the component itself. **/
      /* Sanity check. */
      if (!isset($cmpLibCompByItemHrid[$ItemHrid]))
        my_die('In UCTCF_EnableComponentsForUpdateInCmpLib(), unable to find reference to component XML node!');

      /* Retrieve the xpath query that will get us to the XML node for this component. */
      $xpathQuery = $cmpLibCompByItemHrid[$ItemHrid]["xpath"];
      
      /* Run xpath query to get XML node for this component. */
      UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                     $resultNode);
      //      echo "resultNode is:\n";
      //      print_r($resultNode);

      /* Attempt to enable this component. */
      $resultNode[$cXmlStateName] = $cXmlStateEnabled;

      //      echo "resultNode is now:\n";
      //      print_r($resultNode);


      /** Attempt to semi-enable all folders that are parents of this component. **/
      /* Loop over all path elements (aka. directories aka. groups) that are parents of this component. */
      do
        {
          echo "In repeat loop, path is \"$path\".\n";
          
          /* See if the current path exists in the XML file. */
          if (isset($cmpLibGroupXpathByPath[$path]))
            {
              echo "Attempting to semi-enable this group!\n";

              /* Retrieve the xpath query that will get us to the XML node for this path. */
              $xpathQuery = $cmpLibGroupXpathByPath[$path];

              /* Run xpath query to get XML node for this path. */
              UCTCF_GetXmlNodeFromXpathQuery($CmpLib, $xpathQuery, 
                                             $resultNode);

              //      echo "resultNode is:\n";
              //      print_r($resultNode);
              
              /* Attempt to semi-enable this path element. */
              $resultNode[$cXmlStateName] = $cXmlStateSemiEnabled;
              
              $resultNode[$cXmlCollapsedName] = $cXmlCollapsedEnabled;
            } /* endif */
          
          else
            {
              echo "path does not exist in XML file:  \"$path\".\n";
            }

          /* Strip off last directory from $path. */
          /* Note:  That regex without excessive quoting would be '/[^\\]+\\$/'.
           In other words, replace any (collection of non-backslashes), followed by
           a backslash, at the end of the string, with "". */
          /* FIXME:  Used magic strings instead of $pathSep due to having to escape too much! */
          $path = preg_replace('/[^\\\\]+\\\\$/', "", $path);
          //          echo "In repeat loop, path is now \"$path\".\n";

        }
      while ($path != "");

    } /* end foreach */

} /* end UCTCF_EnableComponentsForUpdateInCmpLib() */



/****************************************************************
 * UCTCF_InitAndGetVaultData()
 *		Function to initialize this script and get all necessary
 * data from the Altium Vault.
 *
 * Outputs:  (all parameters listed are outputs from this function!)
 ****************************************************************/
function UCTCF_InitAndGetVaultData(&$CACFconstants,
                                   &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                   &$UCTCFconstants, $CmpLibFileName, 
                                   &$altiumFoldersByGuid, &$altiumItemsByGuid, &$altiumItemRevsByGuid, 
                                   &$altiumModelDataByItemRevHrid, &$altiumUserParmNames, 
                                   &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid)
{
  /* Setup some constants for use by this script. */
  $UCTCFconstants = array();

  /* Column widths for new tags in CmpLib XML file. */
  $UCTCFconstants["cNewUserParmColumnWidth"] = "25";
  $UCTCFconstants["cNewModelTypeColumnWidth"] = "50";

  /* Name of default Altium Vault. */
  /* Note:  Company-specific info! */
  $UCTCFconstants["defaultVaultHrid"] = "TRT Satellite Vault";

  /* Name of default Altium Revision Naming Scheme. */
  /* Note:  Company-specific info! */
  $UCTCFconstants["defaultRevisionNamingSchemeHrid"] = "TRT 1-Level Rev Scheme";

  /* Name of default Altium LifeCycle Definition. */
  /* Note:  Company-specific info! */
  $UCTCFconstants["defaultLifeCycleDefinitionHrid"] = "TRT Component Lifecycle";

  /* Constants related to the "StateIndex" CmpLib XML attribute. */
  $UCTCFconstants["cXmlStateName"] = "StateIndex";
  $UCTCFconstants["cXmlStateEnabled"] = "2";		// Set this to enable a component to be released to Vault
  $UCTCFconstants["cXmlStateSemiEnabled"] = "1";	// Set this for all folders that are parents of a component in above state.
  $UCTCFconstants["cXmlStateDisabled"] = "0";		// Set this to disable a component/folder being released to Vault.

  /* Constants related to the "Collapsed" CmpLib XML attribute. */
  $UCTCFconstants["cXmlCollapsedName"] = "Collapsed";	
  $UCTCFconstants["cXmlCollapsedDisabled"] = "true";	// Set this to collapse (hide) folders/components that are disabled.
  $UCTCFconstants["cXmlCollapsedEnabled"] = "false";	// Set this to un-collapse (show) folders/components that are enabled.


  //  echo "Hello world.  Attempting to generate .csv files.\n";

  /** Initialize and run various CACF_*() functions to get low level Vault data. **/
  /* Call CACF_Init() to perform all initialization needed for CACF_*() functions. */
  /* Note:  All the parameters are outputs from this function! */
  CACF_Init($db, $CACFconstants,
            $altiumParmsByComponentLines, $auditComponentsByType, $auditComponentsByTypeUnmatched, $altiumUserNamesByGuid, $altiumAclUserPermissions);

  /** Alter certain constants setup by CACF_Init() **/

  /* Flag that we DO NOT wish to keep 0 length output files that we generate. */
  $CACFconstants["doKeepZeroLengthOutputFiles"] = 	false;

  /* Flag that we will not generate the per-component audit files. */
  $CACFconstants["auditComponentsFileName"] =      	"";
  

  /* Retrieve necessary global constants. */
  $auditFileExt = 									$CACFconstants["auditFileExt"];
  $auditComponentsByTypeFileName = 					$CACFconstants["auditComponentsByTypeFileName"];

  /* Strip off the extension from $CmpLibFileName to get a base name. */
  $CmpLibBaseName = preg_replace("/\.[^.]+/", "", $CmpLibFileName);
  //  echo "CmpLibFileName is \"$CmpLibFileName\".\n";
  //  echo "CmpLibBaseName is \"$CmpLibBaseName\".\n";

  /* Change the $auditComponentsByTypeFileName to remove reference to "_vault_database". */
  $auditComponentsByTypeFileName = preg_replace("/_vault_database/", "", $auditComponentsByTypeFileName);

  /* Change the $auditComponentsByTypeFileName to append the name of our CmpLib basename. */
  $auditComponentsByTypeFileName = preg_replace("/$auditFileExt/", "_".$CmpLibBaseName.$auditFileExt, $auditComponentsByTypeFileName);

  /* Create a version of this with a "_Vault" suffix. */
  $auditComponentsByTypeFileNameVault = preg_replace("/$auditFileExt/", "_Vault".$auditFileExt, $auditComponentsByTypeFileName);

  /* Create a version of this with a "_CmpLib" suffix. */
  $auditComponentsByTypeFileNameCmpLib = preg_replace("/$auditFileExt/", "_CmpLib".$auditFileExt, $auditComponentsByTypeFileName);

  /* Create a version of this with a "_Excel" suffix. */
  $auditComponentsByTypeFileNameExcel = preg_replace("/$auditFileExt/", "_Excel".$auditFileExt, $auditComponentsByTypeFileName);


  /* Store back to CACFconstants */
  $CACFconstants["auditComponentsByTypeFileName"] =	$auditComponentsByTypeFileName;
  $CACFconstants["auditComponentsByTypeFileNameVault"] =	$auditComponentsByTypeFileNameVault;
  $CACFconstants["auditComponentsByTypeFileNameCmpLib"] =	$auditComponentsByTypeFileNameCmpLib;
  $CACFconstants["auditComponentsByTypeFileNameExcel"] =	$auditComponentsByTypeFileNameExcel;


  /** Get the GUID of our default Vault. **/
  /* Analyze all Vault Vaults. */
  CACF_AnalyzeVaultVaults($db, $CACFconstants,
                          $altiumVaultsByHrid);

  /* Look up the GUID of our default Vault. */
  $defaultVaultHrid = $UCTCFconstants["defaultVaultHrid"];

  $defaultVaultGuid = $altiumVaultsByHrid[$defaultVaultHrid]["GUID"];
  echo "defaultVaultGuid is \"$defaultVaultGuid\"\n";

  $UCTCFconstants["defaultVaultGuid"] = $defaultVaultGuid;


  /** Get the GUID of our default Revision Naming Scheme. **/
  /* Analyze all Vault Revision Naming Schemes. */
  CACF_AnalyzeVaultRevisionNamingSchemes($db, $CACFconstants,
                                         $altiumRevisionNamingSchemesByHrid);

  /* Look up the GUID of our default Revision Naming Scheme. */
  $defaultRevisionNamingSchemeHrid = $UCTCFconstants["defaultRevisionNamingSchemeHrid"];

  $defaultRevisionNamingSchemeGuid = $altiumRevisionNamingSchemesByHrid[$defaultRevisionNamingSchemeHrid]["GUID"];
  echo "defaultRevisionNamingSchemeGuid is \"$defaultRevisionNamingSchemeGuid\"\n";

  $UCTCFconstants["defaultRevisionNamingSchemeGuid"] = $defaultRevisionNamingSchemeGuid;


  /** Get the GUID of our default LifeCycle Definition. **/
  /* Analyze all Vault LifeCycle Definitions. */
  CACF_AnalyzeVaultLifeCycleDefinitions($db, $CACFconstants,
                                        $altiumLifeCycleDefinitionsByHrid);

  /* Look up the GUID of our default LifeCycle Definition. */
  $defaultLifeCycleDefinitionHrid = $UCTCFconstants["defaultLifeCycleDefinitionHrid"];

  $defaultLifeCycleDefinitionGuid = $altiumLifeCycleDefinitionsByHrid[$defaultLifeCycleDefinitionHrid]["GUID"];
  echo "defaultLifeCycleDefinitionGuid is \"$defaultLifeCycleDefinitionGuid\"\n";

  $UCTCFconstants["defaultLifeCycleDefinitionGuid"] = $defaultLifeCycleDefinitionGuid;


  /** Retrieve and cache various other information from the Vault database. **/
  /* Analyze all Vault folders and extract all linkages so that we understand folder trees. */
  CACF_AnalyzeVaultFolders($db, $CACFconstants,
                           $altiumFoldersByGuid);

  /* Analyze all Vault items and cache certain fields, indexed by GUID. */
  CACF_AnalyzeVaultItems($db, $CACFconstants,
                         $altiumItemsByGuid);

  /* Analyze all Vault item revisions and cache certain fields, indexed by GUID. */
  CACF_AnalyzeVaultItemRevisions($db, $CACFconstants,
                                 $altiumItemRevsByGuid);

  /* Analyze all Vault item user parameters and store for later use. */
  CACF_AnalyzeVaultItemUserParameters($db, $CACFconstants,
                                      $altiumUserParmNames, $altiumItemUserParmValuesByGuid);

  /* Cache ACL data for later use. */
  CACF_CreateAclAuditData($db, $CACFconstants,
                          $altiumUserNamesByGuid, $altiumFoldersByGuid, $altiumItemsByGuid, $altiumItemRevsByGuid, $altiumAclUserPermissions, 
                          $altiumAclDataByObjectHrid, $altiumAclDataByObjectGuid);

  /* Create model audit data. */
  CACF_CreateModelAuditData($db, $CACFconstants,
                            $altiumUserNamesByGuid, $altiumFoldersByGuid, $altiumAclDataByObjectGuid, 
                            $altiumItemRevsByGuid, $altiumModelDataByItemRevHrid);

  /* Create component audit data. */
  CACF_CreateComponentAuditData($db, $CACFconstants,
                                $altiumUserNamesByGuid, $altiumFoldersByGuid, $altiumAclDataByObjectGuid, $altiumItemRevsByGuid, 
                                $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid);

} /* end UCTCF_InitAndGetVaultData() */


/****************************************************************
 * UCTCF_GetCmpLibData()
 *		Function to get all data from specified CmpLib file.
 *
 * Outputs:  &$CmpLib, 
 *           &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
 *           &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
 *           &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
 *           &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid, &$cmpLibDidCorrection
 ****************************************************************/
function UCTCF_GetCmpLibData(&$CACFconstants,
                             &$UCTCFconstants, $CmpLibFileName, 
                             &$altiumItemsByHrid, &$altiumItemRevsByHrid,
                             &$altiumFoldersByGuid, &$altiumItemSysParmValuesByGuid, &$altiumModelDataByItemRevHrid, 
                             &$CmpLib, 
                             &$cmpLibUserParmNamesById, &$cmpLibUserParmIdsByName,
                             &$cmpLibModelTypesById, &$cmpLibModelTypesByType, 
                             &$cmpLibModelLinksById, &$cmpLibModelLinksByItemRevHrid,
                             &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid, &$cmpLibGroupXpathByPath, &$cmpLibCompByItemHrid, &$cmpLibDidCorrection)
{
  /** Read in component information from .CmpLib file. **/
  /* Read existing .CmpLib file. */
  echo date('H:i:s') . " Attempting to open Altium .CmpLib file.\n";
  UCTCF_ReadCmpLibFile($CmpLib, $CmpLibFileName);

  /* Extract all required parameters (aka. declarations of user parameters) from XML file data structures. */
  UCTCF_ExtractRequiredParameters($CmpLib->RequiredParameters, 
                                  $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName);

  /* Extract all model types (aka. declarations of model types) from XML file data structures. */
  UCTCF_ExtractRequiredModels($CmpLib->RequiredModels, 
                              $cmpLibModelTypesById, $cmpLibModelTypesByType);

  /* Extract all model links (aka. pointers to an SCHLIB / PCBLIB object) from XML file data structures. */
  $ModelLinksNode = &$CmpLib->ModelLinks;
  UCTCF_ExtractModelLinks($CACFconstants, 
                          $ModelLinksNode, 
                          $altiumModelDataByItemRevHrid, 
                          $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid);

  /* Extract all components and their parameters from XML file data structures. */
  UCTCF_ExtractAllComponentParmsFromCmpLib($CACFconstants, $UCTCFconstants, 
                                           $altiumItemsByHrid, $altiumItemRevsByHrid,
                                           $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, 
                                           $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName, $cmpLibModelTypesById, $cmpLibModelLinksById, 
                                           $CmpLib,
                                           $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibNumNewComps, $cmpLibDidCorrection);

  /* Extract xpath queries for all groups (database folders) and components so that we may find them later. */
  /* Note:  I made this a separate operation because will we need to re-do this operation later after
   changing ID numbers, but we don't want to ever call UCTCF_ExtractAllComponentParmsFromCmpLib() again. */
  UCTCF_ExtractAllGroupsAndComponentsByXpathFromCmpLib($CACFconstants, $UCTCFconstants, 
                                                       $altiumFoldersByGuid, 
                                                       $CmpLib,
                                                       $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid);

  //  echo "Leaving UCTCF_GetCmpLibData(), altiumItemSysParmValuesByGuid is:\n";
  //  print_r($altiumItemSysParmValuesByGuid);

} /* end UCTCF_GetCmpLibData() */


/****************************************************************
 * UCTCF_CreateVaultComponentAuditData()
 *		Function to create audit data for relevant Vault components.
 *
 * Outputs:  &$altiumUserParmNamesByCompType,
 *           (csv file output)
 ****************************************************************/
function UCTCF_CreateVaultComponentAuditData(&$CACFconstants,
                                             &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                             &$UCTCFconstants, 
                                             &$altiumUserParmNames, &$altiumUserParmNamesByCompType, 
                                             &$altiumItemUserParmValuesByGuid, &$altiumItemSysParmValuesByGuid, &$altiumObsoleteCompsByGuid,
                                             &$cmpLibSysParmsByGuid)
{
  /* Flag that we want to write this audit file with the "_Vault" suffix. */
  $CACFconstants["auditComponentsByTypeFileName"] =	$CACFconstants["auditComponentsByTypeFileNameVault"];

  /* Pretend to write Vault component audit data to csv file. */
  /* This first run through will no components culled out is solely to get a valid $altiumUserParmNamesByCompType output. */
  CACF_WriteComponentAuditDataToCsv($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $altiumUserParmNames, $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid,
                                    $altiumUserParmNamesByCompType, $altiumParmsByCompType);


  //  echo "In UCTCF_CreateVaultComponentAuditData(), altiumParmsByCompType is:\n";
  //  print_r($altiumParmsByCompType);


  /* Cull out components that exist in the Vault but aren't defined in the CmpLib file that we're working on. */
  UCTCF_CullCompsThatDontExistInCmpLib($CACFconstants, 
                                       $altiumItemSysParmValuesByGuid, $cmpLibSysParmsByGuid);

  /* Pretend to write Vault component audit data to csv file. */
  /* This second run through after components were culled is to get a valid $altiumParmsByCompType output. */
  CACF_WriteComponentAuditDataToCsv($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $altiumUserParmNames, $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid,
                                    $altiumUserParmNamesByCompTypeFOO, $altiumParmsByCompType);

  //  echo "In UCTCF_CreateVaultComponentAuditData(), altiumParmsByCompType is now:\n";
  //  print_r($altiumParmsByCompType);


  /* Create all Vault per-component-type audit data and output to csv files. */
  CACF_CreateAllComponentTypeAuditDataAndWriteToCsv($CACFconstants,
                                                    $altiumParmsByComponentLines,
                                                    $altiumUserParmNamesByCompType, $altiumParmsByCompType, $altiumItemSysParmValuesByGuid, $altiumItemUserParmValuesByGuid, $altiumObsoleteCompsByGuid);

} /* end UCTCF_CreateVaultComponentAuditData() */


/****************************************************************
 * UCTCF_CreateCmpLibComponentAuditData()
 *		Function to create audit data for all CmpLib components.
 *
 * Outputs:  (csv file output)
 ****************************************************************/
function UCTCF_CreateCmpLibComponentAuditData(&$CACFconstants,
                                              &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                              &$UCTCFconstants, 
                                              &$altiumUserParmNames, &$altiumUserParmNamesByCompType, &$altiumObsoleteCompsByGuid,
                                              &$cmpLibUserParmsByGuid, &$cmpLibSysParmsByGuid)                                              
{
  /* Flag that we want to write this audit file with the "_CmpLib" suffix. */
  $CACFconstants["auditComponentsByTypeFileName"] =	$CACFconstants["auditComponentsByTypeFileNameCmpLib"];

  //  echo "In UCTCF_CreateCmpLibComponentAuditData(), cmpLibSysParmsByGuid is:\n";
  //  print_r($cmpLibSysParmsByGuid);

  /* Pretend to write CmpLib component audit data to csv file. */
  /* This is the only run using CmpLib derived data.  Output is $altiumParmsByCompType. */
  CACF_WriteComponentAuditDataToCsv($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $altiumUserParmNames, $cmpLibUserParmsByGuid /* CmpLib data! */, $cmpLibSysParmsByGuid /* CmpLib data! */, $altiumObsoleteCompsByGuid,
                                    $altiumUserParmNamesByCompTypeFOO, $altiumParmsByCompType);
  
  /* Create all CmpLib per-component-type audit data and output to csv files. */
  CACF_CreateAllComponentTypeAuditDataAndWriteToCsv($CACFconstants,
                                                    $altiumParmsByComponentLines,
                                                    $altiumUserParmNamesByCompType, $altiumParmsByCompType, $cmpLibSysParmsByGuid /* CmpLib data! */, $cmpLibUserParmsByGuid /* CmpLib data! */, $altiumObsoleteCompsByGuid);
  
} /* end UCTCF_CreateCmpLibComponentAuditData() */


/****************************************************************
 * UCTCF_GetExcelData()
 *		Function to get all component data from Excel file.
 *
 * Outputs:  &$excelUserParmsByItemHrid, &$excelSysParmsByItemHrid);
 ****************************************************************/
function UCTCF_GetExcelData(&$CACFconstants,
                            &$UCTCFconstants, $ExcelFileName, 
                            &$altiumItemsByHrid, &$altiumItemRevsByHrid,
                            &$altiumModelDataByItemRevHrid, 
                            &$excelUserParmsByItemHrid, &$excelSysParmsByItemHrid)
{
  /** Read in component information from .xlsx file. **/
  echo date('H:i:s') . " Attempting to open Excel .xlsx file.\n";
  $objReader = PHPExcel_IOFactory::createReader('Excel2007');	/* Expect an .xlsx file. */
  $objReader->setReadDataOnly(true);							/* Inform PHPExcel reader that it may ignore formatting.  We only want formulae and data. */

  $objPHPExcel = PHPExcel_IOFactory::load($ExcelFileName);
  $objPHPExcel->setActiveSheetIndex(0); /* Choose sheet number 0 (eg. "Sheet1"). */
  //  PHPExcel_Calculation::getInstance()->writeDebugLog = true;


  /* Call UCTCF_ExtractComponentsFromExcel() to extract component information from our Excel spreadsheet. */
  UCTCF_ExtractComponentsFromExcel($CACFconstants, 
                                   $altiumItemsByHrid, $altiumItemRevsByHrid, 
                                   $altiumModelDataByItemRevHrid, 
                                   $objPHPExcel, 
                                   $excelUserParmsByItemHrid, $excelSysParmsByItemHrid);

  //print_r(PHPExcel_Calculation::getInstance()->debugLog);

} /* end UCTCF_GetExcelData() */


/****************************************************************
 * UCTCF_CreateExcelComponentAuditData()
 *		Function to create audit data for all Excel components.
 *
 * Outputs:  (csv file output)
 ****************************************************************/
function UCTCF_CreateExcelComponentAuditData(&$CACFconstants,
                                             &$auditComponentsByType, &$auditComponentsByTypeUnmatched, 
                                             &$UCTCFconstants, 
                                             &$altiumUserParmNames, &$altiumUserParmNamesByCompType, &$altiumObsoleteCompsByGuid,
                                             &$excelUserParmsByItemHrid, &$excelSysParmsByItemHrid)
{
  /* Flag that we want to write this audit file with the "_Excel" suffix. */
  $CACFconstants["auditComponentsByTypeFileName"] =	$CACFconstants["auditComponentsByTypeFileNameExcel"];

  /* Pretend to write Excel component audit data to csv file. */
  /* This is the only run using Excel derived data.  Output is $altiumParmsByCompType. */
  CACF_WriteComponentAuditDataToCsv($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $altiumUserParmNames, $excelUserParmsByItemHrid /* Excel data! */, $excelSysParmsByItemHrid /* Excel data! */, $altiumObsoleteCompsByGuid,
                                    $altiumUserParmNamesByCompTypeFOO, $altiumParmsByCompType);

  /* Create all Excel per-component-type audit data and output to csv files. */
  CACF_CreateAllComponentTypeAuditDataAndWriteToCsv($CACFconstants,
                                                    $altiumParmsByComponentLines,
                                                    $altiumUserParmNamesByCompType, $altiumParmsByCompType, $excelSysParmsByItemHrid /* Excel data! */, $excelUserParmsByItemHrid /* Excel data! */, $altiumObsoleteCompsByGuid);

  //print_r(PHPExcel_Calculation::getInstance()->debugLog);

} /* end UCTCF_CreateExcelComponentAuditData() */


/****************************************************************
 *	ENTRY POINT
 ****************************************************************/

/** Configure error reporting **/
error_reporting(E_ALL);

/** Allow a larger memory footprint for this script. **/
ini_set("memory_limit","400M");

/** Parse command line arguments. **/
$usage = "Usage:  $argv[0] [audit|update] CmpLibFile.CmpLib ExcelFile.xlsx";
if ($argc < 3)
  {
    my_die($usage);
  }

/* Get operating mode. */
$mode = $argv[1];

/* Sanity check. */
if ( ($mode != "audit") && ($mode != "update") )
  my_die($usage);

/* Configure input filenames. */
$CmpLibFileName = $argv[2]; //"ICs_amplifiers.CmpLib";
$ExcelFileName  = $argv[3]; //"ICs_amplifiers.xlsx";


/** Initialize this script and get all Vault data. **/
UCTCF_InitAndGetVaultData($CACFconstants,
                          $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                          $UCTCFconstants, $CmpLibFileName, 
                          $altiumFoldersByGuid, $altiumItemsByGuid, $altiumItemRevsByGuid, 
                          $altiumModelDataByItemRevHrid, $altiumUserParmNames, 
                          $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid);


/* Re-index our cached Vault item and itemRev data by HRID, since we'll need this for CmpLib and Excel analysis. */
UCTCF_ReIndexByHrid($altiumItemsByGuid, $altiumItemRevsByGuid,
                    $altiumItemsByHrid, $altiumItemRevsByHrid);


/** Get all CmpLib file data. **/
$cmpLibDidCorrection = false;
UCTCF_GetCmpLibData($CACFconstants,
                    $UCTCFconstants, $CmpLibFileName, 
                    $altiumItemsByHrid, $altiumItemRevsByHrid,
                    $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, $altiumModelDataByItemRevHrid, 
                    $CmpLib, 
                    $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                    $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                    $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid,
                    $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid, $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid, $cmpLibDidCorrection);
  

/* See if we need to re-write CmpLib data so that corrections to ItemGUIDs, ItemRevGUIDs, etc. get written back to disk. */
/* Note that this is the only circumstance where we will write to CmpLib file while in audit mode! */
if ($cmpLibDidCorrection) 
  {
    echo "\nAttempting to write corrections to CmpLib file back to disk!\n";
    
    /* Write .CmpLib file back out to .xml file. */
    UCTCF_WriteCmpLibFile($CmpLib, $CmpLibFileName);

  } /* endif */


/** Get all Excel file data. **/
UCTCF_GetExcelData($CACFconstants,
                   $UCTCFconstants, $ExcelFileName, 
                   $altiumItemsByHrid, $altiumItemRevsByHrid,
                   $altiumModelDataByItemRevHrid, 
                   $excelUserParmsByItemHrid, $excelSysParmsByItemHrid);


/** Output Vault derived audit data. **/
UCTCF_CreateVaultComponentAuditData($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $UCTCFconstants, 
                                    $altiumUserParmNames, $altiumUserParmNamesByCompType, 
                                    $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid, $altiumObsoleteCompsByGuid,
                                    $cmpLibSysParmsByGuid);


/** Output CmpLib derived audit data. **/
UCTCF_CreateCmpLibComponentAuditData($CACFconstants,
                                     $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                     $UCTCFconstants, 
                                     $altiumUserParmNames, $altiumUserParmNamesByCompType, $altiumObsoleteCompsByGuid,
                                     $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid);


/** Output Excel derived audit data. **/
UCTCF_CreateExcelComponentAuditData($CACFconstants,
                                    $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                    $UCTCFconstants, 
                                    $altiumUserParmNames, $altiumUserParmNamesByCompType, $altiumObsoleteCompsByGuid,
                                    $excelUserParmsByItemHrid, $excelSysParmsByItemHrid);


/* We only will update CmpLib file when we're running in "update" mode. */
if ($mode == "update")
  {
    echo date('H:i:s') . " Proceeding to run \"update\" operation....\n";

    /** Prepare to update CmpLib file from Excel data. **/
    UCTCF_UpdateCmpLibFromExcelData($CACFconstants, $UCTCFconstants, 
                                    $altiumFoldersByGuid, $altiumItemSysParmValuesByGuid, $altiumModelDataByItemRevHrid,
                                    $CmpLib, 
                                    $cmpLibUserParmNamesById, $cmpLibUserParmIdsByName,
                                    $cmpLibModelTypesById, $cmpLibModelTypesByType, 
                                    $cmpLibModelLinksById, $cmpLibModelLinksByItemRevHrid,
                                    $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid,
                                    $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid,
                                    $excelUserParmsByItemHrid, $excelSysParmsByItemHrid);

    /** Inform user of all differences between CmpLib components and the associated Vault components. **/
    UCTCF_CompareAllCmpLibComponentsToVault($CACFconstants, $UCTCFconstants, 
                                            $altiumItemsByGuid, $altiumItemRevsByGuid, 
                                            $altiumItemUserParmValuesByGuid, $altiumItemSysParmValuesByGuid,
                                            $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid,
                                            $changedCompsByItemRevGuid);

    /** Enable all changed CmpLib components for update to Vault. **/
    UCTCF_EnableComponentsForUpdateInCmpLib($CACFconstants, $UCTCFconstants, 
                                            $CmpLib,
                                            $altiumItemsByGuid, $altiumItemRevsByGuid, 
                                            $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid,
                                            $cmpLibGroupXpathByPath, $cmpLibCompByItemHrid,
                                            $changedCompsByItemRevGuid);
  
    /** Sort the components in the CmpLib file by ITEMHRID. **/
    /* Sort $itemListById array using non-case-sensitive sort. */
    /* Note:  Here we are using uasort() (with strcasecmp() as comparison func) to try to emulate what Altium does with ordering CmpLib href id references. */
    $rc = uasort($cmpLibSysParmsByGuid, "UCTCF_CompareByItemHRID");
    if ($rc == FALSE) my_die("uasort() failed!");
    
    /** Sort the system parameters within each component. **/
    /* For some unknown reason, this is necessary when we want to re-write the CmpLib data after updates. */
    /* Loop over all the standard system parameter names. */
    foreach ($cmpLibSysParmsByGuid as $GUID => $val)
      {
        /* Sort the list of CmpLib system parms used by this component. */
        $rc = ksort($cmpLibSysParmsByGuid[$GUID]);
        if ($rc == FALSE) my_die("ksort() failed!");
        
      } /* end foreach */


    /* Note:  We will not re-do Vault audit data, since this script will not actually release new components or new revs
     of components to Vault. */


    /** Re-output CmpLib derived audit data. **/
    UCTCF_CreateCmpLibComponentAuditData($CACFconstants,
                                         $auditComponentsByType, $auditComponentsByTypeUnmatched, 
                                         $UCTCFconstants, 
                                         $altiumUserParmNames, $altiumUserParmNamesByCompType, $altiumObsoleteCompsByGuid,
                                         $cmpLibUserParmsByGuid, $cmpLibSysParmsByGuid);

    /* Write .CmpLib file back out to .xml file. */
    UCTCF_WriteCmpLibFile($CmpLib, $CmpLibFileName);

  } /* endif */


/************************************************************************************************
 **** Wrap things up.                                                                        ****
 ************************************************************************************************/

/** Report script done and peak memory usage. **/
echo date('H:i:s') . " Script is ending successfully.\n";
echo date('H:i:s') . " Peak memory usage: " . (memory_get_peak_usage(true) / 1024 / 1024) . " MiB.\n";


?>

<?php /*_____________________END___OF___THE___CODE______________________

___________________________________________________________________*/ ?>
