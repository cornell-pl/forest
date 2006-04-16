<?
/*
Usage: $php formatinference.php <filename>
*/

include('./constants.php');
include('./initclasses.php');
include('./functions.php');

$ConfigObj=new ClConfig;
$TokenManagerObj=new ClTokenManager;

$FileName=FnGetFileName($argv);

$Fp=fopen($FileName,'r');
if(!$Fp){
  echo "error code 1 \r\n";
  exit;
}

while (!feof($Fp)) {
  $Record=NULL;
  $TokensInRecordArray=NULL;
  $Record = fgets($Fp, 4096); // assuming that new line indicates a new record.
  $TokensInRecordArray=FnTokenize(rtrim($Record)); // stripping the record seperator before sending to tokenize
  if(!isset($RecordNumber)){
    $RecordNumber=0;
  }
  else{
    $RecordNumber++;
  }
  $TokenManagerObj->FnInitializeTokenObjects($TokensInRecordArray,$RecordNumber);
}
fclose($Fp);

/** Algorithm: Token Discovery 
Input: Single byte tokens
Objective: The objective is to discover template multibyte tokens and not all multibyte tokens.
Output: Single and Multibyte tokens. 
**/
$TokenManagerObj->FnMergeTokens();
/** End algorithm **/

/** Algorithm: Format inference 
Input: Single and Multibyte tokens
Objective: The objective is to discover the format
Output: The format
**/
echo "The tokens appearing in each record are \n";
print_r($TokenManagerObj->TokensInEachRecord);
/** End algorithm **/
?>
