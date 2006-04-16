<?
class ClToken{ // the name of the object is the same as the token
  var $Content; // The content of the token.
  var $Records; // which records does the token appear in. 
  var $Stats;  // we maintain the number of unique records the token appears in over here. Though we can calculate it later seems uses less computing power 
              // to maintain this index.

  /* The unique tokens array lookes like:
Array
(
    [i] => cltoken Object
    (
            [Records] => Array
	    (
                    [0] => Array => in record number 0 
		    (
                            [0] => 1 => the 0th occurrence is at 1
			    )

                    [2] => Array => in record number 2 
		    (
                            [0] => 2 => the 0th occurrence is at 2
			    )

                    [4] => Array => in record number 4 
		    (
                            [0] => 1 => the 0th occurrence is at 1
		    )
           )

         [Stats] => Array
                (
                    [NumberOfRecords] => 2
                )

  */

  function ClToken()
  {
    $this->Stats['NumberOfRecords']=NULL;    
    $this->Records=array();    
    $this->Content=NULL;    
  }

}

class ClTokenManager{

  var $UniqueTokens; // This is the array which holds all the unique tokens in the content stream.
  /*
    $UniqueTokens[f] this holds object(cltoken)
  */
  var $Records; // This is the object of class clRecord
  /*
    $Records[0][Tokens][0] this holds &object(cltoken)
  */
  var $TokensInEachRecord;
  /*
    $TokensInEachRecord[r] this holds &object(cltoken)
  */


  function FnMergeTokens(){
    $this->FnInitTokensInEachRecord();

    while (list($TokenContent, $TokenObject) = each($this->TokensInEachRecord)) {
        $DidSomeMerging=$this->FnEvaluateTokenForMerge($TokenContent);
	if($DidSomeMerging==TRUE){
          $this->FnInitTokensInEachRecord();
	  reset ($this->TokensInEachRecord);
	  $DidSomeMerging=FALSE;
        }
    }
  }

  function FnInitTokensInEachRecord(){

    $NumberOfRecords=sizeof($this->Records)-1; // we do all our counting from 0
    $this->TokensInEachRecord=array();
    reset ($this->UniqueTokens);

    // find the tokens which appear in all records
    while (list($TokenContent, $TokenObject) = each($this->UniqueTokens)) {
      if( $TokenObject->Stats['NumberOfRecords'] == $NumberOfRecords ){
	$this->TokensInEachRecord[$TokenContent]=&$this->UniqueTokens[$TokenContent];
      }
    }
  }

  function FnEvaluateTokenForMerge($pTokenContent)
  {
    for($j=0;$j<sizeof($this->TokensInEachRecord[$pTokenContent]->Records);$j++){
      $PositionOfOccurrenceArray=$this->TokensInEachRecord[$pTokenContent]->Records[$j];
      for($i=0;$i<sizeof($PositionOfOccurrenceArray);$i++){
	$DidSomeMerging=$this->FnEvaluateTokenAtPositionForMerge($pTokenContent,$PositionOfOccurrenceArray[$i]);
	if($DidSomeMerging==TRUE){
	  return true;
	}
      }
    }
  }

  function FnEvaluateTokenAtPositionForMerge($pTokenContent,$pPosition)
  {
    // lets find out the merged pair and see if the merged pair exists in all the records
    $TokenLength=strlen($pTokenContent);
    $NextTokenContent=$this->Records[0]->Tokens[$pPosition+$TokenLength]->Content;
    if($NextTokenContent==NULL){ // we have reached the end of the record
      return false;
    }
    $MergedPair=$this->Records[0]->Tokens[$pPosition]->Content.$NextTokenContent;
    $MergePositionInAllRecordsArray=$this->FnCheckMergedPairAcrossAllRecords($MergedPair);
    if($MergePositionInAllRecordsArray!==FALSE){
      $this->FnDoTheMergingInAllrecords($pTokenContent,$NextTokenContent,$MergePositionInAllRecordsArray);

      // after we finished merging we have a bigger token. I would like to evaluate this bigger token for merge
      $this->FnEvaluateTokenAtPositionForMerge($MergedPair,$pPosition);
      // the call stack will be reduced here at this step for 17 times if we form record<html><body> here
      // debugbreak();

      // lets clean up all the memory. Its a recursive function so we are trying to be a careful.
      $TokenLength=NULL;
      $NextTokenContent=NULL;
      $MergedPair=NULL;
      $MergePositionInAllRecordsArray=NULL;
      $pTokenContent=NULL;
      $pPosition=NULL;

      return true;
    }
  }

  function FnDoTheMergingInAllRecords($pFirstTokenContent,$pSecondTokenContent,$pMergePositionInAllRecordsArray){
    // check if the merged token already exists in UniqueTokens if not create it
    if($this->FnCheckIfTokenExistsInUniqueTokens($pFirstTokenContent.$pSecondTokenContent)){
    }
    else{
      $this->FnCreateNewUniqueToken($pFirstTokenContent.$pSecondTokenContent);
    }
    // go through all the records doing the merging
    for($RecordNumber=0;$RecordNumber<sizeof($this->Records);$RecordNumber++){
      $FirstTokenPosition=$pMergePositionInAllRecordsArray[$RecordNumber];
      $this->FnDoTheMergingInOneRecord($pFirstTokenContent,$FirstTokenPosition,$pSecondTokenContent,$RecordNumber);
    }
  }

  function FnDoTheMergingInOneRecord($pFirstTokenContent,$pFirstTokenPosition,$pSecondTokenContent,$pRecordNumber){
    // we need to update the token object both the old one and the new one
    // we need to update the records array
    $this->FnRemoveTokenFromAPositionInARecord($pFirstTokenContent,$pFirstTokenPosition,$pRecordNumber);
    $FirstTokenContentLength=strlen($pFirstTokenContent);
    $this->FnRemoveTokenFromAPositionInARecord($pSecondTokenContent,$pFirstTokenPosition+$FirstTokenContentLength,$pRecordNumber);
    $this->FnAddTokenToAPositionInARecord($pFirstTokenContent.$pSecondTokenContent,$pFirstTokenPosition,$pRecordNumber);
  }

  function FnRemoveTokenFromAPositionInARecord($pTokenContent,$pTokenPosition,$pRecordNumber){

    $OccurrenceNumber=$this->FnGetTokenOccurrenceNumberFromARecordGivenThePosition($pTokenContent,$pTokenPosition,$pRecordNumber);

    if($OccurrenceNumber==0 && sizeof($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber])==1){ // => this is the last/unique occurrence of this token is this record
      unset($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber]); // => this token doesnt appear in this record anymore
      $this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']--;
      if($this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']==-1){
	unset($this->UniqueTokens[$pTokenContent]); // the token is deleted since it doesnt appear in any records
      }
    }
    else{ 

// dealing with the test case: The TokenOccurrenceNumber is 1 and no cascading needs to be done. Cascading only needs to be done if there are following tokens.

      // in this situation cascading need not to be done
      for($TokenOccurrenceNumber=$OccurrenceNumber;$TokenOccurrenceNumber==sizeof($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber])-1;$TokenOccurrenceNumber++){
	  unset($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][$TokenOccurrenceNumber]);
      }// end of for loop

      // debugbreak();
      // in this situation cascading needs to be done
      for($TokenOccurrenceNumber=$OccurrenceNumber;$TokenOccurrenceNumber<sizeof($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber])-1;$TokenOccurrenceNumber++){
	$this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][$TokenOccurrenceNumber]=$this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][$TokenOccurrenceNumber+1];
	if( ($TokenOccurrenceNumber+2) == sizeof($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber]) ){
	  unset($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][$TokenOccurrenceNumber+1]);
	}
      }// end of for loop

    }

  }

  function FnAddTokenToAPositionInARecord($pTokenContent,$pTokenPosition,$pRecordNumber){
    if(!isset($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber])){
      if($this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']===NULL){ // populating the stats field of the tokenobj to say it occurs once
	$this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']=0;
      }
      else{
	$this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']++;
      }
    }
    $this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][]=$pTokenPosition; // [] => the next occurrence number
    $this->Records[$pRecordNumber]->Tokens[$pTokenPosition]=&$this->UniqueTokens[$pTokenContent];
  
    // if the length of this token is more then 1 lets delete the next index.
    $LengthOfToken=strlen($pTokenContent);
    for($i=1;$i<$LengthOfToken;$i++){
      unset($this->Records[$pRecordNumber]->Tokens[$pTokenPosition+$i]);
    }
  }

  function FnGetTokenOccurrenceNumberFromARecordGivenThePosition($pTokenContent,$pTokenPosition,$pRecordNumber){
    for($TokenOccurrenceNumber=0;$TokenOccurrenceNumber<sizeof($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber]);$TokenOccurrenceNumber++){
      if($this->UniqueTokens[$pTokenContent]->Records[$pRecordNumber][$TokenOccurrenceNumber]==$pTokenPosition) return $TokenOccurrenceNumber;
    }
  }
  
  function FnCreateNewUniqueToken($pTokenContent){
    $this->UniqueTokens[$pTokenContent]=new ClToken; // creating a new token object
    $this->UniqueTokens[$pTokenContent]->Content=$pTokenContent; // assigning what does the token hold
  }

  function FnCheckIfTokenExistsInUniqueTokens($pToken){
    if(isset($this->UniqueTokens[$pToken])){
      return true;
    }
    else {
      return false;
    }
  }

  function FnCheckMergedPairAcrossAllRecords($pMergedPair){
    // go through all the records checking if the merged pair exists
    $MergeFlag=TRUE;
    for($RecordNumber=0;$RecordNumber<sizeof($this->Records) && $MergeFlag!=FALSE;$RecordNumber++){
      $MergePositionInOneRecord=$this->FnCheckMergedPairInOneRecord($pMergedPair,$RecordNumber);
      if($MergePositionInOneRecord!==FALSE){
	$MergeFlag=TRUE;
	$MergePositionInAllRecordsArray[$RecordNumber]=$MergePositionInOneRecord;
      }
      else{
	$MergeFlag=FALSE;
      }
    }

    if($MergeFlag==TRUE){
      return $MergePositionInAllRecordsArray;
    }
    elseif($MergeFlag==FALSE){
      return $MergeFlag;
    }

  }

  function FnCheckMergedPairInOneRecord($pMergedPair,$pRecordNumber){
    // lets go through each of the tokens in this record
    $MergePosition=FALSE;
    $RecordPointer=0;
    for($NumberOfUniqueTokens=0;$NumberOfUniqueTokens<sizeof($this->Records[$pRecordNumber]->Tokens) && $MergePosition===FALSE;$NumberOfUniqueTokens++){
      $CurrentTokenContent=$this->Records[$pRecordNumber]->Tokens[$RecordPointer]->Content;
      $CurrentTokenContentLength=strlen($CurrentTokenContent);
      $NextTokenContent=$this->Records[$pRecordNumber]->Tokens[$RecordPointer+$CurrentTokenContentLength]->Content;
      if($CurrentTokenContent.$NextTokenContent==$pMergedPair){
	$MergePosition=$RecordPointer;
	break;
      }
      else{
	$MergePosition=FALSE;
      }
      $RecordPointer=$RecordPointer+$CurrentTokenContentLength;
    }
    return $MergePosition;
  }

  function FnInitializeTokenObjects($pTokensInRecordArray,$pRecordNumber){

    global $ConfigObj;

    $NumberOfTokensInRecord=NULL;
    $Token=NULL;
    $this->Records[$pRecordNumber]= new ClRecord;


    $NumberOfTokensInRecord=sizeof($pTokensInRecordArray);
    for($i=0;$i<$NumberOfTokensInRecord;$i++){ // evaluating each token in the record
      $Token=$pTokensInRecordArray[$i];
      if($this->FnCheckIfTokenExistsInUniqueTokens($Token)){
      }
      else{
	$this->FnCreateNewUniqueToken($Token);
      }
      $this->FnAddTokenToAPositionInARecord($Token,$i,$pRecordNumber);
    }
  }
}
?>
