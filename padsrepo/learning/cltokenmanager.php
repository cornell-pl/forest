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

  function FnDiscoverTokensWithConstantFrequencyAcrossRecords(){
    $this->FnCreateTheTokenFrequencyAcrossRecordsMatrix();
    while(list($Token, $FrequencyAcrossRecords) = each($this->TokenFrequencyAcrossRecords)){
      $StillSame=TRUE;
      	$NumberOfRecordsWhereTheTokenAppears=0;
      foreach($FrequencyAcrossRecords as $RecordNumber => $TokenFrequencyInRecord){
	if(!isset($PrevTokenFrequencyInRecord)){
	  $PrevTokenFrequencyInRecord=$TokenFrequencyInRecord;
	}
	if($TokenFrequencyInRecord==$PrevTokenFrequencyInRecord){
	  $StillSame=TRUE;
	}
	else{
	  $StillSame=FALSE;
	  break;
	}
	$NumberOfRecordsWhereTheTokenAppears++;
      }
      if($StillSame===TRUE &&$NumberOfRecordsWhereTheTokenAppears==5){
	$this->TokensWithConstantFrequencyAcrossRecords[$Token]=$TokenFrequencyInRecord;
      }
    }
  }

  function FnCreateTheTokenFrequencyAcrossRecordsMatrix(){
    for($pRecordNumber=0;$pRecordNumber<sizeof($this->Records);$pRecordNumber++){
      while(list($key, $val) = each($this->Records[$pRecordNumber]->Tokens)){
	$Token=$val->Content;
	$this->TokenFrequencyAcrossRecords[$Token][$pRecordNumber]++;
      }
      reset($this->Records[$pRecordNumber]->Tokens);
    }  
  }

  function FnDiscoverMultiByteTokens(){
    $this->FnCreateTheTokenOccurrenceConditionalProbabilityMatrix();
    $TokensForMerge=$this->FnGetTokensForMerge();
    if(sizeof($TokensForMerge)==0){ 
      return;
    }
    for($i=0;$i<sizeof($TokensForMerge);$i++){
      $this->FnDoTheMergingInAllRecords($TokensForMerge[$i][0],$TokensForMerge[$i][1]);
      //$this->Records[0]->FnPrintRecord(); // this is a good thing to turn on to see how the records are behaving.
    }
    $this->FnDiscoverMultiByteTokens();
  }

  function FnCreateTheTokenOccurrenceConditionalProbabilityMatrix(){
    $this->TokenOccurrenceConditionalProbabilityMatrix=array();
    for($pRecordNumber=0;$pRecordNumber<sizeof($this->Records);$pRecordNumber++){
    $PrevToken='BOR';
      while(list($key, $val) = each($this->Records[$pRecordNumber]->Tokens)){
	$Token=$val->Content;
	$this->TokenOccurrenceConditionalProbabilityMatrix[$PrevToken][$Token][$pRecordNumber]++;
	$PrevToken=$Token;    
      }
      reset($this->Records[$pRecordNumber]->Tokens);
    }
  }


  function FnGetTokensForMerge(){
    $TotalNumberOfRecords=5;
    $TokenNumberToMerge=0;
    while (list($key, $val) = each($this->TokenOccurrenceConditionalProbabilityMatrix)) {
      while (list($key2, $val2) = each($val)) {
	$NumberOfRecordsWhereItOccurs=sizeof($val2);
	if($TotalNumberOfRecords==$NumberOfRecordsWhereItOccurs){
	  $TokensForMerge[$TokenNumberToMerge][0]=$key;
	  $TokensForMerge[$TokenNumberToMerge][1]=$key2;
  	  $TokenNumberToMerge++;
	}
      }
    }
    return $TokensForMerge;
  }


  function FnDoTheMergingInAllRecords($pFirstTokenContent,$pSecondTokenContent){
    // check if the merged token already exists in UniqueTokens if not create it
    if($this->FnCheckIfTokenExistsInUniqueTokens($pFirstTokenContent.$pSecondTokenContent)){
    }
    else{
      $this->FnCreateNewUniqueToken($pFirstTokenContent.$pSecondTokenContent);
    }
    // go through all the records doing the merging
    for($RecordNumber=0;$RecordNumber<sizeof($this->Records);$RecordNumber++){
      $this->FnDoTheMergingInOneRecord($pFirstTokenContent,$pSecondTokenContent,$RecordNumber);
    }
  }

  function FnDoTheMergingInOneRecord($pFirstTokenContent,$pSecondTokenContent,$pRecordNumber){
    // we need to update the token object both the old one and the new one
    // we need to update the records array
    $pMergedPair=$pFirstTokenContent.$pSecondTokenContent;
    $FirstTokenPositionArray=$this->FnGetAllMergedPairPositionsInOneRecord($pMergedPair,$pRecordNumber);
    if($FirstTokenPositionArray===FALSE) return;
    for($i=0;$i<sizeof($FirstTokenPositionArray);$i++){
      $this->FnRemoveTokenFromAPositionInARecord($pFirstTokenContent,$FirstTokenPositionArray[$i],$pRecordNumber);
      $FirstTokenContentLength=strlen($pFirstTokenContent);
      $this->FnRemoveTokenFromAPositionInARecord($pSecondTokenContent,$FirstTokenPositionArray[$i]+$FirstTokenContentLength,$pRecordNumber);
      $this->FnAddTokenToAPositionInARecord($pFirstTokenContent.$pSecondTokenContent,$FirstTokenPositionArray[$i],$pRecordNumber);
    }
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
	$this->UniqueTokens[$pTokenContent]->Stats['NumberOfRecords']=0; // if u see NULL means occurs in no record. 0 means occurs in one record.
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

  function FnGetAllMergedPairPositionsInOneRecord($pMergedPair,$pRecordNumber){
    // lets go through each of the tokens in this record
    $MergePosition=FALSE;
    $RecordPointer=0;
    for($NumberOfUniqueTokens=0;$NumberOfUniqueTokens<sizeof($this->Records[$pRecordNumber]->Tokens);$NumberOfUniqueTokens++){
      $CurrentTokenContent=$this->Records[$pRecordNumber]->Tokens[$RecordPointer]->Content;
      $CurrentTokenContentLength=strlen($CurrentTokenContent);
      $NextTokenContent=$this->Records[$pRecordNumber]->Tokens[$RecordPointer+$CurrentTokenContentLength]->Content;
      if($CurrentTokenContent.$NextTokenContent==$pMergedPair){
	$MergePositionArray[]=$RecordPointer;
      }
      else{
      }
      $RecordPointer=$RecordPointer+$CurrentTokenContentLength;
    }
    return $MergePositionArray;
  }

  function FnInitializeTokenAndRecordObjects($pTokensInRecordArray,$pRecordNumber){

    global $ConfigObj;

    // Initializing the matrix with SOR and EOR each has never occurred so the conditional prob is 0
    $NumberOfTokensInRecord=NULL;
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
