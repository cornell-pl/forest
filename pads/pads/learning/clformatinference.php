<?
class ClFormatInference {

  function FnPrintFormat(&$pTokenManagerObj){
    $TemplateTokensWithIndexOfFollowing=array_keys($pTokenManagerObj->TokenIndexOfFollowing);
    $Template .="*";
    $Template .= $TemplateTokensWithIndexOfFollowing[0];
    $Template .="*";
    $Template .=$TemplateTokensWithIndexOfFollowing[2];
    echo "\n\n";
    echo $Template;
    echo "\n\n";
    return;
  }

  function FnDiscoverTokenOrdering(&$pTokenManagerObj){
    $this->FnCreateTokenOrderingConditionalProbabilityMatrix($pTokenManagerObj);
    return;
  }

  function FnDiscoverTemplateTokens(&$pTokenManagerObj){
    // from $pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix figure out those tokens which occur with the same relative positioning.
    $this->FnDiscoverTokenWithMaximumAndMinimumFollowing($pTokenManagerObj);
  }
  
  function FnDiscoverTokenWithMaximumAndMinimumFollowing(&$pTokenManagerObj){
    foreach($pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix as $FirstDimensionToken => $ProbabilityOfBeingFollowedByAToken){
      $SumOfProbability=0;
      foreach($ProbabilityOfBeingFollowedByAToken as $Token => $Probability){
	$SumOfProbability = $SumOfProbability+$Probability['Numerator'] / $Probability['Denominator'];
      }
      $pTokenManagerObj->TokenIndexOfFollowing[$FirstDimensionToken]=$SumOfProbability;
    }
  }

  function FnCreateTokenOrderingConditionalProbabilityMatrix(&$pTokenManagerObj){
    foreach($pTokenManagerObj->TokensWithConstantFrequencyAcrossRecords as $FirstDimensionToken => $Frequency){
      foreach($pTokenManagerObj->TokensWithConstantFrequencyAcrossRecords as $SecondDimensionToken => $DimensionFrequency){
	$pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix[$FirstDimensionToken][$SecondDimensionToken]['Numerator']=0;
	$pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix[$FirstDimensionToken][$SecondDimensionToken]['Denominator']=0;
	// lets loop through all the records and update the values in each of the cells
	for($pRecordNumber=0;$pRecordNumber<sizeof($pTokenManagerObj->Records);$pRecordNumber++){      
	  $FirstDimensionTokenPosition=$pTokenManagerObj->UniqueTokens[$FirstDimensionToken]->Records[$pRecordNumber][0];
	  $SecondDimensionTokenPosition=$pTokenManagerObj->UniqueTokens[$SecondDimensionToken]->Records[$pRecordNumber][0];
	  if($SecondDimensionTokenPosition>$FirstDimensionTokenPosition){
	    $pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix[$FirstDimensionToken][$SecondDimensionToken]['Numerator']++;
	    $pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix[$FirstDimensionToken][$SecondDimensionToken]['Denominator']++;
	  }
	  else{
	    $pTokenManagerObj->TokenOrderingConditionalProbabilityMatrix[$FirstDimensionToken][$SecondDimensionToken]['Denominator']++;
	  }
	}
      }
    }
  }
}
?>
