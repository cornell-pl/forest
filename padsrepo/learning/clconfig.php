<?
class ClConfig {
  var $VerboseLevel;
  var $DebugLevel;

  function ClConfig(){
    $this->VerboseLevel=0;
    $this->DebugLevel=0;
  }

  function FnGetVerboseLevel(){
    return $this->VerboseLevel;
  }

  function FnGetDebugLevel(){
    return $this->DebugLevel;
  }
}
?>
