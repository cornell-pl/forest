<?
function FnTokenize($pBuffer)
{
  $BufferLength=NULL;
  $Char=NULL;
  $TokenArray=NULL;
  $BufferLength=strlen($pBuffer);
  for ($i=0; $i<$BufferLength; $i++) {
    $Char = substr($pBuffer, $i, 1);
    $TokenArray[]=$Char;
  }
  return ($TokenArray);
}

function FnGetFileName($argv)
{
  if(!$_SERVER['DOCUMENT_ROOT']){ // command line execution 
    if(!$argv[1]){
      echo "This program expects a filename as the first paramater \r\n";
      exit;
    }
    else{
      $FileName=$argv[1];
    }
  }
  elseif($_SERVER['DOCUMENT_ROOT']){ // executed through the web
    if(!$_GET['filename']){
      echo "This program expects a filename sent through get when invoked from the web \r\n";
      exit;
    }
    else{
      $FileName=$_GET['filename'];
    }
  }


  if(!file_exists($FileName)){
      echo "The file $FileName does not exist \r\n";
  }
  return $FileName;
}

?>
