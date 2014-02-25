/* **************************************
 * PADS GUI project (preliminary tests)
 * Tool generation event handling code
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  ToolProcessAcc
 *  ToolProcessFmt
 *  ToolProcessXml
 *  ToolGetNonterminalNames
 *  ExecPADSC
 *  ExecCCTest
 *  ExecCCTool
 *  CgGenerateCodeFromParams
 * *********************************** */

void LaunchPADS::ToolProcessAcc(int whereFrom)
{
  wxString str;
  wxString str2;

  wxString includePath;
  wxString includeName;
  wxString filePath;
  wxString fileName;
  wxString openFile = "";
  wxString saveFile = "";
  wxArrayString includeDirs;

  int tpdReturn;
  wxString tpdLabel = _T("Select tasks for accumulator generation/execution system");
  ToolProcessDialog *tpd = new ToolProcessDialog(this, 
						 wxID_ANY, 
						 _T("Select Tool Tasks"), 
						 this,
						 tpdLabel,
						 &tGenCode,
						 &tCompileCode,
						 &tRunTool);

  tpdReturn = tpd->ShowModal();
  tpd->~ToolProcessDialog();

  if(tpdReturn == wxID_CANCEL)
    return;


  if(tGenCode)
    {
      if(whereFrom == TP_FROM_MENU ||
	 (workingFilePath.IsEmpty() || workingFileName.IsEmpty()))
	{
	  wxFileDialog openFileDialog(this, 
				      "Open header file",
				      "",
				      "",
				      HEADER_FILETYPES,
				      wxOPEN | 
				      wxFILE_MUST_EXIST,	
				      wxDefaultPosition);
	  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      str  = openFileDialog.GetPath();
	      str2 = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      else
	{
	  DB_P("workingfp: \"%s\", workingfn:\"%s\"\n", workingFilePath.c_str(), workingFileName.c_str());
	  str.Printf(_T("%s%s.h"), workingFilePath.c_str(), workingFileName.c_str());
	  str2.Printf(_T("%s.h"), workingFileName.c_str());
	}

      wxArrayString nontermStrs;
      ToolGetNonterminalNames(nontermStrs, whereFrom != TP_FROM_MENU, str);

      AccWizard *AccWiz = new AccWizard(this, *wizBitmap, AccStrings, str, str2, nontermStrs);
      DB_P("trying to run wizard...\n");
      if(AccWiz->wRunWizard())
	{
	  wxFileDialog saveFileDialog(this, 
				      "Save generated c code", 
				      "",
				      "",
				      C_FILETYPES,
				      wxSAVE |
				      wxOVERWRITE_PROMPT,
				      wxDefaultPosition);

	  if(saveFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = saveFileDialog.GetPath();
	      fileName = saveFileDialog.GetFilename();
	      AccWiz->wGetStrings(AccStrings, includePath, includeName);
	      CgGenerateCodeFromParams(CG_ACC,
				       filePath,    fileName,
				       includePath, includeName);
	    }
	  else
	    {
	      AccWiz->Destroy();
	      return;
	    }
	}
      else
	{
	  AccWiz->Destroy();
	  return;
	}
      AccWiz->Destroy();
    }

  if(tCompileCode)
    {
      if(!tGenCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open c code for accumulator compilation",
				      "",
				      "",
				      C_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;

	  wxFileDialog openFileDialog2(this,
				       "Open header file from padsc generated library",
				       "",
				       "",
				       HEADER_FILETYPES,
				       wxOPEN |
				       wxFILE_MUST_EXIST,
				       wxDefaultPosition);
	  if(openFileDialog2.ShowModal() == wxID_OK)
	    {
	      includePath = openFileDialog2.GetPath();
	      includeName = openFileDialog2.GetFilename();
	    }
	  else
	    return;
	}

      int iPathStart = filePath.Find(fileName.c_str());
      str = filePath.Left(iPathStart);
      includeDirs.Add(str);
      DB_P("ipath start string: %s\n", includeDirs.Item(0).c_str());
      str = PADSCinc;
      str.Append(_T("template/"));
      includeDirs.Add(str);
      ExecCCTool(EXEC_COMPILE, filePath, fileName, openFile, saveFile, includeDirs, includePath);
    }

  if(tRunTool)
    {

      if(!tCompileCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open accumulator program",
				      "",
				      "",
				      ANY_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      wxFileDialog openFileDialog(this, 
				  "Open input data file",
				  "",
				  "",
				  IMPORT_FILETYPES,
				  wxOPEN,
				  wxDefaultPosition);  
      if(openFileDialog.ShowModal() == wxID_OK)
	{
	  // the accumulator only needs an input file
	  /*
	    wxFileDialog saveFileDialog (this, 
	    "Save output file",
	    "",
	    "",
	    IMPORT_FILETYPES,
	    wxSAVE | wxOVERWRITE_PROMPT,
	    wxDefaultPosition);
	    if(saveFileDialog.ShowModal() == wxID_OK)
	    {*/

	  DB_P("execing tool name %s\n", filePath.c_str());
	  openFile = openFileDialog.GetPath();
	  saveFile = "";//saveFileDialog.GetPath();
	  ExecCCTool(EXEC_USE_INPUT, filePath, fileName, openFile, saveFile, includeDirs, includePath);
	  //}
	}
      else
	return;
    }
}

void LaunchPADS::ToolProcessFmt(int whereFrom)
{
  wxString str;
  wxString str2;

  wxString includePath;
  wxString includeName;
  wxString filePath;
  wxString fileName;
  wxString openFile = "";
  wxString saveFile = "";
  wxArrayString includeDirs;

  int tpdReturn;
  wxString tpdLabel = _T("Select tasks for format generation/execution system");
  ToolProcessDialog *tpd = new ToolProcessDialog(this, 
						 wxID_ANY, 
						 _T("Select Tool Tasks"), 
						 this,
						 tpdLabel,
						 &tGenCode,
						 &tCompileCode,
						 &tRunTool);

  tpdReturn = tpd->ShowModal();
  tpd->~ToolProcessDialog();

  if(tpdReturn == wxID_CANCEL)
    return;


  if(tGenCode)
    {
      if(whereFrom == TP_FROM_MENU ||
	 (workingFilePath.IsEmpty() || workingFileName.IsEmpty()))
	{
	  wxFileDialog openFileDialog(this, 
				      "Open header file",
				      "",
				      "",
				      HEADER_FILETYPES,
				      wxOPEN | 
				      wxFILE_MUST_EXIST,	
				      wxDefaultPosition);
	  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      str  = openFileDialog.GetPath();
	      str2 = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      else
	{
	  DB_P("workingfp: \"%s\", workingfn:\"%s\"\n", workingFilePath.c_str(), workingFileName.c_str());
	  str.Printf(_T("%s%s.h"), workingFilePath.c_str(), workingFileName.c_str());
	  str2.Printf(_T("%s.h"), workingFileName.c_str());
	}

      wxArrayString nontermStrs;
      ToolGetNonterminalNames(nontermStrs, whereFrom != TP_FROM_MENU, str);

      FmtWizard *FmtWiz = new FmtWizard(this, *wizBitmap, FmtStrings, str, str2, nontermStrs);
      DB_P("trying to run wizard...\n");
      if(FmtWiz->wRunWizard())
	{
	  wxFileDialog saveFileDialog(this, 
				      "Save generated c code", 
				      "",
				      "",
				      C_FILETYPES,
				      wxSAVE |
				      wxOVERWRITE_PROMPT,
				      wxDefaultPosition);

	  if(saveFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = saveFileDialog.GetPath();
	      fileName = saveFileDialog.GetFilename();
	      FmtWiz->wGetStrings(FmtStrings, includePath, includeName);
	      CgGenerateCodeFromParams(CG_FMT,
				       filePath,    fileName,
				       includePath, includeName);
	    }
	  else
	    {
	      FmtWiz->Destroy();
	      return;
	    }
	}
      else
	{
	  FmtWiz->Destroy();
	  return;
	}
      FmtWiz->Destroy();
    }

  if(tCompileCode)
    {
      if(!tGenCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open c code for formatter compilation",
				      "",
				      "",
				      C_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;

	  wxFileDialog openFileDialog2(this,
				       "Open header file from padsc generated library",
				       "",
				       "",
				       HEADER_FILETYPES,
				       wxOPEN |
				       wxFILE_MUST_EXIST,
				       wxDefaultPosition);
	  if(openFileDialog2.ShowModal() == wxID_OK)
	    {
	      includePath = openFileDialog2.GetPath();
	      includeName = openFileDialog2.GetFilename();
	    }
	  else
	    return;
	}

      int iPathStart = filePath.Find(fileName.c_str());
      str = filePath.Left(iPathStart);
      includeDirs.Add(str);
      DB_P("ipath start string: %s\n", includeDirs.Item(0).c_str());
      str = PADSCinc;
      str.Append(_T("template/"));
      includeDirs.Add(str);
      ExecCCTool(EXEC_COMPILE, filePath, fileName, openFile, saveFile, includeDirs, includePath);
    }

  if(tRunTool)
    {

      if(!tCompileCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open formatter program",
				      "",
				      "",
				      ANY_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      wxFileDialog openFileDialog(this, 
				  "Open input data file",
				  "",
				  "",
				  IMPORT_FILETYPES,
				  wxOPEN,
				  wxDefaultPosition);  
      if(openFileDialog.ShowModal() == wxID_OK)
	{
	  
	  wxFileDialog saveFileDialog (this, 
				       "Save output file",
				       "",
				       "",
				       IMPORT_FILETYPES,
				       wxSAVE | wxOVERWRITE_PROMPT,
				       wxDefaultPosition);
	  if(saveFileDialog.ShowModal() == wxID_OK)
	    {
	      DB_P("execing tool name %s\n", filePath.c_str());
	      openFile = openFileDialog.GetPath();
	      saveFile = saveFileDialog.GetPath();
	      ExecCCTool(EXEC_USE_IO, filePath, fileName, openFile, saveFile, includeDirs, includePath);
	  }
	}
      else
	return;
    }
}

void LaunchPADS::ToolProcessXml(int whereFrom)
{
  wxString str;
  wxString str2;

  wxString includePath;
  wxString includeName;
  wxString filePath;
  wxString fileName;
  wxString openFile = "";
  wxString saveFile = "";
  wxArrayString includeDirs;

  int tpdReturn;
  wxString tpdLabel = _T("Select tasks for XML converter generation/execution system");
  ToolProcessDialog *tpd = new ToolProcessDialog(this, 
						 wxID_ANY, 
						 _T("Select Tool Tasks"), 
						 this,
						 tpdLabel,
						 &tGenCode,
						 &tCompileCode,
						 &tRunTool);

  tpdReturn = tpd->ShowModal();
  tpd->~ToolProcessDialog();

  if(tpdReturn == wxID_CANCEL)
    return;


  if(tGenCode)
    {
      if(whereFrom == TP_FROM_MENU ||
	 (workingFilePath.IsEmpty() || workingFileName.IsEmpty()))
	{
	  wxFileDialog openFileDialog(this, 
				      "Open header file",
				      "",
				      "",
				      HEADER_FILETYPES,
				      wxOPEN | 
				      wxFILE_MUST_EXIST,	
				      wxDefaultPosition);
	  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      str  = openFileDialog.GetPath();
	      str2 = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      else
	{
	  DB_P("workingfp: \"%s\", workingfn:\"%s\"\n", workingFilePath.c_str(), workingFileName.c_str());
	  str.Printf(_T("%s%s.h"), workingFilePath.c_str(), workingFileName.c_str());
	  str2.Printf(_T("%s.h"), workingFileName.c_str());
	}

      wxArrayString nontermStrs;
      ToolGetNonterminalNames(nontermStrs, whereFrom != TP_FROM_MENU, str);

      XmlWizard *XmlWiz = new XmlWizard(this, *wizBitmap, XmlStrings, str, str2, nontermStrs);
      DB_P("trying to run wizard...\n");
      if(XmlWiz->wRunWizard())
	{
	  wxFileDialog saveFileDialog(this, 
				      "Save generated c code", 
				      "",
				      "",
				      C_FILETYPES,
				      wxSAVE |
				      wxOVERWRITE_PROMPT,
				      wxDefaultPosition);

	  if(saveFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = saveFileDialog.GetPath();
	      fileName = saveFileDialog.GetFilename();
	      XmlWiz->wGetStrings(XmlStrings, includePath, includeName);
	      CgGenerateCodeFromParams(CG_XML,
				       filePath,    fileName,
				       includePath, includeName);
	    }
	  else
	    {
	      XmlWiz->Destroy();
	      return;
	    }
	}
      else
	{
	  XmlWiz->Destroy();
	  return;
	}
      XmlWiz->Destroy();
    }

  if(tCompileCode)
    {
      if(!tGenCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open c code for XML converter compilation",
				      "",
				      "",
				      C_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);  
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;

	  wxFileDialog openFileDialog2(this,
				       "Open header file from padsc generated library",
				       "",
				       "",
				       HEADER_FILETYPES,
				       wxOPEN |
				       wxFILE_MUST_EXIST,
				       wxDefaultPosition);
	  if(openFileDialog2.ShowModal() == wxID_OK)
	    {
	      includePath = openFileDialog2.GetPath();
	      includeName = openFileDialog2.GetFilename();
	    }
	  else
	    return;
	}

      int iPathStart = filePath.Find(fileName.c_str());
      str = filePath.Left(iPathStart);
      includeDirs.Add(str);
      DB_P("ipath start string: %s\n", includeDirs.Item(0).c_str());
      str = PADSCinc;
      str.Append(_T("template/"));
      includeDirs.Add(str);
      ExecCCTool(EXEC_COMPILE, filePath, fileName, openFile, saveFile, includeDirs, includePath);
    }

  if(tRunTool)
    {

      if(!tCompileCode)
	{
	  wxFileDialog openFileDialog(this, 
				      "Open XML converter program",
				      "",
				      "",
				      ANY_FILETYPES,
				      wxOPEN,
				      wxDefaultPosition);
	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      filePath = openFileDialog.GetPath();
	      fileName = openFileDialog.GetFilename();
	    }
	  else
	    return;
	}
      wxFileDialog openFileDialog(this, 
				  "Open input data file",
				  "",
				  "",
				  IMPORT_FILETYPES,
				  wxOPEN,
				  wxDefaultPosition);  
      if(openFileDialog.ShowModal() == wxID_OK)
	{
	  
	  wxFileDialog saveFileDialog (this, 
				       "Save output XML file",
				       "",
				       "",
				       XML_FILETYPES,
				       wxSAVE | wxOVERWRITE_PROMPT,
				       wxDefaultPosition);
	  if(saveFileDialog.ShowModal() == wxID_OK)
	    {
	      DB_P("execing tool name %s\n", filePath.c_str());
	      openFile = openFileDialog.GetPath();
	      saveFile = saveFileDialog.GetPath();
	      ExecCCTool(EXEC_USE_IO, filePath, fileName, openFile, saveFile, includeDirs, includePath);
	  }
	}
      else
	return;
    }
}

bool LaunchPADS::ToolGetNonterminalNames(wxArrayString& arrStr, bool useMadeCode,
					 wxString& headerPath)
{
  // coppied from ExecPADSC
  wxRegEx endEx(_T("\\.h$"));
  
  wxString namePathStr; // path/name
  wxString nameStr; // just name
  wxString pathStr; // just path

  arrStr.Insert(_T(""), 0);

  if(!endEx.IsValid())
    {
      DB_P("ToolGetNonterminalNames endEx failed!\n");
      return false;
    }

  wxString defPath = headerPath;

  if(endEx.Replace(&defPath, _T(".p"), 1) < 1)
    {
      defPath.Append(_T(".p"));
    }

  /*  
  if(useMadeCode == true) // catch the useMadeCode flag and make sure that's what the user wants
    {
      if(wxMessageBox(_T("Do you want to use the PADS definition most recently saved from the code view?"), 
		      _T("Confirm Definition Usage"),
		      wxICON_QUESTION | wxYES_NO) == wxNO)
	{
	  useMadeCode = false;
	}
    }
  */

  if(useMadeCode == true && bottomCodeMade == true)
    {
      pathStr = bottomCodePath;

      if(!wxFile::Access(pathStr, wxFile::read))
	{
	  wxLogError(_T("Selected PADS definition file could not be openend."));
	  return false;
	}
    }
  else
    {

      bool useFoundFile = false;
      if(wxFile::Access(defPath, wxFile::read))
	{
	  pathStr = defPath;
	  wxString messageString;
	  messageString.Printf(_T("Wizard found PADS definition to match header.  Generate nonterminal list from:\n \"%s\"?"), defPath.c_str());
	  if(wxMessageBox(messageString, 
			  _T("Confirm Definition Usage"),
			  wxICON_QUESTION | wxYES_NO) == wxYES)
	    {
	      useFoundFile = true;
	    }
	 
	}
      
      if(useFoundFile == false)
	{      
	  wxFileDialog openFileDialog (this, 
				       "Select PADS definition from which to generate nonterminal name list",
				       "",
				       "",
				       CODE_FILETYPES,
				       wxOPEN
				       | wxFILE_MUST_EXIST,
				       wxDefaultPosition);

	  if(openFileDialog.ShowModal() == wxID_OK)
	    {
	      pathStr = openFileDialog.GetPath();
	      
	      if(!wxFile::Access(pathStr, wxFile::read))
		{
		  wxLogError(_T("Selected PADS definition file could not be openend."));
		  return false;
		}
	    }
	  else
	    {
	      return false;
	    }
	}
    }

  wxFFile fd(pathStr);
  if(fd.Length() > (MAX_FILE_READ_SIZE))
    {
      wxLogError(_T("Selected PADS definition is too large to read."));
      return false;
    }

  wxString fileText;
  if(!fd.ReadAll(&fileText))
    {
      wxLogError(_T("Selected PADS definition could not be read."));
      return false;
    }

  wxRegEx reNonterm(_T(P_FIND_RECORD_NONTERMINAL_NAMES));
  if(!reNonterm.IsValid())
    {
      DB_P("ERROR: regular expression reNonterm is invalid!\n");
      return false;
    }

  int matchCount;
  wxString matchString;

  if(!reNonterm.Matches(fileText))
  {
    wxLogError(_T("Cannot find nonterminal elements in selected PADS definition file."));
    return false;
  }

  arrStr.Clear();

  while(reNonterm.Matches(fileText))
    {
      matchCount = reNonterm.GetMatchCount();
      DB_P("reNonterm matchCount = %d\n", matchCount);

      if(matchCount == 0)
	{
	  DB_P("ERROR: matchCount == 0\n");
	  break;
	}

      matchString = reNonterm.GetMatch(fileText, 2);
      DB_P("new matchString = \"%s\"\n", matchString.c_str());	  

      arrStr.Insert(matchString, 0);

      reNonterm.ReplaceFirst(&fileText, _T(""));
    }

  return true;
}


void LaunchPADS::ExecPADSC(bool useGeneratedCode)
{
  wxRegEx endEx(_T("\\.p$"));
  wxRegEx pathEx(_T("[^/]+$"));
  
  wxString namePathStr; // path/name
  wxString nameStr; // just name
  wxString pathStr; // just path

  if(bottomCodeMade == true && useGeneratedCode == true)
    {
      nameStr = bottomCodeName;
      pathStr = bottomCodePath;
      namePathStr = bottomCodePath;

      if(!wxFile::Access(pathStr, wxFile::read))
	{
	  wxLogError(_T("Selected pads definition file could not be openend."));
	  return;
	}
    }
  else
    {
      wxFileDialog openFileDialog (this, 
				   "Select PADS code to compile",
				   "",
				   "",
				   CODE_FILETYPES,
				   wxOPEN
				   | wxFILE_MUST_EXIST,
				   wxDefaultPosition);

      if(openFileDialog.ShowModal() == wxID_OK)
	{
	  nameStr = openFileDialog.GetFilename();
	  pathStr = openFileDialog.GetPath();
	  namePathStr = pathStr;

	  if(!wxFile::Access(pathStr, wxFile::read))
	    {
	      wxLogError(_T("Selected pads definition file could not be openend."));
	      return;
	    }
	}
      else
	{
	  return;
	}
    }

  DB_P("save path: %s\n", bottomCodePath.c_str());
  if(!endEx.IsValid())
    {
      wxLogError(_T("Matching expression is invalid."));
      return;
    }
  if(!endEx.Matches(nameStr))
    {
      wxLogError(_T("File is not a valid PADS definition file (must end in \".p\" without trailing spaces)."));
      return;
    }
  endEx.Replace(&nameStr, _T(""), 1);
  DB_P("Name string: %s\n", nameStr.c_str());
  workingFileName = nameStr;
  pathEx.Replace(&pathStr, _T(""), 1);
  DB_P("Path string: %s\n", pathStr.c_str());
  workingFilePath = pathStr;

  DB_P("%s %s\n", PADSCcomppath.c_str(), namePathStr.c_str());

  wxArrayString cmds;
  wxString str;
  str.Printf(_T("%s %s\n"), PADSCcomppath.c_str(), namePathStr.c_str());
  cmds.Add(str);

  ShellDialog shell(this, wxID_ANY, _T("Tools I/O"), this, cmds);
  shell.ShowModal();
}

void LaunchPADS::ExecCCTest(void)
{
  wxString str = lpCode_Test;
  wxFFile file("lp_HelloWorld.c", "w");
  if(!file.IsOpened())
    {
      wxLogError("Couldn't open HelloWorld.c\n");
    }
  
  file.Write(str);
  file.Close();

  wxArrayString cmds;
  str.Printf(_T("%s %s %s\n"), CCpath.c_str(), CCargs.c_str(), _T("-o lp_HelloWorld lp_HelloWorld.c"));
  cmds.Add(str);
  str.Printf(_T("%s\n"), _T("./lp_HelloWorld"));
  cmds.Add(str);

  ShellDialog shell(this, wxID_ANY, _T("Tools I/O"), this, cmds);
  shell.ShowModal();

}

void LaunchPADS::ExecCCTool(int numArgs, 
			    wxString &filePath,
			    wxString &fileName,
			    wxString &inputPath,
			    wxString &outputPath,
			    wxArrayString &includeDirs, 
			    wxString &incPath)
{
  wxString str;
  wxArrayString cmds;
  wxString execName;
  wxString includePath;
  wxString libNames = "";
  wxString cFileName = incPath;

  wxRegEx endEx(_T("\\.c$"));
  wxRegEx hEndEx(_T("\\.h$"));
  wxRegEx pathEx(_T("[^/]+$"));

  
  if((numArgs & EXEC_COMPILE) != 0)
    {
      if(!endEx.IsValid())
	{
	  wxLogError(_T("Matching expression is invalid."));
	  return;
	}
      if(!endEx.Matches(filePath))
	{
	  wxLogError(_T("Cannot compile - file %s is not a valid C file (must end in \".c\" without trailing spaces)."), fileName.c_str());
	  return;
	}
      
      hEndEx.ReplaceFirst(&cFileName, _T(".c"));
      
      execName = filePath;
      endEx.ReplaceFirst(&execName, _T(""));
      DB_P("new exec name = %s\n", execName.c_str());
      
      //int pathEnd = filePath.Find(fileName.c_str());
      //includePath = filePath.Left(pathEnd);
      int includesCount = includeDirs.GetCount();
      for(int i = 0; i < includesCount; i++)
	{
	  includePath.Append(_T("-I"));
	  includePath.Append(includeDirs.Item(i).c_str());
	  includePath.Append(_T(" "));
	}
      
      // don't forget the top level padsc include directory
      includePath.Append(_T("-I"));
      includePath.Append(PADSCinc);
      includePath.Append(_T(" "));
      
      includePath.Append(_T("-I"));
      includePath.Append(ASTincludedir);
      includePath.Append(_T(" "));
      
      includePath.Append(_T("-I"));
      includePath.Append(_T("."));
      includePath.Append(_T(" "));
      
      //  includePath.Append(_T("-L"));
      //includePath.Append(LIBdir);
      //includePath.Append(_T(" "));
      
      for(int i = 0; i < P_NUM_LIBS_FOR_CC; i++)
	{
	  libNames.Append(LIBdir);
	  libNames.Append(PADS_libnames[i]);
	  libNames.Append(_T(" "));
	}
      
      str.Printf(_T("%s %s %s %s %s %s %s %s\n"), 
		 CCpath.c_str(), 
		 CCargs.c_str(),
		 includePath.c_str(),
		 _T("-o"),
		 execName.c_str(),
		 filePath.c_str(),
		 cFileName.c_str(),
		 libNames.c_str()
		 );
      cmds.Add(str);
      
    }

  if((numArgs & ~EXEC_COMPILE) != 0) // other flags are raised
    {
      if((numArgs & EXEC_COMPILE) == 0) 
	{
	  // set exec name if it wasn't set in the compilation
	  execName = filePath;
	  if(endEx.Matches(execName)) // check to make sure we don't have leftover file extensions
	    {
	      endEx.ReplaceFirst(&execName, _T(""));
	    }

	  DB_P("exec name on no compile: %s\n", execName.c_str());
	}

      if(numArgs == EXEC_USE_IO)
	str.Printf(_T("%s %s %s\n"), execName.c_str(), inputPath.c_str(), outputPath.c_str());
      else
	str.Printf(_T("%s %s\n"), execName.c_str(), inputPath.c_str());
      cmds.Add(str);
    }
  
  ShellDialog shell(this, wxID_ANY, _T("Tools I/O"), this, cmds);
  shell.ShowModal();

}


/* ************************************ */
bool LaunchPADS::CgGenerateCodeFromParams(int genType,
					  wxString &filePath,
					  wxString &fileName,
					  wxString &includePath, 
					  wxString &includeName)
{
  wxString paramCmp = "";
  wxString thisParam = "";
  wxString genCode = "";
  int paramCount;

  if(genType == CG_ACC)
    {
      paramCount = AccStrings.GetCount();
      for(int i = 0; i < paramCount; i++)
	{
	  if(!AccStrings.Item(i).IsEmpty())
	    {
	      thisParam.Printf("#define %s %s\n", 
			       lpAccum_Labels[i], 
			       AccStrings.Item(i).c_str());
	      paramCmp.Append(thisParam.c_str());
	    }
	}
      genCode.Printf(lpCode_Accum, paramCmp.c_str(), includeName.c_str());
      DB_P("%s\n", genCode.c_str());
      wxFFile codeFile(filePath.c_str(), "w");
      if(!codeFile.IsOpened())
	{
	  wxLogError("Couldn't open autogenerated C file %s.", filePath.c_str());
	  return false;
	}
      codeFile.Write(genCode);
      codeFile.Close();
      DB_P("Accumulator successfully generated at %s\n", filePath.c_str());
      return true;
    }

  if(genType == CG_XML)
    {
      paramCount = XmlStrings.GetCount();
      for(int i = 0; i < paramCount; i++)
	{
	  if(!XmlStrings.Item(i).IsEmpty())
	    {
	      thisParam.Printf("#define %s %s\n", 
			       lpXml_Labels[i], 
			       XmlStrings.Item(i).c_str());
	      paramCmp.Append(thisParam.c_str());
	    }
	}
      genCode.Printf(lpCode_Xml, paramCmp.c_str(), includeName.c_str());
      DB_P("%s\n", genCode.c_str());
      wxFFile codeFile(filePath.c_str(), "w");
      if(!codeFile.IsOpened())
	{
	  wxLogError("Couldn't open autogenerated C file %s.", filePath.c_str());
	  return false;
	}
      codeFile.Write(genCode);
      codeFile.Close();
      DB_P("XML converter successfully generated at %s\n", filePath.c_str());
      return true;
    }

  return false;
}

/* ************************************ */
