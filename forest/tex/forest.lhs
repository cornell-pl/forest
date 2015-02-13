\newif\ifdraft
\drafttrue
%\draftfalse

\documentclass{sigplanconf}

% The following \documentclass options may be useful:

% preprint      Remove this option only once the paper is in final form.
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage[utf8x]{inputenc}
\usepackage{amsmath, amsthm, amssymb, amsbsy}
\usepackage{microtype}
\usepackage[usenames,dvipsnames]{color}
\usepackage{caption}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{thmtools}
\usepackage{wrapfig}
\usepackage{stmaryrd}
\usepackage{listings}
\usepackage{cancel}
\usepackage[all]{xy}

%%MnSymbol symbols
\DeclareFontFamily{U}{MnSymbolC}{}
\DeclareSymbolFont{MnSyC}{U}{MnSymbolC}{m}{n}
\DeclareFontShape{U}{MnSymbolC}{m}{n}{
    <-6>  MnSymbolC5
   <6-7>  MnSymbolC6
   <7-8>  MnSymbolC7
   <8-9>  MnSymbolC8
   <9-10> MnSymbolC9
  <10->   MnSymbolC10}{}
\DeclareMathSymbol{\smalltriangledown}{\mathop}{MnSyC}{75}
\DeclareMathSymbol{\diamondplus}{\mathop}{MnSyC}{124}
\DeclareMathSymbol{\diamondtimes}{\mathop}{MnSyC}{125}
\DeclareMathSymbol{\diamonddot}{\mathop}{MnSyC}{126}
\DeclareMathSymbol{\filledmedtriangleup}{\mathop}{MnSyC}{201}

\makeatletter
\newcommand{\pushright}[1]{\ifmeasuring@@#1\else\omit\hfill$\displaystyle#1$\fi\ignorespaces}
\newcommand{\pushleft}[1]{\ifmeasuring@@#1\else\omit$\displaystyle#1$\hfill\fi\ignorespaces}
\makeatother

%include polycode.fmt

\ifdraft
\newcommand{\hugo}[1]{{\color{red} [#1]}}
\else
\newcommand{\hugo}[1]{}
\fi

\theoremstyle{theorem}
\newtheorem{proposition}{Proposition}
\theoremstyle{theorem}
\newtheorem{theorem}{Theorem}[section]
\theoremstyle{theorem}
\newtheorem{definition}{Definition}[section]
\theoremstyle{definition}
\newtheorem{lemma}{Lemma}

\begin{document}

\special{papersize=8.5in,11in}
\setlength{\pdfpageheight}{\paperheight}
\setlength{\pdfpagewidth}{\paperwidth}

\conferenceinfo{CONF 'yy}{Month d--d, 20yy, City, ST, Country} 
\copyrightyear{20yy} 
\copyrightdata{978-1-nnnn-nnnn-n/yy/mm} 
\doi{nnnnnnn.nnnnnnn}

% Uncomment one of the following two, if you are not going for the 
% traditional copyright transfer agreement.

%\exclusivelicense                % ACM gets exclusive license to publish, 
                                  % you retain copyright

%\permissiontopublish             % ACM gets nonexclusive license to publish
                                  % (paid open-access papers, 
                                  % short abstracts)

%\titlebanner{banner above paper title}        % These are ignored unless
%\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{TxForest: Composable Transactions over Filestores}

\authorinfo{Forest Team}
           {Cornell, TUFTS}
           {hpacheco@@cs.cornell.edu}
\maketitle

\begin{abstract}

\end{abstract}

%\category{CR-number}{subcategory}{third-level}

%\terms
%term1, term2

\keywords


\section{Introduction}

transactional use cases:
batch changes on group of files (process all the files in a directory)
software upgrade (rollback)
concurrent file access (multiple processes writing to the same log file)
filesystem as a database (ACID guarantees)

transactional filesystems
\url{http://www.fuzzy.cz/en/articles/transactional-file-systems}
\url{http://www.fsl.cs.sunysb.edu/docs/valor/valor_fast2009.pdf}
\url{http://www.fsl.cs.sunysb.edu/docs/amino-tos06/amino.pdf}



libraries for transactional file operations:
\url{http://commons.apache.org/proper/commons-transaction/file/index.html}
\url{https://xadisk.java.net/}
\url{https://transactionalfilemgr.codeplex.com/}

tx file-level operations (copy,create,delete,move,write)
schema somehow equivalent to using the unstructured universal Forest representation

but what about data manipulation: transactional maps,etc?

\section{Examples}

\section{The Forest Language}

the forest description types

a forest description defines a structured representation of a semi-structured filestore.

\section{Forest Transactions}

The key goal of this paper is to make Forest~\cite{Forest} transactional.
As an embedded DSL in Haskell, we borrow the elegant software transactional memory (STM) interface from its host language.

\subsection{Composable transactions}

software transactional memory blocks

\begin{spec}
	-- running transactions
	atomically	:: FTM a -> IO a
	
	-- blocking
	retry		:: FTM a
	
	-- nested transactions
	orElse		:: FTM a -> FTM a -> FTM a
	
	-- exceptions
	throw		:: Exception e => e -> FTM a
	catch 		:: Exception e => FTM a -> (e -> FTM a) -> FTM a
\end{spec}

|FTM a| denotes a transactional action that returns a value of type |a|.
Complex transactions can be defined by composing |FTM| actions, and run |atomically| as an |IO| action.
In Haskell, |IO| is the type of non-revocable I/O operations, including reading/writing to files.

arbitrary pure code

\subsection{Transactional variables}

the forest programming style draws no distinction between data represented on disk and in memory.

The transactional forest compiler generates several Haskell types and functions from every forest type declaration, aggregated as an instance of the |TxForest| class:

programmers can manipulate in-memory representations as if they were working on the filestore itself.

each user-declared forest type |ty| corresponds to a transactional variable that holds a representation of type |rep|.

\begin{spec}
	class TxForest args ty rep where
		new				:: args -> FilePath -> FTM fs ty
		read			:: ty -> FTM rep
		writeOrElse		:: ty -> rep -> b -> (WriteErrors -> FTM fs b) -> FTM fs b
\end{spec}

|new| creates a new forest transactional variable for the specification found in |TxForest| context, with arbitrary arguments and a root path.
|read| the associated fragment of the filesytem into an in-memory representation data structure.
Users can manipulate these structures as they would in regular Haskell programs, and eventually perform FS modifications by writing new representation to a transactional variable. writes may fail if the provided data is not a faithful representation of the filestore for the specification under consideration.

|WriteErrors| have nothing to do with transactional errors and account for the inconsistencies that can arise when programmer attempt to write an erroneous in-memory representation to the filestore.
e.g., attempting to write conflicting data to the same file. more on this later in sec...

the rep of a variable may itself contain other variables. simple programming example.

Notice that we can have multiple variables (with possibly different specs) ``connected'' to the same filepath.

we have sort of a mismatch: transactional variables per type declaration VS fileinfo per directory/file.
the justification is that, since forest always fills default data for inexisting paths, the fileinfo actually determines whether a directory/file exists or not in the real FS. e.g., to delete a file we need to mark its fileinfo as invalid, and to create a file we need to define clean valid fileinfo for it.

\subsection{Validation}

forest dependencies and constraints that can not be statically checked.

\begin{spec}
	validate :: Forest args rep a => rep -> FTM ValidationErrors
\end{spec}

%inner/outer layer = read-only vs read/writes
%we need to have data/medata under the same variable because we issue stores on variable writes: writeData rep >> writeMeta md /= write (rep,md)

\bibliographystyle{abbrvnat}
\bibliography{forest}

\end{document}

