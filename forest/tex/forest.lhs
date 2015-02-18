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

%include formatting.lhs

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

software transactional memory building blocks

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

Notice that we can have multiple variables (with possibly different specs) ``connected'' to the same fragment of a filesystem.
this can cause inter

we have sort of a mismatch: transactional variables per type declaration VS fileinfo per directory/file.
the justification is that, since forest always fills default data for inexisting paths, the fileinfo actually determines whether a directory/file exists or not in the real FS. e.g., to delete a file we need to mark its fileinfo as invalid, and to create a file we need to define clean valid fileinfo for it.

\subsection{Validation}

forest dependencies and constraints that can not be statically checked.

\begin{spec}
	validate :: TxForest args ty rep => ty -> FTM ValidationErrors
\end{spec}

\subsection{Standard filesystem operations}

\begin{spec}
rm :: TxForest args ty rep => ty -> FTM ()
\end{spec}

remove a filepath by writing invalid fileinfo and default data to it.
to avoid loss of information, the default data needs to be precisely the one that is generated by forest.
if we are removing a directory, we need to make sure that its content is the empty list: e.g., a non-existing directory with content inside is not a valid snapshot of a FS, but a valid haskell value nonetheless.
cumbersome for arbitrary specs that touch multiple files/directories.
primitive operation that generates the appropriate default data.

\begin{spec}
cpOrElse :: TxForest args ty rep => ty -> ty -> b -> ([WriteErrors] -> FTM fs b) -> FTM fs b
\end{spec}

copy a forest specification.
copying a single file by hand is simple: read, copy the contents and update the fileinfo, write.
copying a directory...not so much...because we have to recursively copy each child variable and update its fileinfo accordingly.
primitive operation may fail, because the data that we are trying to write may not be consistent with the specification for the target arguments/path. e.g., a spec with a boolean argument that loads file x or y, with source arg = True and target arg = False.

\section{Implementation}

%read-only vs read-write: we only allow read-only expressions in Forest specifications.
%we need to have data/medata under the same variable because we issue stores on variable writes: writeData rep >> writeMeta md /= write (rep,md)

\subsection{Transactional Forest}

transactional semantics of STM: we log reads/writes to the filesystem instead of variables. global lock, no equality check on validation.
load/store semantics of Forest with thunks, explicit laziness

transactional variables created by calling load on its spec with given arguments and root path; lazy loading, so no actual reads occur.
Additionally to the representation data, each transactional variable remembers its creation-time arguments (they never change).


each transaction keeps a local filesystem version number, and a per-tvar log mapping fsversions to values, stored in a weaktable (fsversions are purgeable once a tx commits).

on writes: backup the current fslog, increment the fsversion, add an entry to the table for the (newfsversion,newvalue), run the store function for the new data and writing the modifications to the buffered FS; if there are errors, rollback to the backed-up FS and the previous fsversion.

the store function also changes the in-memory representation by recomputing the validation thunks (hidden to users) to match the new content.

write success theorem: if the current rep is in the image of load, then store succeeds

\subsection{Incremental Transactional Forest}



\subsection{Log-structured Transactional Forest}

\section{Evaluation}

\section{Related Work}

\section{Conclusions}

\bibliographystyle{abbrvnat}
\bibliography{forest}

\onecolumn
\appendix

\section {Forest Semantics}

\begin{align*}
	&|(star F (r/u)) = | \left\{
	\begin{array}{ll}
		|star F (prime r)| & \quad \text{if}~ |app F ((star F r) / u) = (i,Link (prime r))| \\
		|(star F r) / u| & \quad \text{otherwise}
	\end{array} \right. \\
	&|(star F cpath) = cpath|
\end{align*}

\begin{displaymath}
	\frac{}{|r `inSet` cpath|}
	\quad
	\frac{}{|r `inSet` r|}
	\quad
	\frac{|r `inSet` (prime r)|}{|r / u `inSet` (prime r)|}
\end{displaymath}

\begin{align*}
	|focusF F r `def`| F ||_{|{forall (prime r) (star F (prime r) `inSet` r)}|}
\end{align*}

\begin{spec}
	eqUnder F rs (prime F) = forin r rs (focusF F r = focusF (prime F) r)
\end{spec}

\begin{spec}
	Err a = (M Bool,a)
\end{spec}

\begin{tabular}{l||l||l}
	|s|			& |R s| & |C s| \\
	\hline
	|M s|		& |M (Err (R s))| & |M (Err (R s))| \\
	$k_{\tau_1}^{\tau_2}$ & |Err (tau2,tau1)| & |(tau2,tau1)| \\
	|e :: s|	& |R s| & |C s| \\
	|dpair x s1 s2| & |Err (R s1,R s2)| & |(C s1,C s2)| \\
	|flist s x e| & |Err [R s]| & |[C s]| \\
	|P(e)|		& |Err ()| & |()| \\
	|s?|		& |Err (Maybe (R s))| & |Maybe (C s)|
\end{tabular}

|R| is the internal in-memory representation type of a forest declaration;
|C| is the external type of content of a variables that users can inspect/modify

\begin{spec}
	app err a = doM { e <- getM a; (aerr,v) <- e; returnM aerr }
	app err (aerr,v) = returnM aerr
	
	app valid v = doM { aerr <- err v; eerr <- getM aerr; eerr }
\end{spec}

|v1 (sim oenv1 oenv2) v2| denotes value equivalence modulo memory addresses, under the given environments.
|e1 (sim oenv1 oenv2) e2| denotes expression equivalence by evaluation modulo memory addresses, under the given environments.

|v1 (simErr oenv1 oenv2) v2| denotes value equivalence (ignoring error information) modulo memory addresses, under the given environments.

$\boxed{|load oenv eenv r s F (prime oenv) v|}$

$\boxed{|s = M s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		a \notin |dom (oenv)| \quad |aerr `notin` dom oenv| \quad |e = pload eenv r (M s) F | \\
		|eerr = doM { e1 <- getM a; v1 <- e1; valid v1 }|
	\end{array}
	}{
		|load oenv eenv r (M s) F (extoenv2 oenv aerr eerr a e) (aerr,a)|
	}
\end{displaymath}

$\boxed{|s = k|}$

\begin{displaymath}
	\frac{
		|aerr `notin` dom oenv| \quad |meval oenv (loadk k eenv F r) (prime oenv) (b,v)| \quad
	}{
		|load oenv eenv r k F (extoenv (prime oenv) aerr (returnM b)) (aerr,v)|
	}
\end{displaymath}

\begin{displaymath}
	 |loadk File eenv F r| \left\{
	\begin{array}{ll}
		|returnM (True,(i,u))| & \quad \text{if}~ |app F (r) = (i,File u)| \\
		|returnM (False,(iinvalid,""))| & \quad \text{otherwise}
	\end{array} \right.
\end{displaymath}

\begin{displaymath}
	 |loadk Dir eenv F r| \left\{
	\begin{array}{ll}
		|returnM (True,(i,us))| & \quad \text{if}~ |app F (r) = (i,Dir us)| \\
		|returnM (False,(iinvalid,{}))| & \quad \text{otherwise}
	\end{array} \right.
\end{displaymath}

\begin{displaymath}
	 |loadk Link eenv F r| \left\{
	\begin{array}{ll}
		|returnM (True,(i,prime r))| & \quad \text{if}~ |app F (r) = (i,Link (prime r))| \\
		|returnM (False,(iinvalid,cpath))| & \quad \text{otherwise}
	\end{array} \right.
\end{displaymath}

$\boxed{|s = e :: s1|}$

\begin{displaymath}
	\frac{
		|meval oenv (sem (r / e) eenv Path) (prime oenv) (prime r)| \quad 
		|load oenv eenv (prime r) s F (prime2 oenv) v|
	}{
		|load oenv eenv r (e :: s) F (prime2 oenv) v|
	}
\end{displaymath}

$\boxed{|s = dpair x s1 s2|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|load oenv eenv r s1 F oenv1 v1| \\
		|load oenv1 (exteenv eenv x v1) r s2 F oenv2 v2|\\
		|eerr = doM { b1 <- app valid v1; b2 <- app valid v2; returnM (b1 `and` b2) } |
	\end{array}
	}{
		|load oenv eenv r (dpair x s1 s2) F (extoenv oenv2 aerr eerr) (aerr,(v1,v2))|
	}
\end{displaymath}

$\boxed{|s = P e|}$

\begin{displaymath}
	\frac{
		|aerr `notin` dom oenv|
	}{
		|load oenv eenv r (P e) F (extoenv oenv aerr (sem e eenv Bool)) (aerr,())|
	}
\end{displaymath}

$\boxed{|s = s1?|}$

\begin{displaymath}
	\frac{
		|r `notin` dom F| \quad |aerr `notin` dom oenv|
	}{
		|load oenv eenv r (s?) F (extoenv oenv aerr (returnM True)) (aerr,Nothing)|
	}
\end{displaymath}
\begin{displaymath}
	\frac{
		|r `inSet` dom F| \quad |aerr `notin` dom (prime oenv)| \quad |load oenv eenv r s F(prime oenv) v|
	}{
		|load oenv eenv r (s?) F (extoenv oenv aerr (app valid v)) (aerr,Just v)|
	}
\end{displaymath}

$\boxed{|s = flist s1 x e|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|aerr `notin` dom oenv| \quad
		|meval oenv (sem e eenv {tau}) (prime oenv) {t1,..,tk}| \\
		|meval (prime oenv) (forn 1 i k (doM { vi <- pload (exteenv eenv x ti) r s F; returnM (map ti vi) })) (prime2 oenv) vs|\\
		|eerr = forn 1 i k (doM { bi <- app valid (app vs ti); returnM (bigwedge bi) } )|
	\end{array}
	}{
		|load oenv eenv r (flist s x e) F (extoenv (prime2 oenv) aerr eerr) (aerr,vs)|
	}
\end{displaymath}

$\boxed{|store oenv eenv r s F v (prime oenv) (prime F) (prime phi)|}$

$\boxed{|s = M s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|app oenv a = e| \quad |meval oenv e (prime oenv) (aerr,v)| \\
		|store (prime oenv) eenv r s F v (prime2 oenv) (prime F) (prime phi)|
	\end{array}
	}{
		|store oenv eenv r (M s) F a (prime2 oenv) (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|s = k|}$

\begin{displaymath}
	\frac{
		|meval oenv (storek k eenv F r (d,v)) (prime oenv) (prime F,phi) |
	}{
		|store oenv eenv r k F (aerr,(d,v)) (prime oenv) (prime F) phi|
	}
\end{displaymath}

\begin{displaymath}
	 |storek File eenv F r (i,u)| \left\{
	\begin{array}{ll}
		|returnM (extF F r (i,File u),lambda (prime F) ( app (prime F) (r) = (i,File u)))| & \quad \text{if}~ |i `neq` iinvalid| \\
		|returnM (extF F r bot,lambda (prime F) (app (prime F) (r) `neq` (_,File _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) = (_,File _)| \\
		|returnM (F,lambda (prime F) (app (prime F) (r) `neq` (_,File _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) `neq` (_,File _)|
	\end{array} \right.
\end{displaymath}

\begin{displaymath}
	 |storek Dir eenv F r (i,{u1,...,un})| \left\{
	\begin{array}{ll}
		|returnM (extF F r (i,Dir {u1,...,un}),lambda (prime F) ( app (prime F) (r) = (i,Dir {u1,...,un}) ))| & \quad \text{if}~ |i `neq` iinvalid| \\
		|returnM (extF F r bot,lambda (prime F) (app (prime F) (r) `neq` (_,Dir _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) = (_,Dir _)| \\
		|returnM (F,lambda (prime F) (app (prime F) (r) `neq` (_,Dir _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) `neq` (_,Dir _)|
	\end{array} \right.
\end{displaymath}

\begin{displaymath}
	 |storek Link eenv F r (i,prime r)| \left\{
	\begin{array}{ll}
		|returnM (extF F r (i,Link (prime r)),lambda (prime F) ( app (prime F) (r) = (i,Link (prime r))))| & \quad \text{if}~ |i `neq` iinvalid| \\
		|returnM (extF F r bot,lambda (prime F) (app (prime F) (r) `neq` (_,Link _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) = (_,Link _)| \\
		|returnM (F,lambda (prime F) (app (prime F) (r) `neq` (_,Link _)))| & \quad \text{if}~ |i = iinvalid `and` app F (r) `neq` (_,Link _)|
	\end{array} \right.
\end{displaymath}

$\boxed{|s = e :: s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|meval oenv (sem e eenv Path) (prime oenv) (prime r)|\\
		|store (prime oenv) eenv (prime r) s F v (prime2 oenv) (prime F) (prime phi)|
	\end{array}
	}{
		|store oenv eenv r (e :: s) F v (prime2 oenv) (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|s = dpair x s1 s2|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|store oenv eenv r s1 F v1 oenv1 F1 phi1|\\
		|store oenv1 (exteenv eenv x v1) r s2 F v2 oenv2 F2 phi2|\\
		|phi = lambda (prime F) (app phi1 (prime F)) `and` (app phi2 (prime F))|
	\end{array}
	}{
		|store oenv eenv r (dpair x s1 s2) F (aerr,(v1,v2)) oenv2 (F1 `cat` F2) phi|
	}
\end{displaymath}

$\boxed{|s = P e|}$

\begin{displaymath}
	\frac{
		|phi = lambda (prime F) True|
	}{
		|store oenv eenv r (P e) F (aerr,()) oenv F phi|
	}
\end{displaymath}

$\boxed{|s = s1?|}$

\begin{displaymath}
	\frac{
		|phi = lambda (prime F) (r `notin` dom (prime F))|
	}{
		|store oenv eenv r (s?) F (aerr,Nothing) oenv (extF F r bot) phi|
	}
\end{displaymath}
\begin{displaymath}
	\frac{
	\begin{array}{c}
		|store oenv eenv r s F v (prime oenv) F1 phi1| \\
		|phi = lambda (prime F) (app phi1 (prime F) `and` r `inSet` dom (prime F))|
	\end{array}
	}{
		|store oenv eenv r (s?) F (aerr,Just v) oenv F1 phi|
	}
\end{displaymath}

$\boxed{|s = flist s1 x e|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|meval oenv (sem e eenv {tau}) (prime oenv) ts| \quad |vs = {t1 `mapsto` v1,...,tk `mapsto` vk}| \\
		|phi = lambda (prime F) (ts = {t1,...,tk} `and` bigwedge ( app phii (prime F) ))|\\
		|meval (prime oenv) (forn 1 i k (doM { (Fi,phii) <- (pstore (exteenv eenv x vi) r s F vi); returnM (F1 `cat` ... `cat` Fk,phi)} )) (prime2 oenv) (prime F) (prime phi)|
	\end{array}
	}{
		|store oenv eenv r (flist s x e) F (aerr,vs) oenv (prime F) (prime phi)|
	}
\end{displaymath}

%format (tyOf x t) = "{\vdash" x ":" t "}"

\begin{proposition}[Load Type Safety]
	If |load oenv eenv r s F (prime oenv) (prime v)| and |R s = tau| then |tyOf v tau|.
\end{proposition}

\begin{theorem}[LoadStore]
	If
	\begin{align*}
		|load oenv eenv r s F (prime oenv) v| \\
		|store (prime2 oenv) eenv r s F (prime v) (prime3 oenv) (prime F) (prime phi)|\\
		|v (simErr (prime oenv) (prime2 oenv)) (prime v)|
	\end{align*}
	then |F = prime F| and |app (prime phi) (prime F)|.
\end{theorem}

\begin{theorem}[StoreLoad]
	If
	\begin{align*}
		|store oenv eenv r s F v (prime oenv) (prime F) (prime phi)| \\
		|load (prime oenv) eenv r s F (prime2 oenv) (prime v)|
	\end{align*}
	then |app (prime phi) (prime F)| ~iff~ |v (simErr (prime oenv) (prime2 oenv)) (prime v)|
\end{theorem}

stronger than the original forest theorem: store validation only fails for impossible cases (when representation cannot be stored to the FS without loss)

weaker in that we don't track consistency of inner validation variables; equality of the values is modulo error information. in a real implementation we want to repair error information on storing, so that it is consistent with a subsequent load.

the error information is not stored back to the FS, so the validity predicate ignores it.

%\begin{lemma}[Load Non-Sharing]
%	All the memory addresses (of error and forest thunks) found by fully evaluating a the value in the result of load are distinct.
%	So that for a spec |dpair x ("a" :: M s) ("b" :: M s)| we never have |loadDelta (a,a) did|.
%\end{lemma}

\section{Forest Incremental Semantics}

Note that:
\begin{itemize}
	\item We have access to the old filelesystem, since filesystem deltas record the changes to be performed.
	\item We do not have access to the old environment, since variable deltas record the changes that already occurred.
\end{itemize}

%format dbotv = "{\delta_\bot}_v"
%format dbotvi = "{\delta_\bot}_{v_i}"

\begin{spec}
	df ::= addFile r u | addDir r | addLink r (prime r) | rem r | chgAttrs r i | df1 ; df2 | did
\end{spec}

\begin{spec}
	dv ::= dM da dv1 | dv1 `otimes` dv2 | map ti dbotvi | dv1? | did | ddelta
	dbotv ::= bot | dv
\end{spec}

\begin{spec}
	deltav ::= did | ddelta
\end{spec}

\begin{spec}
	(focus ((addFile (prime r) u)) F r) `def`			if (star F (prime r)) `inSet` (star F r) then addFile (prime r) u else did
	(focus ((addDir (prime r))) F r) `def`				if (star F (prime r)) `inSet` (star F r) then addDir (prime r) else did
	(focus ((addLink (prime r) (prime2 r))) F r) `def` 	if (star F (prime r)) `inSet` (star F r) then addLink (prime r) (prime2 r) else did
	(focus ((rem (prime r))) F r) `def` 				if (star F (prime r)) `inSet` (star F r) then rem (prime r) else did
	(focus ((chgAttrs (prime r) i)) F r) `def` 			if (star F (prime r)) `inSet` (star F r) then chgAttrs (prime r) i else did
	(focus (df1 ; df2) F r) `def` (focus df1 F r) ; focus df2 F1 r where F1 = ((focus df1 F r)) F
	(focus did F r) `def` did
\end{spec}


\begin{spec}
	 darrow v oenv dv (prime v) (prime oenv)
\end{spec}
the value delta maps |v| to |v'|

monadic expressions only read from the store and perform new allocations; they can't modify existing addresses.

For any expression application |e oenv = (prime oenv,v)|, we have |oenv = oenv `intersection` prime oenv|.

errors are computed in the background

\begin{displaymath}
	\frac{
		|(prime a) `notin` dom(oenv)|
	}{
		|mset oenv da deltae a e (extoenv oenv (prime a) e) (prime a) ddelta|
	}
	\quad
	\frac{
	}{
		|mset oenv did deltae a e (extoenv oenv a e) a ddelta|
	}
	\quad
	\frac{
	}{
		|mset oenv did did a e oenv a did|
	}
\end{displaymath}



$\boxed{|dload oenv eenv deenv r s F v df dv (prime oenv) (prime v) (prime deltav)|}$

\begin{displaymath}
	\frac{
		\Delta_\varepsilon ||_{fv(s)} = \emptyset
		\quad
		|focus df F r = did|
	}{
		|dload oenv eenv deenv r s F v df did oenv v did|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|load oenv eenv r s (df F) (prime oenv) (prime v)|
	}{
		|dload oenv eenv deenv r s F v df dv (prime oenv) (prime v) ddelta|
	}
\end{displaymath}

$\boxed{|s = M s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|app oenv (a) = e| \quad |meval oenv e (prime oenv) (aerr,v)|\\
		|dload (prime oenv) eenv deenv r s F v df dv (prime2 oenv) (prime v) deltav| \quad |v = prime v|
	\end{array}
	}{
		|dload oenv eenv deenv r (M s) F a df (dM did (did `otimes` dv)) (prime2 oenv) a did|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|app oenv (a) = e| \quad |meval oenv e (prime oenv) (aerr,v)|\\
		|dload (prime oenv) eenv deenv r s F v df dv oenv1 (prime v) deltav|\\

		|mset oenv1 daerr deltav aerr (valid (prime v)) oenv2 (prime aerr) deltaaerr|\\
		|mset oenv2 da deltaaerr a (returnM (prime aerr,prime v)) oenv3 (prime a) deltaa|
		
	\end{array}
	}{
		|dload oenv eenv deenv r (M s) F a df (dM da (daerr `otimes` dv)) oenv3 (prime a) deltaa|
	}
\end{displaymath}

$\boxed{|s = e :: s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		\Delta_\varepsilon ||_{fv(s)} = \emptyset \quad |meval oenv (sem (r / e) eenv Path) (prime oenv) (prime r)| \\ 
		|dload (prime oenv) eenv deenv (prime r) (e :: s) F v df dv (prime2 oenv) (prime v) deltav|
	\end{array}
	}{
		|dload oenv eenv deenv r (e :: s) F v df dv (prime2 oenv) (prime v) deltav|
	}
\end{displaymath}

$\boxed{|s = dpair x s1 s2|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|dload oenv eenv deenv r s1 F v1 df dv1 oenv1 (prime v1) deltav1|\\
		|dload oenv1 (exteenv eenv x (prime v1)) (exteenv deenv x deltav1) r s2 F v2 df dv2 oenv2 (prime v2) deltav2|\\
		|mset oenv2 daerr (deltav1 `and` deltav2) aerr (doM {b1 <- valid (prime v1); b2 <- valid (prime v2); returnM (b1 `and` b2) }) (prime oenv)(prime aerr) deltaaerr|
	\end{array}
	}{
		|dload oenv eenv deenv r (dpair x s1 s2) F (aerr,(v1,v2)) df (daerr `otimes` (dv1 `otimes` dv2)) (prime oenv) (prime aerr,(prime v1,prime v2)) deltaaerr|
	}
\end{displaymath}

$\boxed{|s = P e|}$

\begin{displaymath}
	\frac{
		\Delta_\varepsilon ||_{fv(e)} = \emptyset
	}{
		|dload oenv eenv deenv r (P e) F v df did oenv v did|
	}
\end{displaymath}

$\boxed{|s = s1?|}$

\begin{displaymath}
	\frac{
		|r `notin` dom (df F)| \quad
		|mset oenv daerr dv aerr (returnM True) (prime oenv) (prime aerr) deltaaerr|
	}{
		|dload oenv eenv deenv r (s?) F (aerr,Nothing) df (daerr `otimes` dv) (prime oenv) (aerr,Nothing) deltaaerr|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|r `inSet` dom (df F)| \quad
		|dload oenv eenv deenv r s F v df dv (prime oenv) (prime v) deltav| \\
		|mset oenv daerr deltav aerr (app valid (prime v)) (prime oenv) (prime aerr) deltaaerr|
	\end{array}
	}{
		|dload oenv eenv deenv r (s?) F (aerr,Just v) df (daerr `otimes` dv?) (prime oenv) (aerr,Just (prime v)) deltaaerr|
	}
\end{displaymath}

$\boxed{|s = flist s x e|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|meval oenv (sem e eenv {tau}) oenv1 {t1,...,tk}|\\
		|meval oenv1 (forn 1 i k (doM { (vi,deltavi) <- pdloadx eenv deenv r s F vs df dvs ; returnM (map ti vi,bigwedge deltavi) })) oenv2 (prime vs,deltavs)|\\
		|mset oenv2 daerr deltavs aerr (forn 1 i k (doM { bi <- app valid (app (prime vs) ti); returnM (bigwedge bi) } )) (prime oenv)(prime aerr) deltaaerr|
	\end{array}
	}{
		|dload oenv eenv deenv r (flist s x e) F (aerr,vs) df (daerr `otimes` dvs) (prime oenv) (prime aerr,prime vs) deltaaerr|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|t `inSet` dom (vs)| \quad
		|dload oenv (exteenv eenv x t) (exteenv deenv x did) r s F (app vs t) df (app dvs t) (prime oenv) (prime v) deltav|
	}{
		|dloadx oenv eenv deenv r s F (t,vs) df dvs (prime oenv) (prime v) deltav|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|t `notin` dom (vs)| \quad
		|load oenv eenv r s (df F) (prime oenv) (prime v)|
	}{
		|dloadx oenv eenv deenv r s F (t,vs) df dvs (prime oenv) (prime v) ddelta|
	}
\end{displaymath}

$\boxed{|dstore oenv eenv deenv r s F v df dv (prime oenv) (prime F) (prime phi)|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		\Delta_\varepsilon ||_{fv(s)} = \emptyset
		\quad
		|focus df F r = did| \\
		|sense oenv eenv r s v rs| \quad
		|phi = lambda (prime F) (eqUnder F rs (prime F))|
	\end{array}
	}{
		|dstore oenv eenv deenv r s F v df did oenv F phi|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|store oenv eenv r s (df F) v (prime oenv) (prime F) (prime phi)|
	}{
		|dstore oenv eenv deenv r s F v df dv (prime oenv) (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|s = M s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|app oenv (a) = e| \quad |meval oenv e (prime oenv) (aerr,v)|\\
		|dstore (prime oenv) eenv deenv r s F v df dv (prime2 oenv) (prime F) (prime phi)|
	\end{array}
	}{
		|dstore oenv eenv deenv r (M s) F a df (dM da (daerr `otimes` dv)) (prime2 oenv) (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|s = e :: s1|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		\Delta_\varepsilon ||_{fv(s)} = \emptyset \quad |meval oenv (sem (r / e) eenv Path) (prime oenv) (prime r)| \\ 
		|dstore (prime oenv) eenv deenv (prime r) (e :: s) F v df dv (prime2 oenv) (prime F) (prime phi)|
	\end{array}
	}{
		|dstore oenv eenv deenv r (e :: s) F v df dv (prime2 oenv) (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|s = dpair x s1 s2|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|dstore oenv eenv deenv r s1 F v1 df dv1 oenv1 (prime F1) (prime phi1)|\\
		|dstore oenv1 (exteenv eenv x v1) (exteenv deenv x dv1) r s2 F v2 df dv2 oenv2 (prime F2) (prime phi2)|\\
		|phi = lambda (prime F) (app (prime phi1) (prime F1) `and` app (prime phi2) (prime F2))|
	\end{array}
	}{
		|dstore oenv eenv deenv r (dpair x s1 s2) F (aerr,(v1,v2)) df (daerr `otimes` (dv1 `otimes` dv2)) oenv2 (F1 `cat` F2) phi|
	}
\end{displaymath}

$\boxed{|s = P e|}$

\begin{displaymath}
	\frac{
		|phi = lambda (prime F) (returnM True)|
	}{
		|dstore oenv eenv deenv r (P e) F v df dv oenv F phi|
	}
\end{displaymath}

$\boxed{|s = s1?|}$

\begin{displaymath}
	\frac{
		|r `notin` dom (df F)| \quad
		|phi = lambda (prime F) (r `notin` dom (prime F))|
	}{
		|dstore oenv eenv deenv r (s?) F (aerr,Nothing) df (daerr `otimes` did) oenv F phi|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|r `inSet` dom (df F)| \quad
		|dstore oenv eenv deenv r s F v df dv (prime oenv) F1 phi1| \\
		|phi = lambda (prime F) (app phi1 (prime F) `and` e `inSet` dom (prime F))|
	\end{array}
	}{
		|dstore oenv eenv deenv r (s?) F (aerr,Just v) df (daerr `otimes` dv?) (prime oenv) F1 phi|
	}
\end{displaymath}

$\boxed{|s = flist s x e|}$

\begin{displaymath}
	\frac{
	\begin{array}{c}
		|meval oenv (sem e eenv {tau}) (prime oenv) ts| \quad |vs = {t1 `mapsto` v1,...,tk `mapsto` vk}| \\
		|phi = lambda (prime F) (ts = {t1,...,tk} `and` bigwedge ( app phii (prime F) ))|\\
		|meval oenv1 (forin ti (dom vs) (doM { (Fi,phii) <- pdstore (exteenv eenv x ti) (exteenv deenv x did) r s F (app vs ti) df (app dvs ti) ; returnM (F1 `cat` ... `cat` Fk,phi) })) oenv2 (prime F,prime phi)|
	\end{array}
	}{
		|dstore oenv eenv deenv r (flist s x e) F (aerr,vs) df (daerr `otimes` dvs) oenv2 (prime F) (prime phi)|
	}
\end{displaymath}

$\boxed{|sense oenv eenv r s v rs|}$ ``Sensitivity of a forest specification in respect to a representation''

\begin{displaymath}
	\frac{
		|app oenv a = e|  \quad |meval oenv e (prime oenv) v| \quad
		|sense (prime oenv) eenv r s v rs|
	}{
		|sense oenv eenv r (M s) a rs|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|sense oenv eenv r s v rs|
	}{
		|sense oenv eenv r (e :: s) v ({r} `union` rs)|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|sense oenv eenv r s1 v1 rs1| \quad
		|sense oenv (exteenv eenv x v1) r s2 v2 rs2|
	}{
		|sense oenv eenv r (dpair x s1 s2) (aerr,(v1,v2)) (rs1 `union` rs2)|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
	}{
		|sense oenv eenv r (P e) v {}|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
	}{
		|sense oenv eenv r (s?) (aerr,Nothing) {r}|
	}
\end{displaymath}
\begin{displaymath}
	\frac{
		|sense oenv eenv r s v rs|
	}{
		|sense oenv eenv r (s?) (aerr,Just v) ({r} `union` rs)|
	}
\end{displaymath}

\begin{displaymath}
	\frac{
		|vs = {t1 `mapsto` v1,...,tk `mapsto` vk}| \quad
		|forn i 1 k (sense oenv (exteenv eenv x ti) r s vi ri)|
	}{
		|sense oenv eenv r (flist s x e) (aerr,vs) (bigunion ri)|
	}
\end{displaymath}

\begin{theorem}[Incremental Load Soundness]
	If
	\begin{align*}
		|load oenv eenv r s F1 oenv1 v1|\\
		|darrow v1 oenv1 dv1 (prime v1) oenv2|\\
		|dload oenv2 (prime eenv) deenv r s F1 (prime v1) df1 dv1 oenv3 v2 deltav1p|\\
		|load oenv1 (prime eenv) r s (df1 F1) oenv4 v3|
	\end{align*}
	then |v2 (simErr oenv3 oenv4) v3| and |(app valid v2) (simErr oenv3 oenv4) (app valid v3)|.
\end{theorem}

\begin{displaymath}
\xymatrix@@R=.7cm@@C=2cm{
	|F1| \ar@@{=>}[ddr]^{\mathtt{load}_\Delta}  \ar@@{~>}[d]_{|df1|} \ar[r]^{\mathtt{load}} & |v1| \ar@@{~>}[d]^{|dv1|} \\
	|F2| \ar[d]_{|id|} & |prime v1| \ar@@{~>}[d]^{|deltav1p|} \\
	|F2| \ar[r]^{\mathtt{load}} & |v2|
}
\end{displaymath}

\begin{lemma}[Incremental Load Stability]
	|dload oenv eenv deenv r (M s) F a df (dM did dv) (prime oenv) a deltaa|
\end{lemma}

\begin{theorem}[Incremental Store Soundness]
	If
	\begin{align*}
		|store oenv eenv r s F v1 oenv1 F1 phi1|\\
		|darrow v1 oenv1 dv1 v2 oenv2|\\
		|dstore oenv2 (prime eenv) deenv r s F1 v2 df1 dv1 oenv3 F2 phi2|\\
		|store oenv2 (prime eenv) r s (df1 F1) v2 oenv4 F3 phi3|
	\end{align*}
	then |F2 = F3| and |app phi2 F2 = app phi3 F3|.
\end{theorem}

\begin{displaymath}
\xymatrix@@R=.7cm@@C=2cm{
	|F| \ar@@{~>}[d]_{} & |v1| \ar@@{->}[d]^{|id|} \ar@@{->}[dl]^{\mathtt{store}} \\
	|F1| \ar@@{~>}[d]_{|df1|} \ar[r]_{\mathtt{load}} & |v1| \ar@@{~>}[d]^{|dv1|} \ar@@{=>}[ddl]_{\mathtt{store}_\Delta} \\
	|prime F1| \ar@@{~>}[d]_{} & |v2| \ar@@{->}[d]^{|id|} \ar@@{->}[dl]^{\mathtt{store}} \\
	|F2| \ar[r]_{\mathtt{load}} & |v2|
}
\end{displaymath}

\end{document}

