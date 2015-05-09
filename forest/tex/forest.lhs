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
\newcommand{\jonathan}[1]{{\color{blue} [#1]}}
\newcommand{\nate}[1]{{\color{green} [#1]}}
\newcommand{\kathleen}[1]{{\color{yellow} [#1]}}
\else
\newcommand{\hugo}[1]{}
\newcommand{\jonathan}[1]{}
\newcommand{\nate}[1]{}
\newcommand{\kathleen}[1]{}
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

\title{TxForest: Composable Memory Transactions over Filestores}

\authorinfo{Forest Team}
           {Cornell, TUFTS}
           {forest@@cs.cornell.edu}
\maketitle

\begin{abstract}

\end{abstract}

%\category{CR-number}{subcategory}{third-level}

%\terms
%term1, term2

\keywords

%include formatting.lhs

\section{Introduction}

Databases are a long-standing, effective technology for storing structured and semi-structured data. Using a database has many benefits, including transactions and access to rich set of data manipulation languages and toolkits.

downsides: heavy legacy, relational model is not always adequate

excerpt from~\cite{DBFS}:
Although database systems are equipped with more advanced and
secure data management features such as transactional atomicity,
consistency, durability, manageability, and availability, lack of
high performance and throughput scalability for storage of
unstructured objects, and absence of standard filesystem-based
application program interfaces have been cited as primary reasons
for content management providers to often prefer existing
filesystems or devise filesystem-like solutions for unstructured
objects.

cheaper and simpler alternative: store data directly as a collection of files, directories and symbolic links in a traditional filesystem.

examples of filesystems as databases

filesystems fall short for a number of reasons

Forest~\cite{forest} made a solid step into solving this, by offering an integrated programming environment for specifying and managing filestores.

Although promising, the old Forest suffered two essential shortcomings:
\begin{itemize}
	\item It did not offer the level of transparency of a typical DBMS. Users don't get to believe that they are working directly on the database (filesystem). they explicitly issue load/store calls, and instead manipulate in-memory representations and the filesystem independently. offline synchronization. offers a load/store interface, placing full responsibility for explicitly managing the filesystem with the application.
	\item It provided none of the transactional guarantees familiar from databases. transactions are nice: prevent concurrency and failure problems. successful transactions are guaranteed to run in serial order and failing transactions rollback as if they never occurred. rely on extra programmers' to avoid the hazards of concurrent updates. different hacks and tricks like creating lock files and storing data in temporary locations, that severely increase the complexity of the applications. writing concurrent programs is notoriously hard to get right. even more in the presence of laziness (original forest used the generally unsound Haskell lazy I/O)
\end{itemize}

TxForest offers filestores, materialized in-memory views of fragments of a filesystem structured onto a given schema, that are kept up-to-date as transactions modify filestores (and implicitly the underlying filesystem).

transactional filesystem use cases:

a directory has a group of files that must be processed and deleted and having the aggregate result written to another file.

software upgrade (rollback),

concurrent file access (beautiful account example?)

Specific use cases:
LHC\\
Network logs\\
Dan's scientific data

what filesystem API gives me true transactionality? this is, TxForest is currently a library, but we would need to be able to lock concurrent modifications to shared resources at commit time; to compute all symlinks under a path.
advantages of TxForest over DBs: the user gets to declare the schema according to which he wants to read the data; he chooses the abstraction, instead of a fixed tuple store.

\section{Examples}

\section{The Forest Language}

the forest description types

a forest description defines a structured representation of a semi-structured filestore.

each Forest declaraction is interpreted as:
an expected on-disk shape of a filesystem fragment
a transactional variable
an ordinary Haskell type for the in-memory representation that represents the content of a variable

two expression quotations: non-monadic |(e)| vs monadic |<||e||>|

|FileInfo| for directories/files/symlinks.

\begin{figure}
\begin{spec}
[pads| data Balance = Balance Int |]

[forest|
	type Accounts = [ a :: Account | a <- matches (GL "*") ]
	type Account = File Balance
|]
\end{spec}
\label{fig:accounts}
\end{figure}

\begin{figure}
\begin{spec}

data Balance = ...
type Balance_md = ...

data Accounts
instance TxForest () Accounts (FileInfo,[(FilePath,Account)]) where ...

data Account
instance TxForest () Account ((FileInfo,Balance_md),Balance) where ...
\end{spec}
\label{fig:accountsHaskell}
\end{figure}

\begin{figure}
\begin{spec}
[forest|
	type Universal_d = Directory 
             { ascii_files  is [ f :: TextFile     | f <- matches (GL "*"), (kind  f_att == AsciiK) ]
             , binary_files is [ b :: BinaryFile   | b <- matches (GL "*"), (kind  b_att == BinaryK) ]
             , directories  is [ d :: Universal_d  | d <- matches (GL "*"), (kind  d_att == DirectoryK) ]
             , symlinks     is [ s :: Link         | s <- matches (GL "*"), (isJust (symLink s_att)) ]
             }
|]
\end{spec}
\label{fig:universal}
\end{figure}

\section{Forest Transactions}

The Forest description language introduced in the previous section describes how to specify the expected shape of a filestore as an allegorical Haskell type, independently from the concrete programming artifacts that are used to manipulate such filestores.
We now focus on the key goal of this paper: the design of the Transactional Forest interface.

As we shall see, TxForest (for short) offers an elegant and powerful abstraction for concurrently manipulating structured filestores.
We first describe general-purpose transactional facilities~(Section~\ref{subsec:composable}).
We then introduce transactional forest variables that allow programmers to interact with filestores~(Section~\ref{subsec:tvars}).
We briefly touch on how programmers can verify, at any time, if a filestore conforms to its specification~(Section~\ref{subsec:validation}), and finish by introducing analogues of standard filesystem operations over filestores~(Section~\ref{subsec:fsops}).

\subsection{Composable transactions}
\label{subsec:composable}

As an embedded domain-specific language in Haskell, the inspiration for TxForest is the widely popular \emph{software transactional memory} (\texttt{STM}) Haskell library, that provides a small set of composable operations to define the key components of a transaction. We now explain the intuition of each one of these mechanisms, cast in the context of TxForest.

\paragraph{Running transactions}

In TxForest, one runs a transaction by calling the |atomic| function with type:\footnote{For the original \texttt{STM} interface~\cite{HaskellSTM}, substitute |FTM| by |STM|.}
\begin{spec}
	atomic :: FTM a -> IO a
\end{spec}
It receives a forest memory transaction, of type |FTM a|, and produces an |IO a| action that executes the transaction atomically with respect to all other concurrent transactions, returning a result of type |a|.
In the pure functional language Haskell, |FTM| and |IO| are called monads. Different monads are typically used to characterize different classes of computational effects.
|IO| is the primitive Haskell monad for performing irrevocable I/O actions, including reading/writing to files or to mutable references, managing threads, etc.
For example, consider the Haskell prelude functions:
\begin{spec}
	getChar :: IO Char
	putChar :: Char -> IO ()
\end{spec}
These respectively read a character from the standard input and write a character to the standard output.

Conversely, our |FTM| monad denotes computations that are tentative, in the sense that they happen inside the scope of a transaction and can always be rolled back.
As we discuss in the remainder of this section, these consist of STM-like transactional combinators, filesystem operations on Forest filestores, or arbitrary pure functions.
Note that, since |FTM| and |IO| are different types, the Haskell type system effectively prevents non-transactional actions from being run inside of a transaction. This is a valuable guarantee and one that is not commonly found in transactional libraries for mainstream programming languages lacking a very expressive type system.

\paragraph{Blocking transactions}

To allow a transaction to \emph{block} on a resource, TxForest provides a |retry| operation with type:
\begin{spec}
retry :: FTM a
\end{spec}
Conceptually, |retry| cancels the current transaction, without emitting any errors, and schedules it to be retried at a later time.
Since each transaction logs all the reads/writes that it performs on a filestore, an efficient implementation waits for another transaction to update the shared filestore fragments read by the blocked transaction before retrying.

Using |retry| we can define a pattern for conditional transactions that waits on a condition to be verified before performing an action:
\begin{spec}
wait :: FTM Bool -> FTM a -> FTM a
wait p a = do { b <- p ; if b then retry else a }
\end{spec}
Note that |wait| does not require a cycle; the transactional semantics handles consecutive retries.

%NOTE by Jonathan: Not sure his is necessary anymore given your description above Hugo.
%All of the reads in a transaction are logged and when |retry| is called,
%it blocks until another transaction writes to a file from the read log before restarting the
%transaction from scratch.

\paragraph{Composing transactions}

Multiple transactions can be sequentially composed via the standard |do| notation. For example, we can write:
\begin{spec}
	do { x <- ftm1; fmt2 x }
\end{spec}
This runs a transaction |ftm1 : FTM a| and passes its result to a transaction |ftm2 :: a -> FTM b|. Since the whole computation is itself a transaction, it will be performed indivisibly inside an |atomic| block.

We can also compose transactions as \emph{alternatives}, using the |orElse| primitive:
\begin{spec}
	orElse :: FTM a -> FTM a -> FTM a
\end{spec}
This combinator performs a left-biased choice: It first runs transaction |ftm1|, tries |ftm2| if |ftm1| retries, and the whole transaction retries if |ftm2| retries.
%ToDo by Jonathan: Make sure it's the whole transaction, not just the action. Apparently orElse semantics are a bit funky.
For example, it might be used to read either one of two files depending on the current configuration of the filesystem.

Note that |orElse| provides an elegant mechanism for nested transactions. At any point inside a larger transaction, we can tentatively perform a transaction |ftm1| and rollback to the beginning (of the nested transaction) to try |ftm2| in case |ftm1| retries:
\begin{spec}
do { ... ; orElse ftm1 ftm2; ... }
\end{spec}

\paragraph{Exceptions}

The last general-purpose feature of |FTM| transactions are \emph{exceptions}. In Haskell, both built-in and user-defined exceptions are used to signal error conditions. We can |throw| and |catch| exceptions in the |FTM| monad in the same way as the |IO| monad:
\begin{spec}
	throw :: Exception e => e -> FTM a
	catch :: Exception e => FTM a -> (e -> FTM a) -> FTM a
\end{spec}

For instance, a TxForest user may define a new |FileNotFound| exception and write the following pseudo-code:
\begin{spec}
tryRead = do
	{ exists <- ...find file...
	; if (not exists) then throw FileNotFound else return ()
	; ...read file... }
\end{spec}
If the file in question is not found, then a |FileNotFound| exception is thrown, aborting the current |atomic| block (and hence the file is never read).
Programmers can prevent the transaction from being aborted, and its effects discarded, by catching exceptions inside the transaction, e.g.:
\begin{spec}
	catch tryRead
		(\FileNotFound -> return ...default...) tryRead
\end{spec}

\subsection{Transactional variables}
\label{subsec:tvars}

We have seen how to build transactions from smaller transactional blocks, but we still haven't seen concrete operations to manipulate \emph{shared data}, a fundamental piece of any transactional mechanism.
In vanilla Haskell STM, communication between threads is done via shared mutable memory cells called \emph{transactional variables}.
For a transaction to log all memory effects, transactional variables can only be explicitly created, read from or written to using specific transactional operations. Nevertheless, Haskell programmers can traverse, query and manipulate the content of transactional variables using the rich language of purely functional computations; since these don't have side-effects, they don't ever need to be logged or rolled back.

In the context of TxForest, shared data is not stored in-memory, but instead on the filestore. It is illuminating to quote the STM paper~\cite{HaskellSTM}:
\begin{quote}
``We study internal concurrency between threads interacting through memory [...]; we do not consider here the questions of external interaction through storage systems or databases.''
\end{quote}
We consider precisely the question of external interaction with a filesystem.
Two transactions may communicate, e.g., by reading from or writing to the same file or possibly a list of files within a directory.
To facilitate this interaction, the TxForest compiler generates an instance of the |TxForest| type class (and corresponding types) for each Forest declaration:
\begin{spec}
class TxForest args ty rep | ty -> rep, ty -> args where
	new             ::  args -> FilePath -> FTM fs ty
	read            ::  ty -> FTM rep
	writeOrElse     ::  ty -> rep -> b
	                -> (Manifest -> FTM fs b) ->  FTM fs b
\end{spec}
In this signature, |ty| is an opaque transactional variable type that uniquely identifies a user-declared Forest type. Each transactional variables provides a window into the filesystem, shaped as a plain Haskell representation type |rep|.
The representation type closely follows the declared Forest type, with additional file-content metadata for directories, files and symbolic links; directories have representations of type |(FileInfo,dir_rep)| and basic types have representations of type |((FileInfo,base_md),base_rep)|, for base representation |base_rep| and metadata |base_md|.

\hugo{Shall we instead have type/data declaractions as in Pads, where only data declarations create new tx variables, to provide users more control? E.g., we currently have to create variables for top-level directory records since these need to be declared as top-level types.}

\paragraph{Creation}
The transactional forest programming style makes no distinction between data on the filesystem and in-memory.
Anywhere inside a transaction, users can declare a |new| transactional variable, with argument data pertaining to the forest declaration and rooted at the argument path in the filesystem.
This operation does not have any effect on the filesystem and just establishes the schema to which a filestore should conform.

\paragraph{Reading}
Users can |read| data from a filestore by reading the contents of a transactional variable.
Imagine that we want to retrieve the balance of a particular account from a directory of accounts as specified in Figure~\ref{fig:accounts}:
\begin{spec}
do
	accs :: Accounts <- new () "/var/db/accounts"
	(accs_info,accs_rep) <- read accs
	let acc1 :: Account = fromJust (lookup "account1" accs_rep)
	((acc1_info,acc1_md),Balance balance) <- read acc1
	return balance
\end{spec}
The corresponding generated Haskell functions and types appear in Figure~\ref{fig:accountsHaskell}.
In the background, this is done by lazily traversing the directories, files and symbolic links mentioned in the top-level forest description. The second line reads the account directory and generates a list of accounts, which can be manipulated with standard list operations to find the desired account.
An account is itself a transactional variable, which can be read in the same way. Note that the file holding the balance of |"account1"| is only read in the fourth line.
The type signatures elucidate the type of each transactional variable.
%NOTE by JD to Hugo: What type signatures?

Programmers can control the degree of laziness in a forest description by adjusting the granularity of Forest declarations.
For instance, if we have chosen to inline the type of |Account| in the description as follows:
\begin{spec}
[forest|
	type Accounts = [ a :: File Balance | a <- matches (GL "*") ]
|]
\end{spec}
Then reading the accounts directory would also read the file content of all accounts, since the balance of each account would not be encapsulated behind a transactional variable (as in Figure~\ref{fig:accounts}.

\paragraph{Writing}
Users can modify a filestore by writing new content to a transactional variable.
The |writeOrElse| function accepts additional arguments to handle possible conflicts, which may arise due to data dependencies in the Forest description that cannot be statically checked by the type system. If these dependencies are not met, the data is not a valid representation of a filestore.
If the write succeeds, the filesystem is updated with the new data and a default value of type |b| is returned.
If the write fails, a user-supplied alternate function is executed instead. The function takes a |Manifest| describing the tentative modifications to the filesystem and a report of the inconsistencies.
We can easily define more convenient derived forms of |writeOrElse|:
\begin{spec}
-- optional write
tryWrite :: TxForest args ty rep => ty -> rep -> FTM ()
tryWrite t v = writeOrElse t v () (const (return ()))
-- write or restart the transaction
writeOrRetry :: TxForest args ty rep => ty -> rep -> () -> FTM ()
writeOrRetry t v = writeOrElse t v () (const retry)
-- write or yield an error
writeOrThrow :: (TxForest args ty rep,Exception e) => ty -> rep -> () -> e -> FTM ()
writeOrThrow t v e = writeOrElse t v () (const (throw e))
\end{spec}
A typical example of an inconsistent representation is when a Forest description refers to the same file more than once, to describe it in multiple ways, and the user attempts to write conflicting data in each occurrence. For instance, in Forest we may describe a symbolic link to an ASCII file both as a |SymLink| and a |Text| file:
\begin{spec}
type Folder = Directory {
	{  link   is "README"  :: SymLink
	,  notes  is "README"  :: FFile Text
	,  ... }
\end{spec}
Here, the file information for the |link| and |notes| fields must match.
%For instance, in the universal description in Figure~\ref{fig:universal}, a symbolic link to an ASCII file in the same directory is mapped both under the |ascii_files| and |symlinks| fields.

Akin to reactive environments like (bidirectional) spreadsheets~\cite{Macedo:14}, each write takes \emph{immediate} effect on the (transactional snapshot of the) filesystem: an update on a variable is automatically propagated to the filesystem, eventually triggering the update of other variables dependent on common parts of the filesystem.
We can observe this data flow by defining two accounts pointing to the same file and writing to one of them:
\begin{spec}
	acc1 :: Account <- new () "/var/db/accounts/account"
	acc2 :: Account <- new () "/var/db/accounts/account"
	(acc_md,Balance balance) <- read acc2
	tryWrite acc1 (acc_md,Balance (balance + 1)) 
	(acc_md',Balance balance') <- read acc2
\end{spec}
By incrementing the balance of |acc1|, we are implicitly incrementing the balance of |acc2| (if the write succeeds, then |balance' = balance + 1|).
Note that even if we attempt to write different balances to each variable, in sequence:
\begin{spec}
	tryWrite acc1 (acc_md,Balance 10) 
	tryWrite acc2 (acc_md,Balance 20) 
\end{spec}
it is always the case that |acc1| and |acc2| have the same balance, and there is no inconsistency since the first write propagates to both variables before the second write occurs.

\subsection{Validation}
\label{subsec:validation}

As Forest lays a structured view on top of a semi-structured filesystem, a filestore does not need to conform perfectly to an associated Forest description.
Behind the scenes, TxForest lazily computes a summary of such discrepancies. These may flag, for example, that a mandatory file does not exist or an arbitrarily complex user-defined Forest constraint is not satisfied.
Validation is not performed unless explicitly demanded by the user. At any point, a user can |validate| a transactional variable and its underlying filestore:
\begin{spec}	
	validate :: TxForest args ty rep => ty -> FTM ForestErr
\end{spec}
The returned |ForestErr| reports a top-level error count and the topmost error message:
\begin{spec}
	data ForestErr = ForestErr
		{  numErrors  :: Int
		,  errorMsg   :: Maybe ErrMsg }
\end{spec}

We can always make validation mandatory and validation errors fatal by encapsulating any error inside a |ForestError| exception:
\begin{spec}
validRead :: TxForest args ty rep => ty -> FTM rep
validRead ty = do
	rep <- read ty
	err <- validate ty
	if numErrors err == 0
		then return rep
		else throw (ForestError err)
\end{spec}

\subsection{Standard filesystem operations}
\label{subsec:fsops}

To better understand the TxForest interface, we now discuss how to perform common operations on a Forest filestore.

\paragraph{Creation/Deletion}
Given that validation errors are not fatal, a |read| always returns a representation. For example, if a user tries to read the balance of a non-existent account:
\begin{spec}
do
	badAcc :: Account <- new () "/var/db/accounts/account"
	(acc_info,Balance balance) <- read badAcc
\end{spec}
then |acc_info| will hold invalid file information and |balance| a default value (implemented as |0| for |Int| values).
Perhaps less intuitive is how to create a new account; we create a new variable (that if read would hold default data) and write new valid file information and some balance:
\begin{spec}
newAccount path balance = do
	newAcc :: Account <- new () path
	tryWrite newAcc (validFileInfo path,Balance balance)
\end{spec}
Deleting an account is dual to creating one; we write invalid file information and the default balance to the corresponding variable:
\begin{spec}
delAccount acc = do
	tryWrite acc (invalidFile,Balance 0)
\end{spec}
The takeaway lesson is that the |FileInfo| metadata actually determines whether a directory, file or symbolic link exists or not in the filesystem, since we cannot infer that from the data alone (e.g., an empty account has the same balance as a non-existent account).
This also reveals less obvious data dependencies: For valid paths the |fullpath| in the metadata must match the path to which the representation corresponds in the description, and for invalid paths the representation data must match the Forest-generated default data.
Since this can become cumbersome to ensure manually, we provide a general function that conveniently removes a filestore, named after the POSIX \verb|rm| operation:
\begin{spec}
rm :: TxForest args ty rep => ty -> FTM ()
\end{spec}

\paragraph{Copying}

A user can copy an account from a source path to a target path as follows:
\begin{spec}
copyAccount srcpath tgtpath = do
	src :: Account <- new () srcpath
	tgt :: Account <- new () tgtpath
	(info,balance) <- read src
	tryWrite tgt (info { fullpath = tgtpath },balance)
\end{spec}
The pattern is to create a variable for each path, and copy the content with an updated |fullpath|.
Copying a directory of accounts follows the same pattern but is more complicated, in that we also have to recursively copy underlying accounts and update all the metadata accordingly.
Therefore, we provide an analogue to the POSIX \verb|cp| operation that attempts to copy the content of a representation into another:
\begin{spec}
cpOrElse  ::  TxForest args ty rep => ty -> ty -> b
          ->  (Manifest -> rep -> FTM fs b) -> FTM fs b
\end{spec}
Unlike |rm|, |copyOrElse| is only a best-effort operation that may fail due to arbitrarily complex data dependencies in the Forest description. Such dependencies necessarily hold in the source representation for the source arguments but may not for the target arguments.
Similarly to |writeOrElse|, we provide |tryCopy|, |copyOrRetry| and |copyOrThrow| operations with the expected type signatures.

For an example of what might go wrong while copying, consider the following description for accounts parameterized by a template name:
\begin{spec}
[forest|
	type NamedAccounts (acc :: String) =
		[ a :: Account | a <- matches (GL (acc ++ "*")) ]
|]
\end{spec}
This specification has an implicit data dependency that all the account files listed in the in-memory representation have names matching the Glob pattern.
Thus, trying to copy between filestores with different templates would effectively fail, as in:
\begin{spec}
do
	src :: Accounts <- new "account" "/var/db/accounts"
	tgt :: Accounts <- new "acc" "/var/db/accs"
	tryCopy src tgt 
\end{spec}

\subsection{Read-only transactions}

explain the need for monadic quotation and give an example

however, this introduces problems 
therefore embedded expressions are read-only, to prevent side-effects during loads/stores.

transactional operations (|retry|,|orElse|,|atomic|) are done in write mode (|FTM RW a|).

|read| and |new| in any mode (|FTM mode a|).

|FTM RO a| can only read data from the fs

|FTM RW a| can write data to the fs

expressions statically restricted to just perform a series of new/read operations.

for implementation purposes, it also allows the transaction manager to distinguish read-only transactions

\section{Implementation}

We now delve into how Transactional Forest can be efficiently implemented.
The current implementation is available from the project website (\url{forestproj.org}) and is done completely in Haskell.
We split our presentation into three possible designs, with increasing levels of incremental support and complexity.

%read-only vs read-write: we only allow read-only expressions in Forest specifications.
%we need to have data/medata under the same variable because we issue stores on variable writes: writeData rep >> writeMeta md /= write (rep,md)

\subsection{Transactional Forest}

\paragraph{Original STM interface}
We have implemented TxForest as a domain-specific variant of \texttt{STM} Haskell~\cite{HaskellSTM}, and inherit the same transactional mechanism based on \emph{optimistic concurrency control}: each transaction runs in a (possibly) different thread and keeps a private log of reads and writes (including the tentatively-written data) to \emph{shared resources}. Reads within a transaction first consult its log so that they see preceding writes. Once finished, each transaction validates its log against previous transactions that committed after its starting time and, only if no write-read conflicts are detected, commits its writes permanently; otherwise, it is re-executed.
These validate-and-commit operations are guaranteed to run |atomic|ally with respect to all other threads by relying on per-shared-resource locks (no locks are used during the transaction's execution): the transaction waits on the sorted sequence of read resources to be free (to ensure that it sees the commits of concurrently writing transactions) and acquires the sorted sequence of written resources.
They are \emph{disjoint-access parallel} (meaning that transactions with non-overlapping writes run in parallel) and \emph{read parallel} (meaning that transactions that only read from the same resources run in parallel).
These wait-and-acquire sequences are repeatedly attempted atomically, without interruption from the Haskell scheduler, and implemented over GHC's lightweight concurrency substrate~\cite{HaskellLWC}.

Blocking transactions (|retry|) validate their log and register themselves in wait-queues attached to each read resource while updating transactions unblock any pending waiters.
Nested transactions (|orElse|) work similarly to normal transactions except that writes are recorded only to a nested log and reads consult both the nested logs and those of all enclosing transactions. Validating a nested transaction also entails validating all enclosing transactions.
If the first alternative retries, then the second alternative is attempted; if both retry, then both logs are validated (along with the enclosing transaction) and the thread will wait on the union of the read resources.
Exceptional transactions (|throw|) must also validate the log before raising an exception to the outside world; on success, they rollback all modifications except for newly-created transactional variables; on failure, they retry.
A more detailed account of the general STM interface, including a complete formal semantics, is given in~\cite{HaskellSTM}.

\paragraph{Transaction logs}
The main difference from \texttt{STM} Haskell to TxForest is that the shared resources are not mutable memory cells in the traditional sense, but paths in a filesystem.
\footnote{STM maintains a log with the old value held in a memory cell and the new value written to it by the transaction, and validation tests if they are pointer-equal. We do not remember the old content of file paths, nor test for equality.}
This is to say that, although users manipulate structured filestores, all the in-memory data structures are local to each transaction, and only filesystem operations need to be logged for commit.

The concurrent handling of file paths, however, is subtle in the presence of symbolic links --the identity of a path is not unique (as different paths may refer to the same physical address) nor stable (since the path resolution depends on the current symbolic link configuration)-- making it harder to identify conflicts between transactions and to properly lock resources. For example, one transaction may read a file whose path is concurrently modified by another transaction that changes a symbolic link.
Therefore, our transaction logs keep special track of symbolic link modifications and we perform all file operations over ``canonical'' file paths, calculated against the transaction log while marking each resolved link as read.

\paragraph{Round-tripping functions}
In TxForest, each transactional variable is an in-memory data structure that reflects the content of a particular filestore, derived from a Forest description type with some given arguments and a root path.
Behind the scenes, the transactional engine is responsible for preserving the abstraction, and keeping each variable ``in sync'' with the latest transactional snapshot of the filesystem, such that changes on the filesystem are propagated to the affected in-memory filestore variables, and writes to variables move the filesystem snapshot forward.

This task is performed by a coupled pair of |loadSym| and |storeSym| functions. Their precise definitions and formal semantics is given in Appendix~\ref{sec:semantics}.
Informally, each Forest variable is implemented as a ``thunk'' that lazily computes visible data (denoting the content and metadata of the filestore) and hidden data (remembering errors during validation).

The |loadSym| function for a top-level variable generates a top-level thunk that, once evaluated, recursively reads data from the (transactional snapshot of the) filesystem, building a thunk for each level of structure. Error information is computed behind an additional thunk to guarantee that validation is only performed when explicitly demanded by the user.

The |storeSym| function strictly traverses a nested structure of in-memory thunks and updates the (transactional snapshot of the) filesystem to reflect the same content, returning an additional \emph{validator} that can test the updated filesystem for inconsistencies during storing.

These two functions are carefully designed so that they preserve data on round trips: loading a filestore and immediately storing it back always succeeds and keeps the filesystem unchanged; and storing succeeds as long as loading the updated filesystem yields the same in-memory representation.

\paragraph{Transactional variables}
In TxForest, each transaction keeps a local filesystem snapshot with a unique thread-local version number and a log of tentative updates over the real filesystem.

When users create a |new| transactional variable, the |loadSym| function is called to create the corresponding thunk with a suspended computation that always loads data from the latest version of the filesystem. Note that, due to laziness, no data is actually loaded.
These thunks, acting as transactional variables, can be concurrently accessed by multiple transactions.
Each transacation keeps a memoization table mapping variables to the latest read values at a particular filesystem version.
We implement them as weak hash tables, allowing the Haskell garbage collector to purge older entries.

When a transaction |read|s a transactional variable it first checks the memoization table for a value for the current filesystem, otherwise the associated level of data is loaded from the current filesystem snapshot, and adds a new table entry with the read value at the current version. 

A call to |writeOrElse| starts by making a copy of the current filesystem snapshot and adding an entry to the memoization table of the respective variable mapping the next filesystem version to the newly written value.
It then invokes the |storeSym| function (remembering the creation-time arguments and root path) to update the filesystem log under the new version and runs the resulting validator; on inconsistencies, the transaction rolls back to the backed up filesystem snapshot and executes the user-supplied alternative action instead.

\subsection{Incremental Transactional Forest}

We have described all the components for a complete implementation of TxForest, but not a very efficient one.
To understand its limitations, imagine that a transaction maintains two completely unrelated variables and reads the first, writes new content to the second, and reads the first again.
Since the two reads occur between a filesystem change, our simple memoization mechanism will fail, and the same content will be redundantly loaded from the filesystem twice.
Even worse, writes in TxForest force a deep evaluation of the in-memory filestore, compromising the convenient laziness properties of the runtime system.
For example, if a transaction reads a directory variable and immediately writes the read value to the same variable, the underlying |storeSym| function will strictly traverse the filestore to redundantly overwrite sub- files and directories, even though their content hasn't actually changed.

\paragraph{Round-tripping functions}
The problem in both examples is that the |loadSym| and |storeSym| functions used by the runtime system are agnostic to modifications made to the filesystem or to the in-memory data, and execute from scratch every time with a running ``footprint'' proportional to the size of the Forest description.
Being Forest an embedded DSL in Haskell, we can exploit domain-specific knowledge to design incremental round-tripping functions, intuitively named |loadDeltaSym| and |storeDeltaSym|, with ``footprint'' proportional to the size of the actual updates.
Especially since transactions are already equipped with the machinery to keep logs of modifications, we can extend our runtime system to make use of the incremental functions in place of their non-incremental counterparts.
In this section we informally discuss the necessary extensions. The formal ingredients for an incremental Forest load/store semantics are developed in Appendix~\ref{sec:incsemantics}.

\paragraph{File system updates}
In order to support incrementality within individual transactions, each transaction shall be able to identify the updates that occurred since a previous point in time (denoted by a filesystem version).
We thus split the sequences of writes recorded in transaction logs according to their increasing filesystem versions.

The key behind incrementality is to exploit the locality of updates, so that the algorithm can distinguish affected structures that need to be updated from unaffected ones that are already up-to-date.
In the context of filesystems, with updates as sequences of operations on file paths, locality checking boils down to path inclusion: seeing the filesystem as a tree, an update on path |r'| is local to another path |r| if |r'| is a subpath of |r|.

However, as filesystems are in fact graphs, a branch in the filesystem may be affected by changes made to distinct branches, by following symbolic links.
This requires our transaction manager to keep a materialized record of all the symbolic links in the filesystem.
Although finding all symbolic links in traditional filesystems is an expensive task that involves traversing the whole file hierarchy, it can be amortized by running once when the transaction manager is initialized, and having transactions updating the necessary symbolic link records. This cost becomes negligible in a database-centric model, in which an always-on server assumes full control of the filesystem and accepts transactions issued by clients.

\paragraph{Filestore updates}
Additionally to filesystem updates, transactions must also recognize the data modifications that have been performed on a given in-memory data structure since a particular filesystem version.
For individual variables, our transaction logs already keep entries with the latest value read from or wrote to them, annotated with the corresponding filesystem version.
But filestores often have multiple levels of nested variables (e.g., an accounts directory contains multiple account variables) and data edits may occur deep in the structure (e.g., the user updates one specific account).

In order to support incremental update propagation, a variable must also be aware of modifications to its underlying child variables.
We do so by maintaining an extra acyclic parenthood graph between thunks and an extra field with the version of the last modification made to (the underlying structure of) each variable.
A new variable is created with no parent nor child edges, and set as last modified at the current filesystem version.
When a variable is read or written, we remove its child edges from the graph, add new parent edges for its immediate child variables, and recursively set the modification version for all its parent variables (itself included) to the current filesystem version.

\paragraph{Transactional variables}
In the incremental variant of TxForest, |new| variables are created as before, with a call to |loadSym|.

When |read|ing a variable, we first search for a memoized value; if none is found, we load the contents from the filesystem as before.
If an old value is found, we compute the updates since the previous filesystem version, by filtering the logged filesystem updates in the corresponding interval that affect the variable's root path and by retrieving the variable's latest modification version.
We then invoke |loadDeltaSym| (assuming that top-level arguments may have changed) to incrementally repair the memoized in-memory representation, and finish by reading the content of the variable as before.
The |loadDeltaSym| function strictly traverses the filesystem and the in-memory data according to the Forest description: if there are no filesystem nor data updates (i.e., the latest modified version of the top-level variable matches the version of the memoized value), it stops; it also stops on variables with no memoized data, since there is no old value to be repaired; otherwise, it repairs the memoized value according to the updates and proceeds recursively.

When writing to a variable (|writeOrElse|), we first search for a memoized value; if none is found, we write the new contents as before.
If an old value is found, we compute the updates since the memoized value and invoke |loadDeltaSym| to make sure that all memoized data for the filestore is up-to-date with the current filesystem version.
We then backup the transaction log, increment the filesystem version, write the new content to the variable and invoke |storeDeltaSym| (with empty filesystem updates and only the user's writes to the top-level variable as a data update) to incrementally repair the filesystem.
Likewise, the |storeDeltaSym| function strictly traverses the filesystem and the in-memory data according to the Forest description: if there are no updates, it stops; otherwise, it modifies the filesystem to conform to the new content and proceeds recursively. Note that, in the general case, when |storeDeltaSym| stops the generated validator still needs to verify that the unmodified fragment of the filesystem is consistent with other |storeDeltaSym| traversals that may have modified overlapping fragments of the filesystem ---in order to guarantee that arbitrary data dependencies in a Forest specification are correctly enforced. We can skip these checks in our particular application, as long as we only call |storeDeltaSym| with a top-level data modification.

\subsection{Log-structured Transactional Forest}

Thus far, our implementation of TxForest only supports incremental updates locally to each transaction.
This has the potential to greatly speed-up larger transactions that combine several filestore operations, but is ineffective for smaller transactions. For example, it offers no incrementality if each single filestore operation is performed in a separate transaction.
It is also non-optimal for more common scenarios that involve multiple concurrent transactions trying to access a shared resource, e.g., a set of configuration files: each transaction will have to load all necessary files into memory, perform the respective modifications, and discard all the in-memory data at the end; such loading work is often redundant, even more for workloads dominated by read-only transactions where the filesystem seldom changes between consecutive transactions.

Unfortunately, this is how far TxForest can reasonably go on top of a conventional filesystem.
As a traditional optimistic concurrency method, each transaction builds an interim timeline of filesystem updates that is preserved for the duration of concurrently running transactions.
Consequently, there is no global timeline that records the history of filesystem modifications to which in-memory data can be chronologically affixed across transactions.

\paragraph{Log-structured filesystem}
This is precisely one of the main advantages of a log-structured filesystem, in which updates do not overwrite existing data but rather issue continuous snapshots, that can be made accessible as a log that records the history of modifications on the filesystem since an initial point in time.
To provide inter-transaction incrementality, we have explored a variant of TxForest implemented on top of NILFS, a log-structured filesystem for Linux supporting read-only snapshots.

\paragraph{Transactional logs}
The main difference in our third design is that transactions are logically executed over particular snapshots of the filesystem.
When initialized, each transaction acquires the version number of the latest snapshot from the NILFS filesystem. Reads on the filesystem are instead performed against the initial snapshot; and groups of writes create advancing transaction-local snapshots over the initial physical snapshot. On success, the transaction commits its modifications over the current filesystem (that may be at newer snapshot than the transaction's snapshot due to concurrent updates) and requests the creation of a new NILFS snapshot combining the concurrent and the transaction-local updates.

\paragraph{Transactional variables}
We also refine variables to store memoized content that was previously read at an older NILFS snapshot.
This enables a transaction to incrementally reuse filestores loaded by previous transactions. The filesystem delta is computed as the difference between the memoized and the transaction's NILFS snapshots, prepended to the transaction-local tentative updates. Once finished, the transaction commits its local data updates to the global transactional variables; to ensure that all the committed data is bound to externally visible NILFS snapshots, we first use |loadDeltaSym| to synchronize all the transaction-local memoized data with the latest filesystem snapshot ---the one for which we will later request a new NILFS snapshot.
Since multiple transactions may now concurrently read and write the content of the same transactional variables, each |atomic| block must acquire extra per-variable locks during the validate-and-commit phase.

\paragraph{Read-only transactions}
A side-benefit of using a snapshotting filesystem is that every transaction starts from a possibly outdated, but nevertheless consistent, snapshot of the filesystem.
Following this observation, read-only transactions can be optimized to bypass all the logging, validation and committing mechanisms, as if they happened to run before all other concurrent transactions~\cite{TxCache}.
Read-write transactions need to anyway validate their read file paths against concurrent writes, to guarantee that they are assigned a position in the serial order.

\subsection{Isolated Transactional Forest}

extended mandatory locking could provide native filesystem support for locks on files/directories.

TxForest, however, is only serializable with respect to Forest transactions, and transaction-oblivious applications that may be concurrently modifying the filesystem.
This is a disadvantage of implementing the transactional abstraction in software, independently of the characteristics underlying filesystem.
We can provide stronger isolation if we assume additional control over the filesystem, as common for transactional filesystem implementations.
Our locks are purely in-memory. extended mandatory locks from ~\cite{Valor}: ability to lock files and directories and modify the standard POSIX system calls used by unmodified applications to perform the necessary locking.

\section{Evaluation}

although Haskell is a great language laboratory, we are already paying a severe performance overhead if efficiency is the main concern.

even the Haskell STM is implemented in C

\section{Related Work}


transactional filesystems (user-space vs kernel-space)
\url{http://www.fuzzy.cz/en/articles/transactional-file-systems}\\
\url{http://www.fsl.cs.sunysb.edu/docs/valor/valor_fast2009.pdf} cooperate with pre-existing apps that are not transactional; extended mandatory locking. For
transaction-oblivious processes, each individual
system call is treated as a transaction; Extended mandatory locking ensures that
all processes acquire locks before accessing these resources \\
\url{http://www.fsl.cs.sunysb.edu/docs/amino-tos06/amino.pdf}

libraries for transactional file operations:
\url{http://commons.apache.org/proper/commons-transaction/file/index.html}\\
\url{https://xadisk.java.net/}\\
\url{https://transactionalfilemgr.codeplex.com/}

tx file-level operations (copy,create,delete,move,write)
schema somehow equivalent to using the unstructured universal Forest representation

but what about data manipulation: transactional maps,etc?

os transactions:
\url{http://www.cs.utexas.edu/~porterde/pubs/sosp063-porter.pdf}

\section{Conclusions}

transactional variables do not descend to the content of files. pads specs are read/written in bulk. e.g., append line to log file. extend pads.

\bibliographystyle{abbrvnat}
\bibliography{forest}

\onecolumn
\appendix

\section {Forest Semantics}
\label{sec:semantics}

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

\begin{align*}
	|starIn r1 F r2 = forin r r1 ((star F r) `inSet` (star F r2))|
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


%the store function also changes the in-memory representation by recomputing the validation thunks (hidden to users) to match the new content.

$\boxed{|load oenv eenv r s F (prime oenv) v|}$ ``Under heap |oenv| and environment |eenv|, load the specification |s| for filesystem |F| at path |r| and yield a representation |v|.''

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

$\boxed{|store oenv eenv r s F v (prime oenv) (prime F) (prime phi)|}$ ``Under heap |oenv| and environment |eenv|, store the representation |v| for the specification |s| on filesystem |F| at path |r| and yield an updated filesystem |prime F| and a validation function |prime phi|.''

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
\label{sec:incsemantics}

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
	(focus (addFile (prime r) u) F r) `def`			if (starIn (prime r) F r) then addFile (prime r) u else did
	(focus (addDir (prime r)) F r) `def`				if (starIn (prime r) F r) then addDir (prime r) else did
	(focus (addLink (prime r) (prime2 r)) F r) `def` 	if (starIn (prime r) F r) then addLink (prime r) (prime2 r) else did
	(focus (rem (prime r)) F r) `def` 				if (starIn (prime r) F r) then rem (prime r) else did
	(focus (chgAttrs (prime r) i) F r) `def` 			if (starIn (prime r) F r) then chgAttrs (prime r) i else did
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



$\boxed{|dload oenv eenv deenv r s F v df dv (prime oenv) (prime v) (prime deltav)|}$ ``Under heap |oenv|, environment |eenv| and delta environment |deenv|, incrementally load the specification |s| for the original filesystem |F| and original representation |v|, given filesystem changes |df| and representation changes |dv|, to yield an updated representation |prime v| with changes |prime deltav|.

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

$\boxed{|dstore oenv eenv deenv r s F v df dv (prime oenv) (prime F) (prime phi)|}$ ``Under heap |oenv|, environment |eenv| and delta environment |deenv|, store the representation |v| for the specification |s| on filesystem |F| at path |r|, given filesystem changes |df| and representation changes |dv|, and yield an updated filesystem |prime F| and a filesystem validation function |prime phi|.''

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

