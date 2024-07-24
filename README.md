# CondorcetCycle
Replication Data: On the Prevalence of Condorcet's Paradox by Salvatore Barbaro and Anna Kurella.

You will need to download the CSES data directly. The source is provided in the paper's reference section. The script is written for R. 

\section{Data Acessibility Statement}
The paper uses data from the CSES project. Data is subject to a redistribution restriction, but can be freely downloaded. I requires a free-of-charge registration. The DOI to the data can be found in the references of the paper.

\section{Dataset list}
The data consists of two files, one covers the waves 1-4, the second the fifth wave. We used both. 

\section{Software Requirements}
R 4.1.2 (Bird Hippie) \begin{itemize}
	\item dplyr \item vote \item haven 
\end{itemize}
We run the script on a Linux-debian machine, but the script has also been tested for Windows. In order to ensure cross-OS compatibility, we do not use parallel computing. 


\section{Controlled Randomness}
Not applicable

\section{Approximate time needed to reproduce the analysis on a standard (2024) desktop machine}
10 - 60 minutes.

The code was last run on a 20-core Intel-based laptop with debian (tuxedo) OS.

\subsection{Description of code}
The descriptions can be found as comment lines in the script.

\section{Instruction to Replicators}
The CSES data are separated in two files. The first covers waves 1 - 4, the second file the fifth wave. The script first handles the first file, and reproduces the codes using the second file (starting at line 304)

The results are collected in the object $final_{-}all$. 


	
