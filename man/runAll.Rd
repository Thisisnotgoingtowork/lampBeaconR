% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runAll.R
\name{runAll}
\alias{runAll}
\title{reading a csv file**}
\usage{
runAll(
  file,
  outDir = dirname(file),
  isSAP3 = grepl("[Ss][Aa][Pp][Vv]?3", file),
  nCycle = 200,
  censorY = NULL,
  nPages = 1
)
}
\arguments{
\item{file}{An excel file that have the raw data}

\item{outDir}{output dictionary address path in the local system}

\item{isSAP3}{Is the file with certain format?**}

\item{nCycle}{an int that indicate the number of cycle, initial with 200}

\item{censorY}{a censor**}

\item{nPages}{an int that says the page number??? initial with 1}
}
\description{
reading a csv file** This is an combination of readXls, calcAmps, etc all functions together
}
