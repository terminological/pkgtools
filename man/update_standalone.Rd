% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-standalone.R
\name{update_standalone}
\alias{update_standalone}
\title{Update or create a standalone metadata block in the Rstudio editor.}
\usage{
update_standalone(repo = NULL, license = NULL, dependencies = NULL)
}
\arguments{
\item{repo}{the github repository e.g. \code{organisation/project}}

\item{license}{a license URL (defaults to \verb{http:://unlicense/org})}

\item{dependencies}{a optional list of standalone filenames in the same repository}
}
\value{
nothing, updates the RStudio editor content
}
\description{
This function helps identify package imports and maintains standalone file
metadata. It operates on the code in the Rstudio active window and suggests
changes to be made to keep the standalone file metadata up to date. It is
intended be used interactively.
}
