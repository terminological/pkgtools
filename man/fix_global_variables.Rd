% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_global_variables.R
\name{fix_global_variables}
\alias{fix_global_variables}
\title{Adds non standard and hidden files to the .\code{Rbuildignore} file}
\usage{
fix_global_variables(pkg = ".", check)
}
\arguments{
\item{pkg}{the package location}

\item{check}{the results of a \code{devtools::check}}
}
\value{
nothing
}
\description{
Adds non standard and hidden files to the .\code{Rbuildignore} file
}
