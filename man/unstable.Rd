% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unstable.R
\name{unstable}
\alias{unstable}
\title{Reload a set of packages that are in development on the local machine}
\usage{
unstable(
  path = ".",
  ...,
  force = TRUE,
  upgrade = "never",
  quiet = TRUE,
  load_lib = TRUE
)
}
\arguments{
\item{path}{the package local development repository path. This assumes you
have all your other package code in a sibling directory, e.g. \verb{~/Git/pkg1},
\verb{~/Git/pkg2}}

\item{...}{
  Arguments passed on to \code{\link[remotes:install_local]{remotes::install_local}}
  \describe{
    \item{\code{subdir}}{subdirectory within url bundle that contains the R package.}
    \item{\code{dependencies}}{Which dependencies do you want to check?
Can be a character vector (selecting from "Depends", "Imports",
"LinkingTo", "Suggests", or "Enhances"), or a logical vector.

\code{TRUE} is shorthand for "Depends", "Imports", "LinkingTo" and
"Suggests". \code{NA} is shorthand for "Depends", "Imports" and "LinkingTo"
and is the default. \code{FALSE} is shorthand for no dependencies (i.e.
just check this package, not its dependencies).

The value "soft" means the same as \code{TRUE}, "hard" means the same as \code{NA}.

You can also specify dependencies from one or more additional fields,
common ones include:
\itemize{
\item Config/Needs/website - for dependencies used in building the pkgdown site.
\item Config/Needs/coverage for dependencies used in calculating test coverage.
}}
    \item{\code{build}}{If \code{TRUE} build the package before installing.}
    \item{\code{build_opts}}{Options to pass to \verb{R CMD build}, only used when \code{build} is \code{TRUE}.}
    \item{\code{build_manual}}{If \code{FALSE}, don't build PDF manual ('--no-manual').}
    \item{\code{build_vignettes}}{If \code{FALSE}, don't build package vignettes ('--no-build-vignettes').}
    \item{\code{repos}}{A character vector giving repositories to use.}
    \item{\code{type}}{Type of package to \code{update}.}
  }}

\item{force}{Force installation, even if the remote state has not changed
since the previous install.}

\item{upgrade}{Should package dependencies be upgraded? One of "default", "ask", "always", or "never". "default"
respects the value of the \code{R_REMOTES_UPGRADE} environment variable if set,
and falls back to "ask" if unset. "ask" prompts the user for which out of
date packages to upgrade. For non-interactive sessions "ask" is equivalent
to "always". \code{TRUE} and \code{FALSE} are also accepted and correspond to
"always" and "never" respectively.}

\item{quiet}{If \code{TRUE}, suppress output.}

\item{load_lib}{load the package using a library command}
}
\value{
nothing
}
\description{
Vignette building uses a new session. Any changes in current project or
dependent locally developed projects are not tested unless the packages are
all installed using \code{devtools::install_local(...)}. This causes problems when
developing multiple packages in parallel.
}
\details{
This function assumes the \code{path} variable is a path to a package which is under
version control in a Git directory. Other dependencies to this package may
also be under development in sibling directories. The aim is to install the
current version of the target package and all locally held dependencies that
have changed on the local disk compared to the locally installed version.

This function scans the current package and first order dependencies, looking
for local development directories for any packages imported. Looks for
changes in files in local development directories of package and first order
dependencies versus files currently installed in r-library. If it finds any
differences it checks if there is a version change of the package,
bumps the version number of the development package, and installs it locally,
After installation it restarts R.

Any recent file change in development directories triggers a dev version bump
and local package installation. After a call to \code{unstable()} any dependencies
in your local dev environment are up to date.

If \code{unstable} is called from within a non package project which is using
\code{renv} then rather than installing locally using \code{devtools} the package is
built and deployed locally in the \code{renv} local package directory (
\verb{<proj root>/renv/local}) and installed from there. The \code{renv} local packages are
placed under version control. At the moment it is a manual job to tidy this up once the
development package is finalised and deployed
}
