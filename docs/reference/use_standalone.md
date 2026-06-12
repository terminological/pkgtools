# Extended version of `use_standalone` that works with analysis projects

[`usethis::use_standalone`](https://usethis.r-lib.org/reference/use_standalone.html)
is a package development tool used in `r-lib` to share useful functions
between packages without creating a hard dependency on them. This is
also useful in data analysis projects where no package infrastructure
exists but you want to reuse common functions (e.g. plot themes) between
analysis projects. Developing a package containing these shared
functions and deploying to `CRAN` or `r-universe` is possible but it is
unwieldy and requires more infrastructure that needed.

## Usage

``` r
use_standalone(
  repo_spec,
  file = NULL,
  ref = NULL,
  host = NULL,
  git_dir = fs::path_home("Git")
)
```

## Arguments

- repo_spec:

  A string identifying the GitHub repo in one of these forms:

  - Plain `OWNER/REPO` spec

  - Browser URL, such as `"https://github.com/OWNER/REPO"`

  - HTTPS Git URL, such as `"https://github.com/OWNER/REPO.git"`

  - SSH Git URL, such as `"git@github.com:OWNER/REPO.git"`

- file:

  Name of standalone file. The `standalone-` prefix and file extension
  are optional. If omitted, will allow you to choose from the standalone
  files offered by that repo.

- ref:

  The name of a branch, tag, or commit. By default, the file at `path`
  will be copied from its current state in the repo's default branch.
  This is extracted from `repo_spec` when user provides a URL.

- host:

  GitHub host to target, passed to the `.api_url` argument of
  [`gh::gh()`](https://gh.r-lib.org/reference/gh.html). If `repo_spec`
  is a URL, `host` is extracted from that.

  If unspecified, gh defaults to "https://api.github.com", although gh's
  default can be customised by setting the GITHUB_API_URL environment
  variable.

  For a hypothetical GitHub Enterprise instance, either
  "https://github.acme.com/api/v3" or "https://github.acme.com" is
  acceptable.

- git_dir:

  The directory to the users collection of Git projects

## Details

Using a standalone file we can develop these functions in a basic git
repository with no deployment (with or without package infrastructure),
and import them into an analysis project as standalone files. From a
reproducibility point of view this is sometimes beneficial if the
functions in question are fairly dynamic as the version is hard wired
into the analysis project.

The use cases supported by
[`usethis::use_standalone`](https://usethis.r-lib.org/reference/use_standalone.html)
are predicated around R package development but here we extend this
behaviour to analysis projects with dependencies managed by `renv`, or
not managed at all. If an analysis project is being managed by `renv`
then using a standalone file will install missing `renv` dependencies
and snapshot the project. If no `renv` is detected a check is written to
the `.RProfile` file which will produce a message about missing
dependencies when the project is opened.

In a non package project directives to try and `source` all standalone
files are added to the `.RProfile` file, so that standalone file
functions are immediately available.

If this is not working you may need to set the repository to HTTP/2:
`git config --global http.version HTTP/2 && git push`

## Supported fields

A standalone file has YAML frontmatter that provides additional
information, such as where the file originates from and when it was last
updated. Here is an example:

    ---
    repo: r-lib/rlang
    file: standalone-types-check.R
    last-updated: 2023-03-07
    license: https://unlicense.org
    dependencies: standalone-obj-type.R
    imports: rlang (>= 1.1.0)
    ---

Two of these fields are consulted by `use_standalone()`:

- `dependencies`: A file or a list of files in the same repo that the
  standalone file depends on. These files are retrieved automatically by
  `use_standalone()`.

- `imports`: A package or list of packages that the standalone file
  depends on. A minimal version may be specified in parentheses, e.g.
  `rlang (>= 1.0.0)`. These dependencies are passed to
  [`use_package()`](https://usethis.r-lib.org/reference/use_package.html)
  to ensure they are included in the `Imports:` field of the
  `DESCRIPTION` file.

Note that lists are specified with standard YAML syntax, using square
brackets, for example: `imports: [rlang (>= 1.0.0), purrr]`.

## Examples

``` r
if (FALSE) {
  # Import a standalone file from a GitHub repo:
  use_standalone("terminological/ggrrr", "codegen")

  # Use a local copy (must exist in ~/Git/):
  use_standalone("ggrrr", "codegen")
}
```
