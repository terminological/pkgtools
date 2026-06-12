#!/usr/bin/env Rscript

# Handle dependencies
dependencies <- c("optparse", "fs")
for (dep in dependencies) {
  if (!requireNamespace(dep, quietly = TRUE)) {
    install.packages(dep, repos = "https://cloud.r-project.org")
  }
}
if (!requireNamespace("pkgtools", quietly = TRUE)) {
  devtools::install_github("terminological/pkgtools")
}

# 1. Define options
# option_list <- list(
#   optparse::make_option(
#     c("-c", "--check"),
#     action="store_true",
#     default = FALSE,
#     help = "check whether files need formatting",
#     metavar = "FILE"
#   )
# )

opt_parser <- optparse::OptionParser(
  option_list = list(), # option_list,
  description = "Format files using `pkgtools` style rules"
)
opt <- optparse::parse_args(opt_parser, positional_arguments = c(1, Inf))

for (arg in opt$args) {
  path = fs::path_expand(arg)
  text = readr::read_lines(path)
  out = pkgtools::style_text(text)
  readr::write_lines(out, path)
}
