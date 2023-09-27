



# S3 object with functions.
# e.g.
# 
# @examples
# 
# as.mys3 = function(a,b) {
#   browser()
#   structure(
#     c(.get_args(),
#       list(
#         sum = function(c) {
#           browser()
#           a+b+c
#         }
#       )
#     ),
#     class = "mys3"
#   )
# }
# # make immutable
# `[[<-.mys3` <- function(...) {stop("Can't assign into locked object")}
# `[<-.mys3` <- function(...) {stop("Can't assign into locked object")}
# `$<-.mys3` <- function(...) {stop("Can't assign into locked object")}
# 
# ex = as.mys3(1,2)
# 
# ex$sum(3)
# try({ex$a=10
# 
# 
# # This is 
# 


