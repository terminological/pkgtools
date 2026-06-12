# Auto generate data documentation

Generates a full roxygen2 documentation block (title, description,
columns, usage tag, etc.) for an R object. Intended to be written to a
`.Rd` file or pasted into a roxygen block. Works with data frames, named
lists, and other R objects.

## Usage

``` r
generate_data_description(
  data,
  title = sprintf("The `%s` dataset", name),
  description = sprintf("`%s` dataset description", name),
  url = "http://example.com",
  name = NULL
)
```

## Arguments

- data:

  an arbitrary R object

- title:

  the dataset title

- description:

  one or more paragraphs of description (blank lines separate
  paragraphs)

- url:

  optional source URL

- name:

  the dataset name (defaults to the expression used for `data`)

## Value

the data description block formatted in markdown

## Examples

``` r
# Generate docs for a simple data frame:
generate_data_description(
  iris,
  title = "The myiris dataset",
  description = c("line 1 & test", "line 2", "", "para 2",
                  paste(rep("asda ", 50), collapse = "")),
  url = "http://example.com",
  name = "myiris"
)
#>  [1] ""                                                                             
#>  [2] "## documentation block for `myiris` ----"                                     
#>  [3] ""                                                                             
#>  [4] "#' The myiris dataset"                                                        
#>  [5] "#' "                                                                          
#>  [6] "#' line 1 & test line 2"                                                      
#>  [7] "#' "                                                                          
#>  [8] "#' para 2 asda asda asda asda asda asda asda asda asda asda asda asda asda"   
#>  [9] "#' asda asda asda asda asda asda asda asda asda asda asda asda asda asda asda"
#> [10] "#' asda asda asda asda asda asda asda asda asda asda asda asda asda asda asda"
#> [11] "#' asda asda asda asda asda asda asda"                                        
#> [12] "#'"                                                                           
#> [13] "#' "                                                                          
#> [14] "#' ## `myiris` dataframe with 150 rows and 5 columns"                         
#> [15] "#' "                                                                          
#> [16] "#' \\describe{"                                                               
#> [17] "#'     \\item{ `Sepal.Length` (dbl) }{ "                                      
#> [18] "#'         Sepal.Length description "                                         
#> [19] "#'       }"                                                                   
#> [20] "#'     \\item{ `Sepal.Width` (dbl) }{ "                                       
#> [21] "#'         Sepal.Width description "                                          
#> [22] "#'       }"                                                                   
#> [23] "#'     \\item{ `Petal.Length` (dbl) }{ "                                      
#> [24] "#'         Petal.Length description "                                         
#> [25] "#'       }"                                                                   
#> [26] "#'     \\item{ `Petal.Width` (dbl) }{ "                                       
#> [27] "#'         Petal.Width description "                                          
#> [28] "#'       }"                                                                   
#> [29] "#'     \\item{ `Species` (fct) }{ "                                           
#> [30] "#'         Species description "                                              
#> [31] "#'       }"                                                                   
#> [32] "#'  }"                                                                        
#> [33] "#' "                                                                          
#> [34] "#'"                                                                           
#> [35] "#' @docType data"                                                             
#> [36] "#' @keywords datasets"                                                        
#> [37] "#' @concept datasets"                                                         
#> [38] "#' @source <http://example.com>"                                              
#> [39] "\"myiris\""                                                                   
#> [40] ""                                                                             
#> [41] "## end of documentation block for `myiris`"                                   

# Nest a larger dataset (requires ggplot2):
if (requireNamespace("ggplot2", quietly = TRUE)) {
  nested <- ggplot2::diamonds %>% tidyr::nest(details = -c(cut, color, clarity))

  generate_data_description(
    nested,
    title = "The nested diamonds dataset",
    description = "The ggplot2 diamonds data set nested",
    url = "http://example.com",
    name = "mydiamonds"
  )
}
#>  [1] ""                                                        
#>  [2] "## documentation block for `mydiamonds` ----"            
#>  [3] ""                                                        
#>  [4] "#' The nested diamonds dataset"                          
#>  [5] "#' "                                                     
#>  [6] "#' The ggplot2 diamonds data set nested"                 
#>  [7] "#'"                                                      
#>  [8] "#' "                                                     
#>  [9] "#' ## `mydiamonds` dataframe with 276 rows and 4 columns"
#> [10] "#' "                                                     
#> [11] "#' \\describe{"                                          
#> [12] "#'     \\item{ `cut` (ord) }{ "                          
#> [13] "#'         cut description "                             
#> [14] "#'       }"                                              
#> [15] "#'     \\item{ `color` (ord) }{ "                        
#> [16] "#'         color description "                           
#> [17] "#'       }"                                              
#> [18] "#'     \\item{ `clarity` (ord) }{ "                      
#> [19] "#'         clarity description "                         
#> [20] "#'       }"                                              
#> [21] "#'     \\item{ `details` (list[df[details]]*) }{ "       
#> [22] "#'         details description "                         
#> [23] "#'       }"                                              
#> [24] "#'  }"                                                   
#> [25] "#' "                                                     
#> [26] "#' ### `details` column (`list[df[details]]` type)"      
#> [27] "#' "                                                     
#> [28] "#' \\describe{"                                          
#> [29] "#'     \\item{ `carat` (dbl) }{ "                        
#> [30] "#'         carat description "                           
#> [31] "#'       }"                                              
#> [32] "#'     \\item{ `depth` (dbl) }{ "                        
#> [33] "#'         depth description "                           
#> [34] "#'       }"                                              
#> [35] "#'     \\item{ `table` (dbl) }{ "                        
#> [36] "#'         table description "                           
#> [37] "#'       }"                                              
#> [38] "#'     \\item{ `price` (int) }{ "                        
#> [39] "#'         price description "                           
#> [40] "#'       }"                                              
#> [41] "#'     \\item{ `x` (dbl) }{ "                            
#> [42] "#'         x description "                               
#> [43] "#'       }"                                              
#> [44] "#'     \\item{ `y` (dbl) }{ "                            
#> [45] "#'         y description "                               
#> [46] "#'       }"                                              
#> [47] "#'     \\item{ `z` (dbl) }{ "                            
#> [48] "#'         z description "                               
#> [49] "#'       }"                                              
#> [50] "#'  }"                                                   
#> [51] "#' "                                                     
#> [52] "#'"                                                      
#> [53] "#' @docType data"                                        
#> [54] "#' @keywords datasets"                                   
#> [55] "#' @concept datasets"                                    
#> [56] "#' @source <http://example.com>"                         
#> [57] "\"mydiamonds\""                                          
#> [58] ""                                                        
#> [59] "## end of documentation block for `mydiamonds`"          
```
