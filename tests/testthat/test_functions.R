# A typical package file that needs qualifying


#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#'
#' @returns
#' @export
#'
#' @examples
#' # namespacing is also needed here in examples:
#' ggplot(iris, aes(x=Sepal.Width,y=Petal.Width))+
#'   geom_point()
documented_function = function(x,y,z) {
  
  # the following needs qualification with dplyr functions
  # we dont want comments like filter (example) to be qualified
  
  iris %>% filter(Species != "setosa") %>%
    mutate(Sepal.Width = Sepal.Width/2) %>%
    dplyr::group_by(Species)
  
  # stats package
  z = rnorm(1000)  
  y = quantile(z,0.95)
  
  
}



#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#'
#' @returns
#' @export
#'
#' @examples
#' # namespacing is also needed here in examples:
#' ggplot(iris, aes(x=Sepal.Width,y=Petal.Width))+
#'   geom_point()
another_documented_function = function(x,y,z) {
  
  # the following needs qualification with dplyr functions
  # we dont want comments like filter (example) to be qualified
  
  iris %>% filter(Species != "setosa") %>%
    mutate(Sepal.Width = Sepal.Width/2) %>%
    dplyr::group_by(Species)
  
  # stats package
  z = rnorm(1000)  
  y = quantile(z,0.95)
  
  
}

