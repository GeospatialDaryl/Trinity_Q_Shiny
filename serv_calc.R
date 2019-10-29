# setting up the list of calculations I want to do
serv_calc <- list()

# I only want to build a scatterplot when I click my build button, so my list will be of length 1
serv_calc[[1]] <- function(calc, sess){
  # this is going to activate any time I press "build!"
  observeEvent(calc$go, {
    # create our data frame for visualizing
    sub.df <- data.frame("x" = iris[iris$Species %in% calc$spec,calc$x_axis],
                         "y" = iris[iris$Species %in% calc$spec,calc$y_axis],
                         "species"=iris[iris$Species %in% calc$spec,"Species"])
    
    # add this to calc, since we want to use this in our rendering
    calc[["sub.df"]] <- sub.df
  })
}