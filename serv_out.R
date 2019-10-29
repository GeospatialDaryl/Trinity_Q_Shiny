# set up our serv_out list
serv_out <- list()

# we're just rendering our scatter plot based on the iris dataset
# note the name is the same as the outputid
serv_out[["iris_scatter"]] <- function(calc, sess){
  renderPlot({
    # we add this check to make sure our plot doesn't try to render before we've ever pressed "Build!"
    if (!is.null(calc$sub.df)){
      # build scatterplot
      ggplot(calc$sub.df, aes(x, y, color = factor(species)))+
        geom_point()+ # make scatter
        ggtitle("Iris Comparisons")+ # add title
        xlab(calc$x_axis)+ # change x axis label
        ylab(calc$y_axis)+ # change y axis label
        labs(color="species")+ # change legend label
        NULL
    }
  })
}