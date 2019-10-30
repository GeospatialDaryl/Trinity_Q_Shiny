# load libraries
# note that attaching mwshiny also attaches Shiny
library(mwshiny) # our multiwindow app
library(ggplot2) # cool visualizations
library(datasets) # contains the iris dataset

#### #####
##  mwsApp
#### #####

#run!
mwsApp(ui_win, serv_calc, serv_out)

