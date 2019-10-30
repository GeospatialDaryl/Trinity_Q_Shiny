# named list of ui pages that are the contain the title and content of each of my windows
ui_win <- list()

# first we add what we want to see in the controller to the list
ui_win[["Controller"]] <- fluidPage(
  titlePanel("Iris Dataset Explorer: Controller"),
  sidebarLayout(
    sidebarPanel(
      # choose what goes on the x axis
      selectInput(inputId = "x_axis",
                  label = "What would you like to see on the x axis?",
                  choices = colnames(iris)[colnames(iris)!="Species"]),
      # choose what goes on the y axis
      selectInput(inputId = "y_axis",
                  label = "What would you like to see on the y axis?",
                  choices = colnames(iris)[colnames(iris)!="Species"]),
      # choose which groups you want to see
      checkboxGroupInput(inputId = "spec",
                         label = "Which species would you like to see?",
                         choices = as.character(unique(iris$Species)),
                         selected = as.character(unique(iris$Species))),
      # only build the scatter plot when this is clicked
      actionButton(inputId = "go",
                   label = "Build!")
    ),
    # just an empty main panel
    mainPanel()
  )
)

# then we add what we want to see in the scatter section
ui_win[["Scatter"]] <- fluidPage(
  titlePanel("Iris Dataset Explorer: Scatter"),
  plotOutput(outputId = "iris_scatter")
)

