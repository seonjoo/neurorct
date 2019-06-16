# ui.r
library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("sample", label = "Use Sample Data"),
      h3("Or use your own data"),
      fileInput("image", label = "Imaging data"),

      fileInput("xmat", label = "X_mat data"),
      fileInput("mask", label = "Mask file"),
      checkboxGroupInput("options", label = h3("Options"),
                         choices = list("Imputation" = 1, "Baseline Adjustment" = 2, "Covariates" = 3, "..." = 4),
                         selected = 0),

      h3("Output"),
      h5("Variables"),
      textOutput("vars")
    ),

    mainPanel(
      h3("Imaging plots"),
      downloadButton("downloadPlot", label = "Save Image", class = NULL),
      plotOutput("plot1")


    )
  )

)



