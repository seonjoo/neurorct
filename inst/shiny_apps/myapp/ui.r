# ui.r
library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("sample", label = "Use Sample Data"),
      h3("Or use your own data"),
      fileInput("image", label = "Upload image file (.nii)"),
      fileInput("mask", label = "Upload Mask file (.nii)"),
      fileInput("xmat", label = "Upload X_matrix data (.csv)",
                accept = c('.csv')),

      # checkboxGroupInput("options", label = h3("Options"),
      #                    choices = list("Imputation" = 1, "Baseline Adjustment" = 2, "Covariates" = 3, "..." = 4),
      #                    selected = 0),
      radioButtons("group_var", "Select grouping variable",
                   choices = c("please upload X_matrix data first")),

      actionButton("run", "Run!"),

      h3("Output"),
      # h5("Variables"),
      textOutput("var_names")
    ),

    mainPanel(
      h3("Imaging plots"),
      downloadButton("downloadPlot", label = "Save Image", class = NULL),
      plotOutput("plot1")


    )
  )

)



