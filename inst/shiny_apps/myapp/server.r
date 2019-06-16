# server.r
library(oro.nifti)
library(readr)
# increase upload size limit to 40 Mb
options(shiny.maxRequestSize = 40*1024^2)

function(input, output, session){
#   ntext <- eventReactive(input$sample, {
#     input$n
#   })
#
#   output$nText <- renderText({
#     ntext()
#   })
# }

  output$var_names <- renderText({
    inFile <- input$xmat
    if (is.null(inFile))
      return(NULL)
    x_mat = read_delim(inFile$datapath, delim = ',')
    paste(variable.names(x_mat), sep = ",")
  })


    var_names_re = reactive({
      inFile <- input$xmat
      if (is.null(inFile))
        return(NULL)
      x_mat = read_delim(inFile$datapath, delim = ',')
      variable.names(x_mat)
    })

    observeEvent(
      input$xmat,
      {
        inFile <- input$xmat
        if (is.null(inFile))
          return(NULL)
        x_mat = read_delim(inFile$datapath, delim = ',')
        varnames = c(variable.names(x_mat))
        updateRadioButtons(session, "group_var",
                         label = 'Select grouping variable',
                         choiceNames = c(var_names_re()),
                         choiceValues = 1:length(varnames))
      }
    )




  imagePlot = reactive({
    # need to change it to be really reactive
    # orthographic(study58_fu_CC[[14]])
  })

  output$plot1 = renderPlot(imagePlot())

  output$vars = renderPrint(paste(variable.names(dat_mod), collapse = ", "))

  output$downloadPlot = downloadHandler(
    file = function() {
      paste('imaging_', Sys.Date(), '.png', sep = '')
    },
    content = function(file) {
      png(file)
      # need to use reactive values here
      orthographic(study58_fu_CC[[14]])
      dev.off()
    }
  )
}

