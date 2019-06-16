# server.r
filepath = file.path("study58_cc.rdata")
load("data/study58_cc.rdata")
library(oro.nifti)

function(input, output){
  imagePlot = reactive({
    # need to change it to be really reactive 
    orthographic(study58_fu_CC[[14]])
  })
  output$plot1 = renderPlot(imagePlot())
  
  output$vars = renderPrint(paste(variable.names(dat_mod), collapse = ", "))
  
  output$downloadPlot = downloadHandler(
    filename = function() {
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

