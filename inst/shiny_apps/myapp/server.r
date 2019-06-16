# server.r
library(oro.nifti)
library(readr)
library(neurorct)
library(stringr)
# increase upload size limit to 40 Mb
options(shiny.maxRequestSize = 40*1024^2)

function(input, output, session){

  var_names_re = reactive({
    inFile <- input$xmat
    if (is.null(inFile))
      return(NULL)
    x_mat = read_delim(inFile$datapath, delim = ',')
    output$csvpath = renderText(file.exists(inFile$datapath))
    variable.names(x_mat)
    })

  # update grouping variable options when .csv is uploaded
  observeEvent(input$xmat, {
    updateRadioButtons(session, "group_var",
                       label = 'Select grouping variable',
                       choiceNames = c(var_names_re()),
                       choiceValues = 1:length(var_names_re()))
    })

  observeEvent(input$run, {
    ## read in image file
    imageFile <- input$image
    if (is.null(imageFile))
      return(NULL)
    image = readNIfTI(input$image$datapath)@.Data
    npergroup = dim(image)[4] / 2
    image1 = image[,,,1:npergroup]
    image2 = image[,,,(npergroup+1):(2*npergroup)]
    ## read in mask file
    output$maskpath = renderText(file.exists(input$mask$datapath))
    maskFile <- input$mask
    if (!(is.null(maskFile)))
      mask = readNIfTI(input$mask$datapath)@.Data
    else mask = array(1, dim = dim(image1)[1:3])

    output$plot1 = renderPlot({
      cohend = compute_cohend(image1, image2, mask, breaks = c(-100:100)/100*2)
      })
  })

  # cohend = reactive({
  #   ## read in image file
  #   imageFile <- input$image
  #   if (is.null(imageFile))
  #     return(NULL)
  #   image = readNIfTI(input$image$datapath)@.Data
  #   npergroup = dim(image)[4] / 2
  #   image1 = image[,,,1:npergroup]
  #   image2 = image[,,,(npergroup+1):(2*npergroup)]
  #   ## read in mask file
  #   output$maskpath = renderText(file.exists(input$mask$datapath))
  #   maskFile <- input$mask
  #   if (!(is.null(maskFile)))
  #     mask = readNIfTI(input$mask$datapath)@.Data
  #   else mask = array(1, dim = dim(image1)[1:3])
  #   # compute cohen's d and plot
  #   cohend = compute_cohend(image1, image2, mask, breaks = c(-100:100)/100*2)
  # })
  # output$plot1 = renderPlot({
  #   cohend()
  # })


  output$downloadPlot = downloadHandler(
    file = function() {
      paste('imaging_', Sys.Date(), '.png', sep = '')
    },
    content = function(file) {
      png(file)
      z = compute_cohend(image1, image2, mask, breaks = c(-100:100)/100*2)
      # cohend()
      dev.off()
    }
  )
}


