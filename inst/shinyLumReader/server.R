
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(LumReader)
library(tools)
library(plotly)
library(lattice)
library(gridExtra)

shinyServer(function(input, output,session) {

  #------------------------------------------------------
  # TRICKS
  #------------------------------------------------------

  session$onSessionEnded(stopApp)   #Remove error when closing app...

  #------------------------------------------------------


  #------------------------------------------------------
  # Filters & FilterStack
  #------------------------------------------------------

  output$filtersInput <- renderUI({
    nFilters <- input$nFilters
    if(nFilters == 1){
      fluidRow(column(width = 12,
                      offset = 0,
                      selectInput(inputId = "filter1method",
                                  label = "1st Filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
                      ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter1name")
                      ),
               column(width = 4,
                      uiOutput(outputId = "filter1thickness")
                      ),
               column(width = 2,
                      helpText("[mm]")
                      )
               )

    }else if(nFilters == 2){
      fluidRow(column(width = 12,
                      selectInput(inputId = "filter1method",
                                  label = "1st filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
                      ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter1name")
                      ),
               column(width = 4,
                      uiOutput(outputId = "filter1thickness")
                      ),
               column(width = 2,
                      helpText("[mm]")
                      ),

               column(width = 12,
                      selectInput(inputId = "filter2method",
                                  label = "2nd filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter2name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter2thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ))

    }else if(nFilters == 3){
      fluidRow(column(width = 12,
                      selectInput(inputId = "filter1method",
                                  label = "1st filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
                      ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter1name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter1thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ),
               column(width = 12,
                      selectInput(inputId = "filter2method",
                                  label = "2nd filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter2name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter2thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ),

               column(width = 12,
                      selectInput(inputId = "filter3method",
                                  label = "3rd filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter3name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter3thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ))
    }else if(nFilters == 4){
      fluidRow(column(width = 12,
                      selectInput(inputId = "filter1method",
                                  label = "1st filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
                      ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter1name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter1thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ),

               column(width = 12,
                      selectInput(inputId = "filter2method",
                                  label = "2nd filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter2name")
               ),
               column(width = 4,
                      uiOutput(outputId = "filter2thickness")
               ),
               column(width = 2,
                      helpText("[mm]")
               ),

               column(width = 12,
                      selectInput(inputId = "filter3method",
                                  label = "3rd filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter3name")
                      ),
               column(width = 4,
                      uiOutput(outputId = "filter3thickness")
                      ),
               column(width = 2,
                      helpText("[mm]")
                      ),

               column(width = 12,
                      selectInput(inputId = "filter4method",
                                  label = "4th filter",
                                  choices = c("default","shiny","import"),
                                  selected = "shiny")
               ),
               column(width = 6,
                      offset = 0,
                      uiOutput(outputId = "filter4name")
                      ),
               column(width = 4,
                      uiOutput(outputId = "filter4thickness")
                      ),
               column(width = 2,
                      helpText("[mm]")
               ))
    }else{
     helpText("You can only have 4 filters")
    }
  })

  output$filter1name <- renderUI({

    method <- input$filter1method

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId = "filter1name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/filters")

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "filter1name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "filter1name",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  output$filter1thickness <- renderUI({

    method <- input$filter1method
    name <- input$filter1name

    if(is.null(name)){
      numericInput(inputId = "filter1thickness",
                   label = NULL,
                   value = 1,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }else{
      if(method == "default"){
        filter <- default_Filters(name)
        thickness <- filter[[1]]@reference.thickness

      }else if(method == "shiny"){
        file.name <- paste("data/filters/",name,".FLT",sep = "")

        filter <- import_Filter(file.name = file.name)
        thickness <- filter@reference.thickness

      }else if(method == "import"){

        if(is.list(name)){
          path <- name$datapath
          new.path <- file_path_sans_ext(path)
          ext <- ".FLT"
          new.path <- paste(new.path, ext,sep="")

          file.rename(from=path,
                      to=new.path)

          file.name <- new.path

          filter <- import_Filter(file.name = file.name)

          thickness <- filter@reference.thickness
        }else{
          thickness <- 0
        }

      }else{
        thickness <- 0
      }

      numericInput(inputId = "filter1thickness",
                   label = NULL,
                   value = thickness,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }
  })

  # Filter 1
  filter1 <- reactive({

    method <- input$filter1method
    name <- input$filter1name
    thickness <- input$filter1thickness

    if(is.null(name) || is.null(method) || is.null(thickness)){
      method <- "default"
      name <- "none"
      thickness <- 1
    }

    if(method == "default"){
      filter <- default_Filters(names = name,
                                 thickness = thickness)
      filter <- filter[[1]]

    }else if(method == "shiny"){
      file.name <- paste("data/filters/",name,".FLT",sep = "")

      filter <- import_Filter(file.name = file.name,
                               thickness = thickness)

    }else if(method == "import"){

      if(is.list(name)){
        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".FLT"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        filter <- import_Filter(file.name = file.name,
                                thickness = thickness)
      }else{
        filter <- NULL
      }

    }else{
      filter <- NULL
    }

    return(filter)
  })

  #Filter 2
  output$filter2name <- renderUI({

    method <- input$filter2method

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId = "filter2name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/filters")

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                             replacement = "",
                                             x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "filter2name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "filter2name",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  output$filter2thickness <- renderUI({

    method <- input$filter2method
    name <- input$filter2name

    if(is.null(name)){
      numericInput(inputId = "filter2thickness",label = NULL,
                   value = 1,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }else{
      if(method == "default"){
        filter <- default_Filters(name)
        thickness <- filter[[1]]@reference.thickness

      }else if(method == "shiny"){
        file.name <- paste("data/filters/",name,".FLT",sep = "")

        filter <- import_Filter(file.name = file.name)
        thickness <- filter@reference.thickness

      }else if(method == "import"){

        if(is.list(name)){
          path <- name$datapath
          new.path <- file_path_sans_ext(path)
          ext <- ".FLT"
          new.path <- paste(new.path, ext,sep="")

          file.rename(from=path,
                      to=new.path)

          file.name <- new.path

          filter <- import_Filter(file.name = file.name)
          thickness <- filter@reference.thickness

        }else{
          thickness <- 0
        }

      }else{
        thickness <- 0
      }

      numericInput(inputId = "filter2thickness",label = NULL,
                   value = thickness,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }
  })

  filter2 <- reactive({

    method <- input$filter2method
    name <- input$filter2name
    thickness <- input$filter2thickness

    if(is.null(name) || is.null(method) || is.null(thickness)){
      method <- "default"
      name <- "none"
      thickness <- 1
    }

    if(method == "default"){
      filter <- default_Filters(names = name,
                                thickness = thickness)

      filter <- filter[[1]]

    }else if(method == "shiny"){
      file.name <- paste("data/filters/",name,".FLT",sep = "")

      filter <- import_Filter(file.name = file.name,
                              thickness = thickness)

    }else if(method == "import"){

      if(is.list(name)){
        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".FLT"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        filter <- import_Filter(file.name = file.name,
                                thickness = thickness)
      }else{
        filter <- NULL

      }
    }else{
      filter <- NULL
    }

    return(filter)
  })

  #Filter 3

  output$filter3name <- renderUI({

    method <- input$filter3method

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId = "filter3name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/filters")

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                             replacement = "",
                                             x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "filter3name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "filter3name",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  output$filter3thickness <- renderUI({
    method <- input$filter3method
    name <- input$filter3name

    if(is.null(name)){
      numericInput(inputId = "filter3thickness",label = NULL,
                   value = 1,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }else{
      if(method == "default"){
        filter <- default_Filters(name)
        thickness <- filter[[1]]@reference.thickness

      }else if(method == "shiny"){
        file.name <- paste("data/filters/",name,".FLT",sep = "")

        filter <- import_Filter(file.name = file.name)
        thickness <- filter@reference.thickness

      }else if(method == "import"){

        if(is.list(name)){
          path <- name$datapath
          new.path <- file_path_sans_ext(path)
          ext <- ".FLT"
          new.path <- paste(new.path, ext,sep="")

          file.rename(from=path,
                      to=new.path)

          file.name <- new.path

          filter <- import_Filter(file.name = file.name)
          thickness <- filter@reference.thickness

        }else{
          thickness <- 0
        }

      }else{
        thickness <- 0
      }

      numericInput(inputId = "filter3thickness",label = NULL,
                   value = thickness,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }
  })

  filter3 <- reactive({

    method <- input$filter3method
    name <- input$filter3name
    thickness <- input$filter3thickness

    if(is.null(name) || is.null(method) || is.null(thickness)){
      method <- "default"
      name <- "none"
      thickness <- 1
    }

    if(method == "default"){
      filter <- default_Filters(names = name,
                                thickness = thickness)
      filter <- filter[[1]]

    }else if(method == "shiny"){
      file.name <- paste("data/filters/",name,".FLT",sep = "")

      filter <- import_Filter(file.name = file.name,
                              thickness = thickness)

    }else if(method == "import"){

      if(is.list(name)){
        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".FLT"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        filter <- import_Filter(file.name = file.name,
                                thickness = thickness)
      }else{
        filter <- NULL
      }

    }else{
      filter <- NULL
    }

    return(filter)
  })

  #Filter 4

  output$filter4name <- renderUI({

    method <- input$filter4method

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId = "filter4name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/filters")

      filterList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".FLT",all.file.names[i])){
          filterList <- c(filterList,gsub(pattern = ".FLT",
                                             replacement = "",
                                             x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "filter4name",
                  label = NULL,
                  choices = filterList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "filter4name",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  output$filter4thickness <- renderUI({
    method <- input$filter4method
    name <- input$filter4name

    if(is.null(name)){
      numericInput(inputId = "filter1thickness",label = NULL,
                   value = 1,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }else{
      if(method == "default"){
        filter <- default_Filters(name)
        thickness <- filter[[1]]@reference.thickness

      }else if(method == "shiny"){
        file.name <- paste("data/filters/",name,".FLT",sep = "")

        filter <- import_Filter(file.name = file.name)
        thickness <- filter@reference.thickness

      }else if(method == "import"){

        if(is.list(name)){
          path <- name$datapath
          new.path <- file_path_sans_ext(path)
          ext <- ".FLT"
          new.path <- paste(new.path, ext,sep="")

          file.rename(from=path,
                      to=new.path)

          file.name <- new.path

          filter <- import_Filter(file.name = file.name)
          thickness <- filter@reference.thickness

        }else{
          thickness <- 0
        }

      }else{
        thickness <- 0
      }

      numericInput(inputId = "filter4thickness",label = NULL,
                   value = thickness,
                   min = 0.1,
                   max = 10,
                   step = 0.1)
    }
  })

  filter4 <- reactive({

    method <- input$filter4method
    name <- input$filter4name
    thickness <- input$filter4thickness

    if(is.null(name) || is.null(method) || is.null(thickness)){
      method <- "default"
      name <- "none"
      thickness <- 1
    }

    if(method == "default"){
      filter <- default_Filters(names = name,
                                thickness = thickness)
      filter <- filter[[1]]

    }else if(method == "shiny"){
      file.name <- paste("data/filters/",name,".FLT",sep = "")

      filter <- import_Filter(file.name = file.name,
                              thickness = thickness)

    }else if(method == "import"){
      if(is.list(name)){
        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".FLT"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        filter <- import_Filter(file.name = file.name,
                                thickness = thickness)
      }else{
        filter <- NULL
      }

    }else{
      filter <- NULL
    }

    return(filter)
  })

  # Filter Stack

  filterStack <- reactive({

    name <- input$filterStackName
    description <- input$filterStackDescription

    filter1 <- filter1()
    filter2 <- filter2()
    filter3 <- filter3()
    filter4 <- filter4()

    filters <- list()
    filters <- c(filter1, filter2, filter3, filter4)

    if(length(filters)>0){
      new.filters <- list()
      for(i in 1:4){
        if(filters[[i]]@name != "none" && filters[[i]]@name != ""){
          new.filters <- c(new.filters,filters[[i]])
        }
      }

      if(length(new.filters) == 0){
        new.filters[[1]] <- filters[[1]]
      }

      filterStack <- create_FilterStack(name = name,
                                        description = description,
                                        filters = new.filters)
    }else{
      filterStack <- NULL
    }




    return(filterStack)
  })


  #Output
  output$filterPlots <- renderUI({
    nFilters <- input$nFilters

    filter1 <- filter1()
    filter2 <- filter2()
    filter3 <- filter3()
    filter4 <- filter4()

    if(nFilters == 1){

      fluidRow(column(width = 12,
                      renderPlot({if(is(filter1,"Filter")){plot_Filter(filter1)}})
      ))

    }else if(nFilters == 2){

      fluidRow(column(width = 6,
                      renderPlot({if(is(filter1,"Filter")){plot_Filter(filter1)}})
      ),
               column(width = 6,
                      renderPlot({if(is(filter2,"Filter")){plot_Filter(filter2)}})
               ))

    }else if(nFilters == 3){

      fluidRow(column(width = 4,
                      renderPlot({if(is(filter1,"Filter")){plot_Filter(filter1)}})
      ),
      column(width = 4,
             renderPlot({if(is(filter2,"Filter")){plot_Filter(filter2)}})
      ),
      column(width = 4,
             renderPlot({if(is(filter3,"Filter")){plot_Filter(filter3)}})
      ))

    }else if(nFilters == 4){

      fluidRow(column(width = 3,
                      renderPlot({if(is(filter1,"Filter")){plot_Filter(filter1)}})
      ),
      column(width = 3,
             renderPlot({if(is(filter2,"Filter")){plot_Filter(filter2)}})
      ),
      column(width = 3,
             renderPlot({if(is(filter3,"Filter")){plot_Filter(filter3)}})
      ),
      column(width = 3,
             renderPlot({if(is(filter4,"Filter")){plot_Filter(filter4)}})
      ))
    }
  })

  output$filterStackPlot <- renderPlot({
    filterStack <- filterStack()

    if(is.null(filterStack)){
      return(NULL)
    }

    if(is(filterStack, "FilterStack")){
      plot_FilterStack(filterStack)
    }

  })

  #------------------------------------------------------
  #Detection
  #------------------------------------------------------

  #input
  output$detectionName <- renderUI({

    method <- input$detectionMethod

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      detectionList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".PMT",all.file.names[i])){
          detectionList <- c(detectionList,gsub(pattern = ".PMT",
                                          replacement = "",
                                          x = all.file.names[i]))
        }
      }

      selectInput(inputId = "detectionName",
                  label = NULL,
                  choices = detectionList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/detections")

      detectionList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".PMT",all.file.names[i])){
          detectionList <- c(detectionList,gsub(pattern = ".PMT",
                                             replacement = "",
                                             x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "detectionName",
                  label = NULL,
                  choices = detectionList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "detectionName",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  detection <- reactive({
    method <-  input$detectionMethod
    name <- input$detectionName

    if(is.null(name) || is.null(method)){
      method <- "default"
      name <- "none"
    }

    if(method == "default"){
      detection <- default_PMT(name = name)

    }else if(method == "shiny"){
      file.name <- paste("data/detections/",name,".PMT",sep = "")

      detection <- import_PMT(file.name = file.name)

    }else if(method == "import"){

      if(is.list(name)){
        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".PMT"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        detection <- import_PMT(file.name = file.name)
      }else{
        detection <- NULL
      }

    }else{
      detection <- NULL
    }

    return(detection)

  })

  #output
  output$detectionPlot <- renderPlot({

    detection <- detection()

    if(is(detection, "PMT")){
      plot_PMT(detection)
    }

  })

  #------------------------------------------------------
  # Stimulation
  #------------------------------------------------------

  # input
  output$stimulationName <- renderUI({

    method <- input$stimulationMethod

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      stimulationList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".EXI",all.file.names[i])){
          stimulationList <- c(stimulationList,gsub(pattern = ".EXI",
                                                replacement = "",
                                                x = all.file.names[i]))
        }
      }

      selectInput(inputId = "stimulationName",
                  label = NULL,
                  choices = stimulationList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/stimulations")

      stimulationList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".EXI",all.file.names[i])){
          stimulationList <- c(stimulationList,gsub(pattern = ".EXI",
                                                replacement = "",
                                                x = all.file.names[i]))
        }
      }

      selectInput(inputId =  "stimulationName",
                  label = NULL,
                  choices = stimulationList,
                  selected = "none")

    }else if(method == "import"){
      fileInput(inputId = "stimulationName",
                label = NULL)
    }else{
      helpText("This method is not supported")
    }
  })

  stimulation <- reactive({

    method <-  input$stimulationMethod
    name <- input$stimulationName

    if(is.null(name) || is.null(method)){
      method <- "default"
      name <- "none"
    }

    if(method == "default"){
      stimulation <- default_Stimulation(name = name)

    }else if(method == "shiny"){

      file.name <- paste("data/stimulations/",name,".EXI",sep = "")
      stimulation <- import_Stimulation(file.name = file.name)

    }else if(method == "import"){

      if(is.list(name)){

        path <- name$datapath
        new.path <- file_path_sans_ext(path)
        ext <- ".EXI"
        new.path <- paste(new.path, ext,sep="")

        file.rename(from=path,
                    to=new.path)

        file.name <- new.path

        stimulation <- import_Stimulation(file.name = file.name)

      }else{
        stimulation <- NULL
      }

    }else{
      stimulation <- NULL
    }

    return(stimulation)
  })

  #output
  output$stimulationPlot <- renderPlot({

    stimulation <- stimulation()

    if(is(stimulation, "Stimulation")){
      plot_Stimulation(stimulation)
    }
  })

  #------------------------------------------------------
  # Reader
  #------------------------------------------------------
  #input
  reader <- reactive({
    name <- input$readerName
    description <- input$readerDescription

    filterStack <- filterStack()
    detection <- detection()
    stimulation <- stimulation()

    if(is.null(filterStack) || is.null(detection) || is.null(stimulation)){
      return(NULL)
    }

    reader <- create_Reader(name = name,
                            description = description,
                            stimulation = stimulation,
                            filterStack = filterStack,
                            PMT = detection)

    return(reader)

  })

  #output
  output$readerPlot <- renderPlot({

    reader <- reader()

    if(is(reader, "Reader")){
      plot_Reader(object = reader)
    }
  })

  #------------------------------------------------------
  # Material
  #------------------------------------------------------

  #input
  output$materialName <- renderUI({

    method <- input$materialMethod

    if(method == "default"){
      all.file.names <- dir(system.file("extdata", package="LumReader"))

      materialList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".TL",all.file.names[i])){
          materialList <- c(materialList,gsub(pattern = ".TL",
                                                    replacement = "",
                                                    x = all.file.names[i]))
        }

        if(grepl(".OSL",all.file.names[i])){
          materialList <- c(materialList,gsub(pattern = ".OSL",
                                              replacement = "",
                                              x = all.file.names[i]))
        }
      }
      materialList <- unique(materialList)

      selectInput(inputId = "materialName",
                  label = NULL,
                  choices = materialList,
                  selected = "none")

    }else if(method == "shiny"){
      all.file.names <- dir("data/materials")

      if(length(all.file.names) < 1 ){
        all.file.names <- ""
      }

      materialList <- vector()

      for(i in 1:length(all.file.names)){
        if(grepl(".TL",all.file.names[i])){
          materialList <- c(materialList,gsub(pattern = ".TL",
                                                    replacement = "",
                                                    x = all.file.names[i]))
        }

        if(grepl(".OSL",all.file.names[i])){
          materialList <- c(materialList,gsub(pattern = ".OSL",
                                              replacement = "",
                                              x = all.file.names[i]))
        }
      }
      materialList <- unique(materialList)

      if(length(materialList) < 1 ){
        materialList <- ""
      }

      selectInput(inputId =  "materialName",
                  label = NULL,
                  choices = materialList,
                  selected = "none")

    }else if(method == "import"){
      fluidRow(column(width = 6,
                      fileInput(inputId = "materialTLfile",
                                label = "TL")
                      ),
               column(width = 6,
                      fileInput(inputId = "materialOSLfile",
                                label = "OSL")
                      )
               )

    }else{
      helpText("This method is not supported")
    }
  })

  material <- eventReactive(input$materialButton,{
  #material <- reactive({

    method <-  input$materialMethod
    name <- input$materialName
    TLfile <- input$materialTLfile
    OSLfile <- input$materialOSLfile

    if(is.null(method)){
      method <- "default"
      name <- "none"
    }

    if(is.null(name) && is.null(TLfile) && is.null(OSLfile)){
      return(NULL)
    }else if(name == ""){
      return(NULL)
    }

    if(method == "default"){

      material <- default_Material(name = name)

    }else if(method == "shiny"){

      file.name <- paste("data/materials/",name, sep = "")
      material <- import_Material(file.name = file.name)

    }else if(method == "import"){

      name <- c(TLfile, OSLfile)

      if(is.list(name)){
        TLpath <- TLfile$datapath
        OSLpath <- OSLfile$datapath

        newpath <- file_path_sans_ext(TLpath)

        TLext <- ".TL"
        OSLext <- ".OSL"

        newOSLpath <- paste(newpath, OSLext,sep = "")
        newTLpath <- paste(newpath, TLext,sep = "")

        file.rename(from = OSLpath,
                    to = newOSLpath)

        file.rename(from = TLpath,
                    to = newTLpath)

        file.name <- newpath
        material <- import_Material(file.name = file.name)

      }else{
        material <- NULL
      }

    }else{
      material <- NULL
    }

    return(material)
  })

  #-------------
  #output

  output$material.TL.2D <- renderPlot({
    material <- material()

    name <- material@name

    # TL
    description.TL <- material@description.TL

    TL <- material@TL

    TL.wavelength <- TL[,1]
    TL.temperature <- TL[,2]
    TL.signal <- TL[,3]

    TL.x <- unique(TL.wavelength)
    TL.y <- unique(TL.temperature)
    TL.z <- matrix(data=TL.signal,
                   nrow = length(TL.x),
                   ncol = length(TL.y),
                   byrow = TRUE)

    # contour plot
    #TL
    TL.levelplot <- levelplot(x= TL.z,
                              row.values=TL.x,
                              column.values=TL.y,
                              xlab="Emission wavelength [nm]",
                              ylab="Temperature [\u00b0C]",
                              main=paste("Intensity of the TL emission of", name, "[u.a]"),
                              cuts=39,
                              col.regions=rev(heat.colors(n = 40,alpha = 1)),
                              colorkey=TRUE)
    grid.arrange(TL.levelplot, nrow=1, ncol=1, respect=FALSE)
  })

  output$material.OSL.2D <- renderPlot({

    material <- material()

    name <- material@name

    # OSL
    description.OSL <- material@description.OSL

    OSL <- material@OSL

    OSL.wavelength <- OSL[,1]
    OSL.color <- OSL[,2]
    OSL.signal <- OSL[,3]

    OSL.x <- unique(OSL.wavelength)
    OSL.y <- unique(OSL.color)
    OSL.z <- matrix(data=OSL.signal,
                    nrow = length(OSL.x),
                    ncol = length(OSL.y),
                    byrow = TRUE)

    # contour plot
    OSL.levelplot <- levelplot(x= OSL.z,
                               row.values=OSL.x,
                               column.values=OSL.y,
                               xlab="Emission wavelength [nm]",
                               ylab="Stimulation wavelength [nm]",
                               main=paste("Intensity of the OSL emission of", name, "[u.a]"),
                               cuts=39,
                               col.regions=rev(terrain.colors(n = 40,alpha = 1)),
                               colorkey=TRUE)

    grid.arrange(OSL.levelplot, nrow=1, ncol=1, respect=FALSE)
  })

  output$material.TL.3D <- renderPlotly({
    material <- material()

    name <- material@name

    # TL
    description.TL <- material@description.TL

    TL <- material@TL

    TL.wavelength <- TL[,1]
    TL.temperature <- TL[,2]
    TL.signal <- TL[,3]

    TL.x <- unique(TL.wavelength)
    TL.y <- unique(TL.temperature)
    TL.z <- matrix(data=TL.signal,
                   nrow = length(TL.x),
                   ncol = length(TL.y),
                   byrow = TRUE)

    # Plotly
    TL.3D <- plot_ly(x = TL.x,
                     y = TL.y,
                     z = TL.z,
                     type = "surface")
    # TL.3D <- plot_ly(z = TL.z, type = "surface")

    TL.3D.title <- paste("Intensity of the TL emission of", name, "[u.a]")

    TL.3D.scene <- list(xaxis=list(title="Emission wavelength [nm]"),
                        yaxis=list(title="Temperature [\u00b0C]"),
                        zaxis=list(title="Intensity [a.u.]"))

    layout(p = TL.3D,
           title=TL.3D.title,
           scene=TL.3D.scene)
  })

  output$material.OSL.3D <- renderPlotly({
    material <- material()

    name <- material@name

    # OSL
    description.OSL <- material@description.OSL

    OSL <- material@OSL

    OSL.wavelength <- OSL[,1]
    OSL.color <- OSL[,2]
    OSL.signal <- OSL[,3]

    OSL.x <- unique(OSL.wavelength)
    OSL.y <- unique(OSL.color)
    OSL.z <- matrix(data=OSL.signal,
                    nrow = length(OSL.x),
                    ncol = length(OSL.y),
                    byrow = TRUE)

    #plotly
    OSL.3D <- plot_ly(x=OSL.x,
                      y=OSL.y,
                      z=OSL.z,
                      type = "surface")
    # OSL.3D <- plot_ly(z=OSL.z, type = "surface")

    OSL.3D.title <- paste("Intensity of the OSL emission of", name, "[u.a]")

    OSL.3D.scene <- list(xaxis=list(title="Emission wavelength [nm]"),
                         yaxis=list(title="Stimulation wavelength [nm]"),
                         zaxis=list(title="Intensity [a.u.]"))

    layout(p = OSL.3D,
           title=OSL.3D.title,
           scene=OSL.3D.scene)

  })

  output$materialPlot <- renderUI({

    material <- material()

    if(is(material, "Material")){
      tabsetPanel(tabPanel(title = "Level plot",
                           plotOutput(outputId = "material.TL.2D"),
                           plotOutput(outputId = "material.OSL.2D")
                           ),
                  tabPanel("TL",
                           plotlyOutput(outputId = "material.TL.3D",width = "auto",height = "auto")
                  ),
                  tabPanel("OSL",
                           plotlyOutput(outputId = "material.OSL.3D",width = "auto",height = "auto")

                  ))
    }
  })

  #------------------------------------------------------
  # Material
  #------------------------------------------------------

  # input
  output$experimentSlider <- renderUI({

    material <- material()
    stimulation <- stimulation()

    if(is.null(stimulation) || is.null(material)){
      return(NULL)
    }

    type <- stimulation@type


    if(type == "TL"){
      sliderMin <- min(material@TL[,2])
      sliderMax <- max(material@TL[,2])

      peak.temperature <- 360
      interval <- c(peak.temperature-40,peak.temperature+40)

      sliderInput(inputId = "experimentSlider",
                  label = "Region of interest [°C]",
                  min = sliderMin,
                  max = sliderMax,
                  value = interval)

    }else if(type == "OSL"){
      sliderMin <- min(material@OSL[,2])
      sliderMax <- max(material@OSL[,2])

      emission <- stimulation@emission
      peak.max <- max(emission[,2])
      peak.wavelength <- mean(emission[emission[,2]== peak.max, 1])

      interval <- c(peak.wavelength-20,peak.wavelength+20)

      sliderInput(inputId = "experimentSlider",
                  label = "Region of interest [nm]",
                  min = sliderMin,
                  max = sliderMax,
                  value = interval)
    }
  })

  experiment <- reactive({
    name <- input$experimentName
    description <- input$experimentDescription
    reader <- reader()
    material <- material()
    stimulation <- stimulation()

    if(is.null(reader) || is.null(material) || is.null(stimulation)){
      return(NULL)
    }

    interval <- input$experimentSlider

    type <- stimulation@type

    experiment <- create_Experiment(name = name,
                                    description = description,
                                    reader = reader,
                                    material = material,
                                    type = type,
                                    interval = interval)

    return(experiment)
  })

  # output

  output$plotExperimentMaterial <- renderPlot({

    experiment <- experiment()

    name <- experiment@name
    description <- experiment@description

    reader <- reader()
    material <- material()
    emission <- experiment@emission
    detected <- experiment@detected

    type <- experiment@type
    interval <- experiment@interval

    # page 2: material


    if(type == "TL"){

      material.name <- material@name
      material.description <- material@description.TL
      material.signal <- material@TL

      TL.wavelength <- material.signal[,1]
      TL.temperature <- material.signal[,2]
      TL.signal <- material.signal[,3]

      material.x <- unique(TL.wavelength)
      material.y <- unique(TL.temperature)
      material.z <- matrix(data=TL.signal,
                           nrow = length(material.x),
                           ncol = length(material.y),
                           byrow = TRUE)

      material.levelplot <- levelplot(x= material.z,
                                      row.values=material.x,
                                      column.values=material.y,
                                      xlab="Emission wavelength [nm]",
                                      ylab="Temperature [\u00b0C]",
                                      main=paste("Intensity of the",type, "emission of", name, "[u.a]"),
                                      cuts=39,
                                      col.regions=rev(heat.colors(n = 40,alpha = 1)),
                                      colorkey=TRUE,
                                      panel = function(...){
                                        panel.levelplot(...)
                                        panel.abline(h = interval[1])
                                        panel.abline(h = interval[2])
                                      })

    }else if(type == "OSL"){
      material.name <- material@name
      material.description <- material@description.OSL
      material.signal <- material@OSL

      OSL.wavelength <- material.signal[,1]
      OSL.temperature <- material.signal[,2]
      OSL.signal <- material.signal[,3]

      material.x <- unique(OSL.wavelength)
      material.y <- unique(OSL.temperature)
      material.z <- matrix(data=OSL.signal,
                           nrow = length(material.x),
                           ncol = length(material.y),
                           byrow = TRUE)

      material.levelplot <- levelplot(x= material.z,
                                      row.values=material.x,
                                      column.values=material.y,
                                      xlab="Emission wavelength [nm]",
                                      ylab="Stimulation wavelength [nm]",
                                      main=paste("Intensity of the",type, "emission of", name, "[u.a]"),
                                      cuts=39,
                                      col.regions=rev(terrain.colors(n = 40,alpha = 1)),
                                      colorkey=TRUE,
                                      panel = function(...){
                                        panel.levelplot(...)
                                        panel.abline(h = interval[1])
                                        panel.abline(h = interval[2])
                                      })
    }

    grid.arrange(material.levelplot)
  })

  output$plotExperimentEmission <- renderPlot({

    experiment <- experiment()
    name <- experiment@name
    description <- experiment@description

    reader <- reader()

    material <- material()
    emission <- experiment@emission
    detected <- experiment@detected


    #stimulation <- stimulation()
    detection <- detection()

    type <- experiment@type
    interval <- experiment@interval

    # Plot
    old.par <- par( no.readonly = TRUE )
    #page 3: emission
    #Layout
    par(oma = c(0.5, 0, 3, 0 ),
        mar = c(5,5,4,5) )

    colors <- c("orange", "blue", "black", "forestgreen","red")

    title <- name
    subtitle <- description

    legend.text <- vector()
    legend.col <- vector()
    legend.pch <- vector()


    # Stimulation
    #par(mar = c(5,5,4,5) )

    temp.name <- reader@stimulation@description
    temp.color <- colors[1]
    temp.x <- reader@stimulation@emission[,1]
    temp.y <- reader@stimulation@emission[,2]

    plot.x.min <- 200
    plot.x.max <- 1000

    plot.y.min <- 0
    plot.y.max <- max(temp.y)

    plot(x = temp.x,
         y = temp.y,
         xlim = c(plot.x.min,plot.x.max),
         ylim = c(plot.y.min,plot.y.max),
         yaxt = "n",
         xaxt = "n",
         xlab = "",
         ylab = "",
         type="l",
         col= temp.color)

    axis(4)
    mtext(side = 4,
          text = "Signal intensity [a.u.]",
          line = 2.5,
          cex = 0.8
    )

    polygon(x = c(plot.x.min,temp.x,plot.x.max),
            y = c(0,temp.y,0),
            col = temp.color,
            density=20)

    par(new = TRUE)

    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
    legend.col <- c(legend.col,temp.color)


    # Emission

    temp.name <- emission@description
    temp.color <- colors[2]
    temp.x <- emission@emission[,1]
    temp.y <- emission@emission[,2]

    polygon(x = c(plot.x.min,temp.x,plot.x.max),
            y = c(0,temp.y,0),
            col = temp.color,
            density=20)
    par(new = TRUE)

    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
    legend.col <- c(legend.col,temp.color)


    #Detected signal

    temp.name <- detected@description
    temp.color <- colors[3]
    temp.x <- detected@emission[,1]
    temp.y <- detected@emission[,2]

    polygon(x = c(plot.x.min,temp.x, plot.x.max),
            y = c(0,temp.y,0),
            col = temp.color,
            density=40,
            angle=135)
    par(new = TRUE)

    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
    legend.col <- c(legend.col, temp.color)

    # Detection windows

    temp.name <- reader@name
    temp.color <- colors[4]
    temp.x <- reader@detection@efficiency[,1]
    temp.y <- reader@detection@efficiency[,2]*100

    plot.y.max <- max(temp.y)
    plot(x=temp.x,
         y=temp.y,
         xlim = c(plot.x.min,plot.x.max),
         ylim = c(plot.y.min,plot.y.max),
         main = title,
         sub = subtitle,
         xlab =  "Wavelength [nm]",
         ylab = "Reader quantum efficiency [%]",
         type = "l",
         lwd=2,
         col=temp.color)

    par(new = TRUE)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18, legend.pch)
    legend.col <- c(temp.color,legend.col)

    # Interval

    if(type=="OSL"){
      temp.name <- "Stimulation interval"
      temp.color <- colors[5]

      abline(v=interval[1],
             lty=2,
             col=temp.color)
      abline(v=interval[2],
             lty=2,
             col=temp.color)

      legend.text <- c(legend.text,temp.name)
      legend.pch <- c(legend.pch, 18)
      legend.col <- c(legend.col,temp.color)
    }
    #Legend

    legend(x = "topleft",
           legend = legend.text,
           pch = legend.pch,
           col = legend.col,
           bty = "n")

    par(new = FALSE)

    par(old.par)

  })

  output$experimentPlot <- renderUI({
    experiment <- experiment()

    if(is(experiment,"Experiment")){
      tabsetPanel(tabPanel(title = "Experiment",
                           plotOutput(outputId = "plotExperimentMaterial")),
                  tabPanel("Emission",
                           plotOutput(outputId = "plotExperimentEmission"))
                  )
    }
  })

})
