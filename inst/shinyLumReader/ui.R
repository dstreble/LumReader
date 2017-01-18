
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage("ShinyLumReader",
                   tabPanel("Stimulation unit",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "stimulationMethod",
                                            label = "Stimulation unit",
                                            choices = c("default","shiny","import"),
                                            selected = "shiny"),
                                uiOutput(outputId = "stimulationName")
                              ),
                              mainPanel(
                                plotOutput(outputId = "stimulationPlot")
                              )) 
                   ),
                   tabPanel("Detection unit (PMT)",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "detectionMethod",
                                            label = "Detection unit",
                                            choices = c("default","shiny","import"),
                                            selected = "shiny"),
                                uiOutput(outputId = "detectionName")
                              ),
                              mainPanel(
                                plotOutput(outputId = "detectionPlot")                              ))
                   ),
                   tabPanel("Filters",
                            sidebarLayout(
                              sidebarPanel(
                                textInput(inputId = "filterStackName",
                                          label = "Name",
                                          placeholder = "required"),
                                textInput(inputId = "filterStackDescription",
                                          label = "Description",
                                          placeholder = "required"),
                                
                                numericInput(inputId = "nFilters",
                                             label = "Number of filters (max. 4)",
                                             value = 1,
                                             min = 0,
                                             max = 4,
                                             step = 1),
                                uiOutput(outputId = "filtersInput")
                                ),
                              mainPanel(
                                uiOutput(outputId = "filterPlots"),
                                plotOutput(outputId = "filterStackPlot")
                              ))
                            ),
                   tabPanel("Reader",
                            sidebarLayout(
                              sidebarPanel(
                                textInput(inputId = "readerName",
                                          label = "Name",
                                          placeholder = "required"),
                                textInput(inputId = "readerDescription",
                                          label = "Description",
                                          placeholder = "required")
                                ),
                              mainPanel(
                                plotOutput(outputId = "readerPlot")
                              )) 
                            ),
                   tabPanel("Material",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "materialMethod",
                                            label = "Material",
                                            choices = c("default","shiny","import"),
                                            selected = "shiny"),
                                uiOutput(outputId = "materialName"),
                                actionButton(inputId = "materialButton",label = "Generate"),
                                helpText("Warning: material creation can take some times.")
                              ),
                              mainPanel(
                                uiOutput(outputId = "materialPlot")
                              ))
                            ),
                   tabPanel("Experiment",
                            sidebarLayout(
                              sidebarPanel(
                                textInput(inputId = "experimentName",
                                          label = "Name",
                                          placeholder = "required"),
                                textInput(inputId = "experimentDescription",
                                          label = "Description",
                                          placeholder = "required"),
                                uiOutput(outputId = "experimentSlider")
                              ),
                              mainPanel(
                                uiOutput(outputId = "experimentPlot")
                              )
                            )),
                   tabPanel("help",
                            div(class="helpPage",
                                h4("Generalities"),
                                p("This app is based on the R package 'LumReader' (https://github.com/dstreble/LumReader). 
                                  The purpose of this app is to simulate luminescence readers and experiments.
                                  A luminescence reader is composed of 3 elements: a 'stimulation unit', a 'detection unit' and a set of filters, called 'filter stack', to select the signal that reach the detection unit."),
                                h4("Stimulation Unit"),
                                p("The first step to simulate your experiment is to define the type of stimulation you want to use.
                                  Diferent stimulation units are already included in the 'LumReader' package and in the 'shinyLumReader' app. 
                                  The first box allows you to select the origin of your stimulation unit (default= 'LumReader' package, shiny = 'shinyLumReader' app, import = your own source create using the package 'LumReader').
                                  The second box allows you to select the stimulation unit (TL, BSL, IRSL, etc.)."),
                                h4("Detection Unit"),
                                p("The second step is to select your detection unit. 
                                  In most of the case it will be a PMT, usually the '9235 QB'. However, for some application, like RTL, it is required to use an other PMT.
                                  Once again you can select a detection unit from the LumReader package or from the shiny app or import your own."),
                                h4("Filter Stack"),
                                p("The third step is to define the filters you will use for the experiment.
                                  You can combine up to 4 filters with this app (to combine more filters you will have to use the 'LumReader' package directly in R).
                                  The first 2 boxes allows you to define a name and a description for your 'filter stack.
                                  The 3rd box allows you to select the number of filters you want to combine. Warning, when you change this number, all the filters are reseted.
                                  For each filter you can select the source (default = package 'LumReader', shiny = the app, import = your own filter created using the package).
                                  Once you select a filter, you can modify its thickness. It is recommanded to not use filters thiner than their reference thickness.
                                  The first row of figures show the initial properties of the filters (without taking into account the thickness), the second row figure shows the properties of the complete filter stack."),
                                h4("Reader"),
                                p("This tab show the detection windows of the reader that you create and compares it to the stimulation signal. 
                                  The 2 box allow you to give a name and a description to your reader."),
                                h4("Material"),
                                p("A luminescence experiment requires a reader but also a material to analyse.
                                  This tab allows you to define this material. 
                                  Once again, you first select the origin of the material to use (package, app or your own) and then the material itself.
                                  Generating a material can take some time and require to combine two files. Therefore you will have to click on the 'Generate' button each time you want to upload a new material.
                                  The 'Level plot' tab contains 2 level plots, one for the TL emission properties of the material and one for its OSL emission properties.
                                  The 'TL' tab contains a 3d plot of the TL emission properties. 
                                  The 'OSL' tab contains a 3d plot of the OSL emission properties.
                                  The 3D plots of the 'TL' and 'OSL' tabs are created using the R-package 'plotly'.
                                  It is sometimes require to 'refresh' the tab to see them appear.
                                  To do it, just return on the 'levelplot' tab and come back to the 'TL'/'OSL' tab.
                                  Remember that luminescence properties of a material can vary a lot. The showed properties are theoretical properties defined for specific reference materials."),
                                h4("Experiment"),
                                p("The experiment tab gather all the data you select in the previous tab to create a luminescence experiment.
                                  Do not forget to 'generate' a material before consulting this tab.
                                  The sidebar option allows you to define a name and a description for your experiment but also a region of interest.
                                  For OSL experiments, the region of interest is defined by the stimulation unit (max \u00B1 20 \u03BCm).
                                  For TL experiments, the default region of interest is from 320 to 400 °C.
                                  The 'Experiment' tab contains the luminescence properties of the material and the region of interest.
                                  The 'Emission' tab contains the main emission and detection spectra of the experiment including: the detection windows of the reader, the stimulation signal, the material emission in the region of interest and the amount of signal which is detected by the detection unit."),
                                h4("Support"),
                                p("This app is currently maintained by David Strebler (david.strebler(at)uni(dash)koeln(dot)de).")
                                ))
))