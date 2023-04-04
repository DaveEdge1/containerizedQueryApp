library(shiny)
#library(remotes)
library(tidyr)
#remotes::install_github("nickmckay/lipdR")
library(lipdR)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinycssloaders)
library(DT)


usethis::proj_set('/root/euler')
usethis::create_package('/root/euler')

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

	  #queryTable <- lipdR:::newQueryTable()

	  #world <- ne_countries(scale = "medium", returnclass = "sf")
	  world <- map_data("world")

  D<-reactive({
	      queryLipdverse(variable.name = input$variable.name,
			                        archive.type = input$archiveType,
						                   paleo.proxy = input$paleo.proxy,
						                   paleo.units = input$paleo.units,
								                      coord = c(ymin(),
												                             ymax(),
															                                  xmin(),
															                                  xmax()),
			                        age.min = input$min.age,
						                   age.max = input$max.age,
						                   pub.info = NULL,
								                      country = NULL,
								                      continent = NULL,
										                         ocean = FALSE,
										                         seasonality = input$interpretation.seasonality,
													                    season.not = NULL,
													                    interp.vars = input$interpretation.variable,
															                       interp.details = NULL,
															                       compilation = input$compilation,
																	                          verbose = FALSE,
																	                          skip.update = FALSE
																				      )
	        })

    output$numTimeSeries <- reactive({
	        nrow(D())
		  })

    output$numDatasets <- reactive({
	        length(unique(D()$dataSetName))
		  })


      output$summary2 <- DT::renderDataTable({
	          data.frame(D()[,c(names(D())[14],names(D())[-14])])
		    })


      # yMin <- reactive({
      #   min(D()$geo_latitude)
      # })
      # yMax <- reactive({
      #   max(D()$geo_latitude)
      # })
      # xMin <- reactive({
      #   min(D()$geo_longitude)
      # })
      # xMax <- reactive({
      #   max(D()$geo_longitude)
      # })

      # world <- reactive({
      #   world1[world1$lat > input$min.lat &
      #           world1$lat < input$max.lat &
      #           world1$long > input$min.lon &
      #           world1$long < input$max.lon,]
      # })

      #newColor <- observe(input$pointColor)

      output$plot <- renderPlot({
	          ggplot(data=world, aes(x = long, y = lat, group = group)) +
			        geom_polygon(color="black", fill=alpha("grey50", 0.2)) +
				      # coord_map(
				      #   projection = "mercator", #orientation = c(0, 90, 0),
				      #   xlim = c(xmin(),xmax()),
				      #   ylim = c(ymin(),ymax())
				      #   )+
				      coord_cartesian(        xlim = c(xmin(),xmax()),
						                                    ylim = c(ymin(),ymax()))+
				      geom_jitter(data = D(), inherit.aes = FALSE,
						                   mapping = aes(x=as.numeric(geo_longitude),
										                                y=as.numeric(geo_latitude),
														                               color=get(input$pointColor))) +
				      # scale_y_continuous(limits = c(ymin(),ymax())) +
				      # scale_x_continuous(limits = c(xmin(),xmax())) +
				      xlab("") +
				            ylab("") +
					          theme(legend.title = element_blank())
					    })

        #D()$geo_longitude



        # Downloadable csv of selected dataset ----
        output$SummaryCSV <- downloadHandler(
					         filename = function() {
							       paste("LiPDsummary", Sys.time(), ".csv", sep = "")
						     },
						         content = function(file) {
								       write.csv(data.frame(D()[,c(names(D())[14],names(D())[-14])]), file)

						         }
						       )


        #download zipped lpd files
        output$ZippedLipdData <- downloadHandler(
						     filename = function() {
							           paste("newLIPD", Sys.time(), ".zip", sep = "")
						         },
							     content = function(file) {
								           writeLipd(readLipd(D()), file)


							       owd <- setwd(tempdir())
							             on.exit(setwd(owd))
							             files <- NULL;

								           outputData <- readLipd(D())

								           #loop through the sheets
								           for (i in 1:length(outputData)){
										           #write each sheet to a csv file, save the name
										           fileName <- paste(outputData[[i]]$dataSetName,".lpd",sep = "")
									           writeLipd(D = outputData[i], path = fileName)
										           files <- c(fileName,files)
										           message(fileName)
											         }
									         #create the zip file
									         zip(file,files)
									       }
							   )

	  x_range <- function(e) {
		      if(is.null(e)) return(c(-180,180))
	      c(round(e$xmin, 1), round(e$xmax, 1))
	        }

	  y_range <- function(e) {
		      if(is.null(e)) return(c(-90,90))
	      c(round(e$ymin, 1), round(e$ymax, 1))
	        }

	    #output$xmin <- reactive({x_range(input$plot_brush)[1]})

	    xmin <- reactive({x_range(input$plotBrush)[1]})
	    xmax <- reactive({x_range(input$plotBrush)[2]})

	      #output$ymin <- reactive({y_range(input$plot_brush)[1]})

	      ymin <- reactive({y_range(input$plotBrush)[1]})
	      ymax <- reactive({y_range(input$plotBrush)[2]})

	        brush <- NULL
	        makeReactiveBinding("brush")

		  observeEvent(input$plotBrush, {
				           brush <<- input$plotBrush
					     })

		  observeEvent(input$clearBrush, {
				           session$resetBrush("plotBrush")
					     })

		    observeEvent(input$resetPlot, {
					     session$resetBrush("plotBrush")
					         brush <<- NULL
					       })



}

# Define UI for dataset viewer app ----
ui <- fluidPage(

		  # App title ----
		  titlePanel("LIPDverse Query"),

		    # Sidebar layout with input and output definitions ----
		    sidebarLayout(

				      # Sidebar panel for inputs ----
				      sidebarPanel(


						         # Input: Text for providing a caption ----
						         # Note: Changes made to the caption in the textInput control
						         # are updated in the output area immediately as you type
						         selectizeInput(inputId = "variable.name",
									                label = "variable.name:",
											                choices = unique(queryTable$paleoData_variableName),
											                multiple=TRUE),

						         selectizeInput(inputId = "archiveType",
									                label = "Archive Type:",
											                choices = unique(queryTable$archiveType),
											                multiple=TRUE),

						         selectizeInput(inputId = "paleo.proxy",
									                label = "Paleo Proxy:",
											                choices = unique(queryTable$paleoData_proxy),
											                multiple=TRUE),

						         selectizeInput(inputId = "paleo.units",
									                label = "Paleo Units:",
											                choices = unique(queryTable$paleoData_units),
											                multiple=TRUE),
						         selectizeInput(inputId = "compilation",
									                     label = "Compilation:",
											                          choices = unique(queryTable$paleoData_mostRecentCompilations),
											                          multiple=TRUE),
						         selectizeInput(inputId = "interpretation.variable",
									                     label = "Interpretation Variable:",
											                          choices = unique(queryTable$interp_Vars),
											                          multiple=TRUE),
						         selectizeInput(inputId = "interpretation.seasonality",
									                     label = "Interpretation Seasonality:",
											                          choices = unique(queryTable$interpretation1_seasonality),
											                          multiple=TRUE),
						         numericInput(inputId = "min.age",
								                         label = "Min Age (BP)",
											                    value = 4600000000,
											                    min = -100,
													                       max = 4600000000),
						         numericInput(inputId = "max.age",
								                         label = "Max Age (BP)",
											                    value = -100,
											                    min = -100,
													                       max = 4600000000),




						         # numericInput(inputId = "age.min",
						         #              label = "age.min:",
						         #              value = NULL),
						         #
						         # numericInput(inputId = "age.max",
						         #              label = "age.max:",
						         #              value = NULL),


						         # Input: Numeric entry for number of obs to view ----
						         numericInput(inputId = "obs",
								                         label = "Number of observations to view:",
											                    value = 10),

						         # Button
						         downloadButton("ZippedLipdData", "Download data (Zip of .lpd files)"),
							       downloadButton("SummaryCSV", "Download summary table (.csv)")


							     ),

				      # Main panel for displaying outputs ----
				      mainPanel(

						      fluidRow(
							               column(4,
									                     p("Unique time series in query: ", textOutput(outputId = "numTimeSeries", inline=T)),
											                    p("Unique sites in query: ", textOutput(outputId = "numDatasets", inline=T)),),
							               column(4,
									                     actionButton("resetPlot", "Reset plot")),
							               column(4,
									                     selectInput("pointColor", "Select variable for coloring points",
													                            choices =c(names(queryTable)),
																                               selected = "archiveType")),
							               ),

						      withSpinner(plotOutput("plot",
									                      brush = brushOpts(
														                   id = "plotBrush",
																                      delay = 5000
																                      )
											                       )),


						      #c(verbatimTextOutput("summaryHeaders")),


						      DT::dataTableOutput("summary2")

						            #verbatimTextOutput("numTimeSeries")




						          )
				        )
		  )


app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = FALSE)
