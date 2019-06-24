#mute warnings
options(warn=-1)

prv = data.frame(names(providers))
colnames(prv) = c('Providers')

navbarPage(HTML('<img src="logo.png"/>'), id="nav", collapsible = FALSE,
           tabPanel(HTML('<b>Map</b>'), icon = icon("map-marked-alt"), 
                    div(class="outer",
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "typicons.css")
                    ),
                    
                    leafletOutput('map', width = "100%", height = "100%"),
                    absolutePanel(
                          draggable = TRUE,
                          class= "map-controls", left = 10, fixed = TRUE,
                          div(class="panelbar", HTML('<h4><span class="typcn typcn-image display-icon"></span>Display<span class="minimise"><i class="typcn typcn-arrow-minimise"></i></span></h4>')),
                          div(class="control-elements", 
                              selectInput('sch', 'School', toTitleCase(tolower(sort(schdata@data$school_name))), toTitleCase(tolower(sort(schdata@data$school_name)[[1]]))),
                              hr(),
                              h5(strong('Isochrone'), style="color:white;"),
                              checkboxInput("isochrone", "Show isochrone", TRUE),
                              conditionalPanel(
                                condition = "input.isochrone == true",
                                div(
                                  class = 'inner',
                                  checkboxInput("isochronechart", "Include distribution chart", TRUE)
                                )
                              ),
                              hr(),
                              h5(strong('Hansen Accessibility'), style="color:white;"),
                              checkboxInput("hansenduration", "Show duration accessibility", FALSE),
                              conditionalPanel(
                                condition = "input.hansenduration == true",
                                div(
                                  class = 'inner',
                                  checkboxInput("hansendurationpoints", "Include accessibility points", FALSE)
                                )
                              ),
                              checkboxInput("hansendistance", "Show distance accessibility", FALSE),
                              conditionalPanel(
                                condition = "input.hansendistance == true",
                                div(
                                  class = 'inner',
                                  checkboxInput("hansendistancepoints", "Include accessibility points", FALSE)
                                )
                              ),
                              hr(),
                              h5(strong('Residential'), style="color:white;"),
                              checkboxInput("kernel", "Show kernel density of HDB", FALSE),
                              checkboxInput("residential", "Show HDB points", FALSE),
                              hr(),
                              h5(strong('Misc'), style="color:white;"),
                              checkboxInput("legend", "Show legend", TRUE),
                              selectInput('maptype', 'Map Type', prv, 'CartoDB.DarkMatter')
                              )
                          
                        ),
                    conditionalPanel(
                      condition = "input.isochrone == true && input.isochronechart == true",
                        absolutePanel(class = "graph-output", right = 10, bottom = 10, width = 330, draggable = TRUE,
                                      plotOutput('gg_graph', height = 300, width = 330)
                        )
                      ),
                    tags$body(
                      tags$script(type = "text/javascript", src = "extend.js")
                    )
                    )
                    ),
           tabPanel(HTML('<b>Dataset</b>'), icon = icon("database"),
                    div( class='dataset-page container-fluid',
                    fluidRow(class='row',
                             div(class="col-md-6",
                      div(class="panel panel-info",
                             div(class="panel-heading",
                               h3(class="panel-title", 
                                  span(class='fa fa-map-marker-alt'),
                                  'Location Information')
                             ),
                             div(class="panel-body",
                             div(class="normal-inputs",
                                 
                                 selectInput("selectTable", "Select Table",  
                                             c("Residential HDB information" = 1,
                                               "School information" = 2 ) ,
                                             selected = 1
                                 )
                             ),hr(),
                             div(class='table-outer',
                                 DT::dataTableOutput("table", height = "100%")
                                 )
                             )
                             )
                      ),div(class="col-md-6",
                            div(class="panel panel-info",
                            div(class="panel-heading",
                                h3(class="panel-title", 
                                   span(class='fa fa-braille'),
                                   'Accessibility Information')
                            ),
                            div(class="panel-body",
                                div(class="normal-inputs",
                                    
                                    selectInput("selectTableHansen", "Accessibility to:",  
                                                toTitleCase(tolower(sort(schdata@data$school_name))), 
                                                toTitleCase(tolower(sort(schdata@data$school_name)[[1]]))
                                    )
                                    
                                ),hr(),
                                div(class='table-outer',
                                    DT::dataTableOutput("tablehansen", height = "100%")
                                )
                            )
                        )
                      )
                      
                      )
                    )
           
           )
)

  