usePackage <- function(p) 
{
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE, repos = "https://cran.itam.mx/")
    require(p, character.only = TRUE)
}

# usePackage('tidyverse')
# usePackage('shiny')
# usePackage('shiny.semantic')
# usePackage('semantic.dashboard')
# usePackage('leaflet')
# usePackage('gmt')
# usePackage('sp')

library('tidyverse')
library('shiny')
library('shiny.semantic')
library('semantic.dashboard')
library('leaflet')
library('gmt')
library('sp')

ships <- read_csv('ships.csv', col_types = 
                      cols(
                          .default = col_double(),
                          DESTINATION = col_character(),
                          FLAG = col_character(),
                          SHIPNAME = col_character(),
                          DATETIME = col_datetime(format = ""),
                          PORT = col_character(),
                          date = col_date(format = ""),
                          ship_type = col_character(),
                          port = col_character()
                      ))

ship_types <- unique(ships$ship_type)

ship_names_by_type <- ships %>% group_by(ship_type, SHIPNAME) %>% 
    summarise(count=n(), .groups="drop")

ship_names <- ship_names_by_type %>% 
    filter(ship_type == "Cargo") %>%
    pull(SHIPNAME)


shipNamesDropDown <- function(id, label = "ship_names") {
    ns <- NS(id)
    tabItems(
        selected = 1,
        tabItem(
            tabName = "main",
            fluidRow(
                column(
                    div(    
                        p(strong("Ship Type")),
                        dropdown_input(ns("ship_type_dropdown"), ship_types, type = "search selection", value = "Cargo"),
                        br(),
                        br(),
                        p(strong("Ship Name")),
                        dropdown_input(ns("ship_name_dropdown"), ship_names, type = "search selection", value = ". PRINCE OF WAVES"),
                        br(),
                        br(),
                        # title = "Ship type", width = 4, color = "blue"),
                        width = 4),
                    box(flow_layout(      
                        p(strong("Largest sail:"),.noWS="after"),
                        textOutput(ns("largest_sail"))),
                        br(),
                        p(strong("Starting point"), style="color:Navy;",.noWS="after"),
                        p(strong("Ending point"), style="color:Red;",.noWS="after"),
                        br()
                        , title = "Note", width = 6, color = "blue"), width = 6
                ),
                box(leafletOutput(ns("map")), title = "Ship's largest sail", width = 10, color = "blue")
            )
        ),
        tabItem(
            tabName = "extra",
            box(plotOutput(ns("boxplot"))
                ,title = "Ship's sensor records", width = 16, color = "blue"
            )
        )
    )
}

shipNamesServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ship_type <- reactiveVal("Cargo")
            ship_name <- reactiveVal(". PRINCE OF WAVES")
            largest_sail_value <- reactiveVal(0.0)
            observeEvent(input$ship_type_dropdown, {
                ship_type(input$ship_type_dropdown)
                names <- ship_names_by_type %>% 
                    filter(ship_type == ship_type()) %>%
                    pull(SHIPNAME)
                
                update_dropdown_input(session, "ship_name_dropdown", choices = names) #, value = input$simple_dropdown
            })

            observeEvent(input$ship_name_dropdown, {
                 ship_name(input$ship_name_dropdown)
            })
            
            output$largest_sail <- renderText({
                paste0(largest_sail_value(), " mts")
            })
            
            output$boxplot <- renderPlot({
                tmp <- ships %>% 
                    filter(SHIPNAME==ship_name() & is_parked==0) %>%
                    arrange(desc(DATETIME)) %>%
                    mutate(LAT_PREV = lag(LAT),
                           LON_PREV = lag(LON),
                           dist = suppressWarnings(ifelse(LAT-LAT_PREV+LON-LON_PREV==0,0,
                                                          geodist(LAT, LON, LAT_PREV, LON_PREV, units="nm"))*1000)) %>% 
                    arrange(desc(dist))
                if (dim(tmp)[1]>1) {
                    boxplot(tmp$dist, horizontal=TRUE, pch=16, outcol="red", xlab="Distance traveled between readings (non parked)")
                }
            })
            
            output$map <- renderLeaflet({
                temp <- ships %>% 
                    filter(SHIPNAME==ship_name()) %>%
                    arrange(desc(DATETIME)) %>%
                    mutate(LAT_PREV = lag(LAT),
                           LON_PREV = lag(LON),
                           dist = suppressWarnings(ifelse(LAT-LAT_PREV+LON-LON_PREV==0,0,
                                         geodist(LAT, LON, LAT_PREV, LON_PREV, units="nm"))*1000)) %>% 
                    arrange(desc(dist))
                
                largest_sail_value(round(temp[1,]$dist,2))
                
                if (temp[1,]$dist<1000) {
                    zoom = 13
                } else if (temp[1,]$dist<2500) {
                    zoom = 12
                } else if (temp[1,]$dist<5000) {
                    zoom = 11
                } else if (temp[1,]$dist<10000) {
                    zoom = 10
                } else if (temp[1,]$dist<20000) {
                    zoom = 9
                } else if (temp[1,]$dist<30000) {
                    zoom = 8
                } else if (temp[1,]$dist<50000) {
                    zoom = 5
                } else  {
                    zoom = 3
                }
                
                      
                pal <- colorFactor(c("navy", "red"), domain = c("start", "end"))
                
                leaflet() %>%
                    addTiles() %>%  # Add default OpenStreetMap map tiles
                    setView(lng=temp[1,]$LON , lat=temp[1,]$LAT, zoom = zoom) %>% 
                    addCircleMarkers(
                        lng=temp[1,]$LON , 
                        lat=temp[1,]$LAT,
                        radius = 4,
                        color = pal("start"),
                        stroke = FALSE, fillOpacity = 0.5
                    ) %>% 
                    addCircleMarkers(
                        lng=temp[1,]$LON_PREV, 
                        lat=temp[1,]$LAT_PREV,
                        radius = 4,
                        color = pal("end"),
                        stroke = FALSE, fillOpacity = 0.5
                    ) 
                # largest_sail_value(temp[1,]$dist)
            })
            ship_name
        }
    )
}


ui <- dashboardPage(
    dashboardHeader(color = "blue", title = "Shiny Developer Test", inverted = TRUE),
    dashboardSidebar(
        size = "thin", color = "teal",
        sidebarMenu(
            menuItem(tabName = "main", "Main"),
            menuItem(tabName = "extra", "Extra")
        )
    ),
    dashboardBody(
        shipNamesDropDown("ship_name")
    )
)

server <- function(input, output, session) {
    shipNamesServer("ship_name")
}

shinyApp(ui, server)