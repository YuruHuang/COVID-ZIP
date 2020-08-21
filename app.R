library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(DBI)
library(dplyr)
library(RSQLite)
library(plotly)
library(broom)

con <- dbConnect(RSQLite::SQLite(), "covid_zip_0808.db")
covid <- dbReadTable(con, "covid_addState")
covid$collection_date<-as.Date(as.numeric(covid$collection_date),origin="1970-01-01",tz='UTC')
covid<-covid[!is.na(covid$Cases),]
covid<-covid[!(covid$State==''|is.na(covid$State)),]

zipcodes = unique(covid$ZIP.CODE)
states = sort(unique(covid$State))[-4]

covid_map = readRDS('./cases_formap_0809.rds')

ui <- dashboardPage(
    dashboardHeader(title="Covid data by zip code"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Temporal Trend", icon = icon("bar-chart-o"), tabName = "trend"),
            menuItem("Map", tabName = "map", icon = icon("dashboard")),
            menuItem('Documentation',tabName='Documentation',icon=icon('file-alt'),badgeLabel = "new", badgeColor = "green"),
            selectizeInput(inputId ='state', label='Please select a state',choices = as.list(states),selected='Maryland'),
            selectizeInput(inputId ='zip_code', label='Please select a zip code:',choices = as.list(zipcodes),selected='20878')
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'trend',
                    box(title = "Trend Graph for the Selected Zip Code", width=7, solidHeader=TRUE,status = "primary", plotlyOutput("trendPlot", height = 500), tags$hr(),"Locally weighted smoothing (LOESS) is used to create the trend line."),
                    box(title='Case Number by Collection Date', width=5, solidHeader=TRUE,status='warning', dataTableOutput("trendTable"))
            ),
            tabItem(tabName='map',
                    leafletOutput("mymap")),
            tabItem(tabName='Documentation',
                    #tags$strong('The purpose of this project is to collect covid19 case numbers by zip code in the United States.'),
                    tags$hr(),
                    box(title='Data Source',width=12,solidHeader=TRUE, collapsible=TRUE, collapsed = FALSE,
                        "Data were obtained from official government websites and actively maintained GitHub repositorie. Below we listed
                        all our data sources:"),
                    box(title='Collection Method',width=12, solidHeader=TRUE,collapsible=TRUE, collapsed = FALSE,
                        "This collection process is automated using python packages including scrapy, selenium, BeautifulSoup, and requests. Specifically, 
                        for websites with ArcGIS map layer, we used ArcGIS query services to query the feature layer; for websites with CSV data files to
                        download, we automated the download process from the websites; for static website tables, we leveraged scrapy or beautifulsoup 
                        packages to harvest the web content; for websites with PDF files, we first downloaded the PDF files and utilized OCR technology
                        to convert the data into the CSV format. 
"),
                    box(title='Disclaimer',width=12,solidHeader=TRUE,collapsible=TRUE, collapsed = FALSE,
      "For zip codes with suppressed data, we assume each zip code has 5 cases. Median value is used for range values such as '1-5 cases'. 
")
                    )
            ))
    )

server <- function(input, output,session) { 
    zipcode_table <- reactive({
        req(input$zip_code)
        covid[covid$ZIP.CODE==input$zip_code,]
    })
    
    output$trendPlot <- renderPlotly({
      ll.smooth = loess(Cases~as.numeric(collection_date),data=zipcode_table(), span=0.75)
      ll.pred = predict(ll.smooth, se = TRUE)
      ll.df = data.frame(x=ll.smooth$x, fit=ll.pred$fit,
                         lb = ll.pred$fit - (1.96 * ll.pred$se),
                         ub = ll.pred$fit + (1.96 * ll.pred$se))
      ll.df = ll.df[order(ll.df$as.numeric.collection_date.),]
      
      plot_ly(data=zipcode_table(),name='cumulative case count',x=~collection_date) %>% add_markers(y= ~Cases,showlegend = FALSE) %>% add_lines(x=~collection_date, y=ll.pred$fit, line=list(color="#366092", width=2)) %>%
        add_ribbons(ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI", line=list(opacity=0.2, width=0, color="#366092")) %>%
        layout(xaxis = list(title = "Collection Date"),
               yaxis = list(side = 'left', title = 'Case Number', showgrid = FALSE, zeroline = FALSE))
    })
    
    output$trendTable<-renderDataTable({zipcode_table()})
    
    output$mymap <- renderLeaflet({
      # create color palette 
      pal <- colorQuantile(
        palette = "Reds", n = 7,
        domain = covid_map@data$Cases)
      
      # create labels for zipcodes
      labels <- 
        paste0(
          "Zip Code: ",
          covid_map@data$GEOID10, "<br/>",
          "Cases: ",
          covid_map@data$Cases) %>%
        lapply(htmltools::HTML)
      
      leaflet(covid_map) %>% 
        # add base map
        addProviderTiles("OpenStreetMap") %>% 
        addPolygons(fillColor = ~pal(Cases),
                    stroke=FALSE, 
                    fillOpacity = 1,
                    smoothFactor = 0.2,
                    label = labels) %>%
        addLegend(pal = pal, 
                  values = ~Cases, 
                  opacity = 0.7, 
                  title = htmltools::HTML("Cases by ZIPCODE, Quantiles"),
                  position = "bottomright")
    })
    observe({
      choices_zip<- as.list(unique(covid[covid$State==input$state,'ZIP.CODE']))
      # Can also set the label and select items
      updateSelectInput(session, "zip_code",
                        label = paste("Select a zip code in ", input$State),
                        choices = choices_zip,
                        selected = head(choices_zip, 1)
      )
    })
}


shinyApp(ui, server)