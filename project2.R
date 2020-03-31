#Read in the data and clean it up
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)


at <- read.csv("atlantic_storms.csv")
pt <- read.csv("pacific_storms.csv")

years <- c(2005:2018)
years <- append(years, "All")
at$years <- year(at$date)
atShort <- at[at$years > 2005,]
ID <- as.character(atShort$id)
ID <- append(ID, "All")
pt$years <- year(pt$date)
years1 <-pt$years
years1 <- append(years1, "All")
ID1 <- as.character(pt$id)
ID1 <- append(ID, "All")

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2020 Project 2"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
    
    selectInput("Year", "Select the year to visualize", years, selected = 2018),
    selectInput("ID", "Select Hurricane ID to visualize", ID, selected = "All"),
    selectInput("Year1", "Select the year to visualize", years1, selected = 2018),
    selectInput("ID1", "Select Hurricane ID to visualize", ID1, selected = "All"),
    menuItem("Atlantic", tabName = "Atlantic", icon = icon("dashboard")),
    menuItem("Pacific", tabName = "Pacific", icon = icon("dashboard")),
    menuItem("About", tabName = "About")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Atlantic",
              fluidRow(
                box(title = "Leaflet Map Atlantic", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("leaf", height = 800)
                )
              ),
              fluidRow(
                box(title = "Atlantic Hurricane by year", solidHeader = TRUE, status = "primary", width = 13,
                    dataTableOutput("tab1", height = 400)
                )
              ),
              fluidRow(
                box(title = "", solidHeader = TRUE, status = "primary", width = 6,
                    plotOutput("", height = 400)
                ),
                box(title = "", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("", height = 400)
                )
              )
      ),
      tabItem(tabName = "Pacific",
              fluidRow(
                box(title = "Leaflet Map Pacific and Atlantic", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("leaf1", height = 800)
                )
              ),
              fluidRow(
                
                box(title = "", solidHeader = TRUE, status = "primary", width = 13,
                    dataTableOutput("", height = 400)
                )
              ),
              fluidRow(
                box(title = "Pacific Hurrican by year and Atlantic", solidHeader = TRUE, status = "primary", width = 6,
                    plotOutput("tab0", height = 400)
                ),
                box(title = "", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("", height = 400)
                )
                
              )
              
              
      ),
      tabItem(tabName = "About",
              h2("This Project was created by Usman Siddiqui and the data is from littereli.")
      ))
    
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  
  
  output$hist0 <- renderPlot({
    
    
    
  })
  
  
  # show all of the temperatures for a given room for a given year
  output$hist1 <- renderPlot({
    
    
  })
  
  
  # show a line graph of the temperatures at noon for a given room for a given year
  output$hist2 <- renderPlot({
    
  })
  
  
  
  
  # show box plot of the temperatures at noon for a given room for a given year
  output$hist3 <- renderPlot({
    
  })
  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$tab0 <- DT::renderDataTable(
    DT::datatable({ 
      if( input$Year == "All" && input$ID == "All"){
        pt$years
      }
      else if( input$Year != "All" && input$ID == "All"){
        ptYear <- pt[pt$years == input$Year,]
      }
      else if( input$Year == "All" && input$ID != "All"){
        ptID <- pt[pt$id == input$ID,]
        ptID <- ptID[ptID$years > 2005,]
      }
      else{
        ptYear <- pt[pt$years == input$Year,]
        ptID <- ptYear[ptYear$id == input$ID,]
      }
      })
  )
  output$tab1 <- DT::renderDataTable(
      DT::datatable({ 
        if( input$Year == "All" && input$ID == "All"){
          at[at$years > 2005,]
        }
        else if( input$Year != "All" && input$ID == "All"){
          atYear <- at[at$years == input$Year,]
        }
        else if( input$Year == "All" && input$ID != "All"){
          atID <- at[at$id == input$ID,]
          atID <- atID[atID$years > 2005,]
        }
        else{
          atYear <- at[at$years == input$Year,]
          atID <- atYear[atYear$id == input$ID,]
        }
    })
  )
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
    })
  )
  
  output$leaf <- renderLeaflet({
    if( input$Year == "All" && input$ID == "All")  {
      map <- leaflet( data = at[at$years > 2005,])
      Tags <- at$name
    }
    
    else if(input$ID != "All" && input$Year == "All"){
      IDMap <- at[at$id == input$ID,]
      map <- leaflet( data = IDMap[IDMap$years > 2005,])
      Tags <- IDMap$name
    }
    else if(input$ID == "All" && input$Year != "All"){
      
      dataUser <- at[at$years == input$Year,]
      map <- leaflet( data = dataUser)
      Tags <- dataUser$name
    }
    else{
      dataUser <- at[at$years == input$Year,]
      BothInput <- dataUser[dataUser$id == input$ID,]
      map <-leaflet(data = BothInput)
      Tags <- BothInput$name
    }
    map <- addTiles(map)
    map <- setView(map, lng = -10.047998, lat = 15.870, zoom = 3)
    map <- addMarkers(map, lng = ~longitude, lat = ~latitude, popup = Tags,
                      clusterOptions = markerClusterOptions())
    map
  })  
  output$leaf1 <- renderLeaflet({
    if( input$Year1 == "All" && input$ID1 == "All" && input$Year == "All" && input$ID == "All")  {
      Ys <- append(pt$years, at$years[at$years > 2005,])
      map <- leaflet( data = Ys)
      Tags <- pt$name
      Tags <- append (Tags, at$name)
    }
    
    else if(input$ID1 != "All" && input$Year1 == "All"){
      IDMap <- pt[pt$id == input$ID1,]
      map <- leaflet( data = IDMap$years)
      Tags <- IDMap$name
    }
    else if(input$ID1 == "All" && input$Year1 != "All"){
      
      dataUser <- pt[pt$years == input$Year1,]
      map <- leaflet( data = dataUser)
      Tags <- dataUser$name
    }
    else{
      dataUser <- pt[at$years == input$Year1,]
      BothInput <- dataUser[dataUser$id == input$ID1,]
      map <-leaflet(data = BothInput)
      Tags <- BothInput$name
    }
    map <- addTiles(map)
    map <- setView(map, lng = -10.047998, lat = 15.870, zoom = 3)
    map <- addMarkers(map, lng = ~longitude, lat = ~latitude, popup = Tags,
                      clusterOptions = markerClusterOptions())
    map
  })
}

#shinyApp(ui = ui, server = server)

