library(htmltools)
library(htmlwidgets)
library(mapview)
library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)
library(sp)
library(maps)
library(jsonlite)
library(leafpop)
library(RColorBrewer)
library(pals)
library(rsconnect)
library(bslib)

#read in data from .csv file
df <- read_csv("code/smash_info.csv")

#jitter each marker's location to prevent markers from stacking
df$Lat <- jitter(df$Lat, factor = 3)
df$Long <- jitter(df$Long, factor = 3)

#marker size is determined by how many markers are at a specific location
df2 <- df %>% group_by(`Origin:`) %>% summarize(n = n())

df <- merge(x = df,
            y = df2,
            by = "Origin:",
            all = TRUE)

#read in polygon data for country polygons(not state polygons)
world <- readOGR("code/ne_50m_country.shp", encoding = 'UTF-8')

#establish which countries/states belong to each region
#this will be used to color/show a marker based on region
north_east <-
  c(
    "Exmore",
    "District of Columbia",
    "Connecticut",
    "Delaware",
    "Illinois",
    "Indiana",
    "Iowa",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "New Hampshire",
    "New Jersey",
    "New York",
    "Ohio",
    "Pennsylvania",
    "Rhode Island",
    "Vermont",
    "Virginia",
    "West Virginia",
    "Wisconsin",
    "Manitoba",
    "New Brunswick",
    "Newfoundland and Labrador",
    "Nova Scotia",
    "Nunavut",
    "Ontario",
    "Prince Edward Island",
    "Quebec"
  )
north_west <-
  c(
    "Alaska",
    "Idaho",
    "Montana",
    "Nebraska",
    "North Dakota",
    "Oregon",
    "South Dakota",
    "Washington",
    "British Columbia",
    "Alberta",
    "Northwest Territories",
    "Saskatchewan",
    "Yukon",
    "Wyoming"
  )
south_east <-
  c(
    "Alabama",
    "Arkansas",
    "Florida",
    "Georgia",
    "Kentucky",
    "Louisiana",
    "Mississippi",
    "Missouri",
    "North Carolina",
    "South Carolina",
    "Tennessee"
  )
south_west <-
  c(
    "Arizona",
    "California",
    "Colorado",
    "Hawaii",
    "Kansas",
    "Nevada",
    "New Mexico",
    "Oklahoma",
    "Texas",
    "Utah"
  )

mexico <- c("Mexico")
oceania <- c("Australia", "New Zealand")
south_america <-
  c(
    "pip",
    "spencer",
    "Aruba",
    "French Guiana",
    "Guyane",
    "Dégrad des Cannes",
    "Cayenne Island",
    "Cayenne",
    "Belem",
    "Leguan",
    "Wakenaam",
    "Pedernales",
    "Argentina",
    "Bolivia",
    "Brazil",
    "Chile",
    "Colombia",
    "Ecuador",
    "French Guiana",
    "Guyana",
    "Paraguay",
    "Peru",
    "Suriname",
    "Uruguay",
    "Venezuela"
  )
japan <- c("Japan")
europe <-
  c(
    "michelle",
    "kevin",
    "jack",
    "Republic of Macedonia",
    "Albania",
    "Andorra",
    "Austria",
    "Belarus",
    "Belgium",
    "Bosnia and Herzegovina",
    "Bulgaria",
    "Croatia",
    "Czech Republic",
    "Denmark",
    "Estonia",
    "Faroe Islands",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Isle of Man",
    "Italy",
    "Kosovo",
    "Latvia",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Moldova",
    "Monaco",
    "Montenegro",
    "Netherlands",
    "North Macedonia",
    "Norway",
    "Poland",
    "Portugal",
    "Romania",
    "San Marino",
    "Serbia",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Ukraine",
    "United Kingdom"
  )
asia <-
  c(
    "Hong Kong",
    "Indonesia",
    "Malaysia",
    "Philippines",
    "Singapore",
    "South Korea",
    "Taiwan",
    "Thailand",
    "Vietnam"
  )
central_america <-
  c(
    "The Bahamas",
    "Costa Rica",
    "Dominican Republic",
    "El Salvador",
    "Guatemala",
    "Honduras",
    "Jamaica",
    "Nicaragua",
    "Panama",
    "Puerto Rico"
  )

#load in polygon data for states
states <-
  geojsonio::geojson_read("code/ne_10m_admin_1_states3.shp", what = "sp")

#categorize state polygons into regions
states$tournament <- factor(ifelse(
  states$name_en %in% north_east,
  "American Northeast",
  ifelse(
    states$name %in% north_west,
    "American Northwest",
    ifelse(
      states$name %in% south_east,
      "American Southeast" ,
      ifelse(states$name %in% south_west, "American Southwest", "Unknown")
    )
  )
),
)

#categorize world polygons into regions
world$tournament <- factor(ifelse(
  world$NAME_EN %in% mexico,
  "Mexico",
  ifelse(
    world$NAME_EN  %in% oceania,
    "Oceania",
    ifelse(
      world$NAME_EN  %in% south_america,
      "South America",
      ifelse(
        world$NAME_EN  %in% japan,
        "Japan",
        ifelse(
          world$NAME_EN  %in% europe,
          "Europe",
          ifelse(
            world$NAME_EN  %in% asia,
            "East Asia",
            ifelse(
              world$NAME_EN  %in% central_america,
              "Central America",
              "Unknown"
            )
          )
        )
      )
    )
  )
))


#give a unique color for each region

values <-  c(states$tournament, world$tournament)
values <- values[!(values %in% "Unknown")]

factpal <- colorFactor(brewer.pal(n = 11, name = 'Paired'), values)


valuesss <- unique(df$Tournament)

#set the theme of the app

theme <- bs_theme(bootswatch = "lumen")

#user interface
ui <-
  #css styles and layout for page
  fillPage(
    theme = theme,
    tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
    tags$style(
      "
        #controls {
           background-color: #f2f2f2;
           border-radius: 3vh;
           cursor: move;
           opacity: 0.85;
           zoom: 0.9;
           transition: opacity 500ms 1s;
        }
        .leaflet-popup-content {
           width: auto;
           height: auto;
           font-size: 1.1vh;
        }
        .selectize-input {
           height: vh;
           width: 7vw;
        }
        
 

               "),
    leafletOutput("mymap", width = "100%", height = "100%"),
    
    #panel that holds all of our controls
    absolutePanel(
      id = "controls",
      top = 5,
      right = 1,
      style = "padding-left: .5vw;",
      width = "21vw",
      height = "auto",
      fixed = TRUE,
      h2("Smash World Tour 2021 Ultimate Online Qualifiers"),
      
      fluidRow(
        column(
          4,
          selectInput("play",
                      "Select Player:",
                      c("All",
                        unique(
                          sort(as.character(df$`Name:`[df$`Name:`!="Unknown"]))
                        ))),
          
          
          selectInput("chrs",
                      "Select Character:",
                      c("All",
                        unique(
                          sort(c(
                            df$char1, df$char2, df$char2, df$char2, df$char2
                          ), na.last = NA)
                        )),)
          ,
          style = "font-size: 1.4vh;"
        ),
        
        column(
          8,
          checkboxGroupInput(
            inputId = "test1",
            label = "Select Region(s):",
            inline = TRUE,
            choices = sort(unique(df$Tournaments))
          ),
          style = "font-size: 1.5vh;"
        )
        
        
      )
    )
    
  )






server <- function(input, output, session) {
  
  #update map with filtered data
  
  filteredData <- reactive({
    rows <- (input$play == "All" | df$`Name:` == input$play) &
      (
        input$chrs == "All" |
          df$char1 == input$chrs |
          df$char2 == input$chrs |
          df$char3 == input$chrs |
          df$char4 == input$chrs | df$char5 == input$chrs
      )
    
    
    
    x <- df[rows,]
    
    if (nrow(x) == 0) {
      updateSelectInput(session, "play",
                        selected = "All")
      
      updateSelectInput(session, "chrs",
                        selected = "All")
      
      df2 <- df[,] %>% group_by(`Origin:`) %>% summarize(n1 = n())
      df3 <- merge(
        x = df,
        y = df2,
        by = "Origin:",
        all = TRUE
      )
      
    } else {
      df2 <- x %>% group_by(`Origin:`) %>% summarize(n1 = n())
      
      
      df3 <- merge(
        x = x,
        y = df2,
        by = "Origin:",
        all = TRUE
      )
      
      
      return(df3)
    }
    
    
  })
  
  
  #Based on checkbox/dropdown decisions update the map
  observe({
    pal <- colorFactor(c("#0061fc", "#00d7fc", "#00fc04","2b752d","#ff1e00",
                         "#000000", "#ff00dd","#d99f00","#916754"),
                       values,
                       levels = unique(df$Tournament))
    
    x <-  leafletProxy("mymap", data = filteredData()) %>%
      clearMarkers()%>%
      addCircleMarkers(
        radius = ~ (n1 + 2) ^ 1.1,
        weight = 1,
        color = "#777777",
        fillColor = ~ pal(Tournament),
        fillOpacity = 0.9,
        label = filteredData()$`Name:`,
        popup = popupTable(
          filteredData(),
          zcol = c(
            "Player:",
            "Origin:",
            "Sponser:",
            "Characters:",
            "Place:",
            "Tournament:"
          ),
          feature.id = FALSE,
          row.numbers = FALSE
        )
      )
      
    
    
    if (length(input$test1) == 0) {
      x %>% hideGroup(
        c(
          "Central America",
          "East Asia",
          "Japan",
          "South America",
          "Oceania",
          "Europe",
          "Mexico",
          "Southwest",
          "Southeast",
          "Northwest",
          "Northeast"
        )
      )
      
    }
    
    if ("Mexico" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Mexico")
      
    } else {
      x <- x %>% showGroup("Mexico") %>%
        addCircleMarkers(
        radius = ~ (n1 + 2) ^ 1.1,
        weight = 1,
        color = "#777777",
        fillColor = ~ pal(Tournament),
        fillOpacity = 0.9,
        label = filteredData()$`Name:`,
        popup = popupTable(
          filteredData(),
          zcol = c(
            "Player:",
            "Origin:",
            "Sponser:",
            "Characters:",
            "Place:",
            "Tournament:"
          ),
          feature.id = FALSE,
          row.numbers = FALSE
        )
      )
      
    }
    
    
    if ("Oceania" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Oceania")
      
    } else {
      x <- x %>% showGroup("Oceania") %>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Central America" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Central America")
      
    } else {
      x <- x %>% showGroup("Central America")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("East Asia" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("East Asia")
      
    } else {
      x <- x %>% showGroup("East Asia")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Japan" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Japan")
      
    } else {
      x <- x %>% showGroup("Japan")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("South America" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("South America")
      
    } else {
      x <- x %>% showGroup("South America")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Europe" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Europe")
      
    } else {
      x <- x %>% showGroup("Europe")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Southwest" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Southwest")
      
    } else {
      x <- x %>% showGroup("Southwest")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Southeast" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Southeast")
      
    } else {
      x <- x %>% showGroup("Southeast")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Northwest" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Northwest")
      
    } else {
      x <- x %>% showGroup("Northwest")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
    
    if ("Northeast" %in% input$test1 == FALSE)
    {
      x <- x %>% hideGroup("Northeast")
      
    } else {
      x <- x %>% showGroup("Northeast")%>%
        addCircleMarkers(
          radius = ~ (n1 + 2) ^ 1.1,
          weight = 1,
          color = "#777777",
          fillColor = ~ pal(Tournament),
          fillOpacity = 0.9,
          label = filteredData()$`Name:`,
          popup = popupTable(
            filteredData(),
            zcol = c(
              "Player:",
              "Origin:",
              "Sponser:",
              "Characters:",
              "Place:",
              "Tournament:"
            ),
            feature.id = FALSE,
            row.numbers = FALSE
          )
        )
      
    }
    
  }
  
)
  
  
  pal <- colorFactor(c("#0061fc", "#00d7fc", "#00fc04","2b752d","#ff1e00",
                       "#000000", "#ff00dd","#d99f00","#916754"),
                     values,
                     levels = unique(df$Tournament))
  
  #creates the map when the application starts
  
  output$mymap <- renderLeaflet({
    leaflet(df) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = subset(states, name_en %in% north_east),
        opacity = 1,
        smoothFactor = 0.1,
        stroke = FALSE,
        weight = 0.7,
        group = "Northeast",
        fillColor  = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(states, name_en %in% north_west),
        opacity = 1,
        stroke = FALSE,
        smoothFactor = 0.1,
        
        weight = 0.7,
        group = "Northwest",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(states, name_en %in% south_east),
        smoothFactor = 0.1,
        
        stroke = FALSE,
        opacity = 1,
        weight = 0.7,
        group = "Southeast",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(states, name_en %in% south_west),
        opacity = 1,
        stroke = FALSE,
        smoothFactor = 0.1,
        
        weight = 0.7,
        group = "Southwest",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% mexico),
        opacity = 1,
        stroke = FALSE,
        weight = 0.7,
        smoothFactor = 0.1,
        
        group = "Mexico",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% europe),
        opacity = 1,
        stroke = FALSE,
        weight = 0.7,
        smoothFactor = 0.1,
        
        group = "Europe",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% oceania),
        opacity = 1,
        stroke = FALSE,
        weight = 0.7,
        smoothFactor = 0.1,
        
        group = "Oceania",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% south_america),
        opacity = 1,
        stroke = FALSE,
        smoothFactor = 0.1,
        
        weight = 0.7,
        group = "South America",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% japan),
        opacity = 1,
        stroke = FALSE,
        smoothFactor = 0.1,
        
        weight = 0.7,
        group = "Japan",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% asia),
        opacity = 1,
        smoothFactor = 0.1,
        
        stroke = FALSE,
        weight = 0.7,
        group = "East Asia",
        fillColor = ~ pal(tournament)
      ) %>%
      addPolygons(
        data = subset(world, NAME_EN %in% central_america),
        opacity = 1,
        stroke = FALSE,
        smoothFactor = 0.1,
        
        weight = 0.7,
        group = "Central America",
        fillColor = ~ pal(tournament)
      )  %>%
      addLegend(
        position = "bottomright",
        title = "Regions",
        pal = pal,
        values = unique(df$Tournament),
        opacity = 0.4
      ) %>%
      hideGroup(
        c(
          "Central America",
          "East Asia",
          "Japan",
          "South America",
          "Oceania",
          "Europe",
          "Mexico",
          "Southwest",
          "Southeast",
          "Northwest",
          "Northeast"
        )
      ) %>%
      fitBounds(~ min(Long), ~ min(Lat), ~ max(Long), ~ max(Lat)) %>%
      addEasyButton(easyButton(
        icon = "fa-globe",
        title = "Overview",
        onClick = JS("function(btn, map){ map.setZoom(3); }")
      ))
    
    
    
  })
  
  
  
}

#start the app

shinyApp(ui = ui, server = server)
