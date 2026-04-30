library(shiny)
library(leaflet)
library(dplyr)
library(sf)


# LOAD DATA
df <- read.csv("master_data.csv")

# Clean state names
df$STATEFIP <- trimws(df$STATEFIP)
df$STATEFIP <- tools::toTitleCase(tolower(df$STATEFIP))

# Select relevant columns
wage_by_citizenship <- df %>%
  select(AGE, STATEFIP, EDUCD, DEGFIELD, INCWAGE, CITIZEN)

wage_by_citizenship$INCWAGE <- as.numeric(wage_by_citizenship$INCWAGE)

# Citizenship grouping
citizen_col <- c("Naturalized citizen",
                 "Born abroad of American parents",
                 "N/A")

wage_by_citizenship <- wage_by_citizenship %>%
  filter(!is.na(INCWAGE), INCWAGE > 0) %>%
  mutate(CITIZEN_GROUP = case_when(
    CITIZEN %in% citizen_col ~ "Citizen",
    CITIZEN == "Not a citizen" ~ "Non-citizen",
    TRUE ~ NA_character_
  ))

# Summary table
df_summary <- wage_by_citizenship %>%
  filter(!is.na(STATEFIP),
         !is.na(DEGFIELD),
         !is.na(CITIZEN_GROUP)) %>%
  group_by(STATEFIP, DEGFIELD, CITIZEN_GROUP) %>%
  summarize(
    MEDIAN_WAGE = median(INCWAGE, na.rm = TRUE),
    .groups = "drop"
  )


# LOAD PRESAVED STATES

states_sf <- readRDS("states_sf.rds")


# SHIFT ALASKA & HAWAII
states_shifted <- states_sf

# Alaska
alaska_idx <- which(states_shifted$NAME == "Alaska")
if (length(alaska_idx) > 0) {
  states_shifted$geometry[alaska_idx] <-
    (st_geometry(states_shifted[alaska_idx, ]) * 0.5) + c(-60, -5)
}

# Hawaii
hawaii_idx <- which(states_shifted$NAME == "Hawaii")
if (length(hawaii_idx) > 0) {
  states_shifted$geometry[hawaii_idx] <-
    st_geometry(states_shifted[hawaii_idx, ]) + c(52, 5)
}

states_shifted <- st_set_crs(states_shifted, 4326)


# UI
ui <- fluidPage(
  titlePanel("Median Wage by Major"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "major",
        "Select Major:",
        choices = sort(unique(df_summary$DEGFIELD))
      ),
      radioButtons(
        "citizen",
        "Citizenship:",
        choices = c("Citizen", "Non-citizen")
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  filtered <- reactive({
    df_summary %>%
      filter(
        DEGFIELD == input$major,
        CITIZEN_GROUP == input$citizen
      )
  })
  
  output$map <- renderLeaflet({
    
    map_data <- states_shifted %>%
      left_join(filtered(), by = c("NAME" = "STATEFIP"))
    
    # Prevent crash if no data
    validate(
      need(any(!is.na(map_data$MEDIAN_WAGE)), "No data available")
    )
    
    pal <- colorNumeric(
      "YlOrRd",
      domain = map_data$MEDIAN_WAGE,
      na.color = "#ccc"
    )
    
    leaflet(map_data, options = leafletOptions(
      zoomControl = FALSE,
      dragging = FALSE,
      minZoom = 4,
      maxZoom = 4
    )) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(MEDIAN_WAGE),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        label = ~paste0(
          NAME,
          "<br>Median Wage: $",
          round(MEDIAN_WAGE, 0)
        ) %>% lapply(htmltools::HTML)
      ) %>%
      addLegend(
        pal = pal,
        values = ~MEDIAN_WAGE,
        title = "Median Wage",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)