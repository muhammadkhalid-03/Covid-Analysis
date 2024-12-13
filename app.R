#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(cowplot)
library(sf)
library(tigris)
library(shiny)
library(DT)
library(plotly)
# setwd("/Users/muhammadkhalid/Desktop/STA230/project/midterm")

df = read_excel("./covid.xlsx")
counties <- counties(cb = TRUE, year = 2020, class = "sf")
# Categorize data by regions
my_data_new <- df %>%
  mutate(region = case_when(
    state == "Alabama" ~ "South",
    state == "Alaska" ~ "West",
    state == "Arizona" ~ "West",
    state == "Arkansas" ~ "South",
    state == "California" ~"West",
    state == "Colorado" ~ "West",
    state == "Connecticut" ~ "Northeast",
    state == "Delaware" ~ "Northeast",
    state == "District of Columbia" ~ "South",
    state == "Florida" ~ "South",
    state == "Georgia" ~ "South",
    state == "Hawaii" ~ "West",
    state == "Idaho" ~ "West",
    state == "Illinois" ~ "Midwest",
    state == "Indiana" ~ "Midwest",
    state == "Iowa" ~ "Midwest",
    state == "Kansas" ~ "Midwest",
    state == "Kentucky" ~ "South",
    state == "Louisiana" ~ "South",
    state == "Maine" ~ "Northeast",
    state == "Maryland" ~ "Northeast",
    state == "Massachusetts" ~ "Northeast",
    state == "Michigan" ~ "Midwest",
    state == "Minnesota" ~ "Midwest",
    state == "Mississippi" ~ "South",
    state == "Missouri" ~ "Midwest",
    state == "Montana" ~ "West",
    state == "Nebraska" ~ "Midwest",
    state == "Nevada" ~ "West",
    state == "New Hampshire" ~ "Northeast",
    state == "New Jersey" ~ "Northeast",
    state == "New Mexico" ~ "West",
    state == "New York" ~ "Northeast",
    state == "North Carolina" ~ "South",
    state == "North Dakota" ~ "Midwest",
    state == "Ohio" ~ "Midwest",
    state == "Oklahoma" ~ "South",
    state == "Oregon" ~ "West",
    state == "Pennsylvania" ~ "Northeast",
    state == "Rhode Island" ~ "Northeast",
    state == "South Carolina" ~ "South",
    state == "South Dakota" ~ "Midwest",
    state == "Tennessee" ~ "South",
    state == "Texas" ~ "South",
    state == "Utah" ~ "West",
    state == "Vermont" ~ "Northeast",
    state == "Virginia" ~ "South",
    state == "Washington" ~ "West",
    state == "West Virginia" ~ "South",
    state == "Wisconsin" ~ "Midwest",
    state == "Wyoming" ~ "West",
    TRUE ~ "Other"
  ))

# we cleaned the numeric data to 2 d.p. for scatter plot
cols = c("fips", "state", "county")
for (col in colnames(df)) {
  if (!(col %in% cols)) {
    my_data_new[,col] <- as.numeric(unlist(df[,col])) #unlisting b/c we can't cast from double to list apparently?
    my_data_new[,col] <- round(my_data_new[,col], digits=2) # rounding to 2 digits
  }
}

# now drop "Other" due to null values & convert to log scale for ease of viewing
my_data_new <- my_data_new %>%
  filter(region != "Other") %>%
  mutate(logcases_per_100k = log(cases_per_100k))
my_data_new$fips <- as.character(my_data_new$fips)
counties$GEOID <- as.character(counties$GEOID)
# we're creating a spatial dataset for the map
# we use geographic boundaries for each county from "counties" b/c this # has important information on how to draw the map
# we then get all our covid related variables from my_data_new
# geom_sf is used for creating the actual map

#clean the string so that any data that has 0 as 1st character in string ID is removed
counties$GEOID <- sub("^0", "", counties$GEOID)

# join by common ID's
map_data <- counties %>%
  left_join(my_data_new, by = c("GEOID" = "fips"))
# map_data <- sf::st_cast(map_data, "MULTIPOLYGON")
# Join data and ensure consistent geometry type
map_data <- counties %>%
  left_join(my_data_new, by = c("GEOID" = "fips")) %>%
  st_transform(4326) %>%  # Transform to WGS84
  st_cast("MULTIPOLYGON")  # Convert all geometries to MULTIPOLYGON

#group COVID CASES by state and find mean of log cases
state_data <- map_data %>%
  group_by(state) %>%
  summarize(logcases_per_100k = mean(logcases_per_100k, na.rm = TRUE)) %>%
  st_cast("MULTIPOLYGON")

# group POLITICAL AFFILIATION by state and get the mean dem_to_rep ratio for entire state
affiliation_state_data <- map_data %>%
  group_by(state) %>%
  summarize(aff = mean(dem_to_rep, na.rm = TRUE)) %>%
  st_cast("MULTIPOLYGON")
# imputing any NA values
affiliation_state_data$aff[is.na(affiliation_state_data$aff)] <- mean(affiliation_state_data$aff, na.rm = TRUE)
affiliation_data <- map_data %>%
  mutate(political_affiliation = case_when( # clean to divide into affiliation
    dem_to_rep > 1 ~ "Democratic",
    dem_to_rep < 1 ~ "Republican",
    TRUE ~ "Even"  # for exactly 1, though this might be rare
  ))

#mutate to change to democratic & republican
affiliation_state_data <- affiliation_state_data %>%
  mutate(political_affiliation = case_when( # clean to divide into affiliation
    aff > 1 ~ "Democratic",
    aff < 1 ~ "Republican",
    TRUE ~ "Even"  # for exactly 1, though this might be rare
  ))

## Set up the UI object
ui <- navbarPage("Covid-19 Analysis",
                 tabPanel("Graphs",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          radioButtons(inputId = "region", label = "Select Region:",
                                                       choices = c("Midwest", "Northeast", "South", "West", "All")),
                                          selectInput(inputId = "group", label = "Select Group:",
                                                      choices = c("Poverty Rate" = "poverty_rt",
                                                                  "Elderly Percentage" = "elder_pct",
                                                                  "Non-white Percentage" = "nonwhite_pct"))),
                                        mainPanel(
                                          plotlyOutput(outputId = 'scatterplot')
                                        )
                          )
                 ),
                 tabPanel("Map",
                          # First map section
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          radioButtons(inputId = "geo_level", label = "Case Geography Level:",
                                                       choices = c("County" = "county", "State" = "state")
                                          )
                                        ),
                                        mainPanel(
                                          plotlyOutput(outputId = 'map')
                                        )
                          ),
                          # Second map section
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          radioButtons(inputId = "geo_affiliation", label = "Affiliation Geography Level:",
                                                       choices = c("County" = "county", "State" = "state")
                                          )
                                        ),
                                        mainPanel(
                                          plotlyOutput(outputId = 'pol_map')
                                        )
                          ))
)

## set up the server function
server <- function(input, output){
  output$scatterplot <- renderPlotly({
    ## use aes_string, note that "displ" must now be in quotes
    region_filtered <-
      if (input$region == "All") my_data_new else
        my_data_new[my_data_new$region == input$region, ]
    
    p <- ggplot(region_filtered, aes_string(x = input$group, y = "logcases_per_100k", color = "region")) +
      geom_point(size = 1, alpha = .9, shape = 1) +
      labs(x = case_when(
        input$group == "poverty_rt" ~ "Poverty Rate",
        input$group == "elder_pct" ~ "Elderly Percentage",
        input$group == "nonwhite_pct" ~ "Non-white Percentage",
        TRUE ~ "College Pct"
      ),
      y = "log cases per 100k", color = "Region") +
      theme_classic() +
      theme(
        axis.text.x = element_text(color = "black",size = 15),
        axis.text.y = element_text(color = "black", size = 15, hjust = 1),
        strip.text = element_text(size = 20)
      )
    ggplotly(p)
  })
  output$map <- renderPlotly({
    if (input$geo_level == "county") {
      p <- ggplot(data = map_data) +
        geom_sf(aes(fill = logcases_per_100k), color = "white", size = 0.1) + 
        # creating a color scale with continuous change
        # scale_fill_viridis_c(option = "plasma", na.value = "grey50", name = "Cases per 100k") +
        scale_fill_gradient(
          low = "#e5f5e0",  # Light green
          high = "#004600",  # Dark green
          na.value = "grey50",
          name = "Log Cases per 100k"
        ) +
        labs(title = "COVID-19 Cases per 100,000 People by County") +
        theme_minimal() +
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1, "cm")
        )
      ggplotly(p)
    } else {
      p <- ggplot(data = state_data) +
        geom_sf(aes(fill = logcases_per_100k), color = "white", size = 0.1) + 
        # creating a color scale with continuous change
        # scale_fill_viridis_c(option = "plasma", na.value = "grey50", name = "Cases per 100k") +
        scale_fill_gradient(
          low = "#e5f5e0",  # Light green
          high = "#004600",  # Dark green
          na.value = "grey50",
          name = "Log Cases per 100k"
        ) +
        labs(title = "COVID-19 Cases per 100,000 People by State") +
        theme_minimal() +
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1, "cm")
        )
      ggplotly(p)
    }
  })
  output$pol_map <- renderPlotly({
    if (input$geo_affiliation == "county") {
      p2 <- ggplot(data = affiliation_data) +
        geom_sf(aes(fill = political_affiliation), color = "white", size = 0.1) +
        scale_fill_manual(
          values = c(
            "Democratic" = "blue",
            "Republican" = "red",
            "Even" = "purple"
          ),
          name = "Political Affiliation"
        ) +
        labs(title = "Political Affiliation by County") +
        theme_minimal() +
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        theme( #adjust graph items
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1, "cm")
        )
      ggplotly(p2)
    } else {
      p2 <- ggplot(data = affiliation_state_data) +
        geom_sf(aes(fill = political_affiliation), color = "white", size = 0.1) +
        scale_fill_manual(
          values = c(
            "Democratic" = "blue",
            "Republican" = "red",
            "Even" = "purple"
          ),
          name = "Political Affiliation"
        ) +
        labs(title = "Political Affiliation by State") +
        theme_minimal() +
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        theme( #adjust graph items
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1, "cm")
        )
      ggplotly(p2)
    }
  })
}
## Build and run the
shinyApp(ui, server)