# =====================================================
# Triton Coalition – Interactive Demographic Dashboard
# ACS 5-Year (2019–2023)
# =====================================================

library(shiny)
library(shinyWidgets)
library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(leaflet)
library(stringr)
library(plotly)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# -----------------------------
# Census API Key
# -----------------------------
census_key <- Sys.getenv("CENSUS_API_KEY")

if (nzchar(census_key)) {
  tidycensus::census_api_key(census_key, overwrite = TRUE)
} else {
  message("Census API key not set; configure CENSUS_API_KEY.")
}

# -----------------------------
# Triton Color Palette
# -----------------------------
TRITON_NAVY  <- "#102761"
TRITON_GOLD  <- "#CEAA78"
TRITON_LIGHT <- "#E9EEF3"
TRITON_BLUE  <- "#295C9D"
TRITON_GREEN <- "#4D835B"
TRITON_COLORS <- c(TRITON_NAVY, TRITON_BLUE, TRITON_GREEN, TRITON_GOLD)

# -----------------------------
# Parameters
# -----------------------------
ACS_YEAR <- 2023
TOWNS <- c("Burlington", "Lexington", "Wilmington")

# -----------------------------
# ACS VARIABLES
# -----------------------------
acs_vars <- c(
  total_popE = "B01003_001",
  maleE      = "B01001_002",
  femaleE    = "B01001_026",
  
  whiteE       = "B03002_003",
  blackE       = "B03002_004",
  nativeE      = "B03002_005",
  asianE       = "B03002_006",
  islanderE    = "B03002_007",
  otherE       = "B03002_008",
  multiraceE   = "B03002_009",
  hispanicE    = "B03002_012",
  
  med_incomeE = "B19013_001",
  inc_10k = "B19001_002",
  inc_15k = "B19001_003",
  inc_20k = "B19001_004",
  inc_25k = "B19001_005",
  inc_30k = "B19001_006",
  inc_35k = "B19001_007",
  inc_40k = "B19001_008",
  inc_45k = "B19001_009",
  inc_50k = "B19001_010",
  inc_60k = "B19001_011",
  inc_75k = "B19001_012",
  inc_100k = "B19001_013",
  inc_125k = "B19001_014",
  inc_150k = "B19001_015",
  inc_200k = "B19001_016",
  inc_200k_plus = "B19001_017",
  
  # Age male
  age0_4M  = "B01001_003",
  age5_17M = "B01001_004",
  age18_24M = "B01001_006",
  age25_34M = "B01001_007",
  age35_44M = "B01001_009",
  age45_54M = "B01001_010",
  age55_59M = "B01001_012",
  age60_64M = "B01001_013",
  age65M = "B01001_015",
  
  # Age female
  age0_4F  = "B01001_027",
  age5_17F = "B01001_028",
  age18_24F = "B01001_030",
  age25_34F = "B01001_031",
  age35_44F = "B01001_033",
  age45_54F = "B01001_034",
  age55_59F = "B01001_036",
  age60_64F = "B01001_037",
  age65F = "B01001_039"
)

income_bins <- list(
  "< $20K"      = c("inc_10k","inc_15k","inc_20k"),
  "$20K–$39K"   = c("inc_25k","inc_30k","inc_35k","inc_40k"),
  "$40K–$59K"   = c("inc_45k","inc_50k","inc_60k"),
  "$60K–$149K"  = c("inc_75k","inc_100k","inc_125k","inc_150k"),
  "$150K+"      = c("inc_200k","inc_200k_plus")
)

age_labels  <- c("0–17","18–34","35–54","55–64","65+")
race_labels <- c(
  "White"            = "whiteE",
  "Black"            = "blackE",
  "Asian"            = "asianE",
  "Native American"  = "nativeE",
  "Pacific Islander" = "islanderE",
  "Other"            = "otherE",
  "Multiracial"      = "multiraceE",
  "Hispanic"         = "hispanicE"
)

# -----------------------------
# Helper: Get ACS wide (tidy)
# -----------------------------
get_acs_wide <- function(geo) {
  get_acs(
    geography = geo,
    state     = "MA",
    variables = acs_vars,
    year      = ACS_YEAR,
    survey    = "acs5"
  ) |>
    select(GEOID, NAME, variable, estimate) |>
    tidyr::pivot_wider(names_from = variable, values_from = estimate) |>
    mutate(
      across(where(is.numeric), ~replace_na(., 0)),
      town = str_extract(NAME, "^[^,]+") |>
        str_remove(" town$| city$")
    ) |>
    mutate(
      age0_17M  = age0_4M + age5_17M,
      age18_34M = age18_24M + age25_34M,
      age35_54M = age35_44M + age45_54M,
      age55_64M = age55_59M + age60_64M,
      age0_17F  = age0_4F + age5_17F,
      age18_34F = age18_24F + age25_34F,
      age35_54F = age35_44F + age45_54F,
      age55_64F = age55_59F + age60_64F,
      age0_17E  = age0_17M + age0_17F,
      age18_34E = age18_34M + age18_34F,
      age35_54E = age35_54M + age35_54F,
      age55_64E = age55_64M + age55_64F,
      age65E    = age65M + age65F
    )
}

# -----------------------------
# Data: Towns, Region, State
# -----------------------------
town_stats <- get_acs_wide("county subdivision") |>
  filter(town %in% TOWNS)

town_shapes <- county_subdivisions("MA", cb = TRUE, year = ACS_YEAR) |>
  st_transform(4326) |>
  mutate(
    town = str_extract(NAME, "^[^,]+") |>
      str_remove(" town$| city$")
  ) |>
  filter(town %in% TOWNS)

town_data <- left_join(town_shapes, town_stats, by = "town")

region_data <- town_data |>
  st_drop_geometry() |>
  summarise(
    total_popE  = sum(total_popE,  na.rm = TRUE),
    maleE       = sum(maleE,       na.rm = TRUE),
    femaleE     = sum(femaleE,     na.rm = TRUE),
    whiteE      = sum(whiteE,      na.rm = TRUE),
    blackE      = sum(blackE,      na.rm = TRUE),
    nativeE     = sum(nativeE,     na.rm = TRUE),
    asianE      = sum(asianE,      na.rm = TRUE),
    islanderE   = sum(islanderE,   na.rm = TRUE),
    otherE      = sum(otherE,      na.rm = TRUE),
    multiraceE  = sum(multiraceE,  na.rm = TRUE),
    hispanicE   = sum(hispanicE,   na.rm = TRUE),
    age0_17M    = sum(age0_17M,    na.rm = TRUE),
    age0_17F    = sum(age0_17F,    na.rm = TRUE),
    age18_34M   = sum(age18_34M,   na.rm = TRUE),
    age18_34F   = sum(age18_34F,   na.rm = TRUE),
    age35_54M   = sum(age35_54M,   na.rm = TRUE),
    age35_54F   = sum(age35_54F,   na.rm = TRUE),
    age55_64M   = sum(age55_64M,   na.rm = TRUE),
    age55_64F   = sum(age55_64F,   na.rm = TRUE),
    age65M      = sum(age65M,      na.rm = TRUE),
    age65F      = sum(age65F,      na.rm = TRUE),
    across(starts_with("inc_"), ~sum(.x, na.rm = TRUE))
  ) |>
  mutate(
    age65E = age65M + age65F,
    town   = "Region"
  )

ma_data <- get_acs_wide("state") |>
  filter(str_detect(NAME, "Massachusetts")) |>
  mutate(town = "Massachusetts")

ma_shape <- states(cb = TRUE, year = ACS_YEAR) |>
  filter(STUSPS == "MA") |>
  st_transform(4326)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Triton Coalition Dashboard (ACS 5-Year)"),
  tabsetPanel(
    tabPanel("Region vs Massachusetts",
             fluidRow(
               column(
                 6,
                 leafletOutput("map_region", height = 250),
                 radioGroupButtons(
                   "region_type", "View",
                   choices   = c("Race", "Age", "Income"),
                   justified = TRUE
                 ),
                 plotlyOutput("region_plot", height = 350)
               ),
               column(
                 6,
                 leafletOutput("map_ma", height = 250),
                 radioGroupButtons(
                   "ma_region_type", "View",
                   choices   = c("Race", "Age", "Income"),
                   justified = TRUE
                 ),
                 plotlyOutput("ma_plot", height = 350)
               )
             )
    ),
    tabPanel("Town vs Massachusetts",
             selectInput("town", "Select Town", TOWNS),
             fluidRow(
               column(
                 6,
                 leafletOutput("map_town", height = 250),
                 radioGroupButtons(
                   "town_type", "View",
                   choices   = c("Race", "Age", "Income"),
                   justified = TRUE
                 ),
                 plotlyOutput("town_plot", height = 350)
               ),
               column(
                 6,
                 leafletOutput("map_ma2", height = 250),
                 radioGroupButtons(
                   "ma2_type", "View",
                   choices   = c("Race", "Age", "Income"),
                   justified = TRUE
                 ),
                 plotlyOutput("ma2_plot", height = 350)
               )
             )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  safe_ggplotly <- function(p) {
    tryCatch(
      ggplotly(p, tooltip = "text"),
      error = function(e) {
        ggplotly(
          ggplot() +
            annotate("text", 0, 0,
                     label = "Data not available",
                     color = TRITON_NAVY, size = 5) +
            theme_void()
        )
      }
    )
  }
  
  make_plot <- function(df, type) {
    if (is.null(df) || nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", 0, 0,
                   label = "No data",
                   color = TRITON_NAVY, size = 5) +
          theme_void()
      )
    }
    
    row <- df[1, , drop = FALSE]
    
    if (type == "Income") {
      bins <- lapply(names(income_bins), function(bin) {
        cols <- intersect(income_bins[[bin]], names(row))
        Count <- if (length(cols) > 0)
          sum(as.numeric(row[1, cols, drop = TRUE]), na.rm = TRUE)
        else 0
        data.frame(Category = bin, Count = Count)
      })
      df_long <- bind_rows(bins)
      total <- sum(df_long$Count, na.rm = TRUE)
      if (total <= 0) {
        return(
          ggplot() +
            annotate("text", 0, 0,
                     label = "Income data not available",
                     color = TRITON_NAVY, size = 5) +
            theme_void()
        )
      }
      df_long$Percent <- df_long$Count / total
      
      ggplot(df_long, aes(
        x = "",
        y = Percent,
        fill = Category,
        text = paste0(Category, ": ",
                      comma(Count), " (", percent(Percent), ")")
      )) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = TRITON_COLORS) +
        scale_y_continuous(labels = percent_format()) +
        theme_void()
      
    } else if (type == "Age") {
      
      age_df <- tibble::tibble(
        Age = rep(age_labels, each = 3),
        Sex = rep(c("Male","Female","Total"), times = length(age_labels)),
        Count = NA_real_
      )
      
      # 0–17
      age_df$Count[age_df$Age=="0–17" & age_df$Sex=="Male"]   <-
        sum(row$age0_4M, row$age5_17M, na.rm = TRUE)
      age_df$Count[age_df$Age=="0–17" & age_df$Sex=="Female"] <-
        sum(row$age0_4F, row$age5_17F, na.rm = TRUE)
      
      # 18–34
      age_df$Count[age_df$Age=="18–34" & age_df$Sex=="Male"]   <-
        sum(row$age18_24M, row$age25_34M, na.rm = TRUE)
      age_df$Count[age_df$Age=="18–34" & age_df$Sex=="Female"] <-
        sum(row$age18_24F, row$age25_34F, na.rm = TRUE)
      
      # 35–54
      age_df$Count[age_df$Age=="35–54" & age_df$Sex=="Male"]   <-
        sum(row$age35_44M, row$age45_54M, na.rm = TRUE)
      age_df$Count[age_df$Age=="35–54" & age_df$Sex=="Female"] <-
        sum(row$age35_44F, row$age45_54F, na.rm = TRUE)
      
      # 55–64
      age_df$Count[age_df$Age=="55–64" & age_df$Sex=="Male"]   <-
        sum(row$age55_59M, row$age60_64M, na.rm = TRUE)
      age_df$Count[age_df$Age=="55–64" & age_df$Sex=="Female"] <-
        sum(row$age55_59F, row$age60_64F, na.rm = TRUE)
      
      # 65+
      age_df$Count[age_df$Age=="65+" & age_df$Sex=="Male"]   <-
        if ("age65M" %in% names(row)) as.numeric(row$age65M) else 0
      age_df$Count[age_df$Age=="65+" & age_df$Sex=="Female"] <-
        if ("age65F" %in% names(row)) as.numeric(row$age65F) else 0
      
      # Totals per age band
      for (a in age_labels) {
        age_df$Count[age_df$Age==a & age_df$Sex=="Total"] <-
          sum(age_df$Count[age_df$Age==a & age_df$Sex %in% c("Male","Female")],
              na.rm = TRUE)
      }
      
      if (sum(age_df$Count, na.rm = TRUE) <= 0) {
        return(
          ggplot() +
            annotate("text", 0, 0,
                     label = "Age data not available",
                     color = TRITON_NAVY, size = 5) +
            theme_void()
        )
      }
      
      age_df <- age_df |>
        group_by(Age) |>
        mutate(Percent = ifelse(sum(Count, na.rm = TRUE) > 0,
                                Count / sum(Count, na.rm = TRUE), 0)) |>
        ungroup()
      
      ggplot(age_df, aes(
        Age, Percent,
        fill = Sex,
        text = paste0(
          Sex, " (", Age, "): ",
          comma(Count), " (", percent(Percent), ")"
        )
      )) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_manual(values = c("Male" = TRITON_BLUE,
                                     "Female" = TRITON_GOLD,
                                     "Total" = TRITON_GREEN)) +
        labs(y = "Percent of age group") +
        theme_minimal()
      
    } else {  # Race
      
      race_counts <- lapply(names(race_labels), function(r_name) {
        col <- race_labels[[r_name]]
        Count <- if (col %in% names(row)) as.numeric(row[[col]]) else 0
        data.frame(Race = r_name, Count = Count)
      })
      df_long <- bind_rows(race_counts)
      
      total <- sum(df_long$Count, na.rm = TRUE)
      if (total <= 0) {
        return(
          ggplot() +
            annotate("text", 0, 0,
                     label = "Race data not available",
                     color = TRITON_NAVY, size = 5) +
            theme_void()
        )
      }
      
      df_long$Percent <- df_long$Count / total
      df_long$Race <- factor(df_long$Race, levels = names(race_labels))
      
      ggplot(df_long, aes(
        Race, Percent,
        fill = Race,
        text = paste0(
          as.character(Race), ": ",
          comma(Count), " (", percent(Percent), ")"
        )
      )) +
        geom_col() +
        scale_fill_manual(values = TRITON_COLORS) +
        scale_y_continuous(labels = percent_format()) +
        labs(y = "Percent of population") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }
  
  # Region vs Massachusetts plots
  output$region_plot <- renderPlotly({
    safe_ggplotly(make_plot(region_data, input$region_type))
  })
  
  output$ma_plot <- renderPlotly({
    safe_ggplotly(make_plot(ma_data, input$ma_region_type))
  })
  
  # Town vs Massachusetts plots
  output$town_plot <- renderPlotly({
    td <- town_data |>
      filter(town == input$town) |>
      st_drop_geometry()
    safe_ggplotly(make_plot(td, input$town_type))
  })
  
  output$ma2_plot <- renderPlotly({
    safe_ggplotly(make_plot(ma_data, input$ma2_type))
  })
  
  # Maps
  output$map_region <- renderLeaflet({
    leaflet(town_data) |>
      addTiles() |>
      addPolygons(
        fillColor   = TRITON_BLUE,
        fillOpacity = 0.6,
        color       = "white",
        weight      = 1,
        popup       = ~town
      )
  })
  
  output$map_town <- renderLeaflet({
    td <- town_data |>
      filter(town == input$town)
    leaflet(td) |>
      addTiles() |>
      addPolygons(
        fillColor   = TRITON_GOLD,
        fillOpacity = 0.7,
        color       = "white",
        weight      = 2,
        popup       = ~town
      )
  })
  
  output$map_ma <- renderLeaflet({
    leaflet(ma_shape) |>
      addTiles() |>
      addPolygons(
        fillColor   = TRITON_NAVY,
        fillOpacity = 0.4,
        color       = "white",
        weight      = 1
      )
  })
  
  output$map_ma2 <- renderLeaflet({
    leaflet(ma_shape) |>
      addTiles() |>
      addPolygons(
        fillColor   = TRITON_NAVY,
        fillOpacity = 0.4,
        color       = "white",
        weight      = 1
      )
  })
}

shinyApp(ui, server)

