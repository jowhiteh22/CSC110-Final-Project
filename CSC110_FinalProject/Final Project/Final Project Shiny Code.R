# Final Project Shiny Code - Updated
# Requires: shiny, tmap, sf, tidycensus, dplyr, readxl, ggplot2,
# gridExtra, tidyr, scales, tigris, markdown, forcats
#
# Note: Uncomment and set census_api_key(...) if you have a key for better performance.

library(shiny)
library(tmap)
library(sf)
library(tidycensus)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(scales)
library(tigris)
library(markdown)
library(forcats)

# Optional: set tigris cache
options(tigris_use_cache = TRUE)

# Optional: set your Census API key for tidycensus (uncomment and replace YOUR_KEY)
#census_api_key("56157dab791d62b2e15f74e6b6ae11d52307a14f", install = TRUE)
# restart R after installing API key or use Sys.setenv(CENSUS_API_KEY = "56157dab791d62b2e15f74e6b6ae11d52307a14f")

# ------------------------
# Load datasets (local paths)
# ------------------------
lfprfw_state <- read_excel("data/Labor_force_participation_rate_wm.xlsx")
female_participation <- read_excel("data/Participation.xlsx")
job_level_2017 <- read_excel("data/Change_in_representation_of_women_at_every_job_level.xlsx", sheet = "All2017")
job_level_2024 <- read_excel("data/Change_in_representation_of_women_at_every_job_level.xlsx", sheet = "All2024")
w_m_income <- read_excel("data/women_men_income.xlsx")

# ------------------------
# Static helper objects
# ------------------------
# MUST be defined before using in mutate(...)
desired_order <- c("Entry_level", "Manager", "Director", "VP", "Sr_P", "C_suite_exec")

# ------------------------
# Get ACS data (state geometry)
# ------------------------
us <- suppressMessages(
  get_acs(
    geography   = "state", 
    variables   = "B02001_001", 
    year        = 2022,
    geometry    = TRUE
  )
)

# Merge US data with labor force participation rates by state
merged_us_lfprfw_state_data <- us %>%
  left_join(
    lfprfw_state,
    by = c("NAME" = "State")
  ) %>%
  select(
    NAME,
    Female_Participation,
    Male_Participation,
    geometry
  )

# try shifting geometry if shift_geometry is available; otherwise keep original
merged_us_lfprfw_state_data <- tryCatch(
  shift_geometry(merged_us_lfprfw_state_data),
  error = function(e) merged_us_lfprfw_state_data
)

# ------------------------
# Transform job-level tables
# ------------------------
# Use pivot_longer then relevel Position with desired_order (use forcats to avoid errors)
new_2017 <- job_level_2017 %>%
  pivot_longer(
    cols = c("White", "Color", "Overall"),
    names_to = "Race",
    values_to = "Percent"
  ) %>%
  mutate(
    # relevel if Position column exists; if not, leave as-is
    Position = if ("Position" %in% names(.)) fct_relevel(as.character(Position), desired_order) else Position
  )

new_2024 <- job_level_2024 %>%
  pivot_longer(
    cols = c("White", "Color", "Overall"),
    names_to = "Race",
    values_to = "Percent"
  ) %>%
  mutate(
    Position = if ("Position" %in% names(.)) fct_relevel(as.character(Position), desired_order) else Position
  )

# ------------------------
# Wages: combine into long format for easier plotting
# ------------------------
wage_long <- w_m_income %>%
  pivot_longer(cols = c(Women, Men), names_to = "Gender", values_to = "Income")

# ------------------------
# Shiny UI
# ------------------------
ui <- fluidPage(
  titlePanel("Impact of Societal Expectations on Female Careers"),
  tabsetPanel(
    tabPanel("Labor Force Participation Rate for Women & Men by State",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gender", "Select Gender:",
                             choices = c("Female" = "Female_Participation", "Male" = "Male_Participation")),
                 width = 2
               ),
               mainPanel(
                 tmapOutput("map"),
                 width = 10
               )
             )
    ),
    tabPanel("Labor Force Participation Rate for Women by Age Over Time",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ages", "Select Age Groups:",
                                    choices = c("Ages 25 to 34" = "25to34", 
                                                "Ages 35 to 44" = "35to44", 
                                                "Ages 45 to 54" = "45to54")),
                 width = 2
               ),
               mainPanel(
                 plotOutput("linePlot"),
                 width = 10
               )
             )
    ),
    tabPanel("Change in Representation of Women in Job-levels",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("year", "Select Year:",
                                    choices = c("2017", "2024")),
                 checkboxGroupInput("race", "Select Race:",
                                    choices = c("White", "Color", "Overall")),
                 width = 2
               ),
               mainPanel(
                 plotOutput("jobLevelPlot"),
                 width = 10
               )
             )
    ),
    tabPanel("Women vs. Men Average Annual Income Over Time",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("gender_income", "Select Gender:",
                                    choices = c("Women" = "Women", "Men" = "Men")),
                 width = 2
               ),
               mainPanel(
                 plotOutput("incomePlot"),
                 width = 10
               )
             )
    ),
    tabPanel("User Guide",
             includeMarkdown("Userguide.md")
    )
  )
)

# ------------------------
# Server
# ------------------------
server <- function(input, output) {
  
  # Map
  output$map <- renderTmap({
    req(input$gender)
    tmap_mode("plot")
    tm_shape(merged_us_lfprfw_state_data) +
      tm_polygons(
        col = input$gender,
        palette = "Greens",
        # If you want dynamic breaks you could compute quantiles; kept static for interpretability
        breaks = c(0, 60, 65, 70, 75, 80, 85, 90, 100),
        labels = c("0% to 60%", "60% to 65%", "65% to 70%", "70% to 75%", "75% to 80%", "80% to 85%", "85% to 90%", "90% and higher"),
        title = "Labor Force Participation Rate (%)",
        id = "NAME",
        popup.vars = c("Participation Rate" = input$gender)
      ) +
      tm_layout(
        main.title = paste("Labor Force Participation Rate by State for", ifelse(input$gender == "Female_Participation", "Women", "Men"), "(2022)"),
        main.title.position = c("center", "top"),
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = FALSE
      )
  })
  
  # Line plot for female participation by age groups
  output$linePlot <- renderPlot({
    selected_ages <- input$ages
    validate(
      need(length(selected_ages) > 0, "Please select at least one age group from the sidebar.")
    )
    
    # check that selected age columns exist in the data
    missing_cols <- setdiff(selected_ages, names(female_participation))
    validate(
      need(length(missing_cols) == 0, paste("Missing age columns in data:", paste(missing_cols, collapse = ", ")))
    )
    
    plot_data <- female_participation %>%
      select(Year, all_of(selected_ages)) %>%
      pivot_longer(cols = all_of(selected_ages), names_to = "AgeGroup", values_to = "ParticipationRate")
    
    # dynamic y-limits with small padding
    yr_min <- min(plot_data$ParticipationRate, na.rm = TRUE)
    yr_max <- max(plot_data$ParticipationRate, na.rm = TRUE)
    y_pad <- (yr_max - yr_min) * 0.05
    y_lower <- ifelse(is.finite(yr_min - y_pad), yr_min - y_pad, NA)
    y_upper <- ifelse(is.finite(yr_max + y_pad), yr_max + y_pad, NA)
    
    ggplot(plot_data, aes(x = Year, y = ParticipationRate, color = AgeGroup)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "Female Labor Force Participation by Age Group",
        x = "Year",
        y = "Participation Rate (%)"
      ) +
      scale_x_continuous(
        breaks = seq(min(female_participation$Year, na.rm = TRUE), max(female_participation$Year, na.rm = TRUE), by = 1),
        labels = as.integer
      ) +
      scale_y_continuous(
        labels = scales::percent_format(scale = 1),
        limits = c(y_lower, y_upper)
      ) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  # Job level bar charts (2017 and/or 2024)
  output$jobLevelPlot <- renderPlot({
    selected_years <- input$year
    selected_races <- input$race
    
    validate(
      need(length(selected_years) > 0, "Please select at least one year (2017 or 2024)."),
      need(length(selected_races) > 0, "Please select at least one race category.")
    )
    
    plots <- lapply(selected_years, function(year) {
      data_selected <- if (year == "2017") new_2017 else new_2024
      data_selected <- data_selected %>% filter(Race %in% selected_races)
      
      # if Position column is character, convert to factor with desired_order (handles missing gracefully)
      if ("Position" %in% names(data_selected)) {
        # Warn if there are position levels not in desired_order (not stopping the app)
        pos_vals <- unique(as.character(data_selected$Position))
        missing_levels <- setdiff(pos_vals, desired_order)
        if (length(missing_levels) > 0) {
          # use a message printed to console; don't stop rendering
          message("Warning: Position values not in desired_order: ", paste(missing_levels, collapse = ", "))
        }
        data_selected <- data_selected %>%
          mutate(Position = factor(Position, levels = desired_order))
      }
      
      ggplot(data_selected, aes(x = Position, y = Percent, fill = Race)) +
        geom_col(position = "dodge", width = 0.7) +
        labs(
          title = paste0("Changes in Representation in Job Levels (", year, ")"),
          x = "Job Position",
          y = "Percent"
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_minimal() +
        theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # If only one plot, just print it; otherwise arrange vertically
    if (length(plots) == 1) {
      print(plots[[1]])
    } else {
      do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
    }
  })
  
  # Income plot (Women vs Men)
  output$incomePlot <- renderPlot({
    selected_genders <- input$gender_income
    validate(
      need(length(selected_genders) > 0, "Please select at least one gender (Women and/or Men).")
    )
    
    plot_data <- wage_long %>%
      filter(Gender %in% selected_genders)
    
    # dynamic y-limits w/ padding
    yr_min <- min(plot_data$Income, na.rm = TRUE)
    yr_max <- max(plot_data$Income, na.rm = TRUE)
    y_pad <- (yr_max - yr_min) * 0.05
    y_lower <- ifelse(is.finite(yr_min - y_pad), yr_min - y_pad, NA)
    y_upper <- ifelse(is.finite(yr_max + y_pad), yr_max + y_pad, NA)
    
    ggplot(plot_data, aes(x = Year, y = Income, color = Gender)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(
        title = "Average Annual Income by Gender",
        x = "Year",
        y = "Average Annual Income ($)"
      ) +
      scale_x_continuous(
        breaks = seq(min(plot_data$Year, na.rm = TRUE), max(plot_data$Year, na.rm = TRUE), by = 1),
        labels = as.integer
      ) +
      scale_y_continuous(labels = label_dollar(), limits = c(y_lower, y_upper)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

