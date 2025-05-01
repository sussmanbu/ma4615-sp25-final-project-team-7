library(shiny)
library(tidyverse)
options("readr.edition" = 1)

# Load data
data <- read_rds("https://github.com/sussmanbu/ma4615-sp25-final-project-team-7/raw/refs/heads/main/dataset_for_shiny/cdi_shiny_data.rds")

# Prepare ordered stratification options
strata_choices <- c("Overall", sort(setdiff(unique(data$StratificationCategory1), "Overall")))

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    body {
      background-color: #f4f7fa;
      font-family: 'Segoe UI', sans-serif;
      color: #2c3e50;
    }
    .shiny-input-container {
      max-width: 600px;
      margin-bottom: 20px;
    }
    h3, h4 {
      color: #2c3e50;
    }
    .well {
      background-color: #e9eef4;
      border: 1px solid #cbd6e2;
      border-radius: 8px;
      padding: 20px;
    }
    .btn {
      background-color: #2980b9;
      color: white;
      border: none;
    }
    .btn:hover {
      background-color: #1c5980;
    }
    ul {
      font-size: 16px;
      color: #34495e;
    }
    li {
      margin-bottom: 10px;
    }
    #dataMessage {
      font-size: 16px;
      color: #555;
      margin-top: 10px;
    }
  "))
  ),
  titlePanel("U.S. Chronic Disease Explorer"),
  br(),
  h3("Explore Disease/Disorder Frequency by Topic, State, and Stratification"),
  wellPanel(
    selectInput("topic", "Select a Disease Topic:",
                choices = sort(unique(data$Topic))),
    selectInput("state", "Select a State:",
                choices = sort(setdiff(unique(data$LocationDesc), "United States"))),
    selectInput("strata_filter", "Select Stratification Category:",
                choices = strata_choices,
                selected = "Overall")
  ),
  hr(),
  h4("Visualization"),
  plotOutput("diseasePlot", height = "600px", width = "90%"),
  textOutput("dataMessage"),
  hr(),
  h4("Takeaways"),
  uiOutput("takeaways"),
  br(),
  uiOutput("disclaimer"),
  br(),
)

# Define server
server <- function(input, output, session) {
  
  observe({
    valid_states <- sort(
      setdiff(
        data %>% filter(Topic == input$topic) %>% pull(LocationDesc) %>% unique(),
        "United States"
      )
    )
    
    # Retain the selected state if still valid
    selected_state <- if (input$state %in% valid_states) input$state else valid_states[1]
    
    updateSelectInput(
      session,
      "state",
      choices = valid_states,
      selected = selected_state
    )
  })
  
  filtered_data <- reactive({
    req(input$topic, input$state, input$strata_filter)
    data %>%
      filter(
        Topic == input$topic,
        LocationDesc %in% c(input$state, "United States"),
        StratificationCategory1 == input$strata_filter,
        DataValueType == "Crude Prevalence"
      ) %>%
      complete(StratificationID1, LocationDesc)
  })
  
  output$diseasePlot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    # Define your custom labels
    label_map <- c(
      "SEXM" = "Male", "SEXF" = "Female", "OVR" = "Overall",
      "AGE1844" = "18–44", "AGE0_44" = "0–44", "AGE4564" = "45–64",
      "AGE65P" = "65+", "AGE4M5Y" = "0–5", "AGE6_14" = "6–14",
      "AGE1_5" = "1–5", "AGE1217" = "12–17", "AGE6_9" = "6–9",
      "AGE1013" = "10–13", "AGE6_11" = "6–11",
      "GRD9" = "Grade 9", "GRD10" = "Grade 10", "GRD11" = "Grade 11", "GRD12" = "Grade 12",
      "WHT" = "White", "BLK" = "Black", "HIS" = "Hispanic", "MRC" = "Multiracial",
      "AIAN" = "American Indian/Alaskan Native", "ASN" = "Asian", "API" = "Asian or Pacific Islander",
      "HAPI" = "Hawaiian/Pacific Islander"
    )
    
    
    ggplot(df, aes(x = StratificationID1, y = DataValue, fill = LocationDesc)) +
      geom_col(position = "dodge") +
      # facet_wrap(~ LocationDesc, ncol = 1) +
      labs(
        title = paste("Chronic Disease Data for", input$state, "vs United States"),
        x = "Stratification Group",
        y = "Prevalence",
        fill = "Location"
      ) +
      scale_fill_manual(values = setNames(c("#1f77b4", "#ff7f0e"), c("United States", input$state))) +
      scale_x_discrete(labels = label_map) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.spacing = unit(3, "cm"))
  })
  
  output$dataMessage <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) {
      "⚠️ No data available for the selected filters. Please try different options."
    } else {
      paste("✅ Showing", nrow(df), "records for", input$state, "and United States.")
    }
  })
  
  output$takeaways <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    label_map <- c(
      "SEXM" = "Male", "SEXF" = "Female", "OVR" = "Overall",
      "AGE1844" = "18–44", "AGE0_44" = "0–44", "AGE4564" = "45–64",
      "AGE65P" = "65+", "AGE4M5Y" = "0–5", "AGE6_14" = "6–14",
      "AGE1_5" = "1–5", "AGE1217" = "12–17", "AGE6_9" = "6–9",
      "AGE1013" = "10–13", "AGE6_11" = "6–11",
      "GRD9" = "Grade 9", "GRD10" = "Grade 10", "GRD11" = "Grade 11", "GRD12" = "Grade 12",
      "WHT" = "White", "BLK" = "Black", "HIS" = "Hispanic", "MRC" = "Multiracial",
      "AIAN" = "American Indian/Alaskan Native", "ASN" = "Asian", "API" = "Asian or Pacific Islander",
      "HAPI" = "Hawaiian/Pacific Islander"
    )
    
    summary_df <- df %>%
      group_by(LocationDesc, StratificationID1) %>%
      summarize(mean_val = mean(DataValue, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = LocationDesc, values_from = mean_val) %>%
      filter(
        !is.na(`United States`) & !is.na(!!sym(input$state)),
        `United States` > 0 & !!sym(input$state) > 0  # Only keep rows where both values > 0
      ) %>%
      mutate(
        label = label_map[StratificationID1],
        label = ifelse(is.na(label), StratificationID1, label),  # Fallback for unmapped labels
        comparison = ifelse(!!sym(input$state) > `United States`, "higher", "lower"),
        text = paste0(
          "The average crude prevalence of <strong>", input$topic, "</strong> in <strong>", input$state, "</strong> ",
          "for the <strong>\"", label, "\"</strong> <strong>", input$strata_filter, "</strong> group is ", "<strong>", round(!!sym(input$state), 2), "</strong>. This means that for every 100 people in this group within the state's population, about ", "<strong>", round(!!sym(input$state), 0), "</strong> of them have some kind of <strong>", input$topic, "</strong> case. ", "This is <strong>", comparison, "</strong> than the average prevalence for this group across the United States, which is ", "<strong>", round(`United States`, 2), "</strong>."
        )
      )
    
    bullets <- summary_df$text
    HTML(paste("<ul>", paste(paste0("<li>", bullets, "</li>"), collapse = ""), "</ul>"))
  })
  
  output$disclaimer <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    state_vals <- df %>%
      filter(LocationDesc == input$state & !is.na(DataValue)) %>%
      pull(StratificationID1)
    
    us_vals <- df %>%
      filter(LocationDesc == "United States" & !is.na(DataValue)) %>%
      pull(StratificationID1)
    
    missing_vals <- setdiff(us_vals, state_vals)
    
    if (length(missing_vals) == 0) return(NULL)
    
    label_map <- c(
      "SEXM" = "Male", "SEXF" = "Female", "OVR" = "Overall",
      "AGE1844" = "18–44", "AGE0_44" = "0–44", "AGE4564" = "45–64",
      "AGE65P" = "65+", "AGE4M5Y" = "0–5", "AGE6_14" = "6–14",
      "AGE1_5" = "1–5", "AGE1217" = "12–17", "AGE6_9" = "6–9",
      "AGE1013" = "10–13", "AGE6_11" = "6–11",
      "GRD9" = "Grade 9", "GRD10" = "Grade 10", "GRD11" = "Grade 11", "GRD12" = "Grade 12",
      "WHT" = "White", "BLK" = "Black", "HIS" = "Hispanic", "MRC" = "Multiracial",
      "AIAN" = "American Indian/Alaskan Native", "ASN" = "Asian", "API" = "Asian or Pacific Islander",
      "HAPI" = "Hawaiian/Pacific Islander"
    )
    
    pretty_vals <- ifelse(!is.na(label_map[missing_vals]), label_map[missing_vals], missing_vals)
    
    # Format: "A", "A and B", "A, B, and C"
    formatted <- if (length(pretty_vals) == 1) {
      pretty_vals
    } else if (length(pretty_vals) == 2) {
      paste(pretty_vals, collapse = " and ")
    } else {
      paste0(paste(pretty_vals[-length(pretty_vals)], collapse = ", "), ", and ", pretty_vals[length(pretty_vals)])
    }
    
    HTML(paste0("<p style='color:#a94442; font-size:16px; margin-top:15px;'><strong>Disclaimer:</strong> ", 
                formatted, " data could not be found within our ", input$state, " state data.</p>"))
  })
}

shinyApp(ui = ui, server = server)