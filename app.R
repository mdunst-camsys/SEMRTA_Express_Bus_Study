library(shiny)
library(dplyr)
library(DT)
library(readxl)
library(leaflet)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringi)
library(tigris)
library(sf)
library(mapview)
library(igraph)
library(plotly)
library(purrr)
library(shiny)
library(knitr)
library(kableExtra)
library(gt)
library(gtExtras)
library(reactable)
library(htmltools)
library(scales)
g <- glimpse

# setwd("C:/Users/mdunst/OneDrive - Cambridge Systematics/Documents/GitHub/SEMRTA_Express_Bus_Study")
# rsconnect::writeManifest()
nodes <- read.csv("data/SEMRTA_Nodes.csv") %>%
  st_as_sf(., coords=c("lon","lat"), crs=4326) %>%
  rename(node = 1)
rta_bgs <- readRDS("data/rta_bgs.rds")
rta_counties <- readRDS("data/rta_counties.rds")
days <- c("Weekday","Saturday","Sunday")
time_periods <- c(c("AM Peak (6-9)","Midday (9-3)","PM Peak (3-7)","Evening (7-10)","Night (10-6)"))
communities <- c("Low-Income Only","Minority Only","Low-Income and Minority","Other","Undefined")
locus_time <- readRDS("data/locus_time_no_short.rds")
time_period_order <- c("AM Peak (6-9)","Midday (9-3)","PM Peak (3-7)","Evening (7-10)","Night (10-6)")
time_hours <- data.frame(time_period = c(time_period_order, "All Day"),
                         length = c(3, 6, 4, 3, 8, 24))



# ── UI ──────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { font-family: 'Georgia', serif; background: #f7f7f5; color: #2c2c2c; }
    h4   { font-size: 15px; font-weight: 700; letter-spacing: .04em;
           text-transform: uppercase; color: #444; margin-bottom: 14px; }

    /* label rows */
    .label-row {
    display: grid;
    grid-template-columns: 58px 1fr 1fr;
    align-items: center;
    gap: 8px;
    padding: 5px 0;
    border-bottom: 1px solid #e8e6e0;
    }
    .label-row:last-child { border-bottom: none; }
    .label-text { font-size: 13px; flex: 1; }

    /* tiny numeric inputs */
    .label-row input[type='number'] {
      width: 58px !important; height: 26px !important;
      padding: 2px 5px !important; font-size: 12px !important;
      border: 1px solid #ccc; border-radius: 3px;
      background: #fff; text-align: right;
    }
    .label-row input[type='number']:focus {
      outline: none; border-color: #3a7bd5; box-shadow: 0 0 0 2px rgba(58,123,213,.15);
    }

    /* Go button */
    #go_btn {
      margin-top: 16px; width: 100%;
      background: #2c2c2c; color: #fff; border: none;
      padding: 8px 0; font-size: 13px; letter-spacing: .06em;
      text-transform: uppercase; border-radius: 3px; cursor: pointer;
      transition: background .2s;
    }
    #go_btn:hover { background: #3a7bd5; }

    /* side panel */
    .well { background: #fff; border: 1px solid #e0ddd6;
            border-radius: 4px; padding: 18px; box-shadow: 0 1px 4px rgba(0,0,0,.06); }

    /* table area */
    #result_table { margin-top: 6px; }
    .table-wrap         { display: flex; align-items: center; }
    .table-row-label    { writing-mode: vertical-rl; transform: rotate(180deg);
                          text-align: center; font-weight: bold;
                          margin-right: 8px; white-space: nowrap; }
    .table-col-label    { text-align: center; font-weight: bold; margin-bottom: 6px; }
    .table-inner        { flex: 1; }
  "))),
  
  titlePanel(NULL),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      actionButton("go_btn", "Go", icon = icon("arrow-right")),
      
      hr(),
      
      # selectInput(
      #   inputId  = "day_selection",
      #   label    = "Select a variable",
      #   choices  = days
      # ),
      # 
      # selectInput(
      #   inputId  = "time_selection",
      #   label    = "Select a variable",
      #   choices  = c("All Day", time_periods)
      # ),
      
      h4("Assign Order:"),
      
      # dynamic input rows injected here
      uiOutput("label_inputs"),
      
      # selectInput(
      #   inputId  = "radius_selection",
      #   label    = "Select a variable",
      #   choices  = c("Half-Mile", "Three Miles")
      # ),
    ),
      
    mainPanel(
      width = 9,
      h4("Map of Stops and Catchments:"),
      leafletOutput("nodes_map"),
      
      conditionalPanel(
        condition = "input.go_btn > 0",
        
        hr(),
        
        # dropdowns side by side
        conditionalPanel(
          condition = "input.go_btn > 0",
          fluidRow(
            column(6, selectInput(
              inputId  = "day_selection",
              label    = "Select a Day Type",
              choices  = days
            ),),
            column(6, selectInput(
              inputId  = "time_selection",
              label    = "Select a Time Period",
              choices  = c("All Day", time_periods)
            ))
          )
        ),
        
        hr(),
        
      tabsetPanel(
        tabPanel("Half-Mile",
                 uiOutput("table_title"),
                 
                 fluidRow(
                   column(6, plotlyOutput("half_gross_heat")),
                   column(6, plotlyOutput("half_hourly_heat"))
                 )
                 
                 # div(class = "table-wrap",
                 #     div("Origins",      class = "table-row-label"),
                 #     div(class = "table-inner",
                 #         div("Destinations", class = "table-col-label"),
                 #         DTOutput("interim_table")
                 #     )
                 # )
                 ),
        tabPanel("Three Miles",
                 uiOutput("table_title_2"),
                 
                 fluidRow(
                   column(6, plotlyOutput("three_gross_heat")),
                   column(6, plotlyOutput("three_hourly_heat"))
                 )
                 
                 # div(class = "table-wrap",
                 #     div("Origins",      class = "table-row-label"),
                 #     div(class = "table-inner",
                 #         div("Destinations", class = "table-col-label"),
                 #         DTOutput("interim_table_2")
                 #     )
                 # )
                 )
      ),
      
      hr(),
      
      tabsetPanel(
        br(),
        
        tabPanel("Half-Mile",
                 # uiOutput("direction_title_half"),
                 fluidRow(
                   column(6, plotlyOutput("half_gross_chart")),
                   column(6, plotlyOutput("half_hourly_chart"))
                 )
                 # DTOutput("gt_table_1")),
        ),
        tabPanel("Three Miles",
                 # uiOutput("direction_title_half"),
                 fluidRow(
                   column(6, plotlyOutput("three_gross_chart")),
                   column(6, plotlyOutput("three_hourly_chart"))
                 )
                 # DTOutput("gt_table_1")),
        )
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # at the top of server
  sort_col   <- reactiveVal("label")
  sort_order <- reactiveVal("asc")
  
  # click handlers
  observeEvent(input$sort_label, {
    sort_col("label")
    sort_order(if (sort_order() == "asc") "desc" else "asc")
  })
  
  observeEvent(input$sort_extra, {
    sort_col("extra")
    sort_order(if (sort_order() == "asc") "desc" else "asc")
  })
  
  # Unique labels from the dataset
  unique_labels <- reactive({
    sort(unique(as.character(nodes$label)))
  })
  
  # Build one numeric input per unique label
  output$label_inputs <- renderUI({
    df <- unique(nodes[, c("label", "county")])
    
    # sort by whichever column was clicked
    if (sort_col() == "label") {
      df <- df[order(df$label,       decreasing = sort_order() == "desc"), ]
    } else {
      df <- df[order(df$county, decreasing = sort_order() == "desc"), ]
    }
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      input_id <- paste0("val_", make.names(df$label[i]))
      div(class = "label-row",
          numericInput(inputId = input_id, label = NULL, min = 1, value = NA, step = 1, width = "58px"),
          span(df$label[i],       class = "label-text"),
          span(df$county[i], class = "label-text")
      )
    })
    
    # header row with clickable sort buttons
    header <- div(class = "label-row",
                  div(),  # empty first cell, same as numeric input column
                  actionLink("sort_label", "Stop"),
                  actionLink("sort_extra", "County")
    )
    
    tagList(header, rows)
  })
  
  go_pressed <- reactiveVal(FALSE)
  
  observeEvent(input$go_btn, {
    go_pressed(TRUE)
  })
  
  # On Go: build new column, filter rows where value > 0
  filtered_data <- eventReactive(input$go_btn, {
    labs <- unique_labels()
    
    value_map <- sapply(labs, function(lbl) {
      val <- input[[paste0("val_", make.names(lbl))]]
      if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
    })
    
    # get only the entered (non-NA) values
    entered <- value_map[!is.na(value_map)]
    
    validate(
      need(
        length(entered) == length(unique(entered)),
        "Duplicate values entered — each label must have a unique number."
      )
    )
    
    names(value_map) <- labs
    
    df <- nodes
    df$order <- value_map[as.character(df$label)]
    
    # Keep only rows where assigned_value > 0
    df <- df[!is.na(df$order) & df$order > 0, ]
    rownames(df) <- 1:nrow(df)
    df
  })
  
  #Create node order
  node_order <- reactive({
    arrange(filtered_data(), order) %>% pull(label)
  })
  
  #Create buffers
  buffer_small <- reactive({
    st_buffer(st_transform(filtered_data(), 4267), 5280 / 3.28084 * 0.5) %>%
    st_transform(4326)
  })
  
  buffer_large <- reactive({
    st_buffer(st_transform(filtered_data(), 4267), 5280 / 3.28084 * 3) %>%
    st_transform(4326)
  })
  
  buffer_small_bgs <- reactive({
    st_join(rta_bgs, buffer_small()) %>%
    filter(!is.na(label)) %>%
    pull(GEOID)
  })
  
  buffer_large_bgs <- reactive({
    st_join(rta_bgs, buffer_large()) %>%
    filter(!is.na(label)) %>%
    pull(GEOID)
  })
  
  #Get bgs for small buffer
  final_bgs_small <- reactive({
    rta_bgs %>%
    filter(GEOID %in% buffer_small_bgs()) %>% #filter to just the BGs that touch a node buffer
    mutate(nearest_node = st_nearest_feature(st_centroid(geometry), filtered_data())) %>% #get the nearest node to each BG
    merge(., st_drop_geometry(filtered_data()), by.x="nearest_node", by.y=0) %>% #merge over the node attributes
    select(GEOID, node, geometry) #%>% #retain necessary columns
    #browser()
  })
  
  #Get bgs for large buffer
  final_bgs_large <- reactive({
    rta_bgs %>%
    filter(GEOID %in% buffer_large_bgs()) %>% #filter to just the BGs that touch a node buffer
    mutate(nearest_node = st_nearest_feature(st_centroid(geometry), filtered_data())) %>% #get the nearest node to each BG
    merge(., st_drop_geometry(filtered_data()), by.x="nearest_node", by.y=0) %>% #merge over the node attributes
    select(GEOID, node, geometry) #retain necessary columns %>%
  })
  
  filtered_time_small <- reactive({
    filter(locus_time,
           origin_bg %in% as.numeric(buffer_small_bgs()) & destination_bg %in% as.numeric(buffer_small_bgs())) %>%
    filter(origin_bg != destination_bg) %>%
    merge(., st_drop_geometry(final_bgs_small()), by.x="origin_bg", by.y="GEOID") %>%
    rename(o_node = node) %>%
    merge(., st_drop_geometry(final_bgs_small()), by.x="destination_bg", by.y="GEOID") %>%
    rename(d_node = node) %>%
    mutate(buffer = "Half-Mile")
  })
  
  trips_by_day_tables_small <- reactive({
    filtered_time_small() %>%
    group_by(o_node, d_node, day_type, time_period) %>%
    filter(o_node != d_node) %>%
    summarize(daily_trips = sum(daily_trips)) %>%
    merge(., st_drop_geometry(filtered_data()), by.x="o_node", by.y="node") %>%
    rename(o_label = label, o_node_order = order) %>%
    merge(., st_drop_geometry(filtered_data()), by.x="d_node", by.y="node") %>%
    rename(d_label = label, d_node_order = order) %>%
    mutate(direction = ifelse(d_node_order > o_node_order, "Inbound","Outbound"))
  })
  
  filtered_time_large <- reactive({
    filter(locus_time,
           origin_bg %in% as.numeric(buffer_large_bgs()) & destination_bg %in% as.numeric(buffer_large_bgs())) %>%
      filter(origin_bg != destination_bg) %>%
      merge(., st_drop_geometry(final_bgs_large()), by.x="origin_bg", by.y="GEOID") %>%
      rename(o_node = node) %>%
      merge(., st_drop_geometry(final_bgs_large()), by.x="destination_bg", by.y="GEOID") %>%
      rename(d_node = node) %>%
      mutate(buffer = "Three Miles")
  })
  
  trips_by_day_tables_large <- reactive({
    filtered_time_large() %>%
      group_by(o_node, d_node, day_type, time_period) %>%
      filter(o_node != d_node) %>%
      summarize(daily_trips = sum(daily_trips)) %>%
      merge(., st_drop_geometry(filtered_data()), by.x="o_node", by.y="node") %>%
      rename(o_label = label, o_node_order = order) %>%
      merge(., st_drop_geometry(filtered_data()), by.x="d_node", by.y="node") %>%
      rename(d_label = label, d_node_order = order) %>%
      mutate(direction = ifelse(d_node_order > o_node_order, "Inbound","Outbound"))
  })
  
  df <- reactive({
    expand.grid(o_label = unique(trips_by_day_tables_small()$o_label), d_label = unique(trips_by_day_tables_small()$o_label))
  })
  
  small_table <- reactive({
    if (input$time_selection %in% time_periods) {
      filter(trips_by_day_tables_small(),
             day_type==input$day_selection & time_period==input$time_selection) %>%
        ungroup() %>%
        arrange(o_node, d_node) %>%
        select(c(o_label, d_label, daily_trips)) %>%
        mutate(daily_trips = round(daily_trips, 0)) %>%
        merge(df(), ., by=c("o_label","d_label"), all=T) %>%
        arrange(factor(o_label, node_order()), factor(d_label, node_order())) %>%
        mutate(daily_trips = replace_na(daily_trips, 0)) %>%
        pivot_wider(names_from = d_label, values_from = daily_trips) %>%
        rename(Node = o_label)
    } else {
      filter(trips_by_day_tables_small(),
             day_type==input$day_selection) %>%
        group_by(o_node, d_node, day_type, o_label, d_label) %>%
        summarize(daily_trips = sum(daily_trips)) %>%
        ungroup() %>%
        arrange(o_node, d_node) %>%
        select(c(o_label, d_label, daily_trips)) %>%
        mutate(daily_trips = round(daily_trips, 0)) %>%
        merge(df(), ., by=c("o_label","d_label"), all=T) %>%
        arrange(factor(o_label, node_order()), factor(d_label, node_order())) %>%
        mutate(daily_trips = replace_na(daily_trips, 0)) %>%
        pivot_wider(names_from = d_label, values_from = daily_trips) %>%
        rename(Node = o_label)
    }
  })
  
  large_table <- reactive({
    if (input$time_selection %in% time_periods) {
      filter(trips_by_day_tables_large(),
             day_type==input$day_selection & time_period==input$time_selection) %>%
        ungroup() %>%
        arrange(o_node, d_node) %>%
        select(c(o_label, d_label, daily_trips)) %>%
        mutate(daily_trips = round(daily_trips, 0)) %>%
        merge(df(), ., by=c("o_label","d_label"), all=T) %>%
        arrange(factor(o_label, node_order()), factor(d_label, node_order())) %>%
        mutate(daily_trips = replace_na(daily_trips, 0)) %>%
        pivot_wider(names_from = d_label, values_from = daily_trips) %>%
        rename(Node = o_label)
    } else {
      filter(trips_by_day_tables_large(),
             day_type==input$day_selection) %>%
        group_by(o_node, d_node, day_type, o_label, d_label) %>%
        summarize(daily_trips = sum(daily_trips)) %>%
        ungroup() %>%
        arrange(o_node, d_node) %>%
        select(c(o_label, d_label, daily_trips)) %>%
        mutate(daily_trips = round(daily_trips, 0)) %>%
        merge(df(), ., by=c("o_label","d_label"), all=T) %>%
        arrange(factor(o_label, node_order()), factor(d_label, node_order())) %>%
        mutate(daily_trips = replace_na(daily_trips, 0)) %>%
        pivot_wider(names_from = d_label, values_from = daily_trips) %>%
        rename(Node = o_label)
    }
  })
  
  direction <- reactive({
    trips_by_day_tables_small() %>%
    filter(day_type==input$day_selection) %>%
    group_by(direction, time_period) %>%
    summarize(trips=sum(daily_trips)) %>%
    mutate(trips = round(trips, 0)) %>%
    pivot_wider(names_from = direction, values_from = trips) %>%
    mutate(
      hours = case_when(
        str_detect(time_period, "6-9")  ~ 3,
        str_detect(time_period, "9-3")  ~ 6,
        str_detect(time_period, "3-7")  ~ 4,
        str_detect(time_period, "7-10") ~ 3,
        str_detect(time_period, "10-6") ~ 8,
      ),
      Inbound_hr = round(Inbound / hours, 0),
      Outbound_hr = round(Outbound / hours, 0)
    ) %>%
    select(-hours) %>%
    mutate(time_period = factor(time_period, time_period_order)) %>%
    arrange(time_period)
  })
  
  direction_large <- reactive({
    trips_by_day_tables_large() %>%
      filter(day_type==input$day_selection) %>%
      group_by(direction, time_period) %>%
      summarize(trips=sum(daily_trips)) %>%
      mutate(trips = round(trips, 0)) %>%
      pivot_wider(names_from = direction, values_from = trips) %>%
      mutate(
        hours = case_when(
          str_detect(time_period, "6-9")  ~ 3,
          str_detect(time_period, "9-3")  ~ 6,
          str_detect(time_period, "3-7")  ~ 4,
          str_detect(time_period, "7-10") ~ 3,
          str_detect(time_period, "10-6") ~ 8,
        ),
        Inbound_hr = round(Inbound / hours, 0),
        Outbound_hr = round(Outbound / hours, 0)
      ) %>%
      select(-hours) %>%
      mutate(time_period = factor(time_period, time_period_order)) %>%
      arrange(time_period)
  })
  
  output$nodes_map <- renderLeaflet({
    if (!go_pressed()) {
      leaflet() %>%
        addTiles() %>%
        addPolygons(data=rta_counties, fillColor = "black", fillOpacity = 0.09, color="black", weight=2) %>%
        addCircleMarkers(data=nodes, color = "black", fillColor="lightblue", label=~label, radius=6, weight=1.5, fillOpacity = 0.8)
    } else {
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=rta_counties, fillColor = "black", fillOpacity = 0.07, color="black", weight=1.5) %>%
      addPolygons(data=final_bgs_large(), fillColor = "black", fillOpacity = 0.15, color="black", weight=0.5) %>%
      addPolygons(data=final_bgs_small(), fillColor = "black", fillOpacity = 0.25, color="black", weight=0.75) %>%
      addCircleMarkers(data=nodes, color = "black", fillColor="white", label=~label, radius=4, weight=1.5, fillOpacity = 0.8) %>%
      addPolylines(data=arrange(st_drop_geometry(filtered_data()), order),
                   lng = ~st_coordinates(arrange(filtered_data(), order))[,1],
                   lat = ~st_coordinates(arrange(filtered_data(), order))[,2],
                   color = "darkblue",
                   weight = 2) %>%
      addCircleMarkers(data=filtered_data(), color = "black", fillColor="lightblue", label=~label, radius=6, weight=1.5, fillOpacity = 0.85)
    }
    })
  
  output$table_title <- renderUI({
    h4(paste0("Results for: ", "Half-Mile Catchment ", input$day_selection, " and ", input$time_selection))
  })
  
  # output$interim_table <- renderDT({
  #   req(input$go_btn)
  #   datatable(
  #     small_table(),
  #     rownames  = FALSE,
  #     options   = list(
  #       pageLength = 15,
  #       scrollX    = TRUE,
  #       dom        = "tip"
  #     )
  #   ) %>%
  #     formatCurrency(columns = -1, currency = "", digits = 0)
  # })
  
  output$half_gross_heat <- renderPlotly({
    half_heat <- small_table() %>%
      rename(o_node = Node) %>%
      pivot_longer(cols = -1) %>%
      rename(d_node = name,
             Trips = value) %>%
      mutate(Trips = if_else(o_node == d_node, NA, Trips)) %>%
      ggplot(., aes(d_node, o_node, fill= Trips)) + 
      geom_tile()+
      scale_fill_continuous(label = comma, palette = c("lightblue","navy"), na.value = "grey80")+
      labs(x = "Destination", y = "Origin", fill = "Gross Trips")+
      theme_bw()
    
    ggplotly(half_heat) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$half_hourly_heat <- renderPlotly({
    half_hourly_heat <- small_table() %>%
      rename(o_node = Node) %>%
      pivot_longer(cols = -1) %>%
      rename(d_node = name,
             Trips = value) %>%
      mutate(Trips = if_else(o_node == d_node, NA, Trips)) %>%
      mutate(time_period = input$time_selection) %>%
      merge(., time_hours, by="time_period") %>%
      mutate(`Trips per Hour` = round(Trips/length, 0)) %>%
      ggplot(., aes(d_node, o_node, fill= `Trips per Hour`)) + 
      geom_tile()+
      scale_fill_continuous(palette = c("lightblue","navy"), na.value = "grey80")+
      labs(x = "Destination", y = "Origin", fill = "Trips per Hour")+
      theme_bw()
    
    ggplotly(half_hourly_heat) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$table_title_2 <- renderUI({
    h4(paste0("Results for: ", "Three Mile Catchment ", input$day_selection, " and ", input$time_selection))
  })
  
  # output$interim_table_2 <- renderDT({
  #   req(input$go_btn)
  #   datatable(
  #     large_table(),
  #     rownames  = FALSE,
  #     options   = list(
  #       pageLength = 15,
  #       scrollX    = TRUE,
  #       dom        = "tip"
  #     )
  #   ) %>%
  #     formatCurrency(columns = -1, currency = "", digits = 0)
  # })
  
  output$three_gross_heat <- renderPlotly({
    three_heat <- large_table() %>%
      rename(o_node = Node) %>%
      pivot_longer(cols = -1) %>%
      rename(d_node = name,
             Trips = value) %>%
      mutate(Trips = if_else(o_node == d_node, NA, Trips)) %>%
      ggplot(., aes(d_node, o_node, fill= Trips)) + 
      geom_tile()+
      scale_fill_continuous(label = comma, palette = c("lightblue","navy"), na.value = "grey80")+
      labs(x = "Destination", y = "Origin", fill = "Gross Trips")+
      theme_bw()
    
    ggplotly(three_heat) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$three_hourly_heat <- renderPlotly({
    three_hourly_heat <- large_table() %>%
      rename(o_node = Node) %>%
      pivot_longer(cols = -1) %>%
      rename(d_node = name,
             Trips = value) %>%
      mutate(Trips = if_else(o_node == d_node, NA, Trips)) %>%
      mutate(time_period = input$time_selection) %>%
      merge(., time_hours, by="time_period") %>%
      mutate(`Trips per Hour` = round(Trips/length, 0)) %>%
      ggplot(., aes(d_node, o_node, fill= `Trips per Hour`)) + 
      geom_tile()+
      scale_fill_continuous(palette = c("lightblue","navy"), na.value = "grey80")+
      labs(x = "Destination", y = "Origin", fill = "Trips per Hour")+
      theme_bw()
    
    ggplotly(three_hourly_heat) %>%
      layout(font = list(family = "Georgia"))
  })
  
  # output$direction_title_half <- renderUI({
  #   h4(paste0("Results for: ", "Half-Mile Catchment, ", input$day_selection))
  # })
  # 
  # output$gt_table_1 <- renderDT({
  #   req(input$go_btn)
  #   datatable(
  #     direction(),
  #     colnames = c("Time Period","Inbound (Gross)","Outbound (Gross)","Inbound per Hour","Outbound per Hour"),
  #     rownames  = FALSE,
  #     options   = list(
  #       pageLength = 15,
  #       scrollX    = TRUE,
  #       dom        = "tip"
  #     )
  #   ) %>%
  #     formatCurrency(columns = -1, currency = "", digits = 0)
  # })
  # 
  # output$direction_title_three <- renderUI({
  #   h4(paste0("Results for: ", "Three Mile Catchment, ", input$day_selection))
  # })
  # 
  # output$gt_table_2 <- renderDT({
  #   req(input$go_btn)
  #   datatable(
  #     direction_large(),
  #     colnames = c("Time Period","Inbound (Gross)","Outbound (Gross)","Inbound per Hour","Outbound per Hour"),
  #     rownames  = FALSE,
  #     options   = list(
  #       pageLength = 15,
  #       scrollX    = TRUE,
  #       dom        = "tip"
  #     )
  #   ) %>%
  #     formatCurrency(columns = -1, currency = "", digits = 0)
  # })
  
  output$half_gross_chart <- renderPlotly({
    half_gross_chart <- direction() %>%
      select(c(time_period, Inbound, Outbound)) %>%
      pivot_longer(cols = c(Inbound, Outbound)) %>%
      rename(Trips = value,
             `Time Period` = time_period,
             Direction = name) %>%
      ggplot(., aes(fill=Direction, y=Trips, x=`Time Period`))+
      geom_col(position="dodge", stat="identity")+
      scale_fill_discrete(palette = c("navy","orange"))+
      labs(title= "Gross Trips by Time Period and Direction")+
      theme_bw()
    
    ggplotly(half_gross_chart) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$half_hourly_chart <- renderPlotly({
    half_hourly_chart <- direction() %>%
      rename(`Inbound per Hour` = Inbound_hr,
             `Outbound per Hour` = Outbound_hr) %>%
      select(c(time_period, `Inbound per Hour`, `Outbound per Hour`)) %>%
      pivot_longer(cols = c(`Inbound per Hour`, `Outbound per Hour`)) %>%
      rename(`Trips per Hour` = value,
             `Time Period` = time_period,
             Direction = name) %>%
      ggplot(., aes(fill=Direction, y=`Trips per Hour`, x=`Time Period`))+
      geom_col(position="dodge", stat="identity")+
      scale_fill_discrete(palette = c("navy","orange"))+
      labs(title= "Hourly Trips by Time Period and Direction")+
      theme_bw()
    
    ggplotly(half_hourly_chart) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$three_gross_chart <- renderPlotly({
    three_gross_chart <- direction_large() %>%
      select(c(time_period, Inbound, Outbound)) %>%
      pivot_longer(cols = c(Inbound, Outbound)) %>%
      rename(Trips = value,
             `Time Period` = time_period,
             Direction = name) %>%
      ggplot(., aes(fill=Direction, y=Trips, x=`Time Period`))+
      geom_col(position="dodge", stat="identity")+
      scale_fill_discrete(palette = c("navy","orange"))+
      labs(title= "Gross Trips by Time Period and Direction")+
      theme_bw()
    
    ggplotly(three_gross_chart) %>%
      layout(font = list(family = "Georgia"))
  })
  
  output$three_hourly_chart <- renderPlotly({
    three_hourly_chart <- direction_large() %>%
      rename(`Inbound per Hour` = Inbound_hr,
             `Outbound per Hour` = Outbound_hr) %>%
    select(c(time_period, `Inbound per Hour`, `Outbound per Hour`)) %>%
      pivot_longer(cols = c(`Inbound per Hour`, `Outbound per Hour`)) %>%
      rename(`Trips per Hour` = value,
             `Time Period` = time_period,
             Direction = name) %>%
      ggplot(., aes(fill=Direction, y=`Trips per Hour`, x=`Time Period`))+
      geom_col(position="dodge", stat="identity")+
      scale_fill_discrete(palette = c("navy","orange"))+
      labs(title= "Hourly Trips by Time Period and Direction")+
      theme_bw()
    
    ggplotly(three_hourly_chart) %>%
      layout(font = list(family = "Georgia"))
  })
}

shinyApp(ui, server)