library(shiny)
library(tidyverse)
library(ecotaxar)
library(lubridate)
library(scales)


## Read data ----
#--------------------------------------------------------------------------#
# Coastline data
coast <- read_csv("data/coast.csv")

# Outpur dir for transects 
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# List files in output dir
saved_files <- list.files(output_dir, pattern = ".csv")

# Define colors
cols <- c("aller" = "#66c2a5", "retour" = "#fc8d62", "transit" = "dimgray")

# List projects of sea002 with UVP6
db <- db_connect_ecotaxa()
projects <- tbl(db, 'projects') %>% filter(str_detect(title, 'uvp6.*sea002')) %>% select(projid,title) %>% collect() %>% arrange(projid)

samples <- tbl(db, 'objects') %>% 
  filter(projid %in% !!projects$projid) %>% 
  select(projid, sampleid, depth=depth_min) %>% 
  group_by(projid, sampleid) %>% 
  summarise(
    depth_min = min(depth, na.rm=TRUE),
    depth_max = max(depth, na.rm=TRUE),
  ) %>% 
  left_join(tbl(db, 'samples')) %>% 
  select(
    projid, sample=orig_id, 
    lat=latitude, lon=longitude,
    depth_min, depth_max,
    datetime=t18
  ) %>% 
  collect() %>% 
  mutate(
    datetime = ymd_hms(datetime), # convert datetime to datetime object
    yo = gsub("[^[:digit:]]", "",  sample) %>% as.numeric() # extract yo number from sample
  ) %>% 
  arrange(datetime) %>% 
  mutate(part='transit') %>% 
  left_join(projects)

db_disconnect_ecotaxa(db) # disconnect from database after request


## UI ----
#--------------------------------------------------------------------------#
ui <- fluidPage(
  
  # Application title
  titlePanel("Sea explorer transect cutter"),
  
  # Sidebar for project choice and transect cutting
  sidebarLayout(
    sidebarPanel(style = "overflow-y:scroll; max-height: 1000px; position:relative;",
                 # Select project
                 h2("Select project"),
                 selectInput("project", "", 
                             choices = projects$title),
                 
                 # Name of output file
                 h2("Output file name"),
                 textInput("file_name","",
                           value = ".csv"),
                 
                 # Cut transects
                 h2("Cut transects"),
                 # Portion 1
                 h3("Portion 1"),
                 selectInput("type1", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider1", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 # Portion 2
                 h3("Portion 2"),
                 selectInput("type2", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider2", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 # Portion 3
                 h3("Portion 3"),
                 selectInput("type3", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider3", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 
                 # Portion 4
                 h3("Portion 4"),
                 selectInput("type4", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider4", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 
                 # Portion 5
                 h3("Portion 5"),
                 selectInput("type5", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider5", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 
                 # Portion 6
                 h3("Portion 6"),
                 selectInput("type6", "Type", 
                             choices = list(
                               "Aller" = "aller", 
                               "Retour" = "retour",
                               "Transit" = "transit"
                             ), selected = "transit"),
                 
                 sliderInput("slider6", "Included yos",
                             min = 1, max = max(samples$yo), value = c(max(samples$yo),  max(samples$yo))),
                 
    ),
    
    mainPanel(
      tabsetPanel(#type = "tabs",
        # Panel to cut transects
        tabPanel("Cut transects",
                 
                 br(),
                 textOutput("selected_project"),
                 br(),
                 
                 fluidRow(
                   column(6, plotOutput("map_date")),  
                   column(6, plotOutput("map"))
                 ),
                 
                 plotOutput("bar_plot"),
                 
                 # Save output
                 actionButton("save", "Save transects"),
                 br(),
                 br(),
                 
                 # Display samples 
                 textOutput("aller"),
                 verbatimTextOutput("samples_aller"),
                 textOutput("retour"),
                 verbatimTextOutput("samples_retour"),
                 textOutput("transit"),
                 verbatimTextOutput("samples_transit"),
        ),
        
        # Panel to inspect saved files
        tabPanel("See saved transects",
                 h2("Select file"),
                 selectInput("saved_file", "", 
                             choices = saved_files,
                             selected = FALSE,
                             multiple = FALSE,
                             selectize = FALSE, 
                             size = 1 
                 ),
                 
                 br(),
                 br(),
                 
                 # Display saved samples 
                 textOutput("saved_aller"),
                 verbatimTextOutput("saved_samples_aller"),
                 textOutput("saved_retour"),
                 verbatimTextOutput("saved_samples_retour"),
                 textOutput("saved_transit"),
                 verbatimTextOutput("saved_samples_transit"),
                 
                 br(),
                 br(),
                 
                 # Button to download data
                 downloadLink('download', 'Download')
        ),
        
        # Panel to delete saved files
        tabPanel("Erase saved transects",
                 br(),
                 actionButton("erase", "Erase all saved transects", class = "btn-danger"),
        )
      )
    )
  )
)


## Server ----
#--------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  # Update data to match selected project
  # Downcasts
  df <- reactive(
    samples %>% 
      filter(title == input$project)
  )
  
  # Update output filename and sliders when selected project changes, erase samples
  observeEvent(input$project,{
    # Update output filename
    updateTextInput(session, 'file_name', value = paste0(input$project, ".csv"))
    # Update sliders
    updateSliderInput(session, 'slider1', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    updateSliderInput(session, 'slider2', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    updateSliderInput(session, 'slider3', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    updateSliderInput(session, 'slider4', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    updateSliderInput(session, 'slider5', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    updateSliderInput(session, 'slider6', min = 1, max = max(df()$yo), value = c(max(df()$yo),  max(df()$yo)))
    # Erase samples
    output$aller <- NULL
    output$samples_aller <- NULL
    output$retour <- NULL
    output$samples_retour <- NULL
    output$transit <- NULL
    output$samples_transit <- NULL
  })
  
  # Update transects
  df_parts <- reactive(
    samples %>% 
      filter(title == input$project) %>% 
      mutate(
        part = ifelse(between(yo, input$slider1[1], input$slider1[2]), input$type1, part),
        part = ifelse(between(yo, input$slider2[1], input$slider2[2]), input$type2, part),
        part = ifelse(between(yo, input$slider3[1], input$slider3[2]), input$type3, part),
        part = ifelse(between(yo, input$slider4[1], input$slider4[2]), input$type4, part),
        part = ifelse(between(yo, input$slider5[1], input$slider5[2]), input$type5, part),
        part = ifelse(between(yo, input$slider6[1], input$slider6[2]), input$type6, part)
      )
  )
  
  # Display name of selected project
  output$selected_project <- renderText({
    paste("You have selected project", input$project)
  })
  
  # Plot map with date
  output$map_date <- renderPlot({ 
    df() %>% 
      filter(str_detect(sample, 'd')) %>%  # keep only downcasts
      ggplot() +
      geom_polygon(data = coast, aes(x = lon, y = lat), fill = 'gray') +
      geom_point(aes(x = lon, y = lat, colour = datetime)) +
      scale_color_viridis_c(trans = time_trans(), option = "plasma") +
      theme_minimal() +
      scale_x_continuous(expand = c(0,0))  + scale_y_continuous(expand = c(0,0)) +
      labs(title = 'Mission map') +
      coord_quickmap() +
      theme(text = element_text(size=16))
  })
  
  # Plot map of transects
  output$map <- renderPlot({
    df_parts() %>% 
      filter(str_detect(sample, 'd')) %>% # keep only downcas      
      ggplot() +
      geom_polygon(data = coast, aes(x = lon, y = lat), fill = 'gray') +
      geom_point(aes(x = lon, y = lat, colour = part)) +
      scale_color_manual(values = cols) +
      theme_minimal() +
      scale_x_continuous(expand = c(0,0))  + scale_y_continuous(expand = c(0,0)) +
      labs(title = 'Mission map') +
      coord_quickmap() +
      theme(text = element_text(size=16))
  })
  
  # Bar plot of yo depth
  output$bar_plot <- renderPlot({
    df_parts() %>% 
      filter(str_detect(sample, 'd')) %>% # keep only downcasts    
      ggplot() +
      geom_col(aes(x = yo, y = -depth_max, fill = part), width=0.5) +
      theme_minimal() +
      scale_fill_manual(values = cols) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = expansion(add = c(20, 0))) +
      labs(y = 'max depth', title = 'Max depth of yos') +
      theme(text = element_text(size=16))
  })
  
  # Save and display samples when requested
  observeEvent(input$save,{
    
    # Read file name
    file_name <- input$file_name
    # Save samples
    transects <- df_parts() %>% 
      group_by(part) %>% # group by portion
      summarise(samples = paste(sample, collapse = ",")) %>% # paste samples with comma separation
      ungroup()
    transects %>% write_csv(file.path(output_dir, file_name)) 
    
    ## Display samples
    output$aller <- renderText("Samples aller")
    output$samples_aller <- renderText(transects %>% filter(part == "aller") %>% pull(samples))
    output$retour <- renderText("Samples retour")
    output$samples_retour <- renderText(transects %>% filter(part == "retour") %>% pull(samples))
    output$transit <- renderText("Samples transit")
    output$samples_transit <- renderText(transects %>% filter(part == "transit") %>% pull(samples))
    
    # Update list of saved files
    updateSelectInput(session, "saved_file", choices = list.files(output_dir, pattern = ".csv"))
  })
  
  # Display previously saved transects
  observeEvent(input$saved_file, {
    saved_transects <- read_csv(file.path(output_dir, input$saved_file))
    
    output$saved_aller <- renderText("Samples aller")
    output$saved_samples_aller <- renderText(saved_transects %>% filter(part == "aller") %>% pull(samples))
    output$saved_retour <- renderText("Samples retour")
    output$saved_samples_retour <- renderText(saved_transects %>% filter(part == "retour") %>% pull(samples))
    output$saved_transit <- renderText("Samples transit")
    output$saved_samples_transit <- renderText(saved_transects %>% filter(part == "transit") %>% pull(samples))
    
  })
  
  # Download data as a csv file
  output$download <- downloadHandler(
    filename = function(){input$saved_file},
    content = function(file){
      saved_transects <- read_csv(file.path(output_dir, input$saved_file))
      write_csv(saved_transects, file)
    }
  )
  
  # Erase csv files of saved transects
  observeEvent(input$erase, {
    files <- list.files(output_dir, pattern = ".csv", full.names = TRUE)
    do.call(file.remove, list(files))
    
    # Update list of saved files
    updateSelectInput(session, "saved_file", choices = list.files(output_dir, pattern = ".csv"))
    
    # Erase display of previously saved transects
    output$saved_aller <- renderText(NULL)
    output$saved_samples_aller <- renderText(NULL)
    output$saved_retour <- renderText(NULL)
    output$saved_samples_retour <- renderText(NULL)
    output$saved_transit <- renderText(NULL)
    output$saved_samples_transit <- renderText(NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
