library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(ggrepel)
library(here)
library(markdown)
library(plotly)
library(RefManageR)
library(pak)
library(hatchR)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag)

# Bring in the example crooked river data set

crooked.dat <- read_csv("data-raw/crooked_river_missing.csv")

# Make a link to the hatchR github page

link_gh <- tags$a(shiny::icon("github"), "hatchR Package", href = "https://bmait101.github.io/hatchR/index.html", target = "_blank")

# Make a link for the citation

link_citation <- tags$a(shiny::icon("link"),"hatchR Citation",href = "https://bmait101.github.io/hatchR/authors.html#citation", target="_blank")

# Make the bibtext object for hatchR

hatchr_bib <- BibEntry("Unpublished",
                       title="{hatchR}: predicting fish developmental phenology",
                       
                       author     = c(person(given  = "Bryan M.",
                                             family = "Maitland",
                                             email  = "bryan.maitland@usda.gov"),
                                      person(given  = "Morgan R.",
                                             family = "Sparks"),
                                      person(given  = "Eli",
                                             family = "Felts",
                                             email = "eli_felts@fws.gov")
                       ),
                       year       = "2024",
                       note       = "In preparation")


# make the UI

ui <- page_navbar(
  
  title="hatchR",
  
  theme = bs_theme(preset="cyborg"),
  
  nav_menu(title="Links",
           nav_item(link_gh,link_citation),
           align="left"),
  
  id="nav",
  
  sidebar = sidebar(width=500,
                    
                    conditionalPanel("input.nav===`Import Data`",
                                     
                                     # Put input controls in a separate accordion
                                     
                                     accordion(accordion_panel(
                                       
                                       "Input Data",
                                       
                                       # create a checkbox to be able to use example
                                       # data sets for demonstration purposes
                                       
                                       checkboxInput("demo_check","Check box to use Crooked River example data",
                                                     value=FALSE),
                                       
                                       # Create a place for user to input temp data in csv file
                                       
                                       conditionalPanel(
                                         condition="input.demo_check == false",
                                         
                                         fileInput("upload","Upload daily temperature data",
                                                   accept = ".csv")
                                         
                                       ),
                                       
                                       # Reactive UI element for users to identify which column
                                       # their date data are in
                                       
                                       uiOutput("date_column"),
                                       
                                       # Reactive UI element for users to identify which column
                                       # their date data are in
                                       
                                       uiOutput("temp_column"),
                                       
                                       selectInput("date.format","Date Format",
                                                   choices=c("2000-01-01T00:00:00Z",
                                                             "1/1/2000",
                                                             "2000-01-01"),
                                                   selected="1/1/2000"),
                                       
                                     )
                                     )),
                    
                    conditionalPanel("input.nav===`Model Phenology`",
                                     
                                     accordion(accordion_panel(
                                       
                                       "Model Specifications",
                                       
                                       # Option to select whether to use existing or custom models
                                       
                                       radioButtons(inputId="model_build",
                                                    label="Choose existing or custom model",
                                                    choices=c("Existing",
                                                              "Custom"),
                                                    selected="Existing"),
                                       
                                       # Drop down menu to select species
                                       
                                       uiOutput("existing_species"),
                                       
                                       # Menu to select author for model source; this menu
                                       # populates based on choices of species and development
                                       # stage, and the specifications are made within a
                                       # renderUI({}) function on the server side
                                       
                                       uiOutput("author_select"),
                                       
                                       # Menu to select model id, also reacts to
                                       # choice of species
                                       
                                       uiOutput("id_select"),
                                       
                                       # make a place to input user data for custom model
                                       
                                       uiOutput("custom_input"),
                                       
                                       # make a place for users to provide species
                                       
                                       uiOutput("custom_species_input"),
                                       
                                       # Make a place for users to select which phas
                                       # the custom model is for
                                       
                                       uiOutput("custom_stage_input"),
                                       
                                       # Reactive UI element for users to identify which column
                                       # their temp data are in
                                       
                                       uiOutput("custom_temp_column"),
                                       
                                       # Reactive UI element for users to identify which column
                                       # their days to end data are in
                                       
                                       uiOutput("custom_days_column"),
                                       
                                       # Menu to select spawn date; this menu
                                       # populates based on the date range
                                       # in the user provided csv; this will just
                                       # default to the latest date they provide
                                       
                                       uiOutput("spawn_date")
                                       
                                     ))),
                    
                    HTML('<img src="hatchrlogo2.png" width="50%" height="auto">')
                    
  ),
  
  # Things to put in the main part of the page (this is
  # now outside of the sidebar menu)
  #nav_spacer(),
  
  nav_panel("About",
            
            tags$iframe(src="shiny_overview.html",
                        width="100%",
                        height="100%")),
  
  nav_panel("Import Data",
            
            layout_columns(
              
              col_widths=c(6,6,12),
              
              card(card_header("Input Data"),
                   DTOutput("user.dat")),
              
              card(card_header("Missing Dates"),
                   DTOutput("missing_dates")),
              
              card(card_header("Plot Temperature Check"),
                   plotlyOutput("temp_plot"),
                   full_screen = TRUE)
              
            )
            
  ),
  
  nav_panel("Model Phenology",
            
            layout_columns(
              
              col_widths = c(4,8),
              
              card(card_header("Phenology Summaries"),
                   DTOutput("model"),
                   downloadButton("download_data",
                                  "Download model summary data"),
                   downloadButton("download_ef",
                                  "Download daily accumulation values"),
                   full_screen = TRUE),
              
              card(card_header("Timeline Plot"),
                   plotlyOutput("model_plot"),
                   downloadButton("download_plot",
                                  "Download a copy of timeline plot"),
                   full_screen = TRUE)
              
              
            )
            
  ),
  
  nav_panel("Authors and Citation",
            
            downloadButton("download_citation_bib",
                           "Download BibTex Citation"),
            
            tags$iframe(src="shiny_citation.html",
                        width="100%",
                        height="100%")
            
            
            
  )
  

  
)

