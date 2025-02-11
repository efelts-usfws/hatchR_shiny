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
library(rbibutils)
library(downloadthis)
library(RefManageR)


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

# Make the server

server <- function(input,output,session){
  
  # Make a reactive selectInput for identifying the date column
  
  output$date_column <- renderUI({
    
    
    file <- input$upload
    
    ext <- tools::file_ext(file$datapath)
    
    data <- read_csv(file$datapath)
    
    req(input$demo_check)
    
    
    if(input$demo_check==FALSE)
      
      return(
        
        selectInput(inputId = "date.column",
                    label="Identify which column dates are in",
                    choices=colnames(data))
      )
    
  })
  
  # Make a reactive selectInput for identifying the temp column
  
  output$temp_column <- renderUI({
    
    # req(input$upload)
    
    file <- input$upload
    
    ext <- tools::file_ext(file$datapath)
    
    data <- read_csv(file$datapath)
    
    req(input$demo_check)
    
    if(input$demo_check==FALSE)
      
      return(
        
        selectInput(inputId = "temp.column",
                    label="Identify which column temperatures are in",
                    choices=colnames(data))
        
      )
    
  })
  
  # make the user input data reactive, that way it will
  # go into everything else on the server side
  
  data_reactive <- reactive({
    
    # req(input$date.format)
    # req(input$upload)
    # req(input$date.column)
    # req(input$temp.column)
    
    req(input$demo_check)
    
    file <- input$upload
    
    ext <- tools::file_ext(file$datapath)
    
    # req(file)
    
    if(input$date.format == "2000-01-01T00:00:00Z"&&
       input$demo_check==FALSE)
      
      return(
        
        read_csv(file$datapath) %>%
          rename(date=input$date.column,
                 temperature=input$temp.column) %>%
          mutate(date=as.character(date)) %>%
          mutate(date=as_date(date)) %>%
          group_by(date) %>%
          summarize(daily_temp=mean(temperature))%>%
          filter(!is.na(date))
        
      )
    #
    # if(input$date.format == "1/1/2000"&
    #    input$demo_check==FALSE)
    #
    #   return(
    #
    #     read_csv(file$datapath) %>%
    #       rename(date=input$date.column,
    #              temperature=input$temp.column) %>%
    #       mutate(date=mdy(date)) %>%
    #       group_by(date) %>%
    #       summarize(daily_temp=mean(temperature)) %>%
    #       filter(!is.na(date))
    #
    #   )
    #
    # if(input$date.format == "2000-01-01"&
    #    input$demo_check==FALSE)
    #
    #   return(
    #
    #     read_csv(file$datapath) %>%
    #       rename(date=input$date.column,
    #              temperature=input$temp.column) %>%
    #       mutate(date=as.character(date)) %>%
    #       mutate(date=as_date(date)) %>%
    #       group_by(date) %>%
    #       summarize(daily_temp=mean(temperature))%>%
    #       filter(!is.na(date))
    #
    #   )
    
    
    if(input$demo_check==TRUE)
      
      return(
        
        crooked.dat %>%
          rename(date=SampleDate,
                 temperature=temperature) %>%
          mutate(date=as.character(date)) %>%
          mutate(date=as_date(date)) %>%
          group_by(date) %>%
          summarize(daily_temp=mean(temperature))%>%
          filter(!is.na(date))
        
      )
    
    
  })
  
  # return the user provided csv file in a DataTable output
  
  output$user.dat <- renderDT({
    
    withProgress(message="Making Table",data_reactive())
    
    data_reactive()
    
  })
  
  # make and object that checks for missing dates reactively
  
  
  missing_reactive <- reactive({
    
    dat <- data_reactive()
    
    missing.date <- dat %>%
      summarize(earliest=as_date(min(date,na.rm=T)),
                latest=as_date(max(date,na.rm=T))) %>%
      mutate(date_series=map2(earliest,latest,seq.Date,by="day")) %>%
      unnest(cols=c(date_series)) %>%
      select(date_series) %>%
      anti_join(dat,by=c("date_series"="date")) %>%
      mutate(missing_dates=as.POSIXct(date_series)) %>%
      select(missing_dates)
    
  })
  
  output$missing_dates <- renderDT({
    
    
    dat <- missing_reactive()
    
    
  })
  
  # Make the plot check temp in my code in a reactive
  
  temp_check_reactive <- reactive({
    
    # req(input$upload)
    # req(input$date.format)
    
    dat <- data_reactive() %>%
      mutate(group=1)
    
    missing.date <- missing_reactive()
    
    check.plot <- ggplot()+
      geom_line(data=dat,aes(x=date,y=daily_temp,group=group,
                             text=str_c("Date:", date,
                                        "<br>","Temperature",round(daily_temp,1),
                                        sep=" ")),
                linewidth=0.5)+
      geom_hline(yintercept = 0,linetype="dashed",color="dodgerblue")+
      geom_hline(yintercept = 25,linetype="dashed",color="red")+
      labs(title="Temperature Check",x="Date",y="Temperature")+
      theme_classic()
    
    
    
    
  })
  
  # Run the plot_check_temp function
  
  output$temp_plot <- renderPlotly({
    
    # req(input$upload)
    # req(input$date.format)
    #
    # file <- input$upload
    #
    # ext <- tools::file_ext(file$datapath)
    #
    # req(file)
    #
    # dat <- data_reactive()
    #
    # plot1 <- plot_check_temp(data=dat, dates=date,
    #                          temperature=daily_temp)
    #
    # ggplotly(plot1)
    
    plot1 <- temp_check_reactive()
    
    ggplotly(plot1, tooltip=c("text"))
    
  })
  
  # Make a reactive selectInput for selecting species, that
  # will only appear if choosing to use existing models
  
  output$existing_species <- renderUI({
    
    req(input$model_build)
    
    if(input$model_build=="Existing")
      
      return(
        selectInput("model_species","Species",
                    choices=unique(model_table$species),
                    selected="sockeye")
      )
  })
  
  # Make the model table reactive because the reactive
  # filters on it will be used to populate reactive UI
  # elements such as author
  
  model_tbl.reactive <- reactive({
    
    req(input$model_species)
    
    models <- model_table %>%
      filter(species==input$model_species)
    
  })
  
  # Construct the author select UI that reacts
  # to filters of species and model type
  
  output$author_select <- renderUI({
    
    req(input$model_build)
    
    models <- model_tbl.reactive()
    
    if(input$model_build=="Existing")
      
      return(
        selectInput("model_author","Choose Author",
                    choices=unique(models$author))
      )
    
  })
  
  # Construct the id select UI that reacts
  # to filters of species and model type
  
  output$id_select <- renderUI({
    
    req(input$model_author)
    req(input$model_species)
    req(input$model_build)
    
    models <- model_tbl.reactive() %>%
      filter(species==input$model_species,
             author==input$model_author)
    
    if(input$model_build=="Existing")
      
      return(
        selectInput("model_id","Choose Model ID",
                    choices=unique(models$model_id))
      )
  })
  
  # return a place for a file input if they
  # select custom model
  
  output$custom_input <- renderUI({
    
    req(input$model_build)
    
    if(input$model_build=="Custom")
      
      return(
        
        fileInput("custom_upload","Upload data to build custom model",
                  accept = ".csv")
        
      )
    
  })
  
  # return a place for user to identify species they're building
  # a custom model for
  
  output$custom_species_input <- renderUI({
    
    req(input$model_build)
    
    
    if(input$model_build=="Custom")
      
      return(
        
        textInput("custom_species","Custom Model Species")
        
      )
    
  })
  
  # return a place for user to identify which stage
  # they're model is for
  
  output$custom_stage_input <- renderUI({
    
    req(input$model_build)
    
    
    if(input$model_build=="Custom")
      
      return(
        
        selectInput("custom_stage","Custom Model Development Stage",
                    choices = c("hatch","emerge"),
                    selected="hatch")
        
      )
    
  })
  
  # Make a reactive selectInput for identifying the custom model
  # temperature column
  
  output$custom_temp_column <- renderUI({
    
    req(input$model_build)
    req(input$custom_upload)
    
    file <- input$custom_upload
    
    ext <- tools::file_ext(file$datapath)
    
    data <- read_csv(file$datapath)
    
    if(input$model_build == "Custom")
      
      return(
        
        selectInput(inputId = "customtemp.column",
                    label="Identify which column temperature is in",
                    choices=colnames(data))
        
      )
    
  })
  
  # Make a reactive selectInput for identifying the custom model
  # days to end column
  
  output$custom_days_column <- renderUI({
    
    req(input$model_build)
    req(input$custom_upload)
    
    file <- input$custom_upload
    
    ext <- tools::file_ext(file$datapath)
    
    data <- read_csv(file$datapath)
    
    if(input$model_build == "Custom")
      
      return(
        
        selectInput(inputId = "customdays.column",
                    label="Identify which column days are in",
                    choices=colnames(data))
        
      )
    
  })
  
  # Construct the spawn date select UI that
  # reacts to the user input csv
  
  output$spawn_date <- renderUI({
    
    req(input$upload)
    
    user_dat <- data_reactive()
    
    airDatepickerInput(inputId = "spawn_date",
                       label="Choose Spawn Date(s)",
                       value=NULL,
                       multiple = T,
                       clearButton = T,
                       minDate = min(user_dat$date),
                       maxDate = max(user_dat$date))
    
  })
  
  # make an option for selecting existing models
  
  model_existing <- reactive({
    
    req(input$model_species)
    req(input$model_author)
    req(input$model_id)
    
    model_table <-  model_tbl.reactive()
    
    model.options <- model_table %>%
      filter(author==input$model_author,
             species==input$model_species,
             model_id==input$model_id) %>%
      pull(development_type)
    
    map(model.options,
        model_select,
        author=input$model_author,
        species=input$model_species,
        model_id=input$model_id)
    
  })
  
  # make an option for fitting custom models
  
  model_custom <- reactive({
    
    # req(input$custom_upload)
    # req(input$customtemp.column)
    # req(input$customdays.column)
    req(input$custom_species)
    req(input$custom_stage)
    
    file <- input$custom_upload
    
    ext <- tools::file_ext(file$datapath)
    
    dat <- read_csv(file$datapath) %>%
      rename(temperature=input$customtemp.column,
             days_to_end=input$customdays.column)
    
    fit_mod <- fit_model(temp=dat$temperature,
                         days=dat$days_to_end,
                         species=input$custom_species,
                         development_type=input$custom_stage)
    
    fit_df <- tibble(author="custom",
                     species=fit_mod$expression$species,
                     model_id="custom",
                     development_type=fit_mod$expression$development_type,
                     expression=fit_mod$expression$expression)
    
    fit_output <- list(fit_df)
    
  })
  
  # Make the model selection react to user input;
  # will use custom if selected vs. existing
  # if selected
  
  model_reactive <- reactive({
    
    req(input$model_build)
    
    if(input$model_build=="Existing")
      return(model_existing())
    
    if(input$model_build=="Custom")
      return(model_custom())
    
  })
  
  # Run the model reactively
  
  eval_reactive <- reactive({
    
    req(input$upload)
    req(input$spawn_date)
    # req(input$model_species)
    # req(input$model_author)
    # req(input$model_id)
    
    selected_models <- model_reactive()
    model.dat <- data_reactive()
    spawn.date_value <- as.character(input$spawn_date)
    
    # set up a variable grid for the combinations
    # of selected dates to pair with emergence
    # and hatch
    
    var_grid <- expand_grid(model=selected_models,
                            spawn.date=spawn.date_value)
    
    # map the variable grid to the data using
    # the predict_phenology function
    
    dat <- pmap(var_grid,
                predict_phenology,
                data=model.dat,
                dates=date,
                temperature=daily_temp)
    
  })
  
  # Take the model results output list and make
  # a summary table
  
  summary_reactive <- reactive({
    
    model_output <- eval_reactive()
    
    summary1 <- model_output %>%
      map("days_to_develop") %>%
      unlist()
    
    summary2 <- model_output %>%
      map("dev.period") %>%
      bind_rows()
    
    summary3 <- model_output %>%
      map("model_specs") %>%
      bind_rows()
    
    # pull out the dev types included in the model specs,
    # because the way the outputs are generated will depend
    # on that
    
    dev.types <- summary3 %>%
      distinct(development_type) %>%
      pull(development_type)
    
    if(all(c("hatch","emerge") %in% dev.types))
      
      return(
        
        summary.df <- tibble(spawn_date=summary2$start,
                             end_date=summary2$stop,
                             days_to_end=summary1,
                             dev.type=summary3$development_type,
                             species=first(summary3$species),
                             author=first(summary3$author),
                             model=first(summary3$model_id),
                             hatch_func=first(summary3$expression),
                             emerge_func=last(summary3$expression)) %>%
          pivot_wider(names_from = dev.type,
                      values_from = c(days_to_end,end_date)) %>%
          select(species,author,model,hatch_func,emerge_func,
                 spawn_date,hatch_date=end_date_hatch,
                 days_to_hatch=days_to_end_hatch,
                 emerge_date=end_date_emerge,
                 days_to_emerge=days_to_end_emerge) %>%
          mutate(message=ifelse(!is.na(hatch_date)&
                                  is.na(emerge_date),
                                "Only hatch achieved, did fish spawn too close to the end of your data?",
                                ifelse(is.na(hatch_date),
                                       "Did not develop, did fish spawn too close to the end of your data?",
                                       "Both phases achieved")))
      )
    
    if(dev.types=="hatch")
      
      return(
        
        summary.df <- tibble(spawn_date=summary2$start,
                             end_date=summary2$stop,
                             days_to_end=summary1,
                             dev.type=summary3$development_type,
                             species=first(summary3$species),
                             author=first(summary3$author),
                             model=first(summary3$model_id),
                             hatch_func=first(summary3$expression),
                             emerge_func=as.character(NA)) %>%
          pivot_wider(names_from = dev.type,
                      values_from = c(days_to_end,end_date)) %>%
          mutate(emerge_date=as_date(NA),
                 days_to_emerge=as.integer(NA)) %>%
          select(species,author,model,hatch_func,emerge_func,
                 spawn_date,hatch_date=end_date_hatch,
                 days_to_hatch=days_to_end_hatch,
                 emerge_date,days_to_emerge) %>%
          mutate(message=ifelse(!is.na(hatch_date)&
                                  is.na(emerge_date),
                                "Only hatch achieved, did fish spawn too close to the end of your data?",
                                ifelse(is.na(hatch_date),
                                       "Did not develop, did fish spawn too close to the end of your data?",
                                       "Both phases achieved")))
        
        
        
      )
    
    if(dev.types=="emerge")
      
      return(
        
        summary.df <- tibble(spawn_date=summary2$start,
                             end_date=summary2$stop,
                             days_to_end=summary1,
                             dev.type=summary3$development_type,
                             species=first(summary3$species),
                             author=first(summary3$author),
                             model=first(summary3$model_id),
                             hatch_func=as.character(NA),
                             emerge_func=first(summary3$expression)) %>%
          pivot_wider(names_from = dev.type,
                      values_from = c(days_to_end,end_date)) %>%
          mutate(hatch_date=as_date(NA),
                 days_to_hatch=as.integer(NA)) %>%
          select(species,author,model,hatch_func,emerge_func,
                 spawn_date,hatch_date,
                 days_to_hatch,
                 emerge_date=end_date_emerge,
                 days_to_emerge=days_to_end_emerge) %>%
          mutate(message="Both phases achieved, only emerge date estimated.")
        
        
        
      )
    
    
  })
  
  # Render a DataTable output from the model runs
  
  output$model <- renderDT({
    
    dat <- summary_reactive()
    
    dat %>%
      select(spawn_date,hatch_date,days_to_hatch,
             emerge_date,days_to_emerge,message)
    
  })
  
  # Take the model results for daily accumulation
  # and put them into a table
  
  daily_reactive <- reactive({
    
    model_output <- eval_reactive()
    
    daily1 <- model_output %>%
      map("ef_table") %>%
      bind_rows(.id="id")
    
    daily2 <- model_output %>%
      map("model_specs") %>%
      bind_rows(.id="id")
    
    daily_output <- daily1 %>%
      left_join(daily2,by="id")
    
  })
  
  # output$daily <- renderDT({
  #
  #   dat <- daily_reactive()
  #
  # })
  
  # Generate the plot output in a reactive object; it's in
  # a reactive rather than just renderPlot so that the
  # reactive object can be passed into the download handler
  
  reactive_plot <- reactive({
    
    # get starting df of model outputs
    
    plot_dat <- summary_reactive()
    
    # get df of temp data that were originally the inputs
    
    temp.plot <- data_reactive() %>%
      mutate(group=1)
    
    # some reorganizing to be able to
    # make a timeline plot
    
    plot.join1 <- plot_dat  %>%
      arrange(spawn_date) %>%
      ungroup() %>%
      mutate(model_run=row_number()) %>%
      select(model_run,spawn_date,hatch_date,emerge_date,
             days_to_hatch,days_to_emerge)
    
    plot.join2 <- plot.join1  %>%
      mutate(brood_year=str_c("Brood Year", year(spawn_date),sep=" ")) %>%
      pivot_longer(cols=spawn_date:emerge_date,
                   names_to="what",values_to="when")
    
    # the second data set "type" in this plot
    # will id periods between various times of interest,
    # e.g. spawn-hatch is "Egg" stage and
    # hatch-emergence is "Alevin" stage
    
    phen.period <- plot.join2 %>%
      mutate(start=lag(when),
             end=when) %>%
      filter(!what=="spawn_date") %>%
      mutate(phase=ifelse(what=="hatch_date","Egg",
                          "Alevin")) %>%
      mutate(phase=factor(phase,
                          levels=c("Egg","Alevin")))%>%
      left_join(plot.join1,by=c("model_run","days_to_hatch",
                                "days_to_emerge")) %>%
      mutate(spawn_date2=as.character(spawn_date),
             hatch_date2=as.character(hatch_date),
             days_to_hatch2=as.character(days_to_hatch),
             emerge_date2=as.character(emerge_date),
             days_to_emerge2=as.character(days_to_emerge)) %>%
      mutate(hatch_date2=replace_na(hatch_date2,"NA"),
             days_to_hatch2=replace_na(days_to_hatch2,"NA"),
             emerge_date2=replace_na(emerge_date2,"NA"),
             days_to_emerge2=replace_na(days_to_emerge2,"NA"))
    
    phen_by.limits <- phen.period %>%
      group_by(brood_year) %>%
      summarize(earliest=min(spawn_date)-days(5),
                latest=max(end,na.rm=T)+ days (30)) %>%
      group_by(brood_year) %>%
      mutate(date_series=map2(earliest,latest,seq.Date,by="day")) %>%
      unnest(cols=c(date_series)) %>%
      select(brood_year,date=date_series)
    
    temp.limited <- temp.plot %>%
      inner_join(phen_by.limits,by="date")
    
    
    
    plot_output <- ggplot() +
      geom_segment(data=phen.period,
                   aes(x=start,y=model_run,
                       xend=end,yend=model_run,
                       text=str_c(" Spawn Date: ",spawn_date2,
                                  "<br>","Hatch Date:",hatch_date2,
                                  "<br>","Days to Hatch:",days_to_hatch2,
                                  "<br>","Emerge Date:",emerge_date2,
                                  "<br>","Days to Emerge:",days_to_emerge2,sep=" "),
                       color=phase),linewidth=2)+
      geom_line(data=temp.limited,
                aes(x=date,y=daily_temp,group=group,
                    text=str_c("Date:",date,
                               "<br>","Temperature:",round(daily_temp,1),sep=" ")))+
      scale_color_manual(values=c("blue",
                                  "red"))+
      theme_bw()+
      facet_wrap(~brood_year,scales="free_x",
                 ncol=1)+
      labs(x="Date",y="Temperature (C)",
           color="")
    
    
    
    
    
    
  })
  
  # render plot for display in the shiny app
  
  output$model_plot <- renderPlotly({
    
    start <- reactive_plot()
    
    ggplotly(start, tooltip=c("text","label"))
    
  })
  
  # Put data outputs into a csv to go into the download handler
  
  output$download_data <- downloadHandler(
    
    filename=function(){
      paste("hatchR_modelrun",as.integer(Sys.time()),".csv",sep="")
    },
    content=function(file){
      req(summary_reactive())
      write.csv(summary_reactive(),file,row.names=F)
    })
  
  # Put raw daily accumulation outputs into a csv to go into the download handler
  
  output$download_ef <- downloadHandler(
    
    filename=function(){
      paste("hatchR_modelrun_dailyvalues",as.integer(Sys.time()),".csv",sep="")
    },
    content=function(file){
      req(daily_reactive())
      write.csv(daily_reactive(),file,row.names=F)
    })
  
  
  
  # Put the plot into a png to go into the download handler
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste("hatchR_plot_",as.integer(Sys.time()),".png",sep="")
    },
    content=function(file){
      req(reactive_plot())
      ggsave(file,plot=reactive_plot(),
             width=6.5,height=7.5,units=c("in"),device="png")
    })
  
  output$download_citation_bib <- downloadHandler(
    
    filename = function(){
      paste("hatchR_citation",".bib",sep="")
    },
    content=function(file){
      WriteBib(hatchr_bib,file,biblatex=TRUE)
    }
    
    
  )
  
}

shinyApp(ui,server)


