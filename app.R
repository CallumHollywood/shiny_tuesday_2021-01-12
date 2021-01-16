

library(shiny)
library(shinythemes)
library(shinyLP)


library(tidyverse)
library(networkD3)
library(DT)
library(tidyquant)

# Setup
#------------------------------#
#------------------------------#

artwork_artist_in <- read_csv("scripts/data/artwork_artist_to_file.csv") %>% 
  rownames_to_column('id')


# App
#------------------------------#
#------------------------------#

ui <- fluidPage(
  title = "Shiny Tuesday 20-01-12",
  theme = "spacelab",
  fluidRow(
    column(width = 12,
           align = 'center',
           h2("Art Acquisitions - A Gender Review Tool")       
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      uiOutput("outpt_slt_decade")
    ),
    mainPanel(
      fluidRow(
        column(width = 6,
               align  = 'center',
               h5('Country Network'),
               simpleNetworkOutput('outpt_artwork_artist_in_plt', height = '250')         
        ),
        column(width = 6,
               align  = 'center',
               h5('Country Focus'),
               DT::dataTableOutput('outpt_country_fks')
        )
      ),
      hr(),
      fluidRow(
        column(width = 6,
               plotOutput('outpt_aquisition_over_time')        
        ),
        column(width = 6,
               verbatimTextOutput('outpt_artist_fk'),
               verbatimTextOutput('outpt_artists_ttldecade_male'),
               verbatimTextOutput('outpt_artists_ttldecade_female')
        )
        
      )
    )
  )
  
)

server <- function(input, output, session) {
  

  #### <<<<   REACTIVES        >>>>  ####
  #-------------------------------------#
  
  artwork_artist_in_rctv <- reactive({
    
    req(input$slt_decade)
    
    artwork_artist_in %>% 
      filter(decade == input$slt_decade)
    
  })
  
  artist_country_fks_rctv <- reactive({
    
    req(input$slt_country)
    
    artwork_artist_in_rctv() %>% 
      filter(country == input$slt_country)
    
  })
  

  #### <<<<   OBSERVE EVENTS   >>>>  ####
  #-------------------------------------#
  
  observeEvent(input$slt_decade,{
    
    updt_country_chcs <- artwork_artist_in %>% 
      filter(decade == input$slt_decade) %>% 
      select(country) %>% 
      arrange(country) %>% 
      distinct() %>%
      pull()
    
    updateSelectInput(session,
                      'slt_country',
                      'Select Country',
                      choices = updt_country_chcs,
                      selected = updt_country_chcs[1]
    )
    
  })
  
  
  #### <<<<    OUTPUTS         >>>>  ####
  #-------------------------------------#
  

  output$outpt_artist_fk <- renderPrint({
    
    if(is.null(input$outpt_country_fks_rows_selected))
      return(NULL)
    
    n_row_artist_focus <- artist_focus_rctv() %>% 
      select(acquisition_year) %>% 
      nrow()
    
    paste0("Number of Artist Acquisitions - ", n_row_artist_focus)
    
  })
  
  output$outpt_artists_ttldecade_male <- renderPrint({
    
    if(is.null(input$outpt_country_fks_rows_selected))
      return(NULL)
    
    decade_males <- artwork_artist_in %>% 
      filter(decade == input$slt_decade) %>%
      # filter(decade == 1820) %>% 
      filter(gender == 'Male') %>% 
      nrow()
    
    paste0("Total Acquisitions in Decade (Male) - ", decade_males)
    
  })
  
  output$outpt_artists_ttldecade_female <- renderPrint({
    
    if(is.null(input$outpt_country_fks_rows_selected))
      return(NULL)
    
    decade_males <- artwork_artist_in %>% 
      filter(decade == input$slt_decade) %>%
      filter(gender == 'Female') %>% 
      nrow()
    
    paste0("Total Acquisitions in Decade (Female) - ", decade_males)
    
  })
  
  
  artist_focus_rctv <- reactive({
    
    if(is.null(input$outpt_country_fks_rows_selected))
      return(NULL)
    
    country_fks_rctv() %>%
      slice(input$outpt_country_fks_rows_selected) %>% 
      select(artist_id) %>% 
      inner_join(artwork_artist_in, by = 'artist_id')

  })
  
  
  artist_cnt_rctv <- reactive({
    
    artist_focus_rctv() %>% 
      filter(!is.na(gender)) %>%
      group_by(acquisition_year, gender) %>%
      count()
    
  })
  
  artwork_artist_cnt_rctv <- reactive({
    
    artwork_artist_in %>% 
      filter(!is.na(gender)) %>% 
      group_by(acquisition_year, gender) %>% 
      count() 
    
  })
  
  output$outpt_aquisition_over_time <- renderPlot({
    
    artwork_artist_cnt_rctv() %>% 
      ggplot(aes(acquisition_year, n, color = gender)) +
      geom_point(alpha = 0.6, size = 1.2, show.legend = F) +
      geom_line(alpha = 0.3, show.legend = F) +
      
      {if(!is.null(input$outpt_country_fks_rows_selected))geom_point(
        size = 5,
        shape = 5,
        colour = "darkgreen",
        data = artist_cnt_rctv())}+ 
      
      facet_wrap(~gender, nrow = 2) +
      scale_y_log10() +
      theme_tq() +
      labs(
        title = 'Number of Annual Acquisitions',
        x = 'Year',
        y = 'Count'
      )
    
  })
  
  
  country_fks_rctv <- reactive({
    
    artist_country_fks_rctv() %>% 
      select(id) %>% 
      inner_join(artwork_artist_in, by = 'id')
    
  })
  
  
  output$outpt_country_fks <- DT::renderDataTable({
    
    x <-  country_fks_rctv() %>% 
      select(gender, artist, medium)
    
    DT::datatable(
      x,
      rownames = F,
      selection = 'single',
      options = list(pageLength = 3, lengthChange = FALSE, dom = 'p')
    ) %>% 
      formatStyle(names(x))
  })
  
  
  output$outpt_artwork_artist_in_plt <- renderSimpleNetwork({
    
    src    <- artwork_artist_in_rctv()$gender
    target <- artwork_artist_in_rctv()$country
    
    networkData <- data.frame(src, target)
    
    simpleNetwork(
      networkData,
      fontSize = 12,
      linkColour = "blue",
      nodeColour = "red",
      zoom = T,
      width = 200,
      height = 400,
      charge = -300
    )
    
  })
  
  output$outpt_artwork_artist_in_rctv <- renderPrint({
    
    artwork_artist_in_rctv()
    
  })
  
  
  output$outpt_slt_decade <- renderUI({
    
    decade_chcs <- artwork_artist_in %>% select(decade) %>% arrange(decade) %>% distinct() %>% pull()
    
    country_chcs <- artwork_artist_in %>% filter(decade == 1820) %>% select(country) %>% arrange(country) %>% distinct() %>% pull()
    
    tagList(
      selectInput("slt_decade", "Select Decade",
                  choices = decade_chcs,
                  selected = decade_chcs[1]
      ),
      selectInput('slt_country', 'Select Country',
                  choices = country_chcs,
                  selected = 'United Kingdom'
      )
    )
    
  })
  
}

shinyApp(ui, server)




