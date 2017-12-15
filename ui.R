library(leaflet)
library(shinyjs)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # Application title
  titlePanel('Stream classification in the American River Watershed'),
  
  fluidRow(
  
    column(width = 12, 
      h5('This application can be used to explore stream and site classifications for the American River Watershed.  Classications are based on the relationship of field CSCI scores at a site to biological expectations for the stream reach.  Expectations are based on ranges of predicted CSCI scores for a stream reach and user-defined parameters for CSCI tresholds and range cutoffs (tails).  The user may also choose the model used for predicting CSCI scores as the full (all predictors) or core (selectd predictors) model.  Site classifications for CSCI scores are defined as over-performing, expected, and under-performing or as one of twelve types within each stream reach expectation.  Stream reach expectations are defind as likely constrained, undetermined, or likely unconstrained.')
    ),
      
    column(width = 12, 
      
      # which site classification
      column(width = 3, 
            popify(
              selectInput(inputId  =  'typs',
                         label = h4('Site classifications:'),
                         choices = c('perf', 'type'), selected = 'perf'),
              title = NULL, 
              content = 'Pick the site classifications to display.  The "perf" classification shows sites as over-performing, expected, or under-performing.  The "type" classification shows sites as one of twelve types based on the stream reach expectation, tails on the expectation, and the CSCI threshold.',
              placement = 'right', 
              options=list(container = 'body')
            )
      ),           
      
      # select point radius
      column(width = 3,
             sliderInput("pt_sz", 
                         label = h4("Point size:"), 
                         min = 0, 
                         max = 15,
                         value = 4, 
                         step = 1
             )
      ),
      
      # select line size
      column(width = 3,
             sliderInput("ln_sz", 
                         label = h4("Line size:"), 
                         min = 0, 
                         max = 5,
                         value = 1, 
                         step = 0.1
             )
      )
      
    )
    
  ),
    
  tabsetPanel(selected = 'Estimated constraints',
    
    tabPanel('Score distributions',
             
      # select percentile        
      column(width = 4, 
             
        sliderInput('ptile',
                    label = h4("Percentile estimated score:"),
                    min = 0.05,
                    max = 0.95,
                    value = 0.5,
                    step = 0.05
        )
        
      ),
    
      # map output
      column(width = 12,
        
        leafletOutput('map', width = '100%', height = 550),
        h3()
             
      )
    
    ),
    
    tabPanel('Estimated constraints',
    
      column(width = 12,
            
        # select CSCI threshold       
        column(width = 4, 
               sliderInput('thrsh', 
                           label = h4("CSCI threshold:"), 
                           min = 0, 
                           max = 1.5,
                           value = 0.79, 
                           step = 0.01
               )
        ),
        
        # selected tails
        column(width = 4, 
               sliderInput('tails', 
                           label = h4("Expectation tails:"), 
                           min = 0.05, 
                           max = 0.45,
                           value = 0.05, 
                           step = 0.05
               )
        )
            
      ),       
             
      tabsetPanel(type = 'pills', 
        
        tabPanel('Map',
       
          # map output
          column(width = 12,
                
            leafletOutput('map_exp', width = '100%', height = 550), 
            h3()
                
          ) 
          
        ),
          
        tabPanel('Plot', 
               
          # plot output
          column(width = 12,
                
            plotOutput('plo_exp', width = '90%', height = 850)
                
          ) 
                 
        ), 
        
        tabPanel('Table', 
                 
          # table output
          column(width = 12, 
          
            DT::dataTableOutput('tab_sum'), 
            HTML('<p></p>')
                       
          )
          
        )
                  
      )
      
    )
    
  )
            
))
