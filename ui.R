library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # Application title
  h1('Stream classification in the American River Watershed', 
     style = "font-family: 'Volkhov';
     font-weight: 500; line-height: 1.1"),
  
  fluidRow(
    
    column(width = 2, img(src = "sccwrp_logo.jpg", width = '100px'), align = 'center', style = "margin-top: 0px;"),
    
    column(width = 10, 
           h5('This application can be used to explore stream and site classifications for the American River Watershed.  Classications are based on the relationship of field CSCI scores at a site to biological expectations for the stream reach.  Expectations are based on user-defined parameters for CSCI thresholds and confidence in the biological expectation. Site classifications for CSCI scores are defined as over-performing, expected, and under-performing.  Stream reach expectations are defined as likely constrained, undetermined, or likely unconstrained.')
    ),
    
    column(width = 12, 
           h4('Created by Marcus W. Beck,', a('marcusb@sccwrp.org', href = 'mailto:marcusb@sccwrp.org'), ", Raphael D. Mazor,", a('raphaelm@sccwrp.org', href = 'mailto:raphaelm@sccwrp.org'))
    )
    
  ),
  
  tabsetPanel(
    
    tabPanel('Maps',
             
             h5('These two maps show stream reach classifications by COMID and CSCI scores at monitoring stations.  The', strong('left map'), 'shows the predicted CSCI score for a COMID and measured CSCI score at a station from from field data.  The', strong('right map'), 'shows the CSCI score expectation for a COMID and the site classification (or performance) of a monitoring station.'),   
             
             column(width = 12, 
                    
                    h5('These sliders control the aesthetics in both maps. Use them to change the point/line sizes and apply a jitter for repeat visits as the same station.'), 
                    
                    # select point radius
                    column(width = 4,
                           sliderInput("pt_sz", 
                                       label = h6("Point size:"), 
                                       min = 0, 
                                       max = 15,
                                       value = 4, 
                                       step = 1, 
                                       width = '400px', 
                                       ticks = FALSE
                           )
                    ),
                    
                    # select line size
                    column(width = 4,
                           sliderInput("ln_sz", 
                                       label = h6("Line size:"), 
                                       min = 0, 
                                       max = 5,
                                       value = 1, 
                                       step = 0.1, 
                                       width = '400px', 
                                       ticks = FALSE
                           )
                    ),          
                    
                    column(width = 4,
                           
                           sliderInput('jitr', 
                                       label = h6("Jitter overlaps:"), 
                                       min = 0, 
                                       max = 500,
                                       value = 0, 
                                       step = 25, 
                                       width = '400px', 
                                       ticks = FALSE
                           )
                           
                    )
                    
             ),
             
             column(width = 6, 
                    
                    h5('These controls change the attributes in the',  strong('left map'), '. The first slider controls which percentile of predicted CSCI scores is shown for the stream reaches.  The toggle switch controls the observed CSCI scores shown at each sampling station.  The scores from field samples are shown when the switch is off and the differences between the observed scores and the reach predictions are shown when the switch is on.'),      
                    
                    # which csci percentile to show
                    sliderInput('ptile',
                                label = h6("Reach estimated score (percentile):"),
                                min = 0.05,
                                max = 0.95,
                                value = 0.5,
                                step = 0.05, 
                                width = '600px', 
                                ticks = FALSE
                    ),
                    
                    # show csci differences   
                    materialSwitch('difr', 
                                   label = h6('CSCI observed - predicted:'), 
                                   status = 'primary',
                                   right = F
                    )
                    
             ), 
             
             column(width = 6,
                    
                    h5('These controls change the attributes in the',  strong('right map'), '. The first slider controls the CSCI threshold and the second slider controls the certainty range of the predicted CSCI scores at each stream reach. Overlap of the certainty range with the CSCI threshold determines the expectation of a reach and performance of the CSCI score at a sampling station. See the plot tab for more.'),      
                    
                    # select CSCI threshold, master       
                    sliderInput('thrsh', 
                                label = h6("CSCI threshold:"), 
                                min = 0, 
                                max = 1.5,
                                value = 0.79, 
                                step = 0.01,
                                width = '600px', 
                                ticks = FALSE
                    ),
                    
                    # selected tails, master
                    sliderTextInput(
                      inputId = "tails", 
                      label = h6("Confidence range (+/-):"),  
                      grid = FALSE, 
                      force_edges = TRUE,
                      choices = c('More certain (0.45)', '0.40', '0.35', '0.30', '0.25', '0.20', '0.15', '0.10', 'Less certain (0.05)'), 
                      width = '600px'
                    )
                    
             ),
             
             # map output
             column(width = 6,
                    
                    leafletOutput('map', width = '100%', height = 550), 
                    h3()
                    
             ),
             
             # map output
             column(width = 6,
                    
                    leafletOutput('map_exp', width = '100%', height = 550), 
                    h3()
                    
             ) 
             
    ),
    
    tabPanel('Plot',
             
             h5('This plot shows the CSCI score expectations for every stream reach with CSCI sampling stations.  The CSCI threshold and confidence range define the reach expectation and the CSCI performance for the sampling stations.  Toggle the sliders to see how these change on the plot, including the maps and table in the other tabs.'),
             
             column(width = 4,
                    
                    # select CSCI threshold       
                    sliderInput('thrsh2', 
                                label = h6("CSCI threshold:"), 
                                min = 0, 
                                max = 1.5,
                                value = 0.79, 
                                step = 0.01, 
                                width = '400px', 
                                ticks = FALSE
                    )
                    
             ),   
             
             column(width = 4, 
                    
                    # selected tails
                    sliderTextInput(
                      inputId = "tails2", 
                      label = h6("Confidence range (+/-):"),  
                      grid = FALSE, 
                      force_edges = TRUE,
                      choices = c('More certain (0.45)', '0.40', '0.35', '0.30', '0.25', '0.20', '0.15', '0.10', 'Less certain (0.05)'), 
                      width = '400px'
                    )
                    
             ),
             
             column(width = 4,
                    
                    # order by site
                    materialSwitch('bysta', 
                                   label = h6('Order by site:'), 
                                   status = 'primary',
                                   right = F
                    )
                    
             ),
             
             # plot output
             column(width = 12,
                    
                    plotOutput('plo_exp', width = '90%', height = 850)
                    
             ) 
             
    ), 
    
    tabPanel('Table', 
             
             h5('This table summarizes the sampling station performance for CSCI scores shown in the maps and plot in the other tabs. The "types" are finer divisions that further categorize sites relative to the performance.'),
             
             column(width = 6,
                    
                    # select CSCI threshold       
                    sliderInput('thrsh3', 
                                label = h6("CSCI threshold:"), 
                                min = 0, 
                                max = 1.5,
                                value = 0.79, 
                                step = 0.01, 
                                width = '600px', 
                                ticks = FALSE
                    )
                    
             ),   
             
             column(width = 6, 
                    
                    # selected tails
                    sliderTextInput(
                      inputId = "tails3", 
                      label = h6("Confidence range (+/-):"),  
                      grid = FALSE, 
                      force_edges = TRUE,
                      choices = c('More certain (0.45)', '0.40', '0.35', '0.30', '0.25', '0.20', '0.15', '0.10', 'Less certain (0.05)'), 
                      width = '600px'
                    )
                    
             ),
             
             # table output
             column(width = 12, 
                    
                    DT::dataTableOutput('tab_sum'), 
                    HTML('<p></p>')
                    
             )
             
    )
    
  )
  
  ))


