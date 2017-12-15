library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(scales)
source('R/funcs.R')

# spatial comid data
load('data/spat.RData')

# csci scores at sites
load('data/scrs.RData')

# watersheds
load('data/shed.RData')

# color domain
dmn <- spat %>% 
  select(matches('^core0')) %>% 
  data.frame %>% 
  select(-geometry) %>% 
  gather('var', 'val') %>% 
  .$val %>% 
  na.omit %>% 
  c(., scrs$csci)

# color palette
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),
  na.color = 'yellow',
  domain = dmn)

# color palette for stream expectations
pal_exp <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Set1')[c(1, 2, 3)],
  na.color = 'yellow',
  domain = c('likely constrained', 'undetermined', 'likely unconstrained'))

# color palette for CSCI performance
pal_prf <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Greys')[c(1, 7, 4)],#c('white', 'blue', 'red'),
  na.color = 'yellow',
  domain = c('expected', 'over performing', 'under performing'))

# color palette for CSCI type
pal_typ <- colorFactor(
  palette = hue_pal()(100), 
  na.color = 'yellow',
  domain = paste0('Type', str_pad(seq(1:12), 2, pad = '0'))
)

# server logic
server <- function(input, output) {
  
  # data to plot, polylines with score expections
  dat <- reactive({
    
    ptile <- input$ptile 
    
    # get polylines to plot
    ptile <- ptile %>% 
      format(nsmall = 2) %>% 
      paste0('core', .) %>% 
      gsub('\\.', '.', .)
    names(spat)[names(spat) %in% ptile] <- 'lns'
    
    # set zero values to NA
    out <- spat 
    out
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # inputs
    thrsh <- input$thrsh
    tails <- input$tails %>% as.numeric
    
    # get biological condition expectations
    cls <- getcls2(spat, thrsh = thrsh, tails = tails, modls = 'core')
    
    # join with spatial data
    out <- spat %>% 
      left_join(cls, by = 'COMID')
    
    out
    
  })
  
  # CSCI scores and stream condition expectations
  scr_exp <- reactive({
    
    # inputs
    thrsh <- input$thrsh
    tails <- input$tails %>% as.numeric
    
    # process
    incl <- site_exp(spat, scrs, thrsh = thrsh, tails = tails, modls = 'core')
    
    return(incl)
    
  })
  
  # non-reactive base map
  output$map <- renderLeaflet(
    
    leaflet(shed) %>%
      addPolygons(data = shed, opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white') %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  # non-reactive base map, condition expectations
  output$map_exp <- renderLeaflet(
    
    leaflet(shed) %>%
      addPolygons(data = shed, opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white') %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  # reactive maps
  observe({
    
    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    typs <- input$typs
    dat <- dat()
    dat_exp <- dat_exp()
    scr_exp <- scr_exp()
    
    # score expectations
    leafletProxy("map", data = dat) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = shed, opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white') %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal(lns), 
                   label = ~paste('Likely score:', as.character(round(lns, 2)))
      ) %>%
      addLegend("topright", pal = pal, values = ~lns,
                title = "Likely score",
                opacity = 1
      ) %>% 
      addCircleMarkers(data = scrs, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.5, fillOpacity = 0.8, 
                       label = ~paste('CSCI:', as.character(round(csci, 2))),
                       fillColor = ~pal(csci), color = 'black'
      ) 
  })
  
  observe({

    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    typs <- input$typs
    dat <- dat()
    dat_exp <- dat_exp()
    scr_exp <- scr_exp()
    
    # condition expectations
    exp_bs <- leafletProxy("map_exp", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls()%>% 
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Expected classification",
                opacity = 1
      ) %>% 
      addPolygons(data = shed, opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white') %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste('Stream class:', strcls)
      )
    
    # conditions expections as site performance
    if(typs == 'perf'){
      
      exp_bs %>% 
        addCircleMarkers(data = scr_exp, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.5, fillOpacity = 0.9, 
                         label = ~paste0('CSCI: ', as.character(round(csci, 2)), ', ', perf),
                         fillColor = ~pal_prf(perf), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_prf, values = scr_exp$perf,
                  title = "CSCI performance",
                  opacity = 1
        )
      
      # condition expectations as site types    
    } else {
      
      exp_bs %>% 
        addCircleMarkers(data = scr_exp, lng = ~long, lat = ~lat, radius = ptsz, weight = 1, fillOpacity = 0.9, 
                         label = ~paste0('CSCI: ', as.character(round(csci, 2)), ', ', typelv),
                         fillColor = ~pal_typ(typelv), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_typ, values = scr_exp$typelv,
                  title = "CSCI score type",
                  opacity = 1
        )
      
    }
    
  })
  
  # plot of csci scores and expectations by station code
  output$plo_exp <- renderPlot({
    
    thrsh <- input$thrsh
    typs <- input$typs
    
    # CSCI scores and expectations
    toplo1 <- scr_exp() %>% 
      select(COMID, StationCode, datcut, strcls, csci, perf, typelv) %>% 
      unnest %>% 
      rename(
        `Stream Class` = strcls,
        `Relative\nperformance` = perf,
        Type = typelv
      )
    
    # total expected range
    toplo2 <- scr_exp() %>% 
      select(COMID, StationCode, data, strcls) %>% 
      unnest %>% 
      rename(`Stream Class` = strcls)
    
    # plot
    p <- ggplot(toplo1, aes(y = StationCode, x = val)) + 
      geom_line(data = toplo2, aes(x = val, colour = `Stream Class`), alpha = 0.1, size = 2) +
      geom_line(aes(colour = `Stream Class`), alpha = 0.6, size = 2) + 
      theme_bw(base_family = 'serif', base_size = 18) +
      theme(
        axis.text.y = element_text(size = 10)
      ) +
      scale_x_continuous('CSCI') +
      scale_colour_manual(values = pal_exp(levels(toplo1$`Stream Class`)))
    
    # CSCI points by performance  
    if(typs == 'perf'){
      
      p <- p +
        geom_point(aes(x = csci, fill = `Relative\nperformance`), shape = 21, size = 4, alpha = 0.4) +
        geom_vline(xintercept = thrsh, linetype = 'dashed', size = 1) +
        scale_fill_manual(values = pal_prf(levels(toplo1$`Relative\nperformance`)), na.value = 'yellow')
      
      # CSCI points by type
    } else {
      
      p <- p +
        geom_point(aes(x = csci, fill = Type), shape = 21, size = 4, alpha = 0.4) +
        geom_vline(xintercept = thrsh, linetype = 'dashed', size = 1) +
        scale_fill_manual(values = pal_typ(levels(toplo1$Type)), na.value = 'yellow')
      
    }
    
    print(p)
    
  })
  
  # summary tables
  output$tab_sum <- DT::renderDataTable({
    
    typs <- input$typs
    
    # summary table by csci performance
    if(typs == 'perf'){
      
      totab <- scr_exp() %>% 
        dplyr::select(strcls, perf) %>% 
        group_by(strcls, perf) %>% 
        summarise(Sites = n()) %>% 
        na.omit %>% 
        rename(
          `Biological expectation` = strcls,
          `Performance` = perf
        )
      
    # summary table by csci type            
    } else {
      
      totab <- scr_exp() %>% 
        dplyr::select(strcls, perf, typeoc, typelv) %>% 
        group_by(strcls, perf, typeoc, typelv) %>% 
        summarise(Sites = n()) %>% 
        na.omit %>% 
        arrange(`typelv`) %>% 
        rename(
          `Biological expectation` = strcls,
          `Performance` = perf,
          `Observed score` = typeoc,
          Type = typelv
        ) %>% 
        mutate(Type = gsub('^Type|^Type0', '', Type))
      
    }
    
    return(totab)
    
  }, options = list(dom = 't'), rownames = F)
  
  # outputOptions(output, "map", suspendWhenHidden = FALSE, priority = 10)
  # outputOptions(output, "map_exp", suspendWhenHidden = FALSE)
  
  
}
