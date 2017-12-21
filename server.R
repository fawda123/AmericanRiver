library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(scales)
library(leaflet.minicharts)
library(manipulateWidget)
source('R/funcs.R')

# spatial comid data
load('data/spat.RData')

# csci scores at sites
load('data/scrs.RData')

# color domain, csci scores and expectations
dmn <- spat %>% 
  select(matches('full')) %>% 
  data.frame %>% 
  select(-geometry) %>% 
  gather('var', 'val') %>% 
  .$val %>% 
  c(., scrs$csci) %>% 
  range(na.rm = T)

dmn_difr <- spat %>% 
  select(matches('full')) %>% 
  data.frame %>% 
  select(-geometry) %>% 
  gather('var', 'val') %>% 
  .$val %>% 
  range(na.rm = T)
dmn_difr <- c(min(scrs$csci) - dmn_difr[2], max(scrs$csci) - dmn_difr[1])

# color palette for csci scores
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),
  na.color = 'yellow',
  domain = dmn)

# color palette for csci score differences
pal_difr <- colorNumeric(
  palette = c('black', 'purple', 'white', 'darkgreen', 'black'),
  na.color = 'yellow',
  domain = dmn_difr)

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

# server logic
server <- function(input, output, session) {
  
  # data to plot, polylines with score expections
  dat <- reactive({
    
    ptile <- input$ptile 
    
    # get polylines to plot
    ptile <- ptile %>% 
      format(nsmall = 2) %>% 
      paste0('full', .) %>% 
      gsub('\\.', '.', .)
    names(spat)[names(spat) %in% ptile] <- 'lns'
    
    # set zero values to NA
    out <- spat 
    out
    
  })
  
  # tails input as reactive, passed to multiple
  tlinp <- reactive({
    
    tails <- input$tails %>% 
      gsub('More certain|Less certain|\\(|\\)|\\s+', '', .) %>% 
      as.numeric
    tails <- 0.5 - tails
    return(tails)
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # inputs
    thrsh <- input$thrsh
    
    # get biological condition expectations
    cls <- getcls2(spat, thrsh = thrsh, tails = tlinp(), modls = 'full')
    
    # join with spatial data
    out <- spat %>% 
      left_join(cls, by = 'COMID')
    
    out
    
  })
  
  # CSCI scores, take difference from expectation if difr is true
  csci <- reactive({
    
    difr <- input$difr
    jitr <- input$jitr
    
    out <- scrs
    if(difr){
      
      out <- dat() %>% 
        select(COMID, lns) %>% 
        mutate(COMID = as.character(COMID)) %>% 
        left_join(scrs, ., by = 'COMID') %>% 
        mutate(csci = csci - lns)
      
    }
    
    # jitter scores with overlapping lat/lon
    out <- out %>% 
      mutate(
        lat = ifelse(duplicated(lat), jitter(lat, factor = jitr), lat),
        long = ifelse(duplicated(long), jitter(long, factor = jitr), long)
      )
    
    return(out)
    
  })
  
  # CSCI scores and stream condition expectations
  scr_exp <- reactive({
    
    # inputs
    thrsh <- input$thrsh
    
    # process
    incl <- site_exp(spat, scrs, thrsh = thrsh, tails = tlinp(), modls = 'full') %>% 
      select(-lat, -long) 
    
    # assign csci station locations for jittr
    incl <- csci() %>% 
      select(StationCode, lat, long) %>% 
      mutate(StationCode = factor(StationCode, levels = levels(incl$StationCode))) %>% 
      left_join(incl, ., by = 'StationCode')
    
    return(incl)
    
  })
  
  # non-reactive base map
  output$map <- renderLeaflet(
    
    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      syncWith('maps')
    
  )
  
  # non-reactive base map, condition expectations
  output$map_exp <- renderLeaflet(
    
    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      syncWith('maps')
    
  )
  
  ##
  # these update inputs that are duplicated across tabs
  trs <- ''
  tls <- ''
  
  # thrsh
  observe({
    if (trs != input$thrsh){
      updateSliderInput(session, "thrsh2", NULL, input$thrsh)
      updateSliderInput(session, "thrsh3", NULL, input$thrsh)
      trs <<- input$thrsh
    }
  })
  observe({
    if (trs != input$thrsh2){
      updateSliderInput(session, "thrsh", value = input$thrsh2)
      updateSliderInput(session, "thrsh3", value = input$thrsh2)
      trs <<- input$thrsh2
    }
  })
  observe({
    if (trs != input$thrsh3){
      updateSliderInput(session, "thrsh", value = input$thrsh3)
      updateSliderInput(session, "thrsh2", value = input$thrsh3)
      trs <<- input$thrsh3
    }
  })
  
  # tails
  observe({
    if (trs != input$tails){
      updateSliderTextInput(session, "tails2", selected = input$tails)
      updateSliderTextInput(session, "tails3", selected = input$tails)
      trs <<- input$tails
    }
  })
  observe({
    if (trs != input$tails2){
      updateSliderTextInput(session, "tails", selected = input$tails2)
      updateSliderTextInput(session, "tails3", selected = input$tails2)
      trs <<- input$tails2
    }
  })
  observe({
    if (trs != input$tails3){
      updateSliderTextInput(session, "tails", selected = input$tails3)
      updateSliderTextInput(session, "tails2", selected = input$tails3)
      trs <<- input$tails3
    }
  })
  
  ##
  # reactive maps
  observe({
    
    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    typs <- input$typs
    difr <- input$difr
    
    # reactives
    dat <- dat()
    dat_exp <- dat_exp()
    scr_exp <- scr_exp()
    
    # score expectations
    exp <- leafletProxy("map", data = dat) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal(lns), 
                   label = ~paste0(COMID, ', Likely score:', as.character(round(lns, 2)))
      ) %>% 
      addLegend("topright", pal = pal, values = ~lns,
                title = "Likely score",
                opacity = 1
      )
    
    
    # csci scores if false, otherwise differences
    if(difr){
      
      exp <- exp %>% 
        addCircleMarkers(data = csci(), lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.8, 
                         label = ~paste0(StationCode, ', CSCI: ', as.character(round(csci, 2))),
                         fillColor = ~pal_difr(csci), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_difr, values = csci()$csci,
                  title = "CSCI difference",
                  opacity = 1
        )
      
    } else {
      
      exp <- exp %>% 
        addCircleMarkers(data = csci(), lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.8, 
                         label = ~paste0(StationCode, ', CSCI: ', as.character(round(csci, 2))),
                         fillColor = ~pal(csci), color = 'black'
        )
      
    }
    
    # condition expectations
    exp_bs <- leafletProxy("map_exp", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls()%>% 
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Expected classification",
                opacity = 1
      ) %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste0(COMID, ', Stream class:', strcls)
      ) %>% 
      addCircleMarkers(data = scr_exp, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9, 
                       label = ~paste0(StationCode, ', CSCI: ', as.character(round(csci, 2)), ', ', perf),
                       fillColor = ~pal_prf(perf), color = 'black'
      ) %>% 
      addLegend("topright", pal = pal_prf, values = scr_exp$perf,
                title = "CSCI performance",
                opacity = 1
      )
    
    # sync the maps
    combineWidgets(exp, exp_bs)
    
  })
  
  # plot of csci scores and expectations by station code
  output$plo_exp <- renderPlot({
    
    thrsh <- input$thrsh
    bysta <- input$bysta
    
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
    
    # arrange by station if true
    if(bysta){
      
      toplo1 <- toplo1 %>% 
        mutate(StationCode = as.character(StationCode)) %>% 
        arrange(StationCode)%>% 
        mutate(
          StationCode = factor(StationCode),
          StationCode = factor(StationCode, levels = rev(levels(StationCode)))
        )
      
      toplo2 <- toplo2 %>% 
        mutate(StationCode = as.character(StationCode)) %>% 
        arrange(StationCode) %>% 
        mutate(
          StationCode = factor(StationCode),
          StationCode = factor(StationCode, levels = rev(levels(StationCode)))
        )
      
    }
    
    # plot
    p <- ggplot(toplo1, aes(y = StationCode, x = val)) + 
      geom_line(data = toplo2, aes(x = val, colour = `Stream Class`), alpha = 0.1, size = 2) +
      geom_line(aes(colour = `Stream Class`), alpha = 0.6, size = 2) + 
      theme_bw(base_family = 'serif', base_size = 18) +
      theme(
        axis.text.y = element_text(size = 10)
      ) +
      scale_x_continuous('CSCI') +
      scale_y_discrete('Site') +
      scale_colour_manual(values = pal_exp(levels(toplo1$`Stream Class`))) +
      geom_point(aes(x = csci, fill = `Relative\nperformance`), shape = 21, size = 4, alpha = 0.4) +
      geom_vline(xintercept = thrsh, linetype = 'dashed', size = 1) +
      scale_fill_manual(values = pal_prf(levels(toplo1$`Relative\nperformance`)), na.value = 'yellow')
    
    print(p)
    
  })
  
  # summary tables
  output$tab_sum <- DT::renderDataTable({
    
    thrsh <- input$thrsh
    
    # summary table by csci type          
    totab <- get_tab(scr_exp(), thrsh = thrsh, tails = tlinp())
    
    return(totab)
    
  }, rownames = F, options = list(dom = 't', pageLength = 12))
  
}
