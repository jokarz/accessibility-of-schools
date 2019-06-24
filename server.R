#mute warnings
options(warn=-1)

function(input, output, session) {

current.sch.id = reactive({
  which(schdata@data$school_name == toupper(input$sch), arr.ind=TRUE)
})

iso = reactive({
  schl = schdata@data$school_name[[current.sch.id()]]
  iso = readRDS(file=paste0('./',isopath,'/', schl,'.rds'))
  return(iso)
})

acc.contour = reactive({
  schl = schdata@data$school_name[[current.sch.id()]]
  contour = readRDS(file=paste0('./',matrixpath,'/', schl,'.contour.rds'))
  return(contour)
})

acc = reactive({
  schl = schdata@data$school_name[[current.sch.id()]]
  acc = readRDS(file=paste0('./',matrixpath,'/', schl,'.rds'))
  return(acc)
})


observeEvent(input$maptype, {
  proxy <- leafletProxy("map")
  proxy %>% clearTiles() %>%
    addMapPane("providertiles", zIndex = 401) %>%
  addProviderTiles(input$maptype,
                   options = c(noWrap = FALSE, opacity = 0.8, detectRetina = FALSE, minZoom = 12, 
                                  maxZoom = 17, interactive = TRUE, className = "", pane = 'providertiles'),
                   group = 'providertiles')
})

observeEvent(input$isochrone, {
  proxy <- leafletProxy("map")
  if (input$isochrone){
    updateCheckboxInput(session, 'kernel', value = FALSE)
    updateCheckboxInput(session, 'hansendistance', value = FALSE)
    updateCheckboxInput(session, 'hansenduration', value = FALSE)
    proxy %>% clearGroup('kernellayer')
    schl = schdata@data$school_name[[current.sch.id()]]
    proxy %>% addMapPane("isolayer", zIndex = 410) %>% addPolygons(data =iso(), stroke = TRUE, weight=0.5,
                          smoothFactor = 1, color="black", options = pathOptions(pane = "isolayer"),
                          fillOpacity = 0.8, fillColor =brewer.pal(5,"Purples"), group = 'isolayer' )
  }else{
    proxy %>% clearGroup('isolayer') 
    updateCheckboxInput(session, 'isochronechart', value = FALSE)
  }
})

observeEvent(input$isochronechart, {
  if(!input$isochrone && input$isochronechart){
    updateCheckboxInput(session, 'isochronechart', value = FALSE)
  }
})

observeEvent(input$hansenduration, {
  proxy <- leafletProxy("map")
  if(input$hansenduration){
    updateCheckboxInput(session, 'isochrone', value = FALSE)
    updateCheckboxInput(session, 'kernel', value = FALSE)
    updateCheckboxInput(session, 'hansendistance', value = FALSE)
    
    proxy %>% clearGroup('isolayer') %>% clearGroup('hansendistancelayer') %>% clearGroup('kernellayer') 
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc.contour()$duration_hansen
    )
    proxy %>% addMapPane('hansendurationlayer', zIndex = 411) %>%
      addPolygons(data = acc.contour(), fillColor = pal(acc.contour()$duration_hansen), 
                  fillOpacity = 0.3, stroke = FALSE, 
                  group = 'hansendurationlayer', options = pathOptions(pane = "hansendurationlayer")) 
  }else{
    proxy %>% clearGroup('hansendurationlayer')
    updateCheckboxInput(session, 'hansendurationpoints', value = FALSE)
  }
})

observeEvent(input$hansendurationpoints, {
  proxy <- leafletProxy("map")
  if(input$hansendurationpoints){
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc()@data$durationHansen
    )
    proxy %>% addMapPane('hansendurationpointslayer', zIndex = 412) %>%
      addCircles(lng = acc()@coords[,1], lat = acc()@coords[,2], radius = sqrt(acc()@data$durationHansen)*10, weight= 5, color = pal(acc()@data$durationHansen), 
                 stroke = TRUE, fillOpacity = 0.4, opacity = 0.8,
                 group = 'hansendurationpointslayer', options = pathOptions(pane = "hansendurationpointslayer")) 
  }else{
    proxy %>% clearGroup('hansendurationpointslayer')
  }
})

observeEvent(input$hansendistance, {
  proxy <- leafletProxy("map")
  if(input$hansendistance){
    updateCheckboxInput(session, 'isochrone', value = FALSE)
    updateCheckboxInput(session, 'kernel', value = FALSE)
    updateCheckboxInput(session, 'hansenduration', value = FALSE)
    
    proxy %>% clearGroup('isolayer') %>% clearGroup('kernellayer') %>% clearGroup('hansendurationlayer') 
    pal<- colorFactor(
      palette = 'Reds',
      domain = acc.contour()$distance_hansen
    )
    proxy %>% addMapPane('hansendistancelayer', zIndex = 411) %>%
      addPolygons(data = acc.contour(), fillColor = pal(acc.contour()$distance_hansen), 
                  fillOpacity = 0.3, stroke = FALSE, 
                  group = 'hansendistancelayer', options = pathOptions(pane = "hansendistancelayer")) 
  }else{
    proxy %>% clearGroup('hansendistancelayer')
    updateCheckboxInput(session, 'hansendistancepoints', value = FALSE)
  }
})

observeEvent(input$hansendistancepoints, {
  proxy <- leafletProxy("map")
  if(input$hansendistancepoints){
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc()@data$distanceHansen
    )
    proxy %>% addMapPane('hansendistancepointslayer', zIndex = 412) %>%
      addCircles(lng = acc()@coords[,1], lat = acc()@coords[,2], radius = sqrt(acc()@data$distanceHansen)*10, weight= 5, color = pal(acc()@data$distanceHansen), 
                 stroke = TRUE, fillOpacity = 0.4, opacity = 0.8,
                 group = 'hansendistancepointslayer', options = pathOptions(pane = "hansendistancepointslayer")) 
  }else{
    proxy %>% clearGroup('hansendistancepointslayer')
  }
})

observeEvent(input$kernel, {
  proxy <- leafletProxy("map") 
  if(input$kernel){
    updateCheckboxInput(session, 'isochrone', value = FALSE)
    updateCheckboxInput(session, 'hansendistance', value = FALSE)
    updateCheckboxInput(session, 'hansenduration', value = FALSE)
    
    proxy %>% clearGroup('isolayer') %>% clearGroup('hansendistancelayer') %>% clearGroup('hansendurationlayer') 
    uni = unique(kernelspatial$Value)
    for(j in 1:length(uni)){
      polys = kernelspatial[kernelspatial$Value == uni[j],]
      proxy %>% addMapPane("kernellayer", zIndex = 410) %>% 
        addPolygons(data =polys, stroke = TRUE, weight=0.5, smoothFactor = 0.2, 
                    color="black", fillOpacity = 0.6, options = pathOptions(pane = "kernellayer"), 
                    fillColor = brewer.pal(length(uni),"Greens")[j], group = 'kernellayer' )
    }
  }else{
    proxy %>% clearGroup('kernellayer')
  }
})
  
observeEvent(input$sch, {
  if( is.null(zoomlevel())){
    zlevel = 12
  }else{
    zlevel = zoomlevel()
  }
  
  proxy <- leafletProxy("map")
  schl = schdata@data$school_name[[current.sch.id()]]
  
  if (input$isochrone){
    proxy %>% clearGroup('isolayer') %>% addMapPane("isolayer", zIndex = 410) %>% addPolygons(data =iso(), stroke = TRUE, weight=0.5,
                          smoothFactor = 1, color="black", options = pathOptions(pane = "isolayer"),
                          fillOpacity = 0.6, fillColor =brewer.pal(5,"Purples"), group = 'isolayer' )
  }
  
  if (input$hansenduration){
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc.contour()$duration_hansen
    )
    proxy %>% clearGroup('hansendurationlayer') %>% addMapPane('hansendurationlayer', zIndex = 411) %>%
      addPolygons(data = acc.contour(), fillColor = pal(acc.contour()$duration_hansen), 
                  fillOpacity = 0.3, stroke = FALSE, 
                  group = 'hansendurationlayer', options = pathOptions(pane = "hansendurationlayer")) 
  }
  
  if (input$hansendurationpoints){
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc()@data$durationHansen
    )
    proxy %>% clearGroup('hansendurationpointslayer') %>% addMapPane('hansendurationpointslayer', zIndex = 412) %>%
      addCircles(lng = acc()@coords[,1], lat = acc()@coords[,2], radius = sqrt(acc()@data$durationHansen)*10, weight= 5, color = pal(acc()@data$durationHansen), 
                 stroke = TRUE, fillOpacity = 0.4, opacity = 0.8,
                 group = 'hansendurationpointslayer', options = pathOptions(pane = "hansendurationpointslayer")) 
  }
  
  if(input$hansendistance){
    pal<- colorFactor(
      palette = 'Reds',
      domain = acc.contour()$distance_hansen
    )
    proxy %>% clearGroup('hansendistancelayer') %>% addMapPane('hansendistancelayer', zIndex = 411) %>%
      addPolygons(data = acc.contour(), fillColor = pal(acc.contour()$distance_hansen), 
                  fillOpacity = 0.3, stroke = FALSE, 
                  group = 'hansendistancelayer', options = pathOptions(pane = "hansendistancelayer")) 
  }
  
  if(input$hansendistancepoints){
    pal <- colorFactor(
      palette = 'Reds',
      domain = acc()@data$distanceHansen
    )
    proxy %>% clearGroup('hansendistancepointslayer') %>% addMapPane('hansendistancepointslayer', zIndex = 412) %>%
      addCircles(lng = acc()@coords[,1], lat = acc()@coords[,2], radius = sqrt(acc()@data$distanceHansen)*10, weight= 5, color = pal(acc()@data$distanceHansen), 
                 stroke = TRUE, fillOpacity = 0.4, opacity = 0.8,
                 group = 'hansendistancepointslayer', options = pathOptions(pane = "hansendistancepointslayer")) 
  }
  
  proxy %>% clearGroup('targetlayer') %>% addMapPane("targetlayer", zIndex = 430) %>%
    addMarkers(lng = schdata@coords[current.sch.id(),1], lat = schdata@coords[current.sch.id(),2], 
               popup = schl, options = markerOptions(interactive = TRUE), clusterOptions = markerClusterOptions(), 
               group = 'targetlayer', icon = schIcon) %>% 
    flyTo(lng = schdata@coords[current.sch.id(),1], lat = schdata@coords[current.sch.id(),2], zlevel)
})

observeEvent(input$residential, {
  proxy <- leafletProxy("map")
  if (input$residential){
    proxy %>%  addMapPane("reslayer", zIndex = 420) %>% addCircleMarkers( lng = residentialdata@coords[,1], lat = residentialdata@coords[,2], 
                                opacity = 1, fillOpacity = 1, fillColor = '#225A88',color = '#000', 
                                stroke=TRUE, weight = 0.5, radius= 2, options = pathOptions(pane = "reslayer"),
                                popup = residentialdata@data$ADDRESS, label = residentialdata@data$ADDRESS, 
                                data = residentialdata@data$ADDRESS, group = 'reslayer')
  }else{
    proxy %>% clearGroup('reslayer')
  }
})

#for the legend
observe({
  schl = schdata@data$school_name[[current.sch.id()]]
  proxy <- leafletProxy("map")
  proxy %>% clearControls()
  if (input$isochrone && input$legend){
    count = iso()@data$blocks
    proxy %>% addLegend(position="topright",colors=rev(brewer.pal(5,"Purples")),
                        labels=rev(c('< 90 min', '< 60 min', '< 45 min', '< 30 min', '< 15 min')),
                        opacity = 0.8,
                        title=paste0("Travel time from public transport to ", toTitleCase(tolower(schl)), ' :'))
  }
  
  if(input$hansenduration && input$legend){
    x = acc.contour()$duration_hansen
    qpal <- colorBin("Reds", x,  reverse = TRUE)
    proxy %>%  addLegend(position="topright", pal = qpal, values = x,
                         opacity = 0.8, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                         title=paste0("Hansen Accessibility on DURATION from HDB to ", toTitleCase(tolower(schl)), ' (highest to lowest accessibility)'))
  }
  
  if(input$hansendistance && input$legend){
    x = acc.contour()$distance_hansen
    qpal <- colorBin("Reds", x,  reverse = TRUE)
    proxy %>%  addLegend(position="topright", pal = qpal, values = x,
                         opacity = 0.8, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                         title=paste0("Hansen Accessibility on DISTANCE from HDB to ", toTitleCase(tolower(schl)), ' (highest to lowest accessibility)'))
  }
  
  if(input$kernel && input$legend){
    proxy %>%  addLegend(position="topright",colors=rev(brewer.pal(8,"Greens")),
                         labels=rev(c(unique(kernelspatial@data$Value))),
                         opacity = 0.8,
                         title=paste0("Kernel Density for the HDB blocks (highest to lowest density)"))
  }
  
  if(input$residential && input$legend){
    proxy %>%  addLegend(position="topright",colors=rev(c('navy')),
                         labels=rev(c("HDB")),
                         opacity = 0.8)
  }
  
  if(input$isochrone == FALSE && 
     input$residential == FALSE && 
     input$kernel == FALSE && 
     input$hansenduration == FALSE && 
     input$hansendistance == FALSE && 
     input$legend == TRUE){
    updateCheckboxInput(session, 'legend', value = FALSE)
  }
})

zoomlevel = reactive({
  input$map_zoom
})

output$map <- renderLeaflet({
  leaflet() %>%
    setView( lng = 103.8198, lat = 1.3521, zoom = 12) %>%
    setMaxBounds( lng1 = 103.4057919091
                  , lat1 = 1.1648902351
                  , lng2 = 104.2321161335
                  , lat2 = 1.601881499) %>%
    htmlwidgets::onRender("
          function(el,x) {
              //$('.leaflet-control-zoom-in').html('<span class=\"typcn typcn-zoom-in\"></span>')
            document.title = 'BURP - Balancing Urban Residential Planning'
          }
      ")
  
})

output$gg_graph <- renderPlot({
  cumsum = iso()$blocks
  cumsum = rev(cumsum)
  d = tbl_df(cumsum)
  d$category = c('0-15 min', '15-30min', '30-45 min', '45-60 min', '60-90 min')
  d$count = 0
  d$freq = 0
  d$cum_freq = 0
  names(d)[1] = 'cumsum'
  max = max(cumsum)
  for(i in 1:length(cumsum)){
    if (i == 1){
      d$count[i] = d$cumsum[i]
    }else{
      d$count[i] = d$cumsum[i] - d$cumsum[i-1]
    }
    d$freq[i] = d$count[i]/max
    d$cum_freq[i] = d$cumsum[i]/max
  }
 
  
  def_par <- par() 
  par(mar=c(3,0,2,2)+0.2, bg='black', col.axis = 'white', col.lab = 'white', col.main = 'white')
  
  pc = barplot(d$count, col = '#AAAAAA', cex.lab = 0.8,
               width = 1, space = 0.1, border = NA, axes = F,
               ylim = c(0, 1.1 * max(d$count, na.rm = T)), 
               ylab = "Counts" , cex.names = 0.8, 
               names.arg = d$category, offset = 2,
               main = "HDB block distribution")
  text(x = pc, offset = 0.5 , y = d$count, label = d$count, pos = 3, cex = 0.9, col = "#00aa00")
  px <- d$cum_freq * max(d$count, na.rm = T)
  lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  axis(side = 4, at = c(px), labels = paste(c( round(d$cum_freq * 100)) ,"%",sep=""), 
       las = 0, col.axis = "grey62", col = "white", cex.axis = 0.7, col.axis = "white")
  suppressWarnings(par(def_par))
})

output$table <- renderDataTable(datatable({
  if(input$selectTable == 1){
    data = residentialdata@data
    colnames(data) = c('Address', 'Postal Code', 'X Coordinate', 'Y Coordinate', 'Road Name')
    data = as.data.frame(data)
    data %>% mutate(Address = gsub("( ?SINGAPORE.*)", "", Address)) %>% dplyr::select('Address', 'Postal Code', 'X Coordinate', 'Y Coordinate')
  }else{
    data = schdata@data
    colnames(data) = c('School', 'Postal Code', 'X Coordinate', 'Y Coordinate', 'Road Name')
    data = as.data.frame(data)
    data %>% dplyr::select('School', 'Postal Code', 'X Coordinate', 'Y Coordinate')
  }
  
}, class='compact row-border'))

output$tablehansen <- renderDataTable(datatable({
  sch = input$selectTableHansen
  data = readRDS(file = paste0(matrixpath, '/', toupper(sch), '.rds'))
  data = data@data
  data = data[, -c(2,5:7)]
  data = data %>% mutate(duration = sprintf("%0.2f", duration), distance = sprintf("%0.2f", distance))
  colnames(data) = c('Address', 'Duration (minutes)', 'Distance (km)')
  data %>% mutate(Address = gsub("( ?SINGAPORE.*)", "", Address))
}, class='compact row-border'))

}

