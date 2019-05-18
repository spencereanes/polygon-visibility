library(shiny)
#may need to change this path
source("visibility.R")

ui <- pageWithSidebar(
  headerPanel("Polygon Visilibity"),
  sidebarPanel(
    width=2,
    actionButton("polydone","Complete Current Polygon"),
    actionButton("rem_point", "Remove Last Point"),
    actionButton("compute","Compute Visibility Edges"),
    actionButton("reset","Reset")
  ),
  mainPanel(
    fluidRow(column(width = 8,
                    h4("Click plot to add points"),
                    plotOutput("plot1", click = "plot_click",width="100%"),
                    verbatimTextOutput("test")),
             column(width = 4,
                    h4("Table of points on plot"),
                    tableOutput("table")))
  )
)

server = function(input, output){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          L2 = numeric())
  
  l2 <- reactiveValues()
  l2$ctr <- 1
  
  vverts <- reactiveValues()
  vverts$v <- st_point(c(-10,-10))
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    #if(is.null(vverts$v)){
      gg1 <- ggplot(values$DT) +
        geom_point(size = 5, aes(x = x, y = y, group=L2)) +
        geom_polygon(aes(x = x, y = y, group=L2))+
        lims(x = c(0, 300), y = c(0, 100)) +
        theme(legend.position = "bottom") + 
        geom_sf(data=vverts$v,aes(color="visilibity lines"))
    #} else{
  #    #print("display new plot")
  #    gg1 <- ggplot(values$DT) +
  #      geom_point(aes(x = x, y = y, group=L2),size = 5) +
  #      geom_polygon(aes(x = x, y = y, group=L2))+
  #      lims(x = c(0, 100), y = c(0, 100)) +
  #      geom_sf(data=vverts$v,aes(color="visibility lines"))+
  #      theme(legend.position = "bottom")
        
   # }
    
    gg1
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = round(input$plot_click$x,1),
                          y = round(input$plot_click$y,1),
                          L2 = l2$ctr)
    
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
    
  })
  
  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  ## 5. render a table of the growing dataframe ##
  output$table <- renderTable({
    values$DT
  })
  
  # 6. Complete Polygon
  observeEvent(input$polydone, {
    
    l2$ctr <- l2$ctr+1
  })
  
  observeEvent(input$reset, {
    l2$ctr <- 1
    values$DT <- values$DT[0,]
    vverts$v <- st_point(c(-10,-10))
  })
  
  observeEvent(input$compute, {
    ppts <- values$DT %>% group_split(L2)
    withloop <- lapply(ppts, function(mat) (data.matrix(mat[,1:2])))
    withloop <- lapply(withloop, function(mat) rbind(mat,mat[1,]))
    polygons <- lapply(withloop, function(pp) st_polygon(list(pp)))
    ps <- st_multipolygon(polygons)
    #x`print(ps)
    vverts$v <- vis.graph(ps)
    #output$test=renderPrint({withloop})
    
    #output$plot1 = renderPlot({
    #  vis.graph(ps)
      
    #})
  })
}

shinyApp(ui, server)
