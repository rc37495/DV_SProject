# server.R
require("jsonlite")
require("RCurl")
require("ggplot2")
require("dplyr")
require("shiny")
require("shinydashboard")
require("leaflet")
require("DT")
require("RCurl")
require("plyr")
require("gridExtra")

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  
  ds <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  ds$AGE <- as.numeric(as.character(ds$AGE))
  
  yes_ds <- subset(ds, ds$VICTIM_MINOR=="Y")
  yes_ds = ddply(yes_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
  yes_ds[,'mean']=round(yes_ds[,'mean'],2)
  
  no_ds <- subset(ds, ds$VICTIM_MINOR=="N")
  no_ds = ddply(no_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
  no_ds[,'mean']=round(no_ds[,'mean'],2)
  
  yes_ds <- eventReactive(input$clicks1, {within(yes_ds, {
    KPI = ifelse(ds$mean < KPI_Low_Max_value, "Younger", ifelse(ds$mean < KPI_Medium_Max_value, "Middle", "Older"))})})
  no_ds <- eventReactive(input$clicks1, {within(no_ds, {
    KPI = ifelse(ds$mean < KPI_Low_Max_value, "Younger", ifelse(ds$mean < KPI_Medium_Max_value, "Middle", "Older"))})})
  
  output$distplot1 <- renderPlot({
  yes_plot <- ggplot() + 
    coord_cartesian() + 
    scale_x_discrete() +
    scale_y_discrete() +
    labs(title='Victim Minor') +
    labs(x=("Gender"), y=("Race")) +
    layer(data=yes_ds(), 
          mapping=aes(x=GENDER, y=RACE, label=mean),
          stat="identity", 
          stat_params=list(), 
          geom="text",
          geom_params=list(colour="black"), 
          position=position_identity()
    ) +
    layer(data=yes_ds(), 
          mapping=aes(x=GENDER, y=RACE, fill=KPI), 
          stat="identity", 
          stat_params=list(), 
          geom="tile",
          geom_params=list(alpha=0.50), 
          position=position_identity()
    )
  
  no_plot <- ggplot() + 
    coord_cartesian() + 
    scale_x_discrete() +
    scale_y_discrete() +
    labs(title='Victim Not Minor') +
    labs(x=("Gender"), y=("Race")) +
    layer(data=no_ds(), 
          mapping=aes(x=GENDER, y=RACE, label=mean),
          stat="identity", 
          stat_params=list(), 
          geom="text",
          geom_params=list(colour="black"),
          position=position_identity()
    ) +
    layer(data=no_ds(), 
          mapping=aes(x=GENDER, y=RACE, fill=KPI), 
          stat="identity", 
          stat_params=list(), 
          geom="tile",
          geom_params=list(alpha=0.50), 
          position=position_identity()
    )
  
    plot <- grid.arrange(yes_plot, no_plot)
    plot
  })
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  df$HEIGHT <- as.numeric(as.character(df$HEIGHT))
  df$WEIGHT <- as.numeric(as.character(df$WEIGHT))
  
  ndf <- df %>% filter(HEIGHT != "null")
  ndf$Height_Inches <- 60 + (ndf$HEIGHT %% 100)
  ndf$Height_Inches[ndf$HEIGHT > 550] <- 72 + (ndf$HEIGHT[ndf$HEIGHT > 550] %% 100)
  
  nndf <- eventReactive(input$clicks2, {ndf %>% select(GENDER,WEIGHT,Height_Inches,VICTIM_MINOR)
  })
  
  output$distPlot2 <- renderPlot({
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      facet_grid(.~GENDER, labeller=label_both) +
      layer(data=nndf(), 
            mapping=aes(x=WEIGHT, y=Height_Inches, color=VICTIM_MINOR), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            position=position_identity()
      ) +
      stat_smooth(data=nndf(),
                  mapping = aes(x=WEIGHT, y=Height_Inches, color=VICTIM_MINOR),
                  method = "lm",
                  fullrange = TRUE,
                  se = FALSE
      )
    plot1
  })
  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select region || \\\' \\\' || \\\'Sales\\\' as measure_names, sum(sales) as measure_values from SUPERSTORE_SALES_ORDERS
                                                                                 where country_region = \\\'United States of America\\\'
                                                                                 group by region
                                                                                 union all
                                                                                 select market || \\\' \\\' || \\\'Coffee_Sales\\\' as measure_names, sum(coffee_sales) as measure_values from COFFEE_CHAIN
                                                                                 group by market
                                                                                 order by 1;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Blending 2 Data Sources') +
      labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      )
    plot3
  })
})
