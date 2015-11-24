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
  
  output$distPlot1 <- renderPlot({
    # Start your code here.
    
    # The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"
    
    KPI_Low_Max_value <- reactive({input$KPI1})     
    KPI_Medium_Max_value <- reactive({input$KPI2})
    
    ds <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
    
    ds$AGE <- as.numeric(as.character(ds$AGE))
    
    # Make subset datasets - split by if victim is minor
    yes_ds <- subset(ds, ds$VICTIM_MINOR=="Y")
    yes_ds = ddply(yes_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
    yes_ds[,'mean']=round(yes_ds[,'mean'],2)
    
    no_ds <- subset(ds, ds$VICTIM_MINOR=="N")
    no_ds = ddply(no_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
    no_ds[,'mean']=round(no_ds[,'mean'],2)
    
    # add column of low-med-hig ratings with respect to KPI measures
    yes_ds = within(yes_ds, {
      KPI = ifelse(mean < KPI_Low_Max_value(), "Younger", ifelse(mean < KPI_Medium_Max_value(), "Middle", "Older"))})
    no_ds = within(no_ds, {
      KPI = ifelse(mean < KPI_Low_Max_value(), "Younger", ifelse(mean < KPI_Medium_Max_value(), "Middle", "Older"))})
    
    yes_plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Victim Minor') +
      labs(x=("Gender"), y=("Race")) +
      layer(data=yes_ds, 
            mapping=aes(x=GENDER, y=RACE, label=mean),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=yes_ds, 
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
      layer(data=no_ds, 
            mapping=aes(x=GENDER, y=RACE, label=mean),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"),
            position=position_identity()
      ) +
      layer(data=no_ds, 
            mapping=aes(x=GENDER, y=RACE, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      )
    
    # make cross-tab for rapists without minor victims
    plot <- grid.arrange(yes_plot, no_plot)
    
    
    # End your code here.
    return(plot)
    
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
  
  df3 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  df3$AGE <- as.numeric(as.character(df3$AGE))
  
  sds = ddply(df3, .(GENDER,RACE), summarize, mean_age=mean(AGE))
  ssds = ddply(df3,~GENDER,summarise,avg_age=mean(AGE))
  
  sds <- na.omit(sds)
  sds <- sds[sds$RACE!="UNKNOWN",]
  ssds <- na.omit(ssds)
  
  sds <- inner_join (sds, ssds, by = "GENDER")
  
  output$distPlot3 <- renderPlot(height=650, width=700, {
    
    plot3 <- ggplot() + 
      coord_cartesian() +
      scale_x_discrete() + 
      scale_y_continuous() +
      facet_wrap(~GENDER, ncol = 1) +
      labs(title = 'Gender / Race') +
      labs(x = paste("RACE"), y = paste("AVG.AGE")) +
      #annotate("text", x=1.0, y=500, colour="#00BFC4", label = "41.74") + 
      #annotate("text", x=3.0, y=43, colour="#F8766D", label = "45.08")
      layer(data = sds,
            mapping = aes(x = RACE, y = mean_age, fill = GENDER),
            stat = "identity",
            stat_params = list(), 
            geom = "bar"
      ) +
      layer(data = sds, 
            mapping = aes(yintercept = avg_age), 
            geom= "hline",
            geom_params = list(color = "black")
      )
    
  plot3
  })
})