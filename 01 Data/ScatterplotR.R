require(jsonlite)
require(RCurl)
require(dplyr)
require(ggplot2)
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df$HEIGHT <- as.numeric(as.character(df$HEIGHT))
df$WEIGHT <- as.numeric(as.character(df$WEIGHT))

ndf <- df %>% filter(HEIGHT != "null")
ndf$Height_Inches <- 60 + (ndf$HEIGHT %% 100)
ndf$Height_Inches[ndf$HEIGHT > 550] <- 72 + (ndf$HEIGHT[ndf$HEIGHT > 550] %% 100)

ndf <- ndf %>% select(GENDER,WEIGHT,Height_Inches,VICTIM_MINOR)

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  facet_grid(.~GENDER, labeller=label_both) +
  layer(data=ndf, 
        mapping=aes(x=WEIGHT, y=Height_Inches, color=VICTIM_MINOR), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(), 
        position=position_identity()
  ) +
  stat_smooth(data=ndf,
              mapping = aes(x=WEIGHT, y=Height_Inches, color=VICTIM_MINOR),
              method = "lm",
              fullrange = TRUE,
              se = FALSE
              )
