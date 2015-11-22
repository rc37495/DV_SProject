require("dyplr")
require("ggplot2")
require("jsonlite")
require("RCurl")
require("plyr")
require("gridExtra")

ds <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

ds$AGE <- as.numeric(as.character(ds$AGE))

######### Variable ###########
kpi_low_max = 35.00
kpi_med_max = 50.00
######### Variable ###########


# Make subset datasets - split by if victim is minor
yes_ds <- subset(ds, ds$VICTIM_MINOR=="Y")
yes_ds = ddply(yes_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
yes_ds[,'mean']=round(yes_ds[,'mean'],2)

no_ds <- subset(ds, ds$VICTIM_MINOR=="N")
no_ds = ddply(no_ds, .(GENDER,RACE), summarize, mean=mean(AGE))
no_ds[,'mean']=round(no_ds[,'mean'],2)


# add column of low-med-hig ratings with respect to KPI measures
yes_ds = within(yes_ds, {
  KPI = ifelse(mean < kpi_low_max, "Younger", ifelse(mean < kpi_med_max, "Middle", "Older"))})
no_ds = within(no_ds, {
  KPI = ifelse(mean < kpi_low_max, "Younger", ifelse(mean < kpi_med_max, "Middle", "Older"))})


# make cross-tab for rapists with minor victims
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

# make cross-tab for rapists without minor victims
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

# plot both graphs
grid.arrange(yes_plot, no_plot)
