dfuno <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from SEX_OFFENDERS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rc37495', PASS='orcl_rc37495', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

dfuno$AGE <- as.numeric(as.character(dfuno$AGE))

dfdos <- dfuno %>% group_by(GENDER, RACE, add = FALSE) %>% filter(RACE %in% c("BLACK", "BLACK HISPANIC", "WHITE", "WHITE HISPANIC", "ASIAN/PACIFIC ISLANDER")) %>% summarize(AVG_AGE = mean(AGE)) 

dfdos 

dftres <- dfdos %>% ungroup %>% group_by(GENDER) %>% summarize(WINDOW_AVG_AGE = mean(AVG_AGE)) 

dfcuatro <- inner_join(dfdos, dftres, by = "GENDER")

ggplot() + 
  coord_cartesian() +
  scale_x_discrete() + 
  scale_y_continuous() +
  facet_wrap(~GENDER, ncol = 1) +
  labs(title = 'Gender / Race') +
  labs(x = paste("RACE"), y = paste("AVG.AGE")) +
  layer(data = dffour,
        mapping = aes(x = RACE, y = AVG_AGE, fill = GENDER),
        stat = "identity",
        stat_params = list(), 
        geom = "bar"
  ) +
  layer(data = dfcuatro,
        mapping = aes(yintercept = WINDOW_AVG_AGE), 
        geom= "hline",
        geom_params = list(color = "black")
  )