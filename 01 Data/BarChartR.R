##Attempting to recreate the Tableau bar graph into R. 
##First I needed to edit the data so I grouped the data by the Gender and Race columns, and filtered out the nondescriptive data such as "Unknown" race. 
##In addition, summarize was used to create an AVG_AGE column using the mean() function.
df$AGE <- as.numeric(as.character(df$AGE))
dftwo <- df %>% group_by(GENDER, RACE, add = FALSE) %>% filter(RACE %in% c("BLACK", "BLACK HISPANIC", "WHITE", "WHITE HISPANIC", "ASIAN/PACIFIC ISLANDER")) %>% summarize(AVG_AGE = mean(AGE)) 


##Observing the spread of the data to ensure that the nondescriptive data was removed.
spread(dftwo, GENDER, AVG_AGE) %>% AGE 

##Viewing the data again to double check the columns and rows.
dftwo

##Creation of a test ggplot graph to ensure that the data was editted properly and graphs could be reproduced. 
dftwo %>% ggplot(aes(x = RACE, y = AVG_AGE)) + geom_bar(stat = "identity") 

##First graph created. Faceting the graphy by Gender. Created labels for the Title, X-axis, and Y-axis. 
ggplot() + 
  coord_cartesian() +
  scale_x_discrete() + 
  scale_y_continuous() +
  facet_wrap(~GENDER, ncol = 1) +
  labs(title = 'Gender / Race') +
  labs(x = paste("RACE"), y = paste("AVG.AGE")) +
  layer(data = dftwo,
        mapping = aes(x = RACE, y = AVG_AGE),
        stat = "identity",
        stat_params = list(), 
        geom = "bar",
        geom_params = list(color = "blue"),
        position=position_identity()   )

##Recreation of the previous graph in order to correct the coloring of the graph so that it more closely matches the Tableau graph.
ggplot() + 
  coord_cartesian() +
  scale_x_discrete() + 
  scale_y_continuous() +
  facet_wrap(~GENDER, ncol = 1) +
  labs(title = 'Gender / Race') +
  labs(x = paste("RACE"), y = paste("AVG.AGE")) +
  layer(data = dftwo,
        mapping = aes(x = RACE, y = AVG_AGE, fill = GENDER),
        stat = "identity",
        stat_params = list(), 
        geom = "bar"
  ) 

##In order to create a reference line, I needed to further edit the data. I created a new dataframe, dfthree that ungrouped and then re-grouped the data just by gender in order to get an average age per gender.    
dfthree <- dftwo %>% ungroup %>% group_by(GENDER) %>% summarize(WINDOW_AVG_AGE = mean(AVG_AGE)) 

##Merged the average age per gender data frame that was previously created with the original data frame using inner_join
dffour <- inner_join(dftwo, dfthree, by = "GENDER")

##Final graph with proper coloing and reference line added for each respective gender facet.
##Reference line added by setting the yintercept equal to the average age per gender.
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
  layer(data = dffour, 
        mapping = aes(yintercept = WINDOW_AVG_AGE), 
        geom= "hline",
        geom_params = list(color = "black")
  )