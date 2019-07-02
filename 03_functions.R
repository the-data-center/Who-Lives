

############################################
# # PULLING DATA # #
############################################

##pull parish, metro, and US numbers with census api
##input: census api variable names, human-readable names, and vintage
##output: dataframe in same format as Who Lives data tables excel sheet
wholivesdatapull <- function(variables, names = variables, year=2017){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  parishes <- getCensus(name = "acs/acs1", vintage = year, key = censuskey, vars = variables, region = "county:071,051,103", regionin = "state:22") ##pull parish data
  parishes$state = NULL  #state column pulled automatically & needs to be deleted
  colnames(parishes) <- c("place",names)  #so names match between the three pulls for rbind
  metro <- getCensus(name = "acs/acs1", vintage = year, key = censuskey, vars = variables, region = "metropolitan statistical area/micropolitan statistical area:35380")
  colnames(metro) <- c("place",names)
  us <- getCensus(name = "acs/acs1", vintage = year, key = censuskey, vars = variables, region = "us:1")
  colnames(us) <- c("place",names)
  df <- switch(rbind(parishes, metro, us))
  df[df == -555555555] <- 0
  return(df)  #combine the three pulls, rows 1 & 2 (Jeff & Orl) switched
}
########## Define function to pull variables

# Pull data. Note that this includes 2010-2017.
pullDataPEP <- function(variables, api, year, counties, metro) {
  parish <- getCensus(name = api, 
                      vintage = 2017, 
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                      vars = variables, 
                      region = counties, 
                      regionin = "state:22")
  
  state <- getCensus(name = api, 
                     vintage = 2017, 
                     key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                     vars = variables, 
                     region = "state:22")
  
  usa <- getCensus(name = api, 
                   vintage = 2017, 
                   key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                   vars = variables, 
                   region = "us:1")
  
  df <- parish %>% bind_rows(state) %>% bind_rows(usa) # Bind rows for counties, metro, state, usa
  
  rm(parish, state, usa)  # remove large objects from environment
  
  df <- df %>% 
    left_join(ageGroupCode) %>% # join verbose codes
    left_join(raceCode) %>%
    left_join(sexCode) %>% 
    left_join(hispCode) %>%
    mutate(place = GEONAME) %>% 
    mutate(POP = as.numeric(POP), 
           place = GEONAME,
           place = ifelse(!is.na(county),
                          str_sub(GEONAME, 1, nchar(GEONAME) - 18),
                          GEONAME))
  
  df <- df %>% 
    select(place, DATE_DESC, hispCodeName, sexCodeName, raceCodeName, ageGroupCodeName, POP) %>% 
    rename(hisp = hispCodeName,
           sex = sexCodeName,
           race = raceCodeName, 
           age = ageGroupCodeName,
           population = POP,
           date = DATE_DESC) %>% 
    filter(race %in% c("Total",
                       "White alone",
                       "Black or African American alone",
                       "Asian alone")) %>% 
    mutate(raceSimple = NA, # make variable base on race alone that matches Who Lives races. 
           raceSimple = ifelse(race == "Total" & hisp == "Total", "Total", raceSimple),
           raceSimple = ifelse(race == "White alone" & hisp == "Not Hispanic", "White", raceSimple),
           raceSimple = ifelse(race == "Black or African American alone" & hisp == "Not Hispanic", "Black", raceSimple),
           raceSimple = ifelse(race == "Asian alone" & hisp == "Not Hispanic", "Asian", raceSimple),
           raceSimple = ifelse(race == "Total" & hisp == "Hispanic", "Hispanic", raceSimple))  %>%
    filter(!is.na(raceSimple)) # Filter out other races 
  
  return(df)
  
}


############################################
# # ANALYSIS # #
############################################

##this function is used in "wholivesdatapull" to correctly order rows: Orl,Jeff,St.T/071,051,103
##input: dataframe (from censusapi pull)
##output: dataframe with rows 1 & 2 switched
switch <- function(dataframe){         
  dataframe2 <- dataframe[2,]  #extract row 2 (Orleans)
  dataframe <- rbind(dataframe2, dataframe[-2,])  #move row 2 to be row 1
}

##calculates MOE for aggregated estimates
##moe = sqrt(sum(estimateMOE^2))
##input: dataframe of estimates' MOEs (i.e. use cbind)
##output: column of MOEs
moeagg <- function(estimateMOE){  
  squares <- matrix(0, dim(estimateMOE)[1], dim(estimateMOE)[2])
  for(i in 1:dim(estimateMOE)[2]){
    squares[,i] <- t(estimateMOE[,i]*estimateMOE[,i])
  }
  sumsquares <- apply(squares, 1, sum)
  return(sqrt(sumsquares))
}

##calculates MOE for proportions
##p = x/y
##moe = sqrt(moex^2 - p^2 * moey^2)/y
##input: columns of measures y, moex, moey, p
##output: column of MOEs
moeprop <- function(y, moex, moey, p){
  mp <- matrix(0, length(y))
  for(i in 1:length(y)){
    if((moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]) < 0){
      mp[i] <- (sqrt(moex[i]*moex[i] + p[i]*p[i]*moey[i]*moey[i]))/y[i]
    } else {
      mp[i] <- (sqrt(moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]))/y[i]
    }
  }
  return(mp)
}

##stat testing for 2000 vs 201* data
##input: columns of estimates and their MOEs (zeros for Census 2000)
##output: column of yes or no if significant
stattest <- function(x, moex = matrix(0, length(x)), y, moey){
  significant <- matrix(0, length(x))
  v <- abs((x-y)/sqrt((moex/1.645)^2+(moey/1.645)^2))
  significant <- ifelse(v>1.645,"yes","no")
  return(as.list(significant))
}


############################################
# # GRAPHICS # #
############################################

## Robby's Data Center graph themes
themeDC_horizontal <- function(){
  theme_light() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}

themeDC_vertical <- function() {
  theme_light() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}

##create dodged bar graphs to compare two diff years 
##input: info created during api data pull
##output: bar chart
dodgedBar <- function(data,      
                      stattograph,      #variable name of current yar pct, must be in quo() for dplyr to be able to use it 
                      title,            
                      colors = c(DCcolor.p1skyblue, DCcolor.p1mediumblue), 
                      yscale = c(0,.45),      
                      pct = TRUE,      #used when formatting pct vals vs dollar vals
                      comparisonyear = "2000",
                      year = "2017",
                      digits = 0){     #for rounding, specifically for forbor
  dataGraphic <-  data %>% select(-contains("moeprop")) %>%      #dplyr rejects the format of moeprop, so we drop it
    mutate(placenames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
    mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%     #vars of type "factor" allow you to control order
    select(one_of("census2000", "sf2004", "sf1999"), !!stattograph, placenames, place.fac, significant) %>%     #one_of() chooses correct comparison vals/!! is the second part or the quo() tool
    gather(-placenames,-place.fac, -significant, key=variable, value=value) %>% 
    mutate(description = ifelse(variable == "census2000"|variable =="sf2004"|variable =="sf1999", comparisonyear, year)) %>%     #creates legend info
    mutate(valp = ifelse(value<.01,ifelse(significant == "no" & description == year, "<1%*", "<1%"),     #creates pct labels
                         paste0(round(value*100, digits = digits),"%",ifelse((significant == "no" & description == year), "*", "")))) %>%
    mutate(vald = ifelse((significant == "no" & description == year),      #creates dollar labels
                         paste0(dollar(value, largest_with_cents = 1),"*"), 
                         dollar(value, largest_with_cents = 1)))
  
  chart <- dataGraphic %>% 
    ggplot(aes(place.fac, value, fill=description)) + 
    geom_bar(stat="identity",
             position = position_dodge(),
             width = .7,
             color='gray50') +    #bar outline
    geom_text(data = subset(dataGraphic, as.numeric(value) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
              aes(label = ifelse(rep(pct,sum(dataGraphic$value>0)), 
                                 valp,
                                 vald)), 
              position=position_dodge(width = .7), 
              vjust = -.7, 
              size=2.75, 
              family="Asap") +
    scale_y_continuous(labels = ifelse(pct == TRUE, percent_format(accuracy = 1), comma_format(accuracy = 1)), expand = c(0,0), limits = yscale) + 
    scale_fill_manual(values = colors) + 
    themeDC_horizontal() +
    theme(legend.title = element_blank(),
          legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
          plot.title = element_text(hjust = .5, size=16)) + 
    labs(title = title,
         x="",
         y="")
  return(chart)
}
