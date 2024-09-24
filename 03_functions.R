

############################################
# # PULLING DATA # #
############################################

##pull parish, metro, and US numbers with census api
##input: census api variable names, human-readable names, and vintage
##output: dataframe in same format as Who Lives data tables excel sheet

wholivesdatapull <- function(variables, names = variables, year = 2023, censusname = "acs/acs1"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  parishes <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = "county:071,051", regionin = "state:22") ##pull parish data
  parishes$state = NULL  #state column pulled automatically & needs to be deleted
  colnames(parishes) <- c("place",names)  #so names match between the three pulls for rbind
  metro <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = "metropolitan statistical area/micropolitan statistical area:35380")
  colnames(metro) <- c("place",names)
  us <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = "us:1")
  colnames(us) <- c("place",names)
  df <- switch(rbind(parishes, metro, us)) #this function is created a little later in this document
  df[df == -555555555] <- 0

  df <- df %>% mutate(placename = case_when(place == "051" ~ "Jefferson",
                                            place == "071" ~ "Orleans",
                                            place == "35380" ~ "New Orleans Metro Area",
                                            place == "1" ~ "United States")) %>%
    filter(place != "093")
  return(df)  #combine the three pulls, rows 1 & 2 (Jeff & Orl) switched
}

#creating a separate data pull for 2000 so that we can manually match the metro parish estimates and include stat testing
#WL datapull with error

wholivesdatapull2000 <- function(variables, names = variables, universe = "persons", error = TRUE, parishregions = "county:071,051"){
  censuskey = "530ce361defc2c476e5b5d5626d224d8354b9b9a"
  parishes <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = variables, region = parishregions, regionin = "state:22") 
  parishes$state = NULL  #state column pulled automatically & needs to be deleted
  
  metro <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = variables, region = "county:071,051,075,087,089,093,095", regionin = "state:22")
  metro <- metro %>% select(-state,-county) %>% summarize(across(everything(), sum)) %>% mutate(county = "MSA_2023") %>% relocate("county")
  
  if (universe == "persons") {
    parish_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "P001001", region = "county:071,051", regionin = "state:22") %>% select(-state) %>% rename(POP = P001001)
    metro_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "P001001", region = "county:071,051,075,087,089,093,095", regionin = "state:22") %>% select(-state,-county) %>% summarize(across(everything(), sum)) %>% mutate(county = "MSA_2023") %>% rename(POP = P001001) %>% relocate("county")
    us_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "P001001", region = "us:1") %>% rename(county = us) %>% rename(POP = P001001)
    
  }
  
  if (universe == "households") {
    parish_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "H001001", region = "county:071,051", regionin = "state:22") %>% select(-state) %>% rename(POP = H001001)
    metro_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "H001001", region = "county:071,051,075,087,089,093,095", regionin = "state:22") %>% select(-state,-county) %>% summarize(across(everything(), sum)) %>% mutate(county = "MSA_2023") %>% rename(POP = H001001) %>% relocate("county")
    us_totpop <- getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = "H001001", region = "us:1") %>% rename(county = us) %>% rename(POP = H001001)
  }
  
  parishes <- parishes %>% left_join(parish_totpop)
  metro <- metro %>% left_join(metro_totpop)
  
  LA_data <- rbind(parishes, metro)
  if(error == TRUE) {
    LA_data <- error2000(LA_data, names, "LA")
  } else {
    names(LA_data) <- c("place", names) 
    LA_data <- LA_data %>% select(-last_col())
  }
  
  
  us <-  getCensus(name = "dec/sf3", vintage = 2000, key = censuskey, vars = variables, region = "us:1") %>% rename(county = us)
  us <- us %>% left_join(us_totpop)
  
  if (error == TRUE) {
    US_data <- error2000(us, names, "US")
  } else {
    names(us) <- c("place",names)
    US_data <-us
    US_data <- US_data %>% select(-last_col())
  }
  
  
  df <- switch(rbind(LA_data, US_data))
  df <- df %>% mutate(placename = case_when(place == "051" ~ "Jefferson",
                                            place == "071" ~ "Orleans",
                                            place == "075" ~ "Plaquemines",
                                            place == "087" ~ "St. Bernard",
                                            place == "089" ~ "St. Charles",
                                            place == "093" ~ "St. James",
                                            place == "095" ~ "St. John the Baptist",
                                            place == "MSA_2023" ~ "New Orleans Metro Area",
                                            place == "1" ~ "United States"))
  return(df)
}


error2000 <- function(data2000, names, geo_df){
  
  names_2000 <- paste(names, "2000", sep = "_")
  names_2000 <- c("place", names_2000, "POP")
  names(names_2000) <- colnames(data2000) 
  
  if(geo_df == "LA"){
    
    df <- data2000 %>%
      pivot_longer(cols = -county, names_to = "var", values_to = "val") %>% 
      mutate(var_sum = case_when(var == "POP" ~ "pop",
                                 T ~ str_sub(var, 1, -4))) %>%
      left_join(Census2000_designfac, by = c("var_sum" = "table_name"))
    
    df_totals <- df %>% filter(var_sum == "pop") %>% pivot_wider(names_from = var, values_from = val) %>% select(-c(var_sum, LA_df, US_df))
    
    data_witherror <- df %>% filter(var_sum != "pop") %>% left_join(df_totals, by = "county") %>% 
      mutate(MOE = moe2000(val, POP, LA_df)) %>% 
      pivot_longer(cols = c(val, MOE), names_to = "var_type", values_to = "val") %>%
      pivot_wider(names_from = c(var, var_type), names_sep = "", values_from = val) %>%
      select(-c(var_sum, LA_df, US_df)) 
    
  }
  
  if(geo_df == "US"){
    
    df <- data2000 %>%
      pivot_longer(cols = -county, names_to = "var", values_to = "val") %>% 
      mutate(var_sum = case_when(var == "POP" ~ "pop",
                                 T ~ str_sub(var, 1, -4))) %>%
      left_join(Census2000_designfac, by = c("var_sum" = "table_name"))
    
    df_totals <- df %>% filter(var_sum == "pop") %>% pivot_wider(names_from = var, values_from = val) %>% select(-c(var_sum, LA_df, US_df))
    
    data_witherror <- df %>% filter(!(var_sum %in% c("total", "pop"))) %>% left_join(df_totals, by = "county") %>% 
      mutate(MOE = moe2000(val, POP, US_df)) %>% 
      pivot_longer(cols = c(val, MOE), names_to = "var_type", values_to = "val") %>%
      pivot_wider(names_from = c(var, var_type), names_sep = "", values_from = val) %>%
      select(-c(var_sum, LA_df, US_df)) 
    
  }
  
  colnames_vec <- colnames(data_witherror) %>%
    str_c(collapse = " ") %>%
    str_replace_all(c(names_2000)) %>%
    #str_replace_all(c("total" = as.character(names_2000[2]))) %>% 
    str_replace_all(c("val" = "")) %>%
    str_split(" ")
  
  colnames(data_witherror) <- colnames_vec[[1]]
  
  return(data_witherror)
}

########## Define function to pull variables

# Pull data. Note that this includes 2010-2019.
pullDataPEP <- function(variables, api, year, counties, metro) {
  parish <- getCensus(name = api, 
                      vintage = 2019, 
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                      vars = variables, 
                      region = counties, 
                      regionin = "state:22")
  
  state <- getCensus(name = api, 
                     vintage = 2019, 
                     key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                     vars = variables, 
                     region = "state:22")
  
  usa <- getCensus(name = api, 
                   vintage = 2019, 
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

## calculates MOE for 2000 STF3 files.
## Formula on pg954 of documentation, table A "Unadjusted Standard Error for Estimated Totals"
## This is only for estimate totals and percentages.  Medians and sums will have to be done differently!!!***
## N = population of the geography
## Design factor table not found, so until we multiply by design factor, it's unadjusted std error.
## critical value for ACS is z = 1.645 (they use 90% CI)
moe2000 <- function(est, n, designfac = 1){
  se_unadj <- sqrt((5*est)*(1 - (est/n)))
  se <- se_unadj * designfac #when designfac = 1 it produces the unadjusted standard errors 
  MOE <- se*1.645
  return(MOE)
}

moeprop2000 <- function(prop, n, designfac = 1){
  se_unadj <- sqrt((5/n)*(prop)*(100 - prop))
  se <- se_unadj * designfac #when designfac = 1 it produces the unadjusted standard errors 
  MOE <- se*1.645
  return(MOE)
}

moemedian2000 <- function(frequency_distribution, designfac = 1){
  base <- 1 #get this from frequency distribution table?
  se_unadj <- sqrt((5/base) * (50^2))
  se <- se_unadj * designfac
  MOE <- se * 1.645
  return(MOE)
}


##calculates MOE for aggregated estimates (this works the same for 2000 or current years)
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
stattest <- function(x, moex = matrix(0, length(x)), y, moey, zscore = 1.96){
  significant <- matrix(0, length(x))
  v <- abs((x-y)/sqrt((moex/zscore)^2+(moey/zscore)^2))
  significant <- ifelse(v>zscore,"yes","no")
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
                      year = "2023",
                      digits = 0,
                      lab_pos = position_dodge(width = .7)){     #for rounding, specifically for forbor
  dataGraphic <-  data %>% select(-contains("moeprop")) %>%      #dplyr rejects the format of moeprop, so we drop it  mutate(placenames = NA,
    mutate(placenames = NA,
           placenames = ifelse(place == "051", "Jefferson", placenames),
           placenames = ifelse(place == "071", "Orleans", placenames),
           placenames = ifelse(place == "35380","Metro",placenames),
           placenames = ifelse(place == "1", "U.S.", placenames)) %>%
    mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%     #vars of type "factor" allow you to control order
    select(one_of("census2000", "sf2004", "sf1999", "Ownerpct2000","pcthwc2000","pcthwc"), !!stattograph, placenames, place.fac, significant) %>%     #one_of() chooses correct comparison vals/!! is the second part of the quo() tool
    gather(-placenames,-place.fac, -significant, key=variable, value=value) %>% 
    mutate(description = as.factor(ifelse(variable == "census2000"|variable =="sf2004"|variable =="sf1999" | variable == "Ownerpct2000", comparisonyear, year))) %>%     #creates legend info
    mutate(valp = case_when(value == 0 ~ "  ",
                                    value < .01 & significant == "no"  ~ "<1%*",
                                    value < .01 & significant == "yes"  ~ "<1%",
                                    value > .01 & significant == "yes" ~ paste0(round(value*100, digits = digits), "%"),
                                    value > .01 & significant == "no"  ~ paste0(round(value*100, digits = digits), "%*"))) %>%
    
    mutate(vald = case_when(value == 0 ~ "   ",
                            significant == "no" ~  paste0(dollar(value, largest_with_cents = 1),"*"),
                            significant == "yes" ~ dollar(value, largest_with_cents = 1)))
  
  chart <- dataGraphic %>% 
    ggplot(aes(place.fac, value, fill=description)) + 
    geom_bar(stat="identity",
             position = position_dodge(),
             width = .7,
             color='gray50') +    #bar outline
    geom_text(#data = subset(dataGraphic, as.numeric(value) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
      data = dataGraphic,        
      aes(label = ifelse(rep(pct,sum(dataGraphic$value>=0)), 
                                 valp,
                                 vald)), 
              position=lab_pos, 
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


### for stat testing notes under new charts ###

### for he by race/by geography bar charts
raceList <- function(data){
  data %>%
    mutate(insigList = "",
           insigList = ifelse(sig_wht_blk == "no", "White and Black", insigList),
           insigList = ifelse(sig_wht_asian == "no", paste0(insigList,"White and Asian", sep = ", "), insigList),
           insigList = ifelse(sig_wht_hisp == "no", paste0(insigList,"White and Hispanic",sep = ", "), insigList),
           insigList = ifelse(sig_blk_hisp == "no", paste0(insigList,"Black and Hispanic",sep = ", "), insigList),
           insigList = ifelse(sig_blk_asian  == "no", paste0(insigList, "Black and Asian", sep = ", "), insigList),
           insigList = ifelse(sig_hisp_asian == "no", paste0(insigList, "Hispanic and Asian", sep = ", "), insigList),
           insigList = str_sub(insigList,1,-3)) %>%
    mutate(placename = ifelse(placename %in% c("Orleans", "Jefferson", "St. Tammany"), paste0(placename, " Parish"), placename)) %>%
    filter(insigList != "") %>%
    mutate(note = paste0("&#8224; = In ", placename, ", the difference between ", insigList, " is not statistically significant.")) %>%
    select(placename,note) %>%
    pivot_wider(names_from = "placename", values_from = "note") %>%
    unite("note", 1:dim(.)[2]) %>%
    as.data.frame()
}

round.off <- function (x, digits=0) 
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}
