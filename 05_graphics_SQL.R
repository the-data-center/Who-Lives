library(extrafont)
library(scales)
library(directlabels)
library(grid)
library(here)
source(here("inputs/datacenter_colors.R"))

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
                      year = "2018",
                      digits = 0){     #for rounding, specifically for forbor
  dataGraphic <-  data %>% select(-contains("moeprop")) %>%      #dplyr rejects the format of moeprop, so we drop it
    mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
    mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%     #vars of type "factor" allow you to control order
    select(one_of("census2000", "sf2004", "sf1999"), !!stattograph, PlaceNames, PlaceName.fac, significant) %>%     #one_of() chooses correct comparison vals/!! is the second part or the quo() tool
    gather(-PlaceNames,-PlaceName.fac, -significant, key=variable, value=value) %>% 
    mutate(description = ifelse(variable == "census2000"|variable =="sf2004"|variable =="sf1999", comparisonyear, year)) %>%     #creates legend info
    mutate(valp = ifelse(value<.01,ifelse(significant == "no" & description == year, "<1%*", "<1%"),     #creates pct labels
                         paste0(round(value*100, digits = digits),"%",ifelse((significant == "no" & description == year), "*", "")))) %>%
    mutate(vald = ifelse((significant == "no" & description == year),      #creates dollar labels
                         paste0(dollar(value, largest_with_cents = 1),"*"), 
                         dollar(value, largest_with_cents = 1)))
  
  chart <- dataGraphic %>% 
    ggplot(aes(PlaceName.fac, value, fill=description)) + 
    geom_bar(stat="identity",
             position = position_dodge(),
             width = .7,
             color='gray50') +    #bar outline
    geom_text(data = subset(dataGraphic, as.numeric(value) != 0),     #leave out labels where data point doesn't exist (is PlaceNameheld with 0)
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


############################################
# # RACE/ETHNICITY # #
############################################


#### 1 - African American, white, and Hispanic population ####

AAwhthispGraphic <- AAWhiteHispan %>%
  mutate(race.fac = factor(.$RaceSimple,levels = c("Black", "White","Hispanic"))) %>%
  select(est2000, Population, RaceSimple, race.fac) %>%
  gather(-RaceSimple,-race.fac, key=variable, value =val) %>%
  mutate(description = ifelse(variable == "est2000", "2000", "2018")) %>%
  ggplot(aes(race.fac, val, fill=description)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(val)), position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,350000)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue,
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)) +
  labs(title = "African American, white, and Hispanic population, Orleans Parish",
       x="",
       y="")


#### 2 -  Demographic bar charts for 8 parishes, metro, and US ####

ParishDemoforGraphic <- ParishDemo %>%
  select(PlaceName,
         contains('pct'),
         contains('2000')) %>%
  filter(PlaceName != "Louisiana") %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans Parish", "Jefferson Parish", "Plaquemines Parish",
                                                       "St. Bernard Parish","St. Charles Parish", "St. James Parish", 
                                                       "St. John The Baptist Parish", "St. Tammany Parish", "Metro", "United States"))) %>%
  gather(key = variable, value = value, contains("pct"), contains("2000")) %>% 
  mutate(description  = NA,
         description = ifelse(grepl("pct",variable), 2018, description),     #if variable contains 'pct'
         description = ifelse(grepl("2000", variable), 2000, description)) %>%     #if variable contains '2000'
  mutate(description.fac = factor(.$description, levels = c( "2000",
                                                             "2018")))%>%
  mutate(race = NA,
         race = ifelse(grepl("white", variable),"White", race),
         race = ifelse(grepl("black", variable),"Black", race),
         race = ifelse(grepl("hisp", variable),"Hispanic", race),
         race = ifelse(grepl("asian", variable),"Asian", race)) %>%
  mutate(race.fac = factor(.$race, levels = c("White",
                                              "Black",
                                              "Hispanic",
                                              "Asian"))) %>%
  mutate(val = ifelse(as.numeric(value)<.01,"<1%",paste0(round(as.numeric(value)*100),"%")))

chart.demo.allparishes <- ParishDemoforGraphic %>%
  ggplot(aes(race.fac, value, fill=description.fac, label = val)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 2, scales = "free") + 
  geom_text(aes(label = val), position=position_dodge(width = .7), vjust = -.7, size=4, family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue, 
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        panel.spacing = unit(6, "lines"),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12, vjust=1),
        axis.text.y = element_text(size = 12)) +
  labs(title = "White, black, Hispanic, and Asian, Metro New Orleans parishes and U.S",
       x="",
       y="") 

#### 3 - African American population, New Orleans

AAhistGraphic <- AAhistorical %>%
  ggplot(aes(year, Population, label = comma(Population))) +
  geom_bar(stat="identity", fill = DCcolor.p2blue, width = .7) +
  geom_text(data = subset(AAhistorical, Population != 0),      #remove labels for years without data
            size = 3.75,
            position = position_stack(vjust = 1.05), 
            family="Asap") + 
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0,350000)) + 
  themeDC_horizontal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 24)) +
  labs(title = "African American population, New Orleans",
       x="",
       y="") 


####4 - -Hispanic population change by parish

HispanicPopforGraphic  <- HispanicPop %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans Parish", "Jefferson Parish", "St. Tammany Parish", "Plaquemines Parish", 
                                                       "St. Bernard Parish","St. Charles Parish", "St. James Parish", "St. John The Baptist Parish"))) %>%
  gather(-PlaceName,-PlaceName.fac,key = variable, value = value) %>%
  mutate(description = ifelse(variable == "est2000", "2000", "2018")) %>%
mutate(description.fac = factor(.$description, levels = c( "2018",
                                                           "2000")))

HispanicPopGraphic <- HispanicPopforGraphic %>%
  ggplot(aes(PlaceName.fac, value, fill=description.fac, label = comma(value))) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(value)),position=position_dodge(width = .7), vjust = .5, hjust = -.5, size=2.75, family="Asap") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0,75000)) + 
  scale_fill_manual(values = c(DCcolor.p1mediumblue,
                               DCcolor.p1skyblue)) + 
  themeDC_vertical() +
  coord_flip()+     #it's sideways
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),  
        axis.text.x = element_text(size = 12, vjust=1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5)) + 
  labs(title = "Hispanic population change by parish",
       x="",
       y="")


####5- Hispanic population of parishes in metro by year

HispanpopYearsforGraphic <- HISPpopM  %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("St. James Parish", "Plaquemines Parish", "St. John The Baptist Parish", 
                                                       "St. Charles Parish", "St. Bernard Parish", "St. Tammany Parish", "Orleans Parish", "Jefferson Parish"))) %>%
add_row(CensusYear = 2001, PlaceName.fac = "Jefferson Parish", Population = 0) %>%
  add_row(CensusYear = 2002, PlaceName.fac = "Jefferson Parish", Population = 0) %>%
  add_row(CensusYear = 2003, PlaceName.fac = "Jefferson Parish", Population = 0) %>%
  add_row(CensusYear = 2004, PlaceName.fac = "Jefferson Parish", Population = 0) %>%
  add_row(CensusYear = 2005, PlaceName.fac = "Jefferson Parish", Population = 0)

chart.HispanpopYears.allparishes <- HispanpopYearsforGraphic %>%
  ggplot(aes(CensusYear, as.numeric(Population), fill=PlaceName.fac)) +
  geom_bar(stat="identity", 
           position="stack", 
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2orangered,
                               DCcolor.p2orange,
                               DCcolor.p2green,
                               DCcolor.p2teal,
                               DCcolor.p2purple,
                               DCcolor.p2violet,
                               DCcolor.p2limegreen,
                               DCcolor.p1darkblue90)) +  
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,130000)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12), 
        legend.spacing.y = unit(10, "lines"),
        plot.title = element_text(hjust = .5, size = 24),
        axis.text.x = element_text(size = 10, vjust=1),
        axis.text.y = element_text(size = 12)) +
  labs(title = "Hispanic population by year, Metro",
       x="",
       y="")


####6 - Hispanic Origin, 2018

hispan2018 <- hispan %>%
  select(PlaceName, 
         Cubanpct,
         Dominicanpct,
         Mexicanpct,
         PuertoRicanpct,
         Honduranpct,
         Guatemalanpct,
         Nicaraguanpct,
         Salvadoranpct,
         OtherCApct,
         SouthAmericanpct,
         Otherpct,
         contains('SIG')) %>%
  mutate(PlaceNames = c( "Orleans","Jefferson", "Metro", "United States"))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c( "Orleans", "Jefferson","Metro","United States"))) %>%
  gather(-PlaceName, -PlaceName.fac, -contains('SIG'), key=variable, value = value) %>% 
  .[-(45:48),] %>%
  mutate(description = NA,
         description = ifelse(variable == "Cubanpct", "Cuban", description),
         description = ifelse(variable == "Dominicanpct", "Dominican", description),
         description = ifelse(variable == "Mexicanpct", "Mexican", description),
         description = ifelse(variable == "PuertoRicanpct", "Puerto Rican", description),
         description = ifelse(variable == "Honduranpct", "Honduran", description),
         description = ifelse(variable == "Guatemalanpct", "Guatemalan", description),
         description = ifelse(variable == "Nicaraguanpct", "Nicaraguan", description),
         description = ifelse(variable == "Salvadoranpct", "Salvadoran", description),
         description = ifelse(variable == "OtherCApct", "Other Central American", description),
         description = ifelse(variable == "SouthAmericanpct", "South American", description),
         description = ifelse(variable == "Otherpct", "Other", description)) %>%
  mutate(description.fac = factor(.$description, levels = c("Puerto Rican",
                                                            "Cuban",
                                                            "Dominican",
                                                            "Mexican",
                                                            "Guatemalan",
                                                            "Honduran",
                                                            "Salvadoran",
                                                            "Nicaraguan",
                                                            "Other Central American",
                                                            "South American",
                                                            "Other")))%>% 
  mutate(val = ifelse(as.numeric(value)<.01,     #is too crowded with <.01
                      "",
                      paste0(round(as.numeric(value)*100),"%",ifelse((variable == "Cubanpct" & CubanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Dominicanpct" & DominicanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Mexicanpct" & MexicanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "PuertoRicanpct" & PuertoRicanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Honduranpct"& HonduranSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Guatemalanpct" & GuatemalanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Nicaraguanpct" & NicaraguanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Salvadoranpct" & SalvadoranSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "OtherCApct" & OtherCASIG == "no"& PlaceName != "United States")
                                                                     |(variable == "SouthAmericanpct" & SouthAmericanSIG == "no"& PlaceName != "United States")
                                                                     |(variable == "Otherpct" & OtherSIG == "no"& PlaceName != "United States"), "*", ""))))


chart.hispan2018.allparishes <- hispan2018 %>% 
  ggplot(aes(x=PlaceName.fac, y=as.numeric(value), label = val, fill=description.fac)) + #,
  geom_bar(stat="identity", 
           position=position_stack(),
           width = .7,
           color="gray50") +
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                               DCcolor.p2blue,
                               DCcolor.p2teal,
                               DCcolor.p2green,
                               DCcolor.p2limegreen,
                               DCcolor.p2yellow,
                               DCcolor.p2orange,
                               DCcolor.p2orangered,
                               DCcolor.p2magenta,
                               DCcolor.p2violet,
                               DCcolor.p2purple)) +
  geom_text(size = 3, position =position_stack(vjust = 0.5), family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_vertical() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12), 
        plot.title = element_text(hjust = .5, size = 14)) +
  labs(title = "Hispanic origin, 2018",
       x="",
       y="")
############################################
# # POPULATION BY AGE AND HOUSEHOLD TYPES # #
############################################


####7 - Population by age group, 2000
agepop2000forGraphic <- Agepop %>%
  select(-Population) %>% 
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("St. John The Baptist Parish","St. James Parish", "St. Charles Parish", 
                                                       "St. Bernard Parish", "Plaquemines Parish", "St. Tammany Parish","Jefferson Parish","Orleans Parish"))) %>%
  mutate(age.fac = factor(.$AgeGroupName, levels = c("Under 5 years", "5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 plus")))

chart.agepop2000.allparishes <- agepop2000forGraphic %>%
  ggplot(aes(age.fac, as.numeric(est2000), fill=PlaceName.fac)) +
  geom_bar(stat="identity",
           position="stack",
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2teal50,
                               DCcolor.p2teal,
                               DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2blue70,
                               DCcolor.p1mediumblue,
                               DCcolor.p2blue90,
                               DCcolor.p1darkblue90)) + 
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,120000)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),  
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family="Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16)) +
  labs(title = "Population by age group, 2000",
       x="",
       y="")
  
####8 - Population by age group, 2018

agepop2018forGraphic <- Agepop %>%
  select(-est2000) %>% 
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("St. John The Baptist Parish","St. James Parish", "St. Charles Parish", 
                                                       "St. Bernard Parish", "Plaquemines Parish", "St. Tammany Parish","Jefferson Parish","Orleans Parish"))) %>%
  mutate(age.fac = factor(.$AgeGroupName, levels = c("Under 5 years", "5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 plus")))

chart.agepop2018.allparishes <- agepop2018forGraphic %>%
  ggplot(aes(age.fac, as.numeric(Population), fill=PlaceName.fac)) +
  geom_bar(stat="identity", 
           position="stack", 
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2teal50,
                               DCcolor.p2teal,
                               DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2blue70,
                               DCcolor.p1mediumblue,
                               DCcolor.p2blue90,
                               DCcolor.p1darkblue90)) +  
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,120000)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),  
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family="Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16))+
  labs(title = "Population by age group, 2018",
       x="",
       y="")
####9 - Households with own children under 18

hwcGraphic <- dodgedBar(hwc, 
                        quo(pcthwc), 
                        "Households with own children under 18")

####10 -  Single Person Households

singGraphic <- dodgedBar(sing, 
                         quo(pctsing), 
                         "Single-person households",
                         yscale = c(0,.55))

####11 - Under 18 population

popunder18forGraphic <- popunder18 %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans Parish", "Jefferson Parish","St. Tammany Parish","Metro"))) %>%
  gather(-PlaceName,-PlaceName.fac, key=variable, value =val) %>% 
  mutate(description = ifelse(variable == "est2000", "2000", "2018"))

popunder18Graphic <- popunder18forGraphic %>%
  ggplot(aes(PlaceName.fac, val, fill=description, label = comma(val))) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,400000)) + 
  scale_fill_manual(values = c(DCcolor.p1skyblue, DCcolor.p1mediumblue)) + 
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)) + 
  labs(title = "Under 18 population",
       x="",
       y="")
  
############################################
# # EDUCATIONAL ATTAINMENT, INCOME, AND INTERNET ACCESS # #
############################################

####12 - Less than a high school degree, adults 25 and older

hsGraphic <- dodgedBar(hs, 
                       quo(pctless), 
                       "Less than a high school degree, adults 25 and older", 
                       yscale = c(0,.35))

####13 - Bachelor's degree or higher, adults 25 and older

bachGraphic <- dodgedBar(bach, 
                         quo(pctbach), 
                         "Bachelor's degree or higher, adults 25 and older")

####14 - Median household income, 2016 inflation-adjusted dollars

medhhGraphic <- dodgedBar(medhh, 
                          quo(MedianHHIncome), 
                          "Median household income, 2018 inflation adjusted", 
                          yscale = c(0,1.3*max(medhh$MedianHHIncome)), 
                          colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue), 
                          pct = FALSE,  
                          comparisonyear = "1999")

####15 - Internet Access

intaforGraphic <- inta %>% 
  select(PlaceName,
         contains('pct'),
         contains('SIG')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  gather(key = variable, value = value, contains("pct")) %>% 
  mutate(description = NA,
         description = ifelse(variable == "broadbandpct", "Broadband and all other", description),
         description = ifelse(variable == "noaccpct", "No Internet access", description),
         description = ifelse(variable == "cellonlypct", "Cellular only", description),
         description = ifelse(variable == "nosubpct", "Access with no subscription", description)) %>%
  mutate(description.fac = factor(.$description, levels = c( "Access with no subscription",
                                                             "Cellular only",
                                                             "No Internet access",
                                                             "Broadband and all other")))%>% 
  mutate(val = ifelse(value<.01,ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "<1%*", ""),
                      paste0(round(value*100),"%",ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "*", ""))))


#<1%",paste0(round(value*100),"%")))

chart.inta.allparishes <- intaforGraphic %>% 
  ggplot(aes(PlaceName.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", position="fill", color = "gray50") +
  scale_fill_manual(values = c(DCcolor.p2orangered,
                               DCcolor.p2yellow,
                               DCcolor.p2violet,
                               DCcolor.p1skyblue)) +
  geom_text(size = 3, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10), 
        plot.title = element_text(hjust = .5)) +
  labs(title = "Internet access, 2018 households",
       x="",
       y="") 
############################################
# # POVERTY AND ACCESS TO VEHICLES # #
############################################


####16 - Poverty rate, population for whom poverty has been determined

povGraphic <- dodgedBar(pov, 
                        quo(pctpov), 
                        "Poverty rate, population for whom poverty has been determined", 
                        yscale = c(0,.35), 
                        colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
                        comparisonyear = "1999")

####17 - Children in poverty, population for whom poverty has been determined

childpovGraphic <- dodgedBar(childpov, 
                             quo(pctBelowChildPov), 
                             "Children in poverty, population for whom poverty has been determined",
                             colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
                             comparisonyear = "1999")

####18 - Households without access to a vehicle

vehGraphic <- dodgedBar(veh, 
                        quo(vehpct), 
                        "Households without access to a vehicle", 
                        yscale = c(0,.35))

############################################
# # FOREIGN-BORN POP # #
############################################

####19 - Population not U.S. citizens at birth

forborGraphic <- dodgedBar(forbor, quo(forborpct),"Population not U.S. citizens at birth", yscale = c(0,.18), digits = 1)

####20 - Population who moved in the past year

mobforGraphic <- mob %>% 
  select(PlaceName, 
         contains('SIG'),
         contains('pct'),
         contains('2004')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-samehousepct, -sf2004samehouse) %>% 
  gather(key = variable, value = value, contains("pct"), contains("2004")) %>% 
  mutate(description = NA,
         description = ifelse(variable == "mobabroadpct"|variable == "sf2004mobabroad", "Moved from abroad", description),
         description = ifelse(variable == "mobStatespct"|variable == "sf2004states", "Moved from out of state", description),
         description = ifelse(variable == "difparishpct"|variable == "sf2004difparish", "Moved from different parish in state", description),
         description = ifelse(variable == "withinparishpct"|variable == "sf2004withinparish", "Moved within same parish", description)) %>%
  mutate(year = NA,
         year = ifelse(variable == "mobabroadpct"|variable == "mobStatespct"|variable == "difparishpct"|variable == "withinparishpct", 2018, year),
         year = ifelse(variable == "sf2004mobabroad"|variable == "sf2004states"|variable == "sf2004difparish"|variable == "sf2004withinparish", 2004,year)) %>%
  mutate(description.fac = factor(.$description, levels = c( "Moved from abroad",
                                                             "Moved from out of state",
                                                             "Moved from different parish in state",
                                                             "Moved within same parish")))%>%
  mutate(year.fac = factor(.$year, levels = c("2004",
                                              "2018"))) %>%
  mutate(val = ifelse(value<.01,ifelse((abroadSIG == "no" & variable == "mobabroadpct")
                                              |(statesSIG ==  "no" & variable == "mobStatespct")
                                              |(difparishSIG ==  "no" & variable == "difparishpct")
                                              |(withinparishSIG ==  "no" & variable == "withinparishpct"), "<1%*", "<1%"),
         paste0(round(value*100),"%",ifelse((abroadSIG == "no" & variable == "mobabroadpct")
                                                                         |(statesSIG ==  "no" & variable == "mobStatespct")
                                                                         |(difparishSIG ==  "no" & variable == "difparishpct")
                                                                         |(withinparishSIG ==  "no" & variable == "withinparishpct"), "*", ""))))

chart.mob.allparishes <- mobforGraphic %>% 
  ggplot(aes(year.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="stack",
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 5) + 
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2teal,
                               DCcolor.p2blue90)) +
  geom_text(data = subset(mobforGraphic, 
                          as.numeric(value) != 0), 
            size = 4, 
            position = position_stack(vjust = 0.6), 
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.18)) +
  scale_x_discrete(labels = c("2004","2018"))+
  themeDC_horizontal() +
  theme(plot.title = element_text(hjust = .5, size = 18),
        strip.text = element_text(size=12),
        legend.position = "right",
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),  
        axis.text.x = element_text(size = 12, vjust = 1),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank()) +
  labs(title = "Population who moved in the past year",
       x="",
       y="") 


############################################
# # HOMEOWNERSHIP # #
############################################

####21 - Homeownership rates

hoGraphic <- dodgedBar(ho, 
                       quo(Ownerpct), 
                       "Homeownership rates", 
                       yscale = c(0,.85))

####22 - Homeowners without a mortgage

honomoGraphic <- dodgedBar(honomo, 
                           quo(honomopct), 
                           "Homeowners without a mortgage", 
                           yscale = c(0,.55))

############################################
# # HOUSING COSTS & COMMUTING # #
############################################

####23 - Renters with severe housing cost burdens

rentburGraphic <- dodgedBar(rentbur, 
                            quo(rentburpct), 
                            "Renters with severe housing cost burdens",
                            colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                            comparisonyear = "2004")

####24 - Homeowners with severe housing cost burdens

hoburGraphic <- dodgedBar(hobur, 
                          quo(hoburpct), 
                          "Homeowners with severe housing cost burdens", 
                          yscale = c(0,.2), 
                          colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                          comparisonyear = "2004")

####25 - Median gross rent, inflation-adjusted dollars

medrentGraphic <- dodgedBar(medrent, 
                            quo(Rent), 
                            "Median gross rent, inflation-adjusted dollars",
                            yscale = c(0,1.2*max(medrent$Rent)), 
                            pct = FALSE, 
                            colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                            comparisonyear = "2004")

####26 - Year structure built, 201* housing units

yrbuiltforGraphic <- yrbuilt %>% 
  select(contains('pct'),
         contains('SIG')) %>%
  mutate(PlaceName = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  gather(-PlaceName, -PlaceName.fac, -contains('SIG'), key=variable, value = value) %>% 
  mutate(description = NA,
         description = ifelse(variable == "orLater1990pct", "1990 or later", description),
         description = ifelse(variable == "mid1950to1989pct", "1950-1989", description),
         description = ifelse(variable == "orbefore1949pct", "1949 or earlier", description)) %>%
  mutate(description.fac = factor(.$description, levels = c( "1990 or later",
                                                             "1950-1989",
                                                             "1949 or earlier")))%>% 
  mutate(val = ifelse(as.numeric(value)<.01,ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.")
                                                   |(variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.")
                                                   |(variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "<1%*", "<1%"),
                      paste0(round(as.numeric(value)*100), "%",ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.")
                                                                      |(variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.")
                                                                      |(variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "*", ""))))


chart.yrbuilt.allparishes <- yrbuiltforGraphic %>% 
  ggplot(aes(PlaceName.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="fill",
           size = .7,
           color="gray50") +
  scale_fill_manual(values = c(DCcolor.p2teal,
                               DCcolor.p1skyblue,
                               DCcolor.p2orangered)) +
  geom_text(size = 3, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5)) +
  labs(title = "Year structure built, 2018 housing units",
       x="",
       y="") 

####27 Means of transportation to work, workers 16 years and older

commuteforGraphic <- commute %>% 
  select(PlaceName,
         contains('pct'),
         contains('2000'),
         contains('SIG')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  gather(key = variable, value = value, contains("pct"), contains("2000")) %>% 
  mutate(description = NA,
         description = ifelse(variable == "Drivepct"|variable == "census2000drive", "Drive Alone", description),
         description = ifelse(variable == "Carpoolpct"|variable == "census2000carpool", "Carpool", description),
         description = ifelse(variable == "PublicTransitpct"|variable == "census2000publictransit", "Public Transit", description),
         description = ifelse(variable == "bikepct"|variable == "census2000bike", "Bike", description),
         description = ifelse(variable == "Walkpct"|variable == "census2000walk", "Walk", description),
         description = ifelse(variable == "Workhomepct"|variable == "census2000workhome", "Work at home", description),
         description = ifelse(variable == "Otherpct"|variable == "census2000other", "Other", description)) %>%
  mutate(year = NA,
         year = ifelse(grepl("pct",variable), 2018, year),
         year = ifelse(grepl("2000", variable), 2000,year)) %>%
  mutate(description.fac = factor(.$description, levels = c("Drive Alone",
                                                            "Carpool",
                                                            "Public Transit", 
                                                            "Bike", 
                                                            "Walk", 
                                                            "Work at home",
                                                            "Other")))%>% 
  mutate(year.fac = factor(.$year, levels = c("2000",
                                              "2018"))) %>%
  mutate(val = ifelse(value<.02, "",
                    paste0(round(value*100),"%",ifelse((DriveSIG == "no" & variable == "Drivepct")
                                                       |(carpoolSIG ==  "no" & variable == "Carpoolpct")
                                                       |(PublicTransitSIG ==  "no" & variable == "PublicTransitpct")
                                                       |(bikeSIG ==  "no" & variable == "bikepct")
                                                       |(walkSIG ==  "no" & variable == "walkpct")
                                                       |(workhomeSIG ==  "no" & variable == "Workhomepct")
                                                       |(otherSIG ==  "no" & variable == "Otherpct"), "*", ""))))

chart.commute.allparishes <- commuteforGraphic %>% 
  ggplot(aes(year.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="fill",
           size = .7,
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 5) + 
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2blue,
                               DCcolor.p2teal,
                               DCcolor.p2green,
                               DCcolor.p2limegreen,
                               DCcolor.p2yellow)) +
  geom_text(size = 4, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) +
  scale_x_discrete(labels = c("2000",
                              "2018"))+
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust=.5)) +
  labs(title = "Means of transportation to work, workers 16 years and older",
       x="",
       y="") 

# #For checking graphics
# #ctrl+shift+c to un-#
AAwhthispGraphic
chart.demo.allparishes
AAhistGraphic
HispanicPopGraphic
chart.HispanpopYears.allparishes
chart.hispan2018.allparishes
chart.agepop2000.allparishes
chart.agepop2018.allparishes
hwcGraphic
singGraphic
popunder18Graphic
hsGraphic
bachGraphic
medhhGraphic
chart.inta.allparishes
povGraphic
childpovGraphic
vehGraphic
forborGraphic
chart.mob.allparishes
hoGraphic
honomoGraphic
rentburGraphic
hoburGraphic
medrentGraphic
chart.yrbuilt.allparishes
chart.commute.allparishes

