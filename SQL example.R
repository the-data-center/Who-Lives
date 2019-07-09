library(RODBC)
library(tidyverse)
library(dplyr)

IDS <- odbcConnect("IDS", uid = "rweinstein", pwd = "Stinge4s12")
sqlColumns(IDS, "wholives.AllParishesRaw")
sqlWL <- sqlQuery(IDS, "SELECT * FROM wholives.AllParishesRaw WHERE WhoLivesYear = 2018")
allparishesRaw <- sqlWL
#Table 1 
AAWhiteHispan <- allparishesRaw %>% 
  filter(PlaceName == "Orleans Parish") %>% 
  filter(DateDesc == "7/1/2018 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total" & (RaceSimple == "Black"|RaceSimple == "White"|RaceSimple == "Hispanic")) %>%
  mutate(est2000=c(128871, 323392, 14826)) %>% #check order of races in data frame. Order is bottom up
  select(RaceSimple, Population, est2000) %>%
  arrange(-row_number())

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

####### AAWhiteHispan using 2017 data
#sqlWL <- sqlQuery(IDS, "SELECT * FROM wholives.AllParishesRaw WHERE WhoLivesYear = 2017")
#allparishesRaw <- sqlWL
##Table 1 
#AAWhiteHispan <- allparishesRaw %>% 
#  filter(PlaceName == "Orleans Parish") %>% 
#  filter(DateDesc == "7/1/2017 population estimate") %>% 
#  filter(AgeGroupName == "Total" & SexName == "Total" & (RaceSimple == "Hispanic"|RaceSimple == "White"|RaceSimple == "Black")) %>%
#  mutate(est2000=c(128871, 323392, 14826)) %>%
#  select(RaceSimple, Population, est2000) %>%
#  arrange(-row_number())
#
#AAwhthispGraphic <- AAWhiteHispan %>%
#  mutate(race.fac = factor(.$RaceSimple,levels = c("Black", "White","Hispanic"))) %>%
#  select(est2000, Population, RaceSimple, race.fac) %>%
#  gather(-RaceSimple,-race.fac, key=variable, value =val) %>%
#  mutate(description = ifelse(variable == "est2000", "2000", "2017")) %>%
#  ggplot(aes(race.fac, val, fill=description)) +
#  geom_bar(stat="identity",
#           position = position_dodge(),
#           width = .7,
#           color="gray50") +
#  geom_text(aes(label = comma(val)), position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
#  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,350000)) +
#  scale_fill_manual(values = c(DCcolor.p1skyblue,
#                               DCcolor.p1mediumblue)) +
#  themeDC_horizontal() +
#  theme(legend.title = element_blank(),
#        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
#        plot.title = element_text(hjust = .5)) +
#  labs(title = "African American, white, and Hispanic population, Orleans Parish",
#       x="",
#       y="")

#Tables 2

ParishDemo1<- allparishesRaw %>% 
  filter(DateDesc == "7/1/2018 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total")  %>% 
  select(PlaceName, Population, RaceSimple)

#Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro
ParishDemo2<- allparishesRaw %>% 
  filter(DateDesc == "7/1/2018 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total") %>% 
  filter(PlaceName != "Louisiana" & PlaceName!= "United States")%>% 
  group_by(RaceSimple)%>%
  summarise(Population=sum(Population)) %>%
  mutate(PlaceName = c("Metro", "Metro", "Metro", "Metro", "Metro")) %>%
  bind_rows(.,ParishDemo1)

#reshape data from long to wide for easy analysis
ParishDemo <- spread(ParishDemo2, RaceSimple, Population) %>%
  mutate(pctwhite = White / Total,
         pctblack = Black / Total,
         pctasian = Asian / Total,
         pcthisp = Hispanic / Total,
         white2000=c(.645,0,.547,.266,.688,.844,.705,.497,.51,.853,.691),
         black2000=c(.227,0,.373,.667,.233,.076,.251,.492,.446,.098,.121),
         asian2000=c(.031,0,.021,.023,.026,.013,.006,0,.005,.008,.037),
         hispanic2000=c(.071,0,.044,.031,.016,.051,.028,.006,.029,.025,.125)) %>%
  .[-2,]


##Graphic


ParishDemoforGraphic <- ParishDemo %>%
  select(PlaceName,
         contains('pct'),
         contains('2000')) %>%
  mutate(place.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson", "Plaquemines", "St. Bernard","St. Charles", "St. James", "St. John the Baptist", "St. Tammany", "Metro", "United States"))) %>%
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
  facet_wrap(~place.fac, ncol = 2, scales = "free") + 
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
  labs(title = element_blank(),
       x="",
       y="") 







# ##Historical AA
# 
# 
# #Table 3 African American Population, New Orleans, 2000-2017
# ##This needs to be reworked because the new allparishraw doesn't include historica
# #Pulling population estimates for 2010-2017
# AAhistorical1 <- allparishesRaw %>% 
#   filter(PlaceName == "Orleans Parish")%>% 
#   filter(RaceSimple=="Black")%>% 
#   filter(SexName=="Total")%>% 
#   filter(AgeGroupName=="Total")%>% 
#   select(DateDesc, Population) %>%
#   .[-(2:3),] %>% #Remove 2010 estimates we don't need. We use Census Population for 2010 so we can delete 2010 population estimate
#   bind_rows(data.frame(Population = c(323392,0,0,0,0,0,133015,159887,181882,197337), row.names = (NULL)), .) %>%
#   select(Population) %>%
#   bind_cols(data.frame(year = as.factor(c(2000:2017))), .)
# 
# ##2018
# 
# AAhistorical <- allparishesRaw %>% 
#   filter(PlaceName == "Orleans Parish")%>% 
#   filter(RaceSimple=="Black")%>% 
#   filter(SexName=="Total")%>% 
#   filter(AgeGroupName=="Total")%>% 
#   mutate(year= "2018",
#          year=(as.factor(year))) %>% 
#   select(year, population) %>% 
#   bind_rows(AAhistorical1)
# 
# 
# #######AA historical part 2
# BlackPopyears <- allparishesRawx %>% 
#   filter(age == "Total")  %>% 
#   filter(raceSimple=="Black") %>%
#   filter(place=="Jefferson"|place=="Orleans"|place=="Plaquemines"|place=="St. Bernard"|place=="St. Charles"|place=="St. James"|
#            place=="St. John the Baptist"|place=="St. Tammany") 
# 
# load("inputs/blackpopestRaw.Rdata")
# BlackpopM <- blackpopestRaw %>% 
#   add_row(year = 2000, place= "Orleans", POP=323392) # 