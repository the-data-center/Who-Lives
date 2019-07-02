IDS <- odbcConnect("DC2 IDS", uid = "jlosh", pwd = "Ax244!?bB12")
sqlColumns(channel, "wholives.AllParishesRaw")
sqlWL <- sqlQuery(channel, "SELECT * FROM wholives.AllParishesRaw WHERE WhoLivesYear = 2018")
allparishesRaw <- sqlWL
#Table 1 
AAWhiteHispan <- allparishesRaw %>% 
  filter(PlaceName == "Orleans Parish") %>% 
  filter(DateDesc == "7/1/2017 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total" & (RaceSimple == "Hispanic"|RaceSimple == "White"|RaceSimple == "Black")) %>%
  mutate(est2000=c(14826, 128871, 323392)) %>%
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

sqlWL <- sqlQuery(channel, "SELECT * FROM wholives.AllParishesRaw WHERE WhoLivesYear = 2017")
allparishesRaw <- sqlWL
#Table 1 
AAWhiteHispan <- allparishesRaw %>% 
  filter(PlaceName == "Orleans Parish") %>% 
  filter(DateDesc == "7/1/2017 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total" & (RaceSimple == "Hispanic"|RaceSimple == "White"|RaceSimple == "Black")) %>%
  mutate(est2000=c(128871, 323392, 14826)) %>%
  select(RaceSimple, Population, est2000) %>%
  arrange(-row_number())

AAwhthispGraphic <- AAWhiteHispan %>%
  mutate(race.fac = factor(.$RaceSimple,levels = c("Black", "White","Hispanic"))) %>%
  select(est2000, Population, RaceSimple, race.fac) %>%
  gather(-RaceSimple,-race.fac, key=variable, value =val) %>%
  mutate(description = ifelse(variable == "est2000", "2000", "2017")) %>%
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

#Tables 2

ParishDemo1<- allparishesRaw %>% 
  filter(date == "7/1/2017 population estimate") %>% 
  filter(age == "Total" & sex == "Total")  %>% 
  select(PlaceName, population, raceSimple)

#Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro
ParishDemo2<- allparishesRaw %>% 
  filter(date == "7/1/2017 population estimate") %>% 
  filter(age == "Total" & sex == "Total") %>% 
  filter(place != "Louisiana" & place!= "United States")%>% 
  group_by(raceSimple)%>%
  summarise(population=sum(population)) %>%
  mutate(place = c("Metro", "Metro", "Metro", "Metro", "Metro")) %>%
  bind_rows(.,ParishDemo1)

#reshape data from long to wide for easy analysis
ParishDemo <- spread(ParishDemo2, raceSimple, population) %>%
  mutate(pctwhite = White / Total,
         pctblack = Black / Total,
         pctasian = Asian / Total,
         pcthisp = Hispanic / Total,
         white2000=c(.645,0,.547,.266,.688,.844,.705,.497,.51,.853,.691),
         black2000=c(.227,0,.373,.667,.233,.076,.251,.492,.446,.098,.121),
         asian2000=c(.031,0,.021,.023,.026,.013,.006,0,.005,.008,.037),
         hispanic2000=c(.071,0,.044,.031,.016,.051,.028,.006,.029,.025,.125)) %>%
  .[-2,]
