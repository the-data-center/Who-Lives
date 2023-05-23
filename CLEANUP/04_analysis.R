#This document is in the same order as the graphs on the website

#Table 1
load("inputs/allparishesRaw.RData")
AAWhiteHispan <- allparishesRaw %>%
  filter(place == "Orleans Parish") %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total" & (raceSimple == "Black"|raceSimple == "White" |hisp == "Hispanic" | raceSimple == "Asian" )) %>%
  mutate(race.fac = factor(.$raceSimple,levels = c("Black", "White","Hispanic", "Asian")))%>% arrange(race.fac) %>%
  mutate(est2000=c(323392, 128871,  14826, 10919)) %>% #check order of races in data frame. Order is bottom up
  select(raceSimple, race.fac, population, est2000) 

AAWhiteHispan %>%
  write.csv("outputs/spreadsheets/AAWhiteHispan.csv")

#Tables 2
ParishDemo1<- allparishesRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total") %>%
  group_by(raceSimple)%>% unique() %>%
  summarise(population=sum(population)) %>% mutate(PlaceName = "Metro") %>% select(PlaceName, population, raceSimple)

ParishDemo2 <- allparishesRaw %>% filter(PlaceName == "United States") %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total")  %>%
  select(PlaceName, population, raceSimple)

ParishDemo3 <- allparishesRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>% arrange((PlaceName)) %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total")  %>%
  select(PlaceName, population, raceSimple)  %>%
  bind_rows(.,ParishDemo2, ParishDemo3) 

#reshape data from long to wide for easy analysis
ParishDemo <- pivot_wider(ParishDemo1, names_from = raceSimple, values_from = population) %>%
  mutate(PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "St. Tammany", "Metro", "United States"))) %>%
  mutate(pctwhite = as.numeric(White)/ as.numeric(Total),
         pctblack = as.numeric(Black) / as.numeric(Total),
         pctasian = as.numeric(Asian) / as.numeric(Total),
         pcthisp = as.numeric(Hispanic) / as.numeric(Total),     
         white2000=c(.266,.645,.688,.844,.705,.497,.51 ,.853,.547,.691),
         black2000=c(.667,.227,.233,.076,.251,.492,.446,.098,.373,.121),
         asian2000=c(.023,.031,.026,.013,.006,0   ,.005,.008,.021,.037),
         hispanic2000=c(.031,.071,.016,.051,.028,.006,.029,.025,.044,.125)) 

orleansdemo_csv <- ParishDemo %>% filter(PlaceName == "Orleans") %>% select(PlaceName, Total:Hispanic) %>% 
  pivot_longer(cols = -PlaceName, names_to = "race", values_to = "est2021") 
write.csv(orleansdemo_csv, "outputs/spreadsheets/orleansdemo.csv")

parishdemo_csv <- ParishDemo %>% select(-c(Total:Hispanic)) %>%
  pivot_longer(cols = -PlaceName, names_to = c("race", "year"), names_sep = "2", values_to = "val") %>% 
  mutate(year = case_when(year == "000" ~ "2000",
                          is.na(year) ~ "2021",
                          T ~ year),
         race = factor(case_when(grepl("white", race) ~ "White, non-Hispanic",
                                 grepl("black", race) ~ "Black",
                                 grepl("hisp", race) ~ "Hispanic, any race",
                                 grepl("asian", race) ~ "Asian"), levels = c("White, non-Hispanic", "Black", "Hispanic, any race", "Asian"))) %>% 
  arrange(PlaceName, year) %>%
  pivot_wider(names_from = c(PlaceName, year), values_from = val) %>% 
  arrange(race) 
write.csv(parishdemo_csv, "outputs/spreadsheets/ParishDemo.csv")


#Table 4 Hispanic population change by parish

HispanicPop <- allparishesRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%
  filter(age == "Total" & sex == "Total")  %>%
  filter(raceSimple=="Hispanic")%>%
  select(PlaceName, population) %>%
  mutate(est2000 = 0,
         est2000 = ifelse(PlaceName == "St. Tammany", 4737, est2000),
         est2000 = ifelse(PlaceName == "St. John the Baptist", 1230, est2000),
         est2000 = ifelse(PlaceName == "St. James", 130, est2000),
         est2000 = ifelse(PlaceName == "St. Charles", 1346, est2000),
         est2000 = ifelse(PlaceName == "St. Bernard", 3425, est2000),
         est2000 = ifelse(PlaceName == "Plaquemines", 433, est2000),
         est2000 = ifelse(PlaceName == "Orleans", 14826, est2000),
         est2000 = ifelse(PlaceName == "Jefferson", 32418, est2000))

#Table 5 Hispanic population for parishes in metro by year

HispanicPopyears <- allparishesRaw %>%
  filter(age == "Total" & sex == "Total") %>%
  filter(raceSimple=="Hispanic") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany"))

HispanicPopyears %>%
  select(PlaceName, date, population) %>%
  pivot_wider(id_cols = date, names_from = PlaceName, values_from = population ) %>%
  write.csv("outputs/spreadsheets/HispanicPopyears.csv")

load("inputs/hisppopestRaw.RData")
HISPpopM <- hisppopestRaw %>%
  mutate(year = as.numeric(year)) %>%
  filter(place %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                      "St. James", "St. John the Baptist", "St. Tammany")) %>%
  add_row(year = 2000, place= "Orleans", POP=14826) %>%
  add_row(year = 2000, place= "Jefferson", POP=32418) %>%
  add_row(year = 2000, place= "St. Tammany", POP=4737) %>%
  add_row(year = 2000, place= "Plaquemines", POP=433) %>%
  add_row(year = 2000, place= "St. Bernard", POP=3425) %>%
  add_row(year = 2000, place= "St. Charles", POP=1346) %>%
  add_row(year = 2000, place= "St. James", POP=130) %>%
  add_row(year = 2000, place= "St. John the Baptist", POP=1230) %>%
  select(place, year, POP) 
HISPpopM_CSV <- HISPpopM %>% pivot_wider(names_from = "place", values_from = POP) %>%
  select(year, Orleans, Jefferson, Plaquemines, `St. Bernard`, `St. Charles`, `St. James`, `St. John the Baptist`, `St. Tammany`) %>%
  arrange(year)
write.csv(HISPpopM_CSV, "outputs/spreadsheets/HISPpopM.csv")



#Table 5 Population by age group

Agepop <- allparishesRaw %>%
  filter(age== "Under 5 years" | age== "5 to 9"| age== "10 to 14" | age== "15 to 19"|
           age=="20 to 24"| age== "25 to 29"| age== "30 to 34"| age== "35 to 39"| age== "40 to 44"
         | age== "45 to 49" | age=="50 to 54"| age== "55 to 59"| age== "60 to 64"| age== "65 to 69"|
           age== "70 to 74"| age== "75 to 79"| age== "80 to 84"| age== "85 plus")%>%
  filter(raceSimple=="Total")%>%
  filter(sex=="Total")%>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%
  arrange(factor(PlaceName, levels = c("Jefferson","Orleans","Plaquemines",
                                       "St. Bernard","St. Charles","St. James",
                                       "St. John the Baptist","St. Tammany"))) %>%
  select(age, PlaceName, population) %>%
  mutate(est2000 = c(30226,31811,32657,32436,29793,31838,32713,36367,36834,34166,30658,23741,17911,15034,14991,11973,6942,5375,33496,
                     37133,36769,38312,38932,36416,34050,35053,36444,34562,29128,21068,16658,14648,14301,12458,7838,7408,1977,2183,2241,
                     2197,1668,1621,2024,2271,2247,1855,1554,1272,1034,854,769,524,259,207,4242,4639,4996,5021,4257,4196,4584,5327,5530,
                     4939,4398,3241,2597,2569,2714,2148,1051,780,3511,3994,4352,4063,2649,2662,3440,4407,4569,3732,2872,2025,1488,1345,
                     1244,859,462,398,1483,1711,1863,1936,1346,1142,1439,1671,1713,1496,1220,918,916,736,616,448,275,287,3463,3692,3874,
                     3837,2721,2699,3118,3612,3588,3240,2503,1907,1434,1006,925,663,416,346,13556,15029,16147,14672,9045,10257,12729,16457,
                     17655,16062,13641,9733,7125,5825,5168,4033,2296,1838))
Agepop_csv <-  Agepop %>%
  mutate(est21 = population,
         est00 = est2000) %>%
  pivot_wider(id_cols = age, names_from = PlaceName, values_from = c(est00, est21)) %>% 
  select(est00_Orleans, est21_Orleans,
         est00_Jefferson, est21_Jefferson,
         est00_Plaquemines, est21_Plaquemines,
         `est00_St. Bernard`, `est21_St. Bernard`,
         `est00_St. Charles`, `est21_St. Charles`,
         `est00_St. James`, `est21_St. James`,
         `est00_St. John the Baptist`, `est21_St. John the Baptist`,
         `est00_St. Tammany`, `est21_St. Tammany`)
write.csv(Agepop_csv, "outputs/spreadsheets/Agepop.csv")

under18pars<-allparishesRaw %>%
  filter(age=="18 years and over" | age=="Total")%>%
  filter(raceSimple=="Total")%>%
  filter(sex=="Total")%>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%
  select(age, PlaceName, population)

#Creating metro
under18metro<- under18pars %>%
  group_by(age)%>%
  summarise(population=sum(population))
under18metro$PlaceName <- "Metro"

#stack back with other under 18
popunder18 <-bind_rows(under18pars,under18metro) %>%
  spread(., age, population) %>%
  mutate(under18 = Total-`18 years and over`) %>%
  filter(PlaceName == "Orleans" | PlaceName =="Jefferson" | PlaceName == "St. Tammany" | PlaceName == "Metro")%>%
  select(PlaceName, under18) %>%
  mutate(est2000=c(115255,358092,129408,54399))

popunder18CSV <- popunder18 %>%
  select(PlaceName, est2000, under18) %>%
  pivot_longer(-c("PlaceName"), names_to = "under18", values_to = "Value") %>%
  mutate(name = paste(PlaceName, under18, sep = "-"),
         year = 2021) %>%
  select(-PlaceName) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  select(`Orleans-est2000`, `Orleans-under18`, `Jefferson-est2000`, `Jefferson-under18`, `St. Tammany-est2000`, `St. Tammany-under18`, `Metro-est2000`, `Metro-under18`) %>%
  write.csv("outputs/spreadsheets/under18.csv")



############################################
# # ACS # #
############################################

#Hispanic Origin

load("inputs/hispanRaw.RData")
hispanRaw[hispanRaw == -555555555] <- 0 
hispan <- hispanRaw %>%
  filter(placename %in% c("Orleans", "Jefferson", "Metro", "U.S.")) %>%
  mutate(Cubanpct = TotCuba / TotalHisporLat,
         Dominicanpct = TotDomin / TotalHisporLat,
         Mexicanpct = TotMex / TotalHisporLat,
         PuertoRicanpct = TotPR / TotalHisporLat,
         Honduranpct = TotHond / TotalHisporLat,
         Guatemalanpct = TotGuat / TotalHisporLat,
         Nicaraguanpct = TotNicarag / TotalHisporLat,
         Salvadoranpct = TotSalva / TotalHisporLat,
         OtherCApct = TotOtherCA / TotalHisporLat,
         SouthAmericanpct =TotSA / TotalHisporLat,
         Otherpct = TotOtherHisporLat / TotalHisporLat,
         
         # for US/other geo sig testing
         CubanUS = rep(Cubanpct[4], 4),
         DominicanUS = rep(Dominicanpct[4],4),
         MexicanUS = rep(Mexicanpct[4],4),
         PuertoRicanUS = rep(PuertoRicanpct[4],4),
         HonduranUS = rep(Honduranpct[4],4),
         GuatemalanUS = rep(Guatemalanpct[4],4),
         NicaraguanUS = rep(Nicaraguanpct[4],4),
         SalvadoranUS = rep(Salvadoranpct[4],4),
         OtherCAUS = rep(OtherCApct[4],4),
         SouthAmericanUS = rep(SouthAmericanpct[4],4),
         OtherUS = rep(Otherpct[4],4),
         
         CubanMoeProp = moeprop(y = TotalHisporLat, moex = TotCubaMOE, moey = TotalHisporLatMOE, p = Cubanpct),
         DominicanMoeProp = moeprop(y = TotalHisporLat, moex = TotDominMOE, moey = TotalHisporLatMOE, p = Dominicanpct),
         MexicanMoeProp = moeprop(y = TotalHisporLat, moex = TotMexMOE, moey = TotalHisporLatMOE, p = Mexicanpct),
         PuertoRicanMoeProp =moeprop(y = TotalHisporLat, moex = TotPRMOE, moey = TotalHisporLatMOE, p = PuertoRicanpct),
         HonduranMoeProp = moeprop(y = TotalHisporLat, moex = TotHondMOE, moey = TotalHisporLatMOE, p = Honduranpct),
         GuatemalanMoeProp = moeprop(y = TotalHisporLat, moex = TotGuatMOE, moey = TotalHisporLatMOE, p = Guatemalanpct),
         NicaraguanMoeProp = moeprop(y = TotalHisporLat, moex = TotNicaragMOE, moey = TotalHisporLatMOE, p = Nicaraguanpct),
         SalvadoranMoeProp = moeprop(y = TotalHisporLat, moex = TotSalvaMOE, moey = TotalHisporLatMOE, p = Salvadoranpct),
         OtherCAMoeProp = moeprop(y = TotalHisporLat, moex = TotOtherCAMOE, moey = TotalHisporLatMOE, p = OtherCApct),
         SouthAmericanMoeProp = moeprop(y = TotalHisporLat, moex = TotSAMOE, moey = TotalHisporLatMOE, p = SouthAmericanpct),
         OtherMoeProp= moeprop(y = TotalHisporLat, moex = TotOtherHisporLatMOE, moey = TotalHisporLatMOE, p = Otherpct),
         
         ###adding US MOEs to the stat test###
         CubanUSMOE = rep(CubanMoeProp[4], 4),
         DominicanUSMOE = rep(DominicanMoeProp[4],4),
         MexicanUSMOE = rep(MexicanMoeProp[4],4),
         PuertoRicanUSMOE = rep(PuertoRicanMoeProp[4],4),
         HonduranUSMOE = rep(HonduranMoeProp[4],4),
         GuatemalanUSMOE = rep(GuatemalanMoeProp[4],4),
         NicaraguanUSMOE = rep(NicaraguanMoeProp[4],4),
         SalvadoranUSMOE = rep(SalvadoranMoeProp[4],4),
         OtherCAUSMOE = rep(OtherCAMoeProp[4],4),
         SouthAmericanUSMOE = rep(SouthAmericanMoeProp[4],4),
         OtherUSMOE = rep(OtherMoeProp[4],4),
         
         CubanSIG = stattest(x=CubanUS, moex = CubanUSMOE, y=Cubanpct, moey = CubanMoeProp),
         DominicanSIG = stattest(x=DominicanUS, moex = DominicanUSMOE, y=Dominicanpct, moey = DominicanMoeProp),
         MexicanSIG =stattest(x=MexicanUS, moex = MexicanUSMOE, y=Mexicanpct, moey = MexicanMoeProp),
         PuertoRicanSIG = stattest(x=PuertoRicanUS, moex = PuertoRicanUSMOE,  y=PuertoRicanpct, moey=PuertoRicanMoeProp),
         HonduranSIG = stattest(x=HonduranUS, moex = HonduranUSMOE, y=Honduranpct, moey = PuertoRicanMoeProp),
         GuatemalanSIG = stattest( x=GuatemalanUS, moex = GuatemalanUSMOE, y=Guatemalanpct, moey = GuatemalanMoeProp),
         NicaraguanSIG = stattest(x=NicaraguanUS, moex = NicaraguanUSMOE, y = Nicaraguanpct, moey = NicaraguanMoeProp),
         SalvadoranSIG = stattest(x=SalvadoranUS, moex = SalvadoranUSMOE, y = Salvadoranpct, moey = SalvadoranMoeProp),
         OtherCASIG = stattest(x=OtherCAUS, moex = OtherCAUSMOE, y = OtherCApct, moey = OtherCAMoeProp),
         SouthAmericanSIG= stattest(x=SouthAmericanUS, moex = SouthAmericanUSMOE,  y=SouthAmericanpct, moey = SouthAmericanMoeProp),
         OtherSIG = stattest(x=OtherUS, moex = OtherUSMOE, y=Otherpct, moey = OtherMoeProp))

hispanCSV <- hispan %>% 
  select(placename, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "Hispanic origin", values_to = "Value") %>% 
  pivot_wider(id_cols = c("Hispanic origin"), names_from = "placename", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hispan.csv")

#Households with own children under 18
load("inputs/hwcRaw.RData")
hwc <- hwcRaw %>%
  mutate(census2000 = c(0.3007,0.3251,0.397,0.3353,0.3339),
         census2000SE = c(0.00259888,0.002743002, 0.004572234,0.001643334,9.24E-05),
         tothwc = Married + MaleHH + FemaleHH,
         pcthwc = tothwc/TotalHH,
         moeagg = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
         moeprop = moeprop(y = TotalHH, moex = moeagg, moey = TotalHHMOE, p = pcthwc),
         significant = stattest(x=census2000,moex = census2000SE*1.645, y=pcthwc,moey = moeprop)) 

hwcCSV <- hwc %>% 
  select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "hwc", values_to = "Value") %>% 
  mutate(name = paste(placename, hwc, sep = "-"),
         year = 2021) %>% 
  select(-placename) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hwc.csv")

#One-person households
load("inputs/singRaw.RData")
sing <- singRaw %>%
  mutate(census2000 = c(0.331,0.2665,0.1968,0.2707,0.2578),
         census2000SE = c(0.002666846, 0.00258897, 0.003715052, 0.001549686, 8.57E-05),
         pctsing = SingleHH/TotalHH,
         moeprop = moeprop(y = TotalHH, moex = SingleHHMOE, moey = TotalMOE, p = pctsing),
         significant = stattest(x=census2000, moex = census2000SE *1.645, y=pctsing,moey = moeprop))

singCSV <- sing %>% 
select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "sing", values_to = "Value") %>% 
  mutate(name = paste( placename, sing, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/sing.csv")

#Population under 18 chart - already pulled with pep data above

#### *Educational Attainment, income, and internet Access* ####

#Bachelor's degree or higher, adults 25 and older
load("inputs/bachRaw.RData")
bach <- bachRaw %>% 
  mutate(census2000=c(0.2575,0.2149,0.2832,0.2256,0.244), 
         census2000SE = c(0.002140185, 0.002016578, 0.003447718,0.001228988, 7.11E-05),
         totbach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         pctbach = totbach / Total,
         moeagg = moeagg(cbind(MaleBachMOE, MaleGradProfMOE, FemaleBachMOE, FemaleGradProfMOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         significant = stattest(x=census2000, moex = census2000SE*1.645, y=pctbach,moey = moeprop))

bachCSV <- bach  %>% 
select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "bach", values_to = "Value") %>% 
  mutate(name = paste( placename, bach, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/bach.csv")

#Bachelor's degree or higher by race
load("inputs/bachraceRaw.RData")
load("hist_data/bachhist.RData")

#we need bachrace to be wider for the current year 
#bar charts by race. Same data but longer for historical.

#for bar charts + stat testing.
bachrace <- bachraceRaw %>%
  mutate(totbach = MaleBach + FemaleBach + MaleGradProf + FemaleGradProf,
         totbach_blk = MaleBach_blk + FemaleBach_blk,
         totbach_wht = MaleBach_wht + FemaleBach_wht,
         totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
         totbach_asian = MaleBach_asian + FemaleBach_asian,
         pctbach = totbach / Total,
         pctbach_blk = totbach_blk / Total_blk,
         pctbach_wht = totbach_wht / Total_wht,
         pctbach_hisp = totbach_hisp / Total_hisp,
         pctbach_asian = totbach_asian / Total_asian,
         
         moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
         moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
         moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
         moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
         moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
         
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
         moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
         moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp),
#stat test each race against each other within parishes.  Check archived if we want to compare each race to the overall parish value.
         sig_wht_blk = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_asian, moey = moeprop_asian),
         sig_blk_hisp = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_asian, moey = moeprop_asian),
         sig_hisp_asian = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach_asian, moey = moeprop_asian)) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>%
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>%
  ungroup() %>%
  unique() 

bachrace_forgraphic <- bachrace %>%
  select(place, placename, pctbach_blk, pctbach_wht, pctbach_hisp, pctbach_asian) %>% #not includjng "pctbach"
  pivot_longer(-c(placename,place), names_to = c("val", "race"), names_sep = "_") %>%
  select(-val) %>%
  mutate(placename.fac = factor(placename, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "U.S.")), #need to figure out a better way to do this one...
         race = case_when(grepl("asian",race) ~ "Asian",
                          grepl("blk",race) ~ "Black",
                          grepl("hisp",race) ~ "Hispanic, any race",
                          grepl("wht", race) ~ "White, non-Hispanic"),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")),
         val_lab = case_when(value == 0 ~ " ",
                             value != 0 ~ paste0(round.off(value*100), "%"))) %>%
  unique()

bachraceCSV <- bachrace %>% 
  select(race, placename.fac, value) %>%
  pivot_wider(names_from = placename.fac, values_from = value) %>%
  write.csv("outputs/spreadsheets/bachrace.csv")

bachhist <- bachhist %>%
  select(year, val, race, var) %>%
  pivot_wider(id_cols = race, names_from = c(year, var), values_from = val) %>%
  #this looks crazy, but it is pivoting what we already created for bachrace so that we can stat test the historical data without creating so many new dataframes.
           left_join(bachrace %>%
            filter(place == "071") %>%
            filter(!duplicated(place)) %>%
                transmute(`all_2021_percent` = pctbach,
                          `wht_2021_percent` = pctbach_wht,
                          `blk_2021_percent` = pctbach_blk,
                          `hisp_2021_percent` = pctbach_hisp,
                          `all_2021_MOE` = moeprop,
                          `blk_2021_MOE` = moeprop_blk,
                          `hisp_2021_MOE` = moeprop_hisp,
                          `wht_2021_MOE` = moeprop_wht) %>%
       pivot_longer(everything(), names_to = c("race", "var","type"), names_sep = "_", values_to = "val") %>%
       pivot_wider(names_from = c(var, type), names_sep = "_", values_from = "val") %>%
       mutate(race = case_when(grepl("all",race) ~ "All",
                               grepl("blk",race) ~ "Black",
                               grepl("hisp",race) ~ "Hispanic, any race",
                               grepl("wht", race) ~ "White, non-Hispanic"),
              race.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race"))),
       # end of left join
       by = "race") %>%
  #now we have all the data to stat test.
  mutate(sig_00_10 = stattest(`2000_percent`, `2000_MOE`, `2010_percent`, `2010_MOE`),
         sig_00_21 = stattest(`2000_percent`, `2000_MOE`, `2021_percent`, `2021_MOE`),
         sig_10_21 = stattest(`2010_percent`, `2010_MOE`, `2021_percent`, `2021_MOE`),
        
         #this gives us the significance star indicating that within a race, the change over time is not signficant 
         val_lab = case_when((sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " ")) %>%
  pivot_longer(cols = `1980_percent`:`2021_MOE`, names_to = c("year", "var"), names_sep = "_", values_to = "val") %>%
  pivot_wider(names_from = var, values_from = val) %>%
  mutate(year = as.numeric(year))

#not working yet to write to csv.
bachhist_CSV <- bachhist %>% filter(race.fac != "All") %>% select(year, race.fac, percent, MOE, val_lab, sig_00_10,sig_10_21, sig_00_21) %>%
  arrange(year, race.fac)
#write_csv(bachhist_CSV, file = "outputs/spreadsheets/bachhist.csv")

#Less than a high school degree, adults 25 and older
load("inputs/hsRaw.RData")
hs <- hsRaw %>%
  mutate(census2000=c(0.2531,0.2073,0.1613,0.1536,0.196),
         census2000SE = c(0.002128079, 0.001990165, 0.002814633, 0.001219893, 6.58E-05),
         totless = Male9 + Male9to12 + Female9 + Female9to12,
         pctless = totless/Total,
         moeagg = moeagg(cbind(Male9MOE, Male9to12MOE, Female9MOE, Female9to12MOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctless),
         significant = stattest(x=census2000, moex = census2000SE*1.645, y=pctless,moey=moeprop))

hsCSV <- hs %>% 
  select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "hs", values_to = "Value") %>% 
  mutate(name = paste( placename, hs, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hs.csv")

#Median household income, 201* inflation-adjusted dollars
census2000 <- data.frame(census2000 = cpi99 * c(27129, 38239, 47453, 35183, 41851),
                         census2000MOE = cpi99 * c(679.63, 770.33, 585.37, 801.32, 902.83))
load("inputs/medhhRaw.RData")
medhh <- medhhRaw %>%
  bind_cols(.,census2000) %>%
  mutate(significant = stattest(x=census2000,moex = census2000MOE, y=MedianHHIncome,moey=MedianHHIncomeMOE))

 medhhCSV <- medhh %>%
select(placename, census2000, MedianHHIncome) %>% 
  pivot_longer(-placename, names_to = "medhh", values_to = "Value") %>% 
  mutate(name = paste( placename, medhh, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/medhh.csv")
 
load("inputs/medhhraceRaw.RData")
load("hist_data/medHHhist.RData")
medhhrace <- medhhraceRaw %>%
  mutate(#doing the significance tests:
         sig_wht_blk = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_blk, moey = MedianHHIncomeMOE_blk),
         sig_wht_hisp = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_wht_asian = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sig_blk_hisp = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_blk_asian = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sig_hisp_asian = stattest(x=MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         ) %>%
  na.omit() %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% 
  ungroup() %>%
  unique() 

medhhrace_forgraphic <- medhhrace %>%
  select(place, placename, MedianHHIncome_blk, MedianHHIncome_wht, MedianHHIncome_hisp, MedianHHIncome_asian) %>% #not including overall
  pivot_longer(-c(placename,place), names_to = c("val", "race"), names_sep = "_") %>%
  select(-val) %>%
  mutate(placename.fac = factor(placename, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "U.S.")), #need to figure out a better way to do this one...
         race = case_when(grepl("asian",race) ~ "Asian",
                          grepl("blk",race) ~ "Black",
                          grepl("hisp",race) ~ "Hispanic, any race",
                          grepl("wht", race) ~ "White, non-Hispanic"),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")),
         val_lab = paste0("$", comma(value))) %>%
  unique()

medhhraceCSV <- medhhrace %>%
  select(race, placename.fac, MedianHHIncome) %>%
  pivot_wider(names_from = placename.fac, values_from = MedianHHIncome) %>%
  write.csv("outputs/spreadsheets/medhhrace.csv")

medHHhist <- medhh_unadjusted %>% 
  select(year, val, race, var) %>%
  pivot_wider(id_cols = race, names_from = c(year, var), values_from = val) %>%
  #this looks crazy, but it is pivoting what we already created for medhhrace so that we can stat test the historical data without creating so many new dataframes.
  left_join(medhhrace %>%
              filter(place == "071") %>%
              filter(!duplicated(place)) %>%
              transmute(`all_2021_dollars` = MedianHHIncome,
                        `wht_2021_dollars` = MedianHHIncome_wht,
                        `blk_2021_dollars` = MedianHHIncome_blk,
                        `hisp_2021_dollars` = MedianHHIncome_hisp,
                        `all_2021_MOE` = MedianHHIncomeMOE,
                        `blk_2021_MOE` = MedianHHIncomeMOE_blk,
                        `hisp_2021_MOE` = MedianHHIncomeMOE_hisp,
                        `wht_2021_MOE` = MedianHHIncomeMOE_wht) %>%
              pivot_longer(everything(), names_to = c("race", "var","type"), names_sep = "_", values_to = "val") %>%
              pivot_wider(names_from = c(var, type), names_sep = "_", values_from = "val") %>%
              mutate(race = case_when(grepl("all",race) ~ "All",
                                      grepl("blk",race) ~ "Black",
                                      grepl("hisp",race) ~ "Hispanic, any race",
                                      grepl("wht", race) ~ "White, non-Hispanic"),
                     race.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race"))),
            # end of left join
            by = "race") %>%
  mutate(#adjust to 2021 dollars.
    `1979_adj` = as.numeric(`1979_dollars`)*cpi79,
    `1989_adj` = as.numeric(`1989_dollars`)*cpi89,
    `1999_adj` = as.numeric(`1999_dollars`)*cpi99,
    `1999_adjMOE` = as.numeric(`1999_MOE`)*cpi99,
    `2010_adj` = as.numeric(`2010_dollars`)*cpi10,
    `2010_adjMOE` = as.numeric(`2010_MOE`)*cpi10,
    
    
    sig_99_10 = stattest(`1999_adj`, `1999_adjMOE`, `2010_adj`, `2010_adjMOE`),
         sig_99_21 = stattest(`1999_adj`, `1999_adjMOE`, `2021_dollars`, `2021_MOE`),
         sig_10_21 = stattest(`2010_adj`, `2010_adjMOE`, `2021_dollars`, `2021_MOE`),
         
         #this gives us the significance star indicating that within a race, the change over time is not signficant 
         val_lab = case_when((sig_99_10 == "no" | sig_99_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " "))
  

#Household internet access ... where did the rest of it go @HT?
# select(placename, (contains("pct"))) %>% 
#   pivot_longer(-placename, names_to = "inta", values_to = "Value") %>% 
#   pivot_wider(id_cols = c("inta"), names_from = "placename", values_from = "Value") %>%
#   write.csv("outputs/spreadsheets/inta.csv")

#Poverty rate, population for whom poverty has been determined
load("inputs/povRaw.RData")
pov <- povRaw %>%
  mutate(sf1999=c(0.2794,0.1365,0.0972,0.1838,0.1238),
         sf1999SE = c(0.002198943, 0.001714384, 0.002287413, 0.001142575,5.78E-05),
         pctpov = BelowPov / Total,
         moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
         significant = stattest(x=sf1999, moex = sf1999SE*1.645, y=pctpov,moey=moeprop))

povCSV <- pov %>% 
select(placename, sf1999, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "pov", values_to = "Value") %>% 
  mutate(name = paste( placename, pov, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/pov.csv")

#Poverty by race
load("inputs/povraceRaw.RData")
load("hist_data/povhist.RData")

#for bar charts + stat testing.
povrace <-  povraceRaw %>% 
  mutate(#adding zeros for asian in st. tammany (suppression):
    # BelowPov_asian = case_when(is.na(BelowPov_asian) == T & place == 103 ~ 0,
    #                            T ~ BelowPov_asian),
    # Total_asian = case_when(is.na(Total_asian) == T & place == 103 ~ 0,
    #                         T ~ Total_asian),
    # BelowPovMOE_asian= case_when(is.na(BelowPovMOE_asian) == T & place == 103 ~ 0,
    #                              T ~ BelowPovMOE_asian),
    # TotalMOE_asian= case_when(is.na(TotalMOE_asian) == T & place == 103 ~ 0,
    #                           T ~ TotalMOE_asian),
    
    pctpov = BelowPov / Total,
         pctpov_blk = BelowPov_blk / Total_blk,
         pctpov_wht = BelowPov_wht / Total_wht,
         pctpov_hisp = BelowPov_hisp / Total_hisp,
         pctpov_asian = BelowPov_asian / Total_asian,
         
         moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
         moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
         moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
         moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
         moeprop_asian = case_when(place != 103 ~ moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian),
                                                          place == 103 ~ 0),
         
         sig_wht_blk = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_asian, moey = moeprop_asian),
         sig_blk_hisp = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_asian, moey = moeprop_asian),
         sig_hisp_asian = stattest(x=pctpov_hisp, moex = moeprop_hisp, y=pctpov_asian, moey = moeprop_asian)) %>%
  
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% 
  unique() %>%
  unique()

povrace_forgraphic <- povrace %>%
  select(place, placename, pctpov_blk, pctpov_wht, pctpov_hisp, pctpov_asian) %>% #not including overall/"pctpov"
  pivot_longer(-c(placename, place), names_to = c("val", "race"), names_sep = "_") %>%
  select(-val) %>%
  mutate(placename.fac = factor(placename, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "U.S.")), #need to figure out a better way to do this one...
         race = case_when(grepl("asian",race) ~ "Asian",
                          grepl("blk",race) ~ "Black",
                          grepl("hisp",race) ~ "Hispanic, any race",
                          grepl("wht", race) ~ "White, non-Hispanic"),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")),
         val_lab = case_when(value == 0 ~ " ",
                             value != 0 ~ paste0(round.off(value*100), "%"))) %>%
  unique()

povhist <- povhist %>%
  select(year, val, race, var) %>%
  pivot_wider(id_cols = race, names_from = c(year, var), values_from = val) %>%
  #this looks crazy, but it is pivoting what we already created for povrace so that we can stat test the historical data without creating so many new dataframes.
  left_join(povrace %>%
              filter(place == "071") %>%
              filter(!duplicated(place)) %>%
              transmute(`all_2021_percent` = pctpov,
                        `wht_2021_percent` = pctpov_wht,
                        `blk_2021_percent` = pctpov_blk,
                        `hisp_2021_percent` = pctpov_hisp,
                        `all_2021_MOE` = moeprop,
                        `blk_2021_MOE` = moeprop_blk,
                        `hisp_2021_MOE` = moeprop_hisp,
                        `wht_2021_MOE` = moeprop_wht) %>%
              pivot_longer(everything(), names_to = c("race", "var","type"), names_sep = "_", values_to = "val") %>%
              pivot_wider(names_from = c(var, type), names_sep = "_", values_from = "val") %>%
              mutate(race = case_when(grepl("all",race) ~ "All",
                                      grepl("blk",race) ~ "Black",
                                      grepl("hisp",race) ~ "Hispanic, any race",
                                      grepl("wht", race) ~ "White, non-Hispanic"),
                     race.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race"))),
            # end of left join
            by = "race") %>%
  #now we have all the data to stat test.
  mutate(sig_00_10 = stattest(`2000_percent`, `2000_MOE`, `2010_percent`, `2010_MOE`),
         sig_00_21 = stattest(`2000_percent`, `2000_MOE`, `2021_percent`, `2021_MOE`),
         sig_10_21 = stattest(`2010_percent`, `2010_MOE`, `2021_percent`, `2021_MOE`),
         
         #this gives us the significance star indicating that within a race, the change over time is not signficant 
         val_lab = case_when((sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " ")) %>%
  pivot_longer(cols = `1980_percent`:`2021_MOE`, names_to = c("year", "var"), names_sep = "_", values_to = "val") %>%
  pivot_wider(names_from = var, values_from = val) %>%
  mutate(year = as.numeric(year))

#Children in poverty, population for whom poverty has been determined			
load("inputs/childpovRaw.RData")
childpov <- childpovRaw %>%
  mutate(sf1999=c(0.4053,0.2034,0.123,0.2623,0.1656),
         sf1999SE = c(0.004610543, 0.00401331, 0.004760158, 0.002505787, 1.28E-04),
         TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
         moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
         TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
         moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
         pctBelowChildPov = TotBelowChildPov / TotChildPov,
         moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
         significant = stattest(x=sf1999, moex = sf1999SE*1.645, y=pctBelowChildPov,moey=moeprop))


childpovCSV <- childpov %>% 
select(placename, sf1999, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "childpov", values_to = "Value") %>% 
  mutate(name = paste(placename, childpov, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/childpov.csv")

load("inputs/childpovbyrace.RData")
childpovbyrace <- childpovbyrace %>%
  mutate(place = place,
         placename = placename,
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
    TotChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk + AbovePovFemaleChild_blk + AbovePovMaleChild_blk,
    moeaggtot_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk, AbovePovFemaleChildMOE_blk, AbovePovMaleChildMOE_blk)),
    TotBelowChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk,
    moeaggbelow_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk)),
    pctBelowChildPov_blk = TotBelowChildPov_blk / TotChildPov_blk,
    moeprop_blk = moeprop(y=TotChildPov_blk, moex = moeaggbelow_blk, moey = moeaggtot_blk, p=pctBelowChildPov_blk),
    TotChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht + AbovePovFemaleChild_wht + AbovePovMaleChild_wht,
    moeaggtot_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht, AbovePovFemaleChildMOE_wht, AbovePovMaleChildMOE_wht)),
    TotBelowChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht,
    moeaggbelow_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht)),
    pctBelowChildPov_wht = TotBelowChildPov_wht / TotChildPov_wht,
    moeprop_wht = moeprop(y=TotChildPov_wht, moex = moeaggbelow_wht, moey = moeaggtot_wht, p=pctBelowChildPov_wht),
    TotChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp + AbovePovFemaleChild_hisp + AbovePovMaleChild_hisp,
    moeaggtot_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp, AbovePovFemaleChildMOE_hisp, AbovePovMaleChildMOE_hisp)),
    TotBelowChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp,
    moeaggbelow_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp)),
    pctBelowChildPov_hisp = TotBelowChildPov_hisp / TotChildPov_hisp,
    moeprop_hisp = moeprop(y=TotChildPov_hisp, moex = moeaggbelow_hisp, moey = moeaggtot_hisp, p=pctBelowChildPov_hisp),
    TotChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian + AbovePovFemaleChild_asian + AbovePovMaleChild_asian,
    moeaggtot_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian, AbovePovFemaleChildMOE_asian, AbovePovMaleChildMOE_asian)),
    TotBelowChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian,
    moeaggbelow_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian)),
    pctBelowChildPov_asian = TotBelowChildPov_asian / TotChildPov_asian,
    
    sig_wht_blk = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_blk, moey = moeprop_blk),
    sig_wht_hisp = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
    sig_wht_asian = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_wht = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov, moey = moeprop),
    sig_blk_hisp = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
    sig_blk_asian = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_blk = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov, moey = moeprop),
    sig_hisp_asian = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_hisp = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov, moey = moeprop),
    sigall_asian = stattest(x=pctBelowChildPov_asian, moex = moeprop_asian, y=pctBelowChildPov, moey = moeprop))
    
    

read_csv("hist_data/raw_data/childpovhist.csv") #from prosperity index

#Households without access to a vehicle
load("inputs/vehRaw.RData")
veh <- vehRaw %>%
  mutate(census2000=c(0.2732,0.0930,0.0442,0.1532,0.1030),
         census2000SE = c(0.002755866, 0.001856238, 0.002095766, 0.001371385, 6.62E-05),
         vehpct = NoVehAvail / Total,
         moeprop = moeprop(y = Total, moex = NoVehAvailMOE, moey = TotalMOE, p = vehpct),
         significant = stattest(x=census2000, moex= census2000SE*1.645,y=vehpct,moey = moeprop))

vehCSV <- veh %>% 
select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "veh", values_to = "Value") %>% 
  mutate(name = paste(placename, veh, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/veh.csv")

#Population not U.S. citizens at birth
load("inputs/forborRaw.RData")
forbor <- forborRaw %>%  mutate(census2000=c(0.0425,0.0748,0.0237,0.048,0.1105),
         census2000SE =c(0.001036254, 0.001394458, 0.001243694, 0.000671199, 5.85E-05),
         forborpct = (TotForeign00to09 + TotForeign90to99 + TotForeignPre90 + TotForeign10on) / TotalPop,
         forbormoeagg = moeagg(cbind(TotForeign00to09MOE, TotForeign90to99MOE, TotForeignPre90MOE, TotForeign10onMOE)),
         forbormoeprop = moeprop(y=TotalPop, moex = forbormoeagg, moey = TotalPopMOE, p=forborpct),
         significant = stattest(x=census2000,moex = census2000SE*1.645, y=forborpct,moey=forbormoeprop))

forborCSV <- forbor %>% 
select(placename, census2000, (contains("pct"))) %>% 
  pivot_longer(-placename, names_to = "forbor", values_to = "Value") %>% 
  mutate(name = paste(placename, forbor, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/forbor.csv")

#Population who moved in the past year
load("inputs/mobRaw.RData")
mob <- mobRaw %>%
  mutate(sf2004mobabroad =c(0.0013,0.0044,0.00,0.00,0.006), #zeroes previously filled in for both metro and St. Tammany 2004 because of missing data, HT is filling in 2004 Metro.
         sf2004mobabroadMOE = c(0.0013025893, 0.0033903959, 0, 0, 0.0002322461),
         sf2004states=c(0.0206,0.02,0.00,0.00,0.0235),
         sf2004statesMOE = c(0.0085199087, 0.0115190278, 0, 0, 0.0006242538),
         sf2004difparish=c(0.0085,0.03,0.00,0.00,0.0302),
         sf2004difparishMOE = c(0.0037592895, 0.0119890315, 0, 0, 0.0005295007),
         sf2004withinparish=c(0.1131,0.0958,0.00,0.00,0.0973),
         sf2004withinparishMOE = c(0.023344033, 0.022923510, 0, 0, 0.001341028),
         sf2004samehouse=c(0.8565,0.8498,0.00,0.00,0.8430),
         sf2004samehouseMOE = c(0.026188401, 0.028107571, 0, 0, 0.001767759),
         
         mobabroadpct = TotMovedfromAbroad / Total,
         mobStatespct = TotMovedbtwnStates / Total,
         difparishpct = TotMovedinState / Total,
         withinparishpct = TotMovedinCty / Total,
         samehousepct = TotSameHouse / Total,
         
         mobabroadmoeprop = moeprop(y=Total,moex = TotMovedfromAbroadMOE,moey = TotalMOE,p=mobabroadpct),
         mobStatesmoeprop = moeprop(y=Total,moex = TotMovedbtwnStatesMOE,moey = TotalMOE,p=mobStatespct),
         difparishmoeprop = moeprop(y=Total,moex = TotMovedinStateMOE,moey = TotalMOE,p=difparishpct),
         withinparishmoeprop = moeprop(y=Total,moex = TotMovedinCtyMOE,moey = TotalMOE,p=withinparishpct),
         samehousemoeprop = moeprop(y=Total, moex = TotSameHouseMOE,moey = TotalMOE,p=samehousepct),
         
         abroadSIG = stattest (x=sf2004mobabroad, moex = sf2004mobabroadMOE, y=mobabroadpct, moey = mobabroadmoeprop),
         statesSIG = stattest (x=sf2004states, moex = sf2004statesMOE, y=mobStatespct,moey = mobStatesmoeprop),
         difparishSIG = stattest (x=sf2004difparish, moex = sf2004difparishMOE, y =difparishpct, moey = difparishmoeprop),
         withinparishSIG = stattest (x=sf2004withinparish, moex = sf2004withinparishMOE, y=withinparishpct, moey = withinparishmoeprop),
         samhouseSIG = stattest (x=sf2004samehouse, moex = sf2004samehouseMOE, y=samehousepct, moey = samehousemoeprop))

mobCSV <- mob %>% 
select(place,  (contains("sf2004") & !contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "mob", values_to = "Value") %>% 
  mutate(year = ifelse(grepl("2004", mob), "2004", "2021"),
         header = paste(place, year, sep = "-"),
         mobfinal = ifelse(grepl("abroad", mob), "abroad", mob),
         mobfinal = ifelse(grepl("tates", mob), "tates", mobfinal),
         mobfinal = ifelse(grepl("difparish", mob), "difparish", mobfinal),
         mobfinal = ifelse(grepl("withinparish", mob), "withinparish", mobfinal),
         mobfinal = ifelse(grepl("samehouse", mob), "samehouse", mobfinal),
         mobfinal = factor(mobfinal, levels = c("samehouse", "withinparish", "difparish", "tates", "abroad"))) %>% 
  arrange(mobfinal) %>%
  pivot_wider(id_cols = c("mobfinal"), names_from = "header", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/mob.csv")

## mobility sig testing for written analysis

OPmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[1],mob$TotMovedbtwnStatesMOE[1],mob$TotMovedfromAbroadMOE[1]))
OPpct <- (mob$TotMovedinState[1]+mob$TotMovedbtwnStates[1]+mob$TotMovedfromAbroad[1])/mob$Total[1]
OPmoeprop <- moeprop(y=mob$Total[1], moex = OPmoeagg, moey = mob$TotalMOE[1], p= OPpct)
OPsig <- stattest(x = (mob$sf2004mobabroad[1]+mob$sf2004states[1]+mob$sf2004difparish[1]), y=OPpct, moey=OPmoeprop)

jeffmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[2],mob$TotMovedbtwnStatesMOE[2],mob$TotMovedfromAbroadMOE[2]))
jeffpct <- (mob$TotMovedinState[2]+mob$TotMovedbtwnStates[2]+mob$TotMovedfromAbroad[2])/mob$Total[2]
jeffmoeprop <- moeprop(y=mob$Total[2], moex = jeffmoeagg, moey = mob$TotalMOE[2], p= jeffpct)
jeffsig <- stattest(x = (mob$sf2004mobabroad[2]+mob$sf2004states[2]+mob$sf2004difparish[2]), y=jeffpct, moey=jeffmoeprop)

#Homeownership rates
load("inputs/hoRaw.RData")
load("inputs/hoRaw2000.RData")
ho <- hoRaw %>% left_join(hoRaw2000, by = "placename") %>%
  mutate(#census2000=c(0.465,0.6385,0.8048,0.6183,0.6619), #got this from SF1 for no SEs
        # census2000SE = c(0.00282756, 0.002814847, 0.0037049, 0.001695053, 1.03E-04), #using SF1 to be consistent and no MOEs
         Ownerpct = Owner / Total,
         Ownerpct2000 = Owner2000 / Total2000,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         significant = stattest(x=Ownerpct2000 ,y=Ownerpct,moey = Ownermoeprop))

hoCSV <- ho %>% 
select(place, (contains("pct"))) %>% select(place, Ownerpct2000, Ownerpct) %>%
  pivot_longer(-c("place"), names_to = "ho", values_to = "Value") %>% 
  mutate(name = paste( place, ho, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/ho.csv")

load("inputs/hobyrace.RData")
hobyrace <- hobyrace %>%
  mutate(Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         Ownerpct_blk = Owner_blk / Total_blk,
         Ownermoeprop_blk = moeprop(y=Total_blk,moex = OwnerMOE_blk,moey = TotalMOE_blk,p=Ownerpct_blk),
         Ownerpct_wht = Owner_wht / Total_wht,
         Ownermoeprop_wht = moeprop(y=Total_wht,moex = OwnerMOE_wht,moey = TotalMOE_wht,p=Ownerpct_wht),
         Ownerpct_hisp = Owner_hisp / Total_hisp,
         Ownermoeprop_hisp = moeprop(y=Total_hisp,moex = OwnerMOE_hisp,moey = TotalMOE_hisp,p=Ownerpct_hisp),
         Ownerpct_asian = Owner_asian / Total_asian) %>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") %>%
  filter(var != "Ownerpct") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

read_csv("hist_data/raw_data/hohist.csv")
hohist <- hohist %>% 
  filter(Geography == "New Orleans") %>%
  rename(val = pctHomeownership) %>%
  mutate(var = Race,
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Lat",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  select (-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(ho_exp %>%
              filter(place.fac == "Orleans",   
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2021, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "Ownerpct", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

#Homeowners without a mortgage
load("inputs/honomoRaw.RData")
honomo <- honomoRaw %>%
  mutate(census2000 = c(0.3298,0.3458,0.2967,0.3476,0.3259),
         census2000SE = c(0.004263821, 0.003804423, 0.005191947, 0.00230056781162641, 1.25E-04),
         honomopct = NoMortgage / Total,
         moeprop = moeprop(y=Total,moex =NoMortgageMOE,moey = TotalMOE,p=honomopct),
         significant = stattest(x=census2000, moex = census2000SE*1.645, y=honomopct,moey=moeprop))

honomoCSV <- honomo %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "honomo", values_to = "Value") %>% 
  mutate(name = paste( place, honomo, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/honomo.csv")

#Renters with severe housing cost burdens
load("inputs/rentburRaw.RData")
rentbur <- rentburRaw %>%
  mutate(#sf2004=c(0.2432,0.2167,0,0.2161,0.2384),#0 for St. tammany missing value ### ****HT commenting out old values, adding ones I pulled from 2004 ACS
         #order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"
         sf2004 = c(0.2304765, 0.2019471, 0, 0, 0.2196190),
         sf2004MOE = c(0.044544178, 0.052252324,0, 0, 0.003006082),
         rentburpct = `50orMore`/ (Total - NotComputed),
         moeagg = moeagg(cbind(TotalMOE, NotComputedMOE)),
         moeprop = moeprop(y=(Total - NotComputed),moex=`50orMoreMOE`,moey = moeagg, p = rentburpct),
         significant = stattest(x=sf2004, moex = sf2004MOE, y=rentburpct, moey=moeprop))

rentburCSV <- rentbur %>% 
select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "rentbur", values_to = "Value") %>% 
  mutate(name = paste( place, rentbur, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/rentbur.csv")

#Homeowners with severe housing cost burdens
load("inputs/hoburRaw.RData")
hobur <- hoburRaw %>%
  mutate(#sf2004=c(0.1620,0.0891,0,0.1134,0.0988), #0 for St. tammany missing value, ### ****HT commenting out old values, adding ones I pulled from 2004 ACS
         #order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"
         sf2004 = c(0.13226868, 0.06815970, 0, 0, 0.07895178),
         sf2004MOE = c(0.030880769, 0.025995416, 0, 0, 0.000911579), #getting these numbers from line 154 of data-pull.R
         hoburpct = (`50orMoreMortgage`+`50orMoreNoMortgage`)/(Total - NotComputedMortgage - NotComputedNoMortgage),
         moexagg = moeagg(cbind(`50orMoreMortgageMOE`,`50orMoreNoMortgageMOE`)),
         moeyagg = moeagg(cbind(TotalMOE, NotComputedMortgageMOE, NotComputedNoMortgageMOE)),
         moeprop = moeprop(y=Total,moex=moexagg,moey=moeyagg,p=hoburpct),
         significant = stattest(x=sf2004,moex = sf2004MOE, y=hoburpct, moey=moeprop))

hoburCSV <- hobur %>% 
select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hobur", values_to = "Value") %>% 
  mutate(name = paste( place, hobur, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hobur.csv")


#Median gross rent, 201* inflation-adjusted dollars
load("inputs/medrentRaw.RData")
#sf2004 <- data.frame(sf2004 = cpi04*c(566,654,0,616,694))### ****HT commenting out old values, adding ones I pulled from 2004 ACS
#order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"

sf2004 <- data.frame(sf2004 = cpi04 * c(566, 654, 0, 0, 694), # note that these are the exact same as what we already had, just adjusting to 2021 dollars and noting that I have the same estimates for med rent, but not the rent/hoburden percentages.) 
                     sf2004MOE = cpi04 * c(29, 48, 0, 0, 2)) #also noting that I am inflation adjusting the MOES the same way

medrent <- medrentRaw %>% 
  bind_cols(.,sf2004) %>%
  mutate(significant = stattest(x=sf2004, moex = sf2004MOE, y=Rent,moey=RentMOE)) 

medrentCSV <- medrent %>%
  select(place, sf2004, Rent) %>% 
  pivot_longer(-c("place"), names_to = "medrent", values_to = "Value") %>% 
  mutate(name = paste( place, medrent, sep = "-"),
         year = 2021) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/medrent.csv")

#Year structure built, 201* housing units
load("inputs/yrbuiltRaw.RData")
yrbuilt <- yrbuiltRaw %>%
  mutate(orLater1990pct = (`1990to1999` + `2000to2009` + `2010to2013` + `2014`)/Total,
         mid1950to1989pct = (`1950to1959`+`1960to1969`+`1970to1979`+`1980to1989`)/Total,
         orbefore1949pct = (`1940to1949`+`1939`)/Total,
         
         orlater1990moeagg = moeagg(cbind(`1990to1999MOE`,`2000to2009MOE`, `2010to2013MOE`,`2014MOE`)),
         mid1950to1989moeagg =moeagg(cbind(`1950to1959MOE`,`1960to1969MOE`,`1970to1979MOE`,`1980to1989MOE`)),
         orbefore1949moeagg = moeagg(cbind(`1940to1949MOE`,`1939MOE`)),
         
         orlater1990moeprop = moeprop(y=Total,moex = orlater1990moeagg, moey = TotalMOE,p=orLater1990pct),
         mid1950to1989moeprop = moeprop(y=Total,moex = mid1950to1989moeagg, moey = TotalMOE,p=mid1950to1989pct),
         orbefore1949moeprop = moeprop(y=Total,moex = orbefore1949moeagg, moey = TotalMOE,p=orbefore1949pct),
         
         orLater1990US = rep(orLater1990pct[5],5),
         mid1950to1989US = rep(mid1950to1989pct[5],5),
         orbefore1949US = rep(orbefore1949pct[5],5),
         
         orLater1990SIG = stattest(x=orLater1990US, y= orLater1990pct, moey = orlater1990moeprop),
         mid1950to1989SIG = stattest(x= mid1950to1989US, y= mid1950to1989pct , moey = mid1950to1989moeprop),
         orbefore1949SIG = stattest(x=orbefore1949US, y= orbefore1949pct, moey = orbefore1949moeprop))
yrbuiltCSV <- yrbuilt %>% 
select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "yrbuilt", values_to = "Value") %>% 
  pivot_wider(id_cols = c("yrbuilt"), names_from = "place", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/yrbuilt.csv")

#Means of transportation to work, 
load("inputs/commuteRaw.RData")
load("inputs/commuteRaw2000.RData")
commute <- commuteRaw %>% left_join(commuteRaw2000, by = "placename") %>%
  filter(placename %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000drive=c(0.6028,0.7855,0.7301,0.7570),
         census2000carpool=c(0.1614,0.1372,0.1465,0.1219),
         census2000publictransit=c(0.1322,0.0023,0.0530,0.0457),
         census2000bike=c(0.0116,0.0032,0.0059,0.0038),
         census2000walk=c(0.0521,0.0174,0.0272,0.0293),
         census2000workhome=c(0.0266,0.0368,0.0241,0.0326),
         census2000other=c(0.0133,0.0143,0.0133,0.0097),
         
         Drivepct = DroveAlone / Total,
         Carpoolpct= Carpool / Total,
         PublicTransitpct = PublicTransit / Total,
         bikepct = Bike / Total,
         Walkpct = Walk / Total,
         Workhomepct = Workhome / Total,
         Otherpct = Other / Total,
         
         Drivepct2000 = DroveAlone2000 / Total2000,
         Carpoolpct2000= Carpool2000 / Total2000,
         PublicTransitpct2000 = PublicTransit2000 / Total2000,
         bikepct2000 = Bike2000 / Total2000,
         Walkpct2000 = Walk2000 / Total2000,
         Workhomepct2000 = Workhome2000 / Total2000,
         Otherpct2000 = Other2000 / Total2000,
         
         Drivemoeprop = moeprop(y=Total, moex=DroveAloneMOE, moey=TotalMOE, p=Drivepct),
         carpoolmoeprop = moeprop(y=Total, moex=CarpoolMOE, moey=TotalMOE, p=Carpoolpct),
         PublicTransitmoeprop = moeprop(y=Total, moex=PublicTransitMOE, moey=TotalMOE, p=PublicTransitpct),
         bikemoeprop = moeprop(y=Total, moex=BikeMOE, moey=TotalMOE, p=bikepct),
         Walkmoeprop = moeprop(y=Total, moex=WalkMOE, moey=TotalMOE, p=Walkpct),
         workhomemoeprop = moeprop(y=Total, moex=WorkhomeMOE, moey=TotalMOE, p=Workhomepct),
         othermoeprop = moeprop(y=Total, moex=OtherMOE, moey=TotalMOE, p=Otherpct),
         
         Drivemoeprop2000 = moeprop(y=Total2000, moex=DroveAloneMOE2000, moey=TotalMOE2000, p=Drivepct2000),
         carpoolmoeprop2000 = moeprop(y=Total2000, moex=CarpoolMOE2000, moey=TotalMOE2000, p=Carpoolpct2000),
         PublicTransitmoeprop2000 = moeprop(y=Total2000, moex=PublicTransitMOE2000, moey=TotalMOE2000, p=PublicTransitpct2000),
         bikemoeprop2000 = moeprop(y=Total2000, moex=BikeMOE2000, moey=TotalMOE2000, p=bikepct2000),
         Walkmoeprop2000 = moeprop(y=Total2000, moex=WalkMOE2000, moey=TotalMOE2000, p=Walkpct2000),
         workhomemoeprop2000 = moeprop(y=Total2000, moex=WorkhomeMOE2000, moey=TotalMOE2000, p=Workhomepct2000),
         othermoeprop2000 = moeprop(y=Total2000, moex=OtherMOE2000, moey=TotalMOE2000, p=Otherpct2000),
         
         DriveSIG = stattest(x=census2000drive, moex = Drivemoeprop2000,y=Drivepct,moey = Drivemoeprop),
         carpoolSIG = stattest (x=census2000carpool, moex = carpoolmoeprop2000, y=Carpoolpct, moey = carpoolmoeprop),
         PublicTransitSIG = stattest (x=census2000publictransit, moex = , y=PublicTransitpct, moey = PublicTransitmoeprop),
         bikeSIG = stattest(x=census2000bike, moex = PublicTransitmoeprop2000, y = bikepct, moey = bikemoeprop),
         walkSIG = stattest (x=census2000walk, moex = Walkmoeprop2000, y = Walkpct, moey = Walkmoeprop),
         workhomeSIG = stattest ( x=census2000workhome, moex = workhomemoeprop2000, y=Workhomepct, moey = workhomemoeprop),
         otherSIG = stattest (x=census2000other, moex = othermoeprop2000, y= Otherpct, moey = othermoeprop))

commuteCSV <- commute %>% 
  select(placename, (contains("pct"))) %>% 
  pivot_longer(-c("placename"), names_to = "commute", values_to = "Value") %>% 
  mutate(year = ifelse(grepl("2000", commute), 2000, 2021),
         header = paste(placename, year, sep = "-"),
         commutefinal = ifelse(grepl("ive", commute), "DroveAlone", commute),
         commutefinal = ifelse(grepl("arpool", commute), "Carpool", commutefinal),
         commutefinal = ifelse(grepl("ublic", commute), "PublicTransit", commutefinal),
         commutefinal = ifelse(grepl("Motorcycle", commute), "Motorcycle", commutefinal),
         commutefinal = ifelse(grepl("ike", commute), "Bike", commutefinal),
         commutefinal = ifelse(grepl("alk", commute), "Walk", commutefinal),
         commutefinal = ifelse(grepl("home", commute), "Workhome", commutefinal),
         commutefinal = ifelse(grepl("ther", commute), "Other", commutefinal)) %>% 
  pivot_wider(id_cols = c("commutefinal"), names_from = "header", values_from = "Value") %>%
  select(commutefinal, `Orleans-2000`, `Orleans-2021`, `Jefferson-2000`, `Jefferson-2021`, `New Orleans Metro Area-2000`, `New Orleans Metro Area-2021`, `United States-2000`, `United States-2021`  ) %>% 
  write.csv("outputs/spreadsheets/commute.csv")

