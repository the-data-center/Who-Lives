############################################
# # ACS # #
############################################
order <- c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")
orderHisp <- c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")

load("inputs/hispanRaw.RData")

hispanRaw[hispanRaw == -555555555] <- 0 
#Hispanic Origin
hispan <- hispanRaw %>%
  filter(placename %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>%
  slice(match(orderHisp, place)) %>%
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
         
         CubanSIG = stattest(x=CubanUS, y=Cubanpct, moey = CubanMoeProp),
         DominicanSIG = stattest(x=DominicanUS, y=Dominicanpct, moey = DominicanMoeProp),
         MexicanSIG =stattest(x=MexicanUS, y=Mexicanpct, moey = MexicanMoeProp),
         PuertoRicanSIG = stattest(x=PuertoRicanUS, y=PuertoRicanpct, moey=PuertoRicanMoeProp),
         HonduranSIG = stattest(x=HonduranUS, y=Honduranpct, moey = PuertoRicanMoeProp),
         GuatemalanSIG = stattest( x=GuatemalanUS, y=Guatemalanpct, moey = GuatemalanMoeProp),
         NicaraguanSIG = stattest(x=NicaraguanUS, y = Nicaraguanpct, moey = NicaraguanMoeProp),
         SalvadoranSIG = stattest(x=SalvadoranUS, y = Salvadoranpct, moey = SalvadoranMoeProp),
         OtherCASIG = stattest(x=OtherCAUS, y = OtherCApct, moey = OtherCAMoeProp),
         SouthAmericanSIG= stattest(x=SouthAmericanUS, y=SouthAmericanpct, moey = SouthAmericanMoeProp),
         OtherSIG = stattest(x=OtherUS, y=Otherpct, moey = OtherMoeProp))

hispanCSV <- hispan %>% 
  select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "Hispanic origin", values_to = "Value") %>% 
  pivot_wider(id_cols = c("Hispanic origin"), names_from = "place", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hispan.csv")

#Households with own children under 18
load("inputs/hwcRaw.RData")
hwc <- hwcRaw %>%
  filter(placename %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>%
  mutate(census2000 = c(0.3007,0.3251,0.397,0.3353,0.3339),
         tothwc = Married + MaleHH + FemaleHH,
         pcthwc = tothwc/TotalHH,
         moeagg = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
         moeprop = moeprop(y = TotalHH, moex = moeagg, moey = TotalHHMOE, p = pcthwc),
         significant = stattest(x=census2000,y=pcthwc,moey = moeprop)) 

hwcCSV <- hwc %>% 
  select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hwc", values_to = "Value") %>% 
  mutate(name = paste( place, hwc, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hwc.csv")

#One-person households
load("inputs/singRaw.RData")
sing <- singRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000 = c(0.331,0.2665,0.1968,0.2707,0.2578),
         pctsing = SingleHH/Total,
         moeprop = moeprop(y = Total, moex = SingleHHMOE, moey = TotalMOE, p = pctsing),
         significant = stattest(x=census2000, y=pctsing,moey = moeprop))

singCSV <- sing %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "sing", values_to = "Value") %>% 
  mutate(name = paste( place, sing, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/sing.csv")

#Less than a high school degree, adults 25 and older
load("inputs/hsRaw.RData")
hs <- hsRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000=c(0.2531,0.2073,0.1613,0.1536,0.196),
         totless = Male9 + Male9to12 + Female9 + Female9to12,
         pctless = totless/Total,
         moeagg = moeagg(cbind(Male9MOE, Male9to12MOE, Female9MOE, Female9to12MOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctless),
         significant = stattest(x=census2000, y=pctless,moey=moeprop))

hsCSV <- hs %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hs", values_to = "Value") %>% 
  mutate(name = paste( place, hs, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hs.csv")

#Bachelor's degree or higher, adults 25 and older
load("inputs/bachRaw.RData")
bach <- bachRaw %>% 
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000=c(0.2575,0.2149,0.2832,0.2256,0.244),
         totbach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         pctbach = totbach / Total,
         moeagg = moeagg(cbind(MaleBachMOE, MaleGradProfMOE, FemaleBachMOE, FemaleGradProfMOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         significant = stattest(x=census2000,y=pctbach,moey = moeprop))

bachCSV <- bach  %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "bach", values_to = "Value") %>% 
  mutate(name = paste( place, bach, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/bach.csv")

#Median household income, 201* inflation-adjusted dollars

census2000 <- data.frame(census2000 = cpi00*c(27133,38435,47883,35317,41994))
load("inputs/medhhRaw.RData")
medhh <- medhhRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  bind_cols(.,census2000) %>%
  mutate(significant = stattest(x=census2000,y=MedianHHIncome,moey=MedianHHIncomeMOE))

 medhhCSV <- medhh %>%
select(place, census2000, MedianHHIncome) %>% 
  pivot_longer(-c("place"), names_to = "medhh", values_to = "Value") %>% 
  mutate(name = paste( place, medhh, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/medhh.csv")

#Internet access
load("inputs/intaRaw.RData")
inta <- intaRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(cellonlypct = CellOnly / Total, 
         cellmoeprop = moeprop(y = Total, moex = CellOnlyMOE, moey = TotalMOE, p = cellonlypct),
         nosubpct = NoSubscript / Total,
         nosubmoeprop = moeprop(y = Total, moex = NoSubscriptMOE, moey = TotalMOE, p = nosubpct),
         noaccpct = NoAccess / Total,
         noaccmoeprop = moeprop(y = Total, moex = NoAccessMOE, moey = TotalMOE, p = noaccpct),
         
         broadband = Total-(CellOnly +NoSubscript+NoAccess),
         broadbandMOE = moeagg(cbind(TotalMOE, CellOnlyMOE, NoSubscriptMOE, NoAccessMOE)),
         broadbandpct = broadband / Total,
         broadbandmoeprop = moeprop(y= Total, moex = broadbandMOE, moey = TotalMOE, p = broadbandpct),
         
         cellonlyUS = rep(cellonlypct[5],5),
         nosubUS = rep(nosubpct[5],5),
         noaccUS = rep(noaccpct[5],5),
         broadbandUS = rep(broadbandpct[5],5),
         
         cellonlySIG = stattest(x=cellonlyUS, y = cellonlypct, moey = cellmoeprop),
         nosubSIG = stattest(x = nosubUS, y = nosubpct, moey = nosubmoeprop),
         noaccSIG = stattest(x = noaccUS, y = noaccpct, moey = noaccmoeprop),
         broadbandSIG = stattest(x = broadbandUS, y = broadbandpct, moey = broadbandmoeprop))


intaCSV <- inta %>% 
select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "inta", values_to = "Value") %>% 
  pivot_wider(id_cols = c("inta"), names_from = "place", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/inta.csv")

#Poverty rate, population for whom poverty has been determined
load("inputs/povRaw.RData")
pov <- povRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(sf1999=c(0.2794,0.1365,0.0972,0.1838,0.1238),
         pctpov = BelowPov / Total,
         moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
         significant = stattest(x=sf1999,y=pctpov,moey=moeprop))


povCSV <- pov %>% 
select(place, sf1999, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "pov", values_to = "Value") %>% 
  mutate(name = paste( place, pov, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/pov.csv")

#Children in poverty, population for whom poverty has been determined			
load("inputs/childpovRaw.RData")
childpov <- childpovRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(sf1999=c(0.4053,0.2034,0.123,0.2623,0.1656),
         TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
         moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
         TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
         moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
         pctBelowChildPov = TotBelowChildPov / TotChildPov,
         moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
         significant = stattest(x=sf1999,y=pctBelowChildPov,moey=moeprop))


childpovCSV <- childpov %>% 
select(place, sf1999, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "childpov", values_to = "Value") %>% 
  mutate(name = paste( place, childpov, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/childpov.csv")

#Households without access to a vehicle
load("inputs/vehRaw.RData")
veh <- vehRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000=c(0.2732,0.0930,0.0442,0.1532,0.1030),
         vehpct = NoVehAvail / Total,
         moeprop = moeprop(y = Total, moex = NoVehAvailMOE, moey = TotalMOE, p = vehpct),
         significant = stattest(x=census2000,y=vehpct,moey = moeprop))

vehCSV <- veh %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "veh", values_to = "Value") %>% 
  mutate(name = paste( place, veh, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/veh.csv")

#Population not U.S. citizens at birth
load("inputs/forborRaw.RData")
forbor <- forborRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(TotalPopMOE = ifelse(TotalPopMOE < 0, 0, TotalPopMOE)) %>%
  mutate(census2000=c(0.0425,0.0748,0.0237,0.048,0.1105),
         forborpct = (TotForeign00to09 + TotForeign90to99 + TotForeignPre90 + TotForeign10on) / TotalPop,
         forbormoeagg = moeagg(cbind(TotForeign00to09MOE, TotForeign90to99MOE, TotForeignPre90MOE, TotForeign10onMOE)),
         forbormoeprop = moeprop(y=TotalPop, moex = forbormoeagg, moey = TotalPopMOE, p=forborpct),
         significant = stattest(x=census2000,y=forborpct,moey=forbormoeprop))

forborCSV <- forbor %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "forbor", values_to = "Value") %>% 
  mutate(name = paste( place, forbor, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/forbor.csv")

#Population who moved in the past year
load("inputs/mobRaw.RData")
mob <- mobRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(sf2004mobabroad =c(0.0013,0.0044,0.00,0.00,0.006), #zero filled in for metro and St. Tammany 2004 because of missing data,
         sf2004states=c(0.0206,0.02,0.00,0.00,0.0235),
         sf2004difparish=c(0.0085,0.03,0.00,0.00,0.0302),
         sf2004withinparish=c(0.1131,0.0958,0.00,0.00,0.0973),
         sf2004samehouse=c(0.8565,0.8498,0.00,0.00,0.8430),
         
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
         
         abroadSIG = stattest (x=sf2004mobabroad, y=mobabroadpct, moey = mobabroadmoeprop),
         statesSIG = stattest (x=sf2004states, y=mobStatespct,moey = mobStatesmoeprop),
         difparishSIG = stattest (x=sf2004difparish, y =difparishpct, moey = difparishmoeprop),
         withinparishSIG = stattest (x=sf2004withinparish, y=withinparishpct, moey = withinparishmoeprop),
         samhouseSIG = stattest (x=sf2004samehouse, y=samehousepct, moey = samehousemoeprop))

mobCSV <- mob %>% 
select(place, (contains("pct")), (contains("sf2004"))) %>% 
  pivot_longer(-c("place"), names_to = "mob", values_to = "Value") %>% 
  mutate(year = ifelse(grepl("2004", mob), "2004", "2019"),
         header = paste(place, year, sep = "-"),
         mobfinal = ifelse(grepl("abroad", mob), "abroad", mob),
         mobfinal = ifelse(grepl("tates", mob), "tates", mobfinal),
         mobfinal = ifelse(grepl("difparish", mob), "difparish", mobfinal),
         mobfinal = ifelse(grepl("withinparish", mob), "withinparish", mobfinal),
         mobfinal = ifelse(grepl("samehouse", mob), "samehouse", mobfinal)) %>% 
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
ho <- hoRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>%
  mutate(census2000=c(0.465,0.6385,0.8048,0.6183,0.6619),
         Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         significant = stattest(x=census2000,y=Ownerpct,moey = Ownermoeprop))

hoCSV <- ho %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "ho", values_to = "Value") %>% 
  mutate(name = paste( place, ho, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/ho.csv")


#Homeowners without a mortgage
load("inputs/honomoRaw.RData")
honomo <- honomoRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000 = c(0.3298,0.3458,0.2967,0.3476,0.3259),
         honomopct = NoMortgage / Total,
         moeprop = moeprop(y=Total,moex =NoMortgageMOE,moey = TotalMOE,p=honomopct),
         significant = stattest(x=census2000,y=honomopct,moey=moeprop))

honomoCSV <- honomo %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "honomo", values_to = "Value") %>% 
  mutate(name = paste( place, honomo, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/honomo.csv")

#Renters with severe housing cost burdens
load("inputs/rentburRaw.RData")
rentbur <- rentburRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>%
  mutate(sf2004=c(0.2432,0.2167,0,0.2161,0.2384),#0 for St. tammany missing value
         sf2004lab =c(0.2432,0.2167," ",0.2161,0.2384),
         rentburpct = `50orMore`/ (Total - NotComputed),
         moeagg = moeagg(cbind(TotalMOE, NotComputedMOE)),
         moeprop = moeprop(y=(Total - NotComputed),moex=`50orMoreMOE`,moey = moeagg, p = rentburpct),
         significant = stattest(x=sf2004, y=rentburpct, moey=moeprop))

rentburCSV <- rentbur %>% 
select(place, sf2004, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "rentbur", values_to = "Value") %>% 
  mutate(name = paste( place, rentbur, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/rentbur.csv")

#Homeowners with severe housing cost burdens
load("inputs/hoburRaw.RData")
hobur <- hoburRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  mutate(sf2004=c(0.1620,0.0891,0,0.1134,0.0988), #0 for St. tammany missing value,
         hoburpct = (`50orMoreMortgage`+`50orMoreNoMortgage`)/(Total - NotComputedMortgage - NotComputedNoMortgage),
         moexagg = moeagg(cbind(`50orMoreMortgageMOE`,`50orMoreNoMortgageMOE`)),
         moeyagg = moeagg(cbind(TotalMOE, NotComputedMortgageMOE, NotComputedNoMortgageMOE)),
         moeprop = moeprop(y=Total,moex=moexagg,moey=moeyagg,p=hoburpct),
         significant = stattest(x=sf2004,y=hoburpct, moey=moeprop))

hoburCSV <- hobur %>% 
select(place, sf2004, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hobur", values_to = "Value") %>% 
  mutate(name = paste( place, hobur, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/hobur.csv")


#Median gross rent, 201* inflation-adjusted dollars
load("inputs/medrentRaw.RData")
sf2004 <- data.frame(sf2004 = cpi04*c(566,654,0,616,694))
medrent <- medrentRaw %>% 
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  bind_cols(.,sf2004) %>%
  mutate(significant = stattest(x=sf2004,y=Rent,moey=RentMOE)) 

medrentCSV <- medrent %>%
  select(place, sf2004, Rent) %>% 
  pivot_longer(-c("place"), names_to = "medrent", values_to = "Value") %>% 
  mutate(name = paste( place, medrent, sep = "-"),
         year = 2019) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/medrent.csv")


#Year structure built, 201* housing units
load("inputs/yrbuiltRaw.RData")
yrbuilt <- yrbuiltRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
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
commute <- commuteRaw %>%
  filter(place %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>% 
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
         
         Drivemoeprop = moeprop(y=Total, moex=DroveAloneMOE, moey=TotalMOE, p=Drivepct),
         carpoolmoeprop = moeprop(y=Total, moex=CarpoolMOE, moey=TotalMOE, p=Carpoolpct),
         PublicTransitmoeprop = moeprop(y=Total, moex=PublicTransitMOE, moey=TotalMOE, p=PublicTransitpct),
         bikemoeprop = moeprop(y=Total, moex=BikeMOE, moey=TotalMOE, p=bikepct),
         Walkmoeprop = moeprop(y=Total, moex=WalkMOE, moey=TotalMOE, p=Walkpct),
         workhomemoeprop = moeprop(y=Total, moex=WorkhomeMOE, moey=TotalMOE, p=Workhomepct),
         othermoeprop = moeprop(y=Total, moex=OtherMOE, moey=TotalMOE, p=Otherpct),
                  
         DriveSIG = stattest(x=census2000drive,y=Drivepct,moey = Drivemoeprop),
         carpoolSIG = stattest (x=census2000carpool, y=Carpoolpct, moey = carpoolmoeprop),
         PublicTransitSIG = stattest (x=census2000publictransit, y=PublicTransitpct, moey = PublicTransitmoeprop),
         bikeSIG = stattest(x=census2000bike, y = bikepct, moey = bikemoeprop),
         walkSIG = stattest (x=census2000walk, y = Walkpct, moey = Walkmoeprop),
         workhomeSIG = stattest ( x=census2000workhome, y=Workhomepct, moey = workhomemoeprop),
         otherSIG = stattest (x=census2000other, y= Otherpct, moey = othermoeprop))

commuteCSV <- commute %>% 
select(place, (contains("pct")), (contains("census2000"))) %>% 
  pivot_longer(-c("place"), names_to = "commute", values_to = "Value") %>% 
  mutate(year = ifelse(grepl("2000", commute), 2000, 2019),
         header = paste(place, year, sep = "-"),
         commutefinal = ifelse(grepl("ive", commute), "DroveAlone", commute),
         commutefinal = ifelse(grepl("arpool", commute), "Carpool", commutefinal),
         commutefinal = ifelse(grepl("ublic", commute), "PublicTransit", commutefinal),
         commutefinal = ifelse(grepl("Motorcycle", commute), "Motorcycle", commutefinal),
         commutefinal = ifelse(grepl("ike", commute), "Bike", commutefinal),
         commutefinal = ifelse(grepl("alk", commute), "Walk", commutefinal),
         commutefinal = ifelse(grepl("home", commute), "Workhome", commutefinal),
         commutefinal = ifelse(grepl("ther", commute), "Other", commutefinal)) %>% 
  pivot_wider(id_cols = c("commutefinal"), names_from = "header", values_from = "Value") %>%
  write.csv("outputs/spreadsheets/commute.csv")



############################################
# # PEP # #
############################################
# 2021 isn't working yet, so commenting out
##############################################

# To analyze, filter to:
# whatever combination of demographics you need: race, age, sex, hisp
# whatever geography you need: place
# whatever year of estimate that you need: date

orderDemo <- c("Orleans Parish", "Jefferson Parish", "Plaquemines Parish", "St. Bernard Parish", "St. Charles Parish",
               "St. James Parish", "St. John the Baptist Parish", "St. Tammany Parish", "Metro", "United States")
#allparishesRaw <- load("inputs/allparishesRaw.RData")

#Table 1
load("inputs/allparishesRaw.RData")
AAWhiteHispan <- allparishesRaw %>%
  filter(place == "Orleans Parish") %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total" & (raceSimple == "Black"|raceSimple == "White"|hisp == "Hispanic")) %>%
  mutate(est2000=c(128871, 323392, 14826)) %>% #check order of races in data frame. Order is bottom up
  select(raceSimple, population, est2000) %>%
  arrange(-row_number())

AAWhiteHispan %>%
  write.csv("outputs/spreadsheets/AAWhiteHispan.csv")



#Tables 2


#Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro

#HT : I just switched these around so it would make Metro last... The dataframe is not ordered by the levels of placename factor 
#that's the order that the 2000 numbers are going in.
ParishDemo2<- allparishesRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total") %>%
  group_by(raceSimple)%>%
  summarise(population=sum(population)) %>% mutate(PlaceName = "Metro") %>% select(PlaceName, population, raceSimple)

ParishDemo3 <- allparishesRaw %>% filter(PlaceName == "United States") %>%
  filter(date == "7/1/2021 population estimate") %>%
  filter(age == "Total" & sex == "Total")  %>%
  select(PlaceName, population, raceSimple)

ParishDemo1<- allparishesRaw %>%
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
  hispanic2000=c(.031,.071,.016,.051,.028,.006,.029,.025,.044,.125)) #%>%
  #.[-2,]


ParishDemo %>%
  write.csv("outputs/spreadsheets/ParishDemo.csv")



#Table 3 African American Population, New Orleans, 2000-current

#Pulling population estimates for 2010-current
load("inputs/blackpopestRaw.RData")
AAhistorical <- blackpopestRaw %>%
  select(year, POP) %>%
  arrange(year) %>%
  #.[-(2:3),] %>% #Remove 2010 estimates we don't need. We use Census Population for 2010 so we can delete 2010 Population estimate
  bind_rows(data.frame(POP = c(323392,0,0,0,0,0,133015,159887,181882,197337), row.names = (NULL)), .) %>%
  select(POP) %>%
  bind_cols(data.frame(year = as.factor(c(2000:2021))), .)


#######AA historical part 2
BlackPopyears <- allparishesRaw %>%
  filter(age == "Total" & sex == "Total")  %>%
  filter(raceSimple=="Black") %>%
  filter(place %in% c("Orleans Parish", "Jefferson Parish", "Plaquemines Parish", "St. Bernard Parish", "St. Charles Parish",
                          "St. James Parish", "St. John the Baptist Parish", "St. Tammany Parish"))

BlackpopM <- blackpopestRaw %>%
  add_row(year = "2000", place= "Orleans", POP=323392) 

BlackpopM %>%
  select(year, POP) %>%
  write.csv("outputs/spreadsheets/BlackpopM.csv")

#Table 4 Hispanic population change by population

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
  add_row(year = 2000, place= "St. John the Baptist", POP=1230)

#For excel
# HISPpopSheet1 <- HISPpopM %>%
#   select(year, place, POP) %>%
#   mutate(POP = as.numeric(POP))
#
# HiSPpopSheet <- spread(HISPpopSheet1, place, POP)

# I thought about calculating age in this way, but in the end, ACS metro-level estimate makes more sense to use -- this is kinda weird to do the weighted mean/median. Leaving this here to remember that I tried it! -- Jenna, 7/13/2020
# medAge <- as.data.frame(matrix(data = c(POPESTIMATE	= c(432493, 390144, 23197	, 47244	, 53100	, 21096	, 42837	, 260419), MEDIAN_AGE_TOT = c(39.6, 37.7, 37.1, 35.1, 38.4, 39.5, 37.8, 40.5)), ncol = 2)) %>%
#   summarise(med = mean(x = V2, wt = V1))


#Table 5 Population by age group
# HT: Why are we doing this? repeating and adding the numbers manually with mutate?

orderAge <- c(rep("Jefferson",18),rep("Orleans", 18),rep("Plaquemines",18),
              rep("St. Bernard", 18),rep("St. Charles", 18),rep("St. James", 18),
              rep("St. John The Baptist", 18),rep("St. Tammany",18))
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
# 
# Agepop <-  Agepop %>%
#   pivot_wider(id_cols = age, names_from = PlaceName, values_from = population ) #%>%
write.csv("outputs/spreadsheets/Agepop.csv")


#Table 6 Under 18 population
#Different than estimates from google sheets but aligns with American fact finder




### waiting to hear back from US census bureau for 2021 PEP
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
         year = 2019) %>%
  select(-PlaceName) %>%
pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
write.csv("outputs/spreadsheets/under18.csv")

