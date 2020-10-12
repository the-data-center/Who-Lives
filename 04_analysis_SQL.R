############################################
# # ACS # #
############################################
order <- c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")
orderHisp <- c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")

hispanRaw[hispanRaw == -555555555] <- 0 
#Hispanic Origin
hispan <- hispanRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>% 
  slice(match(orderHisp, PlaceName)) %>%
  mutate(Cubanpct = TotCuba / TotalHispOrLat,
         Dominicanpct = TotDomin / TotalHispOrLat,
         Mexicanpct = TotMex / TotalHispOrLat,
         PuertoRicanpct = TotPR / TotalHispOrLat,
         Honduranpct = TotHond / TotalHispOrLat,
         Guatemalanpct = TotGuat / TotalHispOrLat,
         Nicaraguanpct = TotNicarag / TotalHispOrLat,
         Salvadoranpct = TotSalva / TotalHispOrLat,
         OtherCApct = TotOtherCA / TotalHispOrLat,
         SouthAmericanpct =TotSA / TotalHispOrLat,
         Otherpct = TotOtherHispOrLat / TotalHispOrLat,
         
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
         
         CubanMoeProp = moeprop(y = TotalHispOrLat, moex = TotCubaMOE, moey = TotalHispOrLatMOE, p = Cubanpct),
         DominicanMoeProp = moeprop(y = TotalHispOrLat, moex = TotDominMOE, moey = TotalHispOrLatMOE, p = Dominicanpct),
         MexicanMoeProp = moeprop(y = TotalHispOrLat, moex = TotMexMOE, moey = TotalHispOrLatMOE, p = Mexicanpct),
         PuertoRicanMoeProp =moeprop(y = TotalHispOrLat, moex = TotPRMOE, moey = TotalHispOrLatMOE, p = PuertoRicanpct),
         HonduranMoeProp = moeprop(y = TotalHispOrLat, moex = TotHondMOE, moey = TotalHispOrLatMOE, p = Honduranpct),
         GuatemalanMoeProp = moeprop(y = TotalHispOrLat, moex = TotGuatMOE, moey = TotalHispOrLatMOE, p = Guatemalanpct),
         NicaraguanMoeProp = moeprop(y = TotalHispOrLat, moex = TotNicaragMOE, moey = TotalHispOrLatMOE, p = Nicaraguanpct),
         SalvadoranMoeProp = moeprop(y = TotalHispOrLat, moex = TotSalvaMOE, moey = TotalHispOrLatMOE, p = Salvadoranpct),
         OtherCAMoeProp = moeprop(y = TotalHispOrLat, moex = TotOtherCAMOE, moey = TotalHispOrLatMOE, p = OtherCApct),
         SouthAmericanMoeProp = moeprop(y = TotalHispOrLat, moex = TotSAMOE, moey = TotalHispOrLatMOE, p = SouthAmericanpct),
         OtherMoeProp= moeprop(y = TotalHispOrLat, moex = TotOtherHispOrLatMOE, moey = TotalHispOrLatMOE, p = Otherpct),
         
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

hispan %>% 
  select(-(contains("SIG"))) %>% 
  write.csv("hispan.csv")

#Households with own children under 18

hwc <- hwcRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000 = c(0.3007,0.3251,0.397,0.3353,0.3339),
         tothwc = Married + MaleHH + FemaleHH,
         pcthwc = tothwc/TotalHH,
         moeagg = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
         moeprop = moeprop(y = TotalHH, moex = moeagg, moey = TotalHHMOE, p = pcthwc),
         significant = stattest(x=census2000,y=pcthwc,moey = moeprop)) 

#hwc %>% 
#  select(-significant) %>% 
#  write.csv("hwc.csv")

#One-person households

sing <- singRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000 = c(0.331,0.2665,0.1968,0.2707,0.2578),
         pctsing = SingleHH/Total,
         moeprop = moeprop(y = Total, moex = SingleHHMOE, moey = TotalMOE, p = pctsing),
         significant = stattest(x=census2000, y=pctsing,moey = moeprop))

#sing %>% 
#  select(-significant) %>% 
#  write.csv("sing.csv")

#Less than a high school degree, adults 25 and older

hs <- hsRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000=c(0.2531,0.2073,0.1613,0.1536,0.196),
         totless = Male9 + Male9To12 + Female9 + Female9To12,
         pctless = totless/Total,
         moeagg = moeagg(cbind(Male9MOE, Male9To12MOE, Female9MOE, Female9To12MOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctless),
         significant = stattest(x=census2000, y=pctless,moey=moeprop))

#hs %>% 
#  select(-significant) %>% 
#  write.csv("hs.csv")

#Bachelor's degree or higher, adults 25 and older

bach <- bachRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000=c(0.2575,0.2149,0.2832,0.2256,0.244),
         totbach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         pctbach = totbach / Total,
         moeagg = moeagg(cbind(MaleBachMOE, MaleGradProfMOE, FemaleBachMOE, FemaleGradProfMOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         significant = stattest(x=census2000,y=pctbach,moey = moeprop))

#bach %>% 
#  select(-significant) %>% 
#  write.csv("bach.csv")

#Median household income, 201* inflation-adjusted dollars

census2000 <- data.frame(census2000 = cpi00*c(27133,38435,47883,35317,41994))
medhh <- medhhRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  bind_cols(.,census2000) %>%
  mutate(significant = stattest(x=census2000,y=MedianHHIncome,moey=MedianHHIncomeMOE))

# medhh %>%
#  select(-significant) %>%
#  write.csv("medhh.csv")

#Internet access

inta <- intaRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
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


# intaforGraphic %>% 
#   select(-(contains("SIG")), -"val") %>% 
#   write.csv("inta.csv")

#Poverty rate, population for whom poverty has been determined

pov <- povRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(sf1999=c(0.2794,0.1365,0.0972,0.1838,0.1238),
         pctpov = BelowPov / Total,
         moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
         significant = stattest(x=sf1999,y=pctpov,moey=moeprop))


#pov %>% 
#  select(-significant) %>% 
#  write.csv("pov.csv")

#Children in poverty, population for whom poverty has been determined			

childpov <- childpovRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(sf1999=c(0.4053,0.2034,0.123,0.2623,0.1656),
         TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
         moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
         TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
         moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
         pctBelowChildPov = TotBelowChildPov / TotChildPov,
         moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
         significant = stattest(x=sf1999,y=pctBelowChildPov,moey=moeprop))


#childpov %>% 
#  select(-significant) %>% 
#  write.csv("childpov.csv")

#Households without access to a vehicle

veh <- vehRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000=c(0.2732,0.0930,0.0442,0.1532,0.1030),
         vehpct = NoVehAvail / Total,
         moeprop = moeprop(y = Total, moex = NoVehAvailMOE, moey = TotalMOE, p = vehpct),
         significant = stattest(x=census2000,y=vehpct,moey = moeprop))

#veh %>% 
# select(-significant) %>% 
#  write.csv("veh.csv")

#Population not U.S. citizens at birth

forbor <- forborRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(TotalPopMOE = ifelse(TotalPopMOE < 0, 0, TotalPopMOE)) %>%
  mutate(census2000=c(0.0425,0.0748,0.0237,0.048,0.1105),
         forborpct = (TotForeign00To09 + TotForeign90To99 + TotForeignPre90 + TotForeign10On) / TotalPop,
         forbormoeagg = moeagg(cbind(TotForeign00To09MOE, TotForeign90To99MOE, TotForeignPre90MOE, TotForeign10OnMOE)),
         forbormoeprop = moeprop(y=TotalPop, moex = forbormoeagg, moey = TotalPopMOE, p=forborpct),
         significant = stattest(x=census2000,y=forborpct,moey=forbormoeprop))

#forbor %>% 
#  select(-significant) %>% 
#  write.csv("forbor.csv")

#Population who moved in the past year

mob <- mobRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(sf2004mobabroad =c(0.0013,0.0044,0.00,0.00,0.006), #zero filled in for metro and St. Tammany 2004 because of missing data,
         sf2004states=c(0.0206,0.02,0.00,0.00,0.0235),
         sf2004difparish=c(0.0085,0.03,0.00,0.00,0.0302),
         sf2004withinparish=c(0.1131,0.0958,0.00,0.00,0.0973),
         sf2004samehouse=c(0.8565,0.8498,0.00,0.00,0.8430),
         
         mobabroadpct = TotMovedFromAbroad / Total,
         mobStatespct = TotMovedBtwnStates / Total,
         difparishpct = TotMovedInState / Total,
         withinparishpct = TotMovedInCty / Total,
         samehousepct = TotSameHouse / Total,
         
         mobabroadmoeprop = moeprop(y=Total,moex = TotMovedFromAbroadMOE,moey = TotalMOE,p=mobabroadpct),
         mobStatesmoeprop = moeprop(y=Total,moex = TotMovedBtwnStatesMOE,moey = TotalMOE,p=mobStatespct),
         difparishmoeprop = moeprop(y=Total,moex = TotMovedInStateMOE,moey = TotalMOE,p=difparishpct),
         withinparishmoeprop = moeprop(y=Total,moex = TotMovedInCtyMOE,moey = TotalMOE,p=withinparishpct),
         samehousemoeprop = moeprop(y=Total, moex = TotSameHouseMOE,moey = TotalMOE,p=samehousepct),
         
         abroadSIG = stattest (x=sf2004mobabroad, y=mobabroadpct, moey = mobabroadmoeprop),
         statesSIG = stattest (x=sf2004states, y=mobStatespct,moey = mobStatesmoeprop),
         difparishSIG = stattest (x=sf2004difparish, y =difparishpct, moey = difparishmoeprop),
         withinparishSIG = stattest (x=sf2004withinparish, y=withinparishpct, moey = withinparishmoeprop),
         samhouseSIG = stattest (x=sf2004samehouse, y=samehousepct, moey = samehousemoeprop))

#mob %>% 
#  select(-(contains("SIG"))) %>% 
#  write.csv("mob.csv")

## mobility sig testing for written analysis

OPmoeagg <- moeagg(cbind(mob$TotMovedInStateMOE[1],mob$TotMovedBtwnStatesMOE[1],mob$TotMovedFromAbroadMOE[1]))
OPpct <- (mob$TotMovedInState[1]+mob$TotMovedBtwnStates[1]+mob$TotMovedFromAbroad[1])/mob$Total[1]
OPmoeprop <- moeprop(y=mob$Total[1], moex = OPmoeagg, moey = mob$TotalMOE[1], p= OPpct)
OPsig <- stattest(x = (mob$sf2004mobabroad[1]+mob$sf2004states[1]+mob$sf2004difparish[1]), y=OPpct, moey=OPmoeprop)

jeffmoeagg <- moeagg(cbind(mob$TotMovedInStateMOE[2],mob$TotMovedBtwnStatesMOE[2],mob$TotMovedFromAbroadMOE[2]))
jeffpct <- (mob$TotMovedInState[2]+mob$TotMovedBtwnStates[2]+mob$TotMovedFromAbroad[2])/mob$Total[2]
jeffmoeprop <- moeprop(y=mob$Total[2], moex = jeffmoeagg, moey = mob$TotalMOE[2], p= jeffpct)
jeffsig <- stattest(x = (mob$sf2004mobabroad[2]+mob$sf2004states[2]+mob$sf2004difparish[2]), y=jeffpct, moey=jeffmoeprop)


#Homeownership rates

ho <- hoRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000=c(0.465,0.6385,0.8048,0.6183,0.6619),
         Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         significant = stattest(x=census2000,y=Ownerpct,moey = Ownermoeprop))

#ho %>% 
#  select(-significant) %>% 
#  write.csv("ho.csv")


#Homeowners without a mortgage

honomo <- honomoRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000 = c(0.3298,0.3458,0.2967,0.3476,0.3259),
         honomopct = NoMortgage / Total,
         moeprop = moeprop(y=Total,moex =NoMortgageMOE,moey = TotalMOE,p=honomopct),
         significant = stattest(x=census2000,y=honomopct,moey=moeprop))

#honomo %>% 
#  select(-significant) %>% 
#  write.csv("honomo.csv")

#Renters with severe housing cost burdens

rentbur <- rentburRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(sf2004=c(0.2432,0.2167,0,0.2161,0.2384),#0 for St. tammany missing value
         sf2004lab =c(0.2432,0.2167," ",0.2161,0.2384),
         rentburpct = `50OrMore`/ (Total - NotComputed),
         moeagg = moeagg(cbind(TotalMOE, NotComputedMOE)),
         moeprop = moeprop(y=(Total - NotComputed),moex=`50OrMoreMOE`,moey = moeagg, p = rentburpct),
         significant = stattest(x=sf2004, y=rentburpct, moey=moeprop))

#rentbur %>% 
#  select(-significant) %>%
#  write.csv("rentbur.csv")

#Homeowners with severe housing cost burdens

hobur <- hoburRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(sf2004=c(0.1620,0.0891,0,0.1134,0.0988), #0 for St. tammany missing value,
         hoburpct = (`50OrMoreMortgage`+`50OrMoreNoMortgage`)/(Total - NotComputedMortgage - NotComputedNoMortgage),
         moexagg = moeagg(cbind(`50OrMoreMortgageMOE`,`50OrMoreNoMortgageMOE`)),
         moeyagg = moeagg(cbind(TotalMOE, NotComputedMortgageMOE, NotComputedNoMortgageMOE)),
         moeprop = moeprop(y=Total,moex=moexagg,moey=moeyagg,p=hoburpct),
         significant = stattest(x=sf2004,y=hoburpct, moey=moeprop))

#hobur %>% 
#  select(-significant) %>%
#  write.csv("hobur.csv")


#Median gross rent, 201* inflation-adjusted dollars

sf2004 <- data.frame(sf2004 = cpi04*c(566,654,0,616,694))
medrent <- medrentRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  bind_cols(.,sf2004) %>%
  mutate(significant = stattest(x=sf2004,y=Rent,moey=RentMOE)) 

medrent %>%
 select(-significant) %>%
 write.csv("medrent.csv")


#Year structure built, 201* housing units

yrbuilt <- yrbuiltRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(orLater1990pct = (`1990To1999` + `2000To2009` + `2010To2013` + `2014`)/Total,
         mid1950to1989pct = (`1950To1959`+`1960To1969`+`1970To1979`+`1980To1989`)/Total,
         orbefore1949pct = (`1940To1949`+`1939`)/Total,
         
         orlater1990moeagg = moeagg(cbind(`1990To1999MOE`,`2000To2009MOE`, `2010To2013MOE`,`2014MOE`)),
         mid1950to1989moeagg =moeagg(cbind(`1950To1959MOE`,`1960To1969MOE`,`1970To1979MOE`,`1980To1989MOE`)),
         orbefore1949moeagg = moeagg(cbind(`1940To1949MOE`,`1939MOE`)),
         
         orlater1990moeprop = moeprop(y=Total,moex = orlater1990moeagg, moey = TotalMOE,p=orLater1990pct),
         mid1950to1989moeprop = moeprop(y=Total,moex = mid1950to1989moeagg, moey = TotalMOE,p=mid1950to1989pct),
         orbefore1949moeprop = moeprop(y=Total,moex = orbefore1949moeagg, moey = TotalMOE,p=orbefore1949pct),
         
         orLater1990US = rep(orLater1990pct[5],5),
         mid1950to1989US = rep(mid1950to1989pct[5],5),
         orbefore1949US = rep(orbefore1949pct[5],5),
         
         orLater1990SIG = stattest(x=orLater1990US, y= orLater1990pct, moey = orlater1990moeprop),
         mid1950to1989SIG = stattest(x= mid1950to1989US, y= mid1950to1989pct , moey = mid1950to1989moeprop),
         orbefore1949SIG = stattest(x=orbefore1949US, y= orbefore1949pct, moey = orbefore1949moeprop))


#yrbuilt %>% 
#  select(-(contains("SIG"))) %>% 
#  write.csv("yrbuilt.csv")

#Means of transportation to work, 
commute <- commuteRaw %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>% 
  slice(match(order, PlaceName)) %>%
  mutate(census2000drive=c(0.6028,0.7855,0.8021,0.7301,0.7570),
         census2000carpool=c(0.1614,0.1372,0.1366,0.1465,0.1219),
         census2000publictransit=c(0.1322,0.0220,0.0023,0.0530,0.0457),
         census2000bike=c(0.0116,0.0032,0.0025,0.0059,0.0038),
         census2000walk=c(0.0521,0.0174,0.0082,0.0272,0.0293),
         census2000workhome=c(0.0266,0.0204,0.0368,0.0241,0.0326),
         census2000other=c(0.0133,0.0143,0.0115,0.0133,0.0097),
         
         Drivepct = DroveAlone / Total,
         Carpoolpct= Carpool / Total,
         PublicTransitpct = PublicTransit / Total,
         bikepct = Bike / Total,
         Walkpct = Walk / Total,
         Workhomepct = WorkHome / Total,
         Otherpct = Other / Total,
         
         Drivemoeprop = moeprop(y=Total, moex=DroveAloneMOE, moey=TotalMOE, p=Drivepct),
         carpoolmoeprop = moeprop(y=Total, moex=CarpoolMOE, moey=TotalMOE, p=Carpoolpct),
         PublicTransitmoeprop = moeprop(y=Total, moex=PublicTransitMOE, moey=TotalMOE, p=PublicTransitpct),
         bikemoeprop = moeprop(y=Total, moex=BikeMOE, moey=TotalMOE, p=bikepct),
         Walkmoeprop = moeprop(y=Total, moex=WalkMOE, moey=TotalMOE, p=Walkpct),
         workhomemoeprop = moeprop(y=Total, moex=WorkHomeMOE, moey=TotalMOE, p=Workhomepct),
         othermoeprop = moeprop(y=Total, moex=OtherMOE, moey=TotalMOE, p=Otherpct),
                  
         DriveSIG = stattest(x=census2000drive,y=Drivepct,moey = Drivemoeprop),
         carpoolSIG = stattest (x=census2000carpool, y=Carpoolpct, moey = carpoolmoeprop),
         PublicTransitSIG = stattest (x=census2000publictransit, y=PublicTransitpct, moey = PublicTransitmoeprop),
         bikeSIG = stattest(x=census2000bike, y = bikepct, moey = bikemoeprop),
         walkSIG = stattest (x=census2000walk, y = Walkpct, moey = Walkmoeprop),
         workhomeSIG = stattest ( x=census2000workhome, y=Workhomepct, moey = workhomemoeprop),
         otherSIG = stattest (x=census2000other, y= Otherpct, moey = othermoeprop))

#commute %>% 
#  select(-(contains("SIG"))) %>% 
#  write.csv("commute.csv")

############################################
# # PEP # #
############################################

##############################################

# To analyze, filter to:
# whatever combination of demographics you need: race, age, sex, hisp
# whatever geography you need: place
# whatever year of estimate that you need: date

orderDemo <- c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
               "St. James", "St. John the Baptist", "St. Tammany", "Metro", "United States")

#Table 1 
AAWhiteHispan <- allparishesRaw %>% 
  filter(PlaceName == "Orleans") %>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total" & (RaceSimple == "Black"|RaceSimple == "White"|RaceSimple == "Hispanic")) %>%
  mutate(est2000=c(128871, 323392, 14826)) %>% #check order of races in data frame. Order is bottom up
  select(RaceSimple, Population, est2000) %>%
  arrange(-row_number())

AAWhiteHispan %>% 
  write.csv("outputs/spreadsheets/AAWhiteHispan.csv")



#Tables 2

ParishDemo1<- allparishesRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany", "United States")) %>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total")  %>% 
  select(PlaceName, Population, RaceSimple)

#Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro
ParishDemo2<- allparishesRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                 "St. James", "St. John the Baptist", "St. Tammany")) %>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total") %>% 
  group_by(RaceSimple)%>%
  summarise(Population=sum(Population)) %>%
  mutate(PlaceName = c("Metro", "Metro", "Metro", "Metro", "Metro")) %>%
  bind_rows(.,ParishDemo1)

#reshape data from long to wide for easy analysis
ParishDemo <- spread(ParishDemo2, RaceSimple, Population) %>%
  filter(PlaceName != "Louisiana") %>%
  slice(match(orderDemo, PlaceName)) %>% 
  mutate(pctwhite = White / Total,
         pctblack = Black / Total,
         pctasian = Asian / Total,
         pcthisp = Hispanic / Total,
         white2000=c(.266,.645,.688,.844,.705,.497,.51 ,.853,.547,.691),
         black2000=c(.667,.227,.233,.076,.251,.492,.446,.098,.373,.121),
         asian2000=c(.023,.031,.026,.013,.006,0   ,.005,.008,.021,.037),
      hispanic2000=c(.031,.071,.016,.051,.028,.006,.029,.025,.044,.125)) #%>%
  #.[-2,]


ParishDemo %>% 
  write.csv("outputs/spreadsheets/ParishDemo.csv")



#Table 3 African American Population, New Orleans, 2000-current

#Pulling population estimates for 2010-current
AAhistorical <- allparishesRaw %>% 
  filter(PlaceName == "Orleans")%>% 
  filter(RaceSimple=="Black")%>% 
  filter(SexName=="Total")%>% 
  filter(AgeGroupName=="Total")%>% 
  select(DateDesc, Population) %>%
  arrange(DateDesc) %>%
  .[-(2:3),] %>% #Remove 2010 estimates we don't need. We use Census Population for 2010 so we can delete 2010 Population estimate
  bind_rows(data.frame(Population = c(323392,0,0,0,0,0,133015,159887,181882,197337), row.names = (NULL)), .) %>%
  select(Population) %>%
  bind_cols(data.frame(year = as.factor(c(2000:yearPEP))), .)


#######AA historical part 2
BlackPopyears <- allparishesRaw %>% 
  filter(AgeGroupName == "Total" & SexName == "Total")  %>% 
  filter(RaceSimple=="Black") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany"))

BlackpopM <- blackpopestRaw %>% 
  add_row(CensusYear = 2000, PlaceName= "Orleans", Population=323392) 

BlackpopM %>% 
  select(CensusYear, Population) %>% 
  write.csv("outputs/spreadsheets/BlackpopM.csv")

#Table 4 Hispanic population change by population

HispanicPop <- allparishesRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(AgeGroupName == "Total" & SexName == "Total")  %>% 
  filter(RaceSimple=="Hispanic")%>% 
  select(PlaceName, Population) %>%
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
  filter(AgeGroupName == "Total" & SexName == "Total")  %>% 
  filter(RaceSimple=="Hispanic") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) 

HispanicPopyears %>%
  select(PlaceName, DateDesc, Population) %>% 
  pivot_wider(id_cols = DateDesc, names_from = PlaceName, values_from = Population ) %>% 
  write.csv("outputs/spreadsheets/HispanicPopyears.csv")


HISPpopM <- hisppopestRaw %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>% 
  add_row(CensusYear = 2000, PlaceName= "Orleans", Population=14826) %>% 
  add_row(CensusYear = 2000, PlaceName= "Jefferson", Population=32418) %>%
  add_row(CensusYear = 2000, PlaceName= "St. Tammany", Population=4737) %>% 
  add_row(CensusYear = 2000, PlaceName= "Plaquemines", Population=433) %>% 
  add_row(CensusYear = 2000, PlaceName= "St. Bernard", Population=3425) %>% 
  add_row(CensusYear = 2000, PlaceName= "St. Charles", Population=1346) %>% 
  add_row(CensusYear = 2000, PlaceName= "St. James", Population=130) %>% 
  add_row(CensusYear = 2000, PlaceName= "St. John the Baptist", Population=1230)

#For excel
# HISPpopSheet1 <- HISPpopM %>%
#   select(year, PlaceName, POP) %>%
#   mutate(POP = as.numeric(POP))
# 
# HiSPpopSheet <- spread(HISPpopSheet1, PlaceName, POP) 

# I thought about calculating age in this way, but in the end, ACS metro-level estimate makes more sense to use -- this is kinda weird to do the weighted mean/median. Leaving this here to remember that I tried it! -- Jenna, 7/13/2020
# medAge <- as.data.frame(matrix(data = c(POPESTIMATE	= c(432493, 390144, 23197	, 47244	, 53100	, 21096	, 42837	, 260419), MEDIAN_AGE_TOT = c(39.6, 37.7, 37.1, 35.1, 38.4, 39.5, 37.8, 40.5)), ncol = 2)) %>%
#   summarise(med = mean(x = V2, wt = V1))


#Table 5 Population by age group
orderAge <- c(rep("Jefferson",18),rep("Orleans", 18),rep("Plaquemines",18),
              rep("St. Bernard", 18),rep("St. Charles", 18),rep("St. James", 18),
              rep("St. John The Baptist", 18),rep("St. Tammany",18))
Agepop <- allparishesRaw %>% 
  filter(AgeGroupName== "Under 5 years" | AgeGroupName== "5 to 9"| AgeGroupName== "10 to 14" | AgeGroupName== "15 to 19"|
           AgeGroupName=="20 to 24"| AgeGroupName== "25 to 29"| AgeGroupName== "30 to 34"| AgeGroupName== "35 to 39"| AgeGroupName== "40 to 44"
         | AgeGroupName== "45 to 49" | AgeGroupName=="50 to 54"| AgeGroupName== "55 to 59"| AgeGroupName== "60 to 64"| AgeGroupName== "65 to 69"| 
           AgeGroupName== "70 to 74"| AgeGroupName== "75 to 79"| AgeGroupName== "80 to 84"| AgeGroupName== "85 plus")%>% 
  filter(RaceSimple=="Total")%>% 
  filter(SexName=="Total")%>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>% 
  arrange(factor(PlaceName, levels = c("Jefferson","Orleans","Plaquemines",
                 "St. Bernard","St. Charles","St. James",
                 "St. John the Baptist","St. Tammany"))) %>%
  select(AgeGroupName, PlaceName, Population) %>%
  mutate(est2000 = c(30226,31811,32657,32436,29793,31838,32713,36367,36834,34166,30658,23741,17911,15034,14991,11973,6942,5375,33496,
                     37133,36769,38312,38932,36416,34050,35053,36444,34562,29128,21068,16658,14648,14301,12458,7838,7408,1977,2183,2241,
                     2197,1668,1621,2024,2271,2247,1855,1554,1272,1034,854,769,524,259,207,4242,4639,4996,5021,4257,4196,4584,5327,5530,
                     4939,4398,3241,2597,2569,2714,2148,1051,780,3511,3994,4352,4063,2649,2662,3440,4407,4569,3732,2872,2025,1488,1345,
                     1244,859,462,398,1483,1711,1863,1936,1346,1142,1439,1671,1713,1496,1220,918,916,736,616,448,275,287,3463,3692,3874,
                     3837,2721,2699,3118,3612,3588,3240,2503,1907,1434,1006,925,663,416,346,13556,15029,16147,14672,9045,10257,12729,16457,
                     17655,16062,13641,9733,7125,5825,5168,4033,2296,1838))

Agepop %>%
  select(-est2000) %>% 
  pivot_wider(id_cols = AgeGroupName, names_from = PlaceName, values_from = Population ) %>% 
  write.csv("outputs/spreadsheets/Agepop.csv")


#Table 6 Under 18 population
#Different than estimates from google sheets but aligns with American fact finder

under18pars<-allparishesRaw %>% 
  filter(AgeGroupName=="18 years and over" | AgeGroupName=="Total")%>% 
  filter(RaceSimple=="Total")%>% 
  filter(SexName=="Total")%>% 
  filter(DateDesc == "7/1/2019 population estimate") %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany")) %>%  
  select(AgeGroupName, PlaceName, Population)

#Creating metro
under18metro<- under18pars %>%
  group_by(AgeGroupName)%>%
  summarise(Population=sum(Population)) %>%
  mutate(PlaceName = c("Metro", "Metro"))

#stack back with other under 18
popunder18 <-bind_rows(under18pars,under18metro) %>%
  spread(., AgeGroupName, Population) %>%
  mutate(under18 = Total-`18 years and over`) %>%
  filter(PlaceName == "Orleans" | PlaceName=="Jefferson" | PlaceName == "St. Tammany" | PlaceName == "Metro")%>%
  select(PlaceName, under18) %>%
  mutate(est2000=c(115255,358092,129408,54399))

