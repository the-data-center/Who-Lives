############################################
# # ACS # #
############################################
order <- c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")
orderHisp <- c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")

load("inputs/totalpop_metro.RData")
load("inputs/hispanRaw.RData")
load("inputs/RacepopestRaw.RData")

hispanRaw[hispanRaw == -555555555] <- 0 
#Hispanic Origin
hispan <- hispanRaw %>%
  filter(placename %in% c("Orleans", "Jefferson","New Orleans Metro Area", "United States")) %>%
  #slice(match(orderHisp, place)) %>%
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

         ######
         
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
  select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "Hispanic origin", values_to = "Value") %>% 
  pivot_wider(id_cols = c("Hispanic origin"), names_from = "place", values_from = "Value")

write.csv(hispanCSV, "outputs/spreadsheets/hispan.csv")
#storage_write_csv(hispanCSV, cont_proj, "who_lives/2024/outputs/hispan.csv")

#Households with own children under 18
load("inputs/hwcRaw.RData")
load("inputs/hwc2000Raw.RData")

hwc <- hwcRaw %>%
  left_join(hwc2000Raw %>% select(-place), by = "placename") %>% #do something about the mismatch place codes
  mutate(tothwc2000 = Married15to64_2000 + MaleHH15to64_2000 + FemaleHH15to64_2000 + Married65plus_2000 + MaleHH65plus_2000 + FemaleHH65plus_2000,
         tothwc2000MOE = moeagg(cbind(Married15to64_2000MOE, MaleHH15to64_2000MOE, FemaleHH15to64_2000MOE, Married65plus_2000MOE, MaleHH65plus_2000MOE, FemaleHH65plus_2000MOE)),
         
         pcthwc2000 = tothwc2000 / TotalHH_2000,
         
         tothwc = Married + MaleHH + FemaleHH,
         pcthwc = tothwc/TotalHH,
         moeagg = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
         
         moeprop = moeprop(y = TotalHH, moex = moeagg, moey = TotalHHMOE, p = pcthwc),
         moeprop2000 = moeprop(y = TotalHH_2000, moex = tothwc2000MOE, moey = TotalHH_2000MOE, p = pcthwc2000),
         significant = stattest(x=pcthwc2000,moex = moeprop2000, y=pcthwc,moey = moeprop)) 

hwcCSV <- hwc %>% 
  select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hwc", values_to = "Value") %>% 
  mutate(name = paste( place, hwc, sep = "-"),
         year = year) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")

write.csv(hwcCSV, "outputs/spreadsheets/hwc.csv")
#storage_write_csv(hwcCSV, cont_proj, "who_lives/2024/outputs/hwc.csv")

#One-person households
load("inputs/singRaw.RData")
load("inputs/singRaw2000.RData") #weird formatting, fixing
singRaw2000_1 <- singRaw2000 %>% select(placename, POP, TotalHH_2000, TotalHH_2000MOE) %>% na.omit()
singRaw2000_2 <- singRaw2000 %>% select(placename, POP, SingleHH_2000, SingleHH_2000MOE) %>% na.omit()
singRaw2000 <- singRaw2000_1 %>% left_join(singRaw2000_2, by = c("placename", "POP"))

sing <- singRaw %>%
  left_join(singRaw2000, by = "placename") %>%
  mutate(census2000 = SingleHH_2000 / TotalHH_2000,
         census2000MOE = moeprop(TotalHH_2000, SingleHH_2000MOE, TotalHH_2000MOE, census2000),
         pctsing = SingleHH/TotalHH,
         moeprop = moeprop(y = TotalHH, moex = SingleHHMOE, moey = TotalMOE, p = pctsing),
         significant = stattest(x=census2000, moex = census2000MOE, y=pctsing,moey = moeprop))

singCSV <- sing %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "sing", values_to = "Value") %>% 
  mutate(name = paste( place, sing, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(singCSV, "outputs/spreadsheets/sing.csv")
#storage_write_csv(singCSV, cont_proj, "who_lives/2024/outputs/sing.csv")

#Less than a high school degree, adults 25 and older
load("inputs/hsRaw.RData")
load("inputs/hsRaw2000.RData")
hs <- hsRaw %>%
  left_join(hsRaw2000 %>% select(-place), by = "placename") %>%
  mutate(totless2000= MaleNS_2000 + Male4_2000 + Male5to6_2000 + Male7to8_2000 + Male9_2000 + Male10_2000 + Male11_2000 + Male12ND_2000 + FemaleNS_2000 + Female4_2000 + Female5to6_2000 + Female7to8_2000 + Female9_2000 + Female10_2000 + Female11_2000 + Female12ND_2000,
         totless2000MOE = moeagg(cbind(MaleNS_2000MOE, Male4_2000MOE, Male5to6_2000MOE, Male7to8_2000MOE, Male9_2000MOE, Male10_2000MOE, Male11_2000MOE, Male12ND_2000MOE, FemaleNS_2000MOE, Female4_2000MOE, Female5to6_2000MOE, Female7to8_2000MOE, Female9_2000MOE, Female10_2000MOE, Female11_2000MOE, Female12ND_2000MOE)),
         census2000 = totless2000 / Total_2000,
         census2000MOE = moeprop(Total_2000, totless2000MOE, Total_2000MOE, census2000),
         totless = Male9 + Male9to12 + Female9 + Female9to12,
         pctless = totless/Total,
         moeagg = moeagg(cbind(Male9MOE, Male9to12MOE, Female9MOE, Female9to12MOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctless),
         significant = stattest(x=census2000, moex = census2000MOE, y=pctless,moey=moeprop))

hsCSV <- hs %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hs", values_to = "Value") %>% 
  mutate(name = paste( place, hs, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(hsCSV, "outputs/spreadsheets/hs.csv")
#storage_write_csv(hsCSV, cont_proj, "who_lives/2024/outputs/hs.csv")

#Bachelor's degree or higher, adults 25 and older
load("inputs/bachRaw.RData")
load("inputs/bachRaw2000.RData")
bach <- bachRaw  %>%
  left_join(bachRaw2000 %>% select(-place), by = "placename") %>% 
  mutate(totbach2000 = MaleBach_2000 + MaleMasters_2000 + MaleProf_2000 + MaleDoc_2000 + FemaleBach_2000 + FemaleMasters_2000 + FemaleProf_2000 + FemaleDoc_2000,
         totbach2000MOE = moeagg(cbind(MaleBach_2000MOE,  MaleMasters_2000MOE,  MaleProf_2000MOE,  MaleDoc_2000MOE,  FemaleBach_2000MOE,  FemaleMasters_2000MOE,  FemaleProf_2000MOE,  FemaleDoc_2000MOE)),
         census2000 = totbach2000 / Total_2000, 
         census2000MOE = moeprop(Total_2000, totbach2000MOE, Total_2000MOE, census2000),
         totbach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         pctbach = totbach / Total,
         moeagg = moeagg(cbind(MaleBachMOE, MaleGradProfMOE, FemaleBachMOE, FemaleGradProfMOE)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         significant = stattest(x=census2000, moex = census2000MOE, y=pctbach,moey = moeprop))

bachCSV <- bach  %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "bach", values_to = "Value") %>% 
  mutate(name = paste( place, bach, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(bachCSV, "outputs/spreadsheets/bach.csv")
#storage_write_csv(bachCSV, cont_proj, "who_lives/2024/outputs/bach.csv")

#Median household income, 201* inflation-adjusted dollars

load("inputs/medhhRaw.RData")
load("inputs/medhhRaw2000.RData")
medhh <- medhhRaw %>%
  left_join(medhhRaw2000 %>% select(-place), by = "placename") %>%
  mutate(census2000 = ifelse(placename == "New Orleans Metro Area", 0, MedianHHIncome_2000 *cpi99),
         census2000MOE = MedianHHIncome_2000MOE * cpi99,
         significant = stattest(x=census2000,moex = census2000MOE, y=MedianHHIncome,moey=MedianHHIncomeMOE))

 medhhCSV <- medhh %>%
select(place, census2000, MedianHHIncome) %>% 
  pivot_longer(-c("place"), names_to = "medhh", values_to = "Value") %>% 
  mutate(name = paste( place, medhh, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(medhhCSV, "outputs/spreadsheets/medhh.csv")
#storage_write_csv(medhhCSV, cont_proj, "who_lives/2024/outputs/medhh.csv")

#Internet access
load("inputs/intaRaw.RData")
inta <- intaRaw %>%
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
         
         cellonlyUS = rep(cellonlypct[4],4),
         nosubUS = rep(nosubpct[4],4),
         noaccUS = rep(noaccpct[4],4),
         broadbandUS = rep(broadbandpct[4],4),
         
         cellonlyUSMOE = rep(cellmoeprop[4],4),
         nosubUSMOE = rep(nosubmoeprop[4],4),
         noaccUSMOE = rep(noaccmoeprop[4],4),
         broadbandUSMOE = rep(broadbandmoeprop[4],4),
         
         cellonlySIG = stattest(x=cellonlyUS, moex = cellonlyUSMOE, y = cellonlypct, moey = cellmoeprop),
         nosubSIG = stattest(x = nosubUS, moex = nosubUSMOE,y = nosubpct, moey = nosubmoeprop),
         noaccSIG = stattest(x = noaccUS, moex = noaccUSMOE, y = noaccpct, moey = noaccmoeprop),
         broadbandSIG = stattest(x = broadbandUS, moex = broadbandUSMOE, y = broadbandpct, moey = broadbandmoeprop))


intaCSV <- inta %>% 
select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "inta", values_to = "Value") %>% 
  pivot_wider(id_cols = c("inta"), names_from = "place", values_from = "Value") 
write.csv(intaCSV, "outputs/spreadsheets/inta.csv") 
#storage_write_csv(intaCSV, cont_proj, "who_lives/2024/outputs/inta.csv")

#Poverty rate, population for whom poverty has been determined
load("inputs/povRaw.RData")
load("inputs/povRaw2000.RData")
pov <- povRaw %>%
  left_join(povRaw2000 %>% select(-place), by = "placename") %>%
  mutate(sf1999= BelowPov_2000 / Total_2000,
         sf1999MOE = moeprop(Total_2000, BelowPov_2000MOE, Total_2000MOE, p = sf1999),
         pctpov = BelowPov / Total,
         moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
         significant = stattest(x=sf1999, moex = sf1999MOE, y=pctpov,moey=moeprop))


povCSV <- pov %>% 
select(place, sf1999, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "pov", values_to = "Value") %>% 
  mutate(name = paste( place, pov, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") 
write.csv(povCSV, "outputs/spreadsheets/pov.csv") 
#storage_write_csv(povCSV, cont_proj, "who_lives/2024/outputs/pov.csv")

#Children in poverty, population for whom poverty has been determined			
load("inputs/childpovRaw.RData")
load("inputs/childpovRaw2000.RData")
childpov <- childpovRaw %>%
  left_join(childpovRaw2000 %>% select(-place), by = "placename") %>%
  mutate(BelowPov_2000 = BelowPovUnder5Years_2000 + BelowPov5Years_2000 + BelowPov6to11Years_2000 + BelowPov12to17Years_2000,
         BelowPov_2000MOE = moeagg(cbind(BelowPovUnder5Years_2000MOE,BelowPov5Years_2000MOE,BelowPov6to11Years_2000MOE,BelowPov12to17Years_2000MOE)),
         Total_2000 = BelowPovUnder5Years_2000 + BelowPov5Years_2000 + BelowPov6to11Years_2000 + BelowPov12to17Years_2000 + AbovePovUnder5Years_2000 + AbovePov5Years_2000 + AbovePov6to11Years_2000 + AbovePov12to17_2000,
         Total_2000MOE = moeagg(cbind(BelowPovUnder5Years_2000MOE, BelowPov5Years_2000MOE,BelowPov6to11Years_2000MOE,BelowPov12to17Years_2000MOE , AbovePovUnder5Years_2000MOE , AbovePov5Years_2000MOE,AbovePov6to11Years_2000MOE ,AbovePov12to17_2000MOE)),
         sf1999= BelowPov_2000 / Total_2000,
         sf1999MOE = moeprop(Total_2000, BelowPov_2000MOE, Total_2000MOE, sf1999),
         TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
         moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
         TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
         moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
         pctBelowChildPov = TotBelowChildPov / TotChildPov,
         moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
         significant = stattest(x=sf1999, moex = sf1999MOE, y=pctBelowChildPov,moey=moeprop))


childpovCSV <- childpov %>% 
select(place, sf1999, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "childpov", values_to = "Value") %>% 
  mutate(name = paste( place, childpov, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(childpovCSV, "outputs/spreadsheets/childpov.csv")
#storage_write_csv(childpovCSV, cont_proj, "who_lives/2024/outputs/childpov.csv")

#Households without access to a vehicle
load("inputs/vehRaw.RData")
load("inputs/vehRaw2000.RData")
veh <- vehRaw %>%
  left_join(vehRaw2000 %>% select(-place), by = "placename") %>%
  mutate(noveh2000 = OwnNoVehAvail_2000 + RentNoVehAvail_2000,
         noveh2000MOE = moeagg(cbind(OwnNoVehAvail_2000MOE, RentNoVehAvail_2000MOE)),
         census2000 = noveh2000 / Total_2000,
         census2000MOE = moeprop(Total_2000, noveh2000MOE, Total_2000MOE, census2000),
         vehpct = NoVehAvail / Total,
         moeprop = moeprop(y = Total, moex = NoVehAvailMOE, moey = TotalMOE, p = vehpct),
         significant = stattest(x=census2000, moex= census2000MOE,y=vehpct,moey = moeprop))

vehCSV <- veh %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "veh", values_to = "Value") %>% 
  mutate(name = paste( place, veh, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(vehCSV, "outputs/spreadsheets/veh.csv")
#storage_write_csv(vehCSV, cont_proj, "who_lives/2024/outputs/veh.csv")

#Population not U.S. citizens at birth
load("inputs/forborRaw.RData")
load("inputs/forborRaw2000.RData")

#Population not U.S. citizens at birth
load("inputs/forborRaw.RData")
load("inputs/forborRaw2000.RData")

forborRaw2000_1 <- forborRaw2000 %>% select(placename, Total_2000, Total_2000MOE) %>% mutate(Total_2000MOE = 0) %>% na.omit()
forborRaw2000_2 <- forborRaw2000 %>% select(-c(place, POP, Total_2000, Total_2000MOE))%>% na.omit()
forborRaw2000 <- forborRaw2000_1 %>% left_join(forborRaw2000_2, by = "placename")

forbor <- forborRaw %>%
  left_join(forborRaw2000, by = "placename") %>%
  mutate(TotalPopMOE = ifelse(TotalPopMOE < 0, 0, TotalPopMOE)) %>%
  mutate(forbor2000 = TotForeign95to00_2000 + TotForegin90to94_2000 + TotForegin85to89_2000 + TotForeign80to84_2000 + TotForegin75to79_2000 + TotForeign70to74_2000 + TotForegin65to69_2000 + TotForeginPre65_2000,
         forbor2000MOE = moeagg(cbind(TotForeign95to00_2000MOE, TotForegin90to94_2000MOE, TotForegin85to89_2000MOE, TotForeign80to84_2000MOE, TotForegin75to79_2000MOE, TotForeign70to74_2000MOE, TotForegin65to69_2000MOE, TotForeginPre65_2000MOE)),
         census2000= forbor2000 / Total_2000,
         census2000MOE = moeprop(Total_2000, forbor2000MOE, Total_2000MOE, census2000),
         forborpct = (TotForeign00to09 + TotForeign90to99 + TotForeignPre90 + TotForeign10on) / TotalPop,
         forbormoeagg = moeagg(cbind(TotForeign00to09MOE, TotForeign90to99MOE, TotForeignPre90MOE, TotForeign10onMOE)),
         forbormoeprop = moeprop(y=TotalPop, moex = forbormoeagg, moey = TotalPopMOE, p=forborpct),
         significant = stattest(x=census2000,moex = census2000MOE, y=forborpct,moey=forbormoeprop))


forborCSV <- forbor %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "forbor", values_to = "Value") %>% 
  mutate(name = paste( place, forbor, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") 
write.csv(forborCSV, "outputs/spreadsheets/forbor.csv") 
#storage_write_csv(forborCSV, cont_proj, "who_lives/2024/outputs/forbor.csv")

#Population who moved in the past year

load("inputs/mobRaw.RData")
load("inputs/mob04Raw.RData")
mob <- mobRaw %>%
  left_join(mob04Raw, by = "placename") %>%
  mutate(mobabroadpct = TotMovedfromAbroad / Total,
         mobStatespct = TotMovedbtwnStates / Total,
         difparishpct = TotMovedinState / Total,
         withinparishpct = TotMovedinCty / Total,
         samehousepct = TotSameHouse / Total,
         
         mobabroadmoeprop = moeprop(y=Total,moex = TotMovedfromAbroadMOE,moey = TotalMOE,p=mobabroadpct),
         mobStatesmoeprop = moeprop(y=Total,moex = TotMovedbtwnStatesMOE,moey = TotalMOE,p=mobStatespct),
         difparishmoeprop = moeprop(y=Total,moex = TotMovedinStateMOE,moey = TotalMOE,p=difparishpct),
         withinparishmoeprop = moeprop(y=Total,moex = TotMovedinCtyMOE,moey = TotalMOE,p=withinparishpct),
         samehousemoeprop = moeprop(y=Total, moex = TotSameHouseMOE,moey = TotalMOE,p=samehousepct))

mob <- mob %>% mutate_all(funs(replace_na(.,0))) %>%
  mutate(
         
         abroadSIG = stattest(x=sf2004mobabroadpct, moex = ifelse(is.na(sf2004mobabroadpctMOE), 0, sf2004mobabroadpctMOE), y=mobabroadpct, moey = mobabroadmoeprop),
         statesSIG = stattest(x=sf2004mobStatespct, moex = ifelse(is.na(sf2004mobStatespctMOE), 0, sf2004mobStatespctMOE), y=mobStatespct,moey = mobStatesmoeprop),
         difparishSIG = stattest(x=sf2004difparishpct, moex = ifelse(is.na(sf2004difparishpctMOE), 0, sf2004difparishpctMOE), y =difparishpct, moey = difparishmoeprop),
         withinparishSIG = stattest(x=sf2004withinparishpct, moex = ifelse(is.na(sf2004withinparishpctMOE), 0 , sf2004withinparishpctMOE), y=withinparishpct, moey = withinparishmoeprop),
         samhouseSIG = stattest (x=sf2004samehousepct, moex = ifelse(is.na(sf2004samehousepctMOE), 0, sf2004samehousepctMOE), y=samehousepct, moey = samehousemoeprop))


mobCSV <- mob %>% 
select(place,  (contains("sf2004") & !contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "mob", values_to = "Value") %>% 
  mutate(year = ifelse(grepl("2004", mob), "2004", "2023"),
         header = paste(place, year, sep = "-"),
         mobfinal = ifelse(grepl("abroad", mob), "abroad", mob),
         mobfinal = ifelse(grepl("tates", mob), "tates", mobfinal),
         mobfinal = ifelse(grepl("difparish", mob), "difparish", mobfinal),
         mobfinal = ifelse(grepl("withinparish", mob), "withinparish", mobfinal),
         mobfinal = ifelse(grepl("samehouse", mob), "samehouse", mobfinal),
         mobfinal = factor(mobfinal, levels = c("samehouse", "withinparish", "difparish", "tates", "abroad"))) %>% 
  arrange(mobfinal) %>%
  pivot_wider(id_cols = c("mobfinal"), names_from = "header", values_from = "Value") 
#write.csv(mobCSV, "outputs/spreadsheets/mob.csv") 
#storage_write_csv(mobCSV, cont_proj, "who_lives/2024/outputs/mob.csv")

## mobility sig testing for written analysis

OPmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[1],mob$TotMovedbtwnStatesMOE[1],mob$TotMovedfromAbroadMOE[1]))
OPpct <- (mob$TotMovedinState[1]+mob$TotMovedbtwnStates[1]+mob$TotMovedfromAbroad[1])/mob$Total[1]
OPmoeprop <- moeprop(y=mob$Total[1], moex = OPmoeagg, moey = mob$TotalMOE[1], p= OPpct)
OPsig <- stattest(x = (mob$sf2004mobabroadpct[1]+mob$sf2004mobStatespct[1]+mob$sf2004difparishpct[1]), y=OPpct, moey=OPmoeprop, moex = moeagg(cbind(mob$sf2004mobabroadpctMOE[1], mob$sf2004mobStatespctMOE[1],mob$sf2004difparishpctMOE[1])))

jeffmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[2],mob$TotMovedbtwnStatesMOE[2],mob$TotMovedfromAbroadMOE[2]))
jeffpct <- (mob$TotMovedinState[2]+mob$TotMovedbtwnStates[2]+mob$TotMovedfromAbroad[2])/mob$Total[2]
jeffmoeprop <- moeprop(y=mob$Total[2], moex = jeffmoeagg, moey = mob$TotalMOE[2], p= jeffpct)
jeffsig <- stattest(x = (mob$sf2004mobabroadpct[2]+mob$sf2004mobStatespct[2]+mob$sf2004difparishpct[2]), y=jeffpct, moey=jeffmoeprop, moex = moeagg(cbind(mob$sf2004mobabroadpctMOE[2], mob$sf2004mobStatespctMOE[2],mob$sf2004difparishpctMOE[2])))


#Homeownership rates
load("inputs/hoRaw.RData")
load("inputs/hoRaw2000.RData")
ho <- hoRaw %>% left_join(hoRaw2000 %>% select(-place), by = "placename") %>%
  mutate(Ownerpct = Owner / Total,
         Ownerpct2000 = Owner_2000 / Total_2000,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         Ownerpct2000moe = moeprop(y = Total_2000, moex = Owner_2000MOE, moey = Total_2000MOE, p = Ownerpct2000),
         significant = stattest(x=Ownerpct2000 ,y=Ownerpct,moey = Ownermoeprop, moex = Ownerpct2000moe))

hoCSV <- ho %>% 
select(place, (contains("pct"))) %>% select(place, Ownerpct2000, Ownerpct) %>%
  pivot_longer(-c("place"), names_to = "ho", values_to = "Value") %>% 
  mutate(name = paste( place, ho, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") 
write.csv(hoCSV, "outputs/spreadsheets/ho.csv") 
#storage_write_csv(hoCSV, cont_proj, "who_lives/2024/outputs/ho.csv")


#Homeowners without a mortgage
load("inputs/honomoRaw.RData")
load("inputs/honomoRaw2000.RData")
honomo <- honomoRaw %>%
  left_join(honomoRaw2000 %>% select(-place), by = "placename") %>%
  mutate(census2000 = NoMortgage_2000 / Total_2000,
         census2000MOE = moeprop(Total_2000, NoMortgage_2000MOE, Total_2000MOE, census2000),
         honomopct = NoMortgage / Total,
         moeprop = moeprop(y=Total,moex =NoMortgageMOE,moey = TotalMOE,p=honomopct),
         significant = stattest(x=census2000, moex = census2000MOE, y=honomopct,moey=moeprop))

honomoCSV <- honomo %>% 
select(place, census2000, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "honomo", values_to = "Value") %>% 
  mutate(name = paste( place, honomo, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(honomoCSV, "outputs/spreadsheets/honomo.csv")
#storage_write_csv(honomoCSV, cont_proj, "who_lives/2024/outputs/honomo.csv")

#Renters with severe housing cost burdens
load("inputs/rentburRaw.RData")
load("inputs/rentburRaw2004.RData")
rentbur <- rentburRaw %>%
  left_join(rentburRaw2004, by = "placename") %>%
  mutate(#sf2004=c(0.2432,0.2167,0,0.2161,0.2384),#0 for St. tammany missing value ### ****HT commenting out old values, adding ones I pulled from 2004 ACS
         #order is "Orleans", "Jefferson", "New Orleans Metro Area", "United States"
         sf2004 = ifelse(is.na(rentburpct2004),0,rentburpct2004),
         sf2004MOE = ifelse(is.na(rentburpct2004MOE),0,rentburpct2004MOE),
         rentburpct = `50orMore`/ (Total - NotComputed),
         moeagg = moeagg(cbind(TotalMOE, NotComputedMOE)),
         moeprop = moeprop(y=(Total - NotComputed),moex=`50orMoreMOE`,moey = moeagg, p = rentburpct),
         significant = stattest(x=sf2004, moex = sf2004MOE, y=rentburpct, moey=moeprop))

rentburCSV <- rentbur %>% 
select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "rentbur", values_to = "Value") %>% 
  mutate(name = paste( place, rentbur, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(rentburCSV, "outputs/spreadsheets/rentbur.csv")
#storage_write_csv(rentburCSV, cont_proj, "who_lives/2024/outputs/rentbur.csv")

#Homeowners with severe housing cost burdens
load("inputs/hoburRaw.RData")
load("inputs/rentburRaw2004.RData") #this one has both variables
hobur <- hoburRaw %>%
  left_join(rentburRaw2004, by = "placename") %>%
  mutate(sf2004 = ifelse(is.na(hoburpct2004), 0,hoburpct2004),
         sf2004MOE = ifelse(is.na(hoburpct2004MOE),0,hoburpct2004MOE), #getting these numbers from line 154 of data-pull.R
         hoburpct = (`50orMoreMortgage`+`50orMoreNoMortgage`)/(Total - NotComputedMortgage - NotComputedNoMortgage),
         moexagg = moeagg(cbind(`50orMoreMortgageMOE`,`50orMoreNoMortgageMOE`)),
         moeyagg = moeagg(cbind(TotalMOE, NotComputedMortgageMOE, NotComputedNoMortgageMOE)),
         moeprop = moeprop(y=Total,moex=moexagg,moey=moeyagg,p=hoburpct),
         significant = stattest(x=sf2004,moex = sf2004MOE, y=hoburpct, moey=moeprop))

hoburCSV <- hobur %>% 
select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "hobur", values_to = "Value") %>% 
  mutate(name = paste( place, hobur, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(hoburCSV, "outputs/spreadsheets/hobur.csv")
#storage_write_csv(hoburCSV, cont_proj, "who_lives/2024/outputs/hobur.csv")


#Median gross rent, 201* inflation-adjusted dollars
load("inputs/medrentRaw.RData")
#sf2004 <- data.frame(sf2004 = cpi04*c(566,654,0,616,694))### ****HT commenting out old values, adding ones I pulled from 2004 ACS
#order is "Orleans", "Jefferson", "New Orleans Metro Area", "United States"

medrent <- medrentRaw %>%
  left_join(rentburRaw2004, by = "placename") %>%
  mutate(sf2004 = ifelse(is.na(medgrossrent2004),0,medgrossrent2004) * cpi04,
         sf2004MOE =  ifelse(is.na(medgrossrent2004),0,medgrossrent2004) * cpi04,
         significant = stattest(x=sf2004, moex = sf2004MOE, y=Rent,moey=RentMOE))
 


medrentCSV <- medrent %>%
  select(place, sf2004, Rent) %>% 
  pivot_longer(-c("place"), names_to = "medrent", values_to = "Value") %>% 
  mutate(name = paste( place, medrent, sep = "-"),
         year = 2023) %>% 
  select(-place) %>% 
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(medrentCSV, "outputs/spreadsheets/medrent.csv")
#storage_write_csv(medrentCSV, cont_proj, "who_lives/2024/outputs/medrent.csv")


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
         
         orLater1990US = rep(orLater1990pct[4],4),
         mid1950to1989US = rep(mid1950to1989pct[4],4),
         orbefore1949US = rep(orbefore1949pct[4],4),
         
         orLater1990SIG = stattest(x=orLater1990US, y= orLater1990pct, moey = orlater1990moeprop),
         mid1950to1989SIG = stattest(x= mid1950to1989US, y= mid1950to1989pct , moey = mid1950to1989moeprop),
         orbefore1949SIG = stattest(x=orbefore1949US, y= orbefore1949pct, moey = orbefore1949moeprop))


yrbuiltCSV <- yrbuilt %>% 
select(place, (contains("pct"))) %>% 
  pivot_longer(-c("place"), names_to = "yrbuilt", values_to = "Value") %>% 
  pivot_wider(id_cols = c("yrbuilt"), names_from = "place", values_from = "Value")
write.csv(yrbuiltCSV, "outputs/spreadsheets/yrbuilt.csv")
#storage_write_csv(yrbuiltCSV, cont_proj, "who_lives/2024/outputs/yrbuilt.csv")



#Means of transportation to work, 
load("inputs/commuteRaw.RData")
load("inputs/commuteRaw2000.RData")
commute <- commuteRaw %>% left_join(commuteRaw2000, by = "placename") %>%
  filter(placename %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>% 
  mutate(census2000drive= DroveAlone_2000,
         census2000carpool= Carpool_2000,
         census2000publictransit= PublicTransit_2000,
         census2000bike= Bike_2000,
         census2000walk= Walk_2000,
         census2000workhome= Workhome_2000,
         census2000other= Other_2000,
         
         Drivepct = DroveAlone / Total,
         Carpoolpct= Carpool / Total,
         PublicTransitpct = PublicTransit / Total,
         bikepct = Bike / Total,
         Walkpct = Walk / Total,
         Workhomepct = Workhome / Total,
         Otherpct = Other / Total,
         
         Drivepct2000 = DroveAlone_2000 / Total_2000,
         Carpoolpct2000= Carpool_2000 / Total_2000,
         PublicTransitpct2000 = PublicTransit_2000 / Total_2000,
         bikepct2000 = Bike_2000 / Total_2000,
         Walkpct2000 = Walk_2000 / Total_2000,
         Workhomepct2000 = Workhome_2000 / Total_2000,
         Otherpct2000 = Other_2000 / Total_2000,
         
         Drivemoeprop = moeprop(y=Total, moex=DroveAloneMOE, moey=TotalMOE, p=Drivepct),
         carpoolmoeprop = moeprop(y=Total, moex=CarpoolMOE, moey=TotalMOE, p=Carpoolpct),
         PublicTransitmoeprop = moeprop(y=Total, moex=PublicTransitMOE, moey=TotalMOE, p=PublicTransitpct),
         bikemoeprop = moeprop(y=Total, moex=BikeMOE, moey=TotalMOE, p=bikepct),
         Walkmoeprop = moeprop(y=Total, moex=WalkMOE, moey=TotalMOE, p=Walkpct),
         workhomemoeprop = moeprop(y=Total, moex=WorkhomeMOE, moey=TotalMOE, p=Workhomepct),
         othermoeprop = moeprop(y=Total, moex=OtherMOE, moey=TotalMOE, p=Otherpct),
         
         Drivemoeprop2000 = moeprop(y=Total_2000, moex=DroveAlone_2000MOE, moey=Total_2000MOE, p=Drivepct2000),
         carpoolmoeprop2000 = moeprop(y=Total_2000, moex= Carpool_2000MOE, moey=Total_2000MOE, p=Carpoolpct2000),
         PublicTransitmoeprop2000 = moeprop(y=Total_2000, moex= PublicTransit_2000MOE, moey=Total_2000MOE, p=PublicTransitpct2000),
         bikemoeprop2000 = moeprop(y=Total_2000, moex= Bike_2000MOE, moey=Total_2000MOE, p=bikepct2000),
         Walkmoeprop2000 = moeprop(y=Total_2000, moex= Walk_2000MOE, moey=Total_2000MOE, p=Walkpct2000),
         workhomemoeprop2000 = moeprop(y=Total_2000, moex=Workhome_2000MOE, moey=Total_2000MOE, p=Workhomepct2000),
         othermoeprop2000 = moeprop(y=Total_2000, moex=Other_2000MOE, moey=Total_2000MOE, p=Otherpct2000),
         
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
  mutate(year = ifelse(grepl("2000", commute), 2000, 2023),
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
  select(commutefinal, `Orleans-2000`, `Orleans-2023`, `Jefferson-2000`, `Jefferson-2023`, `New Orleans Metro Area-2000`, `New Orleans Metro Area-2023`, `United States-2000`, `United States-2023`  )
write.csv(commuteCSV, "outputs/spreadsheets/commute.csv")
#storage_write_csv(commuteCSV, cont_proj, "who_lives/2024/outputs/commute.csv")



############################################
# # PEP # #
############################################

##############################################

# To analyze, filter to:
# whatever combination of demographics you need: race, age, sex, hisp
# whatever geography you need: place
# whatever year of estimate that you need: date

orderDemo <- c("Orleans Parish", "Jefferson Parish", "Plaquemines Parish", "St. Bernard Parish", "St. Charles Parish",
               "St. James Parish", "St. John the Baptist Parish", "Metro", "United States")
#allparishesRaw <- load("inputs/allparishesRaw.RData")

#Table 1
load("inputs/totpoprace2000.Rdata")

totpopraceRaw_tot <- totpopraceRaw %>% select(placename, Total_2000) %>% na.omit()
totpopraceRaw_blkwt <- totpopraceRaw %>% select(placename, TotBlackAlone_2000, TotWhiteAlone_2000, TotAsianAlone_2000) %>% na.omit()
totpopraceRaw_hisp <- totpopraceRaw %>% select(placename, TotHispanicAny_2000) %>% na.omit()

totpoprace_2000 <- totpopraceRaw_tot %>%
  left_join(totpopraceRaw_blkwt) %>%
  left_join(totpopraceRaw_hisp) %>%
  select(placename, Total_2000, TotBlackAlone_2000, TotWhiteAlone_2000, TotHispanicAny_2000, TotAsianAlone_2000) %>%
  pivot_longer(cols = -placename, names_to = c("var2000", "year"), names_sep = "_", values_to = "val2000") %>%
  mutate(raceSimple = case_when(grepl("Total", var2000) ~ "Total",
                                grepl("Black", var2000) ~ "Black",
                                grepl("White", var2000) ~ "White",
                                grepl("Hisp", var2000) ~ "Hispanic",
                                grepl("Asian", var2000) ~ "Asian")) %>%
  select(-c(year, var2000))

AAWhiteHispan2000 <- totpoprace_2000 %>% filter(placename == "Orleans" & raceSimple != "Total") %>% rename(est2000 = val2000)

load("inputs/allparishesRaw2023.RData")
AAWhiteHispan <- allparishesRaw2023 %>%
  filter(place == "Orleans Parish") %>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(age == "Total" & sex == "Total" & (raceSimple == "Black"|raceSimple == "White" |hisp == "Hispanic" | raceSimple == "Asian" )) %>%
  mutate(race.fac = factor(.$raceSimple,levels = c("Black", "White","Hispanic", "Asian")))%>% arrange(race.fac) %>%
  left_join(AAWhiteHispan2000, by = "raceSimple") %>%
  select(raceSimple, race.fac, population, est2000) 


write.csv(AAWhiteHispan, "outputs/spreadsheets/AAWhiteHispan.csv")
#storage_write_csv(AAWhiteHispan, cont_proj, "who_lives/2024/outputs/AAWhiteHispan.csv")



#Tables 2


#Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro

#HT : I just switched these around so it would make Metro last... The dataframe is not ordered by the levels of placename factor 
#that's the order that the 2000 numbers are going in.
ParishDemo2<- allparishesRaw2023 %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(age == "Total" & sex == "Total") %>%
  group_by(raceSimple)%>% unique() %>%
  summarise(population=sum(population)) %>% mutate(PlaceName = "Metro") %>% select(PlaceName, population, raceSimple)

ParishDemo3 <- allparishesRaw2023 %>% filter(PlaceName == "United States") %>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(age == "Total" & sex == "Total")  %>%
  select(PlaceName, population, raceSimple)

ParishDemo1<- allparishesRaw2023 %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>% arrange((PlaceName)) %>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(age == "Total" & sex == "Total")  %>%
  select(PlaceName, population, raceSimple)  %>%
  bind_rows(.,ParishDemo2, ParishDemo3) 



#reshape data from long to wide for easy analysis

load("inputs/totpoprace2000.Rdata")

totpopraceRaw_tot <- totpopraceRaw %>% select(placename, Total_2000) %>% na.omit()
totpopraceRaw_blkwt <- totpopraceRaw %>% select(placename, TotBlackAlone_2000, TotWhiteAlone_2000, TotAsianAlone_2000) %>% na.omit()
totpopraceRaw_hisp <- totpopraceRaw %>% select(placename, TotHispanicAny_2000) %>% na.omit()

totpoprace_2000 <- totpopraceRaw_tot %>%
  left_join(totpopraceRaw_blkwt) %>%
  left_join(totpopraceRaw_hisp) %>%
  select(placename, Total_2000, TotBlackAlone_2000, TotWhiteAlone_2000, TotHispanicAny_2000, TotAsianAlone_2000) %>%
  pivot_longer(cols = -placename, names_to = c("var2000", "year"), names_sep = "_", values_to = "val2000") %>%
  mutate(raceSimple = case_when(grepl("Total", var2000) ~ "Total",
                                grepl("Black", var2000) ~ "Black",
                                grepl("White", var2000) ~ "White",
                                grepl("Hisp", var2000) ~ "Hispanic",
                                grepl("Asian", var2000) ~ "Asian")) %>%
  select(-c(year, var2000))

ParishDemo <- ParishDemo1  %>%
  mutate(placename2 = case_when(PlaceName == "Metro" ~ "New Orleans Metro Area",
                                T ~ PlaceName)) %>%
  left_join(totpoprace_2000, by = c("placename2" = "placename", "raceSimple")) %>%
  
  
  pivot_wider(names_from = raceSimple, values_from = c(population, val2000)) %>%
  mutate(PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "Metro", "United States"))) %>%
  mutate(pctwhite = as.numeric(population_White)/ as.numeric(population_Total),
         pctblack = as.numeric(population_Black) / as.numeric(population_Total),
         pctasian = as.numeric(population_Asian) / as.numeric(population_Total),
         pcthisp = as.numeric(population_Hispanic) / as.numeric(population_Total),     
         
         white2000 = val2000_White / val2000_Total,
         black2000 = val2000_Black / val2000_Total,
         asian2000 = val2000_Asian / val2000_Total,
         hispanic2000 = val2000_Hispanic / val2000_Total
         #    white2000=c(.266,.645,.688,.844,.705,.497,.51 ,.853,.547,.691),
         #    black2000=c(.667,.227,.233,.076,.251,.492,.446,.098,.373,.121),
         #    asian2000=c(.023,.031,.026,.013,.006,0   ,.005,.008,.021,.037),
         # hispanic2000=c(.031,.071,.016,.051,.028,.006,.029,.025,.044,.125)
         
  ) #%>%
#.[-2,]

orleansdemo_csv <- ParishDemo %>% filter(PlaceName == "Orleans") %>% select(PlaceName, population_Total:population_Hispanic) %>% 
  pivot_longer(cols = -PlaceName, names_to = "race", values_to = "est2023") 
write.csv(orleansdemo_csv, "outputs/spreadsheets/orleansdemo.csv")
#storage_write_csv(orleansdemo_csv, cont_proj, "who_lives/2024/outputs/orleansdemo.csv")

parishdemo_csv <- ParishDemo %>% select(PlaceName, pctwhite:hispanic2000) %>%
  pivot_longer(cols = -PlaceName, names_to = c("race", "year"), names_sep = "2", values_to = "val") %>% 
  mutate(year = case_when(year == "000" ~ "2000",
                          is.na(year) ~ "2023",
                          T ~ year),
         race = factor(case_when(grepl("white", race) ~ "White, non-Hispanic",
                                 grepl("black", race) ~ "Black",
                                 grepl("hisp", race) ~ "Hispanic, any race",
                                 grepl("asian", race) ~ "Asian"), levels = c("White, non-Hispanic", "Black", "Hispanic, any race", "Asian"))) %>% 
  arrange(PlaceName, year) %>%
  pivot_wider(names_from = c(PlaceName, year), values_from = val) %>% 
  arrange(race) 
write.csv(parishdemo_csv, "outputs/spreadsheets/ParishDemo.csv")
#storage_write_csv(parishdemo_csv, cont_proj, "who_lives/2024/outputs/ParishDemo.csv")




#Table 3 Hispanic population change by population

HispanicPop <- allparishesRaw2023 %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>%
  filter(age == "Total" & sex == "Total")  %>%
  filter(raceSimple=="Hispanic")%>%
  select(PlaceName, population) %>%
  left_join(totpopraceRaw_hisp, by = c("PlaceName" = "placename")) %>%
  rename(est2000 = TotHispanicAny_2000)

#Table 5 Hispanic population for parishes in metro by year

HispanicPopyears <- allparishesRaw2023 %>%
  filter(age == "Total" & sex == "Total") %>%
  filter(raceSimple=="Hispanic") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist"))

HispanicPopyears <- HispanicPopyears %>%
  select(PlaceName, date, population) %>%
  pivot_wider(id_cols = date, names_from = PlaceName, values_from = population )
write.csv(HispanicPopyears, "outputs/spreadsheets/HispanicPopyears.csv")
#storage_write_csv(HispanicPopyears, cont_proj, "who_lives/2024/outputs/HispanicPopyears.csv")

load("inputs/hisppopestRaw.RData")
HISPpopM <- hisppopestRaw %>%
  mutate(year = as.numeric(year)) %>%
  filter(place %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>%
  add_row(year = 2000, place= "Orleans", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "Orleans"]) %>%
  add_row(year = 2000, place= "Jefferson", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "Jefferson"]) %>%
  add_row(year = 2000, place= "Plaquemines", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "Plaquemines"]) %>%
  add_row(year = 2000, place= "St. Bernard", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "St. Bernard"]) %>%
  add_row(year = 2000, place= "St. Charles", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "St. Charles"]) %>%
  add_row(year = 2000, place= "St. James", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "St. James"]) %>%
  add_row(year = 2000, place= "St. John the Baptist", POP= totpopraceRaw_hisp$TotHispanicAny_2000[totpopraceRaw_hisp$placename == "St. John the Baptist"]) %>%
  select(place, year, POP) %>% unique()
HISPpopM_CSV <- HISPpopM %>% pivot_wider(names_from = "place", values_from = POP) %>%
  select(year, Orleans, Jefferson, Plaquemines, `St. Bernard`, `St. Charles`, `St. James`, `St. John the Baptist`) %>%
  arrange(year)
write.csv(HISPpopM_CSV, "outputs/spreadsheets/HISPpopM.csv")
#storage_write_csv(HISPpopM_CSV, cont_proj, "who_lives/2024/outputs/HISPpopM_CSV.csv")

load("inputs/medageRaw.RData")


#Table 5 Population by age group

load("inputs/totpopage2000.RData")
age2000 <- totpopage2000Raw %>%
  rename_with(~str_replace(., "_2000", "")) %>%
  select(-contains("MOE")) %>%
  mutate("Under 5 years" = MaleUnder1yr + Male1yr + Male2yr + Male3yr + Male4yr + FemaleUnder1 + Female1yr + Female2yr + Female3yr + Female4yr,
         "5 to 9" =  Male5yr + Male6yr + Male7yr + Male8yr +Male9yr +  Female5yr + Female6yr + Female7yr + Female8yr + Female9yr ,
         "10 to 14" =  Male10yr + Male11yr + Male12yr + Male13yr + Male14yr  +  Female10yr + Female11yr + Female12yr + Female13yr + Female14yr ,
         "15 to 19" =  Male15yr + Male16yr + Male17yr + Male18yr + Male19yr  +  Female15yr + Female16yr + Female17yr + Female18yr + Female19yr ,
         "20 to 24" =  Male20yr + Male21yr + Male22to24  +  Female20yr + Female21yr + Female22to24 ,
         "25 to 29" = Male25to29 + Female25to29,
         "30 to 34" = Male30to34 + Female30to34, 
         "35 to 39" = Male35to39 + Female35to39,
         "40 to 44" = Male40to44 + Female40to44,
         "45 to 49" = Male45to49 + Female45to49,
         "50 to 54" = Male50to54 + Female50to54,
         "55 to 59" = Male55to59 + Female55to59,
         "60 to 64" = Male60to61 + Male62to64 + Female60to61 + Female62to64,
         "65 to 69" = Male65to66 + Male67to69 + Female65to66 + Female67to69,
         "70 to 74" = Male70to74 + Female70to74,
         "75 to 79" = Male75to79 + Female75to79,
         "80 to 84" = Male80to84 + Female80to84,
         "85 plus" = MaleOver85 + FemaleOver85,
         
         PlaceName = case_when(place == "051" ~ "Jefferson",
                                          place == "071" ~ "Orleans",
                                          place == "075" ~ "Plaquemines",
                                          place == "087" ~ "St. Bernard",
                                          place == "089" ~ "St. Charles",
                                          place == "093" ~ "St. James",
                                          place == "095" ~ "St. John the Baptist",
                                          place == "MSA_2023" ~ "New Orleans Metro Area",
                                          place == "1" ~ "United States")) %>%
  select(PlaceName, `Under 5 years`:`85 plus`) %>% pivot_longer(cols = -PlaceName, names_to = "age", values_to = "est2000")

orderAge <- c(rep("Jefferson",18),rep("Orleans", 18),rep("Plaquemines",18),
              rep("St. Bernard", 18),rep("St. Charles", 18),rep("St. James", 18),
              rep("St. John The Baptist", 18))
Agepop <- allparishesRaw2023 %>%
  filter(age== "Under 5 years" | age== "5 to 9"| age== "10 to 14" | age== "15 to 19"|
           age=="20 to 24"| age== "25 to 29"| age== "30 to 34"| age== "35 to 39"| age== "40 to 44"
         | age== "45 to 49" | age=="50 to 54"| age== "55 to 59"| age== "60 to 64"| age== "65 to 69"|
           age== "70 to 74"| age== "75 to 79"| age== "80 to 84"| age== "85 plus")%>%
  filter(raceSimple=="Total")%>%
  filter(sex=="Total")%>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>%
  arrange(factor(PlaceName, levels = c("Jefferson","Orleans","Plaquemines",
                 "St. Bernard","St. Charles","St. James",
                 "St. John the Baptist"))) %>%
  select(age, PlaceName, population) %>%
  left_join(age2000, by = c("age", "PlaceName"))
  
 Agepop_csv <-  Agepop %>%
   mutate(est22 = population,
          est00 = est2000) %>%
   pivot_wider(id_cols = age, names_from = PlaceName, values_from = c(est00, est22)) %>% select(est00_Orleans, est22_Orleans,
                                                                                                est00_Jefferson, est22_Jefferson,
                                                                                                est00_Plaquemines, est22_Plaquemines,
                                                                                                `est00_St. Bernard`, `est22_St. Bernard`,
                                                                                                `est00_St. Charles`, `est22_St. Charles`,
                                                                                                `est00_St. James`, `est22_St. James`,
                                                                                                `est00_St. John the Baptist`, `est22_St. John the Baptist`)
write.csv(Agepop_csv, "outputs/spreadsheets/Agepop.csv")
#storage_write_csv(Agepop_csv, cont_proj, "who_lives/2024/outputs/Agepop.csv")


#Table 6 Under 18 population
#Different than estimates from google sheets but aligns with American fact finder
popunder18_2000 <- totpopage2000Raw %>%
  rename_with(~str_replace(., "_2000", "")) %>%
  select(-contains("MOE")) %>%
  mutate(est2000 = MaleUnder1yr + Male1yr + Male2yr + Male3yr + Male4yr + FemaleUnder1 + Female1yr + Female2yr + Female3yr + Female4yr +
           Male5yr + Male6yr + Male7yr + Male8yr +Male9yr +  Female5yr + Female6yr + Female7yr + Female8yr + Female9yr  +
           Male10yr + Male11yr + Male12yr + Male13yr + Male14yr  +  Female10yr + Female11yr + Female12yr + Female13yr + Female14yr +
           Male15yr + Male16yr + Male17yr + Female15yr + Female16yr + Female17yr,
         
         PlaceName = case_when(place == "051" ~ "Jefferson",
                               place == "071" ~ "Orleans",
                               place == "075" ~ "Plaquemines",
                               place == "087" ~ "St. Bernard",
                               place == "089" ~ "St. Charles",
                               place == "093" ~ "St. James",
                               place == "095" ~ "St. John the Baptist",
                               place == "MSA_2023" ~ "Metro",
                               place == "1" ~ "United States")) %>%
  select(PlaceName, est2000)

under18pars<-allparishesRaw2023 %>%
  filter(age=="18 years and over" | age=="Total")%>%
  filter(raceSimple=="Total")%>%
  filter(sex=="Total")%>%
  filter(date == "7/1/2023 population estimate") %>%
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist")) %>%
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
  filter(PlaceName == "Orleans" | PlaceName =="Jefferson" | PlaceName == "Metro")%>%
  select(PlaceName, under18) %>%
  left_join(popunder18_2000, by = c("PlaceName"))

popunder18CSV <- popunder18 %>%
  select(PlaceName, est2000, under18) %>%
  pivot_longer(-c("PlaceName"), names_to = "under18", values_to = "Value") %>%
  mutate(name = paste(PlaceName, under18, sep = "-"),
         year = 2023) %>%
  select(-PlaceName) %>%
pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  select(`Orleans-est2000`, `Orleans-under18`, `Jefferson-est2000`, `Jefferson-under18`, `Metro-est2000`, `Metro-under18`) 
write.csv(popunder18CSV, "outputs/spreadsheets/under18.csv")
#storage_write_csv(popunder18CSV, cont_proj, "who_lives/2024/outputs/under18.csv")


###########################
# Jenna's analysis expanded
###########################

childPovProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/childPov.csv")
homeownershipProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/homeownership.csv")
educationalAttainmentProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment.csv")
medHHincProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")

### Median household income ###
###
load("inputs/medhhRaw_exp.RData")
medhh_exp <- medhhRaw_exp  %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%
  select(-place, -placenames, -placename, -contains("MOE")) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val")


### Across geos median hh income bar chart ###
medhh.totals <- medhh_exp %>%
  filter(var == "MedianHHIncome")%>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

medhh.race <- medhh_exp %>%
  filter(var != "MedianHHIncome") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))


### Historical median hh income line chart ###

## median hh income adjusted to 2022 dollars ###

#using https://data.bls.gov/cgi-bin/cpicalc.pl from january to january
#1979 to 2022 = 3.83
#1989 to 2022 = 2.16
#1999 to 2022 = 1.59
#2010 to 2022 = 1.21
#2016 to 2022 = 1.10
load("inputs/medhh_unadjusted.RData")
medhhinc_adjusted21 <- medhh_unadjusted %>% mutate(value = as.numeric(value),
                                                   inc_adj23 = case_when(Year == 1979 ~ value * cpi79,
                                                                         Year == 1989 ~ value * cpi89,
                                                                         Year == 1999 ~ value * cpi99,
                                                                         Year == 2010 ~ value * cpi10,
                                                                         Year == 2016 ~ value * cpi16))
# write_csv(medhhinc_adjusted21, "inputs/medHHinc_exp.csv")

medhh.hist <- medhhinc_adjusted21 %>% 
  select(-value) %>%
  rename(val = inc_adj23) %>%
  mutate(Year = as.numeric(Year),
         var = ifelse(var == "Overall", "All", var),
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Hispanic,",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  bind_rows(medhh_exp %>%
              filter(place.fac == "Orleans",
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2023, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "MedianHHIncome", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))


### stat testing ###
medhh.race_stattest <- medhhRaw_exp %>%
  mutate(sig_wht_blk = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_blk, moey = MedianHHIncomeMOE_blk),
         sig_wht_hisp = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_wht_asian = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sigall_wht = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sig_blk_hisp = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_blk_asian = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sigall_blk = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sig_hisp_asian = stattest(x=MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sigall_hisp = stattest(x=MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sigall_asian = stattest(x=MedianHHIncome_asian, moex = MedianHHIncomeMOE_asian, y=MedianHHIncome, moey = MedianHHIncomeMOE)
  ) 

medhh_stat_all <- medhh.race_stattest %>% select(place, placename, (contains("sigall")), (contains("MedianHHIncome") & !contains("MOE") & !contains("asian"))) %>%
  pivot_longer(cols = c(-place, -placename, -contains("sig")), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>% group_by(place, placename) %>%
  mutate(race = case_when(race == "MedianHHIncome" ~ "All",
                          grepl("asian",race) & grepl("asian",var) ~ "Asian",
                          grepl("blk",race) & grepl("blk",var) ~ "Black",
                          grepl("hisp",race) & grepl("hisp",var) ~ "Hispanic, any race",
                          grepl("wht", race) & grepl("wht",var) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit()
medhh_stat_all$stat_all[medhh_stat_all$race == "All"] <- "yes"
medhh_stat_all <- medhh_stat_all %>%  group_by(place, placename, race) %>%
  mutate(val_lab = paste0("$", comma(val))) %>% select(-var, -placename) %>% unique()


medhh_stat_race <- medhh.race_stattest %>% select(place, placename, (contains("sig") & !contains("all") & !contains("asian"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% unique()
medhh_with_stats <- medhh_stat_all %>% left_join(medhh_stat_race, by = "place") %>% unique() %>% filter(race != "All") %>%
  mutate(placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson", "Metro", "United States")),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")))


### Across geos pov bar chart ###
medhh.totals <- medhh_stat_all %>% left_join(medhh_stat_race, by = "place") %>% unique() %>%
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")),
         placename.fac = factor(placename.y, levels = c("Orleans ◊", "Jefferson ◊", "Metro ◊", "United States")))

medhh.totals_CSV <- medhh.totals %>% ungroup() %>% select(placename.x, race, val) %>% 
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val) 

medhh.race_CSV <- medhh.race %>% select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>% rbind(medhh.totals_CSV)
write.csv(medhh.race_CSV, "outputs/spreadsheets/medhhrace.csv")
#storage_write_csv(medhh.race_CSV, cont_proj, "who_lives/2024/outputs/medhhrace.csv")

medhh.hist_stattest.EST <- medhhRaw_exp %>% 
  filter(place == "071") %>%
  select(-place,-placename,-contains("MOE")) %>%
  pivot_longer(everything(),names_to = "var",values_to = "val2023") %>%
  filter(var != "MedianHHIncome_asian") %>%
  mutate(race = case_when(var =="MedianHHIncome" ~ "Overall",
                          var =="MedianHHIncome_blk" ~ "Black",
                          var =="MedianHHIncome_wht" ~ "White,\nnon-Hispanic",
                          var =="MedianHHIncome_hisp" ~ "Hispanic,\nany race"
  )) %>%
  select(race, val2023)

medhh.hist_stattest.MOE <- medhhRaw_exp %>% 
  filter(place == "071")%>%
  select(contains("MOE")) %>%
  pivot_longer(everything(),names_to = "var",values_to = "moe2023")%>%
  filter(var != "MedianHHIncomeMOE_asian") %>%
  mutate(race = case_when(var =="MedianHHIncomeMOE" ~ "Overall",
                          var =="MedianHHIncomeMOE_blk" ~ "Black",
                          var =="MedianHHIncomeMOE_wht" ~ "White,\nnon-Hispanic",
                          var =="MedianHHIncomeMOE_hisp" ~ "Hispanic,\nany race"
  )) %>%
  select(race, moe2023)

medhh_unadjusted <- read_xlsx("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute("var" = `...1`,
            `1979` = as.character(`1979 (1979$)`),
            `1989`= as.character(`1989 (1989$)`),
            `1999` = as.character(`1999 (1999$)`),
            `2010` = `2010 (2010$)...5`,
            `2016` = `2016 (2016$)...7`,
            `2010MOE` = `2010 (2010$)...6`,
            `2016MOE` = `2016 (2016$)...8`,
            var = case_when(var == "White, Alone" ~ "White,\nnon-Hispanic",
                            var == "Hispanic, Any Race" ~ "Hispanic,\nany race",
                            T ~ var)) %>% na.omit()

meddhh.hist_withmoe <- medhh_unadjusted %>% 
  select(var,`1999`,`2010`,`2010MOE`) %>%
  mutate(adj1999 = as.numeric(`1999`) * cpi99,adj2010 = as.numeric(`2010`) * cpi10, adj2010moe = as.numeric(`2010MOE`) * cpi10) %>%
  mutate(adj1999moe = moe2000(adj1999, 215091, designfac = 1.2))

medhh.hist_stattest <- left_join(medhh.hist_stattest.EST,medhh.hist_stattest.MOE) %>%
  left_join(meddhh.hist_withmoe, by = c("race" = "var")) %>%
  mutate(sig_99_10= stattest(x=adj1999,moex = adj1999moe, y=adj2010,moey = adj2010moe),
         sig_99_21= stattest(x=adj1999,moex = adj1999moe, y=val2023,moey = moe2023),
         sig_10_21= stattest(x=adj2010,moex = adj2010moe, y=val2023,moey = moe2023)) %>%
  select(race, contains("sig"))

medhh.hist <- medhh.hist %>% left_join(medhh.hist_stattest, by = c("var" = "race")) %>% filter(var != "All") %>%
  mutate(val_lab = case_when((sig_99_10 == "no" | sig_99_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " "))
medhh.hist_csv <- medhh.hist %>% select(Year, race = var, val) %>% filter(Year != 2016) %>% pivot_wider(names_from = Year, values_from = val) 
write.csv(medhh.hist_csv, "outputs/spreadsheets/medhhhist.csv")
#storage_write_csv(medhh.hist_csv, cont_proj, "who_lives/2024/outputs/medhhhist.csv")



### Employment Status

#Employment by Sex and Race
load("inputs/employmentRaw.RData")

employ <- employmentRaw %>%
  mutate(WhtMale_pct = WhtMaleEmp / WhtMaleTot,
         WhtMale_pctMOE = moeprop(WhtMaleTot, WhtMaleEmpMOE, WhtMaleTotMOE, WhtMale_pct),
         
         WhtFemale_pct = WhtFemaleEmp / WhtFemaleTot,
         WhtFemale_pctMOE = moeprop(WhtFemaleTot, WhtFemaleEmpMOE, WhtFemaleTotMOE, WhtFemale_pct),
         
         BlackMale_pct = BlkMaleEmp / BlkMaleTot,
         BlackMale_pctMOE = moeprop(BlkMaleTot, BlkMaleEmpMOE, BlkMaleTotMOE, BlackMale_pct),
         
         BlackFemale_pct = BlkFemaleEmp / BlkFemaleTot,
         BlackFemale_pctMOE = moeprop(BlkFemaleTot, BlkFemaleEmpMOE, BlkFemaleTotMOE, BlackFemale_pct),
         
         HispMale_pct = HispMaleEmp / HispMaleTot, 
         HispMale_pctMOE = moeprop(HispMaleTot, HispMaleEmpMOE, HispMaleTotMOE, HispMale_pct),
         
         HispFemale_pct = HispFemaleEmp / HispFemaleTot,
         HispFemale_pctMOE = moeprop(HispFemaleTot, HispFemaleEmpMOE, HispFemaleTotMOE, HispFemale_pct)) %>%
  select(c(place, placename, WhtMale_pct:HispFemale_pctMOE))

  
employ_stattest.data <- employ %>%
  mutate(sig_wht_blkM = stattest(x = WhtMale_pct, moex = WhtMale_pctMOE, y = BlackMale_pct, moey = BlackMale_pctMOE),
         sig_wht_hispM = stattest(x = WhtMale_pct, moex = WhtMale_pctMOE, y = HispMale_pct, moey = HispMale_pctMOE),
         sig_blk_hispM = stattest(x = BlackMale_pct, moex = BlackMale_pctMOE, y = HispMale_pct, moey = HispMale_pctMOE),
         
         sig_wht_blkF = stattest(x = WhtFemale_pct, moex = WhtFemale_pctMOE, y = BlackFemale_pct, moey = BlackFemale_pctMOE),
         sig_wht_hispF = stattest(x = WhtFemale_pct, moex = WhtFemale_pctMOE, y = HispFemale_pct, moey = HispFemale_pctMOE),
         sig_blk_hispF = stattest(x = BlackFemale_pct, moex = BlackFemale_pctMOE, y = HispFemale_pct, moey = HispFemale_pctMOE),
         
         # sig_whtM_F = stattest(x = WhtMale_pct, moex = WhtMale_pctMOE, y = WhtFemale_pct, moey = WhtFemale_pctMOE),
         # sig_blkM_F = stattest(x = BlackMale_pct, moex = BlackMale_pctMOE, y = BlackFemale_pct, moey = BlackFemale_pctMOE),
         # sig_hispM_F = stattest(x = HispMale_pct, moex = HispMale_pctMOE, y = HispFemale_pct, moey = HispFemale_pctMOE)
         ) %>%
  select(place, placename, contains("sig"), contains("pct")) %>% 
  rename_with(~str_replace(., "Male", "_Male")) %>% 
  rename_with(~str_replace(., "Female", "_Female"))  %>%
  rename_with(~str_replace(., "pctMOE", "MOE"))  %>%
pivot_longer(cols = contains(c("pct", "MOE")), names_to = c("race", "sex", "var"), names_sep = "_", values_to = "val") %>%
pivot_longer(cols = contains("sig"), names_to = "sig", values_to = "stat") %>% group_by(place, placename) %>%
  filter(var != "MOE") %>%
mutate(race = case_when(grepl("Black",race) & grepl("blk",sig) ~ "Black",
                          grepl("Hisp",race) & grepl("hisp",sig) ~ "Hispanic, any race",
                          grepl("Wht", race) & grepl("wht",sig) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit() %>%  group_by(place, placename, race, sex) %>%
  mutate(val_lab = ifelse(val == 0, "", paste0(round.off(val*100), "%"))) %>% select(-var, -placename) %>% unique() %>%
  ungroup() %>%
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat ~ paste0(placename, "*"),
                               T ~ placename)) %>%
  unite("grp", c(race, sex), sep = " ")
  
employ_with_stats <- employ_stattest.data %>% select(placename, grp, val, val_lab) %>% unique() %>%
  mutate(placename.fac = fct_rev(factor(placename, levels = c("Orleans*", "Jefferson*", "Metro*", "United States"))),
         grp.fac = fct_rev(factor(grp, levels = c("Black Male", "Black Female", "White, non-Hispanic Male", "White, non-Hispanic Female", "Hispanic, any race Male", "Hispanic, any race Female"))))


## employment timeseries
load("inputs/employ10Raw.RData")

employ_10_current <- employmentRaw %>%
  mutate(pctWhiteMaleEmploy = WhtMaleEmp / WhtMaleTot,
         pctWhiteMaleEmployMOE = moeprop(WhtMaleTot, WhtMaleEmpMOE, WhtMaleTotMOE, pctWhiteMaleEmploy),
         
         pctWhiteFemaleEmploy = WhtFemaleEmp / WhtFemaleTot,
         pctWhiteFemaleEmployMOE = moeprop(WhtFemaleTot, WhtFemaleEmpMOE, WhtFemaleTotMOE, pctWhiteFemaleEmploy),
         
         pctBlackMaleEmploy = BlkMaleEmp / BlkMaleTot,
         pctBlackMaleEmployMOE = moeprop(BlkMaleTot, BlkMaleEmpMOE, BlkMaleTotMOE, pctBlackMaleEmploy),
         
         pctBlackFemaleEmploy  = BlkFemaleEmp / BlkFemaleTot,
         pctBlackFemaleEmployMOE = moeprop(BlkFemaleTot, BlkFemaleEmpMOE, BlkFemaleTotMOE, pctBlackFemaleEmploy),
         
         pctHispMaleEmploy = HispMaleEmp / HispMaleTot, 
         pctHispMaleEmployMOE = moeprop(HispMaleTot, HispMaleEmpMOE, HispMaleTotMOE, pctHispMaleEmploy),
         
         pctHispFemaleEmploy  = HispFemaleEmp / HispFemaleTot,
         pctHispFemaleEmployMOE = moeprop(HispFemaleTot, HispFemaleEmpMOE, HispFemaleTotMOE, pctHispFemaleEmploy)
  ) %>%
  mutate(year = year) %>%
  filter(place == "071") %>%
  select(c(year, pctWhiteMaleEmploy:pctHispFemaleEmployMOE)) %>%
  pivot_longer(cols = pctWhiteMaleEmploy:pctHispFemaleEmployMOE, values_to = "val") %>% select(year, val, name) %>%
  rbind(employ10) %>%
  mutate(name = str_replace(name,"MOE","_MOE")) %>%  
  separate(name,c("name","type"), sep = "_") %>%
  mutate(type = case_when(is.na(type) ~ "est", 
                          type == "MOE" ~ "MOE")) %>%
  pivot_wider(names_from = c("type","year"), values_from = "val") %>% 
  mutate(racesex_lab = factor(case_when(
    grepl("BlackMale",name) ~ "Black Male",
    grepl("BlackFemale",name) ~ "Black Female",
    grepl("HispMale",name)  ~ "Hispanic, any race Male",
    grepl("HispFemale",name)  ~ "Hispanic, any race Female",
    grepl("WhiteMale", name) ~ "White, non-Hispanic Male",
    grepl("WhiteFemale", name) ~ "White, non-Hispanic Female"
  )),
  name = factor(case_when(
    grepl("BlackMale",name) ~ "Black_Male",
    grepl("BlackFemale",name) ~ "Black_Female",
    grepl("HispMale",name)  ~ "Hispanic, any race_Male",
    grepl("HispFemale",name)  ~ "Hispanic, any race_Female",
    grepl("WhiteMale", name) ~ "White, non-Hispanic_Male",
    grepl("WhiteFemale", name) ~ "White, non-Hispanic_Female"
  )))


employ80to00<- read_csv("inputs/indicator expansion drafts/employment/Employment_Rates_20240919.csv")

employ_80_00 <- employ80to00 %>%
  mutate(est = pct_employed,
        Race = case_when(
          grepl("Black",Race) ~ "Black",
          grepl("White",Race) ~ "White, non-Hispanic",
          grepl("Hisp",Race) ~ "Hispanic, any race"
          )) %>%
  unite("racesex_lab",c(Race,Gender),sep = " ") %>%
  select(racesex_lab, est, MOE, Year)  %>%
  pivot_longer(cols = c(MOE, est), names_to = "type", values_to = "val")%>%
  pivot_wider(names_from = c("type","Year"), values_from = "val")

employment_TS <-left_join(employ_80_00, employ_10_current, by  = "racesex_lab")



# Stat Test

employ_stattest.hist <- employment_TS %>%
  mutate(sig_00_10 = stattest(x = est_2000, moex = MOE_2000, y = est_2010, moey = MOE_2010),
         sig_00_current = stattest(x = est_2000, moex = MOE_2000, y = est_2023, moey = MOE_2023),
         sig_10_current = stattest(x = est_2010, moex = MOE_2010, y = est_2023, moey = MOE_2023)) %>% 
  pivot_longer(cols = -c(name, racesex_lab, sig_00_10, sig_00_current, sig_10_current), names_to = c("type", "year"), names_sep = "_", values_to = "val") %>% 
  filter(type != "MOE") %>%
  select(year, val, name, racesex_lab, sig_00_10, sig_00_current, sig_10_current)  %>%
  mutate(val_lab = case_when((sig_00_10 == "no" | sig_00_current == "no" | sig_10_current == "no") ~ "*",
                             T ~ " ")) %>%
  separate(name, c("race", "sex"), sep = "_") %>%
  filter(val != 0) %>%
  mutate(racesex_lab = factor(racesex_lab,levels = c("White, non-Hispanic Male", "White, non-Hispanic Female", "Hispanic, any race Male", "Hispanic, any race Female","Black Male", "Black Female")))



### Educational attainment ###
###
load("inputs/bachRaw_exp.RData")
bach_exp <- bachRaw_exp %>%
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
         # moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

bach.race_stattest.data <-  bachRaw_exp %>%
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
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))
bach.race_stattest <- bach.race_stattest.data%>%
  mutate(sig_wht_blk = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_asian, moey = moeprop_asian),
         sigall_wht = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach, moey = moeprop),
         sig_blk_hisp = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_asian, moey = moeprop_asian),
         sigall_blk = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach, moey = moeprop),
         sig_hisp_asian = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach_asian, moey = moeprop_asian),
         sigall_hisp = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach, moey = moeprop),
         sigall_asian = stattest(x=pctbach_asian, moex = moeprop_asian, y=pctbach, moey = moeprop)
  )

bach_stat_all <- bach.race_stattest %>% select(place, placename, (contains("sig") & contains("all")), contains("pct")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>% group_by(place, placename) %>%
  mutate(race = case_when(race == "pctbach" ~ "All",
                          grepl("asian",race) & grepl("asian",var) ~ "Asian",
                          grepl("blk",race) & grepl("blk",var) ~ "Black",
                          grepl("hisp",race) & grepl("hisp",var) ~ "Hispanic, any race",
                          grepl("wht", race) & grepl("wht",var) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit()
bach_stat_all$stat_all[bach_stat_all$race == "All"] <- "yes"
bach_stat_all <- bach_stat_all %>%  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val*100), "%")) %>% select(-var, -placename) %>% unique()

bach_stat_race <- bach.race_stattest %>%
  select(place, placename, (contains("sig") & !contains("all")), -contains("asian")) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% unique()
bach_with_stats <- bach_stat_all %>% left_join(bach_stat_race, by = "place") %>% unique() %>% filter(race != "All" & race != "Asian") %>%
  mutate(placename.fac = factor(placename.y, levels = c("Orleans", "Jefferson*", "Metro*", "United States")),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Hispanic, any race")))


bach.totals <- bach_stat_all %>% left_join(bach_stat_race, by = "place") %>% unique() %>%
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")),
         placename.fac = factor(placename.y, levels = c("Orleans", "Jefferson*", "Metro", "United States")))

bach.race <- bach_exp %>%
  filter(var!="pctbach") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Hispanic,\nany race")))

bach.totals_CSV <- bach.totals %>% ungroup() %>% select(placename.x, race, val) %>% 
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val) 

bach.race_CSV <- bach.race %>% select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>% rbind(bach.totals_CSV)
write.csv(bach.race_CSV, "outputs/spreadsheets/bachrace.csv")
#storage_write_csv(bach.race_CSV, cont_proj, "who_lives/2024/outputs/bachrace.csv")


# Historical Educational Attainment Line Chart

EduAtt <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment_byrace.csv")
EduAtt.hist <- EduAtt %>% 
  mutate(var = ifelse(var == "White,\r\nnon-Hispanic", "White,\nnon-Hispanic", ifelse(var == "Hispanic,\r\nany race", "Hispanic,\nany race", var))) %>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race"))) %>%
  bind_rows(bach.race %>%
              filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2023))

bachvars10 <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
                'C15002_017E','C15002_017M',
                'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
                'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
                'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
                'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames10 <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
                 "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
                 "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
                 "FemaleBachMOE_blk",
                 "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
                 "FemaleBachMOE_asian", 
                 "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
                 "FemaleBachMOE_wht", 
                 "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
                 "FemaleBachMOE_hisp")

Bach10_raw <- wholivesdatapull(bachvars10, bachnames10, year = 2010)

Bach10MOE <- Bach10_raw %>%
  filter(place == "071") %>% 
  mutate(year = 2010,
         pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
         pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
         pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
         pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp,
         
         TotalBachmoeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
         Whitemoeagg = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
         Blackmoeagg = moeagg(cbind(MaleBachMOE_blk , FemaleBachMOE_blk)),
         Hispmoeagg = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
         
         
         Totalmoeprop = moeprop(y=Total, moex = TotalBachmoeagg, moey = TotalMOE, p = pctTotalBach),
         Whitemoeprop = moeprop(y=Total_wht, moex = Whitemoeagg, moey = TotalMOE_wht, p = pctWhiteBach),
         Blackmoeprop = moeprop(y=Total_blk, moex = Blackmoeagg, moey = TotalMOE_blk, p = pctBlackBach),
         Hispmoeprop = moeprop(y=Total_hisp, moex = Hispmoeagg, moey = TotalMOE_hisp, p = pctHispBach)
  ) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2010") %>%
  mutate(race = case_when(var =="Totalmoeprop" ~ "All",
                          var =="Blackmoeprop" ~ "Black",
                          var =="Whitemoeprop" ~ "White,\nnon-Hispanic",
                          var =="Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)

load("inputs/Bach00Wht.RData")
load("inputs/Bach00Raw.RData")

Bach00Raw <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

Bach00MOE <- Bach00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, n = 484674, designfac = 1.2),
            totpopMOE = moe2000(est = totpop, n = 484674, designfac = 1.2),
            
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, n = 484674, designfac = 1.2),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, n = 484674, designfac = 1.2),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, n = 484674, designfac = 1.2),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, n = 484674, designfac = 1.2),
            
            pctTotalBach = totBach / totpop,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            BlackBachmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            HispBachmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
  ) %>% cbind(Bach00Wht) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2000") %>%
  mutate(race = case_when(var =="Totalmoeprop" ~ "All",
                          var =="BlackBachmoeprop" ~ "Black",
                          var =="WhiteBachmoeprop" ~ "White,\nnon-Hispanic",
                          var =="HispBachmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)

Bach00MOE <- Bach00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, n = 484674, designfac = 1.2),
            totpopMOE = moe2000(est = totpop, n = 484674, designfac = 1.2),
            
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, n = 484674, designfac = 1.2),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, n = 484674, designfac = 1.2),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, n = 484674, designfac = 1.2),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, n = 484674, designfac = 1.2),
            
            pctTotalBach = totBach / totpop,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            BlackBachmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            HispBachmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
  ) %>% cbind(Bach00Wht) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2000") %>%
  mutate(race = case_when(var =="Totalmoeprop" ~ "All",
                          var =="BlackBachmoeprop" ~ "Black",
                          var =="WhiteBachmoeprop" ~ "White,\nnon-Hispanic",
                          var =="HispBachmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)

bach.hist_stattest <- left_join(Bach10MOE, Bach00MOE) %>%
  left_join(EduAtt.hist %>% filter(year == 2000) %>% transmute(est2000 = val, race = var)) %>%
  left_join(EduAtt.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "moe2023") %>%
              mutate(race = case_when(var =="moeprop" ~ "All",
                                      var =="moeprop_blk" ~ "Black",
                                      var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                      var =="moeprop_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(pctbach,pctbach_blk ,pctbach_wht ,pctbach_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "est2023") %>%
              mutate(race = case_when(var =="pctbach" ~ "All",
                                      var =="pctbach_blk" ~ "Black",
                                      var =="pctbach_wht" ~ "White,\nnon-Hispanic",
                                      var =="pctbach_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2023, moe2023),
         sig_10_21 = stattest(est2010, moe2010, est2023, moe2023)
  ) %>%
  select(race, contains("sig"))
EduAtt.hist <- EduAtt.hist %>% left_join(bach.hist_stattest, by = c("var" = "race")) %>%
  mutate(val_lab = case_when((sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " "))
EduAtt.hist_csv <- EduAtt.hist %>% select(year, race = var, val) %>% filter(year != 2016) %>% pivot_wider(names_from = year, values_from = val) 
write.csv(EduAtt.hist_csv, "outputs/spreadsheets/EduAtthist.csv")
#storage_write_csv(EduAtt.hist_csv, cont_proj, "who_lives/2024/outputs/EduAtthist.csv")

### stat test ### 

bach.race_stattest.data <-  bachRaw_exp %>%
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
         # moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))
bach.race_stattest.race <- bach.race_stattest.data%>%
  mutate(sig_wht_blk = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach_blk, moey = moeprop_blk))
bach.race_stattest.geoEST <- bach.race_stattest.data %>%
  select(place,pctbach_blk, pctbach_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "val")
bach.race_stattest.geoMOE <- bach.race_stattest.data %>%
  select(place,moeprop_blk, moeprop_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "moe")
bach.race_stattest.geo <- left_join(bach.race_stattest.geoEST,bach.race_stattest.geoMOE, by = "place") %>%
  pivot_wider(names_from = place,values_from = c(val, moe)) %>%
  mutate(sig_no_us = stattest(x=val_071, moex = moe_071, y = val_1, moey = moe_1),
         sig_no_metro =stattest(x=val_071, moex = moe_071, y = val_35380, moey = moe_35380),
       sig_no_jeff = stattest(x=val_071, moex = moe_071, y = val_051, moey = moe_051))


# Poverty

load("inputs/povRaw_exp.RData")
pov_exp <- povRaw_exp %>%
  mutate(
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = BelowPov_asian / Total_asian,
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    # moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

pov_stattest.data <- povRaw_exp %>%
  replace(is.na(.),0) %>%
  transmute(place = place,
            placename = placename,
            pctpov = BelowPov / Total,
            pctpov_blk = BelowPov_blk / Total_blk,
            pctpov_wht = BelowPov_wht / Total_wht,
            pctpov_hisp = BelowPov_hisp / Total_hisp,
            pctpov_asian = ifelse(!is.nan(BelowPov_asian / Total_asian),BelowPov_asian / Total_asian,0),
            moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
            moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
            moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
            moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
            moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  )

pov_stattest <- povRaw_exp %>%
  replace(is.na(.),0) %>%
  transmute(place = place, 
            placename = placename,
            pctpov = BelowPov / Total,
            pctpov_blk = BelowPov_blk / Total_blk,
            pctpov_wht = BelowPov_wht / Total_wht,
            pctpov_hisp = BelowPov_hisp / Total_hisp,
            pctpov_asian = ifelse(!is.nan(BelowPov_asian / Total_asian),BelowPov_asian / Total_asian,0),
            moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
            moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
            moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
            moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
            moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  ) %>%
  mutate(sig_wht_blk = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_asian, moey = moeprop_asian),
         sigall_wht = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov, moey = moeprop),
         sig_blk_hisp = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_asian, moey = moeprop_asian),
         sigall_blk = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov, moey = moeprop),
         sig_hisp_asian = stattest(x=pctpov_hisp, moex = moeprop_hisp, y=pctpov_asian, moey = moeprop_asian),
         sigall_hisp = stattest(x=pctpov_hisp, moex = moeprop_hisp, y=pctpov, moey = moeprop),
         sigall_asian = stattest(x=pctpov_asian, moex = moeprop_asian, y=pctpov, moey = moeprop)
  )

pov_stat_all <- pov_stattest %>% select(place, placename, (contains("sig") & contains("all")), contains("pct"), !contains("asian")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>% group_by(place, placename) %>%
  mutate(race = case_when(race == "pctpov" ~ "All",
                          grepl("asian",race) & grepl("asian",var) ~ "Asian",
                          grepl("blk",race) & grepl("blk",var) ~ "Black",
                          grepl("hisp",race) & grepl("hisp",var) ~ "Hispanic, any race",
                          grepl("wht", race) & grepl("wht",var) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit()
pov_stat_all$stat_all[pov_stat_all$race == "All"] <- "yes"
pov_stat_all <- pov_stat_all %>%  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val*100), "%")) %>% select(-var, -placename) %>% unique()
  

pov_stat_race <- pov_stattest %>% select(place, placename, (contains("sig") & !contains("all") & !contains("asian"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% unique()
pov_with_stats <- pov_stat_all %>% left_join(pov_stat_race, by = "place") %>% unique() %>% filter(race != "All") %>%
  mutate(placename.fac = factor(placename.y, levels = c("Orleans", "Jefferson*", "Metro*", "United States")),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")))


### Across geos pov bar chart ###
pov.totals <- pov_stat_all %>% left_join(pov_stat_race, by = "place") %>% unique() %>% 
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")),
         placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro*", "United States")))

pov.race <- pov_exp %>%
  filter(var != "pctpov") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

pov.totals_CSV <- pov.totals %>% ungroup() %>% select(placename.x, race, val) %>% 
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val) 

pov.race_CSV <- pov.race %>% select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>% rbind(pov.totals_CSV)
write.csv(pov.race_CSV, "outputs/spreadsheets/povrace.csv")
#storage_write_csv(pov.race_CSV, cont_proj, "who_lives/2024/outputs/povrace.csv")


totalPov <- read_csv("inputs/hist_pov.csv")
totalPov.hist <- totalPov %>% 
  mutate(year = ifelse(year != 2010, year - 1, year)) %>%
  mutate(var = case_when(var == "White,\r\nnon-Hispanic" ~ "White,\nnon-Hispanic",
                         var == "Hispanic,\r\nany race" ~ "Hispanic,\nany race",
                         T ~  var)) %>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))%>%
  bind_rows(pov.race %>%
              filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2023))

load("inputs/pov00Wht.RData")
pov00Raw <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")
pov00MOE <- pov00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
                                                                                 totpov = GTV001 + GTV003 + GTV005 + GTV007 + GTV009 + GTV011 + GTV013,
                                                                                 totpop = sum(c_across(GTV001:GTV014),na.rm = T),
                                                                                 totpovMOE = moe2000(est = totpov, n = totpop, designfac = 1.3),
                                                                                 totpopMOE = moe2000(est = totpop, n = totpop, designfac = 1.3),
                                                                                 #Whitepov = GTV001,
                                                                                 #totWhitepop = GTV001 + GTV002,
                                                                                 #WhitepovMOE = moe2000(est = Whitepov, n = totpop, designfac = 1.3),
                                                                                 #totWhitepopMOE = moe2000(est = totWhitepop, n = totpop, designfac = 1.3),
                                                                                 Blackpov = GTV003,
                                                                                 totBlackpop = GTV003 + GTV004,
                                                                                 BlackpovMOE = moe2000(est = Blackpov, n = totpop, designfac = 1.3),
                                                                                 totBlackpopMOE = moe2000(est = totBlackpop, n = totpop, designfac = 1.3),
                                                                                 Hisppov = GTY001,
                                                                                 totHisppop = GTY001 + GTY002,
                                                                                 HisppovMOE = moe2000(est = Hisppov, n = totpop, designfac = 1.3),
                                                                                 totHisppopMOE = moe2000(est = totHisppop, n = totpop, designfac = 1.3),
                                                                                 pctTotalpov = totpov / totpop,
                                                                                 #pctWhitepov = Whitepov / totWhitepop,
                                                                                 pctBlackpov = Blackpov / totBlackpop,
                                                                                 pctHisppov = Hisppov / totHisppop,
                                                                                 
                                                                                 Totalmoeprop = moeprop(y = totpop, moex = totpovMOE, moey = totpopMOE, p = pctTotalpov),
                                                                                 #Whitemoeprop = moeprop(y = totWhitepop, moex = WhitepovMOE, moey = totWhitepopMOE, p = pctWhitepov),
                                                                                 Blackmoeprop = moeprop(y = totBlackpop, moex = BlackpovMOE, moey = totBlackpopMOE, p = pctBlackpov),
                                                                                 Hispmoeprop = moeprop(y = totHisppop, moex = HisppovMOE, moey = totHisppopMOE, p = pctHisppov)
) %>% cbind(pov00Wht) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2000") %>%
  mutate(race = case_when(var =="Totalmoeprop" ~ "All",
                          var =="Blackmoeprop" ~ "Black",
                          var =="Whitemoeprop" ~ "White,\nnon-Hispanic",
                          var =="Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)
# getting 2010 and 2016 from ACS1
##View(load_variables(year = 2016, dataset = "acs1"))

povvars10 <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
               'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
               'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
               'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
               'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames10 <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
                "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
                "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
                "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
                "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")
pov10_raw <- wholivesdatapull(povvars10, povnames10, year = 2010)

pov10 <- pov10_raw %>%
  filter(place == "071") %>% 
  transmute(place = place,
            year = 2010,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(place, year, val, name)

pov10MOE <- pov10_raw %>%
  filter(place == "071") %>% 
  transmute(year = 2010,
            place = place,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp,
            
            moeprop = moeprop(Total, BelowPovMOE, TotalMOE, pctTotalpov),
            Blackmoeprop = moeprop(Total_blk, BelowPovMOE_blk, TotalMOE_blk, pctBlackpov),
            Whitemoeprop = moeprop(Total_wht, BelowPovMOE_wht, TotalMOE_wht, pctWhitepov),
            Hispmoeprop = moeprop(Total_hisp, BelowPovMOE_hisp, TotalMOE_hisp, pctHisppov)
  ) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2010") %>%
  mutate(race = case_when(var =="moeprop" ~ "All",
                          var =="Blackmoeprop" ~ "Black",
                          var =="Whitemoeprop" ~ "White,\nnon-Hispanic",
                          var =="Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)


pov.hist_stattest <- left_join(pov10MOE, pov00MOE) %>%
  left_join(totalPov.hist %>% filter(year == 1999) %>% transmute(est2000 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2023) %>% transmute(est2023 = val, race = var)) %>%
  left_join(pov_stattest.data %>% filter(placename == "Orleans") %>% select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "moe2023") %>%
              mutate(race = case_when(var =="moeprop" ~ "All",
                                      var =="moeprop_blk" ~ "Black",
                                      var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                      var =="moeprop_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2023, moe2023),
         sig_10_21 = stattest(est2010, moe2010, est2023, moe2023)
  ) %>%
  select(race, contains("sig")) %>% filter(race != "All")

totalPov.hist <- totalPov.hist %>% left_join(pov.hist_stattest, by = c("var" = "race")) %>%
       mutate(val_lab = case_when((sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                                  T ~ " "))
totalpov.hist_csv <- totalPov.hist %>% select(year, race = var, val) %>% filter(year != 2016) %>% pivot_wider(names_from = year, values_from = val) 
write.csv(totalpov.hist_csv, "outputs/spreadsheets/totalpovhist.csv")
#storage_write_csv(totalpov.hist_csv, cont_proj, "who_lives/2024/outputs/totalpovhist.csv")


### Child poverty ###
###
load("inputs/childpovRaw_exp.RData")
childpov_exp <- childpovRaw_exp %>%
  mutate(
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
    # moeprop_asian = moeprop(y=TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p=pctBelowChildPov_asian)
  )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### stat test ###


childpov_stattest.data <- childpovRaw_exp %>%
  replace(is.na(.),0) %>%
  transmute(
    place = place,
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
    pctBelowChildPov_asian = ifelse(!is.nan(TotBelowChildPov_asian / TotChildPov_asian),TotBelowChildPov_asian / TotChildPov_asian,0),
    moeprop_asian = moeprop(y=TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p=pctBelowChildPov_asian) 
  )
childpov_stattest <- childpov_stattest.data%>%
  mutate(sig_wht_blk = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sigall_wht = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov, moey = moeprop),
         sig_blk_hisp = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sigall_blk = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov, moey = moeprop),
         sig_hisp_asian = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sigall_hisp = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov, moey = moeprop),
         sigall_asian = stattest(x=pctBelowChildPov_asian, moex = moeprop_asian, y=pctBelowChildPov, moey = moeprop)
  )

###
childpov_stat_all <- childpov_stattest %>% select(place, placename, (contains("sig") & contains("all")), contains("pct"), !contains("asian")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>% group_by(place, placename) %>%
  mutate(race = case_when(race == "pctBelowChildPov" ~ "All",
                          grepl("asian",race) & grepl("asian",var) ~ "Asian",
                          grepl("blk",race) & grepl("blk",var) ~ "Black",
                          grepl("hisp",race) & grepl("hisp",var) ~ "Hispanic, any race",
                          grepl("wht", race) & grepl("wht",var) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit()
childpov_stat_all$stat_all[childpov_stat_all$race == "All"] <- "yes"
childpov_stat_all <- childpov_stat_all %>%  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val*100), "%")) %>% select(-var, -placename) %>% unique()


childpov_stat_race <- childpov_stattest %>% select(place, placename, (contains("sig") & !contains("all") & !contains("asian"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% unique()
childpov_with_stats <- childpov_stat_all %>% left_join(childpov_stat_race, by = "place") %>% unique() %>% filter(race != "All" & !(race == "Asian" & place == "071")) %>%
  mutate(placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro*", "United States")),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")))

###

### Across geos child pov bar chart ###
childpov.totals <- childpov_stat_all %>% left_join(childpov_stat_race, by = "place") %>% unique() %>% 
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")),
         placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro*", "United States*")))

childpov.race <- childpov_exp %>%
  filter(var != "pctBelowChildPov") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

childpov.totals_CSV <- childpov.totals %>% ungroup() %>% select(placename.x, race, val) %>% 
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val) 

childpov.race_CSV <- childpov.race %>% select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>% rbind(childpov.totals_CSV)
write.csv(childpov.race_CSV, "outputs/spreadsheets/childpovrace.csv")
#storage_write_csv(childpov.race_CSV, cont_proj, "who_lives/2024/outputs/childpovrace")


### Historical child pov line chart ###
childPov.hist <- childPovProspInd %>% 
  mutate(year = ifelse(Year != 2010, Year - 1, Year)) %>%
  rename(val = childPov) %>%
  mutate(var = Race,
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Lat",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  select (-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(childpov_exp %>%
              filter(place.fac == "Orleans",
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2023, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "pctBelowChildPov", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

childPov.histMOE = data.frame(race = c("Black","White,\nnon-Hispanic","Hispanic,\nany race","All"),
                              se2010 = c(0.023372212, 0.016043476, 0.104259483, 0.021288336),
                              se2000 = c(0.005216453, 0.006067795, 0.026554044, 0.004610543)) %>%
  mutate(moe2010 = se2010*1.645,
         moe2000 = se2000*1.645)

childpov.hist_stattest <- left_join(childPov.histMOE, (childPov.hist %>% 
                                                         filter(Year == 2000) %>% 
                                                         transmute(est2000 = val, race = var)), by = "race") %>%
  left_join(childPov.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(childPov.hist %>% filter(Year == 2023) %>% transmute(est2023 = val, race = var)) %>%
  left_join((childpov_stattest.data %>% 
               filter(placename == "Orleans") %>% 
               select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
               pivot_longer(everything(), names_to = "var", values_to = "moe2023") %>%
               mutate(race = case_when(var =="moeprop" ~ "All",
                                       var =="moeprop_blk" ~ "Black",
                                       var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                       var =="moeprop_hisp" ~ "Hispanic,\nany race"
               )))) %>%
  select(-var) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2023, moe2023),
         sig_10_21 = stattest(est2010, moe2010, est2023, moe2023)
  ) %>%
  select(race, contains("sig"))

childPov.hist <- childPov.hist %>% left_join(childpov.hist_stattest, by = c("var" = "race")) %>% filter(var != "All") %>%
  mutate(val_lab = case_when((sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " "))

childpov.hist_csv <- childPov.hist %>% select(Year, race = var, val) %>% filter(Year != 2016) %>% pivot_wider(names_from = Year, values_from = val) 
write.csv(childpov.hist_csv, "outputs/spreadsheets/childpovhist.csv")
#storage_write_csv(childpov.hist_csv, cont_proj, "who_lives/2024/outputs/childpovhist.csv")


# Homeownership

load("inputs/hoRaw_exp.RData")
ho_exp <- hoRaw_exp %>%
  mutate(Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         Ownerpct_blk = Owner_blk / Total_blk,
         Ownermoeprop_blk = moeprop(y=Total_blk,moex = OwnerMOE_blk,moey = TotalMOE_blk,p=Ownerpct_blk),
         Ownerpct_wht = Owner_wht / Total_wht,
         Ownermoeprop_wht = moeprop(y=Total_wht,moex = OwnerMOE_wht,moey = TotalMOE_wht,p=Ownerpct_wht),
         Ownerpct_hisp = Owner_hisp / Total_hisp,
         Ownermoeprop_hisp = moeprop(y=Total_hisp,moex = OwnerMOE_hisp,moey = TotalMOE_hisp,p=Ownerpct_hisp),
         Ownerpct_asian = Owner_asian / Total_asian,
         #Ownermoeprop_asian = moeprop(y=Total_asian,moex = OwnerMOE_asian,moey = TotalMOE_asian,p=Ownerpct_asian)
  )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### Across geos homeownership bar chart ###

ho.race <- ho_exp %>%
  filter(var != "Ownerpct") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))


### Historical homeownership line chart ###
homeownership.hist <- homeownershipProspInd %>% 
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
              mutate(Year = 2023, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "Ownerpct", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

ho.hist_csv <- homeownership.hist %>% select(Year, race = var, val) %>% filter(Year != 2016) %>% pivot_wider(names_from = Year, values_from = val) 
write.csv(ho.hist_csv, "outputs/spreadsheets/hohist.csv")
#storage_write_csv(ho.hist_csv, cont_proj, "who_lives/2024/outputs/hohist.csv")



### stat test

ho_stattest.data <- hoRaw_exp %>%
  replace(is.na(.),0) %>%
  transmute(place = place,
            placename = placename,
            Ownerpct = Owner / Total,
            Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
            Ownerpct_blk = Owner_blk / Total_blk,
            Ownermoeprop_blk = moeprop(y=Total_blk,moex = OwnerMOE_blk,moey = TotalMOE_blk,p=Ownerpct_blk),
            Ownerpct_wht = Owner_wht / Total_wht,
            Ownermoeprop_wht = moeprop(y=Total_wht,moex = OwnerMOE_wht,moey = TotalMOE_wht,p=Ownerpct_wht),
            Ownerpct_hisp = Owner_hisp / Total_hisp,
            Ownermoeprop_hisp = moeprop(y=Total_hisp,moex = OwnerMOE_hisp,moey = TotalMOE_hisp,p=Ownerpct_hisp),
            Ownerpct_asian = Owner_asian / Total_asian,
            Ownermoeprop_asian = moeprop(y=Total_asian,moex = OwnerMOE_asian,moey = TotalMOE_asian,p=Ownerpct_asian)
  )

ho_stattest <- ho_stattest.data %>%
  mutate(sig_wht_blk = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_blk, moey = Ownermoeprop_blk),
         sig_wht_hisp = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_hisp, moey = Ownermoeprop_hisp),
         sig_wht_asian = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sigall_wht = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct, moey = Ownermoeprop),
         sig_blk_hisp = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct_hisp, moey = Ownermoeprop_hisp),
         sig_blk_asian = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sigall_blk = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct, moey = Ownermoeprop),
         sig_hisp_asian = stattest(x=Ownerpct_hisp, moex = Ownermoeprop_hisp, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sigall_hisp = stattest(x=Ownerpct_hisp, moex = Ownermoeprop_hisp, y=Ownerpct, moey = Ownermoeprop),
         sigall_asian = stattest(x=Ownerpct_asian, moex = Ownermoeprop_asian, y=Ownerpct, moey = Ownermoeprop)
  )

ho_stat_all <- ho_stattest %>% select(place, placename, (contains("sig") & contains("all")), contains("pct") &  !contains("asian")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>% group_by(place, placename) %>%
  mutate(race = case_when(race == "Ownerpct" ~ "All",
                          grepl("asian",race) & grepl("asian",var) ~ "Asian",
                          grepl("blk",race) & grepl("blk",var) ~ "Black",
                          grepl("hisp",race) & grepl("hisp",var) ~ "Hispanic, any race",
                          grepl("wht", race) & grepl("wht",var) ~ "White, non-Hispanic"),
         placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename)) %>% na.omit()
ho_stat_all$stat_all[ho_stat_all$race == "All"] <- "yes"
ho_stat_all <- ho_stat_all %>%  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val*100), "%")) %>% select(-var, -placename) %>% unique()


ho_stat_race <- ho_stattest %>% select(place, placename, (contains("sig") & !contains("all") & !contains("asian"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% select(-stat_race) %>% unique()
ho_with_stats <- ho_stat_all %>% left_join(ho_stat_race, by = "place") %>% unique() %>% filter(race != "All" & !(race == "Asian" & place == "071")) %>%
  mutate(placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro*", "United States")),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")))

ho.totals <- ho_stat_all %>% left_join(ho_stat_race, by = "place") %>% unique() %>% 
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")),
         placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro", "United States")))

ho.totals_CSV <- ho.totals %>% ungroup() %>% select(placename.x, race, val) %>% 
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val) 

ho.race_CSV <- ho.race %>% select(race = var, place.fac, val) %>% unique() %>%
  pivot_wider(names_from = place.fac, values_from = val) %>% rbind(ho.totals_CSV)
write.csv(ho.race_CSV, "outputs/spreadsheets/horace.csv")
#storage_write_csv(ho.race_CSV, cont_proj, "who_lives/2024/outputs/horace.csv")

###

### Across geos HO bar chart ###
ho.totals <- ho_stat_all %>% left_join(ho_stat_race, by = "place") %>% unique() %>% 
  filter(race == "All") %>%
  mutate(var.fac = factor(race, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")),
         placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "Metro", "United States")))

ho.hist_stattest <- 
  left_join((homeownership.hist %>% filter(Year == 2000) %>% transmute(est2000 = val, race = var)), (homeownership.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var))) %>%
  left_join((homeownership.hist %>% filter(Year == 2023) %>% transmute(est2023 = val, race = var)))%>%
  left_join((ho_stattest.data %>% 
               filter(placename == "Orleans") %>% 
               select(Ownermoeprop,Ownermoeprop_blk ,Ownermoeprop_wht ,Ownermoeprop_hisp) %>%
               pivot_longer(everything(), names_to = "var", values_to = "moe2023") %>%
               mutate(race = case_when(var =="Ownermoeprop" ~ "All",
                                       var =="Ownermoeprop_blk" ~ "Black",
                                       var =="Ownermoeprop_wht" ~ "White,\nnon-Hispanic",
                                       var =="Ownermoeprop_hisp" ~ "Hispanic,\nany race"
               ))) %>%
              select(-var)) %>%
  mutate(sig_00_21 = stattest(x=est2000,y= est2023, moey=moe2023),
         sig_10_21 = stattest(x=est2010, y= est2023, moey=moe2023)
  )  %>% filter(race != "All")

homeownership.hist <- homeownership.hist %>% filter(var != "All") %>% left_join(ho.hist_stattest, by = c("var" = "race")) %>%
  mutate(val_lab = case_when((sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
                             T ~ " "))

