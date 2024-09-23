############################################
# # ACS # #
############################################


# Total population change
## test: using the total pop numbers from our site to update the comparison year to 2020 instead of 2010 in the text

totalpop_metro <- read_xlsx("inputs/TheDataCenter_PopulationbyParish.xlsx")
totalpop_metro <- totalpop_metro %>%
  rename(year = `...1`) %>%
  mutate(year = str_replace(year, "Census ", ""),
         year = str_replace(year, "Estimate ", ""),
         year = as.numeric(year),
         
         metro_pop = `Metro Area total`)

save(totalpop_metro, file = "inputs/totalpop_metro.RData")

#Hispanic Origin

hispanvars <-c("B03001_001E","B03001_001M","B03001_002E","B03001_002M","B03001_003E","B03001_003M","B03001_004E","B03001_004M","B03001_005E","B03001_005M","B03001_006E","B03001_006M","B03001_007E","B03001_007M","B03001_008E","B03001_008M","B03001_009E","B03001_009M","B03001_010E","B03001_010M","B03001_011E","B03001_011M","B03001_012E","B03001_012M","B03001_013E","B03001_013M","B03001_014E","B03001_014M","B03001_015E","B03001_015M","B03001_016E","B03001_016M","B03001_027E","B03001_027M")
hispannames <-c("Total","TotalMOE","TotalNotHIsporLat","TotalNotHIsporLatMOE","TotalHisporLat","TotalHisporLatMOE","TotMex","TotMexMOE","TotPR","TotPRMOE","TotCuba","TotCubaMOE","TotDomin","TotDominMOE","TotCentrAm","TotCentrAmMOE","TotCostaR","TotCostaRMOE","TotGuat","TotGuatMOE","TotHond","TotHondMOE","TotNicarag","TotNicaragMOE","TotPanama","TotPanamaMOE","TotSalva","TotSalvaMOE","TotOtherCA","TotOtherCAMOE","TotSA","TotSAMOE","TotOtherHisporLat","TotOtherHisporLatMOE")
hispanRaw <- wholivesdatapull(hispanvars,hispannames)
save(hispanRaw, file = "inputs/hispanRaw.RData") # -3 removes St. Tammany because it is not included in this analysis

#Households with own children under 18
hwcvars <- c('B11001_001E','B11001_001M','B11003_003E','B11003_003M','B11003_010E','B11003_010M','B11003_016E','B11003_016M')
hwcnames <- c("TotalHH", "TotalHHMOE","Married", "MarriedMOE", "MaleHH", "MaleHHMOE", "FemaleHH" ,"FemaleHHMOE")
hwcRaw <- wholivesdatapull(hwcvars, hwcnames)
save(hwcRaw, file = "inputs/hwcRaw.RData")

hwc2000vars <- c("P012001","P012005", "P012009", "P012012", "P012020", "P012024", "P012027")
hwc2000names <- c("TotalHH", "Married15to64", "MaleHH15to64", "FemaleHH15to64", "Married65plus", "MaleHH65plus", "FemaleHH65plus")
hwc2000Raw <- wholivesdatapull2000(hwc2000vars, hwc2000names, universe = "households")
save(hwc2000Raw, file = "inputs/hwc2000Raw.RData")


## Median age calculation

medagevars <- c("B01002_001E")
medagenames <- c("medianage")
medageRaw <- getCensus("acs/acs1",
                       year,
                       key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                       vars = medagevars,
                       region = "metropolitan statistical area/micropolitan statistical area:35380")
names(medageRaw) <- c("metro", "medage")
save(medageRaw, file = "inputs/medageRaw.RData")

medagevars2000 <- c("")

#Single-person households

singvars <- c('B11001_001E','B11001_001M','B11001_008E','B11001_008M')
singnames <- c("TotalHH","TotalMOE","SingleHH","SingleHHMOE")
singRaw <- wholivesdatapull(singvars, singnames)
save(singRaw, file = "inputs/singRaw.RData")

singvars2000 <- c('P012001','P010002')
singnames2000 <- c("TotalHH","SingleHH")
singRaw2000 <- wholivesdatapull2000(singvars2000,singnames2000, universe = "households")
save(singRaw2000,file = "inputs/singRaw2000.RData")


#Less than a high school degree, adults 25 and older

hsvars <- c('C15002_001E','C15002_001M','C15002_003E','C15002_003M','C15002_004E','C15002_004M','C15002_011E','C15002_011M','C15002_012E','C15002_012M')
hsnames <- c("Total", "TotalMOE", "Male9", "Male9MOE", "Male9to12", "Male9to12MOE", "Female9", "Female9MOE", "Female9to12", "Female9to12MOE")
hsRaw <- wholivesdatapull(hsvars, hsnames)
save(hsRaw, file = "inputs/hsRaw.RData")

hsvars2000 <- c('P037001','P037003','P037004','P037005',"P037006",'P037007','P037008','P037009','P037010','P037020','P037021','P037022','P037023','P037024','P037025','P037026','P037027')
hsnames2000 <- c("Total","MaleNS","Male4","Male5to6","Male7to8","Male9","Male10","Male11","Male12ND","FemaleNS","Female4","Female5to6","Female7to8","Female9","Female10","Female11", "Female12ND")
hsRaw2000 <- wholivesdatapull2000(hsvars2000,hsnames2000)
save(hsRaw2000,file = "inputs/hsRaw2000.RData")


#Bachelor's degree or higher, adults 25 and older

bachvars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M','C15002_017E','C15002_017M')
bachnames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE")
bachRaw <- wholivesdatapull(bachvars, bachnames)
save(bachRaw, file = "inputs/bachRaw.RData")

bachvars2000 <- c('P037001','P037015','P037016','P037017','P037018','P037032','P037033','P037034','P037035')
bachnames2000 <- c("Total","MaleBach","MaleMasters","MaleProf","MaleDoc","FemaleBach","FemaleMasters","FemaleProf","FemaleDoc")
bachRaw2000 <- wholivesdatapull2000(bachvars2000,bachnames2000)
save(bachRaw2000,file = "inputs/bachraw2000.RData")

#Median household income, inflation-adjusted dollars

medhhvars <- c('B19013_001E','B19013_001M')
medhhnames <- c("MedianHHIncome", "MedianHHIncomeMOE")
medhhRaw <- wholivesdatapull(medhhvars, medhhnames)
save(medhhRaw, file = "inputs/medhhRaw.RData")

medhhvars2000 <- c('HCT012001')
medhhames2000 <- c("MedianHHIncome")
medhhRaw2000 <- wholivesdatapull2000(medhhvars2000,medhhames2000, universe = "households")
save(medhhRaw2000, file = "inputs/medhhRaw2000.RData")


#Internet access

intavars <- c('B28002_001E','B28002_001M','B28002_006E','B28002_006M','B28002_012E','B28002_012M','B28002_013E','B28002_013M')
intanames <- c("Total","TotalMOE","CellOnly","CellOnlyMOE","NoSubscript","NoSubscriptMOE","NoAccess","NoAccessMOE")
intaRaw <- wholivesdatapull(intavars, intanames)
save(intaRaw, file = "inputs/intaRaw.RData")


#Poverty rate, population for whom poverty has been determined

povvars <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M')
povnames <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE")
povRaw <- wholivesdatapull(povvars, povnames)
save(povRaw, file = "inputs/povRaw.RData")

povvars2000 <-c('P087001','P087002')
povnames2000 <-c("Total", "BelowPov")
povRaw2000 <- wholivesdatapull2000(povvars2000,povnames2000)
save(povRaw2000,file = "inputs/povRaw2000.RData")


#Children in poverty, population for whom poverty has been determined	

childpovvars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M')
childpovnames <- c( "BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE")
childpovRaw <- wholivesdatapull(childpovvars, childpovnames)
save(childpovRaw, file = "inputs/childpovRaw.RData")

childpovvars2000 <-c('P087003','P087004','P087005','P087006','P087011','P087012','P087013',	'P087014') 
childpovnames2000 <-c( "BelowPovUnder5Years","BelowPov5Years","BelowPov6to11Years","BelowPov12to17Years","AbovePovUnder5Years","AbovePov5Years","AbovePov6to11Years","AbovePov12to17")
childpovRaw2000 <- wholivesdatapull2000(childpovvars2000,childpovnames2000)
save(childpovRaw2000,file = "inputs/childpovRaw2000.RData")

#Households without access to a vehicle

vehvars <- c('B08201_001E','B08201_001M','B08201_002E','B08201_002M')
vehnames <- c("Total","TotalMOE","NoVehAvail","NoVehAvailMOE")
vehRaw <- wholivesdatapull(vehvars, vehnames)
save(vehRaw, file = "inputs/vehRaw.RData")

vehvars2000<- c('H044001','H044003','H044010')
vehnames2000<- c("Total","OwnNoVehAvail","RentNoVehAvail")
vehRaw2000 <- wholivesdatapull2000(vehvars2000,vehnames2000, universe = "households")
save(vehRaw2000,file = "inputs/vehRAw2000.RData")

#Population not U.S. citizens at birth

forborvars <- c('C05005_004E','C05005_004M','C05005_007E','C05005_007M','C05005_010E','C05005_010M','C05005_013E','C05005_013M','B01003_001E','B01003_001M')
forbornames <- c("TotForeign10on","TotForeign10onMOE","TotForeign00to09","TotForeign00to09MOE","TotForeign90to99","TotForeign90to99MOE","TotForeignPre90","TotForeignPre90MOE","TotalPop","TotalPopMOE")
forborRaw <- wholivesdatapull(forborvars, forbornames)
save(forborRaw, file = "inputs/forborRaw.RData")

forborvars2000<- c('P001001','P022002','P022003','P022004','P022005','P022006','P022007','P022008','P022009')
forbornames2000<- c("Total","TotForeign95to00","TotForegin90to94","TotForegin85to89","TotForeign80to84","TotForegin75to79","TotForeign70to74","TotForegin65to69","TotForeginPre65")
forborRaw2000<-wholivesdatapull2000(forborvars2000,forbornames2000)
save(forborRaw2000, file = "inputs/forborRaw2000.RData")

#Population who moved in the past year

mobvars <- c('B07003_001E','B07003_001M','B07003_004E','B07003_004M','B07003_007E','B07003_007M','B07003_010E','B07003_010M','B07003_013E','B07003_013M','B07003_016E','B07003_016M')
mobnames <- c("Total","TotalMOE","TotSameHouse","TotSameHouseMOE","TotMovedinCty","TotMovedinCtyMOE","TotMovedinState","TotMovedinStateMOE","TotMovedbtwnStates","TotMovedbtwnStatesMOE","TotMovedfromAbroad","TotMovedfromAbroadMOE")
mobRaw <- wholivesdatapull(mobvars, mobnames)
save(mobRaw, file = "inputs/mobRaw.RData")

# Total Population by race

totpopracevars<- c('P001001','P006003','P006002','P007010','P006005')
totpopracenames<-c("Total","TotBlackAlone","TotWhiteAlone","TotHispanicAny","TotAsianAlone")
totpopraceRaw<-wholivesdatapull2000(totpopracevars,totpopracenames, parishregions = "county:071,051,075,087,089,093,095")
save(totpopraceRaw, file = "inputs/totpoprace2000.Rdata")

#Checking sf1
getCensus(name = "dec/sf1", vintage = 2000, key = mycensuskey, vars = c("P003003", 	
                                                                    "P003004"), region = "county:071,051", regionin = "state:22")

# Total Population by Age,*inhales*
# Need total population 
totpopage2000vars <-c('P008001','P008003','P008004','P008005','P008006','P008007','P008008','P008009','P008010','P008011','P008012','P008013','P008014','P008015','P008016','P008017','P008018','P008019','P008020','P008021','P008022','P008023','P008024','P008025','P008026','P008027','P008028','P008029','P008030','P008031','P008032','P008033','P008034','P008035','P008036','P008037','P008038','P008039','P008040','P008042','P008043','P008044','P008045','P008046','P008047','P008048','P008049','P008050','P008051','P008052','P008053','P008054','P008055','P008056','P008057','P008058','P008059','P008060','P008061','P008062','P008063','P008064','P008065','P008066','P008067','P008068','P008069','P008070','P008071','P008072','P008073','P008074','P008075','P008076','P008077','P008078','P008079')
totpopage2000names <-c("totalsexbyage","MaleUnder1yr","Male1yr","Male2yr","Male3yr","Male4yr","Male5yr","Male6yr","Male7yr","Male8yr","Male9yr","Male10yr","Male11yr","Male12yr","Male13yr","Male14yr","Male15yr","Male16yr","Male17yr","Male18yr","Male19yr","Male20yr","Male21yr","Male22to24","Male25to29","Male30to34","Male35to39","Male40to44","Male45to49","Male50to54","Male55to59","Male60to61","Male62to64","Male65to66","Male67to69","Male70to74","Male75to79","Male80to84","MaleOver85","FemaleUnder1","Female1yr", "Female2yr","Female3yr","Female4yr","Female5yr","Female6yr","Female7yr","Female8yr","Female9yr","Female10yr","Female11yr","Female12yr","Female13yr","Female14yr","Female15yr","Female16yr","Female17yr","Female18yr","Female19yr","Female20yr","Female21yr","Female22to24","Female25to29","Female30to34","Female35to39","Female40to44","Female45to49","Female50to54","Female55to59","Female60to61","Female62to64","Female65to66","Female67to69","Female70to74","Female75to79","Female80to84","FemaleOver85")
totpopage2000Raw <-wholivesdatapull2000(totpopage2000vars,totpopage2000names, parishregions = "county:071,051,075,087,089,093,095")
save(totpopage2000Raw, file = "inputs/totpopage2000.RData")


# https://www2.census.gov/acs2004/Core_Tables/
ACScounty_04 <- read_csv("inputs/ACS_data/ACS_2004_050.csv") 
ACSUS_04 <- read_csv("inputs/ACS_data/ACS_2004_010.csv")
mob04Raw <- ACScounty_04 %>% rbind(ACSUS_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
          grepl("01000US", geoid)) %>% 
          filter((tblid == "B07003" & (order == 1 | 
                                 order == 4 |
                                 order == 7 | 
                                 order == 10 | 
                                 order == 13 | 
                                 order == 16) )) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States"),
         var = case_when(tblid == "B07003" & (order == 1) ~ "Total",
                         tblid == "B07003" & (order == 4) ~ "TotSameHouse",
                         tblid == "B07003" & (order == 7) ~ "TotMovedinCty",
                         tblid == "B07003" & (order == 10) ~ "TotMovedinState",
                         tblid == "B07003" & (order == 13) ~ "TotMovedbtwnStates",
                         tblid == "B07003" & (order == 16) ~ "TotMovedfromAbroad"),
         cest = as.numeric(cest),
         clb = as.numeric(clb))  %>%
  select(placename, var, MOE, cest) %>%
  pivot_wider(names_from = var, values_from = c(MOE, cest)) %>%
  mutate(sf2004mobabroadpct = cest_TotMovedfromAbroad / cest_Total,
         sf2004mobabroadpctMOE = as.numeric(moeprop(y=cest_Total,moex = MOE_TotMovedfromAbroad,moey = MOE_Total,p=sf2004mobabroadpct)),
         sf2004mobStatespct = cest_TotMovedbtwnStates / cest_Total,
         sf2004mobStatespctMOE = as.numeric(moeprop(y=cest_Total,moex = MOE_TotMovedbtwnStates,moey = MOE_Total,p=sf2004mobStatespct)),
         sf2004difparishpct = cest_TotMovedinState / cest_Total,
         sf2004difparishpctMOE = as.numeric(moeprop(y=cest_Total,moex = MOE_TotMovedinState,moey = MOE_Total,p=sf2004difparishpct)),
         sf2004withinparishpct = cest_TotMovedinCty / cest_Total,
         sf2004withinparishpctMOE = as.numeric(moeprop(y=cest_Total,moex = MOE_TotMovedinCty,moey = MOE_Total,p=sf2004withinparishpct)),
         sf2004samehousepct = cest_TotSameHouse / cest_Total,
         sf2004samehousepctMOE = as.numeric(moeprop(y=cest_Total, moex = MOE_TotSameHouse,moey = MOE_Total,p=sf2004samehousepct))) %>%
  select(placename, 
         sf2004mobabroadpct, sf2004mobabroadpctMOE, 
         sf2004mobStatespct, sf2004mobStatespctMOE, 
         sf2004difparishpct, sf2004difparishpctMOE,
         sf2004withinparishpct, sf2004withinparishpctMOE,
         sf2004samehousepct, sf2004samehousepctMOE)
 
save(mob04Raw, file = "inputs/mob04Raw.RData")

#Homeownership rates

hovars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M')
honames <- c("Total","TotalMOE","Owner","OwnerMOE")
hoRaw <- wholivesdatapull(hovars, honames)
save(hoRaw, file = "inputs/hoRaw.RData")

hovars2000 <- c('H007001', 'H007002')
honames2000 <- c("Total", "Owner")
hoRaw2000 <- wholivesdatapull2000(hovars2000, honames2000, universe = "households")
save(hoRaw2000, file = "inputs/hoRaw2000.RData")

#Homeowners without a mortgage

honomovars <- c('B25081_001E','B25081_001M','B25081_002E','B25081_002M','B25081_009E','B25081_009M')
honomonames <- c("Total","TotalMOE","Mortgage","MortgageMOE","NoMortgage","NoMortgageMOE")
honomoRaw <- wholivesdatapull(honomovars, honomonames)
save(honomoRaw, file = "inputs/honomoRaw.RData")


honomovars2000 <- c('H098001','H098018')
honomonames2000 <-c("Total","NoMortgage")
honomoRaw2000<- wholivesdatapull2000(honomovars2000,honomonames2000, universe = "households")
save(honomoRaw2000, file = "inputs/honomoRaw2000.RData")

#Renters with severe housing cost burdens

rentburvars <- c('B25070_001E','B25070_001M','B25070_010E','B25070_010M','B25070_011E','B25070_011M')
rentburnames <- c("Total","TotalMOE","50orMore","50orMoreMOE","NotComputed","NotComputedMOE")
rentburRaw <- wholivesdatapull(rentburvars, rentburnames)
save(rentburRaw, file = "inputs/rentburRaw.RData")


#Homeowners with severe housing cost burdens

hoburvars <- c('B25091_001E','B25091_001M','B25091_011E','B25091_011M','B25091_012E','B25091_012M','B25091_022E','B25091_022M','B25091_023E','B25091_023M')
hoburnames <- c("Total","TotalMOE","50orMoreMortgage","50orMoreMortgageMOE","NotComputedMortgage","NotComputedMortgageMOE","50orMoreNoMortgage","50orMoreNoMortgageMOE","NotComputedNoMortgage","NotComputedNoMortgageMOE")
hoburRaw <- wholivesdatapull(hoburvars, hoburnames)
save(hoburRaw, file = "inputs/hoburRaw.RData")

#### getting SEs for 2004 data:

housing <-  ACScounty_04 %>% rbind(ACSUS_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
            grepl("01000US", geoid)
           ) %>% 
  filter((tblid == "B25064" & order == 1) |
           (tblid == "B25070" & (order == 1 | order == 10 | order == 11 ) |
              (tblid == "B25091" & (order == 1 | order == 11| order == 12| order == 22| order == 23)))) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States"),
         var = case_when(tblid == "B25064" & order == 1 ~ "medgrossrent",
                         tblid == "B25070" & order == 1 ~  "totrenters",
                         tblid == "B25070" & order == 10 ~ "rentcostburden",
                         tblid == "B25070" & order == 11 ~ "renters_notcomp",
                         tblid == "B25091" & order == 1 ~ "tothomeowners",
                         tblid == "B25091" & order == 11 ~ "hocostburden",
                         tblid == "B25091" & order == 12 ~ "ho_notcomp",
                         tblid == "B25091" & order == 22 ~ "hocostburden_nomort",
                         tblid == "B25091" & order == 23 ~ "ho_notcomp_nomort"),
         cest = as.numeric(cest),
         clb = as.numeric(clb)) %>%
  select(placename, var, MOE, cest) %>%
  pivot_wider(names_from = var, values_from = c(MOE, cest)) %>%
  mutate(rentburpct2004 = (cest_rentcostburden) / (cest_totrenters - cest_renters_notcomp),
         MOE_rentersagg = moeagg(cbind(MOE_totrenters,MOE_renters_notcomp)),
         rentburpct2004MOE = moeprop(y = cest_totrenters- cest_renters_notcomp, moex = MOE_rentcostburden, moey = MOE_rentersagg, p = rentburpct2004),
         medgrossrent2004 = cest_medgrossrent,
         medgrossrent2004MOE = MOE_medgrossrent,
         hoburpct2004 = (cest_hocostburden+cest_hocostburden_nomort) / (cest_tothomeowners - (cest_ho_notcomp+cest_ho_notcomp_nomort)),
         MOE_hoburagg = moeagg(cbind(MOE_hocostburden,MOE_hocostburden_nomort)),
         MOE_hoagg = moeagg(cbind(MOE_tothomeowners,MOE_ho_notcomp,MOE_ho_notcomp_nomort)),
         hoburpct2004MOE = moeprop(y = (cest_tothomeowners- (cest_ho_notcomp+cest_ho_notcomp_nomort)), moex = MOE_hoburagg, moey = MOE_hoagg, p = hoburpct2004)) %>%
  select(placename, medgrossrent2004, medgrossrent2004MOE, rentburpct2004, rentburpct2004MOE, hoburpct2004, hoburpct2004MOE)
rentburRaw2004 <- housing
save(rentburRaw2004, file = "inputs/rentburRaw2004.RData")



#Median gross rent, 201* inflation-adjusted dollars

medrentvars <- c('B25064_001E','B25064_001M')
medrentnames <- c("Rent","RentMOE")
medrentRaw <- wholivesdatapull(medrentvars, medrentnames)
save(medrentRaw, file = "inputs/medrentRaw.RData")


#Year structure built, 201* housing units

yrbuiltvars <- c('B25034_001E','B25034_001M','B25034_002E','B25034_002M','B25034_003E','B25034_003M','B25034_004E','B25034_004M','B25034_005E','B25034_005M','B25034_006E','B25034_006M','B25034_007E','B25034_007M','B25034_008E','B25034_008M','B25034_009E','B25034_009M','B25034_010E','B25034_010M','B25034_011E','B25034_011M')
yrbuiltnames <- c("Total","TotalMOE","2014","2014MOE","2010to2013","2010to2013MOE","2000to2009","2000to2009MOE","1990to1999","1990to1999MOE","1980to1989","1980to1989MOE","1970to1979","1970to1979MOE","1960to1969","1960to1969MOE","1950to1959","1950to1959MOE","1940to1949","1940to1949MOE","1939","1939MOE")
yrbuiltRaw <- wholivesdatapull(yrbuiltvars, yrbuiltnames)
save(yrbuiltRaw, file = "inputs/yrbuiltRaw.RData")


#Means of transportation to work, workers 16 and older

commutevars <- c('B08301_001E','B08301_001M','B08301_003E','B08301_003M','B08301_004E','B08301_004M','B08301_010E','B08301_010M','B08301_016E','B08301_016M','B08301_017E','B08301_017M','B08301_018E','B08301_018M','B08301_019E','B08301_019M','B08301_020E','B08301_020M','B08301_021E','B08301_021M')
commutenames <- c("Total","TotalMOE","DroveAlone","DroveAloneMOE","Carpool","CarpoolMOE","PublicTransit","PublicTransitMOE","Taxi","TaxiMOE","Motorcycle","MotorcycleMOE","Bike","BikeMOE","Walk","WalkMOE","Other","OtherMOE","Workhome","WorkhomeMOE")
commuteRaw <- wholivesdatapull(commutevars, commutenames)
save(commuteRaw, file = "inputs/commuteRaw.RData")


commutevars2000 <- c('P030001', 'P030003','P030004','P030005','P030011','P030012','P030013','P030014','P030015','P030016')
commutenames2000 <- c("Total","DroveAlone","Carpool","PublicTransit","Taxi","Motorcycle","Bike","Walk","Other","Workhome")
commuteRaw2000 <- wholivesdatapull2000(commutevars2000, commutenames2000)
save(commuteRaw2000,file = "inputs/commuteRaw2000.RData")


# Creating a time series for total employment - had to get 1980-2000 from IPUMS NHGIS


employ80 <-  read_csv("inputs/indicator expansion drafts/employment/nhgis0014_csv/nhgis0014_ds107_1980_county.csv")
hispemploy80  <- read_csv("inputs/indicator expansion drafts/employment/nhgis0015_csv/nhgis0015_ds107_1980_county.csv")
employ90 <- read_csv("inputs/indicator expansion drafts/employment/nhgis0014_csv/nhgis0014_ds123_1990_county.csv")
hispemploy90<- read_csv("inputs/indicator expansion drafts/employment/nhgis0016_csv/nhgis0016_ds123_1990_county.csv")
employ00 <- read_csv("inputs/indicator expansion drafts/employment/nhgis0019_csv/nhgis0019_ds151_2000_county.csv")
hisemploy00<- read_csv("inputs/indicator expansion drafts/employment/nhgis0017_csv/nhgis0017_ds151_2000_county.csv")
employ10  <- read_csv("inputs/indicator expansion drafts/employment/nhgis0013_csv/nhgis0013_ds175_2010_county.csv")


##Or we can use the data from the Prosperity Index for the years 1980,1990, and 2000. We well then join the 2010 on (constructed below) and the 2023 data pull. 

employ80to00<- read_csv("inputs/indicator expansion drafts/employment/Employment_Rates_20240919.csv")


# Employment Rates by race by sex, current


employmentvars <- c("C23002H_003E", "C23002H_003M", "C23002H_007E", "C23002H_007M", "C23002H_016E",  "C23002H_016M", "C23002H_020E", "C23002H_020M", "C23002B_003E", "C23002B_003M", "C23002B_007E", "C23002B_007M", "C23002B_016E", "C23002B_016M", "C23002B_020E", "C23002B_020M", "C23002I_003E", "C23002I_003M", "C23002I_007E", "C23002I_007M", "C23002I_016E", "C23002I_016M", "C23002I_020E",  "C23002I_020M")
employmentnames <- c("WhtMaleTot", "WhtMaleTotMOE", "WhtMaleEmp", "WhtMaleEmpMOE", "WhtFemaleTot", "WhtFemaleTotMOE", "WhtFemaleEmp", "WhtFemaleEmpMOE", "BlkMaleTot", "BlkMaleTotMOE", "BlkMaleEmp", "BlkMaleEmpMOE", "BlkFemaleTot", "BlkFemaleTotMOE", "BlkFemaleEmp", "BlkFemaleEmpMOE", "HispMaleTot", "HispMaleTotMOE", "HispMaleEmp", "HispMaleEmpMOE", "HispFemaleTot", "HispFemaleTotMOE", "HispFemaleEmp", "HispFemaleEmpMOE")

employmentRaw<-wholivesdatapull(employmentvars,employmentnames)
save(employmentRaw, file = "inputs/employmentRaw.RData")

#filter to Orleans Parish and create the columns we need 


# employ80 <- employ80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
#                                                                                  totWhiteMalepop =  sum(c_across(DHY001:DHY004), na.rm = T),
#                                                                                  totWhiteFemalepop = sum(c_across(DHY001:DHY004), na.rm = T),
#                                                                                  WhiteMaleEmploy =  DHY002,
#                                                                                  WhiteFemaleEmploy = DHY006,
#                                                                                  totBlackMalepop = sum(c_across(DHY009:DHY012), na.rm = T),
#                                                                                  totBlackFemalepop = sum(c_across(DHY013: DHY016), na.rm = T),                     
#                                                                                  BlackFemaleemploy = DHY014,
#                                                                                  BlackMaleEmploy = DHY010,
#                                                                                  pctWhiteMaleEmploy = DHY002 / totWhiteMalepop,
#                                                                                  pctWhiteFemaleEmploy = DHY006 / totWhiteFemalepop,
#                                                                                  pctBlackMaleEmploy = DHY010 / totBlackMalepop,
#                                                                                  pctBlackFemaleEmploy = DHY014 / totBlackFemalepop) %>% pivot_longer(cols = pctWhiteMaleEmploy:pctBlackFemaleEmploy, values_to = "val") %>% select(year, val, name)
# 
# hispemploy80 <- hispemploy80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
#                                                                                          totHispMalepop = sum(c_across(DHZ001:DHZ004), na.rm = T),
#                                                                                          totHispFemalepop = sum(c_across(DHZ004:DHZ008), na.rm = T),
#                                                                                          HispMaleEmploy = DHZ002,
#                                                                                          HispFemaleEmploy = DHZ006,
#                                                                                          pctHispMaleEmploy = DHZ002/totHispMalepop,
#                                                                                          pctHispFemaleEmploy = DHZ006/ totHispFemalepop) %>% pivot_longer(cols = pctHispMaleEmploy:pctHispFemaleEmploy, values_to = "val") %>% select(year, val, name)
# employ80<-rbind(employ80,hispemploy80)                                      
# 
# employ90 <- employ90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1990,
#                                                                                  totWhiteMalepop =  sum(c_across(E4J001:E4J004), na.rm = T),
#                                                                                  totWhiteFemalepop = sum(c_across(E4J005:E4J008), na.rm = T),
#                                                                                  WhiteMaleEmploy =  E4J002,
#                                                                                  WhiteFemaleEmploy = E4J006,
#                                                                                  totBlackMalepop = sum(c_across(E4J009:E4J012), na.rm = T),
#                                                                                  totBlackFemalepop = sum(c_across(E4J013: E4J016), na.rm = T),                     
#                                                                                  BlackFemaleemploy =E4J014,
#                                                                                  BlackMaleEmploy = E4J010,
#                                                                                  pctWhiteMaleEmploy = E4J002/ totWhiteMalepop,
#                                                                                  pctWhiteFemaleEmploy = E4J006 / totWhiteFemalepop,
#                                                                                  pctBlackMaleEmploy = E4J010 / totBlackMalepop,
#                                                                                  pctBlackFemaleEmploy = E4J014 / totBlackFemalepop) %>% pivot_longer(cols = pctWhiteMaleEmploy:pctBlackFemaleEmploy, values_to = "val") %>% select(year, val, name)
# 
# hispemploy90 <- hispemploy90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1990,
#                                                                                          totHispMalepop = sum(c_across(E4K001:E4K004), na.rm = T),
#                                                                                          totHispFemalepop = sum(c_across(E4K005:E4K008), na.rm = T),
#                                                                                          HispMaleEmploy = E4K002,
#                                                                                          HispFemaleEmploy = E4K006,
#                                                                                          pctHispMaleEmploy = E4K002/totHispMalepop,
#                                                                                          pctHispFemaleEmploy = E4K006/ totHispFemalepop) %>% pivot_longer(cols = pctHispMaleEmploy:pctHispFemaleEmploy, values_to = "val") %>% select(year, val, name)
# employ90<-rbind(employ90,hispemploy90)
# save(employment, file = "inputs/employ_TS.RData")
# 
# # We need the 2000 MOE, so we are doing it manually from the NHGIS data.
# #For all of these, we need 2000 total pop from Orleans parish. And the design factor is 2.0 for all of them.
# 
# orleans_totpop<- getCensus(name = "dec/sf3", vintage = 2000, key = mycensuskey, vars = "P001001", region = "county:071", regionin = "state:22") %>% select(-state) %>% rename(POP = P001001)
# 
# employ_test<-employ00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
#                                                                                   WhiteMale_employ = GSS001,
#                                                                                   WhiteMale_employMOE = moe2000(est = WhiteMale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteMalepopInLabor = GSQ001, 
#                                                                                   totWhiteMalepopInLaborMOE = moe2000(est = totWhiteMalepopInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteMalepopNotInLabor =  GSQ002,
#                                                                                   totWhiteMalepopNotInLaborMOE = moe2000(est = totWhiteMalepopNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteMalepop = totWhiteMalepopInLabor + totWhiteMalepopNotInLabor,
#                                                                                   totWhiteMalepopMOE = moeagg(cbind(totWhiteMalepopInLaborMOE, totWhiteMalepopNotInLaborMOE)),
#                                                                                   WhiteFemale_employ = GSS003,
#                                                                                   WhiteFemale_employMOE = moe2000(est = WhiteFemale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteFemaleInLabor = GSQ003,
#                                                                                   totWhiteFemaleInLaborMOE = moe2000(est = totWhiteFemaleInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteFemaleNotInLabor = GSQ004,
#                                                                                   totWhiteFemaleNotInLaborMOE = moe2000(est = totWhiteFemaleNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totWhiteFemalepop = totWhiteFemaleInLabor + totWhiteFemaleNotInLabor,
#                                                                                   totWhiteFemalepopMOE = moeagg(cbind(totWhiteFemaleInLaborMOE, totWhiteFemaleNotInLaborMOE)),
#                                                                                   BlackMale_employ = GSS005,
#                                                                                   BlackMale_employMOE = moe2000(est = BlackMale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackMaleInLabor =  GSQ005, 
#                                                                                   totBlackMaleInLaborMOE = moe2000(est = totBlackMaleInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackMaleNotInLabor = GSQ006,
#                                                                                   totBlackMaleNotInLaborMOE = moe2000(est = totBlackMaleNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackMalepop = totBlackMaleInLabor + totBlackMaleNotInLabor,
#                                                                                   totBlackMalepopMOE = moeagg(cbind(totBlackMaleInLaborMOE, totBlackMaleNotInLaborMOE)),
#                                                                                   BlackFemale_employ = GSS007,
#                                                                                   BlackFemale_employMOE = moe2000(est = BlackFemale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackFemaleInLabor =  GSQ007,
#                                                                                   totBlackFemaleInLaborMOE = moe2000(est = totBlackFemaleInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackFemaleNotInLabor = GSQ006,
#                                                                                   totBlackFemaleNotInLaborMOE = moe2000(est = totBlackFemaleNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                   totBlackFemalepop = totBlackFemaleInLabor + totBlackFemaleNotInLabor,
#                                                                                   totBlackFemalepopMOE = moeagg(cbind( totBlackFemaleInLaborMOE,  totBlackFemaleNotInLaborMOE)),
#                                                                                   pctWhiteMaleEmploy = WhiteMale_employ / totWhiteMalepop,
#                                                                                   pctWhiteMaleEmployMOE = moeprop(y = totWhiteMalepop, moex = WhiteMale_employMOE, moey = totWhiteMalepopMOE, p = pctWhiteMaleEmploy),
#                                                                                   pctWhiteFemaleEmploy = WhiteFemale_employ / totWhiteFemalepop,
#                                                                                   pctWhiteFemaleEmployMOE = moeprop(y = totWhiteFemalepop, moex = WhiteFemale_employMOE, moey = totWhiteFemalepopMOE, p = pctWhiteFemaleEmploy),
#                                                                                   pctBlackMaleEmploy = BlackMale_employ / totBlackMalepop,
#                                                                                   pctBlackMaleEmployMOE = moeprop(y = totBlackMalepop, moex =   BlackMale_employMOE, moey = totBlackMalepopMOE, p = pctBlackMaleEmploy),  
#                                                                                   pctBlackFemaleEmploy =  BlackFemale_employ/totBlackFemalepop,
#                                                                                   pctBlackFemaleEmployMOE = moeprop(y = totBlackFemalepop, moex = BlackFemale_employMOE, moey = totBlackFemalepopMOE, p = pctBlackFemaleEmploy)) %>% pivot_longer(cols = pctWhiteMaleEmploy:pctBlackFemaleEmployMOE, values_to = "val") %>% select(year, val, name)
# 
# hispemploy_test <- hisemploy00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
#                                                                                            HispMale_employ = GSX001,
#                                                                                            HispMale_employMOE = moe2000(est = HispMale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispMaleInLabor = GSV001,
#                                                                                            totHispMaleInLaborMOE =  moe2000(est = totHispMaleInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispMaleNotInLabor = GSV002, 
#                                                                                            totHispMaleNotInLaborMOE = moe2000(est = totHispMaleNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispMalepop = totHispMaleInLabor + totHispMaleNotInLabor,
#                                                                                            totHispMalepopMOE = moeagg(cbind(totHispMaleInLaborMOE,  totHispMaleNotInLaborMOE)),
#                                                                                            HispFemale_employ = GSX003,
#                                                                                            HispFemale_employMOE = moe2000(est = HispFemale_employ, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispFemaleInLabor = GSV003,
#                                                                                            totHispFemaleInLaborMOE = moe2000(est = totHispFemaleInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispFemaleNotInLabor = GSV004,
#                                                                                            totHispFemaleNotInLaborMOE = moe2000(est = totHispFemaleNotInLabor, n = orleans_totpop$POP, designfac = 2),
#                                                                                            totHispFemalepop = totHispFemaleInLabor + totHispFemaleNotInLabor,
#                                                                                            totHispFemalepopMOE = moeagg(cbind(totHispFemaleInLaborMOE,  totHispFemaleNotInLaborMOE)),
#                                                                                            pctHispMaleEmploy = HispMale_employ/ totHispMalepop,
#                                                                                            pctHispMaleEmployMOE = moeprop(y = totHispMalepop, moex = HispMale_employMOE, moey = totHispMalepopMOE, p = pctHispMaleEmploy),
#                                                                                            pctHispFemaleEmploy = HispFemale_employ/ totHispFemalepop,
#                                                                                            pctHispFemaleEmployMOE = moeprop(y = totHispFemalepop, moex = HispFemale_employMOE, moey = totHispFemalepopMOE, p = pctHispFemaleEmploy)) %>% pivot_longer(cols = pctHispMaleEmploy:pctHispFemaleEmployMOE, values_to = "val") %>% select(year, val, name)
# 
# employ00<- rbind(employ_test,hispemploy_test)

employ10 <-employ10 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2010,
                                                                               totWhiteMalepop = I9TE003,
                                                                               totWhiteMalepopMOE = I9TM003,
                                                                               totWhiteFemalepop = I9TE016,
                                                                               totWhiteFemalepopMOE = I9TM016,
                                                                               WhiteMaleEmploy = I9TE007 ,
                                                                               WhiteMaleEmployMOE = I9TM007,
                                                                               WhiteFemaleEmploy = I9TE020,
                                                                               WhiteFemaleEmployMOE = I9TM020,
                                                                               totBlackMalepop = I9HE003,
                                                                               totBlackMalepopMOE = I9HM003,
                                                                               totBlackFemalepop = I9HE016,                     
                                                                               totBlackFemalepopMOE = I9HM016,
                                                                               BlackMaleEmploy =  I9HE007,
                                                                               BlackMaleEmployMOE = I9HM007, 
                                                                               BlackFemaleEmploy  =  I9HE020,
                                                                               BlackFemaleEmployMOE = I9HM020,
                                                                               totHispMalepop = I9VE003,
                                                                               totHispMalepopMOE = I9VM003,
                                                                               totHispFemalepop =   I9VE016,
                                                                               totHispFemalepopMOE = I9VM016,
                                                                               HispMaleEmploy = I9VE007,
                                                                               HispMaleEmployMOE = I9VM007,
                                                                               HispFemaleEmploy = I9VE020,
                                                                               HispFemaleEmployMOE = I9VM020,
                                                                               pctWhiteMaleEmploy = WhiteMaleEmploy / totWhiteMalepop,
                                                                               pctWhiteMaleEmployMOE = moeprop(y = totWhiteMalepop, moex = WhiteMaleEmployMOE, moey = totWhiteMalepopMOE, p = pctWhiteMaleEmploy),
                                                                               pctWhiteFemaleEmploy = WhiteFemaleEmploy / totWhiteFemalepop,
                                                                               pctWhiteFemaleEmployMOE = moeprop(y = totWhiteFemalepop, moex = WhiteFemaleEmployMOE, moey = totWhiteFemalepopMOE, p = pctWhiteFemaleEmploy),
                                                                               pctBlackMaleEmploy = BlackMaleEmploy / totBlackMalepop,
                                                                               pctBlackMaleEmployMOE = moeprop(y = totBlackMalepop, moex = BlackMaleEmployMOE, moey = totBlackMalepopMOE, p = pctBlackMaleEmploy),  
                                                                               pctBlackFemaleEmploy =  BlackFemaleEmploy/totBlackFemalepop,
                                                                               pctBlackFemaleEmployMOE = moeprop(y = totBlackFemalepop, moex = BlackFemaleEmployMOE, moey = totBlackFemalepopMOE, p = pctBlackFemaleEmploy),
                                                                               pctHispMaleEmploy = HispMaleEmploy/ totHispMalepop,
                                                                               pctHispMaleEmployMOE = moeprop(y = totHispMalepop, moex = HispMaleEmployMOE, moey = totHispMalepopMOE, p = pctHispMaleEmploy),
                                                                               pctHispFemaleEmploy = HispFemaleEmploy/ totHispFemalepop,
                                                                               pctHispFemaleEmployMOE = moeprop(y = totHispFemalepop, moex = HispFemaleEmployMOE, moey = totHispFemalepopMOE, p = pctHispFemaleEmploy)) %>% pivot_longer(cols = pctWhiteMaleEmploy:pctHispFemaleEmployMOE, values_to = "val") %>% select(year, val, name)

save(employ10, file = "inputs/employ10Raw.RData")


#################################################
# # Jenna's expanded data pull
#################################################

#Median household income, 201* inflation-adjusted dollars

medhhvars <- c('B19013_001E','B19013_001M',
               'B19013B_001E','B19013B_001M',
               'B19013D_001E','B19013D_001M',
               'B19013H_001E','B19013H_001M',
               'B19013I_001E','B19013I_001M')
medhhnames <- c("MedianHHIncome", "MedianHHIncomeMOE",
                "MedianHHIncome_blk", "MedianHHIncomeMOE_blk",
                "MedianHHIncome_asian", "MedianHHIncomeMOE_asian",
                "MedianHHIncome_wht", "MedianHHIncomeMOE_wht",
                "MedianHHIncome_hisp", "MedianHHIncomeMOE_hisp")
medhhRaw_exp <- wholivesdatapull(medhhvars, medhhnames, year = year)
save(medhhRaw_exp, file = "inputs/medhhRaw_exp.RData")

medhh_unadjusted <- read_xlsx("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute("var" = `...1`,
            `1979` = as.character(`1979 (1979$)`),
            `1989`= as.character(`1989 (1989$)`),
            `1999` = as.character(`1999 (1999$)`),
            `2010` = `2010 (2010$)...5`,
            `2010MOE` = `2010 (2010$)...6`) %>% na.omit()
medhh_unadjusted <- medhh_unadjusted %>% select(-c(`2010MOE`)) %>% pivot_longer(cols = `1979`:`2010`, names_to = "Year")
save(medhh_unadjusted, file = "inputs/medhh_unadjusted.RData")
#Bachelor's degree or higher, adults 25 and older

bachvars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
              'C15002_017E','C15002_017M',
              'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
              'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
              'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
              'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
               "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
               "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
               "FemaleBachMOE_blk",
               "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
               "FemaleBachMOE_asian", 
               "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
               "FemaleBachMOE_wht", 
               "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
               "FemaleBachMOE_hisp")
bachRaw_exp <- wholivesdatapull(bachvars, bachnames, year = year)
save(bachRaw_exp, file = "inputs/bachRaw_exp.RData")

#Historial Educational Attainment. 1980-2000 from NHGIS
Bach80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds123_1990_county.csv")
Bach00 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

Bach80 <- Bach80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1980,
            totBach = DHN005 + DHN010 + DHN015 + DHN020 + DHN025,
            totpop = sum(c_across(DHN001:DHN025), na.rm = T),
            WhiteBach = DHN005,
            totWhite = DHN001 + DHN002 + DHN003 + DHN004 + DHN005,
            BlackBach = DHN010,
            totBlack = DHN006 + DHN007 + DHN008 + DHN009 + DHN010,
            HispBach = DHO005,
            totHisp = DHO001 + DHO002 + DHO003 + DHO004 + DHO005,
            pctTotalBach = totBach / totpop,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90 <- Bach90 %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990,
            totBach = E34006 + E34007 + E34013 + E34014 + E34020 + E34021 + E34027 + E34028 + E34034 + E34035,
            totpop = sum(c_across(E34001:E34035), na.rm = T),
            #WhiteBach = E34006 + E34007,
            #totWhite = E34001 + E34002 + E34003 + E34004 + E34005 + E34006 + E34007,
            BlackBach = E34013 + E34014,
            totBlack = E34008 + E34009 + E34010 + E34011 + E34012 + E34013 + E34014,
            HispBach = E35006 + E35007,
            totHisp = E35001 + E35002 + E35003 + E35004 + E35005 + E35006 + E35007,
            pctTotalBach = totBach / totpop,
            #pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90Wht <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0009_ds125_1990_county.csv")
Bach90Wht <- Bach90Wht %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990, 
            WhiteBach = FF5ABR012 + FF5ABR013 + FF5ABR014 + FF5ABR015 + FF5ABR027 + FF5ABR028 + FF5ABR029 + FF5ABR030,
            totWhite = sum(c_across(FF5ABR001:FF5ABR030), na.rm = T),
            pctWhiteBach = WhiteBach / totWhite) %>% 
  pivot_longer(cols = pctWhiteBach, values_to = "val") %>%
  select(year, val, name)

Bach90 <- rbind(Bach90, Bach90Wht)


Bach00Whtvars <- c('P001001', "P148I001", 'P148I008', 'P148I009', 'P148I016', 'P148I017')
Bach00Whtnames <- c("Totalpop", "TotalWhite", "MaleBach", "MaleGradProf", "FemaleBach", "FemaleGradProf")
Bach00Wht <- wholivesdatapull2000(Bach00Whtvars, Bach00Whtnames, error = F) 


### This isn't as clean as the other 2000's data pull but the DF is correct

Bach00Wht <- Bach00Wht %>%
  mutate(WhiteBach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         WhiteBachMOE = moe2000(WhiteBach, Totalpop, designfac = 2.0),
         TotalWhiteMOE = moe2000(TotalWhite, Totalpop, designfac = 2.0), 
         pctWhiteBach = WhiteBach / TotalWhite,
         WhiteBachmoeprop = moeprop(y = TotalWhite, moex = WhiteBachMOE, moey = TotalWhiteMOE, p = pctWhiteBach)) %>%
  filter(place == "071") %>%
  select(pctWhiteBach, WhiteBachmoeprop)



Bach00 <- Bach00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, totpop, designfac = 2.0),
            totpopMOE = moe2000(est = totpop, totpop, designfac = 2.0),
            
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            totWhiteMOE = moe2000(est = totWhite, totpop, designfac = 2.0),
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, totpop, designfac = 2.0),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, totpop, designfac = 2.0),
            
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, totpop, designfac = 2.0),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, totpop, designfac = 2.0),
            pctTotalBach = totBach / totpop,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            #WhiteBachmoeprop = moeprop(totWhite,WhiteBachMOE,totWhiteMOE,pctWhiteBach),
            BlackBachmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            HispBachmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
  )%>% cbind(Bach00Wht) %>%
  pivot_longer(cols = c(pctTotalBach:pctHispBach, pctWhiteBach), values_to = "val") %>% 
  select(year, val, name)


save(Bach00Wht, file = "inputs/Bach00Wht.RData")
save(Bach00, file = "inputs/Bach00Raw.RData")

#pulling ACS1 for 2010 data

# library(tidycensus)
# View(load_variables(year = 2016, dataset = "acs1"))


#Bachelor's degree or higher, adults 25 and older

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
Bach10 <- wholivesdatapull(bachvars10, bachnames10, year = 2010)

Bach10 <- Bach10 %>%
  filter(place == "071") %>% 
  transmute(year = 2010,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

# Pulling ACS1 for 2016 data

#Bachelor's degree or higher, adults 25 and older

bachvars16 <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
                'C15002_017E','C15002_017M',
                'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
                'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
                'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
                'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames16 <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
                 "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
                 "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
                 "FemaleBachMOE_blk",
                 "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
                 "FemaleBachMOE_asian", 
                 "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
                 "FemaleBachMOE_wht", 
                 "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
                 "FemaleBachMOE_hisp")
Bach16<- wholivesdatapull(bachvars16, bachnames16, year = 2016)

Bach16 <- Bach16 %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

#most recent year

bachRaw_exp <- bachRaw_exp %>% ## something's going on here where this is names the same as bachRaw above and so can't be used in the analysis piece as it was before
  filter(place == "071") %>% 
  transmute(year = year,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

#joining the TS data
Bach_TS <- rbind(Bach80, Bach90, Bach00, Bach10, Bach16, bachRaw_exp) %>%
  mutate(var = case_when(name == "pctTotalBach" ~ "All",
                         name == "pctWhiteBach" ~ "White,\nnon-Hispanic",
                         name == "pctBlackBach" ~ "Black",
                         name == "pctHispBach" ~ "Hispanic,\nany race")) %>% select(-name)
write_csv(Bach_TS, "inputs/hist_educationalAttainment.csv")


#Poverty rate, population for whom poverty has been determined

povvars <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
             'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
             'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
             'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
             'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
              "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
              "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
              "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
              "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")
povRaw_exp <- wholivesdatapull(povvars, povnames, year = year)
save(povRaw_exp, file = "inputs/povRaw_exp.RData")

# Creating a time series for total poverty - had to get 1980-2000 from IPUMS NHGIS

pov80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds140_1990_county.csv")
pov00 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")

#filter to Orleans Parish

pov80 <- pov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                           totpov = DI9002 + DI9004 + DI9006 + DI9008 + DI9010,
                                                                           Whitepov = DI9002,
                                                                           Blackpov = DI9004,
                                                                           totpop = sum(c_across(DI9001:DI9010), na.rm = T),
                                                                           totWhitepop = DI9001 + DI9002,
                                                                           totBlackpop= DI9003 + DI9004,
                                                                           pctTotalpov = totpov / totpop,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop) %>% pivot_longer(cols = pctTotalpov:pctBlackpov, values_to = "val") %>% select(year, val, name)
hisppov80 <- hisppov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                                   Hisppov = DJA002,
                                                                                   totHisppop = DJA001 + DJA002,
                                                                                   pctHisppov = Hisppov / totHisppop,
                                                                                   name = "pctHisppov") %>% select(year, val = pctHisppov, name)
pov90 <- pov90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1990,
                                                                           totpov = sum(c_across(EKT011:EKT020), na.rm = T),
                                                                           Whitepov = EKT011,
                                                                           Blackpov = EKT012 + EKT017,
                                                                           Hisppov = EKT016  + EKT018 + EKT019 + EKT020,
                                                                           totpop = sum(c_across(EKT001:EKT020), na.rm = T),
                                                                           totWhitepop = EKT001 + EKT011,
                                                                           totBlackpop = EKT002 + EKT012 + EKT007 + EKT017,
                                                                           totHisppop = EKT006 + EKT008 + EKT009 + EKT010 + EKT016 + EKT018 + EKT019 + EKT020,
                                                                           pctTotalpov = totpov / totpop,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% select(year, val, name)

pov00Whtvars <- c('P001001', 'P148I001', 'P159I002')
pov00Whtnames <- c('TotalPop2000', "TotalWhitepop", "Whitepov")
pov00Wht <- wholivesdatapull2000(pov00Whtvars, pov00Whtnames, error = F) 

pov00Wht <- pov00Wht %>%
  filter(place == "071") %>%
  mutate(pctWhitepov = Whitepov / TotalWhitepop,
         WhitepovMOE = moe2000(Whitepov, TotalPop2000, designfac = 2),
         TotalWhiteMOE = moe2000(TotalWhitepop, TotalPop2000, designfac = 2),
         Whitemoeprop = moeprop(y = TotalWhitepop, moex = WhitepovMOE, moey = TotalWhiteMOE, p = pctWhitepov)) %>% select(pctWhitepov, Whitemoeprop)

save(pov00Wht, file = "inputs/pov00Wht.RData")
pov00 <- pov00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
                                                                           totpov = GTV001 + GTV003 + GTV005 + GTV007 + GTV009 + GTV011 + GTV013,
                                                                           totpop = sum(c_across(GTV001:GTV014),na.rm = T),
                                                                           #Whitepov = GTV001,
                                                                           #totWhitepop = GTV001 + GTV002,
                                                                           Blackpov = GTV003,
                                                                           totBlackpop = GTV003 + GTV004,
                                                                           Hisppov = GTY001,
                                                                           totHisppop = GTY001 + GTY002,
                                                                           pctTotalpov = totpov / totpop,
                                                                           #pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% cbind(pov00Wht) %>%
  pivot_longer(cols = c(pctTotalpov:pctHisppov, pctWhitepov), values_to = "val") %>% select(year, val, name)
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
pov10 <- wholivesdatapull(povvars10, povnames10, year = 2010)

pov10 <- pov10 %>%
  filter(place == "071") %>% 
  transmute(year = 2010,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

povvars16 <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
               'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
               'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
               'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
               'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames16 <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
                "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
                "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
                "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
                "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")

pov16 <- wholivesdatapull(povvars16, povnames16, year = 2016)

pov16 <- pov16 %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

#current year
povraw_exp <- povRaw_exp %>%
  filter(place == "071") %>% 
  transmute(year = year, 
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)


pov_TS <- rbind(pov80, hisppov80, pov90, pov00, pov10, pov16, povraw_exp) %>% mutate(var = case_when(name == "pctTotalpov" ~ "All",
                                                                                                 name == "pctWhitepov" ~ "White,\nnon-Hispanic",
                                                                                                 name == "pctBlackpov" ~ "Black",
                                                                                                 name == "pctHisppov" ~ "Hispanic,\nany race")) %>% select(-name)


write_csv(pov_TS, "inputs/hist_pov.csv")


#Children in poverty, population for whom poverty has been determined	

childpovvars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M',
                  'C17001B_004E','C17001B_004M','C17001B_008E','C17001B_008M','C17001B_013E','C17001B_013M','C17001B_017E','C17001B_017M',
                  'C17001D_004E','C17001D_004M','C17001D_008E','C17001D_008M','C17001D_013E','C17001D_013M','C17001D_017E','C17001D_017M',
                  'C17001H_004E','C17001H_004M','C17001H_008E','C17001H_008M','C17001H_013E','C17001H_013M','C17001H_017E','C17001H_017M',
                  'C17001I_004E','C17001I_004M','C17001I_008E','C17001I_008M','C17001I_013E','C17001I_013M','C17001I_017E','C17001I_017M')
childpovnames <- c("BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", 
                   "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE",
                   "BelowPovMaleChild_blk", "BelowPovMaleChildMOE_blk", "BelowPovFemaleChild_blk", "BelowPovFemaleChildMOE_blk", "AbovePovMaleChild_blk", 
                   "AbovePovMaleChildMOE_blk", "AbovePovFemaleChild_blk", "AbovePovFemaleChildMOE_blk",
                   "BelowPovMaleChild_asian", "BelowPovMaleChildMOE_asian", "BelowPovFemaleChild_asian", "BelowPovFemaleChildMOE_asian", "AbovePovMaleChild_asian", 
                   "AbovePovMaleChildMOE_asian", "AbovePovFemaleChild_asian", "AbovePovFemaleChildMOE_asian",
                   "BelowPovMaleChild_wht", "BelowPovMaleChildMOE_wht", "BelowPovFemaleChild_wht", "BelowPovFemaleChildMOE_wht", "AbovePovMaleChild_wht", 
                   "AbovePovMaleChildMOE_wht", "AbovePovFemaleChild_wht", "AbovePovFemaleChildMOE_wht",
                   "BelowPovMaleChild_hisp", "BelowPovMaleChildMOE_hisp", "BelowPovFemaleChild_hisp", "BelowPovFemaleChildMOE_hisp", "AbovePovMaleChild_hisp", 
                   "AbovePovMaleChildMOE_hisp", "AbovePovFemaleChild_hisp", "AbovePovFemaleChildMOE_hisp")
childpovRaw_exp <- wholivesdatapull(childpovvars, childpovnames, year = year)
save(childpovRaw_exp, file = "inputs/childpovRaw_exp.RData")

#Homeownership rates

hovars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M',
            'B25003B_001E','B25003B_001M','B25003B_002E','B25003B_002M',
            'B25003D_001E','B25003D_001M','B25003D_002E','B25003D_002M',
            'B25003H_001E','B25003H_001M','B25003H_002E','B25003H_002M',
            'B25003I_001E','B25003I_001M','B25003I_002E','B25003I_002M')
honames <- c("Total","TotalMOE","Owner","OwnerMOE",
             "Total_blk","TotalMOE_blk","Owner_blk","OwnerMOE_blk",
             "Total_asian","TotalMOE_asian","Owner_asian","OwnerMOE_asian",
             "Total_wht","TotalMOE_wht","Owner_wht","OwnerMOE_wht",
             "Total_hisp","TotalMOE_hisp","Owner_hisp","OwnerMOE_hisp")
hoRaw_exp <- wholivesdatapull(hovars, honames)
save(hoRaw_exp, file = "inputs/hoRaw_exp.RData")

childPovProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/childPov.csv")
homeownershipProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/homeownership.csv")
educationalAttainmentProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment.csv")
medHHincProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")




############################################
# # PEP # #
############################################

######### Pull data


allparishesRaw <- read_csv("inputs/PEP_data/PEP2023charagegroups.csv")
allparishesRaw <-  allparishesRaw %>% 
  filter(COUNTY %in% c("071","051","075","087","089","093","095")) %>% #making a total column for each sex.  revise unneeded ones later
  mutate(NHWA_Total = NHWA_MALE + NHWA_FEMALE,
         NHBA_Total = NHBA_MALE + NHBA_FEMALE,
         NHAA_Total = NHAA_MALE + NHAA_FEMALE,
         
         H_Total = H_MALE + H_FEMALE,
         HWA_Total = HWA_MALE + HWA_FEMALE,
         HBA_Total = HBA_MALE + HBA_FEMALE,
         HAA_Total = HAA_MALE + HAA_FEMALE) %>%
  pivot_longer(cols = TOT_POP:HAA_Total, names_sep = "_", names_to = c("race", "sex"))%>%
  mutate(place = CTYNAME,
         age = case_when(AGEGRP == 0 ~ "Total",
                         AGEGRP == 1 ~ "Under 5 years",
                         AGEGRP == 2 ~ "5 to 9",
                         AGEGRP == 3 ~ "10 to 14",
                         AGEGRP == 4 ~ "15 to 19",
                         AGEGRP == 5 ~ "20 to 24",
                         AGEGRP == 6 ~ "25 to 29",
                         AGEGRP == 7 ~ "30 to 34",
                         AGEGRP == 8 ~ "35 to 39",
                         AGEGRP == 9 ~ "40 to 44",
                         AGEGRP == 10 ~ "45 to 49",
                         AGEGRP == 11 ~"50 to 54",
                         AGEGRP == 12 ~ "55 to 59",
                         AGEGRP == 13 ~ "60 to 64",
                         AGEGRP == 14 ~ "65 to 69",
                         AGEGRP == 15 ~ "70 to 74",
                         AGEGRP == 16 ~ "75 to 79",
                         AGEGRP == 17 ~ "80 to 84",
                         AGEGRP == 18 ~ "85 plus"),
         date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                          YEAR == 2 ~ "7/1/2020 population estimate",
                          YEAR == 3 ~ "7/1/2021 population estimate",
                          YEAR == 4 ~ "7/1/2022 population estimate",
                          YEAR == 5 ~ "7/1/2023 population estimate"),
         sex = case_when(sex == "POP" | sex == "Total" ~ "Total",
                         sex == "MALE" ~ "Male",
                         sex == "FEMALE" ~ "Female"),
         population = value) %>%
  filter(sex == "Total") %>%
  select(place, date, sex,race, age, population)

allparishesRaw <- allparishesRaw %>% #for some reason this is working only when I keep it separate
  mutate(hisp = case_when(substr(allparishesRaw$race,1,1) == "H" ~ "Hispanic",
                          allparishesRaw$race == "TOT" ~ "Total",
                          substr(allparishesRaw$race,1,1) != "H" ~ "Not Hispanic"))

allparishesRaw <- allparishesRaw %>% filter(race %in% c("TOT", "NHWA", "NHBA", "NHAA", "H")) %>% 
  mutate(race = case_when(race == "TOT" ~ "Total",
                          race == "NHWA" ~ "White alone",
                          race == "NHBA" ~ "Black or African American alone",
                          race == "NHAA" ~ "Asian alone",
                          race == "H" ~ "Hispanic"),
         raceSimple = case_when(race == "Total" & hisp == "Total" & sex == "Total" ~ "Total",
                                race == "White alone" & hisp == "Not Hispanic"  & sex == "Total" ~ "White",
                                race == "Black or African American alone" & hisp == "Not Hispanic" & sex == "Total"  ~ "Black",
                                race == "Asian alone" & hisp == "Not Hispanic" & sex == "Total" ~ "Asian",
                                hisp == "Hispanic" & sex == "Total"~ "Hispanic")) %>%
  select(place, date, hisp, sex, race, age, population, raceSimple)

popunder18co <- read_csv("inputs/PEP_data/PEP2023_agesex.csv") #for popunder18 measure
popunder18co <- popunder18co %>% 
  filter(COUNTY %in% c("071","051","075","087","089","093","095")) %>% select(CTYNAME, YEAR, AGE18PLUS_TOT) %>% mutate(place = CTYNAME,
                                                                                 date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                                                                                                  YEAR == 2 ~ "7/1/2020 population estimate",
                                                                                                  YEAR == 3 ~ "7/1/2021 population estimate",
                                                                                                  YEAR == 4 ~ "7/1/2022 population estimate",
                                                                                                  YEAR == 5 ~ "7/1/2023 population estimate"),
                                                                                 age = "18 years and over",
                                                                                 race = "Total",
                                                                                 raceSimple = "Total",
                                                                                 sex = "Total",
                                                                                 population = AGE18PLUS_TOT)

allparishesRaw <- allparishesRaw %>% full_join(popunder18co, by = c("place", "date", "age", "sex", "race","raceSimple", "population")) %>%
  select(place, date, hisp, sex, race, age, population, raceSimple)
#pulling in entire US PEP data, then binding to allparishesRaw.  Doing this with the exact same code as from above.

allstates_pep <- read_csv("inputs/PEP_data/PEP2023charagegroups_allstates.csv")

allstates_pep <- allstates_pep %>% 
  mutate(NHWA_Total = NHWA_MALE + NHWA_FEMALE,
         NHBA_Total = NHBA_MALE + NHBA_FEMALE,
         NHAA_Total = NHAA_MALE + NHAA_FEMALE,
         
         H_Total = H_MALE + H_FEMALE,
         HWA_Total = HWA_MALE + HWA_FEMALE,
         HBA_Total = HBA_MALE + HBA_FEMALE,
         HAA_Total = HAA_MALE + HAA_FEMALE) %>%
  pivot_longer(cols = TOT_POP:HAA_Total, names_sep = "_", names_to = c("race", "sex"))%>%
  mutate(place = CTYNAME,
         age = case_when(AGEGRP == 0 ~ "Total",
                         AGEGRP == 1 ~ "Under 5 years",
                         AGEGRP == 2 ~ "5 to 9",
                         AGEGRP == 3 ~ "10 to 14",
                         AGEGRP == 4 ~ "15 to 19",
                         AGEGRP == 5 ~ "20 to 24",
                         AGEGRP == 6 ~ "25 to 29",
                         AGEGRP == 7 ~ "30 to 34",
                         AGEGRP == 8 ~ "35 to 39",
                         AGEGRP == 9 ~ "40 to 44",
                         AGEGRP == 10 ~ "45 to 49",
                         AGEGRP == 11 ~"50 to 54",
                         AGEGRP == 12 ~ "55 to 59",
                         AGEGRP == 13 ~ "60 to 64",
                         AGEGRP == 14 ~ "65 to 69",
                         AGEGRP == 15 ~ "70 to 74",
                         AGEGRP == 16 ~ "75 to 79",
                         AGEGRP == 17 ~ "80 to 84",
                         AGEGRP == 18 ~ "85 plus"),
         date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                          YEAR == 2 ~ "7/1/2020 population estimate",
                          YEAR == 3 ~ "7/1/2021 population estimate",
                          YEAR == 4 ~ "7/1/2022 population estimate",
                          YEAR == 5 ~ "7/1/2023 population estimate"),
         sex = case_when(sex == "POP" | sex == "Total" ~ "Total",
                         sex == "MALE" ~ "Male",
                         sex == "FEMALE" ~ "Female"),
         population = value) %>%
  filter(sex == "Total") %>%
  select(place, date, sex,race, age, population)

allstates_pep  <- allstates_pep  %>% #for some reason this is working only when I keep it separate
  mutate(hisp = case_when(substr(allstates_pep$race,1,1) == "H" ~ "Hispanic",
                          allstates_pep$race == "TOT" ~ "Total",
                          substr(allstates_pep$race,1,1) != "H" ~ "Not Hispanic"))

allstates_pep  <- allstates_pep  %>% filter(race %in% c("TOT", "NHWA", "NHBA", "NHAA", "H")) %>% 
  mutate(race = case_when(race == "TOT" ~ "Total",
                          race == "NHWA" ~ "White alone",
                          race == "NHBA" ~ "Black or African American alone",
                          race == "NHAA" ~ "Asian alone",
                          race == "H" ~ "Hispanic"),
         raceSimple = case_when(race == "Total" ~ "Total",
                                race == "White alone" & hisp == "Not Hispanic" & sex == "Total"  ~ "White",
                                race == "Black or African American alone" & hisp == "Not Hispanic" & sex == "Total"  ~ "Black",
                                race == "Asian alone"& hisp == "Not Hispanic" & sex == "Total"   ~ "Asian",
                                hisp == "Hispanic"  & sex == "Total" ~ "Hispanic")) %>%
  select(place, date, hisp, sex, race, age, population, raceSimple)

popunder18US <- read_csv("inputs/PEP_data/PEP2023_agesex_allstates.csv") #for popunder18 measure
popunder18US <- popunder18US %>% select(CTYNAME, YEAR, AGE18PLUS_TOT) %>% mutate(place = CTYNAME,
                                                                                 date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                                                                                                  YEAR == 2 ~ "7/1/2020 population estimate",
                                                                                                  YEAR == 3 ~ "7/1/2021 population estimate",
                                                                                                  YEAR == 4 ~ "7/1/2022 population estimate",
                                                                                                  YEAR == 5 ~ "7/1/2023 population estimate"),
                                                                                 age = "18 years and over",
                                                                                 race = "Total",
                                                                                 raceSimple = "Total",
                                                                                 sex = "Total",
                                                                                 population = AGE18PLUS_TOT)

allstates_pep <- allstates_pep %>% full_join(popunder18US, by = c("place", "date", "sex", "race", "raceSimple", "age", "population"))


allstates_pep <- allstates_pep  %>% group_by(date, hisp, sex, race, age, raceSimple) %>% 
  summarize(place = "United States", 
            population = sum(population)) %>%
  select(place, date, hisp, sex, race, age, population, raceSimple) %>%
  ungroup()

allparishesRaw2020 <- rbind(allstates_pep, allparishesRaw) %>% filter(date == "7/1/2020 population estimate") %>%
  mutate(PlaceName = case_when(place == "Orleans Parish" ~ "Orleans",
                               place =="Jefferson Parish" ~ "Jefferson",
                               place =="Plaquemines Parish" ~ "Plaquemines", 
                               place == "St. Bernard Parish" ~ "St. Bernard",
                               place == "St. Charles Parish" ~ "St. Charles",
                               place == "St. James Parish" ~ "St. James",
                               place == "St. John the Baptist Parish" ~ "St. John the Baptist",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "Metro", "United States")))
save(allparishesRaw2020, file = "inputs/allparishesRaw2020.RData")

allparishesRaw2021 <- rbind(allstates_pep, allparishesRaw) %>% filter(date == "7/1/2021 population estimate") %>%
  mutate(PlaceName = case_when(place == "Orleans Parish" ~ "Orleans",
                               place =="Jefferson Parish" ~ "Jefferson",
                               place =="Plaquemines Parish" ~ "Plaquemines", 
                               place == "St. Bernard Parish" ~ "St. Bernard",
                               place == "St. Charles Parish" ~ "St. Charles",
                               place == "St. James Parish" ~ "St. James",
                               place == "St. John the Baptist Parish" ~ "St. John the Baptist",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "Metro", "United States")))
save(allparishesRaw2021, file = "inputs/allparishesRaw2021.RData")

allparishesRaw2022 <- rbind(allstates_pep, allparishesRaw) %>% filter(date == "7/1/2022 population estimate") %>%
  mutate(PlaceName = case_when(place == "Orleans Parish" ~ "Orleans",
                               place =="Jefferson Parish" ~ "Jefferson",
                               place =="Plaquemines Parish" ~ "Plaquemines", 
                               place == "St. Bernard Parish" ~ "St. Bernard",
                               place == "St. Charles Parish" ~ "St. Charles",
                               place == "St. James Parish" ~ "St. James",
                               place == "St. John the Baptist Parish" ~ "St. John the Baptist",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "Metro", "United States")))
save(allparishesRaw2022, file = "inputs/allparishesRaw2022.RData")

allparishesRaw2023 <- rbind(allstates_pep, allparishesRaw) %>% filter(date == "7/1/2023 population estimate") %>%
  mutate(PlaceName = case_when(place == "Orleans Parish" ~ "Orleans",
                               place =="Jefferson Parish" ~ "Jefferson",
                               place =="Plaquemines Parish" ~ "Plaquemines", 
                               place == "St. Bernard Parish" ~ "St. Bernard",
                               place == "St. Charles Parish" ~ "St. Charles",
                               place == "St. James Parish" ~ "St. James",
                               place == "St. John the Baptist Parish" ~ "St. John the Baptist",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "Metro", "United States")))
save(allparishesRaw2023, file = "inputs/allparishesRaw2023.RData")


#### Pulling PEP

## this is just for the 2010 inline measures - probably could be done differently but for now..
popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP", "RACE", "SEX", "POP") #added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP", "HISP", "RACE", "SEX", "POP")

allparishes_retro <- getCensus(name = "pep/charagegroups", # most recent
                               vintage = 2019,
                               key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                               vars = popestVars,
                               region = "county: 071,051,075,087,089,093,095,103",
                               regionin = "state:22") %>%
  rename(DATE = DATE_CODE) %>%
  bind_rows(getCensus(name = "pep/int_charagegroups", # Intercensal estimates
                      vintage = 2000,
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                      vars = popestVars2000,
                      region = "county:071,051,075,087,089,093,095",
                      regionin = "state:22")) %>%
  mutate(place = case_when(county == "071" ~ "Orleans",
                           county == "051" ~ "Jefferson",
                           county == "075" ~ "Plaquemines",
                           county == "087" ~ "St. Bernard",
                           county == "089" ~ "St. Charles",
                           county == "093" ~ "St. James",
                           county == "095" ~ "St. John the Baptist"),
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEO_ID, -DATE_, -DATE, -GEONAME) %>%
  filter(HISP == 0) %>%
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year ==2013 | year == 2014 | year == 2015 | year == 2016 | year ==2017 | year ==2018 | year == 2019)
allparishes_retro <- allparishes_retro %>% transmute(PlaceName = place,
                                                     date = DATE_DESC,
                                                     sex = case_when(SEX == 0 ~ "Total"),
                                                     race = case_when(RACE == 0 ~ "Total"),
                                                     hisp = case_when(HISP == 0 ~ "Total"),
                                                     raceSimple = case_when(RACE == 0 ~ "Total"),
                                                     population = POP) %>% filter(sex == "Total" & race == "Total" & hisp == "Total")

save(allparishes_retro, file = "inputs/allparishes_retro.RData")



popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP") #added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP")

hisppopestRaw <- getCensus(name = "pep/charagegroups", # most recent
                               vintage = 2019,
                               key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                               vars = popestVars,
                               region = "county: 071,051,075,087,089,093,095",
                               regionin = "state:22") %>%
                    rename(DATE = DATE_CODE) %>%
  bind_rows(getCensus(name = "pep/int_charagegroups", # Intercensal estimates
                      vintage = 2000,
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                      vars = popestVars2000,
                      region = "county:071,051,075,087,089,093,095",
                      regionin = "state:22")) %>%
  mutate(place = case_when(county == "071" ~ "Orleans",
                           county == "051" ~ "Jefferson",
                           county == "075" ~ "Plaquemines",
                           county == "087" ~ "St. Bernard",
                           county == "089" ~ "St. Charles",
                           county == "093" ~ "St. James",
                           county == "095" ~ "St. John the Baptist"),
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEO_ID, -DATE_, -DATE, -GEONAME, -state, -county) %>%
  filter(HISP == 2) %>%
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year ==2013 | year == 2014 | year == 2015 | year == 2016 | year ==2017 | year ==2018 | year == 2019)

hisppopest20 <- allparishesRaw2020 %>% filter(race == "Hispanic"  & age == "Total" & sex == "Total") %>% mutate(place = PlaceName) %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% mutate(year = 2020) %>% select(POP, DATE_DESC, HISP, place, year)
hisppopest23 <- allparishesRaw2023 %>% filter(race == "Hispanic"  & age == "Total" & sex == "Total") %>% mutate(place = PlaceName) %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place)  %>% mutate(year = 2023) %>% select(POP, DATE_DESC, HISP, place, year)
hisppopestRaw <- rbind(hisppopestRaw, hisppopest20, hisppopest23)

save(hisppopestRaw, file = "inputs/hisppopestRaw.RData")
write_csv(hisppopestRaw %>% arrange(place, year), file = "inputs/pep23_hispanic.csv")


popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP", "RACE")
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP")

blackpopestRaw <- getCensus(name = "pep/charagegroups", # most recent
                                vintage = 2019,
                                key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                                vars = popestVars,
                                region = "county: 071",
                                regionin = "state:22") %>%
                     rename(DATE = DATE_CODE) %>%
  bind_rows(getCensus(name = "pep/int_charagegroups", # Intercensal estimates
                      vintage = 2000,
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                      vars = popestVars2000,
                      region = "county:071",
                      regionin = "state:22")) %>%
  mutate(place = "Orleans",
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEONAME, -DATE) %>%
  filter(HISP == 1) %>%
  filter(RACE == 2) %>%
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year ==2013 | year == 2014 | year == 2015 | year == 2016 | year ==2017| year ==2018 | year ==2019 ) %>%
  select(-GEO_ID, -DATE_)

blackpopest20 <- allparishesRaw2020 %>% filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% mutate(state = 22, county = 071, year = 2020) %>% select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest21 <- allparishesRaw2022 %>% filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>% mutate(state = 22, county = 071, year = 2022) %>% select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest22 <- allparishesRaw2022 %>% filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>% mutate(state = 22, county = 071, year = 2022) %>% select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest23 <- allparishesRaw2023 %>% filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>% mutate(state = 22, county = 071, year = 2023) %>% select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopestRaw <- rbind(blackpopestRaw, blackpopest20, blackpopest21, blackpopest22, blackpopest23)

save(blackpopestRaw, file = "inputs/blackpopestRaw.RData")


###### HT adding new code to get all races retrospectively July 2024

popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP", "RACE")
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP", "RACE")

Historic_popestRaw <- getCensus(name = "pep/charagegroups", # most recent
                            vintage = 2019,
                            key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                            vars = popestVars,
                            region = "county:071,051,075,087,089,093,095",
                            regionin = "state:22") %>%
  rename(DATE = DATE_CODE) %>%
  bind_rows(getCensus(name = "pep/int_charagegroups", # Intercensal estimates
                      vintage = 2000,
                      key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                      vars = popestVars2000,
                      region = "county:071,051,075,087,089,093,095,103",
                      regionin = "state:22"))  %>%
  mutate(place = case_when(county == "071" ~ "Orleans",
                           county == "051" ~ "Jefferson",
                           county == "075" ~ "Plaquemines",
                           county == "087" ~ "St. Bernard",
                           county == "089" ~ "St. Charles",
                           county == "093" ~ "St. James",
                           county == "095" ~ "St. John the Baptist")) %>%
  mutate(year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEONAME, -DATE) %>%
  mutate(race = factor(case_when(RACE == 0 & HISP == 0 ~ "Total",
                          RACE == 1  & HISP == 1 ~ "White",
                          RACE == 2 & HISP == 1 ~ "Black",
                          RACE == 4 & HISP == 1 ~ "Asian",
                          HISP == 2 & RACE == 0 ~ "Hispanic"), levels = c("White", "Black", "Hispanic", "Asian"))) %>%
  filter((DATE_DESC == "4/1/2010 Census population" | DATE_DESC == "4/1/2000 population estimates base") &
           race %in% c("White", "Black", "Hispanic", "Asian")) %>%
  select(-GEO_ID, -DATE_) %>%
  mutate(state = "",
         county = "") %>%
  select(state, county, POP, DATE_DESC, race, place, year)

Racepopest20 <- allparishesRaw2020 %>%
  filter(age == "Total" & sex == "Total" & raceSimple != "Total") %>%
  mutate(place = PlaceName) %>% 
  select(POP = population, DATE_DESC = date, race = raceSimple, place) %>%
  mutate(state = "",
         county = "",
         year = 2020) %>%
  select(state, county, POP, DATE_DESC, race, place, year)

Racepopest23 <- allparishesRaw2023 %>%
  filter(age == "Total" & sex == "Total" & raceSimple != "Total") %>%
  mutate(place = PlaceName) %>%
  select(POP = population, DATE_DESC = date, race = raceSimple, place)  %>%
  mutate(state = "", county = "", year = year) %>%
  select(state, county, POP, DATE_DESC, race, place, year)


RacepopestRaw <- rbind(Historic_popestRaw, Racepopest20, Racepopest23) %>%
  filter(place != "United States") %>%
  group_by(year, race, DATE_DESC) %>%
  summarize(POP = sum(POP)) %>%
  mutate(place = "Metro")

save(RacepopestRaw, file = "inputs/RacepopestRaw.RData")

