############################################
# # PEP # #
############################################


### 2021 PEP data not available by API yet, pulling it in directly.

allparishesRaw <- read_csv("PEPdata/PEP2021charagegroups.csv")
allparishesRaw <-  allparishesRaw %>% 
  filter(COUNTY %in% c("071","051","075","087","089","093","095","103")) %>% #making a total column for each sex.  revise unneeded ones later
  mutate(NHWA_Total = NHWA_MALE + NHWA_FEMALE, #ALERT ALERT MAKE SURE NH AT BEGINNING FOR NONHISPANIC OF ALL RACES!!!!! 
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
                          YEAR == 3 ~ "7/1/2021 population estimate"),
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

popunder18co <- read_csv("PEPdata/county_pep_age2021.csv") #for popunder18 measure
popunder18co <- popunder18co %>% 
  filter(COUNTY %in% c("071","051","075","087","089","093","095","103"))  %>% 
  select(CTYNAME, YEAR, AGE18PLUS_TOT) %>%
  mutate(place = CTYNAME,
         date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                          YEAR == 2 ~ "7/1/2020 population estimate",
                          YEAR == 3 ~ "7/1/2021 population estimate"),
         age = "18 years and over",
         race = "Total",
         raceSimple = "Total",
         sex = "Total",
         population = AGE18PLUS_TOT)

allparishesRaw <- allparishesRaw %>% full_join(popunder18co, by = c("place", "date", "age", "sex", "race","raceSimple", "population")) %>%
  select(place, date, hisp, sex, race, age, population, raceSimple)


#pulling in entire US PEP data, then binding to allparishesRaw.  Doing this with the exact same code as from above.
allstates_pep <- read_csv("PEPdata/PEP2021charagegroups_allstates.csv")
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
                          YEAR == 3 ~ "7/1/2021 population estimate"),
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

popunder18US <- read_csv("PEPdata/US_pep_age2021.csv") #for popunder18 measure
popunder18US <- popunder18US %>% select(CTYNAME, YEAR, AGE18PLUS_TOT) %>% 
  mutate(place = CTYNAME,
         date = case_when(YEAR == 1 ~ "4/1/2020 population estimates base",
                          YEAR == 2 ~ "7/1/2020 population estimate",
                          YEAR == 3 ~ "7/1/2021 population estimate"),
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
                               place == "St. Tammany Parish" ~ "St. Tammany",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "St. Tammany", "Metro", "United States")))
save(allparishesRaw2020, file = "inputs/allparishesRaw2020.RData")

allparishesRaw <- rbind(allstates_pep, allparishesRaw) %>% filter(date == "7/1/2021 population estimate") %>%
  mutate(PlaceName = case_when(place == "Orleans Parish" ~ "Orleans",
                               place =="Jefferson Parish" ~ "Jefferson",
                               place =="Plaquemines Parish" ~ "Plaquemines", 
                               place == "St. Bernard Parish" ~ "St. Bernard",
                               place == "St. Charles Parish" ~ "St. Charles",
                               place == "St. James Parish" ~ "St. James",
                               place == "St. John the Baptist Parish" ~ "St. John the Baptist",
                               place == "St. Tammany Parish" ~ "St. Tammany",
                               place == "United States" ~ "United States"),
         PlaceName = factor(PlaceName, levels = c("Orleans", "Jefferson", "Plaquemines",
                                                  "St. Bernard","St. Charles", "St. James",
                                                  "St. John the Baptist", "St. Tammany", "Metro", "United States")))
save(allparishesRaw, file = "inputs/allparishesRaw.RData")


#### Pulling PEP

## this is just for the 2010 inline measures - probably could be done differently but for now..
popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP", "RACE", "SEX", "POP") #added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP", "HISP", "RACE", "SEX", "POP")
allparishes_hist <- getCensus(name = "pep/charagegroups", # most recent
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
                      region = "county:071,051,075,087,089,093,095,103",
                      regionin = "state:22")) %>%
  mutate(place = case_when(county == "071" ~ "Orleans",
                           county == "051" ~ "Jefferson",
                           county == "075" ~ "Plaquemines",
                           county == "087" ~ "St. Bernard",
                           county == "089" ~ "St. Charles",
                           county == "093" ~ "St. James",
                           county == "095" ~ "St. John the Baptist",
                           county == "103" ~ "St. Tammany"),
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEO_ID, -DATE_, -DATE, -GEONAME) %>%
  filter(HISP == 0) %>%
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year ==2013 | year == 2014 | year == 2015 | year == 2016 | year ==2017 | year ==2018 | year == 2019)
allparishes_hist <- allparishes_hist %>% transmute(PlaceName = place,
                                                   date = DATE_DESC,
                                                   sex = case_when(SEX == 0 ~ "Total"),
                                                   race = case_when(RACE == 0 ~ "Total"),
                                                   hisp = case_when(HISP == 0 ~ "Total"),
                                                   raceSimple = case_when(RACE == 0 ~ "Total"),
                                                   population = POP) %>% filter(sex == "Total" & race == "Total" & hisp == "Total")

save(allparishes_hist, file = "inputs/allparishes_hist.RData")

popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEO_ID", "HISP") #added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP")

hisppopestRaw <- getCensus(name = "pep/charagegroups", # most recent
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
                      region = "county:071,051,075,087,089,093,095,103",
                      regionin = "state:22")) %>%
  mutate(place = case_when(county == "071" ~ "Orleans",
                           county == "051" ~ "Jefferson",
                           county == "075" ~ "Plaquemines",
                           county == "087" ~ "St. Bernard",
                           county == "089" ~ "St. Charles",
                           county == "093" ~ "St. James",
                           county == "095" ~ "St. John the Baptist",
                           county == "103" ~ "St. Tammany"),
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  select(-GEO_ID, -DATE_, -DATE, -GEONAME) %>%
  filter(HISP == 2) %>%
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year ==2013 | year == 2014 | year == 2015 | year == 2016 | year ==2017 | year ==2018 | year == 2019)

hisppopest20 <- allparishesRaw2020 %>% filter(race == "Hispanic"  & age == "Total" & sex == "Total") %>% mutate(place = PlaceName) %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% mutate(state = "", county = "", year = 2020) %>% select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopest21 <- allparishesRaw %>% filter(race == "Hispanic"  & age == "Total" & sex == "Total") %>% mutate(place = PlaceName) %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place)  %>% mutate(state = "", county = "", year = 2021) %>% select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopestRaw <- rbind(hisppopestRaw, hisppopest20, hisppopest21)
save(hisppopestRaw, file = "inputs/hisppopestRaw.RData")


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
blackpopest21 <- allparishesRaw %>% filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>% select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>% filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>% mutate(state = 22, county = 071, year = 2021) %>% select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopestRaw <- rbind(blackpopestRaw, blackpopest20, blackpopest21)

save(blackpopestRaw, file = "inputs/blackpopestRaw.RData")


############################################
# # ACS # #
############################################


#Hispanic Origin

hispanvars <-c("B03001_001E","B03001_001M","B03001_002E","B03001_002M","B03001_003E","B03001_003M","B03001_004E","B03001_004M","B03001_005E","B03001_005M","B03001_006E","B03001_006M","B03001_007E","B03001_007M","B03001_008E","B03001_008M","B03001_009E","B03001_009M","B03001_010E","B03001_010M","B03001_011E","B03001_011M","B03001_012E","B03001_012M","B03001_013E","B03001_013M","B03001_014E","B03001_014M","B03001_015E","B03001_015M","B03001_016E","B03001_016M","B03001_027E","B03001_027M")
hispannames <-c("Total","TotalMOE","TotalNotHIsporLat","TotalNotHIsporLatMOE","TotalHisporLat","TotalHisporLatMOE","TotMex","TotMexMOE","TotPR","TotPRMOE","TotCuba","TotCubaMOE","TotDomin","TotDominMOE","TotCentrAm","TotCentrAmMOE","TotCostaR","TotCostaRMOE","TotGuat","TotGuatMOE","TotHond","TotHondMOE","TotNicarag","TotNicaragMOE","TotPanama","TotPanamaMOE","TotSalva","TotSalvaMOE","TotOtherCA","TotOtherCAMOE","TotSA","TotSAMOE","TotOtherHisporLat","TotOtherHisporLatMOE")
hispanRaw <- wholivesdatapull(hispanvars,hispannames)[-3,]
save(hispanRaw, file = "inputs/hispanRaw.RData") # -3 removes St. Tammany because it is not included in this analysis


#Households with own children under 18
### *** something went wrong here pulling from the warehouse!!!!!! ***
hwcvars <- c('B11001_001E','B11001_001M','B11003_003E','B11003_003M','B11003_010E','B11003_010M','B11003_016E','B11003_016M')
hwcnames <- c("TotalHH", "TotalHHMOE","Married", "MarriedMOE", "MaleHH", "MaleHHMOE", "FemaleHH" ,"FemaleHHMOE")
hwcRaw <- wholivesdatapull(hwcvars, hwcnames)
save(hwcRaw, file = "inputs/hwcRaw.RData")

#One-person households

singvars <- c('B11001_001E','B11001_001M','B11001_008E','B11001_008M')
singnames <- c("TotalHH","TotalMOE","SingleHH","SingleHHMOE")
singRaw <- wholivesdatapull(singvars, singnames)
save(singRaw, file = "inputs/singRaw.RData")

#Bachelor's degree or higher, adults 25 and older

bachvars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M','C15002_017E','C15002_017M')
bachnames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE")
bachRaw <- wholivesdatapull(bachvars, bachnames)
save(bachRaw, file = "inputs/bachRaw.RData")

##Bach time series was made with NHGIS and 2010 census data in "hist_data" folder. Pull most recent year.

bachracevars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
              'C15002_017E','C15002_017M',
              'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
              'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
              'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
              'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachracenames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
               "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
               "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
               "FemaleBachMOE_blk",
               "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
               "FemaleBachMOE_asian", 
               "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
               "FemaleBachMOE_wht", 
               "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
               "FemaleBachMOE_hisp")
bachraceRaw <- wholivesdatapull(bachracevars, bachracenames)
save(bachraceRaw, file = "inputs/bachraceRaw.RData")

#Less than a high school degree, adults 25 and older

hsvars <- c('C15002_001E','C15002_001M','C15002_003E','C15002_003M','C15002_004E','C15002_004M','C15002_011E','C15002_011M','C15002_012E','C15002_012M')
hsnames <- c("Total", "TotalMOE", "Male9", "Male9MOE", "Male9to12", "Male9to12MOE", "Female9", "Female9MOE", "Female9to12", "Female9to12MOE")
hsRaw <- wholivesdatapull(hsvars, hsnames)
save(hsRaw, file = "inputs/hsRaw.RData")

#Median household income, inflation-adjusted dollars

medhhvars <- c('B19013_001E','B19013_001M')
medhhnames <- c("MedianHHIncome", "MedianHHIncomeMOE")
medhhRaw <- wholivesdatapull(medhhvars, medhhnames)
save(medhhRaw, file = "inputs/medhhRaw.RData")

#Median HH Income - 2000 data pull (data is from 1999)

#have to make MOEs slightly differently because it is a household universe.
medhhvars2000 <- c('H002001', 'H003001','HCT012001')
medhhnames2000 <- c("SampHousingUnits2000","TotHousingUnits2000", "MedianHHIncome")
medhhRaw2000 <- wholivesdatapull(medhhvars2000, medhhnames2000, censusname = "dec/sf3", year = 2000)
medhhRaw2000 <- medhhRaw2000 %>% mutate(PctinSamp = SampHousingUnits2000 / TotHousingUnits2000,
                                        MedhianHHIncomeMOE = moe2000(MedianHHIncome, TotHousingUnits2000, designfac = 1.2))
save(medhhRaw2000, file = "inputs/medhhRaw2000.RData")

##Med HH Inc by Race time series was made with Prosperity Index data in "hist_data" folder. Pull most recent year.

medhhracevars <- c('B19013_001E','B19013_001M',
               'B19013B_001E','B19013B_001M',
               'B19013D_001E','B19013D_001M',
               'B19013H_001E','B19013H_001M',
               'B19013I_001E','B19013I_001M')
medhhracenames <- c("MedianHHIncome", "MedianHHIncomeMOE",
                "MedianHHIncome_blk", "MedianHHIncomeMOE_blk",
                "MedianHHIncome_asian", "MedianHHIncomeMOE_asian",
                "MedianHHIncome_wht", "MedianHHIncomeMOE_wht",
                "MedianHHIncome_hisp", "MedianHHIncomeMOE_hisp")
medhhraceRaw <- wholivesdatapull(medhhracevars, medhhracenames)
save(medhhraceRaw, file = "inputs/medhhraceRaw.RData")


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


#Poverty rate by race

povracevars <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
             'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
             'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
             'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
             'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povracenames <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
              "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
              "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
              "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
              "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")
povraceRaw <- wholivesdatapull(povracevars, povracenames)
save(povraceRaw, file = "inputs/povraceRaw.RData")

#Children in poverty, population for whom poverty has been determined	

childpovvars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M')
childpovnames <- c( "BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE")
childpovRaw <- wholivesdatapull(childpovvars, childpovnames)
save(childpovRaw, file = "inputs/childpovRaw.RData")

#children in poverty by race

childpovracevars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M',
                  'C17001B_004E','C17001B_004M','C17001B_008E','C17001B_008M','C17001B_013E','C17001B_013M','C17001B_017E','C17001B_017M',
                  'C17001D_004E','C17001D_004M','C17001D_008E','C17001D_008M','C17001D_013E','C17001D_013M','C17001D_017E','C17001D_017M',
                  'C17001H_004E','C17001H_004M','C17001H_008E','C17001H_008M','C17001H_013E','C17001H_013M','C17001H_017E','C17001H_017M',
                  'C17001I_004E','C17001I_004M','C17001I_008E','C17001I_008M','C17001I_013E','C17001I_013M','C17001I_017E','C17001I_017M')
childpovracenames <- c("BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", 
                   "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE",
                   "BelowPovMaleChild_blk", "BelowPovMaleChildMOE_blk", "BelowPovFemaleChild_blk", "BelowPovFemaleChildMOE_blk", "AbovePovMaleChild_blk", 
                   "AbovePovMaleChildMOE_blk", "AbovePovFemaleChild_blk", "AbovePovFemaleChildMOE_blk",
                   "BelowPovMaleChild_asian", "BelowPovMaleChildMOE_asian", "BelowPovFemaleChild_asian", "BelowPovFemaleChildMOE_asian", "AbovePovMaleChild_asian", 
                   "AbovePovMaleChildMOE_asian", "AbovePovFemaleChild_asian", "AbovePovFemaleChildMOE_asian",
                   "BelowPovMaleChild_wht", "BelowPovMaleChildMOE_wht", "BelowPovFemaleChild_wht", "BelowPovFemaleChildMOE_wht", "AbovePovMaleChild_wht", 
                   "AbovePovMaleChildMOE_wht", "AbovePovFemaleChild_wht", "AbovePovFemaleChildMOE_wht",
                   "BelowPovMaleChild_hisp", "BelowPovMaleChildMOE_hisp", "BelowPovFemaleChild_hisp", "BelowPovFemaleChildMOE_hisp", "AbovePovMaleChild_hisp", 
                   "AbovePovMaleChildMOE_hisp", "AbovePovFemaleChild_hisp", "AbovePovFemaleChildMOE_hisp")
childpovraceRaw <- wholivesdatapull(childpovracevars, childpovracenames)
save(childpovraceRaw, file = "inputs/childpovraceRaw.RData")

#Households without access to a vehicle

vehvars <- c('B08201_001E','B08201_001M','B08201_002E','B08201_002M')
vehnames <- c("Total","TotalMOE","NoVehAvail","NoVehAvailMOE")
vehRaw <- wholivesdatapull(vehvars, vehnames)
save(vehRaw, file = "inputs/vehRaw.RData")

#Population not U.S. citizens at birth

forborvars <- c('C05005_004E','C05005_004M','C05005_007E','C05005_007M','C05005_010E','C05005_010M','C05005_013E','C05005_013M','B01003_001E','B01003_001M')
forbornames <- c("TotForeign10on","TotForeign10onMOE","TotForeign00to09","TotForeign00to09MOE","TotForeign90to99","TotForeign90to99MOE","TotForeignPre90","TotForeignPre90MOE","TotalPop","TotalPopMOE")
forborRaw <- wholivesdatapull(forborvars, forbornames)
save(forborRaw, file = "inputs/forborRaw.RData")


#Population who moved in the past year

mobvars <- c('B07003_001E','B07003_001M','B07003_004E','B07003_004M','B07003_007E','B07003_007M','B07003_010E','B07003_010M','B07003_013E','B07003_013M','B07003_016E','B07003_016M')
mobnames <- c("Total","TotalMOE","TotSameHouse","TotSameHouseMOE","TotMovedinCty","TotMovedinCtyMOE","TotMovedinState","TotMovedinStateMOE","TotMovedbtwnStates","TotMovedbtwnStatesMOE","TotMovedfromAbroad","TotMovedfromAbroadMOE")
mobRaw <- wholivesdatapull(mobvars, mobnames)
save(mobRaw, file = "inputs/mobRaw.RData")

#2004 data created in hist_data folder. 2004 mobility data from ACS 2004.
#join this together in analysis.R

#Homeownership rates

hovars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M')
honames <- c("Total","TotalMOE","Owner","OwnerMOE")
hoRaw <- wholivesdatapull(hovars, honames)
save(hoRaw, file = "inputs/hoRaw.RData")

hovars2000 <- c('H004001', 'H004002')
honames2000 <- c("Total2000", "Owner2000")
hoRaw2000 <- wholivesdatapull(hovars2000, honames2000, censusname = "dec/sf1", year = 2000) %>% select(-place)
save(hoRaw2000, file = "inputs/hoRaw2000.RData")

#Homeownership rates by race

horacevars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M',
            'B25003B_001E','B25003B_001M','B25003B_002E','B25003B_002M',
            'B25003D_001E','B25003D_001M','B25003D_002E','B25003D_002M',
            'B25003H_001E','B25003H_001M','B25003H_002E','B25003H_002M',
            'B25003I_001E','B25003I_001M','B25003I_002E','B25003I_002M')
horacenames <- c("Total","TotalMOE","Owner","OwnerMOE",
             "Total_blk","TotalMOE_blk","Owner_blk","OwnerMOE_blk",
             "Total_asian","TotalMOE_asian","Owner_asian","OwnerMOE_asian",
             "Total_wht","TotalMOE_wht","Owner_wht","OwnerMOE_wht",
             "Total_hisp","TotalMOE_hisp","Owner_hisp","OwnerMOE_hisp")
horaceRaw <- wholivesdatapull(horacevars, horacenames)
save(horaceRaw, file = "inputs/horaceRaw.RData")

#Homeowners without a mortgage

honomovars <- c('B25081_001E','B25081_001M','B25081_002E','B25081_002M','B25081_009E','B25081_009M')
honomonames <- c("Total","TotalMOE","Mortgage","MortgageMOE","NoMortgage","NoMortgageMOE")
honomoRaw <- wholivesdatapull(honomovars, honomonames)
save(honomoRaw, file = "inputs/honomoRaw.RData")


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

# for 2004 data + SEs, ACS 2004 data manipulated in hist_data.

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

# 2000 - means of transportation to work, 16+
#P030

commutevars2000 <- c('P003001','P030001', 'P030003','P030004','P030005','P030011','P030012','P030013','P030014','P030015','P030016')
commutenames2000 <- c("Totalpop2000","Total2000","DroveAlone2000","Carpool2000","PublicTransit2000","Taxi2000","Motorcycle2000","Bike2000","Walk2000","Other2000","Workhome2000")
commuteRaw2000 <- wholivesdatapull(commutevars2000, commutenames2000, year = 2000, censusname = "dec/sf3")

#Design factor for this variable is 1.3 for parishes and metro, 1.1 for US
commuteRaw2000 <- commuteRaw2000 %>%
  mutate(TotalMOE2000 = moe2000(est = Total2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         DroveAloneMOE2000 = moe2000(est = DroveAlone2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         CarpoolMOE2000 = moe2000(est = Carpool2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         PublicTransitMOE2000 = moe2000(est = PublicTransit2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         TaxiMOE2000 = moe2000(est = Taxi2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         MotorcycleMOE2000 = moe2000(est = Motorcycle2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         BikeMOE2000 = moe2000(est = Bike2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)), 
         WalkMOE2000 = moe2000(est = Walk2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         OtherMOE2000 = moe2000(est = Other2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         WorkhomeMOE2000 = moe2000(est = Workhome2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)))
save(commuteRaw2000, file = "inputs/commuteRaw2000.RData")
