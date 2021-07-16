


##Over 18 var
##Had to get from different survey because the pep sex age race did not include an age grouping that allowed for 18 and over

PEPover18 <- PEPover18raw %>% 
  mutate(PlaceName = sub(' Parish', "", CTYNAME)) %>% 
  mutate(WhoLivesYear = 2020) %>% 
  left_join(PEPyear2020crosswalk, by = "YEAR") %>% 
  select(WhoLivesYear, PlaceName, DateDesc, AGE18PLUS_TOT) %>% 
  rename(Population = AGE18PLUS_TOT) %>% 
  mutate(AgeGroupName = "18 years and over") %>% 
  mutate(Variable = "AGE18PLUS_TOT") %>% 
  mutate(HispName = "Total",
         RaceName = "Total",
         RaceSimple = "Total",
         SexName = "Total")



### US time


uspep <- PEPUS2020raw %>% 
  filter(MONTH == 7) %>% 
  filter(UNIVERSE == "R") %>% 
  filter(AGE == 999 ) %>% 
  mutate(AgeGroupName = "Total") %>% 
  mutate(WhoLivesYear = 2020) %>% 
  mutate(PlaceName = "United States") %>% 
  mutate(DateDesc = "7/1/2020 population estimate") %>% 
  select(WhoLivesYear, PlaceName, DateDesc, AgeGroupName, TOT_POP, NHWA_MALE, NHWA_FEMALE, NHBA_MALE, NHBA_FEMALE, NHAA_MALE, NHAA_FEMALE, H_MALE, H_FEMALE) %>% 
  mutate(NHWA_TOTAL = NHWA_FEMALE + NHWA_MALE,
         NHBA_TOTAL = NHBA_FEMALE + NHBA_MALE,
         NHAA_TOTAL = NHAA_FEMALE + NHAA_MALE,
         H_TOTAL = H_FEMALE + H_MALE) %>% 
  pivot_longer(-c(WhoLivesYear, PlaceName, DateDesc, AgeGroupName), names_to = "Variable", values_to = "Population") %>% 
  left_join( PEPsexrace2020crosswalk, by = "Variable")




##Parishes


allparishesRaw <- PEP2020raw %>% 
  mutate(PlaceName = sub(' Parish', "", CTYNAME)) %>% 
  mutate(WhoLivesYear = 2020) %>% 
  left_join(PEPage2020crosswalk, by = "AGEGRP") %>% 
  left_join(PEPyear2020crosswalk, by = "YEAR") %>% 
  select(WhoLivesYear, PlaceName, DateDesc, AgeGroupName, TOT_POP, NHWA_MALE, NHWA_FEMALE, NHBA_MALE, NHBA_FEMALE, NHAA_MALE, NHAA_FEMALE, H_MALE, H_FEMALE) %>% 
  mutate(NHWA_TOTAL = NHWA_FEMALE + NHWA_MALE,
         NHBA_TOTAL = NHBA_FEMALE + NHBA_MALE,
         NHAA_TOTAL = NHAA_FEMALE + NHAA_MALE,
         H_TOTAL = H_FEMALE + H_MALE) %>% 
  pivot_longer(-c(WhoLivesYear, PlaceName, DateDesc, AgeGroupName), names_to = "Variable", values_to = "Population") %>% 
  left_join( PEPsexrace2020crosswalk, by = "Variable") %>% 
  bind_rows(uspep) %>% 
  bind_rows(PEPover18)



  ###Need inter year hisp values








hisppopestRaw <- allparishesRaw %>% 
  filter(AgeGroupName == "Total" ) %>% 
  filter(HispName == "Hispanic") %>% 
  filter(SexName == 'Total') %>% 
  mutate(CensusYear = str_sub(DateDesc, 5, 8)) %>% # Clean July 1 from every year
  select(WhoLivesYear, PlaceName, DateDesc, CensusYear, HispName, Population) %>% 
  filter(DateDesc != "4/1/2010 population estimates base") %>% 
  filter(DateDesc != "7/1/2010 population estimate") %>% 
  bind_rows(hisppopestINTRaw) %>% 
  mutate(CensusYear = as.numeric(CensusYear))

