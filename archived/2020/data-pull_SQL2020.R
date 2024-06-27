############################################
# # ACS # #
############################################

IDS <- odbcConnect("DC2 IDS", uid = "rweinstein", pwd = "July202021!")

#Hispanic Origin
hispanRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HispanRaw WHERE WhoLivesYear = 2019")# -3 removes St. Tammany because it is not included in this analysis


#Households with own children under 18
hwcRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HWCRaw WHERE WhoLivesYear = 2019")


#One-person households
singRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.SingRaw WHERE WhoLivesYear = 2019")


#Less than a high school degree, adults 25 and older
hsRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HSRaw WHERE WhoLivesYear = 2019")


#Bachelor's degree or higher, adults 25 and older
bachRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.BachRaw WHERE WhoLivesYear = 2019")


#Median household income, 201* inflation-adjusted dollars
medhhRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.MedHHRaw WHERE WhoLivesYear = 2019")


#Internet access
intaRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.IntaRaw WHERE WhoLivesYear = 2019")


#Poverty rate, population for whom poverty has been determined
povRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.PovRaw WHERE WhoLivesYear = 2019")


#Children in poverty, population for whom poverty has been determined	
childpovRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.ChildPovRaw WHERE WhoLivesYear = 2019")


#Households without access to a vehicle
vehRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.VehRaw WHERE WhoLivesYear = 2019")


#Population not U.S. citizens at birth
forborRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.ForBorRaw WHERE WhoLivesYear = 2019")


#Population who moved in the past year
mobRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.MobRaw WHERE WhoLivesYear = 2019")

#Homeownership rates
hoRaw <-sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HORaw WHERE WhoLivesYear = 2019")


#Homeowners without a mortgage
honomoRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HoNoMoRaw WHERE WhoLivesYear = 2019")


#Renters with severe housing cost burdens
rentburRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.RentBurRaw WHERE WhoLivesYear = 2019")


#Homeowners with severe housing cost burdens
hoburRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HoBurRaw WHERE WhoLivesYear = 2019")


#Median gross rent, 201* inflation-adjusted dollars
medrentRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.MedRentRaw WHERE WhoLivesYear = 2019")


#Year structure built, 201* housing units
yrbuiltRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.YrBuiltRaw WHERE WhoLivesYear = 2019")


#Means of transportation to work, workers 16 and older
commuteRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.CommuteRaw WHERE WhoLivesYear = 2019")

############################################
# # PEP # #
############################################


#### Detailed age groups, race, sex, and hispanic origin from popest


#allparishesRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.AllParishesRaw WHERE WhoLivesYear = 2019")

hisppopestRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HispPopEstRaw WHERE WhoLivesYear IN (2000, 2019)")

blackpopestRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.BlackPopEstRaw WHERE WhoLivesYear = 2019")



##For 2020 update

library(AzureKeyVault)
library(AzureAuth)
library(AzureStor)

endp_key <- storage_endpoint("https://datacenterdc2datalake.blob.core.windows.net/", key = "ZwDaNK83wJT4qhRkQvbIsqyfEH2MdHSpYiDzWIQ3nKMyleefFiSmqm2yV7JD6WgCmaY7zOHpGw+R0NtRZ5VavQ==")

cont_s <- storage_container(endp_key, "source")
cont_scp2t <- storage_container(endp_key, "source/census/pep/2021 update (2020 data)/transformed data")

# iris_csv <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/ "github/ids-process-test-main/inputs/iris.csv")

PEP2020raw <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/CC-EST2020-ALLDATA-22.csv")

PEPage2020crosswalk <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/PEPage2021crosswalk.csv")

PEPover18raw<- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/CC-EST2020-AGESEX-22.csv")

PEPyear2020crosswalk <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/PEPyear2021crosswalk.csv")

PEPsexrace2020crosswalk <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/PEPsexrace2021crosswalk.csv")

PEPUS2020raw <- storage_read_csv(cont_s, "census/pep/2021 update (2020 data)/NC-EST2020-ALLDATA-R-File22.csv")

##For hisp inter years


popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP")
hisppopestINTRaw <- getCensus(name = "pep/int_charagegroups", # Intercensal estimates
                              vintage = 2000, 
                              key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                              vars = popestVars2000,
                              region = "county:071,051,075,087,089,093,095,103", 
                              regionin = "state:22") %>% 
  mutate(PlaceName = str_sub(GEONAME, 1, nchar(GEONAME) - 18),
         year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
  #select(-GEONAME, -DATE) %>% 
  filter(HISP == 2) %>% 
  filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009) %>% 
  rename(Population = POP,
         DateDesc = DATE_DESC,
         CensusYear = year) %>% 
  mutate(HispName = "Hispanic") %>% 
  mutate(WhoLivesYear = 2000) %>% 
  mutate(Population = as.numeric(Population)) %>% 
  select(WhoLivesYear, PlaceName, DateDesc, CensusYear, HispName, Population)


