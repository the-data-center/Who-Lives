############################################
# # ACS # #
############################################

IDS <- odbcConnect("DC2 IDS", uid = "rweinstein", pwd = "August312020")

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


allparishesRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.AllParishesRaw WHERE WhoLivesYear = 2019")

hisppopestRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.HispPopEstRaw WHERE WhoLivesYear IN (2000, 2019)")

blackpopestRaw <- sqlQuery(IDS, "SELECT * FROM PROD_CensusBureau.wholives.BlackPopEstRaw WHERE WhoLivesYear = 2019")

