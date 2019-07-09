############################################
# # ACS # #
############################################

IDS <- odbcConnect("DC2 IDS", uid = "jlosh", pwd = "Ax244!?bB12")

#Hispanic Origin
hispanRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HispanRaw WHERE WhoLivesYear = 2017")# -3 removes St. Tammany because it is not included in this analysis


#Households with own children under 18
hwcRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HWCRaw WHERE WhoLivesYear = 2017")


#One-person households
singRaw <- sqlQuery(IDS, "SELECT * FROM wholives.SingRaw WHERE WhoLivesYear = 2017")


#Less than a high school degree, adults 25 and older
hsRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HSRaw WHERE WhoLivesYear = 2017")


#Bachelor's degree or higher, adults 25 and older
bachRaw <- sqlQuery(IDS, "SELECT * FROM wholives.BachRaw WHERE WhoLivesYear = 2017")


#Median household income, 201* inflation-adjusted dollars
medhhRaw <- sqlQuery(IDS, "SELECT * FROM wholives.MedHHRaw WHERE WhoLivesYear = 2017")


#Internet access
intaRaw <- sqlQuery(IDS, "SELECT * FROM wholives.IntaRaw WHERE WhoLivesYear = 2017")


#Poverty rate, population for whom poverty has been determined
povRaw <- sqlQuery(IDS, "SELECT * FROM wholives.PovRaw WHERE WhoLivesYear = 2017")


#Children in poverty, population for whom poverty has been determined	
childpovRaw <- sqlQuery(IDS, "SELECT * FROM wholives.ChildPovRaw WHERE WhoLivesYear = 2017")


#Households without access to a vehicle
vehRaw <- sqlQuery(IDS, "SELECT * FROM wholives.VehRaw WHERE WhoLivesYear = 2017")


#Population not U.S. citizens at birth
forborRaw <- sqlQuery(IDS, "SELECT * FROM wholives.ForBorRaw WHERE WhoLivesYear = 2017")


#Population who moved in the past year
mobRaw <- sqlQuery(IDS, "SELECT * FROM wholives.MobRaw WHERE WhoLivesYear = 2017")

#Homeownership rates
hoRaw <-sqlQuery(IDS, "SELECT * FROM wholives.HORaw WHERE WhoLivesYear = 2017")


#Homeowners without a mortgage
honomoRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HoNoMoRaw WHERE WhoLivesYear = 2017")


#Renters with severe housing cost burdens
rentburRaw <- sqlQuery(IDS, "SELECT * FROM wholives.RentBurRaw WHERE WhoLivesYear = 2017")


#Homeowners with severe housing cost burdens
hoburRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HoBurRaw WHERE WhoLivesYear = 2017")


#Median gross rent, 201* inflation-adjusted dollars
medrentRaw <- sqlQuery(IDS, "SELECT * FROM wholives.MedRentRaw WHERE WhoLivesYear = 2017")


#Year structure built, 201* housing units
yrbuiltRaw <- sqlQuery(IDS, "SELECT * FROM wholives.YrBuiltRaw WHERE WhoLivesYear = 2017")


#Means of transportation to work, workers 16 and older
commuteRaw <- sqlQuery(IDS, "SELECT * FROM wholives.CommuteRaw WHERE WhoLivesYear = 2017")

############################################
# # PEP # #
############################################


#### Detailed age groups, race, sex, and hispanic origin from popest


allparishesRaw <- sqlQuery(IDS, "SELECT * FROM wholives.AllParishesRaw WHERE WhoLivesYear = 2018")

hisppopestRaw <- sqlQuery(IDS, "SELECT * FROM wholives.HispPopEstRaw WHERE WhoLivesYear = 2018")

blackpopestRaw <- sqlQuery(IDS, "SELECT * FROM wholives.BlackPopEstRaw WHERE WhoLivesYear = 2018")

