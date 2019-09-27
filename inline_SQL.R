
library(dplyr)

#Year to be changed manually

year <- data_frame("year" = 2018)
year <- year %>%
   select(year) %>%
  pull()
#for 2nd upDateDesc later in year -- aligns with ACS year
yearu2 <- data_frame("yearu2" = 2018)
yearu2 <- yearu2 %>%
  select(yearu2) %>%
  pull()

Overallpop <- allparishesRaw %>%
  filter(DateDesc =="7/1/2018 population estimate") %>%
  filter(SexName =="Total") %>%
  filter(HispName =="Total") %>%
  filter(RaceSimple =="Total") %>%
  filter(AgeGroupName =="Total")

#The U.S. Census Bureau estimates that 1,275,762 residents were living in metro New Orleans Parish as of July 2018
getvalue.pop.no.2018 <- Overallpop %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.jeff.2018 <- Overallpop %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.plaq.2018 <- Overallpop %>%
  filter(PlaceName == "Plaquemines Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.bernard.2018 <- Overallpop %>%
  filter(PlaceName == "St. Bernard Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.charles.2018 <- Overallpop %>%
  filter(PlaceName == "St. Charles Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.james.2018 <- Overallpop %>%
  filter(PlaceName == "St. James Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.john.2018 <- Overallpop %>%
  filter(PlaceName == "St. John The Baptist Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.tam.2018 <- Overallpop %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(Population) %>%
  pull()

metro2018pop <-(getvalue.pop.no.2018 + getvalue.pop.jeff.2018  + getvalue.pop.plaq.2018  + getvalue.pop.bernard.2018 + getvalue.pop.charles.2018 + getvalue.pop.james.2018 + getvalue.pop.john.2018 + getvalue.pop.tam.2018)


#According to the U.S. Census Bureau’s 2018 Population estimates, there are now 91,274 fewer African Americans living in New Orleans Parish (Orleans Parish Parish)

Overallpop2010 <- allparishesRaw %>%
  filter(DateDesc =="4/1/2010 Census population") %>%
  filter(SexName =="Total") %>%
  filter(HispName =="Total") %>%
  filter(RaceSimple =="Total") %>%
  filter(AgeGroupName =="Total")

#The U.S. Census Bureau estimates that 1,275,762 residents were living in metro New Orleans Parish as of July 2018
getvalue.pop.no.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.jeff.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.plaq.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Plaquemines Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.bernard.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Bernard Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.charles.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Charles Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.james.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. James Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.john.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. John The Baptist Parish") %>%
  select(Population) %>%
  pull()
getvalue.pop.tam.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(Population) %>%
  pull()


metro2010pop <- (getvalue.pop.no.2010 + getvalue.pop.jeff.2010  + getvalue.pop.plaq.2010  + getvalue.pop.bernard.2010 + getvalue.pop.charles.2010 + getvalue.pop.james.2010 + getvalue.pop.john.2010 + getvalue.pop.tam.2010)

pctincrease.metro.2018.2010 <- ((metro2018pop - metro2010pop)/metro2010pop)


#The number of African Americans living In New Orleans grew every year post-Katrina (from 2006 to 2018)
#but decreased for the first time post-Katrina from 232,118 in 2018 to 231,147 In 2018.

AA2018 <- AAhistorical %>%
  filter(year == 2018) %>%
  select(Population) %>%
  pull()

AA2018 <- AAhistorical %>%
  filter(year == 2018) %>%
  select(Population) %>%
  pull()







getvalue.AA.no.2018 <- AAWhiteHispan %>%
  filter(RaceSimple == "Black") %>%
  select(Population) %>%
  pull()

getvalue.AA.no.2000 <- AAWhiteHispan %>%
  filter(RaceSimple == "Black") %>%
  select(est2000) %>%
  pull()


#getvalue.AA.no.2000-getvalue.AA.no.2018

#compared to 2000, but there are also 7,945 fewer whites. Meanwhile, the number of Hispanics grew by 7,498.[2]


getvalue.white.no.2018 <- AAWhiteHispan %>%
  filter(RaceSimple == "White") %>%
  select(Population) %>%
  pull()

getvalue.white.no.2000 <- AAWhiteHispan %>%
  filter(RaceSimple == "White") %>%
  select(est2000) %>%
  pull()

#getvalue.white.no.2000 - getvalue.white.no.2018

#the number of Hispanics grew by 7,498.[2]
getvalue.Hispan.no.2018 <- AAWhiteHispan %>%
  filter(RaceSimple == "Hispanic") %>%
  select(Population) %>%
  pull()

getvalue.Hispan.no.2000 <- AAWhiteHispan %>%
  filter(RaceSimple == "Hispanic") %>%
  select(est2000) %>%
  pull()

#In Orleans Parish Parish, the share of the 2018 Population that is African American — while lower than in 2000 when it was 66.7 percent —

getvalue.AApct.no.2000 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(black2000) %>%
  pull()

#continues to represent the majority of city residents at 59.0 percent.

getvalue.AApct.no.2018 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(pctblack) %>%
  pull()

#The share of Hispanics in the city increased from 3.1 percent in 2000 to 5.7 percent in 2018;

getvalue.Hispanpct.no.2018 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(pcthisp) %>%
  pull()

getvalue.Hispanpct.no.2000 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(hispanic2000) %>%
  pull()

#the share of Asians increased from 2.3 percent to 3.0 percent;
getvalue.Asianpct.no.2018 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(pctasian) %>%
  pull()

getvalue.Asianpct.no.2000 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(asian2000) %>%
  pull()
#and the share of whites increased from 26.6 percent to 30.7 percent.

getvalue.whitepct.no.2018 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(pctwhite) %>%
  pull()

getvalue.whitepct.no.2000 <- ParishDemo %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(white2000) %>%
  pull()

#Meanwhile, Hispanic, Asian, and African American Populations increased as a share of the total Population in Jefferson Parish, St. Bernard Parish, St. Charles Parish, St. John The Baptist Parish, and St. Tammany Parish parishes, each.
#In fact, the number and share of Hispanics have increased in all eight parishes in the metro area.


#Between 2000 and 2018, the number of Hispanics in Jefferson Parish Parish increased by 33,163

getvalue.hispan.jeff.2000 <- HispanicPop %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(est2000) %>%
  pull()

getvalue.hispan.jeff.2018 <- HispanicPop %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(Population) %>%
  pull()
#getvalue.hispan.jeff.2018-getvalue.hispan.jeff.2000

#reaching over 14.9 percent of the total parish Population.

getvalue.Hispanpct.jeff.2018 <- ParishDemo %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(pcthisp) %>%
  pull()


#Orleans Parish Parish and St. Tammany Parish Parish gained 7,498 and 10,021 Hispanics, respectively,

#getvalue.Hispan.no.2018 - getvalue.Hispan.no.2000

getvalue.hispan.tam.2000 <- HispanicPop %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(est2000) %>%
  pull()

getvalue.hispan.tam.2018 <- HispanicPop %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(Population) %>%
  pull()

#such that the Hispanic share of the Population was 5.7 percent in Orleans Parish and 5.8 percent in St. Tammany Parish by 2018.

#getvalue.Hispanpct.no.2018

getvalue.Hispanpct.tam.2018 <- ParishDemo %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(pcthisp) %>%
  pull()



#As of July 2018, there were 115,396 Hispanics in the metro area,
getvalue.Hispan.metro.2018 <- ParishDemo %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(Hispanic) %>%
  pull()


#representing 9.0 percent of the metro Population.

getvalue.Hispanpct.metro.2018 <- ParishDemo %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(pcthisp) %>%
  pull()

#This is up from 2000 when there were 58,545,

getvalue.Hispanpop.jeff.2018 <- HispanicPop %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.no.2018 <- HispanicPop %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.plaq.2018 <- HispanicPop %>%
  filter(PlaceName == "Plaquemines Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.bern.2018 <- HispanicPop %>%
  filter(PlaceName == "St. Bernard Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.char.2018 <- HispanicPop %>%
  filter(PlaceName == "St. Charles Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.jam.2018 <- HispanicPop %>%
  filter(PlaceName == "St. James Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.john.2018 <- HispanicPop %>%
  filter(PlaceName == "St. John The Baptist Parish") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.tam.2018 <- HispanicPop %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(est2000) %>%
  pull()

getvalue.hispanpop.metro.2018 <- (getvalue.Hispanpop.jeff.2018 + getvalue.Hispanpop.no.2018 + getvalue.Hispanpop.plaq.2018 + getvalue.Hispanpop.bern.2018 + getvalue.Hispanpop.char.2018 + getvalue.Hispanpop.jam.2018 + getvalue.Hispanpop.john.2018 + getvalue.Hispanpop.tam.2018)



# representing 4.4 percent of the metro Population.
getvalue.Hispanpct.metro.2000 <- ParishDemo %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(hispanic2000) %>%
  pull()


#Despite these recent gains, the Hispanic share of the Population in metro area parishes is far below the averAgeGroupName for the United States,
#which has grown from 12.5 percent to 18.1 percent of the total U.S. Population over these 17 years.

getvalue.Hispanpct.us.2000 <- ParishDemo %>%
  filter(PlaceName == "United States") %>%
  select(hispanic2000) %>%
  pull()

getvalue.Hispanpct.us.2018 <- ParishDemo %>%
  filter(PlaceName == "United States") %>%
  select(pcthisp) %>%
  pull()




#The number of Hispanics in New Orleans Parish metro has grown every year since 2006.
#Indeed while the overall metro Population has grown 7% since 2010,
# pctincrease.metro.2018.2010

#the Hispanic Population has grown 20%

getvalue.Hispgrowth.metro.2010 <-HISPpopM %>%
  filter(CensusYear ==2010) %>%
  mutate(Population = as.numeric(Population)) %>%
  select(Population)%>%
  pull()
# sum(getvalue.Hispgrowth.metro.2010)

getvalue.Hispgrowth.metro.2018 <-HISPpopM %>%
  filter(CensusYear ==2018) %>%
  mutate(Population = as.numeric(Population)) %>%
  select(Population)%>%
  pull()
#sum(getvalue.Hispgrowth.metro.2018)
getvalue.Hispgrowth.metro.2010.2018 <- ((sum(getvalue.Hispgrowth.metro.2018) - sum(getvalue.Hispgrowth.metro.2010)) / sum(getvalue.Hispgrowth.metro.2010))

#such that Hispanics account for more than one-quarter of the metro's Population growth since 2010.
getvalue.hispangrowthCOUNT.metro.2010.2018 <- (sum(getvalue.Hispgrowth.metro.2018) - sum(getvalue.Hispgrowth.metro.2010))
getvalue.growthCOUNT.metro.2010.2018 <-(metro2018pop - metro2010pop)

getvalue.growthpcthispanic.metro.2010.2018 <- (getvalue.hispangrowthCOUNT.metro.2010.2018)/ getvalue.growthCOUNT.metro.2010.2018



#This increase was part of a larger trend that has pushed the Central American Population of the metro to -- percent of the Hispanic metro Population.
getvalue.Hon.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
 filter(PlaceName.fac == "Metro") %>%
 filter(description == "Honduran") %>%
 select(value) %>%
  pull()
getvalue.sal.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "Metro") %>%
  filter(description == "Salvadoran") %>%
  select(value) %>%
  pull()
getvalue.guat.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "Metro") %>%
  filter(description == "Guatemalan") %>%
  select(value) %>%
  pull()
getvalue.nic.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "Metro") %>%
  filter(description == "Nicaraguan") %>%
  select(value) %>%
  pull()
getvalue.otherCA.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "Metro") %>%
  filter(description == "Other Central American") %>%
  select(value) %>%
  pull()

getvalue.CA.metro.2018 <- (getvalue.Hon.2018 + getvalue.sal.2018 + getvalue.guat.2018 + getvalue.nic.2018 + getvalue.otherCA.2018)



#In comparison, Central Americans represent only 9 percent of the national Hispanic Population.
getvalue.Hon.us.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Honduran") %>%
  select(value) %>%
  pull()
getvalue.sal.us.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Salvadoran") %>%
  select(value) %>%
  pull()
getvalue.guat.us.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Guatemalan") %>%
  select(value) %>%
  pull()
getvalue.nic.us.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Nicaraguan") %>%
  select(value) %>%
  pull()
getvalue.otherCA.us.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Other Central American") %>%
  select(value) %>%
  pull()

getvalue.CA.us.2018 <- (getvalue.Hon.us.2018 + getvalue.sal.us.2018 + getvalue.guat.us.2018 + getvalue.nic.us.2018 + getvalue.otherCA.us.2018)


# Mexicans are now solidly the second largest group behind Hondurans in metro New Orleans Parish, representing - percent of the Hispanic Population.
getvalue.Mex.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "Metro") %>%
  filter(description == "Mexican") %>%
  select(value) %>%
  pull()

# Mexican Population is much less prominent in the metro than nationally, where it represents - percent of the Hispanic Population.

getvalue.MexUS.2018 <- hispan2018 %>%
  mutate(value = as.numeric(value)) %>%
  filter(PlaceName.fac == "United States") %>%
  filter(description == "Mexican") %>%
  select(value) %>%
  pull()

#The progression of the baby boomers through the AgeGroupName groups, along with falling birth rates, have brought massive changes to the metro —
#and indeed the whole country — with many more changes yet to come.[4] Looking at the total Population in the metro by five-year AgeGroupName groups for 2000 and 2018,
#the baby boomers are like a demographic tidal wave. Consequently, the median AgeGroupName of the metro has risen to 37.7 in 2016 from 34.8 in 2000.






#As of 2016, 26 percent of households in metro New Orleans Parish included children

getvalue.hwc.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(pcthwc) %>%
pull()

getvalue.hwc.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(census2000) %>%
pull()

# . Between 2000 and 2016, the percent of St. Tammany Parish households with children declined from 40 percent to 29 percent;

getvalue.hwc.StTam.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(census2000) %>%
pull()

getvalue.hwc.StTam.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "St. Tammany Parish") %>%
  select(pcthwc) %>%
pull()

#the percent of Jefferson Parish households with children declined from 33 percent to 26 percent;


getvalue.hwc.Jeff.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(census2000) %>%
pull()

getvalue.hwc.Jeff.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "Jefferson Parish") %>%
  select(pcthwc) %>%
pull()


#and the percent of Orleans Parish households with children declined from 30 percent to 20 percent.

getvalue.hwc.no.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(census2000) %>%
pull()

getvalue.hwc.no.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(PlaceName == "Orleans Parish") %>%
  select(pcthwc) %>%
pull()



# The metro area share of individuals living alone grew from 27 percent in 2000 to - percent in 2016


getvalue.sing.2000 <- sing %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(census2000) %>%
  pull()

  getvalue.sing.2018 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(pctsing) %>%
  pull()

  # similar to the trend for Jefferson Parish Parish. The increase was larger in Orleans Parish Parish, which jumped from 33 to 43 percent.
  getvalue.singNO.2000 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
  pull()

  getvalue.singNO.2018 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(pctsing) %>%
  pull()


  #While the metro has regained much of the post-Katrina Population losses, youth Population is substantially lower than pre-Katrina levels.
  #The metro had 358,092 children under 18 years in 2000 and only 284,231 in 2018.

  getvalue.under18.metro.2000 <- popunder18 %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(est2000) %>%
    pull()

  getvalue.under18.metro.2018 <- popunder18 %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(under18) %>%
    pull()

  #Much of this loss was driven by Orleans Parish Parish, where the under 18 Population declined to 79,264 from 129,408. The under 18 Population is now 21 percent of the metro Population, down from 27 percent in 2000.

  getvalue.under18.no.2000 <- popunder18 %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(est2000) %>%
    pull()

  getvalue.under18.no.2018 <- popunder18 %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(under18) %>%
    pull()

 #The under 18 Population is now 21 percent of the metro Population, down from 27 percent in 2000.
  getvalue.over18.metro.2018 <- under18metro %>%
    filter(AgeGroupName == "18 years and over") %>%
    select(Population) %>%
    pull()

  getvalue.under18totalpop.metro.2018 <- under18metro %>%
    filter(AgeGroupName == "Total") %>%
    select(Population) %>%
    pull()

  getvalue.pct.under18.metro.2018 <- ( (getvalue.under18totalpop.metro.2018 - getvalue.over18.metro.2018) / getvalue.under18totalpop.metro.2018)

  #The proportion of adults 25 years and older with less than a high school education declined across all three of the largest parishes,
  #leading to a metrowide decrease from 15 percent in 2000 to 14 percent in 2016.


  getvalue.hs.metro.2000 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(census2000) %>%
  pull()

  getvalue.hs.metro.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(pctless) %>%
  pull()

  #New Orleans Parish, the share of adults with less than a high school degree fell from 25 percent to 14 percent but is still higher than the U.S. averAgeGroupName of 13 percent.

  getvalue.hs.no.2000 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
  pull()

  getvalue.hs.no.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(pctless) %>%
  pull()

  getvalue.hs.us.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(pctless) %>%
  pull()

  # In New Orleans Parish, 38 percent of adults 25 and older had a college degree in 2016
  getvalue.bach.no.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(pctbach) %>%
    pull()

  #higher than the U.S. averAgeGroupName of 31 percent
  getvalue.bach.us.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(pctbach) %>%
    pull()

  # up from 26 percent in 2000.
  getvalue.bach.no.2000 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
    pull()

  # The overall metro area share of adults with a bachelor’s degree grew from 23 to 29 percent
  getvalue.bach.metro.2000 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(census2000) %>%
    pull()

  getvalue.bach.metro.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(pctbach) %>%
    pull()

  # pushed household income down 5 percent in the nation between 1999 and 2016
  getvalue.medhh.us.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.us.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(MedianHHIncome) %>%
    pull()

  #and 4 percent across the metro
  getvalue.medhh.metro.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.metro.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(MedianHHIncome) %>%
    pull()

  getvalue.medhh.no.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(MedianHHIncome) %>%
    pull()

  #In Jefferson Parish Parish, median household income declined 10 percent between 1999 and 2016, falling to $49,678
  getvalue.medhh.jeff.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.jeff.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(MedianHHIncome) %>%
    pull()

  #Internet access is an important indicator of access to information. Studies have shown that without broadband, computer access, and
  # encompassing technology training services, workers and students are at a disadvantAgeGroupName in the job market and education system.
  #[6] Only 60 percent of households in Orleans Parish Parish and only 67 percent of households in Jefferson Parish Parish are connected to the

  getvalue.inta.no.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(broadbandpct) %>%
    pull()

  getvalue.inta.jeff.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(broadbandpct) %>%
    pull()


  #Internet through a home-based internet service, such as broadband (cable, DSL, or fiber), satellite, or dial-up service,
  #compared to 71 percent nationwide.

  getvalue.inta.us.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(broadbandpct) %>%
    pull()

  #St. Tammany Parish is above the national averAgeGroupName at 75 percent of households connected to the
  getvalue.inta.tam.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(broadbandpct) %>%
    pull()
  #Internet by a home-based service internet connection. Internet access without a subscription refers to households who only
  #have access through group access locations such as school, work, a library, or coffee shop.

  #An increasingly common way to access the Internet is through a smartphone or some other cellular device. While,
  #in general, smartphone access contributes positively to lessening the Digital Divide, having access only through a
  #smartphone restricts ability to fully leverAgeGroupName the Internet to complete common tasks such as writing and researching
  #a resume, registering your kids for school, analyzing data about your neighborhood, or creating content for an internet
  #business.

  #13 percent of households in Orleans Parish Parish and 8 percent of households in Jefferson Parish Parish only have access
  #through a smartphone. This is compared to 10 percent nationwide.

  getvalue.intacell.no.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(cellonlypct) %>%
    pull()

  getvalue.intacell.jeff.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(cellonlypct) %>%
    pull()

  getvalue.intacell.us.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(cellonlypct) %>%
    pull()


 #Individuals living below the poverty level indicate the economy is not providing all residents with the ability to meet
  #their most basic needs, including food, housing, and transportation.

  #The poverty rate in New Orleans Parish declined from 28 percent in 1999 to 24 percent in 2016.
  getvalue.pov.no.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.no.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(pctpov) %>%
    pull()

  #Meanwhile, the Jefferson Parish Parish poverty rate increased from 14 to 16 percent between
  #1999 and 2016.

  getvalue.pov.jeff.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.jeff.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(pctpov) %>%
    pull()

  #St. Tammany Parish pov

  getvalue.pov.tam.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.tam.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(pctpov) %>%
    pull()


  #Across the U.S., the poverty rate increased from 12 to 14 percent between 1999 and 2016.

  getvalue.pov.us.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.us.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(pctpov) %>%
    pull()


  #Like the overall poverty rate, the child poverty rate in New Orleans Parish has fallen, while child poverty has increased in Jefferson Parish and across the U.S.
  #The New Orleans Parish child poverty rate fell from 41 percent in 1999 to 33 percent in 2016.

  getvalue.childpov.no.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.no.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(pctBelowChildPov) %>%
    pull()

  #In Jefferson Parish Parish, the child poverty rate jumped from 20 percent in 1999 to 26 percent in 2016 —
  getvalue.childpov.jeff.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.jeff.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(pctBelowChildPov) %>%
    pull()


  #greatly surpassing the U.S. child poverty rate, which rose from 17 to 19 percent between 1999 and 2016.
  getvalue.childpov.us.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.us.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(pctBelowChildPov) %>%
    pull()

  # Post-Katrina, the share of New Orleans Parish households without access to a vehicle
  #dropped from 27 percent in 2000 to 20 percent in 2016. Nonetheless, at 20 percent,
  #New Orleans Parish’ share is more than twice as high as in neighboring parishes and the nation,
  #indicating the importance of a robust public transportation system and comprehensive evacuation plan.

  getvalue.veh.no.2000 <- veh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.veh.no.2018 <- veh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(vehpct) %>%
    pull()

  #A rising foreign-born share of the Population may reflect expanding economic opportunities for both high-skilled and low-skilled workers.[7]
  #That share of the Population has grown in all three of the most populous metro parishes since 2000,
  #led by a 5 percentAgeGroupName point gain in Jefferson Parish Parish.
  #By 2016, fully 13 percent of Jefferson Parish Parish Population was foreign-born

  getvalue.forbor.jeff.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.jeff.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(forborpct) %>%
    pull()


  #statistically the same as the U.S. share.
  #Need to test if this is true********************************

  #In Orleans Parish Parish and St. Tammany Parish Parish, the foreign-born share of the Population increased by 2 percentAgeGroupName point between
  #2000 and 2016 each.
  getvalue.forbor.no.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.no.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(forborpct) %>%
    pull()

  getvalue.forbor.tam.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.tam.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(forborpct) %>%
    pull()

  #Like the foreign-born Population, a rising share of the Population who moved into Orleans Parish Parish in the past year may
  #reflect expanding economic opportunities. The most frequent reason people move long distances, such as from one state
  #to another state, is for job opportunities.[8] In addition, the young and well-educated are more likely than others to
  #move long distances.[9]

  #In 2016, 7 percent of the Population in Orleans Parish Parish had moved into the parish in the past year,

  getvalue.mobabroadpct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(mobabroadpct) %>%
    pull()

  getvalue.mobStatespct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(mobStatespct) %>%
    pull()

  getvalue.difparishpct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(difparishpct) %>%
    pull()

  #up from 3 percent in 2004.
  getvalue.mobabroadpct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004mobabroad) %>%
    pull()

  getvalue.mobStatespct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004states) %>%
    pull()

  getvalue.difparishpct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004difparish) %>%
    pull()







  getvalue.mobsamehouse.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(samehousepct) %>%
    pull()

  getvalue.mobbtwnparish.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(withinparishpct) %>%
    pull()

  getvalue.mobsamehouse.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004samehouse) %>%
    pull()

  getvalue.mobbtwnparish.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004withinparish) %>%
    pull()



  #Over half of the new movers into Orleans Parish Parish came from outside the state of Louisiana.

  getvalue.mobTotMovedbtwnStates.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(TotMovedBtwnStates) %>%
    pull()

  getvalue.mobTotMovedfromAbroad.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(TotMovedFromAbroad) %>%
    pull()

  getvalue.mobTotal.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(Total) %>%
    pull()

  getvalue.mobTotSameHouse.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(TotSameHouse) %>%
    pull()


  getvalue.mobTotMovedInCity.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(TotMovedInCty) %>%
    pull()

getvalue.outsidestatepct.no.2018 <- (getvalue.mobTotMovedbtwnStates.no.2018 + getvalue.mobTotMovedfromAbroad.no.2018)/(getvalue.mobTotal.no.2018-getvalue.mobTotSameHouse.no.2018 -  getvalue.mobTotMovedInCity.no.2018)

  #In Jefferson Parish Parish, the share of the Population who were new movers into the parish was 5 percent in 2004,
  #and has not significantly changed for 2016.

  getvalue.abroad.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(sf2004mobabroad) %>%
    pull()

  getvalue.states.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(sf2004states) %>%
    pull()

  getvalue.difparish.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(sf2004difparish) %>%
    pull()

getvalue.moboutofparish.jeff.2004 <- (getvalue.abroad.jeff.2004  + getvalue.states.jeff.2004 + getvalue.difparish.jeff.2004)

#and has not significantly changed for 2016.



  #Homeownership rates across the U.S. have fallen since 2000 from 66 to 63 percent in 2016.

  getvalue.ho.us.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(census2000) %>%
    pull()

  getvalue.ho.us.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(Ownerpct) %>%
    pull()

#Homeownership rates have stayed the same in St. Tammany Parish at about 79 percent
  #*************Need to test signifigance********
  getvalue.ho.tam.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.ho.tam.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(Ownerpct) %>%
    pull()

  #and more dramatically dropped in Jefferson Parish from 64 to 59 percent between 2000 and 2016.
  getvalue.ho.jeff.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.ho.jeff.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(Ownerpct) %>%
    pull()

  #In contrast, homeownership rates in New Orleans Parish have held steady, but at a much lower 46 percent.

  getvalue.ho.no.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
    pull()

  getvalue.ho.no.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(Ownerpct) %>%
    pull()




  #Homeowners without a mortgAgeGroupName own their homes free and clear of any type of loan. A high share of such homeowners usually indicates residents living in the same house for long periods of time, and helps shield neighborhoods from foreclosures.
  #The proportion of metro area homeowners without a mortgAgeGroupName has increased from 35 to 42 percent between 2000 and 2016,


  getvalue.honomo.metro.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.metro.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(census2000) %>%
    pull()


  #driven by changes in Orleans Parish and Jefferson Parish. The share of homeowners without a mortgAgeGroupName jumped from 33 to 43 percent in Orleans Parish

  getvalue.honomo.no.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.no.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000) %>%
    pull()

  #and from 35 to 42 percent in Jefferson Parish. One reason for the surge may be that homeowners who returned after Katrina used insurance or Road Home proceeds to pay off their mortgAgeGroupName principal. In fact, Orleans Parish and Jefferson Parish received the first and second largest number of Road Home Option 1 grants among all Louisiana parishes.[10]

  getvalue.honomo.jeff.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.jeff.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(census2000) %>%
    pull()



  #High housing costs can limit a region’s ability to attract and retain the workforce essential for a healthy economy.[11] Severe housing cost burdens of more than 50 percent of household income indicate a serious problem in housing affordability.
  #In 2004, the share of severely cost-burdened renters in New Orleans Parish and the U.S. was 24 percent.

  getvalue.rentbur.no.2004 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004) %>%
    pull()

  getvalue.rentbur.us.2004 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(sf2004) %>%
    pull()

  #In the 12 years since, that share has spiked to 35 percent in Orleans Parish while rising to only 25 percent nationally.

  getvalue.rentburpct.no.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(rentburpct) %>%
    pull()

  getvalue.rentburpct.us.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(rentburpct) %>%
    pull()

  #In Jefferson Parish Parish, the share of renters paying more than 50 percent of household income on housing and utilities
  #is 29 percent in 2016, on par with the nation.

  getvalue.rentbur.jeff.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(rentburpct) %>%
    pull()

  #The share of homeowners paying more than 50 percent of household income on their mortgAgeGroupName, taxes, utilities, and insurance is virtually unchanged
  #in metro area parishes and the nation since 2004.
  #There is a clear gap between the rate of housing cost burden for renters vs. homeowners, and that gap has widened.

  #The surge in the share of severely cost-burdened renters in New Orleans Parish is reflective of the surge in the median gross rent (rent plus utilities) in the city.
  #From 2004 to 2016, monthly rent plus utilities rose from $724 to $934 in New Orleans Parish,

  getvalue.medrent.no.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.no.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(Rent) %>%
    pull()


  #a 29 percent increase.

 #((getvalue.rentbur.no.2018 - getvalue.rentbur.no.2004) / getvalue.rentbur.no.2004)

  # Meanwhile, median gross rents increased 18 percent metrowide
  getvalue.medrent.metro.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.metro.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(Rent) %>%
    pull()
  #((getvalue.medrent.metro.2018 - getvalue.medrent.metro.2004) / getvalue.medrent.metro.2004)

  #compared to only 10 percent nationwide.
  getvalue.medrent.us.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.us.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(PlaceName == "United States") %>%
    select(Rent) %>%
    pull()
  #((getvalue.medrent.us.2018 - getvalue.medrent.us.2004) / getvalue.medrent.us.2004)



  #In Orleans Parish Parish, fully 42 percent of all housing units are in pre-1950 structures.


  getvalue.yrbuilt1950.nola.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(orbefore1949pct) %>%
    pull()



  #Meanwhile, in Jefferson Parish Parish, 77 percent of the housing stock was built in the 1950s, 1960s, 1970s, and 1980s,

  getvalue.yrbuilt1950to1989.jeff.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(mid1950to1989pct) %>%
    pull()

  #and just 16 percent of housing stock has been built since 1990.
  getvalue.yrbuilt1990orlater.jeff.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "Jefferson Parish") %>%
    select(orLater1990pct) %>%
    pull()


  #In contrast, in St. Tammany Parish, the majority of housing units are in structures that have been built since 1990.

  getvalue.yrbuilt1990orlater.tam.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(PlaceName == "St. Tammany Parish") %>%
    select(orLater1990pct) %>%
    pull()

  #The share of commuters in New Orleans Parish using public transportation declined sharply from 13 percent in 2000 to 7 percent in 2016,

  getvalue.publictransit.no.2000 <- commute %>%
    select(-contains("SIG"), -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(census2000publictransit) %>%
    pull()


   getvalue.publictransit.no.2018 <- commute %>%
    select(-contains("SIG"), -contains("moeprop")) %>%
    filter(PlaceName == "Orleans Parish") %>%
    select(PublicTransitpct) %>%
    pull()



  #while the share in Jefferson Parish Parish is stastically the same as 2000, with 1 percent of commuters using public transportation in 2016.

   getvalue.publictransit.jeff.2000 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(PlaceName == "Jefferson Parish") %>%
     select(census2000publictransit) %>%
     pull()


   getvalue.publictransit.jeff.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(PlaceName == "Jefferson Parish") %>%
     select(PublicTransitpct) %>%
     pull()

  #But the share of bike commuters in New Orleans Parish rose to 3 percent, giving the city the sixth highest share of bike commuting of the largest 70 cities nationwide.[15]

   getvalue.bike.no.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(PlaceName == "Orleans Parish") %>%
     select(bikepct) %>%
     pull()

  #Meanwhile, the metro share of carpoolers fell from 15 percent in 2000 to 11 percent in 2016, as did the Orleans Parish Parish

   getvalue.carpool.metro.2000 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(grepl("Metro",PlaceName)) %>%
     select(census2000carpool) %>%
     pull()

    getvalue.carpool.metro.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(grepl("Metro",PlaceName)) %>%
     select(Carpoolpct) %>%
     pull()

  #as did the Orleans Parish Parish share from 16 percent to 10 percent.

    getvalue.carpool.no.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(PlaceName == "Orleans Parish") %>%
      select(census2000carpool) %>%
      pull()

    getvalue.carpool.no.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(PlaceName == "Orleans Parish") %>%
      select(Carpoolpct) %>%
      pull()

    #The percentAgeGroupName of workers who commute by driving alone has increased within the metro region since 2000 from 73 percent to 77 percent,

    getvalue.drivealone.metro.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(grepl("Metro",PlaceName)) %>%
      select(census2000drive) %>%
      pull()

    getvalue.drivealone.metro.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(grepl("Metro",PlaceName)) %>%
      select(Drivepct) %>%
      pull()

    #driven by a 60 to 67 percent rise in Orleans Parish Parish.

    getvalue.drivealone.no.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(PlaceName == "Orleans Parish") %>%
      select(census2000drive) %>%
      pull()

    getvalue.drivealone.no.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(PlaceName == "Orleans Parish") %>%
      select(Drivepct) %>%
      pull()
    #This goes against the national trend, where the share in driving alone remained steady between 2000 and 2016
    #and where public transit use has also remained steady.




