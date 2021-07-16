    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   SUMLEV = col_character(),
    ##   COUNTY = col_character(),
    ##   STNAME = col_character(),
    ##   CTYNAME = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   AGEGRP = col_double(),
    ##   AgeGroupName = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   SUMLEV = col_character(),
    ##   COUNTY = col_character(),
    ##   STNAME = col_character(),
    ##   CTYNAME = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   YEAR = col_double(),
    ##   DateDesc = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   Variable = col_character(),
    ##   HispName = col_character(),
    ##   RaceName = col_character(),
    ##   SexName = col_character(),
    ##   RaceSimple = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   UNIVERSE = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## `summarise()` ungrouping output (override with `.groups` argument)

<div class="highlight" markdown="1"> 

This brief examines the most current demographic data released by the U.S. Census Bureau and identifies important trends in metro area parishes. Included are data on race and ethnicity, age, educational attainment, internet access, poverty, income, children, access to vehicles, foreign-born population, geographic mobility, homeownership, homeowners with a mortgage, housing costs and affordability, single-person households, commuting, and housing stock.
</div>

The U.S. Census Bureau estimates that 1,273,979 residents were living in
metro New Orleans as of July 2020, a 7 percent increase from April
2010.[^1] The metro area now has 95 percent of its 2000 population of
1,337,726. In this brief, we examine demographic data released by the
U.S. Census Bureau and identify important changes in metro area parishes
since 2000 (or the best benchmark available).

Race/Ethnicity {#raceethnicity}
--------------

According to the U.S. Census Bureau’s 2020 population estimates, there
are now 94,276 fewer African Americans living in New Orleans (Orleans
Parish) compared to 2000, but there are also 7,970 fewer whites.
Meanwhile, the number of Hispanics grew by 6,838.[^2]

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/AAWhiteHispan-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 and Population Estimates 2020.</p>
</div>

<div class="highlight" markdown="1"> 

### What is Orleans Parish? 

Orleans Parish is the city of New Orleans. New Orleans and Orleans Parish are interchangeable. Their boundaries are the same, and they contain the same population. 
</div>

In Orleans Parish, the share of the 2020 population that is African
American — while lower than in 2000 when it was 67 percent — continues
to represent the majority of city residents at 59 percent. The share of
Hispanics in the city increased from 3 percent in 2000 to 6 percent in
2020; the share of Asians increased from 2 percent to 3 percent; and the
share of whites increased from 27 percent to 31 percent. Meanwhile,
Hispanic, Asian, and African American populations increased as a share
of the total population in Jefferson, St. Bernard, St. Charles, St. John
the Baptist, and St. Tammany parishes, each. In fact, the number and
share of Hispanics have increased in all eight parishes in the metro
area.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/demoallparish-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 and Population Estimates 2020.</p>
</div>

The number of African Americans living in New Orleans grew every year
post-Katrina (from 2006 to 2017) but decreased for the first time
post-Katrina from 232,189 in 2017 to 231,740 in 2018 and continued to
decrease in 2019.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/AAhistorical-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000, 2010, Intercensal Estimates 2006-2009, and Population Estimates 2020.</p>
</div>

Between 2000 and 2020, the number of Hispanics in Jefferson Parish
increased by 32,636 reaching over 15 percent of the total parish
population. Orleans Parish and St. Tammany Parish gained 6,838 and
11,071 Hispanics, respectively, such that the Hispanic share of the
population was 6 percent in Orleans and 6 percent in St. Tammany in
2020.

As of July 2020, there were 116,254 Hispanics in the metro area,
representing 9 percent of the metro population. This is up from 2000
when there were 58,545, representing 4 percent of the metro population.
Despite these recent gains, the Hispanic share of the population in
metro area parishes is far below the average for the United States,
which has grown from 12 percent to 19 percent of the total U.S.
population over these 20 years.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hispanpop-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 and Population Estimates 2020.</p>
</div>

The number of Hispanics in New Orleans metro has grown every year since
2006. Indeed while the overall metro population has grown 7 percent
since 2010, the Hispanic population has grown 26 percent such that
Hispanics account for 29 percent of the metro’s population growth since
2010.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hispanpopyear-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000, 2010, Intercensal Estimates 2006-2009, and Population Estimates 2020.</p>
</div>

Hispanic is an umbrella term comprising multiple nationalities and
ethnicities. Researchers have shown that most Hispanics prefer to
identify by nationality rather than by pan-ethnic terms such as
“Hispanic” and “Latino.”[^3] The nationalities of Hispanics residing in
metro New Orleans is quite distinct from the national Hispanic profile.

In 2020, the largest Hispanic group in metro New Orleans was Honduran,
representing 25 percent of the Hispanic population. In comparison,
Hondurans represent only 2 percent of the national Hispanic population.
These figures point to metro New Orleans as a hub of Honduran migration.

Not to be ignored, the Mexican population represents 21 percent of the
Hispanic population in metro New Orleans. Nevertheless, the Mexican
population is much less prominent in the metro than nationally, where it
represents 61 percent of the Hispanic population.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hispan2018-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between indicated measure and the U.S. is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 and American Community Survey 2019.</p>
</div>

Population by age and household types {#population-by-age-and-household-types}
-------------------------------------

The progression of the baby boomers through the age groups, along with
falling birth rates, have brought massive changes to the metro — and
indeed the whole country — with many more changes yet to come.[^4]
Looking at the total population in the metro by five-year age groups for
2000 and 2020, the baby boomers are like a demographic tidal wave.
Consequently, the median age of the metro has risen to 38.8 in 2020 from
34.8 in 2000.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/age2000-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000.</p>
</div>

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/agecurrent-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Population Estimates 2020.</p>
</div>

Meanwhile, the share of households with children is shrinking while the
share of individuals living alone is growing — both across the metro and
nation. As of 2020, 23 percent of households in metro New Orleans
included children, down from 34 percent in 2000. Between 2000 and 2020,
the percent of St. Tammany households with children declined from 40
percent to 29 percent; the percent of Jefferson households with children
declined from 33 percent to 23 percent; and the percent of Orleans
households with children declined from 30 percent to 17 percent.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hwc-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

As households with children have declined, the share of single-person
households has grown in the metro and nationwide. The metro area share
of individuals living alone grew from 27 percent in 2000 to 34 percent
in 2020 — similar to the trend for Jefferson Parish where the share of
households living alone grew from 27 percent to 31 percent. The increase
was larger in Orleans Parish, which jumped from 33 to 47 percent.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/sing-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

While the metro has regained much of the post-Katrina population losses,
youth population is substantially lower than pre-Katrina levels. The
metro had 358,092 children under 18 years in 2000 and only 280,243 in
2020. Much of this loss was driven by Orleans Parish, where the under 18
population declined to 76,561 from 129,408. The under 18 population is
now 22 percent of the metro population, down from 27 percent in 2000.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/popunder18-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 and Population Estimates 2020.</p>
</div>

<div class="highlight" markdown="1"> 

### Where is data for Plaquemines, St. Bernard, St. James, St. Charles, and St. John the Baptist?

Although race/ethnicity and age data is available for all eight parishes in metro New Orleans, most other demographic data (such as educational attainment, poverty, and homeownership) is available for only the three most populous parishes of Jefferson, Orleans, and St. Tammany, as well as the metro.
</div>

Educational attainment, income, and internet access {#educational-attainment-income-and-internet-access}
---------------------------------------------------

Educational attainment is an important determinant of household incomes,
workforce skills, and regional resiliency.[^5] The proportion of adults
25 years and older with less than a high school education declined
across all three of the largest parishes, leading to a metrowide
decrease from 15 percent in 2000 to 12 percent in 2020. In the city of
New Orleans, the share of adults with less than a high school degree
fell from 25 percent to 12 percent but is still higher than the U.S.
average of 11 percent.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hs-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

The metro area decline in the share of adults with less than a high
school degree has been coupled with an increase in the share with a
bachelor’s degree or higher. In New Orleans, 40 percent of adults 25 and
older had a college degree in 2020 — higher than the U.S. average of 33
percent, and up from 26 percent in 2000. The overall metro area share of
adults with a bachelor’s degree grew from 23 to 32 percent — lower than
the national average.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/bach-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

The 2020 median household income of $55,710 for the metro, $56,069 for
Jefferson Parish, and $45,615 for the city are significantly lower than
the U.S. median of $65,712.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/medhh-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 1999 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Internet access is an important indicator of access to information.
Studies have shown that without broadband, computer access, and
encompassing technology training services, workers and students are at a
disadvantage in the job market and education system.[^6] Only 65 percent
of households in Orleans Parish and only 70 percent of households in
Jefferson Parish are connected to the Internet through a home-based
internet service, such as broadband (cable, DSL, or fiber), satellite,
or dial-up service, compared to 75 percent nationwide. St. Tammany is
above the national average at 81 percent of households connected to the
Internet by a home-based service internet connection. Internet access
without a subscription refers to households who only have access through
group access locations such as school, work, a library, or coffee shop.

An increasingly common way to access the Internet is through a
smartphone or some other cellular device. While, in general, smartphone
access contributes positively to lessening the Digital Divide, having
access only through a smartphone restricts ability to fully leverage the
Internet to complete common tasks such as writing and researching a
resume, registering your kids for school, analyzing data about your
neighborhood, or creating content for an internet business. 14 percent
of households in Orleans Parish only have access through a smartphone.
This is compared to 12 percent nationwide.

<div class="highlight" markdown="1"> 

### What is the Digital Divide?

Inequalities between individuals, households, businesses, or geographic areas with regard to access, use of, or impact of information and communication technologies.
</div>

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/inta-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between indicated measure and the U.S. is not significant at 95% confidence interval.

**Notes**: Access with no subscription refers to those who receive free internet from their housing environment (e.g. college dorms).

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2019.</p>
</div>

Poverty and access to vehicles {#poverty-and-access-to-vehicles}
------------------------------

Individuals living below the poverty level indicate the economy is not
providing all residents with the ability to meet their most basic needs,
including food, housing, and transportation. The poverty rate in New
Orleans decreased from 28 to 23 percent between 1999 and 2020 while the
Jefferson Parish poverty rate remained statistically unchanged. Across
the U.S., the poverty rate has stayed the same between 1999 and 2020.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/pov-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 1999 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Like the overall poverty rate, the child poverty rate in New Orleans
decreased between 1999 and 2020. In Jefferson Parish, the child poverty
rate at 22 percent in 2020 is higher than the U.S. child poverty rate.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/childpov-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 1999 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Post-Katrina, the share of New Orleans households without access to a
vehicle dropped from 27 percent in 2000 to 16 percent in 2020.
Nonetheless, at 16 percent, New Orleans’ share is more than twice as
high as in neighboring parishes, indicating the importance of a robust
public transportation system and comprehensive evacuation plan.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/veh-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 1999 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Foreign-born population and geographic mobility {#foreign-born-population-and-geographic-mobility}
-----------------------------------------------

A rising foreign-born share of the population may reflect expanding
economic opportunities for both high-skilled and low-skilled
workers.[^7] That share of the population has grown in all three of the
most populous metro parishes since 2000, led by a 5.4 percentage point
gain in Jefferson Parish. By 2020, fully 12.9 percent of Jefferson
Parish population was foreign-born, similar to the U.S. share. In
Orleans and in St. Tammany parishes the foreign-born share of the
population increased by 1.6 percentage points between 2000 and 2020.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/forbor-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Like the foreign-born population, a rising share of the population who
moved into Orleans Parish in the past year may reflect expanding
economic opportunities. The most frequent reason people move long
distances, such as from one state to another state, is for job
opportunities.[^8] In addition, the young and well-educated are more
likely than others to move long distances.[^9] In 2020, 6 percent of the
population in Orleans Parish had moved into the parish in the past year,
up from 3 percent in 2004. Over 57 percent of the new movers into
Orleans Parish came from outside the state of Louisiana. In Jefferson
Parish, the share of the population who were new movers into the parish
was 5 percent in 2004, and has not significantly changed.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/mob-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Notes**: Share not included in the bar chart represents the population who lived in the same house one year ago (non-movers). For people living in the same house as a year ago, the difference between 2004 and 2019 is not significant at the 95% confidence interval for Orleans Parish. It is significant for all other geographies

**Notes**: Also, 2004 data is not available for St. Tammany Parish.

&ast; = Difference between 2004 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2004 and 2019.</p>
</div>

Homeownership {#homeownership}
-------------

Homeownership rates across the U.S. have fallen since 2000 from 66 to 64
percent in 2020. Homeownership rates have held steady in St. Tammany
around 80 percent since 2000. In contrast, homeownership rates in New
Orleans have increased slightly, but still are a much lower 50 percent.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/ho-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 1999 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Homeowners without a mortgage own their homes free and clear of any type
of loan. A high share of such homeowners usually indicates residents
living in the same house for long periods of time, and helps shield
neighborhoods from foreclosures. The proportion of metro area homeowners
without a mortgage has increased from 35 to 44 percent between 2000 and
2020, driven by changes in Orleans and Jefferson. The share of
homeowners without a mortgage jumped from 33 to 43 percent in Orleans
and from 35 to 48 percent in Jefferson. One reason for the surge may be
that homeowners who returned after Katrina used insurance or Road Home
proceeds to pay off their mortgage principal. In fact, Orleans and
Jefferson received the first and second largest number of Road Home
Option 1 grants among all Louisiana parishes.[^10]

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/honomo-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Housing costs and affordability, housing stock, and commuting {#housing-costs-and-affordability-housing-stock-and-commuting}
-------------------------------------------------------------

High housing costs can limit a region’s ability to attract and retain
the workforce essential for a healthy economy.[^11] Severe housing cost
burdens of more than 50 percent of household income indicate a serious
problem in housing affordability. In 2004, the share of severely
cost-burdened renters in New Orleans and the U.S. was 24 percent. In the
16 years since, that share has spiked to 34 percent in Orleans while
remaining at 24 percent nationally. In Jefferson Parish, the share of
renters paying more than 50 percent of household income on housing and
utilities is 25 percent in 2020.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/rentbur-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Notes**: 2004 data is not available for St. Tammany Parish

&ast; = Difference between 2004 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2004 and 2019.</p>
</div>

The share of homeowners paying more than 50 percent of household income
on their mortgage, taxes, utilities, and insurance is 1.4 percentage
points less in metro area since 2004. There is a clear gap between the
rate of housing cost burden for renters vs. homeowners, and that gap has
widened.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/hoburGraphic-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Notes**: 2004 data is not available for St. Tammany Parish

&ast; = Difference between 2004 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2004 and 2019.</p>
</div>

The surge in the share of severely cost-burdened renters in New Orleans
is reflective of the surge in the median gross rent (rent plus
utilities) in the city. From 2004 to 2020, monthly rent plus utilities
rose from $758 to $1,010 in New Orleans, a 33 percent increase after
adjusting for inflation. Meanwhile, median gross rents increased 19
percent metrowide compared to 18 percent nationwide.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/medrent-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

**Notes**: 2004 data is not available for St. Tammany Parish

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2004 and 2019.</p>
</div>

America’s aging housing stock represents both a potential problem and an
opportunity. Older homes are less energy-efficient and more expensive to
maintain.[^12] Moreover, research has shown that lead poisoning in
children is correlated strongly with residing in pre-1950 homes.[^13]
Conversely, in New Orleans, many older homes are protected by
preservation laws that have helped retain the historic character of the
city.

In Orleans Parish, fully 40 percent of all housing units are in pre-1950
structures. Meanwhile, in Jefferson Parish, 76 percent of the housing
stock was built in the 1950s, 1960s, 1970s, and 1980s, and just 16
percent of housing stock has been built since 1990. In contrast, in
St. Tammany, the majority of housing units are in structures that have
been built since 1990.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/yrbuilt-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between indicated measure and the U.S. is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from American Community Survey 2019.</p>
</div>

A metro region is partially defined by the commuting patterns of its
residents. Commuting has been studied more and more as the costs and
burdens (physical, mental, familial, etc.) are uncovered. In addition,
studies have shown that younger generations are less likely to drive
cars, more likely to bike, and more likely to move into urban
cores.[^14]

The share of commuters in New Orleans using public transportation
declined sharply from 13 percent in 2000 to 5 percent in 2020, while the
share in Jefferson Parish has fallen from 2 percent in 2000, to 1
percent of commuters using public transportation in 2020. But the share
of bike commuters in New Orleans rose to 3 percent. An analysis of 2017
ACS data found that New Orleans had the the fifth highest share of bike
commuting of the largest 70 cities nationwide.[^15] Meanwhile, the metro
share of carpoolers fell from 15 percent in 2000 to 10 percent in 2020,
as did the Orleans Parish share from 16 percent to 10 percent.

The percentage of workers who commute by driving alone has increased
within the metro region since 2000 from 73 percent to 77 percent, driven
by a 60 to 67 percent rise in Orleans Parish. This goes against the
national trend, where the share driving alone remained steady between
2000 and 2020 and where public transit use has also remained steady.

<img src="http://www.datacenterresearch.org/a/wp-content/uploads/2021/07/commute-1.png" style="display: block; margin: auto;" />

<div class="source" style="clear: left; padding-left: 30px;" markdown="1"> 

&ast; = Difference between 2000 and 2019 is not significant at 95% confidence interval.

**Source**: The Data Center analysis of U.S. Census Bureau data from Census 2000 SF3 and American Community Survey 2019.</p>
</div>

Data Sources / Methodology {#data-sources-methodology}
--------------------------

Data on race/ethnicity and age is from the Census Bureau vintage 2019
population estimates and Census 2000 Summary File 1 (SF1). Other
demographic data is from the Census 2000 Summary File 3 (SF3) and
American Community Survey 2004, and 2020 (single-year files).

Statistical tests of significance were computed at the 95% confidence
level for all data from the American Community Survey and Census 2000
SF3. An “\*” indicates that differences between two time periods or
geographies are not significant, and therefore are the result of
sampling variability rather than real change in characteristics of the
population.

The significance tests require both estimates and their standard errors.
Standard errors for the ACS estimates were calculated using formulas
from section 7, “Understanding Error and Determining Statistical
Significance” available at:
<https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018.pdf>.

Standard errors for Census 2000 SF3 data were calculated using formulas
from Chapter 8 of the Technical Documentation available at:
<http://www.census.gov/prod/cen2000/doc/sf3.pdf>.

Standard errors for Census 2000 and Census 2010 SF1 data are zero.

Endnotes {#endnotes}
--------

[^1]: The eight-parish New Orleans metro includes Jefferson, Orleans,
    Plaquemines, St. Bernard, St. Charles, St. James, St. John the
    Baptist, and St. Tammany. From 2003-2012, the New Orleans metro was
    comprised of 7 parishes, excluding St. James. This brief has updated
    all 2000 metro data to reflect an 8-parish definition, however
    previous versions of this report will have used the 7-parish metro
    definition and accompanying numbers.

[^2]: Throughout this brief “African American/black,” “Asian,” and
    “white” refer to individuals who report to be only one race and not
    Hispanic. However, “Hispanics” can be of any race(s).

[^3]: Taylor, P., Lopez M. H., Martinez, J., and Velasco, G. (2014).
    When Labels Don’t Fit: Hispanics and Their Views of Identity.
    Retrieved October 3, 2014 from
    <http://www.pewhispanic.org/2012/04/04/when-labels-dont-fit-hispanics-and-their-views-of-identity>.

[^4]: Plyer, A. and Ortiz, E. (2011). Drivers of housing demand:
    Preparing for the impending elder boom. Retrieved July 8, 2013, from
    <http://www.datacenterresearch.org/reports_analysis/drivers-of-housing-demand>.

[^5]: Julian, T. and Kominski, R. (2011). Education and synthetic
    work-life earnings estimates. Retrieved February 8, 2019 from
    <https://www.census.gov/library/publications/2011/acs/acs-14.html>;
    U.S. Department of Housing and Urban Development. (2012).
    Conceptualizing and measuring resilience. Retrieved September 18,
    2012 from
    <http://www.huduser.org/portal/periodicals/em/winter12/highlight2.html#title>.

[^6]: Vigdor J. and Ladd, H. (2010). Scaling the Digital Divide: Home
    Computer Technology and Student Achievement. Retrieved February 8,
    2019 from
    <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.849.6663&rep=rep1&type=pdf>.

[^7]: De Jong, G.F., Graefe, D.R., Hall, M., and Singer, A. (2001). The
    geography of immigrant skills: Educational profiles of metropolitan
    areas. Retrieved February 8, 2019 from
    <https://www.brookings.edu/research/the-geography-of-immigrant-skills-educational-profiles-of-metropolitan-areas/>.

[^8]: Based on surveys conducted by the U.S. Census Bureau,
    employment-related reasons are the most frequent reason for
    inter-county moves of greater than 50 miles. For example, among
    people in the United States who moved over 500 miles, 52 percent
    moved for an employment-related reason compared to 23 percent for a
    family-related reason and 22 percent for a housing-related reason.
    Employment-related reasons include a new job or job transfer, to
    look for work, to be closer to work, retirement, and other
    job-related reasons. See U.S. Census Bureau. (n.d.). Geographic
    Mobility: 2011 to 2012 (Table 27). Retrieved February 8, 2019 from
    <https://www.census.gov/topics/population/migration/data/tables/cps.2012.html>.

[^9]: Based on surveys conducted by the U.S. Census Bureau, people 25 to
    29 years old were more likely than other age groups to be movers
    from a different county, state, region, or country. And people with
    a professional or graduate degree were also more likely than other
    educational groups to be movers from a different county, state,
    region, or country. See U.S. Census Bureau. (n.d.). Geographic
    Mobility: 2011 to 2012 (Table 1). Retrieved September 23, 2013 from
    <https://www.census.gov/topics/population/migration/data/tables/cps.2012.html>.

[^10]: State of Louisiana Office of Community Development. (2013). The
    Homeowner Assistance Program Week 375 Situation & Pipeline Report.

[^11]: Plyer, A., Ortiz, E., and Pettit, K. (2009). Post-Katrina housing
    affordability challenges continue in 2008, worsening among Orleans
    Parish very low income renters. Retrieved September 17, 2013 from
    <http://www.datacenterresearch.org/reports_analysis/housing-affordability>.

[^12]: Joint Center for Housing Studies of Harvard University (2013).
    The US Housing Stock: Ready for Renewal. Retrieved October 10, 2014
    from
    <http://www.jchs.harvard.edu/sites/jchs.harvard.edu/files/harvard_jchs_remodeling_report_2013.pdf>.

[^13]: Roberts, J.R., Hulsey, T.C., Curtis, G.B., and Reigart, J.R.
    (2003). Using Geographic Information Systems to Assess Risk for
    Elevated Blood Lead Levels in Children. Retrieved October 10, 2014.
    <http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1497528/pdf/12766217.pdf>

[^14]: Davis, B., Dutzik, T. (2012). Transportation and the New
    Generation: Why Young People Are Driving Less and What It Means for
    Transportation Policy. Retrieved October 10, 2014 from
    <http://www.uspirg.org/sites/pirg/files/reports/Transportation%20%26%20the%20New%20Generation%20vUS_0.pdf>.

[^15]: The League of American Bicyclists. (2017). Where We Ride:
    Analysis of Bicycle Commuting in American Cities. Retrieved December
    21, 2018 from
    <https://bikeleague.org/sites/default/files/Where_We_Ride_2017_KM_0.pdf>
