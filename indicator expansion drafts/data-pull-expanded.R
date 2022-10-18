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
medhhRaw <- wholivesdatapull(medhhvars, medhhnames, year = 2021)
save(medhhRaw, file = "indicator expansion drafts/medhhRaw.RData")

medhh_unadjusted <- read_xlsx("indicator expansion drafts/ProspInd_tables_WhoLives2022/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute("var" = `...1`,
            `1979` = as.character(`1979 (1979$)`),
            `1989`= as.character(`1989 (1989$)`),
            `1999` = as.character(`1999 (1999$)`),
            `2010` = `2010 (2010$)...5`,
            `2016` = `2016 (2016$)...7`,
            `2010MOE` = `2010 (2010$)...6`,
            `2016MOE` = `2016 (2016$)...8`) %>% na.omit()
medhh_unadjusted <- medhh_unadjusted %>% pivot_longer(cols = `1979`:`2016`, names_to = "Year") %>% select(-c(`2010MOE`, `2016MOE`))

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
bachRaw <- wholivesdatapull(bachvars, bachnames, year = 2021)
save(bachRaw, file = "indicator expansion drafts/bachRaw.RData")

#Historial Educational Attainment. 1980-2000 from NHGIS
Bach80 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds123_1990_county.csv")
Bach00Raw <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

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

Bach90Wht <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0009_ds125_1990_county.csv")
Bach90Wht <- Bach90Wht %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990, 
            WhiteBach = FF5ABR012 + FF5ABR013 + FF5ABR014 + FF5ABR015 + FF5ABR027 + FF5ABR028 + FF5ABR029 + FF5ABR030,
            totWhite = sum(c_across(FF5ABR001:FF5ABR030), na.rm = T),
            pctWhiteBach = WhiteBach / totWhite) %>% 
  pivot_longer(cols = pctWhiteBach, values_to = "val") %>%
  select(year, val, name)

Bach90 <- rbind(Bach90, Bach90Wht)

Bach00Wht <- wholivesdatapull(variables = c('P001001', "P148I001", 'P148I008', 'P148I009', 'P148I016', 'P148I017'),
                              names = c("Totalpop2000", "TotalWhite2000", "MaleBach", "MaleGradProf", "FemaleBach", "FemaleGradProf"),
                              censusname = "dec/sf3",
                              year = 2000) %>%
  mutate(WhiteBach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         WhiteBachMOE = moe2000(WhiteBach, Totalpop2000, designfac = 1.2),
         TotalWhiteMOE = moe2000(TotalWhite2000, Totalpop2000, designfac = 1.2), #is this the correct way to do this for whites 25+? Do I use race/eth designfac? But it's for educational attainment pop.
         pctWhiteBach = WhiteBach / TotalWhite2000,
         WhiteBachmoeprop = moeprop(y = TotalWhite2000, moex = WhiteBachMOE, moey = TotalWhiteMOE, p = pctWhiteBach)) %>% filter(place == "071") %>% select(pctWhiteBach, WhiteBachmoeprop)

Bach00 <- Bach00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, totpop, designfac = 1.3),
            totpopMOE = moe2000(est = totpop, totpop, designfac = 1.3),
            #WhiteBach = GRW006 + GRW007 + GRW013 + GRW014, 
            #WhiteBachMOE = moe2000(est = WhiteBach, totpop, designfac = 1.3),
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            totWhiteMOE = moe2000(est = totWhite, totpop, designfac = 1.3),
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, totpop, designfac = 1.3),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, totpop, designfac = 1.3),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, totpop, designfac = 1.3),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, totpop, designfac = 1.3),
            pctTotalBach = totBach / totpop,
            #pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            #WhiteBachmoeprop = moeprop(totWhite,WhiteBachMOE,totWhiteMOE,pctWhiteBach),
            BlackBachmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            HispBachmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
            )%>% cbind(Bach00Wht) %>%
  pivot_longer(cols = c(pctTotalBach:pctHispBach, pctWhiteBach), values_to = "val") %>% 
  select(year, val, name)

Bach00MOE <- Bach00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, n = 484674, designfac = 1.2),
            totpopMOE = moe2000(est = totpop, n = 484674, designfac = 1.2),
            #WhiteBach = GRW006 + GRW007 + GRW013 + GRW014, 
            #WhiteBachMOE = moe2000(est = WhiteBach, n = 484674, designfac = 1.2),
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            totWhiteMOE = moe2000(est = totWhite, n = 484674, designfac = 1.2),
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, n = 484674, designfac = 1.2),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, n = 484674, designfac = 1.2),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, n = 484674, designfac = 1.2),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, n = 484674, designfac = 1.2),
            pctTotalBach = totBach / totpop,
            #pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            #WhiteBachmoeprop = moeprop(totWhite,WhiteBachMOE,totWhiteMOE,pctWhiteBach),
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

Bach10Raw <- wholivesdatapull(bachvars10, bachnames10, year = 2010)

Bach10 <- Bach10Raw %>%

  filter(place == "071") %>% 
  transmute(year = 2010,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)


Bach10MOE <- Bach10Raw %>%

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

bachRaw <- bachRaw %>% ## something's going on here where this is names the same as bachRaw above and so can't be used in the analysis piece as it was before
  filter(place == "071") %>% 
  transmute(year = 2021,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

#joining the TS data
Bach_TS <- rbind(Bach80, Bach90, Bach00, Bach10, Bach16, bachRaw) %>%
  mutate(var = case_when(name == "pctTotalBach" ~ "All",
                         name == "pctWhiteBach" ~ "White,\nnon-Hispanic",
                         name == "pctBlackBach" ~ "Black",
                         name == "pctHispBach" ~ "Hispanic,\nany race")) %>% select(-name)
#write_csv(Bach_TS, "ProspInd_tables_WhoLives2022/hist_educationalAttainment.csv")


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
povRaw <- wholivesdatapull(povvars, povnames, year = 2021)
save(povRaw, file = "indicator expansion drafts/povRaw.RData")

# Creating a time series for total poverty - had to get 1980-2000 from IPUMS NHGIS

pov80 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds140_1990_county.csv")
pov00Raw <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")

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

pov00Wht <- wholivesdatapull(variables = c('P001001', 'P148I001', 'P159I002'),
                             names = c('TotalPop2000', "TotalWhitepop", "Whitepov"),
                             censusname = "dec/sf3",
                             year = 2000) %>%
  filter(place == "071") %>%
  mutate(pctWhitepov = Whitepov / TotalWhitepop,
         WhitepovMOE = moe2000(Whitepov, TotalPop2000, designfac = 1.5),
         TotalWhiteMOE = moe2000(TotalWhitepop, TotalPop2000, designfac = 2),
         Whitemoeprop = moeprop(y = TotalWhitepop, moex = WhitepovMOE, moey = TotalWhiteMOE, p = pctWhitepov)) %>% select(pctWhitepov, Whitemoeprop)

pov00 <- pov00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
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
                                                                           pctHisppov = Hisppov / totHisppop) %>% 
  cbind(pov00Wht) %>%
  pivot_longer(cols = c(pctTotalpov:pctHisppov, pctWhitepov), values_to = "val") %>% select(year, val, name)

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

pov10Raw <- wholivesdatapull(povvars10, povnames10, year = 2010)

pov10 <- pov10Raw %>%

  filter(place == "071") %>% 
  transmute(place = place,
            year = 2010,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(place, year, val, name)


pov10MOE <- pov10Raw %>%

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

pov16_raw <- wholivesdatapull(povvars16, povnames16, year = 2016)

pov16 <- pov16_raw %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

#current year
povraw <- povRaw %>%
  filter(place == "071") %>% 
  transmute(year = 2021, 
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)


pov_TS <- rbind(pov80, hisppov80, pov90, pov00, pov10, pov16, povraw) %>% mutate(var = case_when(name == "pctTotalpov" ~ "All",
                                                                                         name == "pctWhitepov" ~ "White,\nnon-Hispanic",
                                                                                         name == "pctBlackpov" ~ "Black",
                                                                                         name == "pctHisppov" ~ "Hispanic,\nany race")) %>% select(-name)


#write_csv(pov_TS, "ProspInd_tables_WhoLives2022/hist_pov.csv")


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
childpovRaw <- wholivesdatapull(childpovvars, childpovnames, year = 2021)
save(childpovRaw, file = "indicator expansion drafts/childpovRaw.RData")

childPovProspInd <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/childPov.csv")
childPov.histMOE = data.frame(race = c("Black","White,\nnon-Hispanic","Hispanic,\nany race","All"),
                             se2010 = c(0.023372212, 0.016043476, 0.104259483, 0.021288336),
                             se2000 = c(0.005216453, 0.006067795, 0.026554044, 0.004610543)) %>%
  mutate(moe2010 = se2010*1.645,
         moe2000 = se2000*1.645)

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
hoRaw <- wholivesdatapull(hovars, honames)
save(hoRaw, file = "indicator expansion drafts/hoRaw.RData")

homeownershipProspInd <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/homeownership.csv")
educationalAttainmentProspInd <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment.csv")
medHHincProspInd <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")


