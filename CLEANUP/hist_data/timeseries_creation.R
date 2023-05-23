# In this document, I create time series by race data for Bach, MedHHInc, Pov, Childpov, and Homeownership.
# I pulled this data from IPUMS NHGIS and we can continue to add the new year to it in the main data-pull.R file.

#### Bach over time by race ####

#Historial Educational Attainment. 1980-2000 from NHGIS
Bach80 <- read_csv("raw_data/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("raw_data/nhgis0095_ds123_1990_county.csv")
Bach00 <- read_csv("raw_data/nhgis0095_ds151_2000_county.csv")

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
            BlackBach = E34013 + E34014,
            totBlack = E34008 + E34009 + E34010 + E34011 + E34012 + E34013 + E34014,
            HispBach = E35006 + E35007,
            totHisp = E35001 + E35002 + E35003 + E35004 + E35005 + E35006 + E35007,
            pctTotalBach = totBach / totpop,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90Wht <- read_csv("raw_data/nhgis0009_ds125_1990_county.csv")
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
         Whitemoeprop = moeprop(y = TotalWhite2000, moex = WhiteBachMOE, moey = TotalWhiteMOE, p = pctWhiteBach)) %>%
  filter(place == "071") %>%
  select(pctWhiteBach, Whitemoeprop)



Bach00 <- Bach00Raw %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, totpop, designfac = 1.3),
            totpopMOE = moe2000(est = totpop, totpop, designfac = 1.3),
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
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            Blackmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            Hispmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
  )%>% cbind(Bach00Wht) %>%
  pivot_longer(cols = c(pctTotalBach:Hispmoeprop, pctWhiteBach, Whitemoeprop), values_to = "val") %>% 
  select(year, val, name)

#pulling ACS1 for 2010 data
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
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp,
            
            TotalBachmoeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
            Whitemoeagg = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
            Blackmoeagg = moeagg(cbind(MaleBachMOE_blk , FemaleBachMOE_blk)),
            Hispmoeagg = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
            
            
            Totalmoeprop = moeprop(y=Total, moex = TotalBachmoeagg, moey = TotalMOE, p = pctTotalBach),
            Whitemoeprop = moeprop(y=Total_wht, moex = Whitemoeagg, moey = TotalMOE_wht, p = pctWhiteBach),
            Blackmoeprop = moeprop(y=Total_blk, moex = Blackmoeagg, moey = TotalMOE_blk, p = pctBlackBach),
            Hispmoeprop = moeprop(y=Total_hisp, moex = Hispmoeagg, moey = TotalMOE_hisp, p = pctHispBach)) %>%
  pivot_longer(cols = c(pctTotalBach:pctHispBach, Totalmoeprop:Hispmoeprop), values_to = "val") %>% 
  select(year, val, name)

#joining the TS data
bachhist <- rbind(Bach80, Bach90, Bach00, Bach10) %>%
  mutate(race = case_when(grepl("Total", name) ~ "All",
                         grepl("White", name) ~ "White, non-Hispanic",
                         grepl("Black", name) ~ "Black",
                         grepl("Hisp", name) ~ "Hispanic, any race"),
         var = case_when(grepl("pct", name) ~ "percent",
                         grepl("moe", name) ~ "MOE")) 
#save, then we can join the current year in the data-pull.R file.
save(bachhist, file = "bachhist.RData")

#### Med HH Inc over time by race ####

medhh_unadjusted <- read_xlsx("raw_data/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute(race = `...1`,
            race = case_when(race == "Overall" ~ "All",
                             race == "White, Not Hispanic" ~ "White, non-Hispanic",
                             race == "Hispanic, Any Race" ~ "Hispanic, any race",
                             T ~ race),
            `1979_dollars` = as.character(`1979 (1979$)`),
            `1989_dollars`= as.character(`1989 (1989$)`),
            `1999_dollars` = as.character(`1999 (1999$)`),
            `1999_MOE` = as.character(moe2000(as.numeric(`1999_dollars`), 215091, designfac = 1.2)), 
            `2010_dollars` = as.character(`2010 (2010$)...5`),
            `2010_MOE` = as.character(`2010 (2010$)...6`)) %>% filter(`2010_dollars` != "Estimate") %>%
  pivot_longer(-race, names_to = c("year", "var"), names_sep = "_", values_to = "val") %>%
  select(year, val, race, var) %>%
  mutate(val = ifelse(is.na(val) == T, 15605,val)) %>%  #this line is to use White Alone ( including hispanics in 1979)
  filter(race != "White, Alone")
#save, then we can join the current year in the data-pull.R file.
save(medhh_unadjusted, file = "medHHhist.RData")

#### Poverty over time by race ####
pov80 <- read_csv("raw_data/nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("raw_data/nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("raw_data/nhgis0092_ds140_1990_county.csv")
pov00 <- read_csv("raw_data/nhgis0092_ds151_2000_county.csv")


pov80 <- pov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1980,
            totpov = DI9002 + DI9004 + DI9006 + DI9008 + DI9010,
            Whitepov = DI9002,
            Blackpov = DI9004,
            totpop = sum(c_across(DI9001:DI9010), na.rm = T),
            totWhitepop = DI9001 + DI9002,
            totBlackpop= DI9003 + DI9004,
            pctTotalpov = totpov / totpop,
            pctWhitepov = Whitepov / totWhitepop,
            pctBlackpov = Blackpov / totBlackpop) %>%
  pivot_longer(cols = pctTotalpov:pctBlackpov, values_to = "val") %>%
  select(year, val, name)

hisppov80 <- hisppov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1980,
            Hisppov = DJA002,
            totHisppop = DJA001 + DJA002,
            pctHisppov = Hisppov / totHisppop,
            name = "pctHisppov") %>% 
  select(year, val = pctHisppov, name)

pov90 <- pov90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1990,
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
            pctHisppov = Hisppov / totHisppop) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

pov00Wht <- wholivesdatapull(variables = c('P001001', 'P148I001', 'P159I002'),
                             names = c('TotalPop2000', "TotalWhitepop", "Whitepov"),
                             censusname = "dec/sf3",
                             year = 2000) %>%
  filter(place == "071") %>%
  mutate(pctWhitepov = Whitepov / TotalWhitepop,
         WhitepovMOE = moe2000(Whitepov, TotalPop2000, designfac = 1.5),
         TotalWhiteMOE = moe2000(TotalWhitepop, TotalPop2000, designfac = 2),
         Whitemoeprop = moeprop(y = TotalWhitepop, moex = WhitepovMOE, moey = TotalWhiteMOE, p = pctWhitepov)) %>% 
  select(pctWhitepov, Whitemoeprop)


pov00 <- pov00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 2000,
            totpov = GTV001 + GTV003 + GTV005 + GTV007 + GTV009 + GTV011 + GTV013,
            totpop = sum(c_across(GTV001:GTV014),na.rm = T),
            totpovMOE = moe2000(est = totpov, n = totpop, designfac = 1.3),
            totpopMOE = moe2000(est = totpop, n = totpop, designfac = 1.3),
            Blackpov = GTV003,
            totBlackpop = GTV003 + GTV004,
            BlackpovMOE = moe2000(est = Blackpov, n = totpop, designfac = 1.3),
            totBlackpopMOE = moe2000(est = totBlackpop, n = totpop, designfac = 1.3),
            Hisppov = GTY001,
            totHisppop = GTY001 + GTY002,
            HisppovMOE = moe2000(est = Hisppov, n = totpop, designfac = 1.3),
            totHisppopMOE = moe2000(est = totHisppop, n = totpop, designfac = 1.3),
            pctTotalpov = totpov / totpop,
            pctBlackpov = Blackpov / totBlackpop,
            pctHisppov = Hisppov / totHisppop,
            
            Totalmoeprop = moeprop(y = totpop, moex = totpovMOE, moey = totpopMOE, p = pctTotalpov),
            Blackmoeprop = moeprop(y = totBlackpop, moex = BlackpovMOE, moey = totBlackpopMOE, p = pctBlackpov),
            Hispmoeprop = moeprop(y = totHisppop, moex = HisppovMOE, moey = totHisppopMOE, p = pctHisppov)
) %>%
  cbind(pov00Wht) %>%
  pivot_longer(cols = c(pctTotalpov:Hispmoeprop, pctWhitepov, Whitemoeprop), values_to = "val") %>% 
  select(year, val, name)

# getting 2010 and ACS1

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
            pctHisppov = BelowPov_hisp / Total_hisp,
            
            Totalmoeprop = moeprop(Total, BelowPovMOE, TotalMOE, pctTotalpov),
            Blackmoeprop = moeprop(Total_blk, BelowPovMOE_blk, TotalMOE_blk, pctBlackpov),
            Whitemoeprop = moeprop(Total_wht, BelowPovMOE_wht, TotalMOE_wht, pctWhitepov),
            Hispmoeprop = moeprop(Total_hisp, BelowPovMOE_hisp, TotalMOE_hisp, pctHisppov)) %>%
  pivot_longer(cols = pctTotalpov:Hispmoeprop, values_to = "val") %>% 
  select(year, val, name)

povhist <- rbind(pov80, hisppov80, pov90, pov00, pov10) %>% 
  mutate(race = case_when(grepl("Total", name) ~ "All",
                         grepl("White", name) ~ "White,\nnon-Hispanic",
                         grepl("Black", name) ~ "Black",
                         grepl("Hisp", name) ~ "Hispanic,\nany race"),
         var = case_when(grepl("pct", name) ~ "percent",
                         grepl("moe", name) ~ "MOE")) %>%
  select(-name)
                                                                                                     
#save, then we can join the current year in the data-pull.R file.
save(povhist, file = "povhist.RData")

#### Child poverty over time by race ####
#came from Prosperity index. Doesn't need any manipulating.


#### Homeownership over time by race ####
#came from Prosperity Index.  Doesn't need any manipulating.

#### Mobility Data 2004
ACScounty_04 <- read_csv("raw_data/ACS_2004_050.csv") 
ACSUS_04 <- read_csv("raw_data/ACS_2004_010.csv")
ACSmetro_04 <- read_csv("raw_data/ACS_2004_380.csv")
mob_04 <- ACScounty_04 %>% rbind(ACSUS_04, ACSmetro_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
           grepl("22103", geoid) | #St. Tammany isn't in 2004 ACS for these.
           grepl("01000US", geoid) |
           grepl("38000US5560", geoid)) %>% 
  filter((tblid == "B07003" & (order == 1 | 
                                 order == 4 |
                                 order == 7 | 
                                 order == 10 | 
                                 order == 13 | 
                                 order == 16) )) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States",
                               grepl("38000US5560", geoid) ~ "New Orleans Metro Area"),
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
  mutate(mobabroadpct = cest_TotMovedfromAbroad / cest_Total,
         mobabroadpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedfromAbroad,moey = MOE_Total,p=mobabroadpct),
         mobStatespct = cest_TotMovedbtwnStates / cest_Total,
         mobStatespctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedbtwnStates,moey = MOE_Total,p=mobStatespct),
         difparishpct = cest_TotMovedinState / cest_Total,
         difparishpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedinState,moey = MOE_Total,p=difparishpct),
         withinparishpct = cest_TotMovedinCty / cest_Total,
         withinparishpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedinCty,moey = MOE_Total,p=withinparishpct),
         samehousepct = cest_TotSameHouse / cest_Total,
         samehousepctMOE = moeprop(y=cest_Total, moex = MOE_TotSameHouse,moey = MOE_Total,p=samehousepct)) %>%
  select(placename, 
         mobabroadpct, mobabroadpctMOE, 
         mobStatespct, mobStatespctMOE, 
         difparishpct, difparishpctMOE,
         withinparishpct, withinparishpctMOE,
         samehousepct, samehousepctMOE)
save(mob_04, file = "mobRaw2004.RData")



housing <-  ACScounty_04 %>% rbind(ACSUS_04, ACSmetro_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
           grepl("22103", geoid) | #St. Tammany isn't in 2004 ACS for these.
           grepl("01000US", geoid) |
           grepl("38000US5560", geoid)) %>% 
  filter((tblid == "B25064" & order == 1) |
           (tblid == "B25070" & (order == 1 | order == 10 | order == 11 ) |
              (tblid == "B25091" & (order == 1 | order == 11| order == 12| order == 22| order == 23)))) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States",
                               grepl("38000US5560", geoid) ~ "New Orleans Metro Area"),
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
  mutate(rentburpct = (cest_rentcostburden) / (cest_totrenters - cest_renters_notcomp),
         MOE_rentersagg = moeagg(cbind(MOE_totrenters,MOE_renters_notcomp)),
         rentburpctMOE = moeprop(y = cest_totrenters- cest_renters_notcomp, moex = MOE_rentcostburden, moey = MOE_rentersagg, p = rentburpct),
         medgrossrent = cest_medgrossrent,
         medgrossrentMOE = MOE_medgrossrent,
         hoburpct = (cest_hocostburden+cest_hocostburden_nomort) / (cest_tothomeowners - (cest_ho_notcomp+cest_ho_notcomp_nomort)),
         MOE_hoburagg = moeagg(cbind(MOE_hocostburden,MOE_hocostburden_nomort)),
         MOE_hoagg = moeagg(cbind(MOE_tothomeowners,MOE_ho_notcomp,MOE_ho_notcomp_nomort)),
         hoburpctMOE = moeprop(y = (cest_tothomeowners- (cest_ho_notcomp+cest_ho_notcomp_nomort)), moex = MOE_hoburagg, moey = MOE_hoagg, p = hoburpct)) %>%
  select(placename, medgrossrent, medgrossrentMOE, rentburpct, rentburpctMOE, hoburpct, hoburpctMOE)

save(housing, file = "housingRaw2004.RData")