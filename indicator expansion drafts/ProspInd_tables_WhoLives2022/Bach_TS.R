#NHGIS data - HT re-downloaded because the other files had age and it would have been more time-consuming

Bach80 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds123_1990_county.csv")
Bach00 <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

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
            WhiteBach = E34006 + E34007,
            totWhite = E34001 + E34002 + E34003 + E34004 + E34005 + E34006 + E34007,
            BlackBach = E34013 + E34014,
            totBlack = E34008 + E34009 + E34010 + E34011 + E34012 + E34013 + E34014,
            HispBach = E35006 + E35007,
            totHisp = E35001 + E35002 + E35003 + E35004 + E35005 + E35006 + E35007,
            pctTotalBach = totBach / totpop,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach00 <- Bach00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            WhiteBach = GRW006 + GRW007 + GRW013 + GRW014, 
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            pctTotalBach = totBach / totpop,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp)%>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)


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


#joining the TS data
Bach_TS <- rbind(Bach80, Bach90, Bach00, Bach10, Bach16) %>%
  mutate(var = case_when(name == "pctTotalBach" ~ "All",
                         name == "pctWhiteBach" ~ "White,\nnon-Hispanic",
                         name == "pctBlackBach" ~ "Black",
                         name == "pctHispBach" ~ "Hispanic,\nany race")) %>% select(-name)

write_csv(Bach_TS, "indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment_byrace.csv")
