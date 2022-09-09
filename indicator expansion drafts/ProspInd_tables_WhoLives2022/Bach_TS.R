#NHGIS data - HT re-downloaded because the other files had age and it would have been more time-consuming

Bach80 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds123_1990_county.csv")
Bach00 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

Bach80 <- Bach80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1980,
            WhiteBach = DHN005,
            totWhite = DHN001 + DHN002 + DHN003 + DHN004 + DHN005,
            BlackBach = DHN010,
            totBlack = DHN006 + DHN007 + DHN008 + DHN009 + DHN010,
            HispBach = DHO005,
            totHisp = DHO001 + DHO002 + DHO003 + DHO004 + DHO005,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctWhiteBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90 <- Bach90 %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990,
            WhiteBach = E34006 + E34007,
            totWhite = E34001 + E34002 + E34003 + E34004 + E34005 + E34006 + E34007,
            BlackBach = E34013 + E34014,
            totBlack = E34008 + E34009 + E34010 + E34011 + E34012 + E34013 + E34014,
            HispBach = E35006 + E35007,
            totHisp = E35001 + E35002 + E35003 + E35004 + E35005 + E35006 + E35007,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctWhiteBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach00 <- Bach00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            WhiteBach = GRW006 + GRW007 + GRW013 + GRW014, 
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp)%>%
  pivot_longer(cols = pctWhiteBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach_TS <- rbind(Bach80, Bach90, Bach00) %>% mutate(var = case_when(name == "pctWhiteBach" ~ "White, non-Hispanic",
                                                                           name == "pctBlackBach" ~ "Black",
                                                                           name == "pctHispBach" ~ "Hispanic, any race")) %>% select(-name)
write_csv(Bach_TS, "educationalAttainment_byrace.csv")
