pov80 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds140_1990_county.csv")
pov00 <- read_csv("C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")
#filter to Orleans Parish

pov80 <- pov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                           Whitepov = DI9002,
                                                                           Blackpov = DI9004,
                                                                           totWhitepop = DI9001 + DI9002,
                                                                           totBlackpop= DI9003 + DI9004,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop) %>% pivot_longer(cols = c(pctWhitepov, pctBlackpov), values_to = "val") %>% select(year, val, name)
hisppov80 <- hisppov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                                   Hisppov = DJA002,
                                                                                   totHisppop = DJA001 + DJA002,
                                                                                   pctHisppov = Hisppov / totHisppop,
                                                                                   name = "pctHisppov") %>% select(year, val = pctHisppov, name)
pov90 <- pov90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1990,
                                                                           Whitepov = EKT011,
                                                                           Blackpov = EKT012 + EKT017,
                                                                           Hisppov = EKT016  + EKT018 + EKT019 + EKT020,
                                                                           totWhitepop = EKT001 + EKT011,
                                                                           totBlackpop = EKT002 + EKT012 + EKT007 + EKT017,
                                                                           totHisppop = EKT006 + EKT008 + EKT009 + EKT010 + EKT016 + EKT018 + EKT019 + EKT020,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% pivot_longer(cols = pctWhitepov:pctHisppov, values_to = "val") %>% select(year, val, name)
pov00 <- pov00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
                                                                           Whitepov = GTV001,
                                                                           totWhitepop = GTV001 + GTV002,
                                                                           Blackpov = GTV003,
                                                                           totBlackpop = GTV003 + GTV004,
                                                                           Hisppov = GTY001,
                                                                           totHisppop = GTY001 + GTY002,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% pivot_longer(cols = pctWhitepov:pctHisppov, values_to = "val") %>% select(year, val, name)
# getting 2010 and 2016 from ACS1


View(load_variables(year = 2016, dataset = "acs1"))

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
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctWhitepov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

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

pov16 <- wholivesdatapull(povvars16, povnames16, year = 2016)

pov16 <- pov16 %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctWhitepov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)


pov_TS <- rbind(pov80, hisppov80, pov90, pov00, pov10, pov16) %>% mutate(var = case_when(name == "pctWhitepov" ~ "White, non-Hispanic",
                                                                           name == "pctBlackpov" ~ "Black",
                                                                           name == "pctHisppov" ~ "Hispanic, any race")) %>% select(-name)

write_csv(pov_TS, "C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/totalPov.csv")
