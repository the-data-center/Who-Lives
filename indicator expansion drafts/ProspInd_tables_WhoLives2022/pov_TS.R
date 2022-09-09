pov80 <- read_csv("nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("nhgis0092_ds140_1990_county.csv")
pov00 <- read_csv("nhgis0092_ds151_2000_county.csv")
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


pov_TS <- rbind(pov80, hisppov80, pov90, pov00) %>% mutate(var = case_when(name == "pctWhitepov" ~ "White, non-Hispanic",
                                                                           name == "pctBlackpov" ~ "Black",
                                                                           name == "pctHisppov" ~ "Hispanic, any race")) %>% select(-name)
View(pov_TS)
write_csv(pov_TS, "C:/Users/HaleighTomlin/OneDrive - Nonprofit Knowledge Works/Desktop/Who-Lives/indicator expansion drafts/ProspInd_tables_WhoLives2022/totalPov.csv")
