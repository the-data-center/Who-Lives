bachrace <- bachraceRaw %>%
  mutate(totbach = MaleBach + FemaleBach + MaleGradProf + FemaleGradProf,
         totbach_blk = MaleBach_blk + FemaleBach_blk,
         totbach_wht = MaleBach_wht + FemaleBach_wht,
         totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
         totbach_asian = MaleBach_asian + FemaleBach_asian,
         pctbach = totbach / Total,
         pctbach_blk = totbach_blk / Total_blk,
         pctbach_wht = totbach_wht / Total_wht,
         pctbach_hisp = totbach_hisp / Total_hisp,
         pctbach_asian = totbach_asian / Total_asian,
         
         moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
         moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
         moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
         moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
         moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
         
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
         moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
         moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp),
         
         #stat test each race against each other within parishes.  Check archived if we want to compare each race to the overall parish value.
         sig_wht_blk = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_asian, moey = moeprop_asian),
         sig_blk_hisp = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_asian, moey = moeprop_asian),
         sig_hisp_asian = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach_asian, moey = moeprop_asian)) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>% select(-var) %>% 
  group_by(place, placename) %>%
  mutate(placename = case_when(placename == "New Orleans Metro Area" ~ "Metro",
                               T ~ placename),
         placename = case_when("no" %in% stat_race ~ paste0(placename, "*"),
                               T ~ placename)) %>% 
  ungroup() %>% 
  unique() %>%
  select(place, placename, pctbach_blk, pctbach_wht, pctbach_hisp, pctbach_asian) %>% #not includjng "pctbach"
  pivot_longer(-c(placename,place), names_to = c("val", "race"), names_sep = "_") %>%
  select(-val) %>%
  mutate(placename.fac = factor(placename, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "U.S.")), #need to figure out a better way to do this one...
         race = case_when(grepl("asian",race) ~ "Asian",
                          grepl("blk",race) ~ "Black",
                          grepl("hisp",race) ~ "Hispanic, any race",
                          grepl("wht", race) ~ "White, non-Hispanic"),
         var.fac = factor(race, levels = c("Black","White, non-Hispanic","Asian","Hispanic, any race")),
         val_lab = case_when(value == 0 ~ " ",
                             value != 0 ~ paste0(round.off(value*100), "%"))) %>% 
  unique()

bachraceCSV <- bachrace %>% 
  select(race, placename.fac, value) %>%
  pivot_wider(names_from = placename.fac, values_from = value) %>%
  write.csv("outputs/spreadsheets/bachrace.csv")

bachhist <- rbind(bachhist,
                  (bachrace %>% 
                     filter(place == "071") %>%
                     transmute(year = 2021,
                               val = value,
                               var = race))) #current year