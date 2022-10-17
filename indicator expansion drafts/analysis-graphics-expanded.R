###
### Median household income ###
###
# load("indicator expansion drafts/medhhRaw.RData")
medhh <- medhhRaw  %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place, -placename, -placenames, -contains("MOE")) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val")


### Across geos median hh income bar chart ###
medhh.totals <- medhh%>%
  filter(var == "MedianHHIncome")%>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

medhh.race <- medhh %>%
  filter(var != "MedianHHIncome") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

medhh.raceGeos_chart <- medhh.race %>%
  ggplot(aes(place.fac, val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color='gray70') +    #bar outline
  geom_text(data = subset(medhh.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = paste0("$",scales::comma(val))),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=3,
            family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(medhh.race$var.fac)) +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= medhh.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("All:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3.5,family="Asap", color = "gray70") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= medhh.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("All:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3.5,family="Asap", color = "gray70") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= medhh.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("All:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3.5,family="Asap", color = "gray70") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= medhh.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("All:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3.5,family="Asap", color = "gray70") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= medhh.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("All:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3.5,family="Asap", color = "gray70") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(hjust = .5, size=16)) + 
  labs(title = "Median household income by race/ethnicity, 2021",
       x="",
       y="")
ggsave(medhh.raceGeos_chart,filename = "indicator expansion drafts/graphics/medhh.raceGeos.png",
       width = 10, height = 6, units = "in")


### Historical median hh income line chart ###

## median hh income adjusted to 2021 dollars HT ###

#using https://data.bls.gov/cgi-bin/cpicalc.pl from january to january
#1979 to 2021 = 3.83
#1989 to 2021 = 2.16
#1999 to 2021 = 1.59
#2010 to 2021 = 1.21
#2016 to 2021 = 1.10

medhhinc_adjusted21 <- medhh_unadjusted %>% mutate(value = as.numeric(value),
                                                   inc_adj21 = case_when(Year == 1979 ~ value * 3.83,
                                                                         Year == 1989 ~ value * 2.16,
                                                                         Year == 1999 ~ value * 1.59,
                                                                         Year == 2010 ~ value * 1.21,
                                                                         Year == 2016 ~ value * 1.10))
write_csv(medhhinc_adjusted21, "indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")

medhh.hist <- medhhinc_adjusted21 %>% 
  select(-value) %>%
  rename(val = inc_adj21) %>%
  mutate(Year = as.numeric(Year),
         var = ifelse(var == "Overall", "All", var),
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Hispanic,",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  bind_rows(medhh %>%
              filter(place.fac == "Orleans",
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2021, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "MedianHHIncome", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

medhh.hist_chart <- medhh.hist %>%
  filter(var != "All", Year != 2016) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = dollar_format(accuracy = 1), limits = c(0,90000), breaks = c(0,30000,60000,90000)) + 
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2021")) +
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(medhh.hist, Year %in% c("1979", "2021") & var != "All"), aes(x=Year,y=val, label = label_dollar(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Median household income by race/ethnicity in 2021 dollars, Orleans Parish",
       x="",
       y="") 
medhh.hist_chart
ggsave(medhh.hist_chart,filename = "indicator expansion drafts/graphics/medhh.hist.png",
       width = 8, height = 6, units = "in")

### stat testing ###

medhh.race_stattest <- medhhRaw %>%
  mutate(sig_wht_blk = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_blk, moey = MedianHHIncomeMOE_blk),
         sig_wht_hisp = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_wht_asian = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sig_wht_all = stattest(x=MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sig_blk_hisp = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
         sig_blk_asian = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sig_blk_all = stattest(x=MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sig_hisp_asian = stattest(x=MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y=MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
         sig_hisp_all = stattest(x=MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y=MedianHHIncome, moey = MedianHHIncomeMOE),
         sig_asian_all = stattest(x=MedianHHIncome_asian, moex = MedianHHIncomeMOE_asian, y=MedianHHIncome, moey = MedianHHIncomeMOE)
  ) %>%
  select(placename, contains("sig"))

medhh.hist_stattest.EST <- medhhRaw %>% 
  filter(place == "071") %>%
  select(-place,-placename,-contains("MOE")) %>%
  pivot_longer(everything(),names_to = "var",values_to = "val2021") %>%
  filter(var != "MedianHHIncome_asian") %>%
  mutate(race = case_when(var =="MedianHHIncome" ~ "Overall",
                          var =="MedianHHIncome_blk" ~ "Black",
                          var =="MedianHHIncome_wht" ~ "White, Alone",
                          var =="MedianHHIncome_hisp" ~ "Hispanic, Any Race"
                          )) %>%
  select(race, val2021)

medhh.hist_stattest.MOE <- medhhRaw %>% 
  filter(place == "071")%>%
  select(contains("MOE")) %>%
  pivot_longer(everything(),names_to = "var",values_to = "moe2021")%>%
  filter(var != "MedianHHIncomeMOE_asian") %>%
  mutate(race = case_when(var =="MedianHHIncomeMOE" ~ "Overall",
                          var =="MedianHHIncomeMOE_blk" ~ "Black",
                          var =="MedianHHIncomeMOE_wht" ~ "White, Alone",
                          var =="MedianHHIncomeMOE_hisp" ~ "Hispanic, Any Race"
  )) %>%
  select(race, moe2021)

meddhh.hist_withmoe <- medhh_unadjusted %>% 
  select(var,`1999`,`2010`,`2010MOE`) %>%
  mutate(adj1999 = as.numeric(`1999`) * 1.59,adj2010 = as.numeric(`2010`) * 1.21, adj2010moe = as.numeric(`2010MOE`) * 1.21) %>%
  mutate(adj1999moe = moe2000(adj1999, 215091, designfac = 1.2))

medhh.hist_stattest <- left_join(medhh.hist_stattest.EST,medhh.hist_stattest.MOE) %>%
  left_join(meddhh.hist_withmoe, by = c("race" = "var")) %>%
  mutate(sig_99_10= stattest(x=adj1999,moex = adj1999moe, y=adj2010,moey = adj2010moe),
         sig_99_21= stattest(x=adj1999,moex = adj1999moe, y=val2021,moey = moe2021),
         sig_10_21= stattest(x=adj2010,moex = adj2010moe, y=val2021,moey = moe2021)) %>%
  select(race, contains("sig"))

## stat test notes
medhh.race_note <- raceList(medhh.race_stattest)

###
### Educational attainment ###
###
# load("indicator expansion drafts/bachRaw.RData")
bach <- bachRaw %>%
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
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### Across geos educational attainment bar chart ###

bach.totals <- bach %>%
  filter(var == "pctbach") %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))


bach.race <- bach %>%
  filter(var!="pctbach") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))


bach.raceGeos_chart <- bach.race %>%
  ggplot(aes(x=place.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(bach.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(bach.race$var.fac)) +
  geom_segment(data= bach.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= bach.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= bach.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= bach.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= bach.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= bach.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= bach.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= bach.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= bach.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= bach.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Bachelor's degree or higher, adults 25 years or older by race/ethnicity, 2021",
       x="",
       y="")
ggsave(bach.raceGeos_chart,filename = "indicator expansion drafts/graphics/bach.raceGeos.png",
       width = 10, height = 6, units = "in")

### Historical educational attainment line chart ###

EduAtt <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment_byrace.csv")
EduAtt.hist <- EduAtt %>% 
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race"))) %>%
  bind_rows(bach.race %>%
              filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2021))

EduAtt.hist_chart <- EduAtt.hist %>%
  filter(var != "All", year != 2016) %>%
  ggplot()+
  geom_line(aes(x=year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(EduAtt.hist, year %in% c("1980", "2021") & var != "All"), aes(x=year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Bachelor's degree or higher, adults 25 years or older by race/ethnicity,\nOrleans Parish",
       x="",
       y="") + xlim(1980,2021)

ggsave(EduAtt.hist_chart,filename = "indicator expansion drafts/graphics/bach.hist.png",
       width = 8, height = 6, units = "in")

### stat test ### 

bach.race_stattest.data <-  bachRaw %>%
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
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))
bach.race_stattest <- bach.race_stattest.data%>%
  mutate(sig_wht_blk = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach_asian, moey = moeprop_asian),
         sig_wht_all = stattest(x=pctbach_wht, moex = moeprop_wht, y=pctbach, moey = moeprop),
         sig_blk_hisp = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach_asian, moey = moeprop_asian),
         sig_blk_all = stattest(x=pctbach_blk, moex = moeprop_blk, y=pctbach, moey = moeprop),
         sig_hisp_asian = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach_asian, moey = moeprop_asian),
         sig_hisp_all = stattest(x=pctbach_hisp, moex = moeprop_hisp, y=pctbach, moey = moeprop),
         sig_asian_all = stattest(x=pctbach_asian, moex = moeprop_asian, y=pctbach, moey = moeprop)
  ) %>%
  select(placename, contains("sig"))


bach.race_stattest.geoEST <- bach.race_stattest.data %>%
  select(place,pctbach_blk, pctbach_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "val")
bach.race_stattest.geoMOE <- bach.race_stattest.data %>%
  select(place,moeprop_blk, moeprop_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "moe")
bach.race_stattest.geo <- left_join(bach.race_stattest.geoEST,bach.race_stattest.geoMOE, by = "place") %>%
  pivot_wider(names_from = place,values_from = c(val, moe)) %>%
  mutate(sig_no_us = stattest(x=val_071, moex = moe_071, y = val_1, moey = moe_1),
         sig_no_metro =stattest(x=val_071, moex = moe_071, y = val_35380, moey = moe_35380),
         sig_no_stt = stattest(x=val_071, moex = moe_071, y = val_103, moey = moe_103),
         sig_no_jeff = stattest(x=val_071, moex = moe_071, y = val_051, moey = moe_051))


bach.hist_stattest <- left_join(Bach10MOE, Bach00MOE) %>%
  left_join(EduAtt.hist %>% filter(year == 2000) %>% transmute(est2000 = val, race = var)) %>%
  left_join(EduAtt.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "moe2021") %>%
              mutate(race = case_when(var =="moeprop" ~ "All",
                                      var =="moeprop_blk" ~ "Black",
                                      var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                      var =="moeprop_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(pctbach,pctbach_blk ,pctbach_wht ,pctbach_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "est2021") %>%
              mutate(race = case_when(var =="pctbach" ~ "All",
                                      var =="pctbach_blk" ~ "Black",
                                      var =="pctbach_wht" ~ "White,\nnon-Hispanic",
                                      var =="pctbach_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2021, moe2021),
         sig_10_21 = stattest(est2010, moe2010, est2021, moe2021)
         ) %>%
    select(race, contains("sig"))




  
###
### Poverty ###
###
# load("indicator expansion drafts/povRaw.RData")
pov <- povRaw %>%
  mutate(
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = BelowPov_asian / Total_asian,
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    # moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
    )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### Across geos pov bar chart ###
pov.totals <- pov %>% 
  filter(var == "pctpov") %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

pov.race <- pov %>%
  filter(var != "pctpov") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

pov.raceGeos_chart <- pov.race %>%
  ggplot(aes(x=place.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(pov.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(pov.race$var.fac)) +
  geom_segment(data= pov.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= pov.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= pov.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= pov.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= pov.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= pov.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= pov.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= pov.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= pov.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= pov.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Poverty rate by race/ethnicity, 2021",
       x="",
       y="")
ggsave(pov.raceGeos_chart,filename = "indicator expansion drafts/graphics/pov.raceGeos.png",
       width = 10, height = 6, units = "in")


### Historical total pov line chart ###

totalPov <- read_csv("indicator expansion drafts/ProspInd_tables_WhoLives2022/totalPov.csv")
totalPov.hist <- totalPov %>% 
  mutate(year = ifelse(year != 2010, year - 1, year)) %>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))%>%
  bind_rows(pov.race %>%
              filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2021))

totalPov.hist_chart <- totalPov.hist %>%
  filter(var != "All", year != 2015) %>%
  ggplot()+
  geom_line(aes(x=year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2021")) + 
  geom_text(data = subset(totalPov.hist, year %in% c("1979", "2021") & var != "All"), aes(x=year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Poverty rate by race/ethnicity, Orleans Parish",
       x="",
       y="") 

ggsave(totalPov.hist_chart,filename = "indicator expansion drafts/graphics/pov.hist.png",
       width = 8, height = 6, units = "in")

### stat test ###
pov_stattest.data <- povRaw %>%
  replace(is.na(.),0) %>%
  transmute(placename = placename,
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = ifelse(!is.nan(BelowPov_asian / Total_asian),BelowPov_asian / Total_asian,0),
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  )
pov.race_stattest <- pov_stattest.data %>%
  mutate(sig_wht_blk = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov_asian, moey = moeprop_asian),
         sig_wht_all = stattest(x=pctpov_wht, moex = moeprop_wht, y=pctpov, moey = moeprop),
         sig_blk_hisp = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov_asian, moey = moeprop_asian),
         sig_blk_all = stattest(x=pctpov_blk, moex = moeprop_blk, y=pctpov, moey = moeprop),
         sig_hisp_asian = stattest(x=pctpov_hisp, moex = moeprop_hisp, y=pctpov_asian, moey = moeprop_asian),
         sig_hisp_all = stattest(x=pctpov_hisp, moex = moeprop_hisp, y=pctpov, moey = moeprop),
         sig_asian_all = stattest(x=pctpov_asian, moex = moeprop_asian, y=pctpov, moey = moeprop)
  ) %>%
  select(placename, contains("sig"))

pov.hist_stattest <- left_join(pov10MOE, pov00MOE) %>%
  left_join(totalPov.hist %>% filter(year == 1999) %>% transmute(est2000 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2021) %>% transmute(est2021 = val, race = var)) %>%
  left_join(pov_stattest.data %>% filter(placename == "Orleans") %>% select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "moe2021") %>%
              mutate(race = case_when(var =="moeprop" ~ "All",
                                      var =="moeprop_blk" ~ "Black",
                                      var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                      var =="moeprop_hisp" ~ "Hispanic,\nany race"
              )) %>%
              select(-var)) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2021, moe2021),
         sig_10_21 = stattest(est2010, moe2010, est2021, moe2021)
  ) %>%
  select(race, contains("sig"))
###
### Child poverty ###
###
# load("indicator expansion drafts/childpovRaw.RData")
childpov <- childpovRaw %>%
  mutate(
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
    TotChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk + AbovePovFemaleChild_blk + AbovePovMaleChild_blk,
    moeaggtot_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk, AbovePovFemaleChildMOE_blk, AbovePovMaleChildMOE_blk)),
    TotBelowChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk,
    moeaggbelow_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk)),
    pctBelowChildPov_blk = TotBelowChildPov_blk / TotChildPov_blk,
    moeprop_blk = moeprop(y=TotChildPov_blk, moex = moeaggbelow_blk, moey = moeaggtot_blk, p=pctBelowChildPov_blk),
    TotChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht + AbovePovFemaleChild_wht + AbovePovMaleChild_wht,
    moeaggtot_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht, AbovePovFemaleChildMOE_wht, AbovePovMaleChildMOE_wht)),
    TotBelowChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht,
    moeaggbelow_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht)),
    pctBelowChildPov_wht = TotBelowChildPov_wht / TotChildPov_wht,
    moeprop_wht = moeprop(y=TotChildPov_wht, moex = moeaggbelow_wht, moey = moeaggtot_wht, p=pctBelowChildPov_wht),
    TotChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp + AbovePovFemaleChild_hisp + AbovePovMaleChild_hisp,
    moeaggtot_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp, AbovePovFemaleChildMOE_hisp, AbovePovMaleChildMOE_hisp)),
    TotBelowChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp,
    moeaggbelow_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp)),
    pctBelowChildPov_hisp = TotBelowChildPov_hisp / TotChildPov_hisp,
    moeprop_hisp = moeprop(y=TotChildPov_hisp, moex = moeaggbelow_hisp, moey = moeaggtot_hisp, p=pctBelowChildPov_hisp),
    TotChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian + AbovePovFemaleChild_asian + AbovePovMaleChild_asian,
    moeaggtot_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian, AbovePovFemaleChildMOE_asian, AbovePovMaleChildMOE_asian)),
    TotBelowChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian,
    moeaggbelow_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian)),
    pctBelowChildPov_asian = TotBelowChildPov_asian / TotChildPov_asian,
    # moeprop_asian = moeprop(y=TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p=pctBelowChildPov_asian)
  )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### Across geos child pov bar chart ###
childpov.totals <- childpov %>% 
  filter(var == "pctBelowChildPov") %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

childpov.race <- childpov %>%
  filter(var != "pctBelowChildPov") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

childpov.raceGeos_chart <- childpov.race %>%
  ggplot(aes(x=place.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(childpov.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre), #yellowochre wasn't found for me
                    limits = levels(childpov.race$var.fac)) +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= childpov.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= childpov.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= childpov.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= childpov.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= childpov.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Child poverty rate by race/ethnicity, 2021",
       x="",
       y="")
ggsave(childpov.raceGeos_chart,filename = "indicator expansion drafts/graphics/childpov.raceGeos.png",
       width = 10, height = 6, units = "in")

### Historical child pov line chart ###
childPov.hist <- childPovProspInd %>% 
  mutate(year = ifelse(Year != 2010, Year - 1, Year)) %>%
  rename(val = childPov) %>%
  mutate(var = Race,
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Lat",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  select (-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(childpov %>%
              filter(place.fac == "Orleans",
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2021, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                      var = ifelse(var == "pctBelowChildPov", "All", var),
                      var = ifelse(grepl("blk",var), "Black", var),
                      var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                      var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

childPov.hist_chart <- childPov.hist %>%
  filter(var != "All", Year != 2015) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2021")) + 
  geom_text(data = subset(childPov.hist, Year %in% c("1980", "2021") & var != "All"), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Child poverty rate by race/ethnicity since 1980, Orleans Parish",
       x="",
       y="")

ggsave(childPov.hist_chart,filename = "indicator expansion drafts/graphics/childpov.hist.png",
       width = 10, height = 6, units = "in")

### stat test ###

childpov_stattest.data <- childpovRaw %>%
  replace(is.na(.),0) %>%
  transmute(
    placename = placename,
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y=TotChildPov, moex = moeaggbelow, moey = moeaggtot, p=pctBelowChildPov),
    TotChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk + AbovePovFemaleChild_blk + AbovePovMaleChild_blk,
    moeaggtot_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk, AbovePovFemaleChildMOE_blk, AbovePovMaleChildMOE_blk)),
    TotBelowChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk,
    moeaggbelow_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk)),
    pctBelowChildPov_blk = TotBelowChildPov_blk / TotChildPov_blk,
    moeprop_blk = moeprop(y=TotChildPov_blk, moex = moeaggbelow_blk, moey = moeaggtot_blk, p=pctBelowChildPov_blk),
    TotChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht + AbovePovFemaleChild_wht + AbovePovMaleChild_wht,
    moeaggtot_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht, AbovePovFemaleChildMOE_wht, AbovePovMaleChildMOE_wht)),
    TotBelowChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht,
    moeaggbelow_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht)),
    pctBelowChildPov_wht = TotBelowChildPov_wht / TotChildPov_wht,
    moeprop_wht = moeprop(y=TotChildPov_wht, moex = moeaggbelow_wht, moey = moeaggtot_wht, p=pctBelowChildPov_wht),
    TotChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp + AbovePovFemaleChild_hisp + AbovePovMaleChild_hisp,
    moeaggtot_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp, AbovePovFemaleChildMOE_hisp, AbovePovMaleChildMOE_hisp)),
    TotBelowChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp,
    moeaggbelow_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp)),
    pctBelowChildPov_hisp = TotBelowChildPov_hisp / TotChildPov_hisp,
    moeprop_hisp = moeprop(y=TotChildPov_hisp, moex = moeaggbelow_hisp, moey = moeaggtot_hisp, p=pctBelowChildPov_hisp),
    TotChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian + AbovePovFemaleChild_asian + AbovePovMaleChild_asian,
    moeaggtot_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian, AbovePovFemaleChildMOE_asian, AbovePovMaleChildMOE_asian)),
    TotBelowChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian,
    moeaggbelow_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian)),
    pctBelowChildPov_asian = ifelse(!is.nan(TotBelowChildPov_asian / TotChildPov_asian),TotBelowChildPov_asian / TotChildPov_asian,0),
    moeprop_asian = moeprop(y=TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p=pctBelowChildPov_asian) 
    )
childpov.race_stattest <- childpov_stattest.data%>%
  mutate(sig_wht_blk = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_blk, moey = moeprop_blk),
         sig_wht_hisp = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
         sig_wht_asian = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sig_wht_all = stattest(x=pctBelowChildPov_wht, moex = moeprop_wht, y=pctBelowChildPov, moey = moeprop),
         sig_blk_hisp = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_hisp, moey = moeprop_hisp),
         sig_blk_asian = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sig_blk_all = stattest(x=pctBelowChildPov_blk, moex = moeprop_blk, y=pctBelowChildPov, moey = moeprop),
         sig_hisp_asian = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov_asian, moey = moeprop_asian),
         sig_hisp_all = stattest(x=pctBelowChildPov_hisp, moex = moeprop_hisp, y=pctBelowChildPov, moey = moeprop),
         sig_asian_all = stattest(x=pctBelowChildPov_asian, moex = moeprop_asian, y=pctBelowChildPov, moey = moeprop)
         ) %>%
  select(placename, contains("sig"))

childpov.hist_stattest <- left_join(childPov.histMOE, (childPov.hist %>% 
                                                         filter(Year == 2000) %>% 
                                                         transmute(est2000 = val, race = var)), by = "race") %>%
  left_join(childPov.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(childPov.hist %>% filter(Year == 2021) %>% transmute(est2021 = val, race = var)) %>%
  left_join((childpov_stattest.data %>% 
              filter(placename == "Orleans") %>% 
              select(moeprop,moeprop_blk ,moeprop_wht ,moeprop_hisp) %>%
              pivot_longer(everything(), names_to = "var", values_to = "moe2021") %>%
              mutate(race = case_when(var =="moeprop" ~ "All",
                                      var =="moeprop_blk" ~ "Black",
                                      var =="moeprop_wht" ~ "White,\nnon-Hispanic",
                                      var =="moeprop_hisp" ~ "Hispanic,\nany race"
              )))) %>%
              select(-var) %>%
  mutate(sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
         sig_00_21 = stattest(est2000, moe2000, est2021, moe2021),
         sig_10_21 = stattest(est2010, moe2010, est2021, moe2021)
  ) %>%
  select(race, contains("sig"))

###
### Homeownership ###
###
# load("indicator expansion drafts/hoRaw.RData")
ho <- hoRaw %>%
  mutate(Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         Ownerpct_blk = Owner_blk / Total_blk,
         Ownermoeprop_blk = moeprop(y=Total_blk,moex = OwnerMOE_blk,moey = TotalMOE_blk,p=Ownerpct_blk),
         Ownerpct_wht = Owner_wht / Total_wht,
         Ownermoeprop_wht = moeprop(y=Total_wht,moex = OwnerMOE_wht,moey = TotalMOE_wht,p=Ownerpct_wht),
         Ownerpct_hisp = Owner_hisp / Total_hisp,
         Ownermoeprop_hisp = moeprop(y=Total_hisp,moex = OwnerMOE_hisp,moey = TotalMOE_hisp,p=Ownerpct_hisp),
         Ownerpct_asian = Owner_asian / Total_asian,
         #Ownermoeprop_asian = moeprop(y=Total_asian,moex = OwnerMOE_asian,moey = TotalMOE_asian,p=Ownerpct_asian)
  )%>%
  select(place,contains("pct")) %>%
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

### Across geos homeownership bar chart ###
ho.totals <- ho %>% 
  filter(var == "Ownerpct") %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

ho.race <- ho %>%
  filter(var != "Ownerpct") %>%
  mutate(var = ifelse(grepl("asian",var), "Asian",var),
         var = ifelse(grepl("blk",var), "Black", var),
         var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)) %>%
  mutate(var.fac = factor(.$var, levels = c("Black","White,\nnon-Hispanic","Asian","Hispanic,\nany race")))

ho.raceGeos_chart <- ho.race %>%
  ggplot(aes(x=place.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(ho.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(ho.race$var.fac)) +
  geom_segment(data= ho.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= ho.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= ho.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= ho.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= ho.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= ho.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= ho.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= ho.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  geom_segment(data= ho.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2, color = "gray70") +
  geom_label(data= ho.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("All:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray70") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Homeownership rate by race/ethnicity, 2021",
       x="",
       y="")
ggsave(ho.raceGeos_chart,filename = "indicator expansion drafts/graphics/homeownership.raceGeos.png",
       width = 10, height = 6, units = "in")

### Historical homeownership line chart ###
homeownership.hist <- homeownershipProspInd %>% 
  filter(Geography == "New Orleans") %>%
  rename(val = pctHomeownership) %>%
  mutate(var = Race,
         var = ifelse(grepl("Bla",var), "Black", var),
         var = ifelse(grepl("Lat",var), "Hispanic,\nany race", var),
         var = ifelse(grepl("Whi",var), "White,\nnon-Hispanic", var)) %>%
  select (-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(ho %>%
              filter(place.fac == "Orleans",
                     !grepl("asian",var)) %>% ## remove asian numbers 
              select(-place.fac) %>%
              mutate(Year = 2021, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
                     var = ifelse(var == "Ownerpct", "All", var),
                     var = ifelse(grepl("blk",var), "Black", var),
                     var = ifelse(grepl("hisp",var), "Hispanic,\nany race", var),
                     var = ifelse(grepl("wht",var), "White,\nnon-Hispanic", var)))%>%
  mutate(var.fac = factor(.$var, levels = c("All","Black","White,\nnon-Hispanic","Hispanic,\nany race")))

homeownership.hist_chart <- homeownership.hist %>%
  filter(var != "All", Year != 2016) %>%
  filter(val != 0) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.2,.7)) + 
  scale_x_continuous( labels = c("1970", "1980", "1990", "2000", "2010","2021")) + 
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(homeownership.hist, Year %in% c("1970", "2021") & var != "All"), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(homeownership.hist,  Year == "1980"& var == "Hispanic,\nany race" ), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Homeownership rate by race/ethnicity, Orleans Parish",
       x="",
       y="") 
homeownership.hist_chart
ggsave(homeownership.hist_chart,filename = "indicator expansion drafts/graphics/homeownership.hist.png",
       width = 8, height = 6, units = "in")


### stat test

ho_stattest.data <- hoRaw %>%
  replace(is.na(.),0) %>%
  transmute(placename = placename,
            Ownerpct = Owner / Total,
         Ownermoeprop = moeprop(y=Total,moex = OwnerMOE,moey = TotalMOE,p=Ownerpct),
         Ownerpct_blk = Owner_blk / Total_blk,
         Ownermoeprop_blk = moeprop(y=Total_blk,moex = OwnerMOE_blk,moey = TotalMOE_blk,p=Ownerpct_blk),
         Ownerpct_wht = Owner_wht / Total_wht,
         Ownermoeprop_wht = moeprop(y=Total_wht,moex = OwnerMOE_wht,moey = TotalMOE_wht,p=Ownerpct_wht),
         Ownerpct_hisp = Owner_hisp / Total_hisp,
         Ownermoeprop_hisp = moeprop(y=Total_hisp,moex = OwnerMOE_hisp,moey = TotalMOE_hisp,p=Ownerpct_hisp),
         Ownerpct_asian = Owner_asian / Total_asian,
         Ownermoeprop_asian = moeprop(y=Total_asian,moex = OwnerMOE_asian,moey = TotalMOE_asian,p=Ownerpct_asian)
  )

ho.race_stattest <- ho_stattest.data %>%
  mutate(sig_wht_blk = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_blk, moey = Ownermoeprop_blk),
         sig_wht_hisp = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_hisp, moey = Ownermoeprop_hisp),
         sig_wht_asian = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sig_wht_all = stattest(x=Ownerpct_wht, moex = Ownermoeprop_wht, y=Ownerpct, moey = Ownermoeprop),
         sig_blk_hisp = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct_hisp, moey = Ownermoeprop_hisp),
         sig_blk_asian = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sig_blk_all = stattest(x=Ownerpct_blk, moex = Ownermoeprop_blk, y=Ownerpct, moey = Ownermoeprop),
         sig_hisp_asian = stattest(x=Ownerpct_hisp, moex = Ownermoeprop_hisp, y=Ownerpct_asian, moey = Ownermoeprop_asian),
         sig_hisp_all = stattest(x=Ownerpct_hisp, moex = Ownermoeprop_hisp, y=Ownerpct, moey = Ownermoeprop),
         sig_asian_all = stattest(x=Ownerpct_asian, moex = Ownermoeprop_asian, y=Ownerpct, moey = Ownermoeprop)
  ) %>%
  select(placename, contains("sig"))

ho.hist_stattest <- 
  left_join((homeownership.hist %>% filter(Year == 2000) %>% transmute(est2000 = val, race = var)), (homeownership.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var))) %>%
  left_join((homeownership.hist %>% filter(Year == 2021) %>% transmute(est2021 = val, race = var)))%>%
  left_join((ho_stattest.data %>% 
               filter(placename == "Orleans") %>% 
               select(Ownermoeprop,Ownermoeprop_blk ,Ownermoeprop_wht ,Ownermoeprop_hisp) %>%
               pivot_longer(everything(), names_to = "var", values_to = "moe2021") %>%
               mutate(race = case_when(var =="Ownermoeprop" ~ "All",
                                       var =="Ownermoeprop_blk" ~ "Black",
                                       var =="Ownermoeprop_wht" ~ "White,\nnon-Hispanic",
                                       var =="Ownermoeprop_hisp" ~ "Hispanic,\nany race"
               ))) %>%
  select(-var)) %>%
  mutate(sig_00_21 = stattest(x=est2000,y= est2021, moey=moe2021),
         sig_10_21 = stattest(x=est2010, y= est2021, moey=moe2021)
  ) %>%
  select(race, contains("sig"))

