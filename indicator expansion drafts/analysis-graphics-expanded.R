library(ggrepel)

load("indicator expansion drafts/medhhRaw.RData")
medhh <- medhhRaw  %>%
  mutate(placenames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place, -placenames, -contains("MOE")) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val")

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
           color='gray50') +    #bar outline
  geom_text(data = subset(medhh.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = paste0("$",scales::comma(val))),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(bach.race$var.fac)) +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray") +
  geom_label(data= medhh.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("Overall:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2) +
  geom_label(data= medhh.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("Overall:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2) +
  geom_label(data= medhh.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("Overall:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2) +
  geom_label(data= medhh.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("Overall:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= medhh.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2) +
  geom_label(data= medhh.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("Overall:$",scales::comma(val))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(hjust = .5, size=16)) + 
  labs(title = "Median household income by race/ethnicity",
       x="",
       y="")
ggsave(medhh.raceGeos_chart,filename = "indicator expansion drafts/graphics/medhh.raceGeos.png",
       width = 10, height = 6, units = "in")


load("indicator expansion drafts/bachRaw.RData")
bach <- bachRaw %>%
  mutate(totbach = MaleBach + FemaleBach,
         totbach_blk = MaleBach_blk + FemaleBach_blk,
         totbach_wht = MaleBach_wht + FemaleBach_wht,
         totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
         totbach_asian = MaleBach_asian + FemaleBach_asian,
         pctbach = totbach / Total,
         pctbach_blk = totbach_blk / Total_blk,
         pctbach_wht = totbach_wht / Total_wht,
         pctbach_hisp = totbach_hisp / Total_hisp,
         pctbach_asian = totbach_asian / Total_asian,
         moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE)),
         moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
         moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
         moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
         moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
         moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
         moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
         moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
         # moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
         moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp))%>%
  select(place,contains("pct")) %>%
  mutate(placenames = c("St. Tammany", "Jefferson","Orleans",  "Metro", "U.S."))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 


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
           color='gray50') +    #bar outlineas.factor
  geom_text(data = subset(bach.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(bach.race$var.fac)) +
  geom_segment(data= bach.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray") +
  geom_label(data= bach.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray") +
  geom_segment(data= bach.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2) +
  geom_label(data= bach.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= bach.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2) +
  geom_label(data= bach.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= bach.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2) +
  geom_label(data= bach.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= bach.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2) +
  geom_label(data= bach.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Bachelor's degree or higher, adults 25 years or older by race/ethnicity",
       x="",
       y="")
ggsave(bach.raceGeos_chart,filename = "indicator expansion drafts/graphics/bach.raceGeos.png",
       width = 10, height = 6, units = "in")
  

load("indicator expansion drafts/povRaw.RData")
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
  mutate(placenames = c("St. Tammany", "Jefferson","Orleans",  "Metro", "U.S."))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

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
           color='gray50') +    #bar outlineas.factor
  geom_text(data = subset(pov.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(pov.race$var.fac)) +
  geom_segment(data= pov.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray") +
  geom_label(data= pov.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray") +
  geom_segment(data= pov.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2) +
  geom_label(data= pov.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= pov.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2) +
  geom_label(data= pov.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= pov.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2) +
  geom_label(data= pov.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= pov.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2) +
  geom_label(data= pov.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Poverty rate by race/ethnicity",
       x="",
       y="")
ggsave(pov.raceGeos_chart,filename = "indicator expansion drafts/graphics/pov.raceGeos.png",
       width = 10, height = 6, units = "in")



load("indicator expansion drafts/childpovRaw.RData")
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
  mutate(placenames = c("St. Tammany", "Jefferson","Orleans",  "Metro", "U.S."))  %>% 
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%
  select(-place,-placenames) %>%
  pivot_longer(-place.fac,names_to = "var",values_to = "val") 

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
           color='gray50') +    #bar outlineas.factor
  geom_text(data = subset(childpov.race, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = scales::percent(val,accuracy = 1)),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p2violet,DCcolor.p3yellowochre),
                    limits = levels(childpov.race$var.fac)) +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Orleans"), aes(x = .5 , y = val, xend = 1.5, yend = val), linetype = 2, color = "gray") +
  geom_label(data= childpov.totals %>% filter(place.fac=="Orleans"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap", color = "gray") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Jefferson"),aes(x = 1.5 , y = val, xend = 2.5, yend = val), linetype = 2) +
  geom_label(data= childpov.totals %>% filter(place.fac=="Jefferson"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="St. Tammany"),aes(x = 2.5 , y = val, xend = 3.5, yend = val), linetype = 2) +
  geom_label(data= childpov.totals %>% filter(place.fac=="St. Tammany"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="Metro"),aes(x = 3.5 , y = val, xend = 4.5, yend = val), linetype = 2) +
  geom_label(data= childpov.totals %>% filter(place.fac=="Metro"),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  geom_segment(data= childpov.totals %>% filter(place.fac=="U.S."),aes(x = 4.5 , y = val, xend = 5.5, yend = val), linetype = 2) +
  geom_label(data= childpov.totals %>% filter(place.fac=="U.S."),
             aes(label = paste0("Overall:",scales::percent(val,accuracy = 1))),
             hjust = 1, vjust =1, label.size = NA, fill = NA,size=3,family="Asap") +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16)) + 
  labs(title = "Child poverty rate by race/ethnicity",
       x="",
       y="")
ggsave(childpov.raceGeos_chart,filename = "indicator expansion drafts/graphics/childpov.raceGeos.png",
       width = 10, height = 6, units = "in")
