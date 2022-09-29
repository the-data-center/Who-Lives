se2000 <- read_xlsx("2000_ASE.xlsx")
se2000 <- se2000 %>% pivot_longer(cols = `United States`:`St. Tammany`,names_to = "placename")
se2000 <- se2000 %>% pivot_wider(names_from = Variable, values_from = value)
