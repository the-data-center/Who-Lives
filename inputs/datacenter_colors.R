###### OBJECTIVE

# This script is intended to load into memory a set of colors that can be called for generating
# colors according to the Data Center guidelines.

# See location on Box: 
# https://nkw.app.box.com/folder/5988427545

# Alternately, it could be possible to read-in the Excel file with RGB values. 



# The basic idea:

# Make colors with two sets of names:
# "p1darkblue"
# "p2teal"
# 
# I think we could use the RGB package to populate the rest of the 2ndary color palette table.
# 1. Convert between RGB and hex.
# 2. Use math to fill-in the less-saturated parts. 
#
# I think this would require the package "rgb" or something similar to convert between hex and RGB



###### DEFINE COLORS

# This early attempt is using only the primary and secondary color palettes.

# Three ways to select colors:

# 1. Frame a data frame

DCcolors <- data.frame(stringsAsFactors = TRUE, # Factors are a pain, but this might make things easier later.
                       colorName = c("p1darkblue",
                                     "p1grayblue",
                                     "p1mediumblue",
                                     "p1skyblue",
                                     "p1lightskyblue",
                                     "p2blue",
                                     "p2teal",
                                     "p2green",
                                     "p2limegreen",
                                     "p2yellow",
                                     "p2orange",
                                     "p2orangered",
                                     "p2magenta",
                                     "p2violet",
                                     "p2purple",
                                     "p3yellowochre"),
                       hex = c("#002F45",
                               "#4A6576",
                               "#6892AB",
                               "#ABE1FA",
                               "#D4EFFC",
                               "#166E95",
                               "#35A39B",
                               "#5D893C",
                               "#9EB23B",
                               "#F1C62B",
                               "#EF812C",
                               "#E65E3F",
                               "#E61C43",
                               "#B13F80",
                               "#71266E",
                               "#D7892C"))

DCcolors <- DCcolors %>% 
  mutate(palette = ifelse(str_sub(colorName, 1,2) == "p1", "primary","secondary"))

# 2. From a defined set of values (auto-complete is nice)

DCcolor.p1darkblue <- "#002F45"
DCcolor.p1grayblue <- "#4A6576"
DCcolor.p1mediumblue <- "#6892AB"
DCcolor.p1skyblue <- "#ABE1FA"
DCcolor.p1lightskyblue <- "#D4EFFC"
DCcolor.p2blue <- "#166E95"
DCcolor.p2teal <- "#35A39B"
DCcolor.p2green <- "#5D893C"
DCcolor.p2limegreen <- "#9EB23B"
DCcolor.p2yellow <- "#F1C62B"
DCcolor.p2orange <- "#EF812C"
DCcolor.p2orangered <- "#E65E3F"
DCcolor.p2magenta <- "#E61C43"
DCcolor.p2violet <- "#B13F80"
DCcolor.p2purple <- "#71266E"

for (i in 1:dim(DCcolors)[1]){
  ramp <- colorRamp(c(as.character(DCcolors$hex[i]),"white"))
  collist <- as.list(rgb(ramp(seq(.1, .9, length = 9)), names = paste("DCcolor.",as.character(DCcolors$colorName[i]), seq(90,10, by=-10), sep = ""), max = 255))
  list2env(collist,globalenv())
}

# 3. Selecting values from a list (good, for instance, if you want to use the first 3 or every third value.)

DCcolorsPrimary <- as.character(DCcolors$hex[1:5])
DCcolorsSecondary <- as.character(DCcolors$hex[6:length(DCcolors$hex)])

DCcolor.p3yellowochre <- "#D7892C"
