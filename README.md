# Who Lives

opening the projects in RStudio and clicking "knit" on the WhoLivesMarkdown.Rmd doc will produce the html and wp-image files for the most recent update. 

#### Bare minimum steps to complete the 2018 data update (upcoming as of 2/8/19):
1. update variables in flie 02_variables.R. As of right now, variable "year" isn't connected to anything. 
2. update year in inline.R (maybe by using the "year" variable)
3. update language in WhoLivesMarkdown.Rmd
4. if you currently have a folder called "wp-graphics" in your local Who-Lives directory, delete it (otherwise, the graphics won't be saved)
5. click knit on WhoLivesMarkdown.Rmd
6. add to website by adding the new graphics to the media page, creating a new Page, pasting the html there...

#### To do list for an honest update, fixing stuff we did wrong the first time:
1. automate data tables if possible (DAISI and/or downloadable excels)
2. create functions for inline data pulls
3. create or add in auto-naming for variables
4. pull 2000 census data from API

#### Tips on the process:
1. when iterating through data checking graphics, there's a list of the graphic names at the bottom of 05_graphics.R that you can un-# to quickly run through them
2. you do have to delete the wp-graphics folder in your local directory whenever you re-knit if the knit involves updating the images. If the folder is already there, knitting won't save the updated graphics.



