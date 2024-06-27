## Who Lives
### Objective
Pull, clean and analyze ACS 1-year data on New Orleans and the metro area, destined for this webpage: https://www.datacenterresearch.org/data-resources/who-lives-in-new-orleans-now/

### Contents

#### inputs
all static data input files: .RData's generated in data-pull.R + graph formatting

#### outputs
empty because the markdown file lives in root (graphics in wp-graphics in our local directories)

#### documentation
additional documentation in drive - to be added here

#### sandbox
exploring data - everyone can have their own folder in here
opening the projects in RStudio and clicking "knit" on the WhoLivesMarkdown.Rmd doc will produce the html and wp-image files for the most recent update. 

### Bare minimum steps to complete the 2018 data update (upcoming as of 2/8/19):
1. update variables in flie 02_variables.R. As of right now, variable "year" isn't connected to anything. 
2. update year in inline.R (maybe by using the "year" variable)
3. update language in WhoLivesMarkdown.Rmd
4. if you currently have a folder called "wp-graphics" in your local Who-Lives directory, delete it (otherwise, the graphics won't be saved)
5. click knit on WhoLivesMarkdown.Rmd
6. add to website by adding the new graphics to the media page, creating a new Page, pasting the html there...

### To do list for an honest update, fixing stuff we did wrong the first time:
1. automate data tables if possible (DAISI and/or downloadable excels)
2. create functions for inline data pulls
3. create or add in auto-naming for variables
4. pull 2000 census data from API

### Tips on the process:
1. when iterating through data checking graphics, there's a list of the graphic names at the bottom of 05_graphics.R that you can un-# to quickly run through them
2. you do have to delete the wp-graphics folder in your local directory whenever you re-knit if the knit involves updating the images. If the folder is already there, knitting won't save the updated graphics.

### Fonts
We have a custom data center font that requires download onto new computers
1.  Download all ASAP fonts from https://www.fontsquirrel.com/fonts/asap
2.  Your font files must be truetype fonts (not opentype). If you have otf, convert to ttf with an online converter. https://convertio.co/otf-ttf/
3.  Move these fonts into your windows --> font folder on computer
4.  Run this command --> extrafont::font_import()
5.  Run Extrafonts library in 01_libraries.R





