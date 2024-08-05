## New Who Lives Readme! 

updated July 2024 by Haleigh Tomlin


### Files

There is the following series of files and some brief descriptions that are important to updating the page.

01_libraries.R

02_variables.R

03_functions.R - creates a data pull function for either getting the Who Lives 2 data from the warehouse or from the census api, and creates a data visualization function for the bar charts

04_analysis.R - depends on the data from data-pull.R

05_graphics.R

There is also:

datalake-connection.R - connects to the data warehouse 

data-pull.R - either pulls data from the census API or the warehouse and writes RData that are used in 04_analysis.R

inline.R - updates the data points referenced in the Who Lives text in WhoLivesMarkdown.Rmd

css_helper.R - has html hacks to make Who Lives pretty on the website

WhoLivesMarkdown.Rmd - this generates the html document that gets pasted into wordpress directly to update the website

outputs/spreadsheets - this folder has the data for each chart, and is used to update the downloadable excel sheet with the data.

### Instructions 

#### Getting the data

Who Lives happens in two installments. 

##### Who Lives 1

The first happens in July, when the County population estimates by characteristics are released by the Census (PEP data). They no longer have an API for this data, so you have to go to https://www.census.gov/data/tables/time-series/demo/popest to download the necessary tables. 

The PEP data is used in 5 charts in Who Lives. The data you need is downloaded in 4 files. 

County population by characteristics -> Annual County Population Estimates by Selected Age Groups and Sex -> Download US and Louisiana data

County population by characteristics -> Annual County Resident Population Estimates by Age, Race, and Hispanic Origin -> Download US and Louisiana data.

I usually rename them and put them in inputs/PEP_data. 

In data-pull.R, the PEP data is updated toward the bottom of the file. Update the file path to the new data. Since each population estimates release updates the old data, you will also need to update several parts of the code to add the new year from the `YEAR` code in the file layout pdf on the census page where you download the data. 

Also, since the PEP data updates the year-over-year changes in population, rather than changing the data year to reflect the new year, you must add the new year analysis. For example, last time I updated this data it was the 2022 pep, and I will just add the 2023 code onto this, which includes the updated numbers since 2020.

In 04_analysis.R, you'll have to change the PEP year date.


##### Who Lives 2

The second installment happens in the fall, when the new ACS 1-year estimates are released. This is a bigger data update, and most of the charts are updated in this release. I get this data from the census api unless the data warehouse is prepared to host this step. 

#### Update process

Check updates.txt document before updating anything!

Run 01_libraries.R

Update the variable year in 02_variables.R. If it's the PEP update, only update the yearPEP and yearPEP.char variable to the data release year. 
For the ACS update (Who Lives 2) update the cpi variable from BLS from 2004 and 1999 to the update year so that any dollar amounts will be inflation adjusted to the current year.

Run 03_functions.R. For now, this pulls from the census API, but when the warehouse is ready, edit to pull from the warehouse api.

Run data-pull.R. 

Run 04_analysis.R - there are a few places in this file where the date might need to be updated to the new year.

Run 05_graphics.R - there are a few places where you need to add on the new year label. Also, check chart title labels to ensure they say the new year.

update inline.R - there are a few places here where it pulls from allparishesRaw[YEAR] - change that to the new year, or make a new variable that is immune to year changes like "current."

In WhoLivesMarkdown.Rmd, change the year in the description text in the blue box at the top. You will also need to edit the css-style captions to reflect the new year of the respective data update. When you are ready, knit the WhoLivesMarkdown to "md document." You will have to do this two times.
   
The first time you do this, comment out the manual "fig.path" so that it generates images to a url that will point to aws. This is the one you will copy into wordpress.

The second time you knit, you want fig.path to be wp-graphics/uploads/[year]/[month number] so that it generates all the graphics to a folder that you can upload the contents of into aws to the url specified - "https://s3.amazonaws.com/files.datacenterresearch/who-lives"

Before the project wraps up, be sure to make notes in the "updates.txt" document for future reference, so we don't re-do old work. Also, edit this Readme to be up to date.

------

Improvement ideas:

- automatically download the link to the PEP data using the url 

- anywhere that needs to be edited in the series laid out above, make that immune to changes so you only need to update the "year" variable - including name of graphs

- have the WhoLivesMarkdown.Rmd write images to both aws and generate the link in the same go.

- further automate the process: in a separate script, have the year variable, have it source the data-pull.R file, and have it knit the WhoLivesMarkdown.Rmd (which calls the other files). 

- automatically update the excel sheet so it isn't manual - have the R script write into the excel sheet at the appropriate locations

- archive things that are commented out 

- cpi adjustment: can we do that via API or get automatically from BLS?

- PEP data doesn't add the previous year anymore, make that automatically add new year

- can we connect to aws remotely?
  
- move analysis out of graphics.R

