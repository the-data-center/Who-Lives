## New Who Lives Readme! 

updated June 2024 by Haleigh Tomlin


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


##### Who Lives 2

The second installment happens in the fall, when the new ACS 1-year estimates are released. This is a bigger data update, and most of the charts are updated in this release. I get this data from the census api unless the data warehouse is prepared to host this step. 

#### Update process


Run 01_libraries.R

Update the variable year in 02_variables.R. If it's the PEP update, only update the yearPEP and yearPEP.char variable to the data release year. 
For the ACS update (Who Lives 2) update the cpi variable from BLS from 2004 and 1999 to the update year so that any dollar amounts will be inflation adjusted to the current year.


