# spi-malnutrition
A repo for my dissertation work on SPI and malnutrition

# Combining DHS Data
I used some these scripts to combine variables from multiple DHS surveys for analysis.  I'm putting some documentation here for people who might want to draw on my work to make their own dataset from multiple DHS survyes.

To run these scripts, you will have to make some of your own modifications, i.e., change file paths.

## Scope survyes
First, you generate a table of all surveys based on country, survey number, subersation, and the available survey files (WI, KR, GE, PR) by running `scope/Scope_surveys.R`.  For an overview of types of survey files, see [this](https://www.youtube.com/watch?v=BmiSDPebmgc) video and for an overview of surveys and subversions, see [this](https://www.youtube.com/watch?v=puCCqGU71uM) video.  It also checks for whether the surveys contain child anthropometry data, as this was the variable my research focuses on.  The table is saved in `scope/UseFiles.csv`

## Combine Survyes
Then, the script `extract/ExtractHHVars_PR+KR.R` combines all of the surveys.  It draws on the files `extract/headervars.csv`, `extract/Utils.R`, and `scope/UseFiles`.  `headervars.csv` tells the script what variables to extract, what they are coded as in the PR and KR files, and what to rename them as.  So, for example, a child's age is coded as `hc1` in the PR files and `hw1` in the KR files, and I rename that variable as `age`.  `scope/UseFiles.csv` determines which files are available for each survey and which survey files contain anthropometry information.  The script works in the following way:

1.  For each DHS survey:
  1. Check if anthropometry data is in the PR file, KR file, or both.  If it is in both, collect PR data first and then join the KR data to the PR data.  For variables about other household members (mother and fathers age and education; household dependency ratio), these are in the PR file and are hardcoded in the `Utils.R` script, so they cant be changed from the `headervars.csv` file.
  2.  Some surveys have household wealth data in the PR file.  For later surveys, it is in the WI file.  For surveys with WI files, that data is extracted and joined to the data from the PR or KR files.
  3.  Then, collect spatial data from the GE files (which are in shapefile format).  That data is then joined to the PR, KR, and WI data.
2.  Combine all data
3.  Go through each variable and do some cleaning (dealing with `NA` values coded as `999`, etc).  For categorical variables, they are mapped to new categories using the tables `all_water_sources.csv`, `family_relations.csv`, `toiletcodes.csv`, which are in the `extract` folder.
4.  Finally, calculate raw HAZ, WHZ, and WAZ scores based on age, height, and weight.

## Disclaimer
These scripts were designed only for my project and its goals.  They may be a useful starting point for other researchers using DHS data, but come with no guarantee and will likely need some considerable modification before they work for other research applications of the DHS.
