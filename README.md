# Suspected drug deaths in Scotland

This repository contains the reproducible analytical pipeline (RAP) used to create the quarterly Suspected Drug Deaths publication. The data is sourced from management information collected by Police Scotland, and the latest iteration of the report can be found here: https://www.gov.scot/publications/suspected-drug-deaths-scotland-january-march-2022/

## Running the RAP

Compiling the final document requires executing the following:

  1. When Police Scotland furnish Scottish Government with new data (i.e. every quarter) an updated .xlsx dataset must be saved into the **Data** folder
  2. Update the <code>Readingdata</code> function in the <code>2_Wrangling.R</code> script (in the Mainfunctions folder) to reflect the dataset name you saved in Step 1
  3. In the <code>README</code> chunk of the <code>Mainoutput.Rmd</code> Markdown document, change the source call to whichever quarter it is. For example, if you are reporting on suspected drug deaths for Q3 – that is, January to September of a given year – the source will be <code>"~/Q3_Functions.R"</code>

## Methodological note

Classifying a death as drug-related is complex. In Scotland there are two different sets of statistics on the number of drug deaths. The suspected drug deaths reported here are classified as such based on an officer’s observations and initial enquiries at the scene of death. Note that these are *NOT* the Official Statistics published by the National Records of Scotland. Instead they are intended to provide an indicator of recent drug death trends in Scotland, to inform prevention and enforcement activities. More information on the methods can be found in the <code>Background_doc.Rmd</code> markdown file.