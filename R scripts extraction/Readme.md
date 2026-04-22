Readme file for: R scripts for data extraction

This folder contains scripts to extract the data from a victa data entry excel template and assemble them to a centralised data file for plotting/data analysis purposes

An example dataset from victa analysis is included in the file: fang fumoz victa data entry template.xlsx

Each tab in the excel template contains a set of results from a single strain and treatment combination.

The extraction files are in the form of a Shiny UI r script. The conversion process lets the user select one tab at a time, fill in relevant parameters (e.g. strain, treatment, operator etc) and the program will then extract and save each tab as a separate csv file in the folder: 'multiplecsvoutput'

These files can be aggregated using the Shiny App to yield a single victa results database file which, when generated, will be saved in the 'centraldatabaseoutput' folder.

Example csv files and central database files are also included.

The central database can then be loaded and plots generated, as shown in the contents of the 'r scripts plots' folder in the main github archive for this project. 



