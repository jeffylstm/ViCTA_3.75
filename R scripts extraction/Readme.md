Readme file for: R scripts for data extraction

This folder contains scripts to extract the data from a victa data entry excel template and assemble them to a centralised data file for plotting/data analysis purposes

An example dataset from victa analysis is included in the file: 'fang fumoz victa data entry template.xlsx'

Each tab in the excel template contains a set of results from a single strain and treatment combination.

The extraction files are in the form of a Shiny UI r GUI app. The r project file should be loaded first by double clicking the shinyapp_victa_all.Rproj file. The user may need to install some of the required libraries on their own pc running R studio. The main runnable file is victaAllApp.r. The conversion process lets the user select one tab at a time, fill in relevant parameters (e.g. strain, treatment, operator etc) and the program will then extract and save each tab as a separate csv file in the folder: 'multiplecsvoutput'

There are a wide range of strains, treatments and so on already stored in the victaappUI.R file. The user can add more entries by editing this particular script carefully to include new strains or treatments.

These files can be aggregated using the Shiny App to yield a single victa results database file which, when generated, will be saved in the 'centraldatabaseoutput' folder.

Example csv files and central database files are also included in the folders 'multiplecsvsoutput' and 'centraldatabaseoutput'.

The central database can then be loaded and plots generated, as shown in the contents of the 'r scripts plots' folder in the main github archive for this project. 



