#
#  analysis of ViCTA cone test results for plots
#
#  Anopheles funestus strains Fang (Sus) and Fumoz (res)
#
#  treatments: UT, P3, OSP
#
# with kd and mortality data
#
#
#  jeff jones and jessie carson 31/3/23
#



library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(janitor)

rm(list = ls())

dfall = read_excel("data\\JessieFangFumozVCD.xlsx")
dfall = janitor::clean_names(dfall)
dfall$date = as.Date(x = dfall$date, format = "%d/%m/%Y")
dfall$date = format(dfall$date, "%d/%m/%Y")
dfall = rename(dfall, "top_half_tot" = "tophalftot", "bottom_half_tot" = "bottomhalftot")


unique(dfall$treatment)
unique(dfall$strain)


dfall$treatment <- factor(dfall$treatment, levels = c("UT","P3","OSP"))
print(levels(dfall$treatment)) # order of the treatment

dfall$strain <- factor(dfall$strain, levels = c("FA","FZ"))
print(levels(dfall$strain)) # order of the treatment


source("PlotUtils.R")
dfcurrent = dfall
country = "An_funestus Fang vs Fumoz"
resultspath = "Results\\Jessie\\"

theme_set(theme_bw())
# mean total activity
generateAndPlotMeansTotByStrain(dfcurrent, country, resultspath, TRUE,"_mean_tot_act_by_strain.png") # create the plot
# mean total Inactivity
generateAndPlotMeansTotInactivityByStrain(dfcurrent, country, resultspath, TRUE,"_mean_tot_inact_by_strain.png") # create the plot
# mean absolute regional activity 
generateAndPlotRegionMeansPlotbyStrain(dfcurrent,country, resultspath,TRUE,"_mean_absolute_regional_by_strain.png")
# mean proportional regional activity 
generateAndPlotRegionMeansPropPlotbyStrain(dfcurrent,country, resultspath, TRUE, "_mean_proportional_regional_by_strain.png")

# mean mortality by strain and treatment
generateAndPlotMeansMortality24hByStrainAndTreatment(dfcurrent,country, resultspath, TRUE, "_mean_24h_mortality_by_strain_and_treatment.png")
# mean kd at 1h by strain and treatment
generateAndPlotMeansKDat1hByStrainAndTreatment(dfcurrent,country, resultspath, TRUE, "_mean_kd_at_1h_by_strain_and_treatment.png")
# mean kd at 3min by strain and treatment
generateAndPlotMeansKDat3MinByStrainAndTreatment(dfcurrent,country, resultspath, TRUE, "_mean_kd_at_3min_by_strain_and_treatment.png")
# scatter plot of total activity vs percent mortality
generateAndPlotScatterActivityByMortality(dfcurrent,country, resultspath, TRUE, "_scatterplot_total_activity_vs_percent_mortality.png")
# scatter plot of total inactivity vs percent mortality
generateAndPlotScatterInctivityByMortality(dfcurrent,country, resultspath, TRUE, "_scatterplot_total_inactivity_vs_percent_mortality.png")




#
#  temporal inactivity over time
#
generateAndPlotTemporalInactivityByStrain(dfcurrent,country, resultspath, TRUE, "_inactivity_over_time_by_strain.png")


#
#  temporal activity over time
#
generateAndPlotTemporalActivityByStrain(dfcurrent,country, resultspath, TRUE, "_activity_over_time_by_strain.png")
generateAndPlotOverlaidTemporalActivityByStrain(dfcurrent,country, resultspath, TRUE, "_overlaid_activity_over_time_by_strain.png")

#
# area plots of regional activity over time
#
generateAndPlotTemporalActivityTopBottomAreaChartByTreatmentAndStrain(dfcurrent,"",resultspath,TRUE,FALSE,"_region_activity_area_plot.png")
generateAndPlotTemporalActivityTopBottomProportionalAreaChartByTreatmentAndStrain(dfcurrent,"",resultspath,TRUE,FALSE,"_prop_region_activity_area_plot.png")
