#
palette.trt = c('UT'='#BFBFBF','P3'='#6BD9FD','OSP'='#FFA7E5')  
#dfall$treatment <- factor(dfall$treatment, levels = c("UT","P6","P7"))


#p+ scale_fill_manual(values = palette.trt) # bar or column plots
#p+ scale_color_manual(values = palette.trt) # point or line plots

palette.reg4  <- c('r0'= "#00B0F0",'r1'= "#F59D56",'r2' = "#BFBFBF",'r3'= "#FFD965")
palette.reg2  <- c( 'bottom' =  "#FFD965",'top'= "#00B0F0")



# simplest plot of mean totals
#
generateAndPlotMeansTot <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    ggtitle(finaltitletext)
   p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext <- paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext, sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# simplest plot of mean totals
#
generateAndPlotMeansTotbyCountry <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(country,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Country")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(country~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext <- paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext, sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total activity by strain and treatment
#
generateAndPlotMeansTotByStrain <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
           means_tot$lower.ci.tot = ifelse(means_tot$lower.ci.tot <0, 0, means_tot$lower.ci.tot)
            
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# plot mean total activity by strain and treatment
#
generateAndPlotMeanGroupedTotByTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  df$combo = paste(df$strain,df$treatment,sep="_")
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  means_tot$lower.ci.tot = ifelse(means_tot$lower.ci.tot <0, 0, means_tot$lower.ci.tot)
  
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Strain and Treatment")
  dftemp <<- means_tot
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment, group=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1, position = position_dodge(0.9)) +
    ylab("Mean Total Activity (95% CI)") + xlab("Strain") +
      ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total activity by strain and treatment
#
generateAndPlotMeansTotByStrainAndWashedStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment,washed) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Strain and Washing Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain+washed~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# plot mean total activity by strain and treatment
#
generateAndPlotMeansTotByStrainFixedScale <- function(df, titletext, resultspath, ylimitmin, ylimitmax,   saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    ylim(c(ylimitmin,ylimitmax))+
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total activity by operator strain and treatment
#
generateAndPlotMeansTotByOperatorAndStrainFixedScale <- function(df, titletext, resultspath, ylimitmin, ylimitmax,   saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter, strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Operator and Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    ylim(c(ylimitmin,ylimitmax))+
    facet_grid(experimenter~strain)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



generateAndPlotMeansTotByStrainAndTreatmentViolin <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment)
  
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity Violin by Treatment and Strain")
  p <- ggplot(means_tot, aes(x=strain, y=total, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_violin()+
    geom_boxplot(width=0.1, color="blue", alpha=0.2, position = position_dodge(0.9))+
    geom_point( position = position_dodge(0.9))+
    #  geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    #   geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Strain") +
   # facet_grid(strain~treatment)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


generateAndPlotMeansTotByStrainAndTreatmentAndHostViolin <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment)
  
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity Violin by Treatment and Strain and Host")
  p <- ggplot(means_tot, aes(x=strain, y=total, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_violin()+
    geom_boxplot(width=0.1, color="blue", alpha=0.2, position = position_dodge(0.9))+
    geom_point( position = position_dodge(0.9))+
    #  geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    #   geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Strain") +
     facet_grid(.~host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total activity violin plot by strain and treatment
#
generateAndPlotMeansTotByTreatmentStrainAndHostViolin <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present,strain,treatment)
 
   dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Treatment, Strain and Host Status")
  p <- ggplot(means_tot, aes(x=strain, y=total, fill=strain)) + # fill=name allow to automatically dedicate a color for each group
    geom_violin()+
    geom_boxplot(width=0.1, color="blue", alpha=0.2)+
    geom_point(aes(col = strain), position = position_jitter(seed = 1, width = 0.1))+
  #  geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
 #   geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Strain") +
    facet_wrap(~treatment+host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# plot mean total contact index by strain and treatment
#
generateAndPlotMeansContactIndexByStrain <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(boris_contact_index, na.rm = TRUE),
              sd.tot = sd(boris_contact_index, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Contact Index by Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Contact Index (95% CI)") + xlab("Treatment") +
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


generateAndPlotMeansAvoidanceIndexByStrain <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(boris_avoidance_index, na.rm = TRUE),
              sd.tot = sd(boris_avoidance_index, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Avoidance Index by Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Avoidance Index (95% CI)") + xlab("Treatment") +
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean mortality 
#
generateAndPlotMeansPercentMortalityByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
  group_by(strain,treatment) %>%
  summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
            sd.tot = sd(mo_perc, na.rm = TRUE),
            n.tot = n())%>%
  mutate(se.tot = sd.tot / sqrt(n.tot),
         lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
         upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Percentage Mortality by Strain and Treatment")
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  dftemp <<- means_tot
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
  geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
  geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
  ylab("Mean Mortality (95% CI)") + xlab("Strain") +
  ylim(c(0,100))+
  #geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$mean.tot), nudge_x = -0.25, nudge_y = 4) +
  ggtitle(finaltitletext)+
  facet_grid(~treatment)
p+ scale_fill_manual(values = palette.trt)
if (saveplot)
{ 
  finalsavetext = paste(titletext, savetitle, sep="_")
  finalsavetext <- paste(resultspath,finalsavetext,sep="")
  ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
}
}


# barplot of mean mortality 
#
generateAndPlotMeansPercentMortalityByStrainTreatmentAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present, strain,treatment) %>%
    summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
              sd.tot = sd(mo_perc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Percentage Mortality by Strain, Treatment and Host Status")
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  dftemp <<- means_tot
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,100))+
    #geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$mean.tot), nudge_x = -0.25, nudge_y = 4) +
    ggtitle(finaltitletext)+
    facet_grid(strain~treatment+host_present)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}

generateAndPlotMeansPercentMortalityByTreatmentStrainAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present, strain,treatment) %>%
    summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
              sd.tot = sd(mo_perc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Percentage Mortality by Treatment, Strain and Host Status")
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  dftemp <<- means_tot
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Treatment") +
    ylim(c(0,100))+
    #geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$mean.tot), nudge_x = -0.25, nudge_y = 4) +
    ggtitle(finaltitletext)+
    facet_grid(strain~host_present)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean mortality 
#
generateAndPlotMeansPercentMortalityByOperatorStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,strain,treatment) %>%
    summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
              sd.tot = sd(mo_perc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Percentage Mortality by Operator Strain and Treatment")
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  dftemp <<- means_tot
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,100))+
    #geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$mean.tot), nudge_x = -0.25, nudge_y = 4) +
    ggtitle(finaltitletext)+
    facet_grid(experimenter~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}





# barplot of mean mortality - number of dead representation
#
generateAndPlotMeansMortality24hByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(mort24, na.rm = TRUE),
              sd.tot = sd(mort24, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito 24h Mortality by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,5))+
    ggtitle(finaltitletext)+
    facet_grid(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean mortality - number of dead representation
#
generateAndPlotMeansMortality24h_Num_ByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(mort_1, na.rm = TRUE),
              sd.tot = sd(mort_1, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito 24h Mortality by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,5))+
    ggtitle(finaltitletext)+
    facet_grid(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


generateAndPlotMeansMortality48h_Num_ByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(mort_2, na.rm = TRUE),
              sd.tot = sd(mort_2, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito 48h Mortality by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,5))+
    ggtitle(finaltitletext)+
    facet_grid(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


generateAndPlotMeansMortality72h_Num_ByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(mort_3, na.rm = TRUE),
              sd.tot = sd(mort_3, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito 72h Mortality by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,5))+
    ggtitle(finaltitletext)+
    facet_grid(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean mortality by treatment and site
#
generateAndPlotMeansMortalityByTreatmentAndSite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,treatment) %>%
    summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
              sd.tot = sd(mo_perc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Mortality by Treatment and Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Treatment") +
    ggtitle(finaltitletext)+
    facet_grid(site~.)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean mortality by treatment and operator
#
generateAndPlotMeansMortalityByTreatmentAndOperator <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,treatment) %>%
    summarise(mean.tot = mean(mo_perc, na.rm = TRUE),
              sd.tot = sd(mo_perc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Mortality by Operator and Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Mortality (95% CI)") + xlab("Treatment") +
    ggtitle(finaltitletext)+
    facet_grid(experimenter~.)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean longevity by str and TRT 
#
generateAndPlotMeansLongevityByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(longevity, na.rm = TRUE),
              sd.tot = sd(longevity, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Post Test Longevity by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Longevity (days) (95% CI)") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_grid(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean longevity by TRT and site
#
generateAndPlotMeansLongevityBySiteAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,strain,treatment) %>%
    summarise(mean.tot = mean(longevity, na.rm = TRUE),
              sd.tot = sd(longevity, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Post Test Longevity by Site and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Longevity (days) (95% CI)") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_grid(site~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# barplot of mean longevity by host status and trt 
#
generateAndPlotMeansLongevityByHostAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host,strain,treatment) %>%
    summarise(mean.tot = mean(longevity, na.rm = TRUE),
              sd.tot = sd(longevity, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Post Test Longevity by Host Status and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Longevity (days) (95% CI)") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_grid(host~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean longevity by operator status and trt 
#
generateAndPlotMeansLongevityByOperatorAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,strain,treatment) %>%
    summarise(mean.tot = mean(longevity, na.rm = TRUE),
              sd.tot = sd(longevity, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Post Test Longevity by Operator and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Longevity (days) (95% CI)") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_grid(experimenter~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean longevity by label and treatment (grouped by round of tests)
#
generateAndPlotMeansLongevityByLabelAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(round,strain,treatment) %>%
    summarise(mean.tot = mean(longevity, na.rm = TRUE),
              sd.tot = sd(longevity, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Post Test Longevity by Round Number and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Longevity (days) (95% CI)") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_grid(round~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean 1h kd 
#
generateAndPlotMeansKDat1hByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(kd_1h, na.rm = TRUE),
              sd.tot = sd(kd_1h, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito 1h KD by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 1h (95%  CI") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_wrap(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# barplot of mean 1h kd by treatment and site
#
generateAndPlotMeansKDat1hBySiteAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,treatment) %>%
    summarise(mean.tot = mean(kd_1h, na.rm = TRUE),
              sd.tot = sd(kd_1h, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito 1h KD by Site and Treatment")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 1h (95%  CI)") + xlab("Treatment") +
    ggtitle(finaltitletext)+
    facet_wrap(site~.)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean 1h kd by treatment and operator
#
generateAndPlotMeansKDat1hByOperatorAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,treatment) %>%
    summarise(mean.tot = mean(kd_1h, na.rm = TRUE),
              sd.tot = sd(kd_1h, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito 1h KD by Operator and Treatment")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 1h") + xlab("Treatment") +
    ggtitle(finaltitletext)+
    facet_wrap(experimenter~.)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean 3min kd 
#
generateAndPlotMeansKDat3MinByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(kd_3min, na.rm = TRUE),
              sd.tot = sd(kd_3min, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito 3min KD by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 3min") + xlab("Strain") +
    ggtitle(finaltitletext)+
    facet_wrap(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of grouped mean kd 3m, 1h, mort 24h
#
generateAndPlotMeansGroupedKDMortStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  df = df %>% select(!(kd_24h))
  vic.condition <- df %>% gather( cond, value, kd_3min:mort_1, factor_key=TRUE)
  df = vic.condition
  df$cond =  recode(df$cond, mort_1 = "mort_24h")
  df$cond = as.factor(df$cond)
  means_tot <- df %>%
    group_by(strain,treatment,cond) %>%
    summarise(mean.tot = mean(((value/nummoz)*100), na.rm = TRUE),
              sd.tot = sd(((value/nummoz)*100), na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  means_tot$upper.ci.tot = replace(means_tot$upper.ci.tot, means_tot$upper.ci.tot>100,100)  
  finaltitletext = paste(titletext, ": Mean Mosquito knockdown and mortality by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=factor(cond))) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = cond), stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), position = position_dodge(0.9), linewidth=0.5, alpha=0.9, size=1) +
    ylab("percentage knockdown and mortality (95% CI)") + xlab("Strain") +
    ylim(c(0,100))+
    ggtitle(finaltitletext)+
    theme(legend.title=element_blank()) 
  p  
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}




# barplot of grouped mean kd 3m, 1h, mort 24h
#
generateAndPlotMeansGroupedKDMortStrainAndTreatmentAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  df = df %>% select(!(kd_24h))
  vic.condition <- df %>% gather( cond, value, kd_3min:mort_1, factor_key=TRUE)
  df = vic.condition
  df$cond =  recode(df$cond, mort_1 = "mort_24h")
  df$cond = as.factor(df$cond)
  means_tot <- df %>%
    group_by(strain,treatment,host_present,cond) %>%
    summarise(mean.tot = mean(((value/nummoz)*100), na.rm = TRUE),
              sd.tot = sd(((value/nummoz)*100), na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
           lower.ci.tot = ifelse(means_tot$lower.ci.tot <0, 0, means_tot$lower.ci.tot)
  dftemp <<- means_tot
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  means_tot$upper.ci.tot = replace(means_tot$upper.ci.tot, means_tot$upper.ci.tot>100,100)  
  finaltitletext = paste(titletext, ": Mean Mosquito knockdown and mortality by Strain and Treatment and Host")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=factor(cond))) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = cond), stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), position = position_dodge(0.9), linewidth=0.5, alpha=0.9, size=1) +
    ylab("percentage knockdown and mortality (95% CI)") + xlab("Strain") +
    facet_grid(.~host_present)+
    ylim(c(0,100))+
    ggtitle(finaltitletext)+
    theme(legend.title=element_blank()) 
  p  
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}



# barplot of mean 3min kd 
#
generateAndPlotMeansKDat3MinPercentByStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(kd_3min_pc, na.rm = TRUE),
              sd.tot = sd(kd_3min_pc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  finaltitletext = paste(titletext, ": Mean Mosquito 3min KD Percentage by Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 3min Percentage (95% CI)") + xlab("Strain") +
    ylim(c(0,100))+
    ggtitle(finaltitletext)+
    facet_wrap(~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# barplot of mean 3min kd 
#
generateAndPlotMeansKDat3MinPercentByOperatorStrainAndTreatment <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,strain,treatment) %>%
    summarise(mean.tot = mean(kd_3min_pc, na.rm = TRUE),
              sd.tot = sd(kd_3min_pc, na.rm = TRUE),
              n.tot = n())%>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  means_tot$lower.ci.tot = replace(means_tot$lower.ci.tot, means_tot$lower.ci.tot<0,0)  
  finaltitletext = paste(titletext, ": Mean Mosquito 3min KD Percentage by Operator, Strain and Treatment")
  p <- ggplot(means_tot, aes(x=strain, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=strain, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean KD at 3min Percentage (95% CI)") + xlab("Strain") +
    ylim(c(0,100))+
    ggtitle(finaltitletext)+
    facet_wrap(experimenter~treatment)
  p+ scale_fill_manual(values = palette.trt)
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# plot mean total inactive by strain and treatment
#
generateAndPlotMeansTotInactiveWithDataByStrain <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(totinact, na.rm = TRUE),
              sd.tot = sd(totinact, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  #  finaltitletext= " "
  finaltitletext = paste(titletext, "Mean Mosquito Total Resting Period by Strain")
  means_tot$secs = as.character(format(round(means_tot$mean.tot,2),nsmall=2))
  means_tot$secs = paste(means_tot$secs, "s")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_linerange(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=.25) +
    geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$secs), nudge_x = -0.25, nudge_y = 4) +
    ylab("Mean Total Resting Period (s) (95% CI)") + xlab("Treatment") +
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 20, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total inactive by site and treatment
#
generateAndPlotMeansTotInactiveWithDataBySite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,treatment) %>%
    summarise(mean.tot = mean(totinact, na.rm = TRUE),
              sd.tot = sd(totinact, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Resting Period by Site")
  means_tot$secs = as.character(format(round(means_tot$mean.tot,2),nsmall=2))
  means_tot$secs = paste(means_tot$secs, "s")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_linerange(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=.25) +
    geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$secs), nudge_x = -0.25, nudge_y = 2) +
    ylab("Mean Total Resting Period (s) (95% CI)") + xlab("Treatment") +
    facet_grid(site~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 20, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total inactive by operator
#
generateAndPlotMeansTotInactiveWithDataByOperator <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,treatment) %>%
    summarise(mean.tot = mean(totinact, na.rm = TRUE),
              sd.tot = sd(totinact, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Resting Period by Operator")
  #  write.csv(means_tot,"mean resting period.csv",col.names = TRUE)
  means_tot$secs = as.character(format(round(means_tot$mean.tot,2),nsmall=2))
  means_tot$secs = paste(means_tot$secs, "s")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_linerange(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=.25) +
    geom_text(data=means_tot,aes(x=treatment,y=mean.tot, label=means_tot$secs), nudge_x = -0.25, nudge_y = 2) +
    ylab("Mean Total Resting Period (s) (95% CI)") + xlab("Treatment") +
    facet_grid(experimenter~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 20, height = 20, units = "cm", dpi=300)
  }
}


# plot mean Inactivity totals by strain and treatment
#
generateAndPlotMeansTotInactivityByStrain <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(strain,treatment) %>%
    summarise(mean.tot = mean(totinact, na.rm = TRUE),
              sd.tot = sd(totinact, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Inactivity by Strain")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Inactivity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~.)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean Inactivity totals by strain and treatment and host
#
generateAndPlotMeansTotInactivityByStrainTreatmentAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present,strain,treatment) %>%
    summarise(mean.tot = mean(totinact, na.rm = TRUE),
              sd.tot = sd(totinact, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Inactivity by Strain, Treatment and Host Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Inactivity (95% CI)") + xlab("Treatment") +
    facet_grid(~strain+host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean totals by site
#
generateAndPlotMeansTotBySite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(~site)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by strain and site
#
generateAndPlotMeansTotByStrainAndSite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Strain and Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~site)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}





# mean total activity by site and host
#
generateAndPlotMeansTotBySiteAndHost <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,host_present,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Site and Host Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(host_present~site)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by strain and host 
#
generateAndPlotMeansTotByStrainAndHost <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present,strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Strain and Host Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by strain and site
#
generateAndPlotMeansTotByStrainAndSite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(site,strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Strain and Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~site)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}

# mean total activity by strain and operator
#
generateAndPlotMeansTotByStrainAndOperator <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Strain and Operator")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(strain~experimenter)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by operator and site
#
generateAndPlotMeansTotByOperatorAndSite <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,site,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Operator and Site")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(experimenter~site)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by operator and host status
#
generateAndPlotMeansTotByOperatorAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,host_present,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Operator and Host Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(experimenter~host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# plot mean total activity by operator and experimental round (if recorded in the experimentlabel field)
#
generateAndPlotMeansTotByOperatorAndExpRound <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,experimentlabel,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
    dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity By Operator and Experimental Round")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
   # geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(experimentlabel~experimenter)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# mean total activity by operator
#
generateAndPlotMeansTotByOperator <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(experimenter,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Operator")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(~experimenter)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# mean total activity by strain, treatment and host status
#
generateAndPlotMeansTotByStrainTreatmentAndHostStatus <- function(df, titletext, resultspath, saveplot, savetitle)
{
  means_tot <- df %>%
    group_by(host_present, strain,treatment) %>%
    summarise(mean.tot = mean(total, na.rm = TRUE),
              sd.tot = sd(total, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_tot
  finaltitletext = paste(titletext, ": Mean Mosquito Total Activity by Strain, Treatment and Host Status")
  p <- ggplot(means_tot, aes(x=treatment, y=mean.tot, fill=treatment)) + # fill=name allow to automatically dedicate a color for each group
    geom_bar(aes(fill = treatment),stat = "identity",position = "dodge")+
    geom_errorbar(aes(x=treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), linewidth=0.5, alpha=0.9, size=1) +
    ylab("Mean Total Activity (95% CI)") + xlab("Treatment") +
    facet_grid(~strain+host_present)+
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.trt)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}





#######################
#
# region activity plots
#
#######################

# absolute region activity
#
generateAndPlotRegionMeansPlot <- function(df,titletext,  resultspath,saveplot, savetitle, maxy = 4000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(region,treatment) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity")
  dftemp <<- means_reg
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,maxy)+
    facet_grid(~treatment) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# absolute region activity by host status
#
generateAndPlotRegionMeansPlotbyHost <- function(df,titletext,  resultspath,saveplot, savetitle, maxy = 4000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(host_present,region,treatment) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity by Host Status")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,maxy)+
    facet_grid(host_present~treatment) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}

# absolute region activity by strain
#
generateAndPlotRegionMeansPlotbyStrain <- function(df,titletext,  resultspath, saveplot, savetitle, maxy = 6000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(strain,region,treatment) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity by Strain")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,maxy)+
    facet_grid(strain~treatment) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
   p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# absolute region activity by strain
#
generateAndPlotRegionMeansPlotbyStrainAndWashedStatus <- function(df,titletext,  resultspath, saveplot, savetitle, maxy = 4000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(strain,region,treatment,washed) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity by Strain And Wash Status")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,maxy)+
    facet_grid(strain~treatment+washed) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# absolute region activity by strain and host status
#
generateAndPlotRegionMeansPlotbyStrainTreatmentAndHostStatus <- function(df,titletext,  resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(strain,region,treatment,host_present) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity by Strain, Treatment And Host Status")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    #ylim(0,maxy)+
    facet_grid(strain~treatment+host_present) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}

# absolute region activity by strain and host status
#
generateAndPlotRegionMeansPlotbyTreatmentStrainAndHostStatus <- function(df,titletext,  resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(strain,region,treatment,host_present) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means_reg
  finaltitletext = paste(titletext, ": Mean Regional Activity by Treatment, Strain And Host Status")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    #ylim(0,maxy)+
    facet_grid(treatment~strain+host_present) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# absolute region activity by site
#
generateAndPlotRegionMeansPlotbySite <- function(df,titletext,  resultspath,saveplot, savetitle, maxy = 4000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(site,region,treatment) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Activity by Site")
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,maxy)+
    facet_grid(site~treatment) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# absolute region activity by site by operator
#
generateAndPlotRegionMeansPlotbyOperator <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 4000)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means_reg <- vic.reg1 %>%
    group_by(experimenter,region,strain,treatment) %>%
    summarise(mean.tot = mean(activity, na.rm = TRUE),
              sd.tot = sd(activity, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
           lower.ci.tot = ifelse(means_reg$lower.ci.tot <0, 0, means_reg$lower.ci.tot)  
  finaltitletext = paste(titletext, ": Mean Regional Activity by Operator, Strain and Treatment")
  dftemp <<- means_reg  
  p <- ggplot(means_reg) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    #ylim(0,maxy)+
    facet_grid(experimenter~treatment+strain) + ylab("Mean Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


#####################
#
#  proportional regional activity
#
#####################


# proportional region activity by site
#
generateAndPlotRegionMeansPropPlot <- function(df,titletext,  resultspath,saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(region, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(~treatment) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by host status
#
generateAndPlotRegionMeansPropPlotbyHost <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(host_present,region, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Host Status")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(host_present~treatment) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by strain, treatment and host status
#
generateAndPlotRegionMeansPropPlotbyStrTrtandHost <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(host_present,region, strain, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Strain, Treatment and Host Status")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    #facet_grid(host_present~strain+treatment) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    facet_grid(strain~treatment+host_present) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by treatment, strain and host status
#
generateAndPlotRegionMeansPropPlotbyTrtStrandHost <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(host_present,region, strain, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Treatment, Strain and Host Status")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(treatment~strain+host_present) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by strain
#
generateAndPlotRegionMeansPropPlotbyStrain <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(strain,region, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  dftemp <<- means.reg.prop
#  dftemp <<- vic.reg1
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Strain")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(strain~treatment) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# proportional region activity by site and treatment
#
generateAndPlotRegionMeansPropPlotbySite <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(site,region, treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Site")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(site~treatment) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by operator
#
generateAndPlotRegionMeansPropPlotbyOperator <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(experimenter,region, strain,treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Operator")
  p <- ggplot(means.reg.prop) +     
    geom_bar( aes(x=region, y=mean.tot, fill=region), stat="identity", alpha=1.5) +
    ylim(0,1)+
    facet_grid(experimenter~treatment+strain) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    geom_errorbar(aes(x=region, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, linewidth = 0.5) +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# proportional region activity by operator and site
#
generateAndPlotRegionMeansPropPlotbyOperatorAndSite <- function(df,titletext, resultspath, saveplot, savetitle)
{
  vic.reg11 <- df %>% gather( reg, activity, top_half_tot:bottom_half_tot, factor_key=TRUE)
  vic.reg1 <- vic.reg11 %>% separate(reg, c("region", NA, NA), sep = "_")
  means.reg.prop <- vic.reg1 %>%
    mutate(p1 = activity/ total) %>%
    group_by(site,experimenter,region,treatment) %>%
    summarise(mean.tot = mean(p1, na.rm = TRUE),
              sd.tot = sd(p1, na.rm = TRUE),
              n.tot = n()) %>%
    mutate(se.tot = sd.tot / sqrt(n.tot),
           lower.ci.tot = mean.tot - qt(1 - (0.05 / 2), n.tot - 1) * se.tot,
           upper.ci.tot = mean.tot + qt(1 - (0.05 / 2), n.tot - 1) * se.tot)
  finaltitletext = paste(titletext, ": Mean Regional Proportional Activity by Operator and Site")
  dftemp <<- means.reg.prop
  p <- ggplot(means.reg.prop, aes(x=treatment, y=mean.tot, fill=region)) +     
    geom_bar( position="dodge", alpha=1.5,  stat="identity") +
    geom_errorbar(aes(x= treatment, ymin=lower.ci.tot, ymax=upper.ci.tot), width=0.4, alpha=0.9, position=position_dodge(0.9), linewidth = 0.5) +
    ylim(0,1)+
    facet_grid(experimenter~site) + ylab("Mean Proportion Activity (95% CI)") + xlab("Region") +
    ggtitle(finaltitletext)
  p+ scale_fill_manual(values = palette.reg2)
  p
  if (saveplot)
  { 
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


############################
#
# inactivity (resting proxy) over time plots
#
############################



# inactivity (resting) over time by strain
#
generateAndPlotTemporalInactivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  ############# Inactivity over time analysis #################
  # put the time columns (t5-t180) into a single column
  dfiot <-  gather(df, time, inactivity, inactive_t5:inactive_t180)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by( strain,treatment,vcd_record_id, time)
  dfiotsummary = summarize(dfiot, inactivity = sum(inactivity)) 
  means_iot <- dfiotsummary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  finaltitletext = paste(titletext, ": Mean Inactivity Over Time by Strain")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain)+ 
    #facet_grid(rows = vars(treatment)) + 
    ylab("Mean Inactivity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)+
    geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}  

# inactivity (resting) over time by Trt strain and Host Status
#
generateAndPlotTemporalInactivityByTreatmentStrainAndHostStatus <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  ############# Inactivity over time analysis #################
  # put the time columns (t5-t180) into a single column
  dfiot <-  gather(df, time, inactivity, inactive_t5:inactive_t180)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by( host_present, strain,treatment,vcd_record_id, time)
  dfiotsummary = summarize(dfiot, inactivity = sum(inactivity)) 
  means_iot <- dfiotsummary %>%
    group_by(host_present, strain,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  finaltitletext = paste(titletext, ": Mean Inactivity Over Time by Treatment Strain and Host Status")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain+host_present)+ 
    #facet_grid(rows = vars(treatment)) + 
    ylab("Mean Inactivity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)+
    geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}  


# inactivity (resting) over time by operator
#
generateAndPlotTemporalInactivityByOperator <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  ############# Inactivity over time analysis #################
    # put the time columns (t5-t180) into a single column
  dfiot <-  gather(df, time, inactivity, inactive_t5:inactive_t180)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by( experimenter,treatment,vcd_record_id, time)
  dfiotsummary = summarize(dfiot, inactivity = sum(inactivity)) 
  means_iot <- dfiotsummary %>%
    group_by(experimenter,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  finaltitletext = paste(titletext, ": Mean Inactivity Over Time by Operator")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~experimenter) + ylab("Mean Inactivity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)+
  geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}  


# inactivity (resting) over time by site
#
generateAndPlotTemporalInactivityBySite <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  ############# Inactivity over time analysis #################
  # put the time columns (t5-t180) into a single column
  dfiot <-  gather(df, time, inactivity, inactive_t5:inactive_t180)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by( site,treatment,vcd_record_id, time)
  dfiotsummary = summarize(dfiot, inactivity = sum(inactivity)) 
  means_iot <- dfiotsummary %>%
    group_by(site,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  finaltitletext = paste(titletext, ": Mean Inactivity Over Time by Site")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~site) + ylab("Mean Inactivity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)+
    geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
  
}  


# cumulative inactivity (resting) over time by strain
#
generateAndPlotCumulativeInactivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfiot <-  gather(df, time, activity, inactive_t5:inactive_t180)
  dfiot$time = sub(".*_t", "", dfiot$time, ignore.case = TRUE)
  dfiot$time = as.numeric(dfiot$time)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by(strain,treatment,vcd_record_id, time)
  dfiot$cumul_inactivity = 0 
  print(nrow(dfiot))
  dfiot <- dfiot[order(dfiot$vcd_record_id,dfiot$time),]
  tot = 0 
  for (i in 1:nrow(dfiot))
  {
    t = dfiot$time[i]
    if (t == 180)
    {
      tot = tot + dfiot$activity[i] 
      dfiot$cumul_inactivity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfiot$activity[i]
      dfiot$cumul_inactivity[i] = tot
    }
  }
  dftemp <<- dfiot
  dfiot.summary = summarize(dfiot, inactivity = sum(cumul_inactivity)) 
  
  means_iot <- dfiot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Resting Over Time by Strain")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain)+
    ggtitle(finaltitletext)+
    #  facet_grid(rows = vars(treatment))+
    ylab("Mean Cumulative Resting (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# cumulative inactivity (resting) over time by site
#
generateAndPlotCumulativeInactivityBySite <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfiot <-  gather(df, time, activity, inactive_t5:inactive_t180)
  dfiot$time = sub(".*_t", "", dfiot$time, ignore.case = TRUE)
  dfiot$time = as.numeric(dfiot$time)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by(site,treatment,vcd_record_id, time)
  dfiot$cumul_inactivity = 0 
  print(nrow(dfiot))
  dfiot <- dfiot[order(dfiot$vcd_record_id,dfiot$time),]
  tot = 0 
  for (i in 1:nrow(dfiot))
  {
    t = dfiot$time[i]
    if (t == 180)
    {
      tot = tot + dfiot$activity[i] 
      dfiot$cumul_inactivity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfiot$activity[i]
      dfiot$cumul_inactivity[i] = tot
    }
  }
  dftemp <<- dfiot
  dfiot.summary = summarize(dfiot, inactivity = sum(cumul_inactivity)) 
  
  means_iot <- dfiot.summary %>%
    group_by(site,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Resting Over Time by site")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~site)+
    ggtitle(finaltitletext)+
    #  facet_grid(rows = vars(treatment))+
    ylab("Mean Cumulative Resting (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# cumulative inactivity (resting) over time by operator
generateAndPlotCumulativeInactivityByOperator <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfiot <-  gather(df, time, activity, inactive_t5:inactive_t180)
  dfiot$time = sub(".*_t", "", dfiot$time, ignore.case = TRUE)
  dfiot$time = as.numeric(dfiot$time)
  # make time a factor so it can be grouped
  dfiot$time = factor(dfiot$time)
  dfiot <- dfiot %>%  group_by(experimenter,treatment,vcd_record_id, time)
  dfiot$cumul_inactivity = 0 
  print(nrow(dfiot))
  dfiot <- dfiot[order(dfiot$vcd_record_id,dfiot$time),]
  tot = 0 
  for (i in 1:nrow(dfiot))
  {
    t = dfiot$time[i]
    if (t == 180)
    {
      tot = tot + dfiot$activity[i] 
      dfiot$cumul_inactivity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfiot$activity[i]
      dfiot$cumul_inactivity[i] = tot
    }
  }
  dftemp <<- dfiot
  dfiot.summary = summarize(dfiot, inactivity = sum(cumul_inactivity)) 
  
  means_iot <- dfiot.summary %>%
    group_by(experimenter,treatment,time) %>%
    summarise(mean.iot = mean(inactivity, na.rm = TRUE),
              sd.iot = sd(inactivity, na.rm = TRUE),
              n.iot = n()) %>%
    mutate(se.iot = sd.iot / sqrt(n.iot),
           lower.ci.iot = mean.iot - qt(1 - (0.05 / 2), n.iot - 1) * se.iot,
           upper.ci.iot = mean.iot + qt(1 - (0.05 / 2), n.iot - 1) * se.iot) # 95% CI using the t-distrn
  means_iot$time = sub(".*_t", "", means_iot$time, ignore.case = TRUE)
  means_iot$time = as.numeric(means_iot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Resting Over Time by Operator")
  p <- ggplot(data=means_iot, aes(x=time, y=mean.iot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~experimenter)+
    ggtitle(finaltitletext)+
    #  facet_grid(rows = vars(treatment))+
    ylab("Mean Cumulative Resting (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_iot$lower.ci.iot, ymax=means_iot$upper.ci.iot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


##########################
#
# plot activity over time  
#  
##########################



# activity over time by treatment
#
generateAndPlotTemporalActivityByTreatment <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  #dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Treatment")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain)+
    ggtitle(finaltitletext)+
    #ylim(c(NA,180))+
   ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time by treatment
#
generateAndPlotTemporalActivityByTreatmentOffsetStart <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_5:tot_t_175)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  #dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Treatment")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain)+
    ggtitle(finaltitletext)+
    #ylim(c(NA,180))+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time by Strain
#
generateAndPlotTemporalActivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
        means_aot$lower.ci.aot <- ifelse(means_aot$lower.ci.aot <=0, 0, means_aot$lower.ci.aot)
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Strain")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(strain~treatment)+
    ggtitle(finaltitletext)+
    #ylim(c(NA,180))+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time by Strain
#
generateAndPlotTemporalActivityByStrainOffsetStart <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_5:tot_t_175)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$lower.ci.aot <- ifelse(means_aot$lower.ci.aot <=0, 0, means_aot$lower.ci.aot)
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Strain")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(strain~treatment)+
    ggtitle(finaltitletext)+
    #ylim(c(NA,180))+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# activity over time by Strain
#
generateAndPlotTemporalActivityByStrain5min <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_300)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$lower.ci.aot <- ifelse(means_aot$lower.ci.aot <=0, 0, means_aot$lower.ci.aot)
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Strain")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(strain~treatment)+
    ggtitle(finaltitletext)+
    #ylim(c(NA,180))+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}





# activity over time by strain
#
generateAndPlotTemporalActivityByTreatmentStrainAndHostStatus <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot$treatment = factor(dfaot$treatment)
  dfaot$host_present = factor(dfaot$host_present)
  dfaot <- dfaot %>%  group_by(host_present, strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(host_present, strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
   
  means_aot$lower.ci.aot <- ifelse(means_aot$lower.ci.aot <=0, 0, means_aot$lower.ci.aot)
  
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  #dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Treatment, Strain and Host Status")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(strain~treatment+host_present)+
    ggtitle(finaltitletext)+
    ylim(c(NA,180))+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# cumulative activity over time by strain
#
generateAndPlotCumulativeActivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  dfaot$time = sub(".*_t_", "", dfaot$time, ignore.case = TRUE)
  dfaot$time = as.numeric(dfaot$time)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  dfaot$cumul_activity = 0 
  print(nrow(dfaot))
  dfaot <- dfaot[order(dfaot$vcd_record_id,dfaot$time),]
  tot = 0 
  for (i in 1:nrow(dfaot))
  {
    t = dfaot$time[i]
    if (t == 180)
    {
      tot = tot + dfaot$activity[i] 
      dfaot$cumul_activity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfaot$activity[i]
      dfaot$cumul_activity[i] = tot
    }
  }
  dftemp <<- dfaot
  dfaot.summary = summarize(dfaot, activity = sum(cumul_activity)) 
  
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
 # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Activity Over Time by Strain")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain)+
    ggtitle(finaltitletext)+
    ylab("Mean Cumulative Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# cumulative activity over time by site
#
generateAndPlotCumulativeActivityBySite <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  dfaot$time = sub(".*_t_", "", dfaot$time, ignore.case = TRUE)
  dfaot$time = as.numeric(dfaot$time)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(site,treatment,vcd_record_id, time)
  dfaot$cumul_activity = 0 
  print(nrow(dfaot))
  dfaot <- dfaot[order(dfaot$vcd_record_id,dfaot$time),]
  tot = 0 
  for (i in 1:nrow(dfaot))
  {
    t = dfaot$time[i]
    if (t == 180)
    {
      tot = tot + dfaot$activity[i] 
      dfaot$cumul_activity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfaot$activity[i]
      dfaot$cumul_activity[i] = tot
    }
  }
  dftemp <<- dfaot
  dfaot.summary = summarize(dfaot, activity = sum(cumul_activity)) 
  
  means_aot <- dfaot.summary %>%
    group_by(site,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Activity Over Time by Site")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~site)+
    ggtitle(finaltitletext)+
    ylab("Mean Cumulative Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# cumulative activity over time by operator
#
generateAndPlotCumulativeActivityByOperator <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  dfaot$time = sub(".*_t_", "", dfaot$time, ignore.case = TRUE)
  dfaot$time = as.numeric(dfaot$time)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(experimenter,treatment,vcd_record_id, time)
  dfaot$cumul_activity = 0 
  print(nrow(dfaot))
  dfaot <- dfaot[order(dfaot$vcd_record_id,dfaot$time),]
  tot = 0 
  for (i in 1:nrow(dfaot))
  {
    t = dfaot$time[i]
    if (t == 180)
    {
      tot = tot + dfaot$activity[i] 
      dfaot$cumul_activity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfaot$activity[i]
      dfaot$cumul_activity[i] = tot
    }
  }
  dftemp <<- dfaot
  dfaot.summary = summarize(dfaot, activity = sum(cumul_activity)) 
  
  means_aot <- dfaot.summary %>%
    group_by(experimenter,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Cumulative Activity Over Time by Operator")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~experimenter)+
    ggtitle(finaltitletext)+
    ylab("Mean Cumulative Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  p+ scale_color_manual(values = palette.trt)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# cumulative activity over time by strain
#
generateAndPlotCumulativeOverlaidActivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  dfaot$time = sub(".*_t_", "", dfaot$time, ignore.case = TRUE)
  dfaot$time = as.numeric(dfaot$time)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  dfaot$cumul_activity = 0 
  print(nrow(dfaot))
  dfaot <- dfaot[order(dfaot$vcd_record_id,dfaot$time),]
  tot = 0 
  for (i in 1:nrow(dfaot))
  {
    t = dfaot$time[i]
    if (t == 180)
    {
      tot = tot + dfaot$activity[i] 
      dfaot$cumul_activity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfaot$activity[i]
      dfaot$cumul_activity[i] = tot
    }
  }
  dftemp <<- dfaot
  dfaot.summary = summarize(dfaot, activity = sum(cumul_activity)) 
  
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Overlaid Cumulative Activity Over Time by Treatment")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(rows = vars(strain))+
    ggtitle(finaltitletext)+
    #  facet_grid(rows = vars(treatment))+
    ylab("Mean Cumulative Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# cumulative overlaid inactivity over time by strain
#
generateAndPlotCumulativeOverlaidInactivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, inactive_t5:inactive_t180)
  dfaot$time = sub(".*_t", "", dfaot$time, ignore.case = TRUE)
  dfaot$time = as.numeric(dfaot$time)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  dfaot$cumul_inactivity = 0 
  print(nrow(dfaot))
  dfaot <- dfaot[order(dfaot$vcd_record_id,dfaot$time),]
  tot = 0 
  for (i in 1:nrow(dfaot))
  {
    t = dfaot$time[i]
    if (t == 180)
    {
      tot = tot + dfaot$activity[i] 
      dfaot$cumul_inactivity[i] = tot
      tot = 0 
    }
    else
    {
      tot = tot + dfaot$activity[i]
      dfaot$cumul_inactivity[i] = tot
    }
  }
  dftemp <<- dfaot
  dfaot.summary = summarize(dfaot, activity = sum(cumul_inactivity)) 
  
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  # dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Overlaid Cumulative Inactivity Over Time by Treatment")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(rows = vars(strain))+
    ggtitle(finaltitletext)+
    ylab("Mean Cumulative Resting (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity overlaid over time by strain
#
generateAndPlotOverlaidTemporalActivityByStrain <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(strain,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  means_aot <- dfaot.summary %>%
    group_by(strain,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Overlaid Activity Over Time by Strain")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(rows = vars(strain))+
    ggtitle(finaltitletext)+
    ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.05)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time by operator
#
generateAndPlotTemporalActivityByOperator <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
# put the time columns (t10-t180) into a single column
dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
# make time a factor so it can be grouped
dfaot$time = factor(dfaot$time)
dfaot <- dfaot %>%  group_by(experimenter,strain,treatment,vcd_record_id, time)
print(nrow(dfaot))
dfaot.summary = summarize(dfaot, activity = sum(activity)) 
#dftemp <<- dfaot.summary
means_aot <- dfaot.summary %>%
  group_by(experimenter,strain,treatment,time) %>%
  summarise(mean.aot = mean(activity, na.rm = TRUE),
            sd.aot = sd(activity, na.rm = TRUE),
            n.aot = n()) %>%
  mutate(se.aot = sd.aot / sqrt(n.aot),
         lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
         upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
         ) # 95% CI using the t-distrn
      means_aot$lower.ci.aot <- ifelse(means_aot$lower.ci.aot <=0, 0, means_aot$lower.ci.aot)
means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
means_aot$time = as.numeric(means_aot$time)
dftemp <<- means_aot
finaltitletext = paste(titletext, ": Mean Activity Over Time by Str, Trt and Operator")
p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
  facet_grid(experimenter~strain+treatment) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
  ggtitle(finaltitletext)
# add error bars
p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
# show plot
p+ scale_color_manual(values = palette.trt)
p
if(saveplot)
{
  finalsavetext = paste(titletext, savetitle, sep="_")
  finalsavetext <- paste(resultspath,finalsavetext,sep="")
  ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
}
}


# activity over time by site
#
generateAndPlotTemporalActivityBySite <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  # put the time columns (t10-t180) into a single column
  dfaot <-  gather(df, time, activity, tot_t_10:tot_t_180)
  # make time a factor so it can be grouped
  dfaot$time = factor(dfaot$time)
  dfaot <- dfaot %>%  group_by(site,treatment,vcd_record_id, time)
  print(nrow(dfaot))
  dfaot.summary = summarize(dfaot, activity = sum(activity)) 
  #dftemp <<- dfaot.summary
  means_aot <- dfaot.summary %>%
    group_by(site,treatment,time) %>%
    summarise(mean.aot = mean(activity, na.rm = TRUE),
              sd.aot = sd(activity, na.rm = TRUE),
              n.aot = n()) %>%
    mutate(se.aot = sd.aot / sqrt(n.aot),
           lower.ci.aot = mean.aot - qt(1 - (0.05 / 2), n.aot - 1) * se.aot,
           upper.ci.aot = mean.aot + qt(1 - (0.05 / 2), n.aot - 1) * se.aot) # 95% CI using the t-distrn
  means_aot$time = sub(".*_t_", "", means_aot$time, ignore.case = TRUE)
  means_aot$time = as.numeric(means_aot$time)
  dftemp <<- means_aot
  finaltitletext = paste(titletext, ": Mean Activity Over Time by Site")
  p <- ggplot(data=means_aot, aes(x=time, y=mean.aot, colour=treatment)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~site) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)
  # add error bars
  p <- p+geom_ribbon(aes(ymin=means_aot$lower.ci.aot, ymax=means_aot$upper.ci.aot), linetype=2, alpha=0.1)
  # show plot
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time top bottom
#
generateAndPlotTemporalActivityTopBottomByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "Bottom", "Top")
dfaot_r$time = as.numeric(dfaot_r$time)
dfaot_grouped <- dfaot_r %>%                               
  group_by( strain,treatment,vcd_record_id,region, time) %>% 
  summarise(activity = sum(activity)) 
means_2reg <- dfaot_grouped %>%
  group_by(strain,treatment,time,region) %>%
  summarise(mean.reg = mean(activity, na.rm = TRUE),
            sd.reg = sd(activity, na.rm = TRUE),
            n.reg = n()) %>%
  mutate(se.reg = sd.reg / sqrt(n.reg),
         lower.ci.reg = mean.reg - qt(1 - (0.05 / 2), n.reg - 1) * se.reg,
         upper.ci.reg = mean.reg + qt(1 - (0.05 / 2), n.reg - 1) * se.reg) # 95% CI using the t-distrn
finaltitletext = paste(titletext, ": Mean Activity Over Time Top vs Bottom of Cone")
p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, colour=region)) + geom_line() + # + geom_point() includes dots
  facet_grid(treatment~strain) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
  ggtitle(finaltitletext)
# add error bars
if (useerrorbars)
  p <- p+geom_ribbon(aes(ymin=means_2reg$lower.ci.reg, ymax=means_2reg$upper.ci.reg), linetype=2, alpha=0.1)
# show plot
p
if(saveplot)
{
  finalsavetext = paste(titletext, savetitle, sep="_")
  finalsavetext <- paste(resultspath,finalsavetext,sep="")
  ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
}
}



# activity over time 4 regions separately
#
generateAndPlotTemporalActivity4RegionsByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")#separate the region and time into separate columns
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n()) %>%
    mutate(se.reg = sd.reg / sqrt(n.reg),
           lower.ci.reg = mean.reg - qt(1 - (0.05 / 2), n.reg - 1) * se.reg,
           upper.ci.reg = mean.reg + qt(1 - (0.05 / 2), n.reg - 1) * se.reg) # 95% CI using the t-distrn
  finaltitletext = paste(titletext, ": Mean Activity Over Time in Four Regions of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, colour=region)) + geom_line() + # + geom_point() includes dots
    facet_grid(treatment~strain) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
    ggtitle(finaltitletext)
  # add error bars
  if (useerrorbars)
    p <- p+geom_ribbon(aes(ymin=means_2reg$lower.ci.reg, ymax=means_2reg$upper.ci.reg), linetype=2, alpha=0.1)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



# activity over time top bottom Area Chart
#
generateAndPlotTemporalActivityTopBottomAreaChartByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  dftemp <<- means_2reg
      finaltitletext = paste(titletext, ": Mean Activity Over Time Area Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black") + 
    facet_grid(treatment~strain) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}

# activity over time top bottom Area Chart - 5min test
#
generateAndPlotTemporalActivityTopBottomAreaChartByTreatmentAndStrain5min <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Activity Over Time Area Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black") + 
    facet_grid(strain~treatment) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time top bottom Area Chart - by pre change nets and post change nets (lisa and jonathan)
#
#
# NOTE: use record instead of id to guarantee no duplicates
#
generateAndPlotTemporalActivityTopBottomAreaChartByPrePostAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,prepost,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,prepost,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Activity Over Time Area Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    theme(
      axis.text = element_text(size = 16),      # Axis labels
      axis.title = element_text(size = 16),     # Axis titles
      plot.title = element_text(size = 16),     # Plot title
      legend.text = element_text(size = 14),    # Legend text
      legend.title = element_text(size = 14)    # Legend title
    )+
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black") + 
    facet_grid(prepost~strain) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# activity over time top bottom Area Chart
#
generateAndPlotTemporalActivityTopBottomAreaChartByTreatmentStrainAndHostStatus <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( host_present, strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(host_present, strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Activity Over Time Area Top vs Bottom of Cone Trt, Str and Host")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black") + 
    facet_grid(treatment~strain+host_present) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time top bottom Area Chart - Proportional
#
generateAndPlotTemporalActivityTopBottomProportionalAreaChartByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Proportional Activity Over Time Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black", position = "fill") + 
    facet_grid(treatment~strain) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time top bottom Area Chart - Proportional - 5min test
#
generateAndPlotTemporalActivityTopBottomProportionalAreaChartByTreatmentAndStrain5min <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Proportional Activity Over Time Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black", position = "fill") + 
    facet_grid(strain~treatment) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time top bottom Area Chart - Proportional - pre and post (lisa and jonathan)
#
#
# NOTE: use record instead of id to guarantee no duplicates
#

generateAndPlotTemporalActivityTopBottomProportionalAreaChartByPrePostAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,prepost,record,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(strain,prepost,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Proportional Activity Over Time Top vs Bottom of Cone")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black", position = "fill") + 
    facet_grid(prepost~strain) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}





# activity over time top bottom Area Chart - Proportional
#
generateAndPlotTemporalActivityTopBottomProportionalAreaChartByTreatmentAndStrainAndHost <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")
  dfaot_r$region <- ifelse(dfaot_r$region %in% c("r2","r3"), "bottom", "top")
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( host_present, strain,treatment, vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_2reg <- dfaot_grouped %>%
    group_by(host_present, strain,treatment, time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  means_2reg$region <- factor(means_2reg$region, sort(unique(means_2reg$region), decreasing = TRUE))
  dftemp <<- means_2reg
  finaltitletext = paste(titletext, ": Mean Proportional Activity Over Time Top vs Bottom of Cone By Host")
  p <- ggplot(data=means_2reg, aes(x=time, y=mean.reg, fill=region)) + 
    #geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black", position = "fill") + 
    facet_grid(treatment~strain+host_present) + ylab("Mean Activity Top/Bottom") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg2) + # bar or column plots
    ggtitle(finaltitletext)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time 4 regions separately area chart
#
generateAndPlotTemporalActivity4RegionsAreaChartByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")#separate the region and time into separate columns
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_4reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
#  dftemp <<- means_4reg
    
  finaltitletext = paste(titletext, ": Mean Activity Over Time in Four Regions of Cone")
  p <- ggplot(data=means_4reg, aes(x=time, y=mean.reg, fill=region)) + 
  # geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black") + 
    facet_grid(treatment~strain) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg4)+
    ggtitle(finaltitletext)
  # add error bars
  #if (useerrorbars)
  #  p <- p+geom_ribbon(aes(ymin=means_4reg$lower.ci.reg, ymax=means_4reg$upper.ci.reg), linetype=2, alpha=0.1)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




# activity over time 4 regions separately area chart - proportional
#
generateAndPlotTemporalActivity4RegionsProportionalAreaChartByTreatmentAndStrain <- function(df,titletext, resultspath, saveplot, useerrorbars, savetitle)
{
  dfaot_r <-  gather(df, reg, activity, r0_t_5:r3_t_180)
  dfaot_r <- separate(dfaot_r,reg,into = c("region","time"),sep="_t_")#separate the region and time into separate columns
  dfaot_r$time = as.numeric(dfaot_r$time)
  dfaot_grouped <- dfaot_r %>%                               
    group_by( strain,treatment,vcd_record_id,region, time) %>% 
    summarise(activity = sum(activity)) 
  means_4reg <- dfaot_grouped %>%
    group_by(strain,treatment,time,region) %>%
    summarise(mean.reg = mean(activity, na.rm = TRUE),
              sd.reg = sd(activity, na.rm = TRUE),
              n.reg = n())
  #  dftemp <<- means_4reg
  
  finaltitletext = paste(titletext, ": Mean Activity Over Time in Four Regions of Cone")
  p <- ggplot(data=means_4reg, aes(x=time, y=mean.reg, fill=region)) + 
    # geom_line() + # + geom_point() includes dots
    geom_area(alpha=1 , size=1, colour="black", position = "fill") + 
    facet_grid(treatment~strain) + ylab("Mean Activity (95% CI)") + xlab("Time (Secs)")+
    scale_fill_manual(values = palette.reg4)+
    ggtitle(finaltitletext)
  # add error bars
  #if (useerrorbars)
  #  p <- p+geom_ribbon(aes(ymin=means_4reg$lower.ci.reg, ymax=means_4reg$upper.ci.reg), linetype=2, alpha=0.1)
  # show plot
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




############################
#
# scatter plots
#
############################



##
# scatter plots of tot activity vs mortality
##
generateAndPlotScatterActivityByMortality <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  finaltitletext = paste(titletext,"Total activity vs Percentage Mortality")
p = ggplot(df, aes(x=total, y=mo_perc,   shape=strain,  color=treatment, ylim(0,100) )) + 
  geom_point(size = 5 ) +
  geom_point() +
  scale_y_continuous()+
  scale_x_continuous()+
  xlab("Total Activity") + ylab("Percentage Mortality")+
  ggtitle(finaltitletext)
print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
p+ scale_color_manual(values = palette.trt)
p
if(saveplot)
{
  finalsavetext = paste(titletext, savetitle, sep="_")
  finalsavetext <- paste(resultspath,finalsavetext,sep="")
  ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
}
}

##
#  plots of tot activity vs date
##
generateAndPlotActivityByDate <- function(df,titletext, resultspath, saveplot, savetitle)
{
  dfgrouped <- df %>%
    group_by(date,id,strain,treatment)
  finaltitletext = paste(titletext,"Total activity vs Date")
  dftemp <<- dfgrouped
  p = ggplot(dfgrouped, aes(x=dfgrouped$date, y=total )) + 
    #geom_point(size = 5 ) +
    geom_col(aes(fill = treatment),stat = "identity",position = "dodge")+
    xlab("Date of Test") + ylab("Total Activity")+
    #facet_grid(strain~treatment)
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


##
#  plots of tot activity vs rep
##
generateAndPlotActivityByRep <- function(df,titletext, resultspath, saveplot, savetitle)
{
  dfgrouped <- df %>%
    group_by(strain,treatment,id)
  finaltitletext = paste(titletext,"Total activity vs Individual Rep")
  dftemp <<- dfgrouped
  p = ggplot(dfgrouped, aes(x=dfgrouped$id, y=total )) + 
    #geom_point(size = 5 ) +
    geom_col(aes(fill = treatment),stat = "identity",position = "dodge")+
    xlab("Rep Number") + ylab("Total Activity")+
    facet_grid(strain~treatment)+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


# 
# freq dist of total activity
#
generateAndPlotFreqDistOfTotalActivityByStrainTreatment <- function(df,titletext, resultspath, saveplot, savetitle)
{
  dfgrouped <- df %>%
    group_by(strain,treatment)
  finaltitletext = paste(titletext," Frequency Distribution of Total Activity")
p<-ggplot(dfgrouped, aes(x=total))+
  geom_histogram(color="black", fill="white",binwidth=500)+
  facet_grid(strain~treatment)+
  ylab("Mean Frequency Distribution of Total Activity") + 
  xlab("Total Activity")
p
if(saveplot)
{
  finalsavetext = paste(titletext, savetitle, sep="_")
  finalsavetext <- paste(resultspath,finalsavetext,sep="")
  ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
}
}

##
# scatter plots of tot activity vs contact index
##
generateAndPlotScatterActivityByContactIndex <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  finaltitletext = paste(titletext,"Total activity vs Contact Index")
  p = ggplot(df, aes(x=total, y=boris_contact_index,   shape=strain,  color=treatment, ylim(0,100) )) + 
    geom_point(size = 5 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    xlab("Total Activity") + ylab("Net Contact Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



##
# scatter plots of tot activity vs avoidance index
##
generateAndPlotScatterActivityByAvoidanceIndex <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  finaltitletext = paste(titletext,"Total activity vs Avoidance Index")
  p = ggplot(df, aes(x=total, y=boris_avoidance_index,   shape=strain,  color=treatment, ylim(0,100) )) + 
    geom_point(size = 5 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    xlab("Total Activity") + ylab("Net Avoidance Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p+ scale_color_manual(values = palette.trt)
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



##
# scatter plot
##
generateAndPlotScatterActivityByAvoidanceIndexByStrTrt <- function(df, titletext, resultspath, saveplot, savetitle)
{
  finaltitletext = paste(titletext,"Total activity vs Avoidance Index")
  p = ggplot(df, aes(x=total_activity, y=avoidance_index,    color=treatment)) + 
    geom_point(size = 3 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    facet_grid(strain~treatment)+
    xlab("Total Activity") + ylab("Net Avoidance Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    print(finalsavetext)
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}




generateAndPlotScatterTopHalfTotByAvoidanceIndexByStrTrt <- function(df, titletext, resultspath, saveplot, savetitle)
{
  finaltitletext = paste(titletext,"Top Half Total Activity vs Avoidance Index")
  p = ggplot(df, aes(x=tophalfprop, y=avoidance_index,    color=treatment)) + 
    geom_point(size = 3 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    facet_grid(strain~treatment)+
    xlab("Total of Top Half Activity") + ylab("Net Avoidance Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    print(finalsavetext)
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



##
# scatter plot
##
generateAndPlotScatterTopHalfPropByAvoidanceIndexByStrTrt <- function(df, titletext, resultspath, saveplot, savetitle)
{
  finaltitletext = paste(titletext,"Top Half Prop vs Avoidance Index")
  p = ggplot(df, aes(x=tophalfprop, y=avoidance_index,    color=treatment)) + 
    geom_point(size = 3 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    facet_grid(strain~treatment)+
    xlab("Proportion of Top Half Activity") + ylab("Net Avoidance Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    print(finalsavetext)
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}


generateAndPlotScatterBottomHalfPropByAvoidanceIndexByStrTrt <- function(df, titletext, resultspath, saveplot, savetitle)
{
  finaltitletext = paste(titletext,"Bottom Half Prop vs Avoidance Index")
  p = ggplot(df, aes(x=tophalfprop, y=avoidance_index,    color=treatment)) + 
    geom_point(size = 3 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    facet_grid(strain~treatment)+
    xlab("Proportion of Bottom Half Activity") + ylab("Net Avoidance Index")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    print(finalsavetext)
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}



##
# scatter plots of tot inactivity vs mortality
##
generateAndPlotScatterInctivityByMortality <- function(df,titletext, resultspath, saveplot, savetitle, maxy = 50)
{
  finaltitletext = paste(titletext,"Total Inactivity vs Percentage Mortality")
  p = ggplot(df, aes(x=totinact, y=mo_perc,   shape=strain,  color=treatment, ylim(0,100) )) + 
    geom_point(size = 5 ) +
    geom_point() +
    scale_y_continuous()+
    scale_x_continuous()+
    xlab("Total Inactivity") + ylab("Percentage Mortality")+
    ggtitle(finaltitletext)
  print(p + theme(plot.title = element_text(hjust = 0.5))) # centering the title
  p+ scale_color_manual(values = palette.trt)
  
  p
  if(saveplot)
  {
    finalsavetext = paste(titletext, savetitle, sep="_")
    finalsavetext <- paste(resultspath,finalsavetext,sep="")
    ggsave(finalsavetext, width = 30, height = 20, units = "cm", dpi=300)
  }
}
















showUniqueProperties <- function(df)
{
  v = unique(df$country)
  printVector(v)
  v = unique(df$experimenter)
  printVector(v)
  v = unique(df$site)
  printVector(v)
  v = unique(df$treatment)
  printVector(v)
  v = unique(df$strain)
  printVector(v)
  v = unique(df$host_present)
  printVector(v)
}

printVector <- function(v)
{
  for (i in 1:length(v))
  {
    print(v[i])
  }
}





