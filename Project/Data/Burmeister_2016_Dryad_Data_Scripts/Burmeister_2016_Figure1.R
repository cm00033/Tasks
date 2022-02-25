# Script written by Alita Burmeister
# Data analysis for manuscript titled: "Selection for increased adsorption to both ancestral and coevolved hosts drives a key innovation in a virus," by Alita R. Burmeister, Richard E. Lenski, and Justin R. Meyer


# Purpose: 1) Calculate confidence intervals for selection rate of coevolved hosts with and without phage present; 2) Plot adsorption rate data (Paper Figure 1); and 3) Perform t-tests for phage adsorption rates.


############################################
########## Load packages and data ##########
############################################

# Load required R package for plotting
require(ggplot2)

# Load data file
ads_data = read.csv("/Burmeister_2016_Dryad_Data_Scripts/Burmeister_2016_Figure1_Data.csv") 

# Data file history:
#### Data originally collected to Excel sheet with mulitple tabs named "Adsorption_assays_Aug122015.xlsx." This file contains preliminary adsorption rate data (Aug 2015) and the final adsorption rate data (Sept-Oct 2015) presented in the paper and analyzed here.
#### The final data were saved to a csv with a single tab, named "Adsorption_assays_Aug122015_FinalDataCSV.csv," imported and used in this script.

# Prepare the data for R
ads_data$rate_corrected_neg_cell = as.numeric(as.character(ads_data$rate_corrected_neg_cell))
ads_data <- subset(ads_data, rate_corrected_neg_cell != 'NA')


##############################################################
################## Description of ads_data data set #################
##############################################################

# date: Date a replicate was collected
# rep: Replicate of each phage-host combination
# treatment: Host type that phages were adsorbed to. anc = ancestor cells; evo = evolved cells
# genotype: Phage genotype; 1A = ancestor phage
# stat_label: Label used for pairing evolved and ancestor phage for t-tests
# dil_t0: Plating dilution at the initial timepoint
# count_t0: Phage count on the dil_t0 plate
# titer_t0: Phage density at initial timepoint
# dil_t2: Plating dilution at the final timepoint
# count_t2: Phage count on the dil_t2 plate
# titer_t2: Phage density at final timepoint
# rate: Total phage loss from initial to final timepoint
# rate_per_h: Phage loss loss per hour
# rate_corrected: Phage adsorption rate. This is equal to the rate_per_h minus the background decay rate in the absense of host
# rate_corrected_neg: The final phage adsorption rate expressed as a positive value and used throughout this script.


#######################################################
############# Figure 1: Adsorption rates ##############
#######################################################

# Prepare the adsorption data. The data needs to be in dataframe format for plotting with ggplot.
df1 <- with(data = subset(ads_data), aggregate(rate_corrected_neg_cell, list(Genotype = genotype, host = treatment), mean))
df1$Genotype <- factor(df1$Genotype, as.character(df1$Genotype))

# Add a column with the 95% confidence intervals for each genotype
df1$CI <- with(data = subset(ads_data), aggregate(rate_corrected_neg_cell, list(Genotype = genotype, host = treatment),  function(x) (sd(x)/sqrt(length(x)))*(qt(p=.05/2,df=length(x)-1)))[,3])

library(scales)   
# Plots the data
   ggplot(subset(df1), aes(x = Genotype, y = x)) +
   scale_y_continuous(limits = c(-.5e-8, 5e-8)) +  
    theme(axis.text = element_text(colour = "black")) +
   theme(axis.ticks = element_line(colour = 'black'), legend.key = element_rect(fill = "white")) +
   labs(y = expression(paste("Adsorption Rate (ml h"^"-1",")")), x = expression("Phage")) + # Custom label
  geom_errorbar(aes(ymax=x+CI, ymin=x-CI), width = .2) +     
  geom_point(aes(shape = host, color = host), size = 3) +
  scale_color_manual(values = c("black","black"), name = "Host") +
  scale_shape_manual(values = c(19, 21), name = "Host") + 
  geom_segment(aes(x = .5, xend = 7.5, y = 0.0, yend = 0.0), colour = "gray", linetype = 3) +
     theme(axis.title.x = element_text(vjust=-0.7), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
     panel.background = element_blank(), axis.line = element_line(colour = "black"))

      
#############################################################
######### Statistical comparison of adsorption rates ######## 
############################################################# 


# Generate a new data frame in which the mean each of the evolved phage-host combinations (N=4) is listed. The data frame also contains the mean of the paired ancestor phage adsorption rates (N = 4 for each paired comparison).
df2 <- with(data = subset(ads_data), aggregate(rate_corrected_neg, list(Genotype = stat_label, host = treatment), mean))
df2$Genotype <- factor(df1$Genotype, as.character(df1$Genotype))

# Add column to designate phage category
phage_cat <- c(rep("evo", 6), rep("anc", 6))
df2$phage_cat <- phage_cat


###### Evolved vs. Ancestor Phage on ANCESTOR HOST ########
var.test(subset(df2, host == "anc" & phage_cat == "evo")$x, subset(df2, host == 'anc' & phage_cat == "anc")$x, ratio = 1, alternative = 't', conf.level = 0.95)
# Results: F = 53.06, num df = 5, denom df = 5, p-value = 0.0004957
# Result: unequal variance

t.test(subset(df2, host == "anc" & phage_cat == "evo")$x, subset(df2, host == 'anc' & phage_cat == "anc")$x, paired = TRUE, var.equal = FALSE, alternative = 'greater')
# Result: t = 18.893, df = 5, p-value = 3.826e-06


###### Evolved vs. Ancestor Phage on EVOLVED HOST #######
var.test(subset(df2, host == "evo" & phage_cat == "evo")$x, subset(df2, host == 'evo' & phage_cat == "anc")$x, ratio = 1, alternative = 't', conf.level = 0.95)
# Result: F = 1.8894, num df = 5, denom df = 5, p-value = 0.5019

t.test(subset(df2, host == "evo" & phage_cat == "evo")$x, subset(df2, host == 'evo' & phage_cat == "anc")$x, paired = TRUE, var.equal = TRUE, alternative = 'greater')
# Result: t = 1.8369, df = 5, p-value = 0.06282


###### Evolved Phage on ANCESTOR vs EVOLVED HOST ########
var.test(subset(df2, host == "anc" & phage_cat == "evo")$x, subset(df2, host == 'evo' & phage_cat == "evo")$x, ratio = 1, alternative = 't', conf.level = 0.95)
# Result: F = 6.8899, num df = 5, denom df = 5, p-value = 0.05391

t.test(subset(df2, host == "anc" & phage_cat == "evo")$x, subset(df2, host == 'evo' & phage_cat == "evo")$x, paired = TRUE, var.equal = TRUE, alternative = 'greater')
# Result: t = 21.895, df = 5, p-value = 1.844e-06

