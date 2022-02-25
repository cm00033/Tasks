# Script written by: Alita Burmeister
# Data analysis for: "Selection for increased adsorption to both ancestral and coevolved hosts drives a key innovation in a virus," By Alita R. Burmeister, Richard E. Lenski, and Justin R. Meyer
# Final version for publication v. Jan. 28, 2016
# Purpose: This script plots the phage lambda growth rates (paper Figure 2A) and selection rates (paper Figure 2B), including confidence intervals, and performs statistical tests asssociated with these rates. C

############################################
########## Load packages and data ##########
############################################

# Load required R package for plotting
require(ggplot2)

# Loads csv file with competition data formatted for plotting
comp_data_8 = read.csv("/Burmeister_2016_Dryad_Data_Scripts/Burmeister_2016_Figure2_Data_B.csv") 

##### Description of comp_data_8 data ##### 
# Date: Date of assay
# Time: Duration of assay in h
# Host: Host strain
# Final: Indicates if the replicate was included in final data analysis.
# Competitor.1: Evolved phage strain 
# Flask: Replicate
# Mb (per h): Malthusian parameter of the ancestor Lac-Z marked phage ("b" indicates blue plaques)
# Mc (per h): Malthusian parameter of the evolved phage ("c" indicates clear, non-makred plaques)
# Mbar: Average Malthusian parameter
# r(b-c): Selection rate relative to ancestor
# r(c-b): Selection rate relative to evolved
# label: Tag to indicate whether the "growth" column shows the growth rate of the evolved ("t") or ancestor ("n") growth rate. This is used by ggplot to generate the growth rate plots, Figure 2A.
# sel_rate: Final selection rate to be plotted
# growth: Final growth rate to be plotted

# Brings in the data set for marker neutrality and statistical comparison tests
comp_data = read.csv("/Burmeister_2016_Dryad_Data_Scripts/Burmeister_2016_Figure2_Data_A.csv")

##### Description of comp_data data ##### 
# Date: Date of assay	
# Time: Duration of assay	
# Host: Host Strain	
# Final: Included in final data analysis?	
# Competitor 1: Evolved phage strain
# Flask: Replicate	
# T0 Dilution Blue: Plating dilution at initial time point on which blue plaques were counted
# T0 Blue: Count of blue plaques at initial time point
# T0 Blue PFU/ml: Density of ancestor phages at initial time point
# T0 Clear Diltuion: Plating dilution at initial time point on which clear plaques were counted
# T0 Clear	: Count of clear plaques at initial time point
# T0 Clear PFU/ml: Density of evolved phages at initial time point
# T Final Dilution Blue: 	Plating dilution at final time point on which blue plaques were counted
# Tf Blue: Count of blue plaques at final time point
# Tf PFU/ml: Final density of ancestor phage
# Blue	Tf Dilution Clear: Plating dilution at final time point on which clear plaques were counted
# Tf Clear: Count of clear plaques at final time point
# Tf PFU/ml Clear	: Final density of evolved phage
# Mb (per h): Malthusian parameter of the ancestor Lac-Z marked phage ("b" indicates blue plaques)
# Mc (per h): Malthusian parameter of the evolved phage ("c" indicates clear, non-makred plaques)
# Mbar: Average Malthusian parameter
# Wapprox	: Relative fitness, W, approximated from selection rate; not valid in some cases; see main text.
# W=Mb/Mc: Relative fitness of ancestor relative to evolved
# W=Mc/Mb: Relative fitness of evolved relative to ancestor
# r(b-c): Selection rate relative to ancestor
# r(c-b): Selection rate relative to evolved	


# Function to find confidence intervals and means
ConfidenceIntervalMean <- function(data, alpha=0.05){
	data <- na.omit(data) # removing missing data!!!
	mean_data <- mean(data)
	std_err <- sd(data)/sqrt(length(data))
	lower_CI <- mean(data) + std_err*(qt(p=alpha/2,df=length(data)-1))
	upper_CI <- mean(data) + std_err*(qt(p=(1- alpha/2),df=length(data)-1))
	return(c(mean=mean_data, StdErr=std_err, UpperCI=upper_CI, Lower.CI=lower_CI))
	}
	

# Defines function to find relative fitness neutrality confidence intervals. Pass in subsetted neutrality data and column name, e.g., selection rate or growth rate.
CIMDates <- function(dataset, col){
	# Find 
	dates <- c()
	DateLevels <- levels(dataset$Date)
	for(d in 1:length(DateLevels)){
		host_date <- subset(dataset, Date == DateLevels[d])
		host_date <- mean(host_date[,col], na.rm = T)
		dates = c(dates, host_date)
	}
	rnF <- ConfidenceIntervalMean(dates)
	return(rnF)
}
  
###########################################
############ LacZ Marker Cost ############# 
###########################################

# This section computes the cost of the lacZ competition marker. The cost varies by host and is indicated by the black dashed lines on Figure 1.

# Subsets the neutrality data to include only the 8 hr fitness data. 
neut_data = comp_data
neut_data = subset(neut_data, Time == '8hr')

# Subset the marker cost data by host and whether the test was an evolved competition ("c") or neutrallity test ("n")
neut_data_8_anc_n <- subset(neut_data, Host == '606' & Final != 'yes') # Neutrality data, host REL606
neut_data_8_res_n <- subset(neut_data, Host == 'ECC4'& Final != 'yes') #  Neutrality data, host EcC4

## Selection rate marker cost confidence intervals
REL606_rn <- CIMDates(neut_data_8_anc_n, 'r..c.b.')
ECC4_rn <- CIMDates(neut_data_8_res_n, 'r..c.b.')

# Prints the marker cost data for reporting in the Methods section
REL606_rn # 0.120 +/- 0.019 per hour (95% CI)
ECC4_rn   # 0.039 +/- 0.017 per hour (95% CI)

## Malthusian growth rate marker cost confidence intervals ("absolute fitness")
REL606_Mn <- CIMDates(neut_data_8_anc_n, 'Mc..per.h.')
ECC4_Mn <- CIMDates(neut_data_8_res_n, 'Mc..per.h.')

# Prints the marker cost data for absolute fitness
REL606_Mn
ECC4_Mn


#####################################################
############# Figure 2B: Selection rates ############
#####################################################

# Figure 2B: Relative fitness of each phage genotype on two hosts. The horizontal black dashed lines indicate the cost of the lacZ marker on each host.

# Prepare the competition data. The data needs to be in dataframe format for plotting with ggplot.
df1 <- with(data = subset(comp_data_8, label == "t"), aggregate(sel_rate, list(Genotype = Competitor.1, Host = Host), mean))


# Original colorblind palette:
# black, orange, bright blue, green, yellow, dark blude, redish, pink
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Edited: 
# orange, bright blue, green, dark blude, redish, pink
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


# Add a column with the 95% confidence intervals for each genotype

df1$CI <- with(data = subset(comp_data_8, label == "t"), aggregate(sel_rate, list(Genotype = Competitor.1, Host = Host),  function(x) (sd(x)/sqrt(length(x)))*(qt(.975,df=(length(x)-1))))[,3])

# Generates Figure 2b.
figure2b <- ggplot(df1, aes(x = Host, y = x, group = Genotype, colour = Genotype, shape = Genotype), x)  #+
figure2b + 
     scale_y_continuous(limits = c(-.1, 1.5)) +  
     theme(axis.text = element_text(colour = "black")) +
   theme(axis.ticks = element_line(colour = 'black'), legend.key = element_rect(fill = "white")) +
     labs(y = expression(paste("Selection Rate (h"^"-1",")"))) + # Custom label
     scale_colour_manual(values=cbbPalette) +
     geom_segment(aes(x = .8, xend = 1.2, y = REL606_rn[3], yend = REL606_rn[3]), colour = "black", linetype = 2) + # Neutrality CI
     geom_segment(aes(x = .8, xend = 1.2, y = REL606_rn[4], yend = REL606_rn[4]), colour = "black", linetype = 2) + # Neutrality CI
     geom_segment(aes(x = 1.8, xend = 2.2, y = ECC4_rn[3], yend = ECC4_rn[3]), colour = "black", linetype = 2) + # Neutrality CI
     geom_segment(aes(x = 1.8, xend = 2.2, y = ECC4_rn[4], yend = ECC4_rn[4]), colour = "black", linetype = 2) +# Neutrality CI
     geom_point(aes(colour = Genotype)) +  #Color points by phage genotype
     geom_line(aes(color = Genotype) ) +  # Color lines by phage genotype
     geom_errorbar(aes(ymax=x+CI, ymin=x-CI), width = .1) +  # Adds 95% CIs
     scale_x_discrete(breaks=c("606", "ECC4"), labels=c("Ancestor","Coevolved")) + # Custom x-axis labels
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
      
#####################################################
############# Figures 2A: Growth rates ##############
#####################################################

# Plots growth rate of each phage genotype (solid lines) and the marked ancestor (dashed lines) grown alongside each genotype. 

# Set up the growth rate data for plotting with ggplot
df2 <- with(data = comp_data_8, aggregate(growth, list(Genotype = Competitor.1, Host = Host, label = label), mean))
# df2 <- subset(df2, label == "t")
df2$CI <- with(data = comp_data_8, aggregate(growth, list(Genotype = Competitor.1, Host = Host, label = label),  function(x) (sd(x)/sqrt(length(x)))*(qt(.975, df = (length(x)-1))))[,4])

df2 <- df2[order(df2$label),]
df2

# Generates Figure 2A 
figure2a <- ggplot(df2, aes(x = Host, y = x, group = interaction(label, Genotype), colour = Genotype), x) 

figure2a + 
      geom_point(aes(colour = Genotype, shape = label)) + 
      facet_wrap( ~ Genotype, ncol = 3, scales = "free") +
    #  titles <- c("A12","A7", "B2","D9","E4","G9") +
    geom_hline(aes(yintercept=0), colour = "black", linetype = 2) +
   theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0), strip.background = element_rect(colour="white", fill="white")) +
      geom_line(aes(color = Genotype, linetype = label)) +
      geom_errorbar(aes(ymax=x+CI, ymin=x-CI), width = .1) + 
      scale_x_discrete(breaks=c("606", "ECC4"), 
      labels=c("Ancestor","Coevolved")) + 
      scale_y_continuous(limits = c(-.8, 2.0)) + 
      scale_colour_manual(values=cbbPalette, name="Phage\nGenotype", breaks=c("A12.10-6","A7-10.2", "B2-13.1","D9-8.2","E4-13.2","G9-11.3"), labels=c("A12","A7", "B2","D9","E4","G9")) +
     theme(axis.text = element_text(colour = "black")) +
   theme(axis.ticks = element_line(colour = 'black')) +
      labs(y = expression(paste("Growth Rate (h"^"-1",")"))) +
      scale_shape_manual(values=c(1, 16), breaks = c("t","n"), labels = c("Evolved", "Ancestor")) +    
      scale_linetype_manual(values = c("dotted", "solid"), guide = FALSE) +
      theme(legend.title=element_blank()) + # Custom x-axis labels
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"))


#################################################
############## Between-host t-tests ############# 
#################################################


# Subset data to include only test assays, excluding neutrality tests
test_data = subset(comp_data, Final == 'yes')
# List of genotypes for the t-tests
genotypes <- c("A12-10.6", "A7-10.2", "B2-13.1", "D9-8.2", "E4-13.2", "G9-11.3")

# Selection rate tests: Compares selection rate of each genotype BETWEEN THE TWO HOSTS. No marker correction.
for (g in (genotypes)){
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$r..c.b., y = subset(test_data, Competitor.1 == g & Host == "ECC4")$r..c.b., paired = FALSE, var.equal = TRUE, alternative = "less"))
print(g)
 }
 
# Growth rate tests: Compares growth rate of each genotype BETWEEN THE TWO HOSTS. No marker correction.
for (g in (genotypes)){
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$Mc..per.h., y = subset(test_data, Competitor.1 == g & Host == "ECC4")$Mc..per.h., paired = FALSE, var.equal = TRUE, alternative = "greater"))
print(g)
 }
 
# Selection rate tests: Compares selection rate of each genotype BETWEEN THE TWO HOSTS. WITH marker correction.
comp_data$r_corrected119 = (comp_data$r..c.b.) - 0.12
comp_data$r_corrected041 = (comp_data$r..c.b.) - 0.039
test_data = subset(comp_data, Final == 'yes')
for (g in (genotypes)){
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$r_corrected119, y = subset(test_data, Competitor.1 == g & Host == "ECC4")$r_corrected041, paired = FALSE, var.equal = TRUE, alternative = "less"))
print(g)
 }
 

###################################################
########## Ancestor vs. Evolved T-tests ########### 
###################################################

############### Selection Rates ###################

# Selection Rates on REL606: Tests for increase in fitness (selection rate) of each evolved genotype compared to the ancestral fitness (= 0 by definition) on the ancestor host, uncorrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$r..c.b., alternative = "greater"))
 }


# Selection Rates on ECC4: Tests for increase in fitness (selection rate) of each evolved genotype compared to the ancestral fitness (= 0 by definition) on the coevolved host, uncorrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "ECC4")$r..c.b., alternative = "greater"))
 }


# Corrected Selection Rates on REL606: Tests for increase in fitness (selection rate) of each evolved genotype compared to the ancestral fitness (= 0 by definition) on the ancestor host, corrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$r..c.b. - 0.1202, alternative = "greater"))
}
 

# Corrected Selection Rates on ECC4: Tests for increase in fitness (selection rate) of each evolved genotype compared to the ancestral fitness (= 0 by definition) on the coevolved host, corrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "ECC4")$r..c.b. - 0.0387, alternative = "greater"))
 }
 
 
############### Growth Rates ###################

# Growth Rates on REL606: Tests for increase in absolute fitness of each evolved genotype compared to the ancestral phage on the ancestor host, uncorrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$Mc..per.h., y = subset(test_data, Competitor.1 == g & Host == "606")$Mb..per.h., paired = FALSE, var.equal = TRUE, alternative = "greater"))
 }
  

# Growth Rates on ECC4: Tests for increase in absolute fitness of each evolved genotype compared to the ancestral phage on the coevolved host, uncorrected for marker effect. 
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "ECC4")$Mc..per.h., y = subset(test_data, Competitor.1 == g & Host == "ECC4")$Mb..per.h., paired = FALSE, var.equal = TRUE, alternative = "greater"))
 }


# Corrected Growth Rates on REL606: Tests for increase in absolute fitness of each evolved genotype compared to the ancestral phage on the ancestor host, corrected for marker effect. 
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "606")$Mc..per.h., y = (subset(test_data, Competitor.1 == g & Host == "606")$Mb..per.h. + .1202), paired = FALSE, var.equal = TRUE, alternative = "greater"))
 }


# Corrected Growth Rates on ECC4: Tests for increase in absolute fitness of each evolved genotype compared to the ancestral phage on the coevolved host, corrected for marker effect.
for (g in (genotypes)){
print(g)
print(t.test(x = subset(test_data, Competitor.1 == g & Host == "ECC4")$Mc..per.h., y = (subset(test_data, Competitor.1 == g & Host == "ECC4")$Mb..per.h. + .0387), paired = FALSE, var.equal = TRUE, alternative = "greater"))
 }

 