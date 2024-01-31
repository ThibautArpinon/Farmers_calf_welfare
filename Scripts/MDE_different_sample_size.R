# Ex-post power analysis and simulations for different sample sizes

# Clear environment
rm(list=ls())


#--------------------------------------------------------#
##### Packages #####
#--------------------------------------------------------#
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library("miceadds")
library("ddpcr")
library("pastecs")
library(qwraps2)
options(qwraps2_markup = 'latex')
library(clusrank)
library(Hmisc)
library(psy)
library(sandwich)
library("texreg")
library(stargazer)
library(tidyverse)
library(mgcv)
library(splines)
library(segmented)
library(haven)

#--------------------------------------------------------#
##### Functions #####
#--------------------------------------------------------#
# Function to make graph start at 0 
prettyZero <- function(l){
  max.decimals = max(nchar(str_extract(l, "\\.[0-9]+")), na.rm = T)-1
  lnew = formatC(l, replace.zero = T, zero.print = "0",
                 digits = max.decimals, format = "f", preserve.width=T)
  return(lnew)
}

# Prevent scientific notation for clarity, set to 0 for scientific notation
options(scipen=999)

#--------------------------------------------------------#
##### Working directory #####
#--------------------------------------------------------#
# Setting a working directory
setwd("/Users/ThibautArpinon_1/Desktop/Codes_data/")

#--------------------------------------------------------#
##### Simulations #####
#--------------------------------------------------------#

##### Ex-post power #####

# First, I run an ex-post power analysis to assess the power in the experiment. 

# Import data from the experiment 
mydf <- read_dta("Data/modified_data.dta")

# Keep only columns of interest
df <- dplyr::select(mydf, ID, chose_see_image, treatment_info)

# Logit model to find the same results as in Stata
logit <- glm(chose_see_image ~ treatment_info, data = df, family = "binomial")
summary(logit)
exp(coef(logit))


# Simulating data based on the observed data from experiment 

# Table to see data distribution 
table(df$chose_see_image, df$treatment_info)

# Set the simulation parameters
alpha = 0.05
J=1000 #number of simulations
eff_size = 0.7875752-0.9787234  
# observed effect size in the experiment calculated as difference between proba to see image in control and proba to see image in treatment 
vectorResults=rep(NA,J) #Vector to store the results of the simulations
set.seed(2023) #Set seed for replication

# Post-hoc power analysis 
for(j in 1:J){
  y = rbinom(47, size=1, prob=0.9787234) #Simulate data in the control
  y = c(y, rbinom(499, size=1, prob=0.9787234+eff_size)) #Simulate data with treatment effect size
  t = c(rep(0,47), rep(1,499)) # Distribute the treatment with sample sizes 
  df_sim = data.frame(cbind(t,y)) # bind together as dataframe 
  
  pval = summary(glm(y ~ t, binomial, df_sim))$coef[2, 4] #Logit model and store p-value

  # Compare p-value to the 5% alpha level 
  vectorResults[j] = ifelse(pval<alpha,1,0)
  }

#Statistical power:
post_hoc_power = mean(vectorResults)
post_hoc_power
# 0.628







# Clear environment
rm(list=setdiff(ls(), c("post_hoc_power")))



##### Simulation for increasing percentage of control group and decreasing treatment group #####
# In the following simulation, n is fixed at 499

# Now reiterate the post-hoc power analysis but simulating for different control sample sizes (i.e., greater number of observations)
# In the original experiment, the control sample size is ~10% if the treatment sample size (47/499 = 0.094)
# In the following, I calculate the power for control sample sizes of 20%, 40%, 60%, 80%, and 100% of the treatment group


# Set the simulation parameters
n = 546 # Number of observations in the treatment group 
ctl <- c(seq(0.1, 0.5, by = 0.05)) # Defining size of control as percentage of treat group  
trt <- c(1-ctl)
results <- expand.grid(n = n, ctl = ctl) # Create matrix to store results
results$trt <- trt
results$power <- NA # Create column to store each power


# Set the simulation parameters for power analysis
alpha = 0.05
J=1000 #number of simulations
eff_size = 0.7875752-0.9787234  
# observed effect size in the experiment calculated as difference between proba to see image in control and proba to see image in treatment 
vectorResults=rep(NA,J) #Vector to store the results of the simulations
set.seed(2023) #Set seed for replication


# Post-hoc power analysis with different control sample size
gen_dat <- function(n, ctl, trt) {
  data.frame(
    y = c(rbinom(n*ctl, size=1, prob=0.9787234), rbinom(n*trt, size=1, prob=0.9787234 + eff_size)),
    t = c(rep(0, n*ctl), rep(1, n*trt)) # Distribute the treatment with sample sizes 
  )
}

get_pvalue <- function(data) summary(glm(y ~ t, binomial, data))$coef[2, 4] 


for (i in seq_len(nrow(results))) {
  ps <- sapply(1:J, function(zzz) {
    get_pvalue(
      gen_dat(results$n[i], results$ctl[i], results$trt[i])
    )
  })
  results$power[i] <- mean(ps < alpha)
  print(i)
}

# Show the results
results
# n corresponds to the treatment sample size
# ctl corresponds to percentage of the total sample that is control group 
# trt corresponds to percentage of the total sample that is treatment group  
# Power corresponds to power for each control-treatment sample size combination


# Add a row with the ex-post power using from the data 
results[nrow(results) + 1,] <- list(499, 0.094, 0.906, post_hoc_power)

g1 <- subset(results, ctl == 0.094)


# Graph to show the differences 

sim_ctrl_incr_treat_decr <- results %>% 
  ggplot(aes(x = ctl, y = power)) +
  geom_line( color="#564D80", size = 1) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=5) +
  geom_point(data=g1, shape=21, fill="coral", color="black", size=5) +
  geom_hline(aes(yintercept = .8), linetype = 2, color = "grey10") +
  geom_hline(aes(yintercept = 1), linetype = 2, color = "grey10") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 0, vjust = +3),
        text = element_text(size = 18),
        legend.position = "none",
        plot.background = element_rect(fill = 'white', color = 'white')) +
  labs(x = expression(atop(bold("Control group size"),atop(italic("(in % of N)")))),
       y = expression(atop(bold("Power"), atop(~(1-beta))))) +
  scale_x_continuous(breaks=seq(0, 0.5, 0.05),
                     label = function(x) paste0(x * 100, "%")) +
  scale_y_continuous(label = function(x) paste0(x * 100, "%")) +
  coord_cartesian(ylim = c(.5, 1)) 

sim_ctrl_incr_treat_decr

ggsave("Graphs/sim_ctrl_incr_treat_decr.png", width = 12, height = 6, sim_ctrl_incr_treat_decr)


# END
