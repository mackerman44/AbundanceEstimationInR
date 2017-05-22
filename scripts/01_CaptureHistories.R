# Load necessary libraries
library(FSA)
library(dplyr)
library(Rcapture)

# Set the working directory
setwd('C:/Users/mikea/Dropbox/R/WDAFS Workshop/Abundance Estimation/')

#############################
### 01. CAPTURE HISTORIES ###
#############################

# Capture Histories (Individual Format)
chIndividual <- data.frame(fish   = c(1,2,3,4,5),
                           event1 = c(1,0,0,1,0),
                           event2 = c(0,1,1,0,0),
                           event3 = c(1,0,0,1,0),
                           event4 = c(0,1,1,0,0),
                           event5 = c(0,0,0,0,1))
chIndividual

# Capture Histories (Frequency Format)
chFrequency <- data.frame(freq   = c(2,2,1),
                          event1 = c(1,0,0),
                          event2 = c(0,1,0),
                          event3 = c(1,0,0),
                          event4 = c(0,1,0),
                          event5 = c(0,0,1))
chFrequency

# Read in data from Zehfuss et al. (1999). Data are from Gulf Sturgeon captured from below the 
# Jim Woodruff Dam on the Apalachicola River (Florida) over 13 sampling events during summer 1985.
# Data is in MARK format
gs.M <- read.table('data/01_Zehfussetal_1985.txt', header = TRUE, colClasses = c('character', 'character'))
gs.M
headtail(gs.M) # headtail() is from Derek's FSA package

# Convert Capture Histories from MARK format to Individual Format
?capHistConvert
gs.I <- capHistConvert(gs.M, in.type = 'MARK', freq = 'freq', out.type = 'individual', include.id = TRUE, 
                       var.lbls.pre = 'e')

# Summarize Capture Histories 
?capHistSum
gs.ch <- capHistSum(gs.I, cols2use = 2:14)
gs.ch <- capHistSum(gs.I, cols2use = -1)

# Frequency of fish with each unique capture history
gs.ch$caphist

#################################################################
### 02. CLOSED POPULATION, SINGLE RECAPTURE (SIMPLE PETERSON) ###
#################################################################

# SIMPLE PETERSON ESTIMATOR (CLOSED POPULATION, SINGLE RECAPTURE)
sl1 <- mrClosed()

# Run Chapman modification of the simple Peterson estimator
sl1 <- mrClosed(M = 2555, n = 274, m = 92, method = 'Chapman')

# The abundance estimate w/ standard error
summary(sl1,incl.SE=TRUE)

# Extract the confidence interval
confint(sl1, verbose = TRUE)

gs.ch56 <- capHistSum(gs.I, cols2use = 5:6)
gs.mr56 <- mrClosed(gs.ch56, method = 'Chapman')
gs.56.summary <- cbind(summary(gs.mr56, incl.SE = TRUE), confint(gs.mr56))
gs.56.summary  
  