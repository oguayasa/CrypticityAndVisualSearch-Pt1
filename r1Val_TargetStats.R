# r1Val_TargetStats.R
# 
# Summary: For R1 and Val target data, compare the percent of participants who 
# found most, least, and distractor targets, compare gaze and fixation rates,  
# distance to the center, relationship between distance to center, gaze, and
# fixation rates, percent found, and time to find. 
# 
# Requires: Data files, MASS, ggplot2, readxl, scales, plyr
#
# Outputs: Results of LDA dimensionality reduction in .doc file containing 
# console output, jpeg files of histograms and cluster plots of LDA components 
# by class
# 
# Author: Olivia Guayasamin
# Date: 5/22/2018
# ----- Initializing steps -----

# clean up workspace and console
rm(list=ls())  # workspace
cat("\014")  # console

# load libraries
library(readxl)  # work with xlsx files
library(reshape2)  # format data frames 
library(plyr)  # "apply" functions for working with lists

library(stats)  # stats fuctions
library(MASS)  # more stats
library(Hmisc)  # more stats

library(ggplot2)  # plotting
library(gridExtra)  # plotting figures
library(knitr)  # nice output formatting
library(broom)  # more nice output formatting

# quick function for getting legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# quick function for formatting corr matrix print output
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#set working directory
getwd()
setwd("~/R/r1Val_TargetStats/")

# Import target data files

val.MostData <- read_excel("val_mostTargetInfo.xlsx")
val.LeastData <- read_excel("val_leastTargetInfo.xlsx")
r1.MostData <- read_excel("r1_mostTargetInfo.xlsx")
r1.LeastData <- read_excel("r1_leastTargetInfo.xlsx")
r1.DistData <- read_excel("r1_distractorInfo.xlsx")

# Save all console output to file
# sink("r1Val_TargetStats.doc", append = FALSE)  # initialize output sink

# ----- Import and format data -----

# Format target data files for analysis
# R1 files
r1.Most <- cbind(r1.MostData[, 1], r1.MostData[, c(4,5,7)],r1.MostData[, 16])
r1.Least <- cbind(r1.LeastData[, 1], r1.LeastData[, c(4,5,7)], r1.LeastData[, 16])
r1.Distract <- cbind(r1.DistData[, 1], r1.DistData[, c(4,5,7)], r1.DistData[, 16])
head(r1.Most, 3)  # pick one data frame and check first 3 rows 

# val files
val.Most <- cbind(val.MostData[, 1], val.MostData[, c(5,7,8)], val.MostData[, 16])
val.Least <- cbind(val.LeastData[, 1], val.LeastData[, c(5,7,8)], 
                   val.LeastData[, 16])
head(val.Most, 3)  # pick one data frame and check first 3 rows 

# combine data files into list
r1.Val.List = list(r1Most = r1.Most, r1Least = r1.Least,
                   r1Distract = r1.Distract,  valMost = val.Most,
                   valLeast = val.Least)

# replace 0.0 values in MedDwellTime with NA
for (i in 1:length(r1.Val.List)){
  r1.Val.List[[i]]$MedDwellTime[r1.Val.List[[i]]$MedDwellTime == 0] <- NA
}

# ----- Check data distribution prior to analysis -----

# visualize data using hists and normal qq plots, draw multiple plots per figure
# using par(mfcol), save as jpeg figures

# iterate through data frames in list
for (i in 1:length(r1.Val.List)){
  cur.Data <- data.frame(r1.Val.List[i])  # retrieve each member of list
  # create and save figure
  fig.Title <- sprintf("distCheck.%s.jpg", names(r1.Val.List[i]))
  jpeg(filename <- fig.Title, units = "in", height = 5, width = 9, res = 300)
  par(mfcol = c(2, ncol(cur.Data) - 1))  # format figure window
  # for each variable plot a histogram and qqplot 
  for (j in 2:ncol(cur.Data)){
    thisPlot.Title <- sprintf("%s", names(cur.Data[j]))  # format and add title
    hist(cur.Data[, j], main = thisPlot.Title)  # histogram
    qqnorm(cur.Data[, j]); qqline(cur.Data[, j])  # qqplot with normal line
  }
  dev.off()  # close and save figures
}


# ----- Compare features of most, least, and distractor targets -----

# clearly, the distributions are not normal, so let's choose non-parametric 
# tests. Use wilcoxon signed-rank test to determine if data populations are 
# from identical distributions

# get median values of variables for central tendency comparison
# make empty list for holding medians
r1.val.Medians <- lapply(1:5, 
                         function(x) data.frame(matrix(NA, 
                                                       nrow=4, 
                                                       ncol=2)))
names(r1.val.Medians) = names(r1.Val.List)  # rename objects in new list
# iterate through objects in list (variables)
for (i in 1:length(r1.Val.List)){
  cur.Data <- data.frame(r1.Val.List[i])  # format current list object as df
  # iterate through variabes in df 
  for (j in 1:(ncol(cur.Data) - 1)){
    r1.val.Medians[[i]][j, 1] <- names(cur.Data)[j + 1]  # names of varibles
    r1.val.Medians[[i]][j, 2] <- median(cur.Data[, (j + 1)], na.rm = TRUE)  # get medians
  }
  # output as a formatted table
  cap.Title = sprintf("Measurement Medians for %s", names(r1.Val.List)[i])
  medians.Table <- as.data.frame(r1.val.Medians[[i]][, 2], 
                                 row.names = r1.val.Medians[[i]][, 1])
  print(kable(medians.Table, results = 'asis', caption = cap.Title, 
                        col.names = "MedianValues", digits = 2))
}

# for groups from the r1 study do wilcoxon test
# iterate through all variables and conduct wilcoxon tests across target groups
for (i in 2:length(r1.Val.List$r1Most)){
  # compare most and least
  r1.test1 = wilcox.test(r1.Val.List[[1]][, i], r1.Val.List[[2]][, i], 
                                    paired = TRUE, exact = TRUE, conf.int = TRUE, 
                                    na.action = na.exclude)
  # compare most and distractors
  r1.test2 = wilcox.test(r1.Val.List[[1]][, i], r1.Val.List[[3]][, i], 
                                    paired = TRUE, exact = TRUE, conf.int = TRUE, 
                                    na.action = na.exclude)
  # compare least and distractors
  r1.test3 = wilcox.test(r1.Val.List[[2]][, i], r1.Val.List[[3]][, i], 
                                    paired = TRUE, exact = TRUE, conf.int = TRUE, 
                                    na.action = na.exclude)
  # turn test results into list
  r1.testResults = list(r1.test1, r1.test2, r1.test3)
  
  # Print test output 
  cap.Title = print(sprintf("Obs. Study: Wilcoxon Signed Rank Tests for %s", 
                       names(r1.Val.List[[1]][i])))  # table caption
  # label test output with groups being compare
  testResult.colnames = rbind(sprintf("%sVS%s", names(r1.Val.List[1]), 
                                 names(r1.Val.List[2])),
                         sprintf("%sVS%s", names(r1.Val.List[1]), 
                                 names(r1.Val.List[3])), 
                         sprintf("%sVS%s", names(r1.Val.List[2]),
                                 names(r1.Val.List[3])))
  # extract your values using `sapply`
  test.frame = as.data.frame(sapply(r1.testResults, 
                                    function(x) {c(x$statistic,
                                                   lowCI = x$conf.int[1],
                                                   upCI = x$conf.int[2],
                                                   p = x$p.value)}))
  # turn list items into data table to print
  print(kable(test.frame, results = 'asis', 
                        caption = cap.Title, col.names = testResult.colnames,
                        digits = 2))
}

# for groups from the val study
# iterate through all variables and conduct wilcoxon tests across target groups
for (i in 2:length(r1.Val.List$valMost)){
  # compare most and least
  val.test1 = list(wilcox.test(r1.Val.List[[4]][, i], r1.Val.List[[5]][, i], 
                           paired = TRUE, exact = TRUE, conf.int = TRUE, 
                           na.action = na.exclude))
  # Print test output 
  cap.Title = (sprintf("Val. Experiment: Wilcoxon Signed Rank Tests for %s", 
                       names(r1.Val.List[[4]][i])))  # table caption
  # label test output with groups being compare
  testResult.colnames = c(sprintf("%sVS%s", names(r1.Val.List[4]), 
                                 names(r1.Val.List[5])))
  # extract your values using `sapply`
  test.List = as.data.frame(sapply(val.test1, 
                                   function(x) { c(x$statistic,
                                                   lowCI = x$conf.int[1],
                                                   upCI = x$conf.int[2],
                                                   p = x$p.value)}))
  # turn list items into data table to print
  print(kable(test.List, results = 'asis', 
                          caption = cap.Title, col.names = testResult.colnames,
                          digits = 2))
}


# ----- Create plots to visualize group comparisons -----

# plot names
var.names.4plot = c("Distance from Center", 
                    "Percent of Subjects who Gazed",
                    "Percent of Subjects who Clicked",
                    "Total Search Duration")
var.names.ylab = c("Distance (px)", 
                   "Percent Subjects",
                   "Percent Subjects",
                   "Time (ms)")

# Create scatterplots overlaid with boxplots to visualize comparisons, put plots
# into a list
# iterate through variables
for (i in 1:(length(r1.Val.List) - 1)){
  
  # box plot for r1 data
  # create data frame for plotting
  r1.plot.data = data.frame(MostFound = r1.Val.List[[1]][, i + 1], 
                            LeastFound = r1.Val.List[[2]][, i + 1], 
                            Distractors = r1.Val.List[[3]][, i + 1])
  # format data for plotting with ggplot 2
  r1.plot.data.m = melt(r1.plot.data, value.name = "VarName",
                        variable.name = "ExperimentalGroups")
  # create plot object
  r1.plot = ggplot(r1.plot.data.m, aes(x = ExperimentalGroups, 
                                                  y = VarName)) +
    geom_jitter(aes(colour = ExperimentalGroups), width = 0.2,  # jitter plot
                alpha = 0.6) +
    geom_boxplot(aes(fill = ExperimentalGroups), width = 0.6, alpha = 0.8, 
                 notch = TRUE, outlier.shape = NA) +  # box plot
    labs(title = var.names.4plot[i],  # change labels and titles
         subtitle = "Observational Study",  
         y = var.names.ylab[i]) +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
           plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    # change colors manually
    scale_fill_manual(values = c("#253494", "#2c7fb8", "#a1dab4"), 
                      name = "Experimental\n    Groups", 
                      breaks = c("MostFound", 
                                 "LeastFound", 
                                 "Distractors"), 
                      labels = c("Most Found", 
                                 "Least Found", 
                                 "Distractors")) +  # boxplot
    scale_colour_manual(values = c("#253494", "#2c7fb8", "#a1dab4")) +  # jitter
    guides(colour = FALSE)  # remove legend for jitter points
   
      
  # create boxplots for val data
  # data frames for plotting
  val.plot.data = data.frame(MostFound = r1.Val.List[[4]][, i + 1], 
                             LeastFound = r1.Val.List[[5]][, i + 1]) 
  # format data for plotting with ggplot 2
  val.plot.data.m = melt(val.plot.data, value.name = "VarName",
                      variable.name = "ExperimentalGroups")
  # create plot object
  val.plot = ggplot(val.plot.data.m, aes(x = ExperimentalGroups,
                                                   y = VarName)) +
    geom_jitter(aes(colour = ExperimentalGroups), width = 0.2,  # jitter plot
                alpha = 0.6) +
    geom_boxplot(aes(fill = ExperimentalGroups), width = 0.6, alpha = 0.8, 
                 notch = TRUE, outlier.shape = NA) +  # box plot
    labs(title = var.names.4plot[i],  # change labels and titles
         subtitle = "Validation Experiment",  
         y = var.names.ylab[i]) +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    # change colors manually
    scale_colour_manual(values = c("#253494", "#2c7fb8")) +  # jitter
    scale_fill_manual(values = c("#253494", "#2c7fb8"))  # boxplot
  
  # get legend for figure
  r1.val.legend = get_legend(r1.plot)
  
  # remove remaining legends
  val.plot.new <- val.plot + theme(legend.position = "none")
  r1.plot.new <- r1.plot + theme(legend.position = "none")
  
  # put r1 and val plots into a list
  plotting.list <- list(r1.val.legend, r1.plot.new, val.plot.new)
  
  # use grid arrange to print plots and save as image
  fig.Title <- sprintf("compareR1Val.%d.jpg", i) # title
  jpeg(filename <- fig.Title, units = "in", height = 4, width = 9, res = 300)
  grid.arrange(grobs = plotting.list, ncol = 3, widths = c(2, 4, 4))  
  dev.off()  # close and save figures
}
dev.off()  # make sure all plots save

# ----- Correlate target features with stimuli properties and performance -----

# create new list
r1.val.corr.names <- c(names(r1.Val.List[c(1, 2, 4, 5)]))  # get list names
r1.val.corr.data = lapply(cbind(1, 2, 4, 5), 
                          function(x) r1.Val.List[[x]][, c(2,4,5)])
 
names(r1.val.corr.data) = r1.val.corr.names  # add list names


test.frame = as.data.frame(sapply(r1.testResults, 
                                  function(x) {c(x$statistic,
                                                 lowCI = x$conf.int[1],
                                                 upCI = x$conf.int[2],
                                                 p = x$p.value)}))

# calculate correlations for data from each experimental group
for (i in 1:length(r1.val.corr.data)){
  # run spearmans correlation
  corr.res = rcorr(as.matrix(r1.val.corr.data[[i]]), type = "spearman")  
  # print with nice format
  sprintf("Spearman's correlation results for %s", names(r1.val.corr.data[i]))
  kable(flattenCorrMatrix(corr.res$r, corr.res$P)) 
}

# ----- Create scatterplots to visualize correlations -----

# create and print plots for data from each experimental group
axes.names = c("Distance to Center (px)", "Percent Identified",  # var names
               "Search Duration (ms)") 
titles.names = c("Distance to Center", "Percent Identified",  # title names
                 "Search Duration") 
counter = 0 # initialize counter

# iterate through data and print plots. this is a bit repetitive because some
# of the plots with be values vs. themselves, but it gets the job done
for (i in 1:2){
  for (j in 1:3){
    counter = counter + 1
    
    # format r1 data for plotting
    r1.corr.plot.most = data.frame(var1 = r1.val.corr.data[[1]][, i],
                                   var2 = r1.val.corr.data[[1]][, j], 
                                   ExperimentalGroup = rep("MostFound", 
                                                           length(r1.val.corr.data[[1]][, j])))
    r1.corr.plot.least = data.frame(var1 = r1.val.corr.data[[2]][, i],
                                    var2 = r1.val.corr.data[[2]][, j], 
                                    ExperimentalGroup = rep("LeastFound", 
                                                            length(r1.val.corr.data[[2]][, j])))
    r1.corr.plot.all = rbind(r1.corr.plot.most, r1.corr.plot.least)
    colnames(r1.corr.plot.all) = c("VarName1", "VarName2", 
                                   "ExperimentalGroups")
    
    # format val data for plotting
    val.corr.plot.most = data.frame(var1 = r1.val.corr.data[[3]][, i],
                                    var2 = r1.val.corr.data[[3]][, j], 
                                    ExperimentalGroup = rep("MostFound", 
                                                            length(r1.val.corr.data[[3]][, j])))
    val.corr.plot.least = data.frame(var1 = r1.val.corr.data[[4]][, i],
                                     var2 = r1.val.corr.data[[4]][, j], 
                                     ExperimentalGroup = rep("LeastFound", 
                                                             length(r1.val.corr.data[[4]][, j])))
    val.corr.plot.all = rbind(val.corr.plot.most, val.corr.plot.least)
    colnames(val.corr.plot.all) = c("VarName1", "VarName2", 
                                    "ExperimentalGroups")
    
    # create r 1 plot object
    r1.plot = ggplot(r1.corr.plot.all, aes(x = VarName1, y = VarName2, 
                                           col = ExperimentalGroups)) +
      geom_point(shape = 1, size = 2, na.rm = TRUE, show.legend = TRUE) +
      geom_smooth(method = lm, se = TRUE) +
      labs(title = sprintf("Correlation between %s \n and %s",
                           titles.names[i], titles.names[j]), 
           subtitle = "Observational Study", x = axes.names[i], 
           y = axes.names[j]) +  # change text
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            plot.subtitle = element_text(hjust = 0.5, size = 10)) +
      # change colors manually and format legend
      scale_colour_manual(values = c("#253494", "#2c7fb8"), 
                          name = "Experimental\n    Groups", 
                          breaks = c("MostFound", 
                                     "LeastFound"),
                          labels = c("Most Found", 
                                     "Least Found"))
    
    # val correlation plot objects
    val.plot = ggplot(val.corr.plot.all, aes(x = VarName1, y = VarName2, 
                                             col = ExperimentalGroups)) +
      geom_point(shape = 1, size = 2, na.rm = TRUE, show.legend = TRUE) +
      geom_smooth(method = lm, se = TRUE) +
      labs(title = sprintf("Correlation between %s \n and %s",
                           titles.names[i], titles.names[j]), 
           subtitle = "Validation Experiment", x = axes.names[i], 
           y = axes.names[j]) +  # change text
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            plot.subtitle = element_text(hjust = 0.5, size = 10)) +
      # change colors manually and format legend
      scale_colour_manual(values = c("#253494", "#2c7fb8"), 
                          name = "Experimental\n    Groups", 
                          breaks = c("MostFound", 
                                     "LeastFound"),
                          labels = c("Most Found", 
                                     "Least Found")) 
    # get legend for figure
    corr.plot.legend = get_legend(r1.plot)
    
    # remove remaining legends
    val.plot.new <- val.plot + theme(legend.position = "none")
    r1.plot.new <- r1.plot + theme(legend.position = "none")
    
    # put r1 and val plots into a list
    plotting.list <- list(corr.plot.legend, r1.plot.new, val.plot.new)
    
    # use grid arrange to print plots and save as image
    fig.Title <- sprintf("CorrsR1Val.%d.jpg", counter) # title
    jpeg(filename <- fig.Title, units = "in", height = 4, width = 10, 
         res = 300)
    grid.arrange(grobs = plotting.list, ncol = 3, widths = c(2, 5, 5))  
    dev.off()  # close and save figures
    
  }
}

dev.off()

# ----- Clean Up -----
# 
# End Sink
# sink()
# dev.off()  # make sure sink saves

# Detach libraries
detach(package:MASS)
detach(package:readxl)
detach(package:gridExtra)
detach(package:plyr)
detach(package:reshape2)
detach(package:knitr)
detach(package:broom)
detach(package:Hmisc)
detach(package:ggplot2)
detach(package:stats)


