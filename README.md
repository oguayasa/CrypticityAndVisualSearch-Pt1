# VisualSearchDifficultyStudy1
The data, code, and analysis for the creation and validation of easy and difficult sets of visual search stimuli. 

Summary
=======

This code demonstrates the initial analysis of two visual search studies (an observational study to discover and sort potential target stimuli and a validation experiment conducted on stimuli selected from the observatio study) that were designed to 1) identify which individual target stimuli were found by the greatest number of subjects and which were found by the least number of subjects during a visual search task, 2) use these most and least found stimuli to create easy and hard versions of a visual search task, and 3) validate the difficulty of these two stimuli setsin a true experiment.

During the observational Study, each trial included targets of unknown, mixed difficulty. The Most Found and Least Found targets were identified post-hoc and then used to make separate sets of easy (Most Found) and difficult (Least Found) stimuli for use in the Validation Experiment.

The data files are named according whether they came from the observational study or the validation experiment, and whether they contain data from the most or least found target groups used in those studies. During the observational study, data was also collected on "distractor" non-targets to serve as a proper control group. Files are organized by individual target stimuli, not subjects, where each row represents a different target. For each target, we gathered the follwing data: distance from center, percent found (percent of participants who found the target), percent gaze (percent of participants who gazed on the target), and median dwell time. For both studies, compared data from the separate groups of targets using wilcoxon signed rank tests, and checked relationships between variables using spearmans correlation.

The current analysis is designed to answer questions such as, *Are certain* *targets and/or groups of targets significantly more or less likely to be found * *than others?*, or *Is target findability affected by central bias?*, and *What is* *the relationship between time spent searching and findability?*, to name a few.

This code also provides examples for working with lists [plyr](https://cran.r-project.org/web/packages/plyr/plyr.pdf) and different formatting libraries that are available in R. It implements basic data visualization and figure plotting using the [ggplot2](https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf), [gridExtra](https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf), and [knitr](https://cran.r-project.org/web/packages/knitr/knitr.pdf) packages, and basic analysis using the MASS [MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf), [stats](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html), and [Hmisc](https://cran.r-project.org/web/packages/Hmisc/Hmisc.pdf) packages.

Summary: Check data distributions, compare group central tendencies using [Wilcoxon Signed Rank Tests](https://www.r-bloggers.com/wilcoxon-signed-rank-test/),
and compare relationships between variables using correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/). Output results as histogram, boxplot, and scatterplot figures.

Requires: Mentioned R libraries, data files.

Outputs: Results of basic comparative and correlational analysis as data tables, analysis plots as jpg figures.

Initializing Steps
==================

Load libraries from CRAN
------------------------

``` r
# work with data
library(readxl)  
library(reshape2)  
library(plyr) 
# stats
library(stats)
library(MASS)
# plotting and formatting 
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(knitr)
```

Define custom functions
-----------------------

``` r
# get legend from a ggplot2 object
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# format output from a correlation matrix 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    Var1 = rownames(cormat)[row(cormat)[ut]],
    Var2 = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
```

Import data files
-----------------

``` r
val.MostData <- read_excel("val_mostTargetInfo.xlsx")
val.LeastData <- read_excel("val_leastTargetInfo.xlsx")
r1.MostData <- read_excel("r1_mostTargetInfo.xlsx")
r1.LeastData <- read_excel("r1_leastTargetInfo.xlsx")
r1.DistData <- read_excel("r1_distractorInfo.xlsx")
```

Select and format relevant data
-------------------------------

### Convert independent data frames into a single list of data frames

``` r
# obs. study files
r1.Most <- cbind(r1.MostData[, 1], r1.MostData[, c(4,5,7)],r1.MostData[, 16])
r1.Least <- cbind(r1.LeastData[, 1], r1.LeastData[, c(4,5,7)], r1.LeastData[, 16])
r1.Distract <- cbind(r1.DistData[, 1], r1.DistData[, c(4,5,7)], r1.DistData[, 16])
# val files
val.Most <- cbind(val.MostData[, 1], val.MostData[, c(5,7,8)], val.MostData[, 16])
val.Least <- cbind(val.LeastData[, 1], val.LeastData[, c(5,7,8)], 
                   val.LeastData[, 16])
# combine data files into list
r1.Val.List = list(r1Most = r1.Most, r1Least = r1.Least, 
                   r1Distract = r1.Distract,  valMost = val.Most,
                   valLeast = val.Least)
```

Check data distributions prior to analysis
==========================================

Visualize data using histograms and normal quantile-quantile (qq) plots
-----------------------------------------------------------------------

``` r
# iterate through data frames in list
for (i in 1:length(r1.Val.List)){
  cur.Data <- data.frame(r1.Val.List[i])  # retrieve each member of list
  # create and save figure
  fig.Title <- sprintf("distCheck.%s.jpg", names(r1.Val.List[i]))
  jpeg(filename <- fig.Title, units = "in", height = 5, width = 10, res = 300)
  par(mfcol=c(2,4))  # format figure window
  # for each variable plot a histogram and qqplot 
  for (j in 2:ncol(cur.Data)){
    thisPlot.Title <- sprintf("%s", names(cur.Data[j]))  # format and add title
    hist(cur.Data[, j], main = thisPlot.Title)  # histogram
    qqnorm(cur.Data[, j]); qqline(cur.Data[, j])  # qqplot with normal line
  }
  dev.off()  # close and save figures
}
```

**Figure 1** Histograms and qq-plots describing variable distributions. This example contains data from the Least Found targets in the Validation Experiment.

Clearly, some of these distributions are not normal. So to formally compare central tendencies we are going to use non-parametric t-tests. Because with each study participants looked at data from targets from several groups, the data can be considered dependent. We will apply Wilcoxon Signed rank tests to determine if data are from identical or significantly different distributions.

Formally compare variable distributions across target groups
============================================================

The particular function that I will be using for conducting the Wilcoxon Signed Rank tests does not return the median values of the groups being compared. However, median values will help interpret the results of the Wilcoxon Signed Rank Tests. Not to mention, if you are interested in publishing your results, including measures of central tendency is standard when reporting formal test results.

Calculate median values to help interpret comparisons
-----------------------------------------------------

``` r
# make empty list for holding medians
r1.Val.Medians <- lapply(1:5, 
                         function(x) data.frame(matrix(NA, 
                                                       nrow=4, 
                                                       ncol=2)))
names(r1.Val.Medians) = names(r1.Val.List)  # rename objects in new list
# iterate through objects in list (variables)
for (i in 1:length(r1.Val.List)){
  cur.Data <- data.frame(r1.Val.List[i])  # format current list object as df
  # iterate through variabes in df 
  for (j in 1:(ncol(cur.Data)-1)){
    r1.Val.Medians[[i]][j, 1] <- names(cur.Data)[j + 1]  # names of varibles
    r1.Val.Medians[[i]][j, 2] <- median(cur.Data[, (j + 1)], na.rm = TRUE)  # get medians
  }
  # output as a formatted table
  cap.Title = sprintf("Measurement Medians for %s", names(r1.Val.List)[i])
  medians.Table <- as.data.frame(r1.Val.Medians[[i]][, 2], 
                                 row.names = r1.Val.Medians[[i]][, 1])
  print(kable(medians.Table, results = 'asis', caption = cap.Title, 
        col.names = "MedianValues", digits = 2))
}
```

|                       |  MedianValues|
|-----------------------|-------------:|
| r1Most.DistFromCenter |        451.88|
| r1Most.PerGaze        |        100.00|
| r1Most.PerClick       |         92.31|
| r1Most.MedDwellTime   |       1273.38|

|                        |  MedianValues|
|------------------------|-------------:|
| r1Least.DistFromCenter |        451.71|
| r1Least.PerGaze        |        100.00|
| r1Least.PerClick       |         23.08|
| r1Least.MedDwellTime   |       1828.65|

|                           |  MedianValues|
|---------------------------|-------------:|
| r1Distract.DistFromCenter |        482.98|
| r1Distract.PerGaze        |         98.08|
| r1Distract.PerClick       |          0.00|
| r1Distract.MedDwellTime   |        910.15|

|                        |  MedianValues|
|------------------------|-------------:|
| valMost.DistFromCenter |        464.26|
| valMost.PerGaze        |        100.00|
| valMost.PerClick       |         63.16|
| valMost.MedDwellTime   |       1436.20|

|                         |  MedianValues|
|-------------------------|-------------:|
| valLeast.DistFromCenter |        457.55|
| valLeast.PerGaze        |        100.00|
| valLeast.PerClick       |         27.05|
| valLeast.MedDwellTime   |       1820.30|

From briefly glancing at these tables, it appears that during the Observational Study (Tables 1-3) there were no meaningful differences in either average target position relative to the screen center or in target likelihood to be gazed at in the first place. The fact the likelihood ot be gazed at is similar across groups is actually quite important. It means that the Most Found targets did not have any more "pop out" features the Least Found targets or distractors, indicating that a similar type of visual search was conducted on all target groups.

However, there were big differences between groups in terms of average proportion of participants who clicked on targets, and the average time spent searching for those targets. Targets from the Most Found group were the most likely to be found, (obviously), but they also took less time to find than targets from the Least Found group. Distractor controls were almost never misidentified as targets, and participants seem to have spent little time searching over time. Results from Validation Experiment (Tables 4 & 5) are quite similar.

Iterate through variables and conduct Wilcoxon Tests across groups
------------------------------------------------------------------

But these are just median values, which don't say anything about what we actually need to compare, the sample distributions.

To avoid repetition, we'll just use the code for the Validation Wxperiment as an example of how to conduct and quickly format results from the Wilcoxon Signed Rank test. However, we'll still include the results for both studies.

### Code example

``` r
# iterate through all variables and conduct wilcoxon tests across target groups
for (i in 2:length(r1.Val.List$valMost)){
  # compare most and least
  val.Test1 = list(wilcox.test(r1.Val.List[[4]][, i], r1.Val.List[[5]][, i], 
                           paired = TRUE, exact = TRUE, conf.int = TRUE, 
                           na.action = na.exclude))
  # Print test output 
  cap.Title = (sprintf("Val. Experiment: Wilcoxon Signed Rank Tests for %s", 
                       names(r1.Val.List[[4]][i])))  # table caption
  # label test output with groups being compare
  testResult.colNames = c(sprintf("%sVS%s", names(r1.Val.List[4]), 
                                 names(r1.Val.List[5])))
  # extract your values using `sapply`
  test.List = as.data.frame(sapply(val.Test1, 
                                   function(x) { c(x$statistic,
                                                   lowCI = x$conf.int[1],
                                                   upCI = x$conf.int[2],
                                                   p = x$p.value)}))
  # turn list items into data table to print
  print(kable(test.List, results = 'asis', caption = cap.Title, 
              col.names = testResult.colNames, digits = 2))
}
```

A close reading of the code will show that "statistic", "lowCI", "upCI", and "p", are formatted into the output. When reporting test results, it is best practice to include the test statistic itself (V), lower and upper confidence intervals, and finally the p-value. All of these together, not just the p-value, make interpreting the meaning and strength of the test results much easier.

### Observational Study results

|       |  r1MostVSr1Least|  r1MostVSr1Distract|  r1LeastVSr1Distract|
|-------|----------------:|-------------------:|--------------------:|
| V     |          5334.50|             7357.00|              7613.00|
| lowCI |           -67.44|              -46.57|               -34.46|
| upCI  |            49.40|               54.27|                60.17|
| p     |             0.84|                0.89|                 0.59|

|       |  r1MostVSr1Least|  r1MostVSr1Distract|  r1LeastVSr1Distract|
|-------|----------------:|-------------------:|--------------------:|
| V     |           295.00|             3833.50|              2954.00|
| lowCI |             7.42|                1.92|                 0.00|
| upCI  |             7.69|                2.88|                 1.92|
| p     |             0.00|                0.00|                 0.06|

|       |  r1MostVSr1Least|  r1MostVSr1Distract|  r1LeastVSr1Distract|
|-------|----------------:|-------------------:|--------------------:|
| V     |         14535.00|            14535.00|             11934.00|
| lowCI |            62.32|               88.46|                23.08|
| upCI  |            68.91|               92.31|                28.57|
| p     |             0.00|                0.00|                 0.00|

|       |  r1MostVSr1Least|  r1MostVSr1Distract|  r1LeastVSr1Distract|
|-------|----------------:|-------------------:|--------------------:|
| V     |          1195.00|            13628.00|             11361.50|
| lowCI |          -588.68|              321.45|               772.35|
| upCI  |          -404.97|              413.33|               951.90|
| p     |             0.00|                0.00|                 0.00|

Just as indicated by the median values (Tables 1-3), the difference in target distance from the center across groups is not significant (Table 6). Surprisingly, even though the likelihood that a target was gazed at was almost identical across groups, the difference in distributions is significant (Table 7). Yet, there are cases where is it necessary to use best judgement when interpreting test results. While the difference is mathematically significant, the median values indicates that it is probably not functionally significant.

The average proportion of participants to click on targets was significantly different across groups, as was total search time (Tables 8 & 9), strongly suggesting that the Most Found targets are so named because they are easier to find.

### Validation Experiment Results

|       |  valMostVSvalLeast|
|-------|------------------:|
| V     |            4947.00|
| lowCI |              -5.59|
| upCI  |              -0.14|
| p     |               0.04|

|       |  valMostVSvalLeast|
|-------|------------------:|
| V     |            7286.00|
| lowCI |               0.00|
| upCI  |               0.00|
| p     |               0.98|

|       |  valMostVSvalLeast|
|-------|------------------:|
| V     |           14248.50|
| lowCI |              31.87|
| upCI  |              38.16|
| p     |               0.00|

|       |  valMostVSvalLeast|
|-------|------------------:|
| V     |            1565.00|
| lowCI |            -559.45|
| upCI  |            -367.40|
| p     |               0.00|

We see a similar pattern of results in from Validation Experiment, except that the likelihood that a target was gazed at did not differ between difficulty conditions (Table 11).

Consistent with the Observational study, for targets in the Most Found treatment the average proportion of participants to click on targets was significantly greater than in the Least Found, the average search time was far less than for targets in the Least Found Treatment (Tables 12 & 13). However, looking at the median values (Tables 4 & 5), the magnitude of these differences is much smaller in the Validation Experiment than during the Observational Study.

This is likely due to the design of Observational Study, where each trial included targets of unknown, mixed difficulty (the Most Found and Least Found targets were identified post-hoc and then used to make separate sets of entirely easy and difficulty stimuli for use in the Validation Experiment). Having targets of mixed difficulty would make the Most Found targets the easiest to find in any one stimuli. In comparison, during the Validation Experiment, stimuli contained only Most Found targets or Least Found targets (as identified in the Observation Study), so no one target could be realtively easier than any others.

Visualize group comparisons with boxplots
=========================================

Numbers can be boring and text takes time to read. Pictures have colors, and can convey more information more quickly, so lets make figures that include all of our results described above.

Target likelihood of being found
--------------------------------

**Figure 2**

Total Search Time
-----------------

**Figure 4**

Target likelihood of being gazed at
-----------------------------------

**Figure 5**

Distance from Center
--------------------

**Figure 6**

Here, we used boxplots to convey our information. I prefer them to bar graphs because in addition to showing average values and some measures of spread, they give a visualization of the distributions. In addition, many packages for plotting boxplots come with the option to include "Notches", the triangles around the average. These basically represent 95% confindence intervals around the average, so if the notches of two boxplots do not overlap there is a good chance that a statistical test will determine that the two distributions are meaningfully different.

Correlate target features with stimuli properties and performance
=================================================================

In visual search studies it is important to check for possible biases that may be affecting your results. A big one to look for is called the central bias, where stimuli towards the center of a screen tend to be more readily located. The median results, Wilcoxon reuslts, and boxplot of target distance from center (Fig 6) show that there are not significant differences in target distance across groups. But, this doesn't disprove a center bias, it just says that the target positions across groups were equivalent. To really check to see if there was a central bias, we need to look at the relationship between target positions and the likelihood that it was found, and position and total search time.

In addition, the above results suggest that there is a realtionship between likehood of being found and search time. So, let's determine if either central bias or this possible relationship exist in our data. To do this, we are going to use a non-parametric correlation analysis with Spearman's *ρ*.

Spearman's correlation analysis
-------------------------------

``` r
# create new list
r1.Val.corNames <- c(names(r1.Val.List[c(1, 2, 4, 5)]))  # get list names
r1.Val.corData = lapply(c(1, 2, 4, 5),  # create abbreviate list
                          function(x) (r1.Val.List[[x]][, c(2, 4, 5)]))
names(r1.Val.corData) = r1.Val.corNames  # add list names

# calculate correlations for data from each experimental group
for (i in 1:length(r1.Val.corData)){
  # run spearmans correlation
  corr.Res = rcorr(as.matrix(r1.Val.corData[[i]]), type = "spearman")
  # print with nice format
  cap.Title = sprintf("Spearman's correlation results for %s", 
                      names(r1.Val.corData[i]))
  print(kable(flattenCorrMatrix(corr.Res$r, corr.Res$P), results = 'asis', 
        caption = cap.Title, digits = 2))
}
```

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |  -0.01|  0.89|
| DistFromCenter | MedDwellTime |   0.09|  0.25|
| PerClick       | MedDwellTime |  -0.40|  0.00|

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |  -0.06|  0.41|
| DistFromCenter | MedDwellTime |  -0.08|  0.33|
| PerClick       | MedDwellTime |  -0.35|  0.00|

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |   0.06|  0.43|
| DistFromCenter | MedDwellTime |  -0.03|  0.71|
| PerClick       | MedDwellTime |  -0.32|  0.00|

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |   0.17|  0.03|
| DistFromCenter | MedDwellTime |  -0.12|  0.12|
| PerClick       | MedDwellTime |  -0.04|  0.63|

For every group except the Least Found targets in the Validation Study (Table 17), there is large and significant negative relationship between likelihood of being found and search time (Tables 14-16).

Fortunately, there seems to be no evidence of a central bias (Tables 14-16), with the exception of the Least Found group during the Validation Experiment (Table 17). However, this relationship is in the positive direction, indicating that the farther away a target was from the center, the more likely it was to be found, the opposite of a central bias. But let's plot these relationships and see what it looks like for ourselves.

Create scatterplots to visualize correlations
=============================================

To save space and more meaningfully convey information, I am going to include plots from both the Observational Study and the Validation Experiment in the same figure. Additionally, I am going to include data from both the Most and Least Found target groups on each plot to allow for easier visual comparison.

Checking for an Effect of Center Bias
-------------------------------------

**Figure 7**

Remember how the correlation analysis said that for the Least Found targets in the Validation Experiment there was a positive relationship between distance from center and likelihood of being found? Sure, it's technically significant, but in reality the relationship doesn't look like much of anything exept a line drawn through a cloud. Well, this is a great example of why you should visualize your data in addition to analysis.

**Figure 8**

Almost flat lines, no relationships here.

Realtionship Between Search Time and Identificiation Likelihood
-------------------------------------------------------------

**Figure 9**

For the Least Found in the Validation Experiment, ot looks like there could be something there, but it could just be the effect of those outliers on the left hand side of the plot. Those outlier may be exerting a lot of influence for just a few data points, dragging that side of the line upwards. If you got rid of those, the line would probaby lie flat. This is also a great example of why should look at both the data and do statisical tests, but for the opposite reason as before. Here, the non-parametric test was less affected by the magnitude of outliers, and correctly reported a non-significant relationship.

Conclusion
==========

Overall, we learned a few important things from this data set. First of all, we were able to validate that the Most and Least Found targets identified in the Observational Study held up during the Validation Experiment. From here on, the Most and Least found groups from the Validation Experiment will now be considered Easy and Hard treatments. Secondly, we were able to demonstrate that there were no significant biases affecting target difficulty.

From these two studies, we were able to create easy and difficult versions of a visual search task that can be used in later experiments when we are interested in manipulating search difficulty.
