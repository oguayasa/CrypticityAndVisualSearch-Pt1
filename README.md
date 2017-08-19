Validating the Difficulty of Visual Search Stimuli Sets
=======================================================

Olivia Guayasamin
7/7/2017

Introduction
--------

This code demonstrates the initial analysis of two visual search studies, an Observation Study to discover and sort potential target stimuli, and a Validation Experiment conducted on selected stimuli from the Observation Study. These two studies were designed to 1) identify which individual target stimuli were found by the greatest (and least) number of subjects during a visual search task, 2) use these Most and Least found stimuli to create Easy and Hard versions of a visual search task, and 3) validate the difficulty of the Easy and Hard stimuli sets in a true experiment.

During the Observation Study, each trial included several targets of unknown, mixed difficulty. The Most and Least found targets were identified post-hoc, and then used to make separate sets of Easy and Hard stimuli for use in the Validation Experiment. The current analysis is designed to answer questions such as, *Are certain targets and/or groups of targets found significantly more (or less) often than others?*, or *Is this target findability affected by central bias?*, and *What is the relationship between a target's findability and the amount of time required to find it?*, to name a few.

The data files are named according whether they came from the Observation study or the Validation Experiment, and whether they contain data from the Most or Least found (Easy or Hard) target stimuli groups from their respective studies. During the Observational Study, data was also collected on "distractor" non-targets to serve as a proper control group. Files are organized by individual targets, not subjects, where each row represents the complete data collected for a single target including: distance from screen center, percent found (percent of participants who found the target), percent gaze (percent of participants who gazed on the target), and median dwell time. Within each study, we compared data from the separate groups of targets using [Wilcoxon Signed Rank Tests](https://www.r-bloggers.com/wilcoxon-signed-rank-test/), and checked relationships between measures using correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/). Data summaries, comparisions, and relationships are visualized with tables, boxplots, and scatterplots respectively.

This code also provides examples for working with lists [plyr](https://cran.r-project.org/web/packages/plyr/plyr.pdf) and different formatting libraries that are available in R. It implements basic data visualization and figure plotting using the [ggplot2](https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf), [gridExtra](https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf), and [knitr](https://cran.r-project.org/web/packages/knitr/knitr.pdf) packages, and basic analysis using the [MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf), [stats](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html), and [Hmisc](https://cran.r-project.org/web/packages/Hmisc/Hmisc.pdf) packages.

**Summary**: Check data distributions, compare group central tendencies using [Wilcoxon Signed Rank Tests](https://www.r-bloggers.com/wilcoxon-signed-rank-test/), and compare relationships between variables using correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/).
**Requires**: Mentioned R libraries, data files.
**Outputs**: Results of basic comparative and correlational analysis as data tables, analysis plots as jpg figures.

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

Before we begin with statistical analyses, it is important to first visualize the data. By doing so, we can get a sense of the data's shape, or distribution, which will help us determine which statistical tests are most appropriate for our data. If we were to skip this step and choose an innapropirate test, we would end up with false or unreliable results which could mislead future research. 

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

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/distCheck.valLeast.jpg)


Clearly, some of these distributions are not normal. So, instead of using standard parametric t-tests to formally compare central tendencies, we are going to use non-parametric t-tests. Because targets from the Easy and Hard groups were viewed and acted on by the same group of participants, the data for the two sets of targets can be described as dependent. Therefore, we will apply Wilcoxon Signed-Rank tests (a dependent, non-parametric t-test) to determine if target features across the Easy and Hard sets come from significantly different distributions.

Formally compare variable distributions across target groups
============================================================

The particular function that I will be using for conducting the Wilcoxon Signed-Rank tests does not return the median values of the groups being compared, but these values are necessary to interpret the results of the Wilcoxon Signed Rank Tests. Not to mention, if you are interested in publishing your results, including measures of central tendency is standard when reporting formal test results.

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

However, there were big differences between groups in terms of average proportion of participants who clicked on targets, and the average time spent searching for those targets. Targets from the Most Found group were the most likely to be found, (obviously), but they also took less time to find, reinforcing our interpretation that they are in fact, "easier". Distractor controls were almost never misidentified as targets (which means that our subjects were focused and made few mistakes, something that's reassuring to know). Results from Validation Experiment (Tables 4 & 5) are quite similar.

Iterate through variables and conduct Wilcoxon Tests across groups
------------------------------------------------------------------

But median values are just one way of summarizing data. Because we want to compare the sample *distributions*, we need to apply our statistical tests, which take both central tendency and elements of distribution shape into account. 

To avoid repetition, we'll use the code for examining data from the Validation Experiment, which demonstrates how to quickly run and format resutls from the Wilcoxon Signed-Rank test.  However, we'll still include the results for both studies below.

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

A close reading of the code will show that "statistic", "lowCI", "upCI", and "p", are formatted into the output. When reporting test results it is best practice to include the test statistic itself (in this case, V), lower and upper confidence intervals, and p-values. All of these together, not just the p-values, make interpreting the meaning and strength of the test results much easier. When these results are drafted into a manuscript, they will be presented with the median values. 

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

The average proportion of participants to click on targets was significantly different across groups, as was total search time (Tables 8 & 9), supporting our previous interpretation of the median values. 

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

This is likely due to the design of Observational Study, where each trial included targets of unknown, mixed difficulty (the Most Found and Least Found targets were identified post-hoc and then used to make separate sets of entirely easy and difficulty stimuli for use in the Validation Experiment). Having targets of mixed difficulty would make the Most Found targets the easiest to find in any one trial (and vice versa for the Least Found targets). In comparison, during the Validation Experiment, stimuli contained only Most Found targets or Least Found targets (as identified in the Observation Study), so no one target could be that much easier or harder relative to the others. 

Visualize group comparisons with boxplots
=========================================

Numbers can be boring and text takes time to read. Pictures have pretty colors, so lets make figures that include all of our results described above. 

More seriously, many formally published reports can either be very subject-specific, long, or too-technical to for most people to easily read and acquire information from. Images are a way of conveying lots of information and statistical results in a faster and more accessible way than text. However, just like how some writing can be confusing and hard to read, so can images. Take your time making them. Ask people not familiar with your project if they understand the results that the images are tryng to convey. 

Here, we will use boxplots to convey our information. I prefer them to bar graphs because in addition to showing average values and some measures of spread, they give a visualization of the distributions. In addition, many packages for plotting boxplots come with the option to include "Notches", the triangles around the average. These basically represent 95% confindence intervals around the average, so if the notches of two boxplots do not overlap there is a good chance that a statistical test will determine that the two distributions are meaningfully different.

**Figure 2** Comparing arget likelihood of being found across groups. 

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/compareR1Val.3.jpg)



**Figure 3** Comparing total time spent searching for a target across groups. 

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/compareR1Val.4.jpg)



**Figure 4** Comparing target likelihood of being gazed at across groups. 

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/compareR1Val.2.jpg)



**Figure 5** Comparing target distance from center across groups. 

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/compareR1Val.1.jpg)

These all validate what we've seen before, and that's the point. Instead of needing to have a dozen or so tables to show median values, test results, and comparisons, we can condense all of our work into four easily readible plots. Nice. 

Correlate target features with stimuli properties and performance
=================================================================

In visual search studies it is important to check for possible biases that may be affecting your results. A big one to look for is called the central bias, where stimuli towards the center of a screen tend to be more readily located. The median results, Wilcoxon reuslts, and boxplot of target distance from center (Fig 6) show that there are not significant differences in target distance across groups. But, this doesn't disprove a center bias, it just says that the target positions across groups were equivalent. To really check to see if there was a central bias, we need to look at the relationships between: 1) target position and the likelihood the target was found, and 2) target position and search time spent on the target.

In addition to determining if a central bias is present in our data, as mentioned before the above resutls suggest that there is a realtionship between a target's likehood of being found and the time it took to find it. Let determine if any of these relationships actually exist in our data. To do this, we are going to use a non-parametric correlation analysis with Spearman's *ρ*.

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
  sprintf("Spearman's correlation results for %s", 
                      names(r1.Val.corData[i]))
  print(kable(flattenCorrMatrix(corr.Res$r, corr.Res$P), results = 'asis', 
        digits = 2))
}
```
Observational Study Most Found Targets

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |  -0.01|  0.89|
| DistFromCenter | MedDwellTime |   0.09|  0.25|
| PerClick       | MedDwellTime |  -0.40|  0.00|

Observational Study Least Found Targets

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |  -0.06|  0.41|
| DistFromCenter | MedDwellTime |  -0.08|  0.33|
| PerClick       | MedDwellTime |  -0.35|  0.00|

Validation Experiment Easy Targets

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |   0.06|  0.43|
| DistFromCenter | MedDwellTime |  -0.03|  0.71|
| PerClick       | MedDwellTime |  -0.32|  0.00|

Validation Experiment Hard Targets

| Var1           | Var2         |    cor|     p|
|:---------------|:-------------|------:|-----:|
| DistFromCenter | PerClick     |   0.17|  0.03|
| DistFromCenter | MedDwellTime |  -0.12|  0.12|
| PerClick       | MedDwellTime |  -0.04|  0.63|

Fortunately, there seems to be no evidence of a central bias (Tables 14-16), with the exception of the Least Found group during the Validation Experiment (Table 17). However, this relationship is in the positive direction, indicating that the farther away a target was from the center, the more likely it was to be found, the opposite of a central bias. 

For every group except the Least Found targets in the Validation Study (Table 17), there is large and significant negative relationship between likelihood of being found and search time (Tables 14-16). Not just across difficulty groups, but also within each group, targets that were more likely to be found to less time to find, suggesting that subjects required less information to make a decision about whether a target was present or not. 

But let's plot these relationships and see what it looks like for ourselves.

Create scatterplots to visualize correlations
=============================================

To save space and more meaningfully convey information, I am going to include plots from both the Observational Study and the Validation Experiment in the same figure. Additionally, I am going to include data from both the Most and Least Found target groups on each plot to allow for visual comparison.

Checking for an Effect of Center Bias
-------------------------------------

**Figure 7**

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/CorrsR1Val.2.jpg)

Remember how the correlation analysis said that for the Least Found targets in the Validation Experiment there was a positive relationship between distance from center and likelihood of being found? Sure, it's technically significant, but in reality the relationship doesn't look like much of anything exept a line drawn through a cloud. Well, this is a great example of why you should visualize your data in addition to analysis.

**Figure 8**

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/CorrsR1Val.3.jpg)

Almost flat lines, no relationships here.

Realtionship Between Search Time and Identificiation Likelihood
-------------------------------------------------------------

**Figure 9**

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt1/blob/master/imgs/CorrsR1Val.6.jpg)

For the Least Found in the Validation Experiment, ot looks like there could be something there, but it could just be the effect of those outliers on the left hand side of the plot. Those outlier may be exerting a lot of influence for just a few data points, dragging that side of the line upwards. If you got rid of those, the line would probaby lie flat. This is also a great example of why should look at both the data and do statisical tests, but for the opposite reason as before. Here, the non-parametric test was less affected by the magnitude of outliers, and correctly reported a non-significant relationship.

Conclusion
==========

Overall, we learned a few important things from this data set. First of all, we were able to demonstrate that there were no significant biases affecting target findability. Most importantly, we were able to validate that the Most and Least Found targets identified in the Observation Study held up during the Validation Experiment. From here on, the Most and Least found groups from the Validation Experiment will now be considered Easy and Hard treatments that can be used to examine the effects of target crypticity on search behaviors. Next, we will look at how target crypticity affected subject's search performance and efficiency.  
