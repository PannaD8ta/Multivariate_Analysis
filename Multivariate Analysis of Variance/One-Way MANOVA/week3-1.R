#load required libraries
library(dplyr)
library(ggplot2)
library(car)

#set working directory to where you have saved your school.RData file
load("school.RData")

##ANOVA test hypotheses:

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.


#Visualize your data and compute one-way ANOVA

# Show the levels in SES - categorical IV
table(school$SES)
levels(school$SES)
#check SES for missin values
table(is.na(school$SES))

#summary of MATHSACH - metric DV (maths achievement)
summary(school$MATHSACH)

#clean the data - remove NA

#same students have missing SES and MATHSACH: 113, 131
school[is.na(school$MATHSACH),]

#remove rows 113, 131
school_anova<-school[-which(is.na(school$MATHSACH)==TRUE),]

#check IV
table(is.na(school_anova$SES))

#Compute summary statistics by groups - count, mean, sd:
group_by(school_anova, SES) %>%
  summarise(
    count = n(),
    mean = mean(MATHSACH, na.rm = TRUE),
    sd = sd(MATHSACH, na.rm = TRUE)
  )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggplot(school_anova, aes(x=SES, y= MATHSACH, group=SES)) + 
  geom_boxplot()

school_anova$SES<-as.factor(school_anova$SES)
# Compute the analysis of variance
res.aov <- aov(MATHSACH ~ SES, data = school_anova)
# Summary of the analysis
summary(res.aov)

##As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the groups highlighted with “*" in the model summary.

##Tukey multiple pairwise-comparisons
#As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) for performing multiple pairwise-comparison between the means of groups.

#The function TukeyHD() takes the fitted ANOVA as an argument.
TukeyHSD(res.aov)
##It can be seen from the output, that only the difference between high and low is significant with an adjusted p-value of 0.001.
#Check ANOVA assumptions: test validity?

# 1. Homogeneity of variances
plot(res.aov, 1)
#points 39, 143, and 158 are detected as outliers, which can severely affect normality and homogeneity of variance. It can be useful to remove outliers to meet the test assumptions.


#use Bartlett's test or Levene's test to check the homogeneity of variances.

leveneTest(MATHSACH ~ SES, data = school_anova)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
qqnorm(school_anova$MATHSACH)
qqline(school_anova$MATHSACH)
# 2. Normality
plot(res.aov, 2)
#As all the points deviate from the reference line, we can not assume normality.
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
##The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals (W = 0.98, p = 0.004) 
#p-value is less than 0.05, then the null hypothesis that the data are normally distributed is rejected

##Non-parametric alternative to one-way ANOVA test
kruskal.test(MATHSACH ~ SES, data = school_anova)


##@@@@@@@@@@@@@@
#Compute MANOVA test
# MANOVA test
cor.test(school_anova$MATHSACH, school_anova$ENGACH)
res.man <- manova(cbind(MATHSACH, ENGACH) ~ SES, data = school_anova)
summary(res.man)

#From the output above, it can be seen that MATHSACH and ENGACH are highly significantly different among SES.

summary(res.man, intercept = TRUE, test = "Wilks") 

summary.aov(res.man, intercept = TRUE)

