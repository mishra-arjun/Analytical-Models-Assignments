############################### HW7 - Q3 ==============================================
setwd("G:/Georgia Tech/Analytical Models/Assignments")

install.packages("fitdistrplus")
install.packages("logspline")

require(data.table)
require(ggplot2)
require(fitdistrplus)
require(logspline)

#Read in the dataset
dist = fread("distributions.csv")


#Variable 1

#Using the fitdistrplus package to create the the pdf and cdf of the variable.

plotdist(dist$V1[!is.na(dist$V1)], histo = TRUE, demp = TRUE)

#Using the fitdistrplus package to view the Cullen and Frey grpah which
#helps us identify the distribution our data most closely resembles.
#The Cullen/Frey uses the kurtosis and the square of skewness to identify
#the distribution.

descdist(dist$V1[!is.na(dist$V1)], discrete = FALSE, boot = 100)

#The bootstrap allows to normalize the random variability in the data.

#The plot says Normal with almost ~100% certainty.
#Even the bootstrapped values revolve around normal.

#If we know the distribution, we can use this function to find out
#the distribution stats:
#fitdistr(dist$V1[!is.na(dist$V1)], "normal")
#descdist is already giving us that although.

#Fitting the distribution to a normal 
normal_V1 = fitdist(dist$V1[!is.na(dist$V1)], "norm")
plot(normal_V1)

#From these 4 plots, we can infer that this distribution is definitely a NORMAL
#with mean = 0.93064, sd = 2.121

#######################################################################################
################################### From the paper for fitdistrplus

#Nevertheless, the user needs to know that skewness and kurtosis, like all higher moments, have a very high
#variance. This is a problem which cannot be completely solved by the use of bootstrap. The skewness-kurtosis plot
#should then be regarded as indicative only. The properties of the random variable should be considered, notably its
#expected value and its range, as a complement to the use of the plotdist and descdist functions.

###################################
#######################################################################################


#Variable 2

plotdist(dist$V2[!is.na(dist$V2)], histo = TRUE, demp = TRUE)

descdist(dist$V2[!is.na(dist$V2)], discrete = FALSE, boot = 100)

#Cullen Frey suggests that this could be a Gamma or Weibull as Weibull is close to lognormal
#and Gamma.


weibull_V2 = fitdist(dist$V2[!is.na(dist$V2)], "weibull")
plot(weibull_V2)

gamma_V2 = fitdist(dist$V2[!is.na(dist$V2)], "gamma")
plot(gamma_V2)

#Making Comparison plots of the two
legend = c("Weibull", "Gamma")

denscomp(list(weibull_V2, gamma_V2), legendtext = legend)
qqcomp(list(weibull_V2, gamma_V2), legendtext = legend)
cdfcomp(list(weibull_V2, gamma_V2), legendtext = legend)
ppcomp(list(weibull_V2, gamma_V2), legendtext = legend)

#Comparing the statistics
summary(weibull_V2)
summary(gamma_V2)

#Both the distributions have a good fit but the weibull has a much better fit in the 
#QQ plot and the pdf.

#Calculating the Weibull parameters, which the summary also gave.
fitdistr(dist$V2[!is.na(dist$V2)], "weibull")

#The distribution is a Weibull with B = 2..2231 and N = 1.927


#Variable 3
plotdist(dist$V3[!is.na(dist$V3)], histo = TRUE, demp = TRUE)

descdist(dist$V3[!is.na(dist$V3)], discrete = FALSE, boot = 100)

#From the CF graph, it is very vague to conclude which distribution this is.
#It could be normal, weibull, lognormal or gamma.
#For Beta, the values have to be between 0 & 1.

#Fitting the various plots
normal_V3 = fitdist(dist$V3[!is.na(dist$V3)], "norm")
weibull_V3 = fitdist(dist$V3[!is.na(dist$V3)], "weibull")
lognormal_V3 = fitdist(dist$V3[!is.na(dist$V3)], "lnorm")
gamma_V3 = fitdist(dist$V3[!is.na(dist$V3)], "gamma")

legend1 = c("Normal", "Weibull", "Lognormal", "Gamma")

plot_list = list(normal_V3, weibull_V3, lognormal_V3, gamma_V3)

denscomp(plot_list, legendtext = legend1)
qqcomp(plot_list, legendtext = legend1)
cdfcomp(plot_list, legendtext = legend1)
ppcomp(plot_list, legendtext = legend1)

#Weibull has the worst fit.
summary(normal_V3)
summary(gamma_V3)
summary(lognormal_V3)
