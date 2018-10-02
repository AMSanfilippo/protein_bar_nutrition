library('ggplot2')
library('DescTools')
library('rrcov')

setwd('your/data/directory')

# load in protein bar data
protein_data <- read.csv('protein_bars_all.csv') 
protein_data <- protein_data[,-c(1)]
protein_data[,'type'] <- 'protein'

# load in candy bar data
candy_data <- read.csv('candy_bars_all.csv')  
candy_data <- candy_data[,-c(1)]
candy_data[,'type'] <- 'candy'

data <- rbind(protein_data, candy_data)
ty <- factor(data[,'type'],levels = c('protein','candy'))
data[,'type'] <- ty

##############################
# EMPIRICS

# raw histograms of macronutrients for protein bars and candy;
# univariate t-tests for differences in means:
calories <- ggplot(data, aes(x=calories,color=type,fill=type)) + geom_histogram(binwidth=10, alpha=0.5, position="identity") + labs(title="Comparison of calorie counts")
calories

# NOTE: function uses unequal variances t-test formula
t.test(data[,'calories']~data[,'type'])
# t = -1.2, p = 0.23
# the difference in average calories per serving for protein bars and candy bars is not statistically significant
# mean(protein) = 222.90, mean(candy) = 230.98

carbohydrates <- ggplot(data, aes(x=carbohydrate,color=type,fill=type)) + geom_histogram(binwidth=1, alpha=0.5, position="identity") + labs(title="Comparison of carbohydrate counts")
carbohydrates

# NOTE: we're computing mean(protein) - mean(candy)
t.test(data[,'carbohydrate']~data[,'type'])
# t = -3.057, p = 0.0026
# on average, protein bars have significantly fewer grams of carbs per serving than candy bars
# mean(protein) = 27.06, mean(candy) = 29.97

protein <- ggplot(data, aes(x=protein,color=type,fill=type)) + geom_histogram(binwidth=1, alpha=0.5, position="identity") + labs(title="Comparison of protein counts")
protein

t.test(data[,'protein']~data[,'type']) 
# t = 30.48, p ~= 0
# on average, protein bars have significantly more protein per serving than candy bars
# mean(protein) = 13.17, mean(candy) = 2.86

fats <- ggplot(data, aes(x=fat,color=type,fill=type)) + geom_histogram(binwidth=1, alpha=0.5, position="identity") + labs(title="Comparison of fat counts")
fats

t.test(data[,'fat']~data[,'type'])
# t = -7.75, p ~= 0
# on average, protein bars have significantly less fat per serving than candy bars
# mean(protein) = 8.29, mean(candy) = 11.91

sugars <- ggplot(data, aes(x=sugar,color=type,fill=type)) + geom_histogram(binwidth=1, alpha=0.5, position="identity") + labs(title="Comparison of sugar counts")
sugars

t.test(data[,'sugar']~data[,'type'])
# t = -15.20, p ~= 0
# on average, protein bars have significantly less sugar per serving than candy bars
# mean(protein) = 12.33, mean(candy) = 24.78

# cluster analysis:

# macronutrients only
km_macro <- kmeans(data[,c(3,4,12)],2,20)
macro_clusters <- km_macro$cluster
table(macro_clusters,data[,17])
# macro_clusters protein candy
#         1      55      34
#         2      240     54
# in the macro-dimensional space, the clustering algorithm is able to mostly place protein bars into one group, but also places the majority of candy bars into this group
# in this 3D space, these two groups of bars are not as distinct as they may appear using one-dimensional tests

data[,'kmeans_group'] <- as.factor(macro_clusters)

macro_scatter_pc <- ggplot(data, aes(x=carbohydrate, y=protein, shape=type, color=kmeans_group)) + geom_point() 
macro_scatter_pc + geom_hline(yintercept = mean(data[data$type=='protein','protein']),color='grey',linetype='dashed') + geom_hline(yintercept = mean(data[data$type=='candy','protein']),color='grey',linetype='dashed')

macro_scatter_pf <- ggplot(data, aes(x=fat, y=protein, shape=type, color=kmeans_group)) + geom_point()  

macro_scatter_fc <- ggplot(data, aes(x=fat, y=carbohydrate, shape=type, color=kmeans_group)) + geom_point()  

# macronutrients and sugar
km_full <- kmeans(data[!is.na(data$sugar),c(3,4,12,15)],2,20)
full_clusters <- km_full$cluster
table(full_clusters,data[!is.na(data$sugar),17])
# full_clusters protein candy
#        1      58      69
#        2      232     16
# using the 4D macro+sugar space, the clustering algorithm is able to find significantly more distinct groups. 
# sorts about 80% of protein/candy bars into their own categories, respectively

data[!is.na(data$sugar),'kmeans_group'] <- as.factor(full_clusters)
full_scatter_pc <- ggplot(data[!is.na(data$sugar),], aes(x=carbohydrate, y=protein, shape=type, color=kmeans_group)) + geom_point() 

full_scatter_pf <- ggplot(data[!is.na(data$sugar),], aes(x=fat, y=protein, shape=type, color=kmeans_group)) + geom_point() 

full_scatter_fc <- ggplot(data[!is.na(data$sugar),], aes(x=fat, y=carbohydrate, shape=type, color=kmeans_group)) + geom_point() 

# next: attempt to apply a more formal test for difference in nutrient distributions between protein bars and candy bars
# one way we can do this is with the T-Squared (T2) test, which is a multivariate generalization of the t-test

# note that the T2 test requires normally distributed data
# in preparation: log-transform nutrient data
# plot histograms of ln(macronutrients) for protein bars and candy to show (approximate) normality

ln_calories <- ggplot(data, aes(x=log(calories),color=type,fill=type)) + geom_histogram(binwidth=.05, alpha=0.5, position="identity") + labs(title="Comparison of ln(calorie counts)")
ln_calories

ln_carbohydrates <- ggplot(data, aes(x=log(carbohydrate),color=type,fill=type)) + geom_histogram(binwidth=0.2, alpha=0.5, position="identity") + labs(title="Comparison of ln(carbohydrate counts)")
ln_carbohydrates

ln_protein <- ggplot(data, aes(x=log(protein),color=type,fill=type)) + geom_histogram(binwidth=.5, alpha=0.5, position="identity") + labs(title="Comparison of ln(protein counts)")
ln_protein

ln_fats <- ggplot(data, aes(x=log(fat),color=type,fill=type)) + geom_histogram(binwidth=0.3, alpha=0.5, position="identity") + labs(title="Comparison of ln(fat counts)")
ln_fats

sqrt_sugars <- ggplot(data, aes(x=sqrt(sugar),color=type,fill=type)) + geom_histogram(binwidth=0.55, alpha=0.5, position="identity") + labs(title="Comparison of sqrt(sugar counts)")
sqrt_sugars

# T2 test
# here we are testing H0: E[log protein nutrients] = E[log candy nutrients]
# note that this is NOT the same as testing H0: E[protein nutrients] = E[candy nutrients]
# however, if the log of nutrients is close to normal then e^E[log nutrients] ~= median of nutrients, so e^(E[log protein nutrients] - E[log candy nutrients]) ~= (median of protein nutrient)/(median of candy nutrients)
# i.e. in the original units, we are doing inference on the ratio of medians of the nutrient distributions

p <- data[data$type=="protein",c(3,4,12)]
p$carbohydrate <- log(p$carbohydrate)
p$protein <- log(p$protein)
p$fat <- log(p$fat)

p_alt <- data[data$type=="protein",c(3,4,12,15)]
p_alt$carbohydrate <- log(p_alt$carbohydrate)
p_alt$protein <- log(p_alt$protein)
p_alt$fat <- log(p_alt$fat)
p_alt$sugar <- sqrt(p_alt$sugar)

c <- data[data$type=="candy",c(3,4,12)]
c$carbohydrate <- log(c$carbohydrate)
c$protein <- log(c$protein)
c$fat <- log(c$fat)

c_alt <- data[data$type=="candy",c(3,4,12,15)]
c_alt$carbohydrate <- log(c_alt$carbohydrate)
c_alt$protein <- log(c_alt$protein)
c_alt$fat <- log(c_alt$fat)
c_alt$sugar <- sqrt(c_alt$sugar)

# test for difference in mv means in ln(macro) space
T2.test(p,c)
# T2 = 1257.0, F = 416.8, df1 = 3, df2 = 379, p-value < 2.2e-16
#           ln(carbohydrate)  ln(fat) ln(protein)
# mean x-vector     3.256726 2.032887 2.5140316
# mean y-vector     3.366406 2.427139 0.9028716
# i.e. significant evidence that the ratio of medians != [1 1 1]

# test for difference in mv means in macro + sugar space
T2.test(p_alt,c_alt)
# T2 = 1250.40, F = 310.07, df1 = 4, df2 = 370, p-value < 2.2e-16
#           ln(carbohydrate)  ln(fat) ln(protein) ln(sugar)
# mean x-vector     3.258744 2.031369 2.5119317 3.296167
# mean y-vector     3.361630 2.426671 0.8973488 4.932989
# i.e. significant evidence that the ratio of medians != [1 1 1 1]


