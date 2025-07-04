#install.packages("msos")
library(msos)
data("SAheart")
data <- SAheart
data <- SAheart[,c("sbp","tobacco","ldl","adiposity","typea","obesity","alcohol","age","famhist", "chd")]
head(data)

#Comparing the numerical values with CHD using boxplot.
attach(data)
boxplot(sbp ~ chd,col="pink",main="sbp")
boxplot(tobacco ~ chd,col="pink",main="tobacco")
boxplot(ldl ~ chd,col="pink",main="ldl")
boxplot(adiposity ~ chd,col="pink",main="adiposity")
boxplot(typea ~ chd,col="pink",main="typea")
boxplot(obesity ~ chd,col="pink",main="obesity")
boxplot(alcohol ~ chd,col="pink",main="alcohol")
boxplot(age ~ chd,col="pink",main="Age")


#More in depth boxplot using multiple categorical values. Might be good to just present noticeable 
#difference values of values with many outliers. It might be good to present this plots after the hotelling
#analysis since then we can look into the famhist and then do the GLM to see if it famhist is important.

ggplot(data, aes(x = chd, y = ldl, fill = famhist)) +
  geom_boxplot() +
  labs(title = "LDL Levels by CHD Status and Family History")

ggplot(data, aes(x = chd, y = sbp, fill = famhist)) +
  geom_boxplot() +
  labs(title = "SBP Levels by CHD Status and Family History")

ggplot(data, aes(x = chd, y = tobacco, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Tobacco Consumption by CHD Status and Family History")

ggplot(data, aes(x = chd, y = adiposity, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Adiposity Levels by CHD Status and Family History")

ggplot(data, aes(x = chd, y = typea, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Typea Levels by CHD Status and Family History")

ggplot(data, aes(x = chd, y = obesity, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Obesity Levels by CHD Status and Family History")

ggplot(data, aes(x = chd, y = alcohol, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Alcohol compsumption by CHD Status and Family History")

ggplot(data, aes(x = chd, y = age, fill = famhist)) +
  geom_boxplot() +
  labs(title = "Age by CHD Status and Family History")


#Before running the hoteling function, we need to check for the behavior of the variance in the groups. i.e. 
#is there a difference in variance behavior between chd groups in obsesity?

myLevenes.test(data[,1:8], data$chd) #At least one group variance behavior is not normal. 

#Individual comparisons
leveneTest(sbp ~ chd, data = data) #Nonnormal
leveneTest(tobacco ~ chd, data = data)#Nonnormal
leveneTest(ldl ~ chd, data = data)#Nonnormal but p = 0.04475
leveneTest(adiposity ~ chd, data = data) #Normal
leveneTest(typea ~ chd, data = data)#Normal
leveneTest(obesity ~ chd, data = data)#Normal
leveneTest(alcohol ~ chd, data = data)#Normal
leveneTest(age ~ chd, data = data)#Nonnormal

#NOTE: Since we have groups whose variance behave non-normal, 
#It might be best to just run individual t-test on values
#Identified as normal. Then the wilcox test for non-normal
#Run the hotteling function in functions.R

#Nonnormal
wilcox.test(sbp ~ chd, data = data)
wilcox.test(tobacco ~ chd, data = data)
wilcox.test(age ~ chd, data = data)

#Normal
t.test(ldl[chd=="1"],ldl[chd=="0"], var.equal=T)#Difference
t.test(adiposity[chd=="1"],adiposity[chd=="0"], var.equal=T)#Difference
t.test(typea[chd=="1"],typea[chd=="0"], var.equal=T) #Difference but not at 0.01%
t.test(obesity[chd=="1"],obesity[chd=="0"], var.equal=T) #Difference but not at 0.01%
t.test(alcohol[chd=="1"],alcohol[chd=="0"], var.equal=T) #No difference


#################################################


data$chd <- factor(data$chd)    

myHotelling(data[,1:8],data$chd)   
#You can obtain all the means in the first 2 rows.
#There is a difference in at least 1 numerical variable between groups of chd based on the p values of  
#P = 3.559e-19

########################################

#Now lets look at the categorical variable

#Stacked bar plot to easily see the proportions of people that get chd or not based on family hsitory
ggplot(data, aes(x = chd, fill = famhist)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Family History Prevalence by CHD")

# Contingency table.
table(data$famhist, data$chd)
chisq.test(data$famhist, data$chd)

#we can use a logistic regression model GLM to check the influence of all variables on getting chd

model1 <- glm(chd ~ famhist + sbp + tobacco +ldl + adiposity + typea + obesity + 
      alcohol + age, data = data, family = binomial)

#now we can check if famhist (and other predictors) influence the probability of chd?"
summary(model1)

#From here we can check the odds ratio. Remember since we are using logit link, we have to exponentiate the 
#value. That is beta for famhistPresent  0.9253704 then exp{0.9253704} = 2.522803. If you have family history,
# 2.5 times more likely.

#Create confidence intervals.
confint(model1)

#you can create and odds confidence interval
exp(confint(model1))

#Running a reduce model discarding sbp, adiposity, alcohol

reduced_model1 <- glm(chd ~ famhist + tobacco + ldl + typea + age, 
                     data = data, family = binomial)
summary(reduced_model1)
#Now the odds are exp(0.90818) = 2.479805 if you have family history. 

#AIC is not big. Only 5 points in difference. Not necessarily bad, but not drastic.

######Linear discriminant analysis to find out what variable separates people thaat get
#chd and people that dont#### 
#Requires normality, so we only use the normal variables.

library(MASS)

lda_model <- lda(chd ~ adiposity + typea + obesity + alcohol + age, data = data)
lda_model #This is how you can see the influence of the variables.
lda_scores <- predict(lda_model)$x  # Returns LD1 values

# Plot LD1 distributions by `chd` group
library(ggplot2)
ggplot(data.frame(LD1 = lda_scores, chd = data$chd), aes(x = LD1, fill = chd)) +
  geom_density(alpha = 0.5) +
  labs(title = "LDA Separation: LD1 Scores by CHD Status")

################ Last using Random forest because we can use categorical values and not normal values
#install.packages("randomForest")
library(randomForest)
set.seed(123)  # For reproducibility

# Convert 'chd' to factor (RF requires it for classification)
data$chd <- as.factor(data$chd)

# Fit model (include all predictors, even categorical 'famhist')
rf_model <- randomForest(chd ~ famhist + sbp + adiposity + obesity+ typea + ldl + tobacco + alcohol + age, 
                         data = data, 
                         importance = TRUE)  # To assess variable importance

# View results
print(rf_model)
varImpPlot(rf_model)  # Variables at the top are the most important in correctly predicting chd.
importance(rf_model) #All the values. 





############## EXTRAS ##############


#Finding out which group has differences.
#What is our decision route? 0.05, 0.01?
t.test(sbp[chd=="1"],sbp[chd=="0"], var.equal=T) #Difference
t.test(tobacco[chd=="1"],tobacco[chd=="0"], var.equal=T) #Difference
t.test(ldl[chd=="1"],ldl[chd=="0"], var.equal=T)#Difference
t.test(adiposity[chd=="1"],adiposity[chd=="0"], var.equal=T)#Difference
t.test(typea[chd=="1"],typea[chd=="0"], var.equal=T) #Difference but not at 0.01%
t.test(obesity[chd=="1"],obesity[chd=="0"], var.equal=T) #Difference but not at 0.01%
t.test(alcohol[chd=="1"],alcohol[chd=="0"], var.equal=T) #No difference
t.test(age[chd=="1"],age[chd=="0"], var.equal=T) #Difference,



library(car)
qqPlot(data[,1])
shapiro.test(data[,1])
qqp(log(data[,1]))
shapiro.test(log(data[,1]))

qqPlot(sqrt(data[,2]))
shapiro.test(sqrt(data[,2]))

qqPlot(data[,3])
shapiro.test(data[,3])

qqPlot(data[,4])
shapiro.test(data[,4])

qqPlot(data[,5])
shapiro.test(data[,5])

qqPlot(data[,6])
shapiro.test(data[,6])

qqPlot(data[,7])
shapiro.test(data[,7])

qqPlot(data[,8])
shapiro.test(data[,8])

leveneTest(sbp ~ chd, data = data) #Nonnormal
leveneTest(tobacco ~ chd, data = data)#Nonnormal
leveneTest(ldl ~ chd, data = data)#Nonnormal but p = 0.04475
leveneTest(adiposity ~ chd, data = data) #Normal
leveneTest(typea ~ chd, data = data)#Normal
leveneTest(obesity ~ chd, data = data)#Normal
leveneTest(alcohol ~ chd, data = data)#Normal
leveneTest(age ~ chd, data = data)#Nonnormal

p <- ncol(data[,1:8])
means <- apply(data[,1:8],2,mean)
var.data <- cov(data[,1:8])
d2 <- mahalanobis(x=data[,1:8], center=means, cov = var.data)
ks.test(d2,"pchisq",p)

library(goftest,quietly = TRUE)
cvm.test(d2, "pchisq",df=p)
ad.test(d2,"pchisq",df=p)
wilcox.test(sbp ~ chd, data = data)


library(dplyr)
chd_yes <- data %>% filter(chd == "1")
chd_no <- data %>% filter(chd == "0")

ggplot(chd_yes, aes(sample = adiposity)) + 
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of LDL (CHD=Yes)")
shapiro.test(chd_yes$adiposity)
ad.test(chd_yes$ldl, 'pchisq', df = 8)

pairs(~ sbp + ldl + tobacco + age, data = data, 
      col = ifelse(data$chd == "Yes", "red", "blue"),
      main = "Scatterplot Matrix (Red = CHD)")

ggplot(data, aes(x = chd, fill = famhist)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Family History Prevalence by CHD")


ggplot(data, aes(x = age, fill = chd)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution by CHD Status")


library(cluster)
dd <- dist(data[1:8])
km8 <- kmeans(data[1:8], centers=8)
cl8 <- silhouette(x = km8$cluster, dd)
ss8 <- cl8[,"sil_width"]
ss8
ss <- rep(0,12)
for (k in 1:12) {
  km <- kmeans(data[1:8], centers = k)
  cl <- silhouette(x = km$cluster,dd)
  if(k > 1){
    ss[k] <- mean(cl[,"sil_width"])
  }
}
ss
plot(ss)

install.packages("randomForest")
library(randomForest)
set.seed(123)  # For reproducibility

# Convert 'chd' to factor (RF requires it for classification)
data$chd <- as.factor(data$chd)

# Fit model (include all predictors, even categorical 'famhist')
rf_model <- randomForest(chd ~ famhist + sbp + adiposity + obesity+ typea + ldl + tobacco + alcohol + age, 
                         data = data, 
                         importance = TRUE)  # To assess variable importance

# View results
print(rf_model)
varImpPlot(rf_model)  # Plot variable importance
importance(rf_model)



