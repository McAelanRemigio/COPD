# Loading and Removing NA
copd = read.csv("copd_data_project.csv")
head(copd)
 
dat1 = na.omit(copd)
nrow(dat1)

dat2 = copd[is.na(copd$FEV1_phase2), ]
nrow(dat2)

# Plotting Histogram for FEV1
histdat1 = dat1$FEV1
hist(histdat1)

# Check for percentage of values within 1-2 SD of mean, FEV1 closely follows shape of bell curve/Normal distribution
mean = mean(histdat1)
sd = sd(histdat1)
mean # 3.1843
sd # 0.8736
mean-sd; mean+sd
pnorm(3.1843, mean=2.3107, sd=0.8736) - pnorm(1.4370, mean=2.3107, sd=0.8736)
mean-2*sd; mean+2*sd
pnorm(4.0579, mean=2.3107, sd=0.8736) - pnorm(0.5634, mean=2.3107, sd=0.8736)
mean-3*sd; mean+3*sd
pnorm(4.9312, mean=2.3107, sd=0.8736) - pnorm(-0.3102, mean=2.3107, sd=0.8736)

# Plot histograms of total lung capacity and visit_age
histdat2 = dat1$total_lung_capacity
hist(histdat2)

histdat3 = dat1$visit_age
hist(histdat3)

mean = mean(histdat2)
sd = sd(histdat2)
mean
sd
pnorm(7.1529, mean=4.2995, sd=2.8534) - pnorm(1.4461, mean=4.2995, sd=2.8534)
pnorm(-1.4072, mean=4.2995, sd=2.8534) - pnorm(0, mean=4.2995, sd=2.8534)
pnorm(12.8597, mean=4.2995, sd=2.8534) - pnorm(-4.2606, mean=4.2995, sd=2.8534)
# total_lung_capacity has errors in distribution and does not follow empirical rule

mean = mean(histdat3)
sd = sd(histdat3)
mean
sd
pnorm(3.1843, mean=2.3107, sd=8.6114) - pnorm(1.4370, mean=2.3107, sd=8.6114)
pnorm(76.8943, mean=2.3107, sd=8.6114) - pnorm(42.4486, mean=2.3107, sd=8.6114)
pnorm(85.5057, mean=2.3107, sd=8.6114) - pnorm(33.8372, mean=2.3107, sd=8.6114)
# visit_age does not follow empirical rule

library(ggplot2)

# FEV1_phase2 vs smoking status boxplot
g = ggplot(dat1, aes(x=FEV1_phase2, y=smoking_status)) + geom_boxplot()
g

mean(dat1$FEV1[dat1$smoking_status == "Never smoked"] )
mean(dat1$FEV1[dat1$smoking_status == "Current smoker"] )
mean(dat1$FEV1[dat1$smoking_status == "Former smoker"] )

current = dat1$FEV1[dat1$smoking_status == "Current smoker" ]
former = dat1$FEV1[dat1$smoking_status == "Former smoker" ]
# Welch 2 Sample T-Test
t.test(current, former)
# We can safely say we have sufficient evidence to reject the null hypothesis
# and say that the difference in average FEV1 between current smokers and non-smokers
# is significant because the 95% Confidence Interval does not contain 0.

# More boxplots to visualize FEV1_phas2 vs other categorical variables
# Gender and Emphysema
g1 = ggplot(dat1, aes(x=FEV1_phase2, y=gender)) + geom_boxplot()
g1
g2 = ggplot(dat1, aes(x=FEV1_phase2, y=emphysema)) + geom_boxplot()
g2

yes = dat1$FEV1[dat1$emphysema == "Yes" ]
no = dat1$FEV1[dat1$emphysema == "No" ]
t.test(yes, no)

male = dat1$FEV1[dat1$gender == "Male" ]
female = dat1$FEV1[dat1$gender == "Female" ]
t.test(male, female)

# measuring p < 0.05

# We can safely conclude that we have sufficient evidence to reject the null hypothesis
# and say that there is an average difference in FEV1_phase2
# between those with emphysema vs those without emphysema.

# We can also safely conclude that we have sufficient evidence to reject the null hypothesis
# and say that there is an average difference in FEV1_phase2 between
# males and females. 

# FEV1 vs FEV1_phase2 scatterplot + Regression
plot(x = dat1$FEV1,y = dat1$FEV1_phase2,
   xlab = "FEV1",
   ylab = "FEV1_phase2",
   main = "FEV1 vs FEV1_phase2"
)

# linear modeling, R^2, confidence interval
fit = lm(FEV1_phase2 ~ FEV1, data = dat1)
abline(fit)
summary(fit)
confint(fit)

dat1 # check

# Strong positive linear relationship between FEV1 and FEV1_phase2
# ~79% of variability in FEV1_phase2 can be explained by FEV1 variation

# In terms of a patient's breathing health, we can sufficiently
# conclude that FEV1 will greatly decrease in 5+ years and FEV1_phase 2
# is a strong indicator of deteriorating health by factors such as smoking or diseases

# Measuring other possible variables related to breathing health via scatterplots

plot(x = dat1$total_lung_capacity,y = dat1$FEV1_phase2,
   xlab = "Total Lung Capacity",
   ylab = "FEV1_phase2",
   main = "Total Lung Capacity vs FEV1_phase2"
)

plot(x = dat1$pct_gastrapping,y = dat1$FEV1_phase2,
   xlab = "Percent Gastrapping",
   ylab = "FEV1_phase2",
   main = "Percent Gastrapping vs FEV1_phase2"
)

fit1 = lm(FEV1_phase2 ~ total_lung_capacity, data = dat1)
fit2 = lm(FEV1_phase2 ~ pct_gastrapping, data = dat1)

summary(fit1)
summary(fit2)
confint(fit1)
confint(fit2)

# lung capacity vs FEV1_phase2 has weak positive linear correlation, little to no correlation
# pct_gas_trapping has a moderate negative linear correlation.

# Using Regression Model From total_lung_capacity, FEV1_phase2, and pct_gastrapping
# also calculate root mean squared error to view model's predictions matching actual values

y = dat1$FEV1_phase2
y_predicted1 = fitted(lm(y ~ pct_emphysema, data = dat1))
y_predicted2 = fitted(lm(y ~ pct_gastrapping, data = dat1))
y_predicted3 = fitted(lm(y ~ total_lung_capacity, data = dat1))

rmse1 = sqrt(mean((y - y_predicted1)^2))
rmse1

rmse2 = sqrt(mean((y - y_predicted2)^2))
rmse2

rmse3 = sqrt(mean((y - y_predicted3)^2))
rmse3

install.packages("randomForest")
library(randomForest)

# Statistical/Machine Learning Random Foresting

samp = sample(1:nrow(dat1), 3000)
train = dat1[samp, ]

valid = dat1[-samp, ]

mse = function(true, pred){
  return(mean((true - pred)^2))
}



fit = randomForest(FEV1_phase2 ~ FEV1 +
  FVC +
  FEV1_FVC_ratio +
  copd + pct_gastrapping +
  height_cm +
  exp_meanatt +
  gender +
  pct_emphysema +
  total_lung_capacity +
  insp_meanatt +
  visit_age +
  Duration_Smoking, data = train[,-(1:3)],
                    importance = TRUE,

                    # hyperparameters (change to improve predictions)
                    ntree    = 10000,  # number of trees (more trees, better accuracy)
                    mtry     = 5,   # number of variables (based on running this forest the first time, terms = FEV1_phase2 ~ sid + visit_year + visit_date + visit_age + gender + race + height_cm + .     weight_kg + sysBP + diasBP + hr + O2_hours_day + bmi + asthma + .     hay_fever + bronchitis_attack + pneumonia + chronic_bronchitis + .     emphysema + copd + sleep_apnea + SmokStartAge + CigPerDaySmokAvg + .     Duration_Smoking + smoking_status + total_lung_capacity + .     pct_emphysema + functional_residual_capacity + pct_gastrapping + .     insp_meanatt + exp_meanatt + FEV1_FVC_ratio + FEV1 + FVC)))
                    nodesize = 5,    # size of nodes
                    maxnodes = 2000, # maximum number of nodes
                    )

                    fit

mse(valid$FEV1_phase2, predict(fit, newdata = valid))
sqrt(mse(valid$FEV1_phase2, predict(fit, newdata = valid)))

predict(fit, newdata = train)
print("MSE TRAIN SET")
mse(train$FEV1_phase2, predict(fit, newdata = train))
print("RMSE TRAIN SET")
sqrt(mse(train$FEV1_phase2, predict(fit, newdata = train)))

predict(fit, newdata = valid)
print("MSE VALID SET")
mse(valid$FEV1_phase2, predict(fit, newdata = valid))
print("RMSE VALID SET")
sqrt(mse(valid$FEV1_phase2, predict(fit, newdata = valid)))

FEV1_phase2_predictions = predict(fit, newdata = dat2)

preds = data.frame(sid = dat2$sid, FEV1_phase2_predictions)
preds
write.csv(valid, 'copd_predictions.csv')
