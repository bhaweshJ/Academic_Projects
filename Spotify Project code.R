Spotify_csv <- read.csv("C:/Users/bhawe/Downloads/top50.csv")

str(Spotify_csv)

dim(Spotify_csv)

nrow(Spotify_csv)
ncol(Spotify_csv)

Spotify_csv[5 :14] <- as.data.frame(scale(Spotify_csv[5:14]))

view(Spotify_csv)

ggplot(Spotify_csv, aes(x=beats_per_minute, y=Popularity, group = 1)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(Spotify_csv, aes(x=beats_per_minute, y=Popularity, group = 1)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=9,
               outlier.size=4)



any_null <- is.na(Spotify_csv)

View(any_null)

colnames(Spotify_csv)[1] <- "Position"  #Renamed column 1 to position

View(Spotify_csv)  


colnames(Spotify_csv)[2] <- "Track_names"  #Renamed column 2 to Track_names

colnames(Spotify_csv)[3] <- "artists_names"  #Renamed column 3 to Artisit_Names

colnames(Spotify_csv)[5] <- "beats_per_minute"  #Renamed column 5 to Artisit_Names

colnames(Spotify_csv)[8] <- "Loudness"  #Renamed column 8 to Loudness,Acousticness

colnames(Spotify_csv)[10] <- "Valence"  #Renamed column 10 to Valence

colnames(Spotify_csv)[11] <- "Length"  #Renamed column 11 to Length

colnames(Spotify_csv)[12] <- "Acousticness"  #Renamed column 12 to Length

colnames(Spotify_csv)[13] <- "Speechiness"  #Renamed column 13 to Length


View(Spotify_csv)


# Converting integer to time

Spotify_csv['temp_length']=(Spotify_csv['Length']/60)

Spotify_csv['temp_length']=as.integer(Spotify_csv$temp_length)  #converted to integer


library(lubridate)
tm <- seconds_to_period(Spotify_csv$Length)
Spotify_csv['temp_length1']=sprintf("%02d:%02d", minute(tm), second(tm))


Spotify_csv <- subset(Spotify_csv,select=-temp_length)

View(Spotify_csv)

install.packages("tidyverse")

library(tidyverse)

install.packages('corrgram')

library(corrgram)

#Spotify_csv %>% select(c(colnames(Spotify_csv)) ) %>% corrgram()
Spotify_csv[,5:14] %>% corrgram()

write.csv(round(cor(Spotify_csv[,5:14]),3),file="abc.csv")
?write.csv
#cor
#str(Spotify_csv[,3:14])

getwd()

num_spot <- Spotify_csv[,5:14]

view(num_spot)

str(Spotify_csv[,5:13])
num_spot_pca <- prcomp(num_spot, center = TRUE,scale. = TRUE)

summary(num_spot_pca)


write.csv(a,file="abc_pca1.csv")



Eigenvalues <- eigen(cov(num_spot))$values
Eigenvectors <- eigen(cov(num_spot))$vector


Eigenvalues

Eigenvectors



#install.packages("tidyverse")






install.packages(c('devtools','curl'))
library(devtools)
install_github("vqv/ggbiplot",force = TRUE)

library(ggbiplot)
# Creating the vector with the x-values for dt function

num_spot_pca <- prcomp(Spotify_csv[,5:14], center = TRUE,scale. = TRUE)

?prcomp

ggbiplot(num_spot_pca)

eigenvals <- c(1595.019536,962.440292,575.315754,364.834404,165.806103,113.867032,96.490145,72.637381,15.890172,1.851834);
plot(
  x = seq(1:length(eigenvals)), y = eigenvals,
  type = "o",
  xlab = "Principle Component", ylab = "Variance");

#then beats e=oer minute 

view(Spotify_csv)

Spotify_csv %>% select(Liveness, beats_per_minute, Popularity) %>% cor()




#1.since the values of p>0.05 (0.09),the values are statistically insginificant
#1.since the values of p>0.05 (0.09),the values are statistically insginificant


#x_dt <- seq(Spotify_csv$Energy, Spotify_csv$Popularity, by = 0.01)

# Applying the dt() function
y_dt <- dt(Spotify_csv$Speechiness, Spotify_csv$Popularity, df = 50)

# Plotting 
plot(y_dt, type = "l", main = "t-distribution density function example", las=1)

Spotify_csv %>% select(Speechiness, beats_per_minute, Popularity) %>% cor()

Spotify_csv %>% select(Speechiness, beats_per_minute, Popularity) %>% corrgram()   


#the correlation coffcient between the populrity abd speechiness and beats_per_minute is less than
#the correlation between the otheotify_csv %>% select(Acousticness, Popularity) %>% cor()r 2 valirables.
Sp

Spotify_csv %>% select( Acousticness, Popularity) %>% corrgram()  #-34%





Spotify_csv %>% select(Energy, Popularity) %>% cor()
Spotify_csv %>% select( Energy, Popularity) %>% corrgram()  #-8%


Spotify_csv %>% select(Loudness, Popularity) %>% cor()
Spotify_csv %>% select( Loudness, Popularity) %>% corrgram()  #-4%


Spotify_csv %>% select(Liveness, Popularity) %>% cor()
Spotify_csv %>% select( Liveness, Popularity) %>% corrgram()  #9.26%         #######


Spotify_csv %>% select(Danceability, Popularity) %>% cor()
Spotify_csv %>% select( Danceability, Popularity) %>% corrgram()  #0.07


Spotify_csv %>% select(Liveness,Valence,Acousticness,Speechiness,beats_per_minute,Energy,Danceability, Popularity) %>% cor()
Spotify_csv %>% select( Liveness,Valence,Acousticness,Speechiness,beats_per_minute,Energy,Danceability, Popularity) %>% corrgram()


#based on the diagram.there is a strong correlation between the popularity-speechiness;popularity -beats_per_minute


res <-cor.test(num_spot$beats_per_minute,num_spot$Popularity,method = 'pearson')

res

?cor.test





View(Spotify_csv)

ggplot(Spotify_csv, aes(x=Popularity, y=artists_names)) + 
  geom_point()+
  geom_smooth(method=lm)
# Remove the confidence interval
ggplot(Spotify_csv, aes(x=Popularity, y=beats_per_minute)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# Loess method

ggplot(Spotify_csv, aes(x=beats_per_minute, y=Popularity)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Spotify_csv, aes(x=Speechiness, y=Popularity)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)


?rownames()


model1_pop_beats <- lm(Popularity~beats_per_minute, Spotify_csv) 

?lm

model1_pop_beats.scatter <- ggplot(Spotify_csv, aes(x=beats_per_minute, y=Popularity)) + 
  geom_point(color='#2980B9', size = 4)  + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50') + 
   
  labs(title='Original Regression Line')

library(gridExtra)

grid.arrange(model1_pop_beats.scatter)

#Y~X+X2

#simple.fit = lm(Sales~Spend, data=dataset)
#summary(simple.fit)
#multi.fit = lm(Sales~Spend+Month, data=dataset)
#summary(multi.fit)


summary(model1_pop_beats)

ab <- data.frame(Spotify_csv$beats_per_minute,Spotify_csv$Popularity)

View(ab)

plot(ab, col='blue', pch=20, cex=2, main="Relationship between Poplarity and beats per minute",
     xlab="beats per minute", ylab="popularity");



###########summarizing model 1##########

###residuals##

#Residuals are essentially the difference between the actual observed response values
#(Popularity to beats_per_minute in our case) and
#the response values that the model predicted
#Smaller residuals are better.

#When assessing how well the model fit the data, 
#you should look for a symmetrical distribution across these points on the mean value zero (0). 
#In our example, we can see that the distribution of the residuals do not appear to be strongly symmetrical. 
#That means that the model predicts certain points that fall far away from the actual observed points.


########Coefficients  #############
#Intercept
#Theoretically, in simple linear regression, the coefficients are two unknown constants that
#represent the intercept and slope terms in the linear model.
#The intercept is the left over when you average the independent and dependent variable.  
#In the simple regression we see that the intercept is much larger meaning there's a fair amount left over. 


#The coefficient Estimate contains two rows; the first one is the intercept.
#The intercept, in our example, is essentially the expected value of the popularity required 
#for a track to be popular when we consider the average beats oer min for each track in the dataset. 
#In other words, it takes an average track in our dataset 84.07766 populance  level.



#The second row in the Coefficients is the slope, or in our example,
#the effect beats_per_minute has in populance level. 
#The slope term in our model is saying that for every 1 bpm increase , the required populance  goes up by 0.02851.



########Coefficient -Standard Error

#The coefficient Standard Error measures the average amount 
#hat the coefficient estimates vary
#from the actual average value of our response variable

#The Standard Error can be used to compute an estimate of the expected difference in case we ran the model again and again. 
#In other words, we can say that the required popularity based on beats per minute  can vary by 0.02057.

#The Standard Errors can also be used to compute confidence intervals and to statistically test the hypothesis
#of the existence of a relationship between speed and distance required to stop

#It's really only useful for calculating the t-value.

########Coefficient -tvalue

#The coefficient t-value is a measure of how many standard deviations our coefficient estimate is far away from 0.
#We want it to be far away from zero as this would indicate we could reject the null hypothesis - that is, we could declare a relationship
#between population  and beats per min exists

#In our example, the t-statistic values are relatively far away from zero and are large relative
#to the standard error, which could indicate a relationship exists. In general, t-values are also used to compute p-values.


################Coefficient - Pr(>t)

#The Pr(>t) acronym found in the model output relates to the probability of observing any value equal or larger than t. 
#A small p-value indicates that it is unlikely we will observe a relationship between the predictor (beats_per_min) and response (popularity) variables due to chance.

#Typically, a p-value of 5% or less is a good cut-off point. In our model example, the p-values are very close to zero. 
#Note the 'signif. Codes' associated to each estimate. 
#Three stars (or asterisks) represent a highly significant p-value. 
#Consequently, a small p-value for the intercept and the slope indicates that we can reject the null hypothesis which allows us to conclude that there is a relationship between Popularity and beats per min.



#######################################Residual Standard Error#####################

#(performance measure)


#Residual Standard Error is measure of the quality of a linear regression fit.
#Theoretically, every linear model is assumed to contain an error term E.
#Due to the presence of this error term, we are not capable of perfectly predicting our response variable (popularity) from the predictor (beats per minute) one
#The Residual Standard Error is the average amount that the response (popularity) will deviate from the true regression line.
#In our example, the actual distance required to stop can deviate from the true regression line by approximately 4.45, on average(Residual standard error: 4.45 on 48 degrees of freedom)


#In other words, given that the mean populance  based on the beats per minute is 84.07766 and that the Residual Standard Error is 15.3795867, 
#we can say that the percentage error is (any prediction would still be off by) 18.13%.

#Smaller is better.

#In R, the lm summary produces the standard deviation of the error  with a slight twist

#######################Multiple R-squared, Adjusted R-squared########################


#The R-squared (R2) statistic provides a measure of how well the model is fitting the actual data.
#It takes the form of a proportion of variance.  R2  is a measure of the linear relationship between our predictor variable (beats per minute) and our response / target variable (population ).

#It always lies between 0 and 1 
#(i.e.: a number near 0 represents a regression that does not explain the variance in the response variable well and a number close to 1 does explain the observed variance in the response variable)


#In our example, the R2 we get is 0.03845. Or roughly 3.85% of the variance found in the response variable (popularity) can be explained by the predictor variable (beats per minute).

#Step back and think: If you were able to choose any metric to predict popularity,
#would beats_per_minute be one and would it be an important one that could help explain how popularity  would vary based on beats_per_minute? Baes on R2 values it's easy to see that the answer is not entirely sure. That why we get a relatively weak R2.

#In multiple regression settings, the R2 will always increase as more variables are included in the model. That's why the adjusted R2 is the preferred measure as it adjusts for the number of variables considered.


##########################F=stastic##################################


#F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables.

#The further the F-statistic is from 1 the better it is.

#However, how much larger the F-statistic needs to be depends on both the number of data points and the number of predictors.
#Generally, when the number of data points is large, an F-statistic that is only a little bit larger than 1 is already sufficient to reject the null hypothesis (H0 : There is no relationship between Popularity and beats_per_min). 
#The reverse is true as if the number of data points is small, a large F-statistic is required to be able to ascertain that there may be a relationship between predictor and response variables. In our example the F-statistic is 1.92
#which is relatively larger than 1 given the size of our data.



###Analyzing residuals#######################################################


#There are four things we're looking for when analyzing residuals.

#1.The mean of the errors is zero (and the sum of the errors is zero)
#2.The distribution of the errors are normal.
#3.All of the errors are independent.
#4.Variance of errors is constant (Homoscedastic)

#In R, you pull out the residuals by referencing the model
#and then the resid variable inside the model.


### 1& 2.Residuals are normally distributed###############3




res_model1 <- model1_pop_beats$
residuals
t.test(res_model1)
#since p value is 1 ,true mean is  equal to zero


#beats_per_min x Residuals Plot
plot(res_model1~ab$Spotify_csv.beats_per_minute[order(ab$Spotify_csv.beats_per_minute)]
     +ab$Spotify_csv.beats_per_minute[order(ab$Spotify_csv.beats_per_minute)],
     main="beats_per_min,speechiness x Residuals\nfor Simple Regression",
     xlab="beats_per_min,speechiness", ylab="Residuals")
abline(h=0,lty=2)

#Histogram of Residuals
hist(res_model1, main="Histogram of Residuals",
     ylab="Residuals")

res_hist <- ggplot(data.frame(res_model1)) + geom_histogram(aes(res_model1)) 
res_hist

#If the histogram looks like a bell-curve it might be normally distributed.

#Q-Q Plot
qqnorm(res_model1)
qqline(res_model1)



#The plots don't seem to be very close to a normal distribution, but we can also use a statistical test.
#The Jarque-Bera test (in the fBasics library,
#which checks if the skewness and kurtosis of your residuals are similar to that of a normal distribution.
#The Null hypothesis of the jarque-bera test is that skewness and kurtosis of your data are both equal to zero (same as the normal distribution).

install.packages("gss")
install.packages("datetime")
install.packages("fBasics")
library(fBasics)
jarqueberaTest(res_model1)



res_model1

#With a p value of 3.545e-11, 
#we donot  the null hypothesis that the skewness and kurtosis of residuals are statistically equal to zero.
#the residual model is normally distrubuted.


###shapiro test#####
###The key point is that the null hypothesis of the Shapiro-Wilk Normality Test
#is that the random variable under consideration is normally disributed. 
#As is always the case, if there is sufficient evidence in the data to reject the claim of the null hypothesis, 
#the p-value of the test will be small (in our case, we will require a p-value less than 0.05 to reject the null hypothesis).

shapiro.test( res_model1)

#W = 0.88428, p-value = 0.0001513

#by shapiro it is not normally ditributed.

### 3.Residuals are independent###############


#The Durbin-Watson test is used in time-series analysis to test if there is a trend in the data based on previous instances 
#e.g. a seasonal trend or a trend every other data point.

#Using the lmtest library, we can call the "dwtest" function on the model to check if the residuals are independent of one another.

#The Null hypothesis of the Durbin-Watson test is that the errors are serially UNcorrelated.
install.packages("lmtest")
library(lmtest)

dwtest(model1_pop_beats) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
#Results: DW = 1.8429, p-value = 0.2948

#thus the 2 values are correlated amongst residuals



###################4.Variance of errors is constant (Homoscedastic)

plot(model1_pop_beats$fitted.values, res_model1)


#since the residuals for the model donot follow any pattern,The errors are distributed with a constant variance 




###########################log model 2################################################

##Spotify_csv1 <- Spotify_csv %>% mutate(ln_Popularity = log(Popularity)) 


model2_beats_per <- lm(ln_Popularity ~ beats_per_minute, Spotify_csv)
summary(model2_beats_per)

ggplot(Spotify_csv, aes(x=beats_per_minute, y=ln_Popularity)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

res_model2 <- model2_beats_per$residuals

shapiro.test(res_model2)  #p-value = 2.103e-05  and thus normally ditributed

jarqueberaTest   # P VALUE:Asymptotic p Value: < 2.2e-16 



#beats_per_min x Residuals Plot
plot(res_model2~ab$Spotify_csv.beats_per_minute[order(ab$Spotify_csv.beats_per_minute)],
     main="beats_per_min x Residuals\nfor Simple Regression",
     xlab="beats_per_min", ylab="Residuals")
abline(h=0,lty=2)

#Histogram of Residuals
hist(res_model2, main="Histogram of Residuals",
     ylab="Residuals")

res_hist <- ggplot(data.frame(res_model2)) + geom_histogram(aes(res_model2)) 
res_hist

#If the histogram looks like a bell-curve it might be normally distributed.

#Q-Q Plot
qqnorm(res_model2)
qqline(res_model2)

?qqnorm


############the residual value remains the same and the model has improved



##############################model performance measures#############################################

#Spotify_csv = subset(Spotify_csv, select = -c(Popularity ))

set.seed(123)

train_idx <- sample(nrow(Spotify_csv),.7*nrow(Spotify_csv))
Spotify_csv_train <- Spotify_csv[train_idx,]
Spotify_csv_test <- Spotify_csv[-train_idx,]

str(Spotify_csv_train)


str(Spotify_csv_test)


can_mod1_beats_per_min <- lm(Popularity~beats_per_minute+Speechiness,Spotify_csv_train)

summary(can_mod1_beats_per_min)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-18.0433  -1.0640   0.6207   2.2430   6.5916 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      83.92684    3.02860  27.711   <2e-16 ***
#  beats_per_minute  0.03320    0.02439   1.361    0.183    
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.535 on 33 degrees of freedom
#Multiple R-squared:  0.05314,	Adjusted R-squared:  0.02445 
#F-statistic: 1.852 on 1 and 33 DF,  p-value: 0.1828


can_mod1_beats_per_min <- lm(Popularity~beats_per_minute,Spotify_csv_train)
summary(can_mod1_beats_per_min)
mod1_1_OOS_predict <- predict(can_mod1_beats_per_min,Spotify_csv_test)

str(mod1_1_OOS_predict)

  view(mod1_1_OOS_predict)


can_mod1_beats_per_min_speechiness_5 <- lm(Popularity~beats_per_minute+Acousticness+Speechiness+Valence+Liveness,Spotify_csv_train)

summary(can_mod1_beats_per_min_speechiness_5)

can_mod1_beats_per_min_speechiness_5 <- lm(Popularity~beats_per_minute+Acousticness+Speechiness+Valence+Liveness,Spotify_csv_train)

summary(can_mod1_beats_per_min_speechiness_5)


mod1_5_OOS_predict <- predict(can_mod1_beats_per_min_speechiness_5,Spotify_csv_test)

str(mod1_5_OOS_predict)


############FINAL step of model performance calculations#########################


#rootmean squared error
#Mean Absolute Error
#Mean Absolute Percentage error

calc_performance <- function(actual, pred) { 
  rmse <- sqrt(mean((actual - pred)**2)) 
  mae <- mean(abs(actual - pred)) 
  mape <- mean(abs((actual-pred)/actual)) 
  retvals <- list(rmse = rmse, mae = mae, mape = mape) 
  return(retvals) 
}
calc_performance(Spotify_csv_test$Popularity,mod1_1_OOS_predict)

#$rmse
#[1] 4.331056

#$mae
#[1] 3.666521

#$mape
#[1] 0.04344837

calc_performance(Spotify_csv_test$Popularity,mod1_5_OOS_predict)

#$rmse
#[1] 5.789122

#$mae
#[1] 4.851109

#$mape
#[1] 0.05631393

rmarkdown::render("C:/Users/bhawe/Downloads/ABC.Rmd")


