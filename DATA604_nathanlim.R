data1 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_1995_2000.csv")
data2 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2001_2005.csv")
data3 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2006_2010.csv")
data4 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2011_2015.csv")

data_19952015 <- rbind(data1, data2, data3, data4)
data_20062015 <- rbind(data3, data4)
data_2016 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2016_Oct16.csv")
str(data_2016)


# Clean data set

library(dplyr)
library(tidyr)
library(ggplot2)

data_train = data_20062015 %>%
            select(EST, Max.TemperatureF,  Mean.TemperatureF, Min.TemperatureF) %>%
            separate(EST, c("yyyy", "mm", "dd"), "-", remove = FALSE)
            
colnames(data_train) <- c("date","yyyy", "mm", "dd","max_temp","mean_temp","min_temp")
data_train$date <- as.Date(data_train$date)
data_train$yyyy <- as.numeric(data_train$yyyy)
data_train$mm <- as.numeric(data_train$mm)
data_train$dd <- as.numeric(data_train$dd)
data_train<- transform(data_train,
                DayOfYear = as.numeric(format(date, '%j')))


data_test = data_2016 %>%
  select(EDT, Max.TemperatureF,  Mean.TemperatureF, Min.TemperatureF) %>%
  separate(EDT, c("yyyy", "mm", "dd"), "-", remove = FALSE)

colnames(data_test) <- c("date","yyyy", "mm", "dd","max_temp","mean_temp","min_temp")
data_test$date <- as.Date(data_test$date)
data_test$yyyy <- as.numeric(data_test$yyyy)
data_test$mm <- as.numeric(data_test$mm)
data_test$dd <- as.numeric(data_test$dd)
str(data_test)

# Check missing value
data_train[!complete.cases(data_train),] #None
data_test[!complete.cases(data_test),] #None


str(data_train)
ggplot(data_train,aes(x=DayOfYear, y=mean_temp))+geom_line()+facet_grid(yyyy ~ .)



# Do Monte Carlo simulation
#How to build matrix??  Markov Chain

# Get yesterday's temp

for(i in 1:length(data_train$mean_temp)){
  data_train$tmr_temp[i] = data_train$mean_temp[i+1]
}


# Prob matrix
temp_df = data_train %>%
            select(mean_temp, tmr_temp)

temp_matrix=as.matrix(table(temp_df))

head(temp_matrix)

n = length(temp_matrix[1,])
prob_matrix <- matrix(data=NA, nrow=n, ncol=n)

for (i in 1:n){
  for (j in 1:n){
  prob_matrix[i,j] <- temp_matrix[i,j]/sum(temp_matrix[i,])
  }
}

head(prob_matrix)

#degrees are between 12 and 91 total 80 s

# Monte Carlo

predict_function <- function(data_set, probMatrix=prob_matrix){
  data_predict = data.frame(actual=data_set$mean_temp, predicted=NA)
  for (i in 2:nrow(data_predict)){
    temp_yesterday <- data_predict$actual[i-1]
    temp_vec = numeric(length=80)
    temp_vec[temp_yesterday-11] <- 1
    Prob=temp_vec %*% prob_matrix
    next_date_temp=sample(c(1:80), size=1, prob=Prob)
    data_predict$predicted[i] <- next_date_temp+11
  }

  df = cbind(Dayofyear=seq(1:nrow(data_set)), data_predict)
  return(df)
}

#Function for prediction loop
loop_function <- function(data_set, n){
  result = data.frame(Dayofyear=seq(1:nrow(data_set)), actual=data_set$mean_temp)
    for (i in 1:n){
      predicted_temp = predict_function(data_set)[3]
      colnames(predicted_temp) = paste("predicted",i, sep="_")
      result = cbind(result, predicted_temp)
    }
  # Add mean of predicted Temp
    result$mean_predicted <- NA
    for (i in 2:nrow(result)){
      result$mean_predicted[i] = sum(result[i,3:n+2])/n #except Dayofyear and actual temp
    }
    return(result)
}

# n = 100, 1000, 10000, 100000
DF100 <- loop_function(data_test, 100)
DF1000 <- loop_function(data_test, 1000)
DF10000 <- loop_function(data_test, 10000)
DF100000 <- loop_function(data_test, 100000)

#Get Predicted_DF of each means
Predicted_DF <- data.frame(DF100[,1:2])
Predicted_DF$predicted_100 <- DF100$mean_predicted
Predicted_DF$predicted_1000 <- DF1000$mean_predicted
Predicted_DF$predicted_10000 <- DF10000$mean_predicted
Predicted_DF$predicted_100000 <- DF100000$mean_predicted

head(Predicted_DF)
qplot(Predicted_DF$actual, Predicted_DF$predicted_100)
qplot(Predicted_DF$actual, Predicted_DF$predicted_1000)
qplot(Predicted_DF$actual, Predicted_DF$predicted_10000)
qplot(Predicted_DF$actual, Predicted_DF$predicted_100000)


library(reshape2)
df_for_plot <- melt(Predicted_DF, id.vars='Dayofyear')
ggplot(df_for_plot, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6)
head(df_for_plot)

df_part=df_for_plot[which((df_for_plot$Dayofyear>100)&(df_for_plot$Dayofyear<120)),]
ggplot(df_part, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6) +geom_point()




#Find Abnormal year among last 5 years
#10000 loop is enough to predict tmr temp

data_2011 <- data_train[which(data_train$yyyy==2011),]
data_2012 <- data_train[which(data_train$yyyy==2012),]
data_2013 <- data_train[which(data_train$yyyy==2013),]
data_2014 <- data_train[which(data_train$yyyy==2014),]
data_2015 <- data_train[which(data_train$yyyy==2015),]

DF_2011 <- loop_function(data_2011, 10000)
DF_2012 <- loop_function(data_2012, 10000)
DF_2013 <- loop_function(data_2013, 10000)
DF_2014 <- loop_function(data_2014, 10000)
DF_2015 <- loop_function(data_2015, 10000)


DF_5year <- data.frame(Dayofyear=DF_2011[,1])

DF_5year$actual2011 <- DF_2011$actual[1:365]
DF_5year$predicted_2011 <- DF_2011$mean_predicted[1:365]
DF_5year$diff2011 <- sqrt((DF_5year$actual2011-DF_5year$predicted_2011)^2)

DF_5year$actual2012 <- DF_2012$actual[1:365]
DF_5year$predicted_2012 <- DF_2012$mean_predicted[1:365]
DF_5year$diff2012 <- sqrt((DF_5year$actual2012-DF_5year$predicted_2012)^2)

DF_5year$actual2013 <- DF_2013$actual[1:365]
DF_5year$predicted_2013 <- DF_2013$mean_predicted[1:365]
DF_5year$diff2013 <- sqrt((DF_5year$actual2013-DF_5year$predicted_2013)^2)

DF_5year$actual2014 <- DF_2014$actual[1:365]
DF_5year$predicted_2014 <- DF_2014$mean_predicted[1:365]
DF_5year$diff2014 <- sqrt((DF_5year$actual2014-DF_5year$predicted_2014)^2)

DF_5year$actual2015 <- DF_2015$actual[1:365]
DF_5year$predicted_2015 <- DF_2015$mean_predicted[1:365]
DF_5year$diff2015 <- sqrt((DF_5year$actual2011-DF_5year$predicted_2015)^2)

head(DF_5year)

library(gridExtra)
library(knitr)
p1=qplot(DF_5year$diff2011) + geom_histogram(bins=20) + xlim(0, 30)
p2=qplot(DF_5year$diff2012) + geom_histogram(bins=20) + xlim(0, 30)
p3=qplot(DF_5year$diff2013) + geom_histogram(bins=20) + xlim(0, 30)
p4=qplot(DF_5year$diff2014) + geom_histogram(bins=20) + xlim(0, 30)
p5=qplot(DF_5year$diff2015) + geom_histogram(bins=20) + xlim(0, 30)

grid.arrange(p1, p2, p3, p4, p5, ncol=3)

#Sum of errors
(c(
SumDiff2011=sum(DF_5year$diff2011, na.rm=TRUE),
SumDiff2012=sum(DF_5year$diff2012, na.rm=TRUE),
SumDiff2013=sum(DF_5year$diff2013, na.rm=TRUE),
SumDiff2014=sum(DF_5year$diff2014, na.rm=TRUE),
SumDiff2015=sum(DF_5year$diff2015, na.rm=TRUE)
)
)

#2015 looks like unpredictable

# Kolmogorov-Smirnov compare two distributions

#H0: the two distributions are not different 
#Ha: the two distributions are different 

head(DF_5year)

ks_df <- data.frame(Row.names=c(
  "compare20112012","compare20112013",
  "compare20112014","compare20112015",
  "compare20122013","compare20122014",
  "compare20122015","compare20132014",
  "compare20132015","compare20142015"),
  ks_pvalue=
c(
ks.test(DF_5year$diff2011, DF_5year$diff2012)[[2]],
ks.test(DF_5year$diff2011, DF_5year$diff2013)[[2]],
ks.test(DF_5year$diff2011, DF_5year$diff2014)[[2]],
ks.test(DF_5year$diff2011, DF_5year$diff2015)[[2]],
ks.test(DF_5year$diff2012, DF_5year$diff2013)[[2]],
ks.test(DF_5year$diff2012, DF_5year$diff2014)[[2]],
ks.test(DF_5year$diff2012, DF_5year$diff2015)[[2]],
ks.test(DF_5year$diff2013, DF_5year$diff2014)[[2]],
ks.test(DF_5year$diff2013, DF_5year$diff2015)[[2]],
ks.test(DF_5year$diff2014, DF_5year$diff2015)[[2]]
)
)
ks_df$under0.05 <- ks_df$ks_pvalue<0.05
ks_df

# 2015's p_value are all under 0.05. we reject the null hyperthesis and accept the alternative.
# 2015's distribution is different from others

df_compare2012 = DF_5year %>%
          select(Dayofyear, actual2012, predicted_2012)
df_compare2015 = DF_5year %>%
          select(Dayofyear, actual2015, predicted_2015)

df_compare_for_plot2012 <- melt(df_compare2012, id.vars='Dayofyear')
df_compare_for_plot2015 <- melt(df_compare2015, id.vars='Dayofyear')


g1=ggplot(df_compare_for_plot2012, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6) + geom_point(alpha=0.3)
g2=ggplot(df_compare_for_plot2015, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6) + geom_point(alpha=0.3)
grid.arrange(g1, g2, ncol=1)


df_part2012=df_compare_for_plot2012[which((df_compare_for_plot2012$Dayofyear>140)&(df_compare_for_plot2012$Dayofyear<160)),]
df_part2015=df_compare_for_plot2015[which((df_compare_for_plot2015$Dayofyear>140)&(df_compare_for_plot2015$Dayofyear<160)),]

g3=ggplot(df_part2012, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6) + geom_point(alpha=0.3)
g4=ggplot(df_part2015, aes(Dayofyear, value, color=variable)) + geom_line(alpha=0.6) + geom_point(alpha=0.3)
grid.arrange(g3, g4, ncol=1)


#We can tell mean_temparature of 2015 were abnormally unpredictable year.
