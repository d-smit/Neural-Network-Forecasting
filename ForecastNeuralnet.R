require(reshape2)
library(neuralnet)
library(DataCombine)
library(Hmisc)
library(TTR)
library(BBmisc)
library(forecast)
library(Metrics)
library(astsa)
library(ggplot2)
library(Ecdat)
options(scipen = 10, digits=4)
set.seed(123)

data <- scan('http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts/m-hsy6299.dat');data

# Raw data 

returns <- as.data.frame(data);returns
returns <- cbind(returns, Date=seq(as.Date("1977-2-1"), as.Date("2015-1-1"), by="day"));returns
dates <- returns$Date;dates
all.returns <- returns[,1];all.returns

ggplot() + geom_line(aes(x=dates, y=all.returns)) +
         labs(x='Time', y='Returns (%)') +
         ggtitle('Hershey Monthly Returns: Unprocessed') 
         ggsave('cw3_unprocessed_returns.pdf')

# Smoothed and partitioned data 

data <- SMA(data, n=5)
data <- na.omit(data)

data <- data[1:360];data   # 30 years 
data <- normalize(data, method='range', range=c(0,1))

returns <- as.data.frame(data);returns
returns <- cbind(returns, Date=seq(as.Date("1977-2-1"), as.Date("2007-1-1"), by="month"));returns
dates <- returns$Date;dates

ggplot() + geom_line(aes(x=dates, y=returns[,1])) +
           labs(x='Time', y='Returns (%)') +
           ggtitle('Hershey 30 Year Stock Performance: Smoothed (SMA 5-Days)')
           ggsave('cw3_processed_returns.pdf')
           

y_ts <- ts(data, f=12)

# Making a lagged matrix 

lagged_mat <- data.frame(y_ts, x1=Lag(y_ts,1), x2=Lag(y_ts,2), x3=Lag(y_ts,3),
                        x4=Lag(y_ts,4), x5=Lag(y_ts,5));lagged_mat

dat <- ts(lagged_mat)
dat <- na.omit(dat);dat

# Spliting lagged matrix into train and test sets 

colnames(dat) <- c('Output', 'Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5')

index <- round((2/3)*nrow(dat));index
train <- dat[1:index,];train
test <- dat[index:nrow(dat),];test

# Data for plotting 

tr.dates <- dates[1:index]
tr.prices <- returns[1:index, 1]
tt.dates <- dates[index:360];tt.dates
tt.prices <- returns[index:360, 1]   

holt_dat <- train[,1];holt_dat

# Plotting train/test split 

ggplot() + geom_line(aes(x=tt.dates, y=tt.prices, colour='Testing Data')) +
  geom_line(aes(x=tr.dates, y=tr.prices, colour='Training Data')) +
  labs(x='Time', y='Returns (%)') + 
  ggtitle('Hershey Monthly Returns: Training and Test Data') +
  theme(legend.position = c(0.85,0.15), legend.justification = c(0, 1)) +
  scale_color_manual(name=NULL, values = c('red', 'black'))
  ggsave('cw3_train_test_split.pdf')

# Decompose into seasonality, trend and random components

returns_t <- ts(returns$data, start = c(1977, 2, 1), end = c(2007, 1, 1), frequency = 12)
decomposed_dat <- decompose(returns_t, 'multiplicative');decomposed_dat
na.omit(decomposed_dat)
plot(decomposed_dat)

# Setting up neural net model 

nn <- neuralnet(Output ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5,
                         train, hidden = c(90,30,10),
                         stepmax=1e+06, threshold=0.01)

plot(nn)

# Training model on seen data 

nn$net.result
nn$result.matrix[1:3,]
trainpreds <- as.data.frame(nn$net.result);trainpreds
trainpreds <- trainpreds[1:213,1];trainpreds
trainpreds <- as.data.frame(trainpreds);trainpreds
colnames(trainpreds) <- c('Preds');trainpreds
trainlabels <- as.data.frame(train[1:213,1]);trainlabels
x <-ts(trainlabels)
y <-ts(trainpreds)
training <-cbind(x, y);training

mse(trainlabels, trainpreds)
rmse(trainlabels, trainpreds)
train_dates <- as.data.frame(tr.dates[1:213]);train_dates

ggplot() + geom_line(aes(x=train_dates$`tr.dates[1:213]`, y=trainpreds$Preds, colour='Predictions')) +
  geom_line(aes(x=train_dates$`tr.dates[1:213]`, y=trainlabels$`train[1:213, 1]`, colour='Actual Returns')) +
  labs(x='Time', y='Returns (%)') +
  ggtitle('Hershey Monthly Returns: Predicted vs Actual Training Data') +
  theme(legend.position = c(0.8,0.2), legend.justification = c(0, 1)) +
  scale_color_manual(name=NULL, values = c('dimgray', 'red'))
  ggsave('cw3_training_preds.pdf')

# Testing on unseen data 

pred <- predict(nn, test);pred
testlabels <- as.data.frame(test[,1]);testlabels
testpreds <- as.data.frame(pred);testpreds
testing <- cbind(testlabels, testpreds);testing

mae(testlabels$`test[, 1]`, testpreds$V1)

# Getting accuracies 

mae(testlabels$`test[, 1]`, testpreds$V1)
rmse(testlabels$`test[, 1]`, testpreds$V1)

test_dates <- as.data.frame(tt.dates[1:nrow(test)]);test_dates

# Predicted vs actual for test data 

ggplot() + geom_line(aes(x=test_dates$`tt.dates[1:nrow(test)]`, y=testpreds$V1, colour='Predictions')) +
           geom_line(aes(x=test_dates$`tt.dates[1:nrow(test)]`, y=testlabels$`test[, 1]`, colour='Actual Returns')) +
           labs(x='Time', y='Returns (%)') +
           ggtitle('Hershey Monthly Returns: Predicted vs Actual Test Data') +
           theme(legend.position = c(0.825,0.2), legend.justification = c(0, 1)) +
           scale_color_manual(name=NULL, values = c('dimgray', 'deepskyblue2'))
           ggsave('cw3_test_preds_norm.pdf')
           
# Comparisons 

ggplot() + geom_line(aes(x=tt.dates, y=tt.prices, colour='Testing Data')) +
          geom_line(aes(x=tr.dates, y=tr.prices, colour='Training Data')) +
          geom_line(aes(x=test_dates$`tt.dates[1:nrow(test)]`, y=testpreds$V1, colour='Predictions')) +
          labs(x='Time', y='Returns (%)') + 
          ggtitle('Hershey Monthly Returns: Train, Test and Predictions') +
          theme(legend.position = c(0.825,0.2),legend.justification = c(0, 1)) +
          scale_color_manual(name=NULL, values = c('deepskyblue','darkgray', 'dimgray'))
          ggsave('cw3_train_test_preds.pdf')
          
# Holt-Winters
          
holt_dat <- na.omit(holt_dat)
hw <- HoltWinters(ts(holt_dat, frequency = 12), beta=FALSE)#, alpha=NULL)#, gamma=FALSE)#, beta=FALSE)
hw_pred <- forecast(hw, 120)
plot(hw_pred, xlab='Time (years)', ylab='Returns (%)')

# Mean comparison

mean.pred <- ts(apply(dat[,2:6], 1, mean));mean.pred
means <- as.data.frame(mean.pred[231:354]);means

# Comparison of nn, mean on test data 

ggplot() + geom_line(aes(x=tt.dates, y=tt.prices, colour='Test Data')) +
           geom_line(aes(x=tt.dates, y=means$`mean.pred[231:354]`, colour='5-Day Moving Average')) +
           geom_line(aes(x=test_dates$`tt.dates[1:nrow(test)]`, y=testpreds$V1, colour='Neural Network')) +
           labs(x='Time', y='Returns (%)') + 
           ggtitle('Hershey Monthly Returns: Neural Network and Moving Average Predictions') +
           theme(legend.position = c(0.78,0.2),legend.justification = c(0, 1)) +
           scale_color_manual(name=NULL, values = c('green','dimgray', 'deepskyblue2'))
           ggsave('cw3_nn_mean_preds.pdf')
