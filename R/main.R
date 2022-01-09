# Algorithm: ONI index modelling and forecasting
# Author: Jair Paulino
# Date: January 06 2021 

# Clear R environment
rm(list=ls()); nNodes=10

# Libraries
library(forecast)
library(keras)
library(tensorflow)
library(GenSA)
library(caret)

# Import functions
source("R/web_scraping.R")
source("R/Opt_LSTM_GenSA.R")
source("R/performanceMetrics.R")

# 01: Web scraping ----
oni_data = web_scraping()

# 02: data analysis ----
# ONI boxplot
boxplot(oni_data$oni_data[,(-1)], ylab="ONI index", xlab="Média trimestral")

# Oni time series
plot.ts(oni_data$oni_ts, lwd=1.5, xlab="Observações", ylab="ONI index")
abline(h=c(0.5), col=2, lty=3, lwd=3)
abline(h=c(-0.5), col=4, lty=3, lwd=3)
#abline(h=0, col=4, lty=3, lwd=2)
#abline(h=c(1,-1), col=4, lty=3, lwd=2)
#abline(h=c(2,-2), col=2, lty=3, lwd=2)

oni_ts = na.omit(oni_data$oni_ts); plot.ts(oni_ts)
ts_decompose = decompose(ts(oni_ts, frequency = 12, start = 1950.3))
plot(ts_decompose)

# 03: Pre-processing ----
# Split
N = round(.7*length(oni_ts))
train_df = c(target = oni_ts[1:N]); plot.ts(train_df)
test_df = c(target = oni_ts[(N+1):length(oni_ts)]); plot.ts(test_df)

# Creating sliding windows dataframe and setting LSTM parameters
nnParameters = list(); 
nnParameters$pAR = 24
nnParameters$pARS = 0
nnParameters$seasonality = 0
nnParameters$nNodes = nNodes
nnParameters$acfFuncion = "relu"
nnParameters$optimizer = "rmsprop"
nnParameters$epochs = 30
nnParameters$batch_size = 1

data_train_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                        , series=train_df); #View(data_train_series_df)

covariates = data_train_series_df$covariates
target = data_train_series_df$target

# Modelling ----
my_seed = 42
begin = proc.time()
model = keras_model_sequential()
model %>%
  layer_lstm(units = nnParameters$nNodes
             , input_shape = dim(covariates)[-1] 
             , recurrent_activation = nnParameters$acfFuncion
             , kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = my_seed)) %>%
  layer_dense(units = 1, activation = 'linear'
              , kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = my_seed))

model %>% 
  compile(loss = 'mse', optimizer = nnParameters$optimizer, metrics = 'mae')

early_stopping = callback_early_stopping(monitor = 'val_loss', patience = 10)
history = model %>% 
  fit(x = covariates, y = target #, callbacks = c(early_stopping)
      , epochs = nnParameters$epochs
      , shuffle = FALSE
      , batch_size = nnParameters$batch_size
      , validation_split = 0.25
      , verbose = TRUE)
end = proc.time() - begin; end

pred_norm = predict(model, covariates)
mse_train = getMSE(pred_norm, target) 
mape_train = getMAPE(pred_norm, target)
mae_train= getMAE(pred_norm, target)
theil_train = getTheil(pred_norm, target)

# Test ----
data_test_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                         , series=test_df); #View(data_train_series_df)

covariates_test = data_test_series_df$covariates
target_test = data_test_series_df$target

begin_test = proc.time()
pred_norm_test = predict(model, covariates_test)
mse_test = getMSE(pred_norm_test, target_test); mse_test
end_test = proc.time() - begin_test

mse_test = getMSE(pred_norm_test, target_test) 
mape_test = getMAPE(pred_norm_test, target_test)
mae_test = getMAE(pred_norm_test, target_test)
theil_test = getTheil(pred_norm_test, target_test)

resultsMetrics = data.frame(matrix(nrow=2, ncol=5))
colnames(resultsMetrics) = c('mse', 'mape', 'mae', 'theil', 'time')
rownames(resultsMetrics) = c('train', 'test')
resultsMetrics$mse[1] = mse_train
resultsMetrics$mape[1] = mape_train
resultsMetrics$mae[1] = mae_train
resultsMetrics$theil[1] = theil_train
resultsMetrics$mse[2] = mse_test
resultsMetrics$mape[2] = mape_test
resultsMetrics$mae[2] = mae_test
resultsMetrics$theil[2] = theil_test
resultsMetrics$time[1] = end[3]
resultsMetrics$time[2] = end_test[3]

write.csv(round(resultsMetrics,4)
          , paste("Results/resultsMetrics_p24_n=", nnParameters$nNodes ,".csv", sep=""))

# Saving results 
resultsTrain = data.frame(matrix(ncol=2, nrow=length((pred_norm))))
colnames(resultsTrain) = c('forecast', 'target')
resultsTrain$forecast = pred_norm
resultsTrain$target = target
write.csv(resultsTrain, row.names = F
          , paste("Results/resultsTain_n=", nnParameters$nNodes ,".csv", sep=""))

resultsTest = data.frame(matrix(ncol=2, nrow=length((pred_norm_test))))
colnames(resultsTest) = c('forecast', 'target')
resultsTest$forecast = pred_norm_test
resultsTest$target = target_test
write.csv(resultsTest, row.names = F
          , paste("Results/resultsTest_n=", nnParameters$nNodes ,".csv", sep=""))

#Train
minT = min(min(target),min(pred_norm))
maxT = max(max(target),max(pred_norm))*1.4
png(paste('Results/Figure/Resultado_train_A2_n=', nnParameters$nNodes,'.png', sep="")
    , width = 800, height = 400, res = 100)
plot.ts(target, xlab="Observações (conjunto de treinamento)"
        , ylab='ONI Index', ylim=c(minT, maxT), lwd=2)
lines(pred_norm, lwd=3, col=2, lty=1)
points(pred_norm, pch=19, col=2, lty=1)
legend("top", legend = c("Série temporal", "Previsão (LSTM)")
       , col = c(1,2), lty=c(1,1), pch=c(NA,19), horiz = T
       , lwd=c(2,2), box.col = "white", inset = 0.01)
dev.off()
#Test
minT = min(min(target_test),min(pred_norm_test))
maxT = max(max(target_test),max(pred_norm_test))*1.5
png(paste('Results/Figure/Resultado_test_A2_n=', nnParameters$nNodes,'.png', sep="")
    , width = 800, height = 400, res = 100)
plot.ts(target_test, xlab="Observações (conjunto de teste)"
        , ylab='ONI Index', ylim=c(minT, maxT), lwd=2)
lines(pred_norm_test, lwd=3, col=2, lty=1)
points(pred_norm_test, pch=19, col=2, lty=1)
legend("top", legend = c("Série temporal", "Previsão (LSTM)")
       , col = c(1,2), lty=c(1,1), pch=c(NA,19), horiz = T
       , lwd=c(2,2), box.col = "white", inset = 0.01)
dev.off()
#topright

resultsMetrics
