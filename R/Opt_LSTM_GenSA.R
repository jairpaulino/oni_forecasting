#loadPackages(c("GenSA", "caret"))
LSTM_actFunctions = c("relu", "sigmoid", "linear", "softmax")

getTsTfANN_Matrix = function(nnParameters, series){
  # nnParameters = list()
  # nnParameters$pAR = 5
  # nnParameters$seasonality = 12
  # nnParameters$pARS = 2
  # series = norm_train_df$target
  seasonalityRange = nnParameters$seasonality*nnParameters$pARS
  initialIndex = seasonalityRange+1
  loss = seasonalityRange
  if(loss==0) {
    loss = nnParameters$pAR
    initialIndex = nnParameters$pAR+1
  }
  ncols = (1+nnParameters$pAR+nnParameters$pARS)
  seriesSize = length(series)
  nrows = (seriesSize-loss)
  if(nrows <=0){
    return (NULL)
  }
  else{
    TsTfANN.matrix = as.data.frame(matrix(nrow= nrows, ncol= 0))
    for(i in 0:nnParameters$pAR){
      colName_i = paste("ut_", i, sep="")
      TsTfANN.matrix[[colName_i]] = series[(initialIndex-i):(seriesSize-i)]
    }
    #View(TsTfANN.matrix)
    if(nnParameters$pARS>0){
      for(i in 1:nnParameters$pARS){
        colName_i = paste("ut_", (i*nnParameters$seasonality), sep="")
        TsTfANN.matrix[[colName_i]] = series[(initialIndex-i*nnParameters$seasonality):(seriesSize-i*nnParameters$seasonality)]
      }
    }
    #View(TsTfANN.matrix)
    target = as.matrix(TsTfANN.matrix$ut_0)
    covariates = as.matrix(TsTfANN.matrix[-1])
    dim(covariates) = c(dim(covariates), 1)
    ret = list(target = target, covariates = covariates)
    return(ret)
  }
}

getOptimal_LSTM_GenSA = function(data){
  #data = incDia

  fitness_LSTM = function(parameters){
    
    #parameters = c(2, 1, 14, 15, 4)

    nnParameters = list(); 
    nnParameters$pAR = floor(parameters[1])
    nnParameters$pARS = floor(parameters[2])
    nnParameters$seasonality = floor(parameters[3])
    nnParameters$nNodes = floor(parameters[4])
    nnParameters$acfFuncion = floor(parameters[5])
    actFunction = LSTM_actFunctions[nnParameters$acfFuncion]
    
    nnParameters$optimizer = "rmsprop"
    nnParameters$epochs = 50
    nnParameters$batch_size = 1
    
    series = norm_train_df$target; #plot.ts(series)
    
    datatrain_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                            , series=series)
    
    covariates = datatrain_series_df$covariates
    target = datatrain_series_df$target
    
    model = keras_model_sequential()
    model %>%
      layer_lstm(units = nnParameters$nNodes
               , input_shape = dim(covariates)[-1] 
               , recurrent_activation = actFunction) %>%
      layer_dense(units = 1, activation = 'linear')
    
    model %>% 
      compile(loss = 'mse', optimizer = nnParameters$optimizer, metrics = 'mae')
    
    early_stopping = callback_early_stopping(monitor = 'val_loss'
                                             , patience = 5, mode = "min")
    
    history = model %>% 
      fit(x = covariates, y = target, callbacks = c(early_stopping)
          , epochs = 30#nnParameters$epochs
          , shuffle = FALSE
          , batch_size = nnParameters$batch_size
          , validation_split = 0.25
          , verbose = TRUE)
    
    pred_norm = predict(model, covariates)
    mse = getMSE(pred_norm, target)
    plot.ts(target)
    lines(pred_norm, lwd=2, col=2)
    #length(target); length(pred_norm)
    return(mse)
  }
  
  #data = incDia
  
  # Split
  N = round(.75*length(data$target))
  train_df = data.frame(target = data$target[1:N])
  test_df = data.frame(target = data$target[(N+1):length(data$target)])
  
  # Scaling 
  mean_scaling_f = mean(train_df[['target']])
  sd_scaling_f = sd(train_df[['target']])
  norm_train_df <<- (train_df-mean_scaling_f)/sd_scaling_f; plot.ts(norm_train_df)
  norm_test_df = (test_df-mean_scaling_f)/sd_scaling_f; plot.ts(norm_test_df)
  #norm_train_test_df = (train_test_df-mean_scaling)/sd_scaling
  # normParam = preProcess(train_df, method = "scale")
  # norm_train_df = predict(normParam, train_df) 
  # norm_test_df = predict(normParam, test_df)
  
  # GenSA
  set.seed(1234) # The user can use any seed.
  pAR_min = 1; pARS_min = 0; seasonality_min = 12; 
  nNodes_min = 10; acfFunc_min = 1
  
  pAR_max = 10+1-1e-10; pARS_max = 10+1-1e-10; seasonality_max = 20+1-1e-10;
  nNodes_max = 50+1-1e-10; acfFunc_max= 1+1-1e-10
  
  lower = c(pAR_min, pARS_min, seasonality_min, nNodes_min, acfFunc_min)
  upper = c(pAR_max, pARS_max, seasonality_max, nNodes_max, acfFunc_max)
  
  begin = proc.time()[[3]]
  out = GenSA(lower = lower
              , upper = upper
              , fn = fitness_LSTM
              , control = list(max.call = 1000
                               , max.time = 300
                               , maxit = 1000
                               , verbose = TRUE
                               , smooth = FALSE
                               , seed = 1234
                               , nb.stop.improvement = 3
                               , temperature = 1000))
  end_proc = proc.time()[[3]] - begin
  
  
  values = out[c("par")]

  ret = list()
  ret$values = data.frame('pAR' = floor(values$par[1])
                          , 'pARS' =  floor(values$par[2])
                          , 'S' = floor(values$par[3])
                          , 'nNodes' = floor(values$par[4])
                          , 'actFunction' = floor(values$par[5]))
  ret$time = end_proc
  #out[c("value","par","counts")]
  #end_proc
  return(ret)
}

get_LSTM_model = function(data, opt_parameters){

  #opt_parameters = c(10,	8,	19,	5,	3); data = incDia

  # Split
  m = round(.75*length(data$target))
  n = length(data$target)
  m_n = n - m

  train_df = data.frame(target = data$target[1:m])
  test_df = data.frame(target = data$target[(m+1):length(data$target)])
  train_test_df = rbind(train_df, test_df)

  # Scaling
  mean_scaling = mean(train_df[['target']])
  sd_scaling = sd(train_df[['target']])
  norm_train_df = (train_df-mean_scaling)/sd_scaling
  norm_test_df = (test_df-mean_scaling)/sd_scaling
  norm_train_test_df = (train_test_df-mean_scaling)/sd_scaling
  #dataNorme = (data_t-m)/d
  #dataDesnorme = dataNorme*d + m
  
  #normParam = preProcess(train_df, method = "scale")
  #norm_train_df = predict(normParam, train_df)
  #norm_test_df = predict(normParam, test_df)
  #norm_train_test = predict(normParam, train_test)

  #plot.ts(norm_train_test)

  nnParameters = list();
  nnParameters$pAR = floor(opt_parameters[1])
  nnParameters$pARS = floor(opt_parameters[2])
  nnParameters$seasonality = floor(opt_parameters[3])
  nnParameters$nNodes = floor(opt_parameters[4])
  nnParameters$acfFuncion = floor(opt_parameters[5])
  actFunction = LSTM_actFunctions[nnParameters$acfFuncion]
  
  nnParameters$optimizer = "rmsprop"
  nnParameters$epochs = 30
  nnParameters$batch_size = 1

  length(norm_train_df[['target']])
  datatrain_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                          , series=norm_train_df[['target']])

  covariates = datatrain_series_df$covariates
  target = datatrain_series_df$target
  my_seed = 2311
  
  model = keras_model_sequential()
  model %>%
    layer_lstm(units = nnParameters$nNodes
             , input_shape = dim(covariates)[-1]
             , recurrent_activation = actFunction
             , kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = my_seed)) %>% #actFunction) %>%
    layer_dense(units = 1, activation = 'linear'
                , kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = my_seed))

  model %>% compile(loss = 'mse', optimizer = nnParameters$optimizer, metrics = 'mae')
  early_stopping = callback_early_stopping(monitor = 'val_loss', patience = 5)

  history = model %>%
    fit(x = covariates, y = target, callbacks = c(early_stopping)
        , epochs = nnParameters$epochs
        , shuffle = FALSE
        , batch_size = nnParameters$batch_size
        , validation_split = 0.25
        , verbose = TRUE
        , )

  pred_norm = predict(model, covariates)
  plot.ts(pred_norm) #length(target); length(pred_norm)
  pred_desnorm = pred_norm*sd_scaling + mean_scaling
  target_desnorm = target*sd_scaling + mean_scaling

  plot.ts(target); lines(pred_norm, col=2)
  plot.ts(target_desnorm); lines(pred_desnorm, col = 2)
  
  matrix_train_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                    , series=norm_train_df[['target']])
  
  matrix_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                     , series=norm_train_test_df[['target']])

  len_train = length(matrix_train_df[['target']])
  len_all = length(matrix_series_df[['target']])

  pred_norm = predict(model, matrix_train_df$covariates)
  newseries = c(norm_train_df$target, pred_norm[len_train])
  
  length(norm_train_df$target); length(newseries)
  loss = nnParameters$pARS*nnParameters$seasonality
  
  for(i in (m+2):(len_all+loss)){#i=(m+2)
    pred_new_df = getTsTfANN_Matrix(nnParameters=nnParameters
                                      , series=newseries)
    
    pred_new = predict(model, pred_new_df$covariates)
    len_new = length(pred_new)
    newseries[i] = pred_new[len_new]
    
  }
  
  # pred train + test
  pred_train_test = c(pred_norm, newseries[(m):(len_all+loss)])
  
  min_graph = min(min(matrix_series_df$target)
                  , min(newseries[(loss+1):(len_all+loss)]))
  max_graph = max(max(matrix_series_df$target)
                 , max(newseries[(loss+1):(len_all+loss)]))
  
  plot.ts(matrix_series_df$target, ylim=c(min_graph, max_graph))
  #lines(newseries[(loss+1):(len_all+loss)], col=2)
  lines(pred_train_test, col=2)
  abline(v=(len_all-m_n), col=4, lty=3, lwd=2)
  length(matrix_series_df$target); length(newseries)-loss

  target_train_test_denormalize = matrix_series_df$target*sd_scaling + mean_scaling
  pred_train_test_denormalize = pred_train_test*sd_scaling + mean_scaling
  
  len = length(target_train_test_denormalize)
  len_pred = length(pred_train_test_denormalize)
  
  for(i in 1:len){
    if(pred_train_test_denormalize[i] < 0){
      pred_train_test_denormalize[i] = 0
    }
  }
  
  options(scipen = 999)
  min_graph = min(min(target_train_test_denormalize)
                  , min(pred_train_test_denormalize))
  max_graph = max(max(target_train_test_denormalize)
                  , max(pred_train_test_denormalize))
  plot(target_train_test_denormalize, ylim=c(min_graph, max_graph)
       , type="l", ylab = country_label)
  #lines(newseries[(loss+1):(len_all+loss)], col=2)
  lines(pred_train_test_denormalize, col=2, lwd=2)
  abline(v=(len_all-m_n), col=4, lty=3, lwd=2)
  length(matrix_series_df$target); length(newseries)-loss
 
   
  rmse = getRMSE(target = target_train_test_denormalize[(len-m_n):len]
                 , forecast = pred_train_test_denormalize[(len-m_n):len])
  mape = getMAPE(target = target_train_test_denormalize[(len-m_n):len]
                 , forecast = pred_train_test_denormalize[(len-m_n):len])
  mae = getMAE(target = target_train_test_denormalize[(len-m_n):len]
               , forecast = pred_train_test_denormalize[(len-m_n):len])
  
  rmse; mape; mae
  
  data_csv = data.frame(matrix(ncol=2, nrow=len))
  names(data_csv) = c('target', 'lstm')
  data_csv$target = target_train_test_denormalize
  data_csv$lstm = pred_train_test_denormalize[1:(len_pred-1)]
  #View(data_csv)
  write.csv(data_csv, paste('Results/', country_label, '_lstm_result.csv', sep="")
            , row.names = F)
  
  #target_all = all_series_df$target
}



# # Split
# N = round(.75*length(incDia$target))
# train_df = data.frame(target = incDia$target[1:N])
# test_df = data.frame(target = incDia$target[(N+1):length(incDia$target)])
# 
# # Scaling 
# normParam = preProcess(train_df, method = "scale")
# norm_train_df = predict(normParam, train_df)
# norm_test_df = predict(normParam, test_df)
# 
# fitness_LSTM = function(parameters){
#   
#   #parameters = c(2, 1, 14, 15)
#   series = norm_train_df$target
#   
#   nnParameters = list(); 
#   nnParameters$pAR = floor(parameters[1])
#   nnParameters$pARS = floor(parameters[2])
#   nnParameters$seasonality = floor(parameters[3])
#   nnParameters$nNodes = floor(parameters[4])
#   
#   nnParameters$optimizer = "rmsprop"
#   nnParameters$epochs = 100
#   nnParameters$batch_size = 1
#   
#   datatrain_series_df = getTsTfANN_Matrix(nnParameters=nnParameters
#                                           , series=series)
#   
#   covariates = datatrain_series_df$covariates
#   target = datatrain_series_df$target
#   
#   model = keras_model_sequential()
#   layer_lstm(units = nnParameters$nNodes
#              , input_shape = dim(covariates)[-1] 
#              , recurrent_activation = 'sigmoid'
#   )
#   model %>%
#     layer_dense(units = 1, activation = 'linear')
#   
#   model %>% compile(loss = 'mse', optimizer = nnParameters$optimizer, metrics = 'mae')
#   early_stopping = callback_early_stopping(monitor = 'val_loss', patience = 5)
#   
#   history = model %>% 
#     fit(x = covariates, y = target, callbacks = c(early_stopping)
#         , epochs = nnParameters$epochs
#         , shuffle = FALSE
#         , batch_size = nnParameters$batch_size
#         , validation_split = 0.25
#         , verbose = TRUE)
#   
#   pred_norm = predict(model, target)
#   mse = getMSE(pred_norm, target)
#   return(mse)
# }

# nnParameters = list()
# nnParameters$pAR = 5; nnParameters$seasonality = 10
# nnParameters$pARS = 2; nnParameters$nNodes = 8
# nnParameters$optimizer = "rmsprop"; nnParameters$epochs = 100
# nnParameters$batch_size = 1; #series = norm_train_df
#fitness_LSTM(series=norm_train_df, nnParameters = nnParameters)


#pPropAR_ARS = round(length(norm_train_df$target)*.1)

# set.seed(1234) # The user can use any seed.
# pAR_min = 1; pARS_min = 0; seasonality_min = 12; nNodes_min = 1
# pAR_max = 10+1-1e-10; pARS_max = 10+1-1e-10; seasonality_max = 20+1-1e-10; nNodes_max = 20+1-1e-10
# 
# lower = c(pAR_min, pARS_min, seasonality_min, nNodes_min)
# upper = c(pAR_max, pARS_max, seasonality_max, nNodes_max)
# 
# begin = proc.time()[[3]]
# out = GenSA(lower = lower
#             , upper = upper
#             , fn = fitness_LSTM
#             , control = list(max.call = 10000
#                             , max.time = 300
#                             , maxit = 1000
#                             , verbose = TRUE
#                             , smooth = FALSE
#                             , seed = 1234
#                             , nb.stop.improvement = 5
#                             , temperature = 10000))
# end_proc = proc.time()[[3]] - begin
# 
# out[c("value","par","counts")]
# end_proc




