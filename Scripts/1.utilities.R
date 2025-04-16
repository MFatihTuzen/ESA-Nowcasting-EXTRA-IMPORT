
# get star regressors ----

get_star_reg <- function(rebased_data,trade_weights){
  
  rebased_data |>
    left_join(trade_weights,
              by = c("country_code" = "trade_partner", "Year" = "year")) |>
    rename(trade_partner = country_code) |>
    pivot_longer(
      cols = -c(TIME_PERIOD, Year, trade_partner, index, country),
      names_to = "country_code",
      values_to = "weight"
    ) |>
    mutate(weighted_index = index * weight) |>
    group_by(TIME_PERIOD, country_code) |>
    summarise(total_weighted_index = sum(weighted_index)) |>
    pivot_wider(id_cols = TIME_PERIOD,
                names_from = country_code,
                values_from = total_weighted_index) |>
    select(Date = TIME_PERIOD , all_of(countries)) |>
    arrange(Date) |>
    mutate(Date = paste0(year(Date), "M", month(Date)))

}


# import data from folder ----
import_data <- function(indicator, period){
  
  file_path <- paste0("Data/",period,"/data.xlsx")  
  openxlsx::read.xlsx(file_path, sheet = indicator) |> 
    mutate(Date = parse_date(Date, format = "%YM%m")) |>
    pivot_longer(cols = -c("Date"),
                 names_to = "country",
                 values_to = "value") |>
    mutate(indicator = indicator )
  
}

# get month diff between two dates ----
month_diff <- function(date1, date2) {
  (year(date2) - year(date1)) * 12 + (month(date2) - month(date1))
}


# lstm functions ----

# align data according date of last obs date of any regressor

create_data_lag <- function(data){
  
  n_obs <- nrow(data)
  reg_columns <- colnames(data)[!colnames(data) %in% c("Date","target")]
  df_list <- list()
  
  for(regs in reg_columns){
    
    n_obs_reg <- data |> 
      select(Date,regs) |> 
      na.omit() |> 
      nrow()
    
    lag_val <- n_obs-n_obs_reg
    
    df <- data |> 
      select(Date,regs) |> 
      mutate(!!sym(regs) := lag(!!sym(regs),lag_val))
    df_list[[regs]] <- df
  }
  
  data_final <- reduce(df_list, full_join, by = "Date") |> 
    left_join(data[,c("Date","target")], by = c("Date" = "Date"))
  
  return(data_final)
  
}


# normalize data

normalize_data <- function(data, columns) {
  normalized_data <- data
  normalization_params <- list()
  
  for(col in columns) {
    min_val <- min(data[[col]], na.rm = TRUE)
    max_val <- max(data[[col]], na.rm = TRUE)
    normalized_data[[col]] <- (data[[col]] - min_val) / (max_val - min_val)
    normalization_params[[col]] <- list(min = min_val, max = max_val)
  }
  
  return(list(
    normalized_data = normalized_data,
    params = normalization_params
  ))
}



# Veri seti oluşturma fonksiyonunu düzeltme
create_multivariate_dataset <- function(data, target_col, feature_cols, lookback) {
  # NA olmayan target değerlerini al
  valid_indices <- which(!is.na(data[[target_col]]))
  
  if(length(valid_indices) == 0) {
    stop("No valid target values found in the data")
  }
  
  max_valid_index <- max(valid_indices)
  min_valid_index <- min(valid_indices)
  
  total_features <- length(feature_cols) + 1  # target + features
  
  # Geçerli veri noktası sayısını hesapla
  # lookback kadar geçmiş veri ve bir gelecek tahmin için gereken veri
  n_valid_samples <- length(valid_indices) - lookback
  
  if(n_valid_samples <= 0) {
    stop("Not enough valid samples for the given lookback period")
  }
  
  x <- array(NA, dim = c(n_valid_samples, lookback, total_features))
  y <- numeric(n_valid_samples)
  
  sample_index <- 1
  
  for(i in min_valid_index:(max_valid_index - lookback)) {
    # Hedef değişken ve feature'lar için pencere kontrolü
    window_indices <- i:(i + lookback)
    target_window <- data[[target_col]][window_indices]
    
    # Penceredeki tüm değerlerin geçerli olup olmadığını kontrol et
    if(!any(is.na(target_window))) {
      # Feature'ları kontrol et
      feature_valid <- TRUE
      for(feat in feature_cols) {
        if(any(is.na(data[[feat]][window_indices]))) {
          feature_valid <- FALSE
          break
        }
      }
      
      if(feature_valid) {
        # Target değişkenini ekleme
        x[sample_index,,1] <- target_window[1:lookback]
        
        # Feature'ları ekleme
        for(j in seq_along(feature_cols)) {
          x[sample_index,,j+1] <- data[[feature_cols[j]]][window_indices[1:lookback]]
        }
        
        # Hedef değeri ekleme (bir sonraki zaman adımı)
        y[sample_index] <- data[[target_col]][i + lookback]
        
        sample_index <- sample_index + 1
      }
    }
  }
  
  # Kullanılmayan boş alanları kaldır
  if(sample_index - 1 < n_valid_samples) {
    x <- x[1:(sample_index-1),,, drop = FALSE]
    y <- y[1:(sample_index-1)]
  }
  
  return(list(
    x = x,
    y = array(y, dim = c(length(y), 1))
  ))
}

# Progress logger fonksiyonu ----
log_progress <- function(message, log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " - ", message, "\n")
  cat(log_entry)
  write(log_entry, log_file, append = TRUE)
}

# Model oluşturma fonksiyonunu güncelleme ----
create_lstm_model <- function(lookback, n_features, units, dropout_rate, learning_rate) {
  model <- keras_model_sequential() %>%
    # Tek LSTM katmanı
    layer_lstm(units = units, 
               input_shape = c(lookback, n_features),
               return_sequences = FALSE) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    
    # Tek Dense katmanı
    layer_dense(units = units/2, activation = "relu") %>%
    layer_batch_normalization() %>%
    
    # Çıkış katmanı
    layer_dense(units = 1)
  
  # L1L2 regularization ekle
  regularizer <- regularizer_l1_l2(l1 = 0.01, l2 = 0.01)
  
  # Compile
  model %>% compile(
    optimizer = optimizer_adam(
      learning_rate = learning_rate,
      beta_1 = 0.9,
      beta_2 = 0.999
    ),
    loss = "mse",
    metrics = c("mae")
  )
  
  return(model)
}


# Early stopping callback'i güncelleme ----

# Early stopping callback'i güncelleme ----
create_callbacks <- function() {
  list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = 15,  # Daha az bekleme süresi
      restore_best_weights = TRUE
    ),
    callback_reduce_lr_on_plateau(
      monitor = "val_loss",
      factor = 0.5,
      patience = 5,
      min_lr = 1e-6
    )
  )
}


# Değerlendirme metrikleri için yardımcı fonksiyon ----
calculate_metrics <- function(y_true, y_pred) {
  rmse <- sqrt(mean((y_true - y_pred)^2))
  mae <- mean(abs(y_true - y_pred))
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  
  # R-kare hesaplama
  ss_tot <- sum((y_true - mean(y_true))^2)
  ss_res <- sum((y_true - y_pred)^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Theil's U istatistiği
  changes_actual <- diff(y_true)
  changes_pred <- diff(y_pred)
  correct_direction <- sum(sign(changes_actual) == sign(changes_pred))
  direction_accuracy <- correct_direction / length(changes_actual) * 100
  
  return(list(
    rmse = rmse,
    mae = mae,
    mape = mape,
    r_squared = r_squared,
    direction_accuracy = direction_accuracy
  ))
}

# Tek bir hiperparametre kombinasyonunu değerlendiren fonksiyon güncelleme ----
evaluate_single_configuration <- function(data, target_col, feature_cols, 
                                          params, n_months_ahead, epochs = 100, 
                                          validation_split = 0.2, seed = 1967,
                                          log_file) {
  tic(quiet = TRUE)
  log_progress(sprintf("Starting evaluation with parameters: %s", 
                       jsonlite::toJSON(params)), log_file)
  
  set.seed(seed)
  tensorflow::set_random_seed(seed)
  
  # Veriyi normalize et
  log_progress("Normalizing data...", log_file)
  norm_result <- normalize_data(data, c(target_col, feature_cols))
  normalized_data <- norm_result$normalized_data
  norm_params <- norm_result$params
  
  # Veri setini oluştur
  log_progress("Creating dataset...", log_file)
  dataset <- create_multivariate_dataset(normalized_data, target_col, 
                                         feature_cols, params$lookback)
  
  # Train-validation-test split
  log_progress("Splitting dataset...", log_file)
  total_samples <- nrow(dataset$x)
  train_size <- floor(0.7 * total_samples)
  val_size <- floor(0.15 * total_samples)
  
  train_idx <- 1:train_size
  val_idx <- (train_size + 1):(train_size + val_size)
  test_idx <- (train_size + val_size + 1):total_samples
  
  x_train <- dataset$x[train_idx,,, drop = FALSE]
  y_train <- dataset$y[train_idx,, drop = FALSE]
  x_val <- dataset$x[val_idx,,, drop = FALSE]
  y_val <- dataset$y[val_idx,, drop = FALSE]
  x_test <- dataset$x[test_idx,,, drop = FALSE]
  y_test <- dataset$y[test_idx,, drop = FALSE]
  
  # Model oluştur
  log_progress("Creating model...", log_file)
  model <- create_lstm_model(
    lookback = params$lookback,
    n_features = length(feature_cols) + 1,
    units = params$neurons,
    dropout_rate = params$dropout_rate,
    learning_rate = params$learning_rate
  )
  
  # Callbacks oluştur
  callbacks <- create_callbacks()
  
  # Model eğitimi
  log_progress("Training model...", log_file)
  history <- model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = params$batch_size,
    validation_data = list(x_val, y_val),
    callbacks = callbacks,
    verbose = 0
  )
  
  # Test seti tahminleri
  log_progress("Making predictions...", log_file)
  test_pred <- model %>% predict(x_test)
  
  # Denormalize
  test_pred_original <- test_pred * 
    (norm_params[[target_col]]$max - norm_params[[target_col]]$min) + 
    norm_params[[target_col]]$min
  
  y_test_original <- (y_test * 
                        (norm_params[[target_col]]$max - norm_params[[target_col]]$min) + 
                        norm_params[[target_col]]$min)[,1]
  
  # Geliştirilmiş metrikler
  metrics <- calculate_metrics(y_test_original, test_pred_original)
  metrics$val_loss <- min(history$metrics$val_loss)
  
  elapsed <- toc(quiet = TRUE)
  processing_time <- elapsed$toc - elapsed$tic
  
  log_progress(sprintf("Evaluation completed. Processing time: %.2f seconds", 
                       processing_time), log_file)
  
  return(list(
    metrics = metrics,
    history = history,
    model = model,
    processing_time = processing_time,
    predictions = list(
      test_pred = test_pred_original,
      y_test = y_test_original
    )
  ))
}

# Feature tahminleri için make_future_predictions fonksiyonunu güncelleme ----
make_future_predictions <- function(model, last_known_sequences, n_ahead, 
                                    normalization_params, target_col,
                                    feature_forecasts = NULL) {
  predictions <- numeric(n_ahead)
  current_sequence <- last_known_sequences
  
  # Son geçerli sequence'i bul
  if(any(is.na(current_sequence))) {
    warning("NAs found in the last sequence. Using the last valid sequence.")
    valid_sequences <- which(!apply(current_sequence[1,,1], 1, function(x) any(is.na(x))))
    if(length(valid_sequences) == 0) {
      stop("No valid sequences found for prediction")
    }
    current_sequence <- current_sequence[1,valid_sequences[length(valid_sequences)],,drop=FALSE]
  }
  
  for(i in 1:n_ahead) {
    # Tahmin yap
    x_pred <- array(current_sequence, dim = c(1, dim(current_sequence)[2], dim(current_sequence)[3]))
    pred <- model %>% predict(x_pred)
    predictions[i] <- pred[1,1]
    
    # Sequence'i güncelleme
    new_sequence <- current_sequence[1, -1, , drop = FALSE]
    
    # Feature değerlerini hazırla
    if(!is.null(feature_forecasts) && i <= nrow(feature_forecasts)) {
      new_features <- feature_forecasts[i,]
    } else {
      # Son bilinen feature değerlerini kullan
      new_features <- current_sequence[1, dim(current_sequence)[2], -1]
    }
    
    # Yeni tahmin ve feature'ları birleştir
    new_row <- c(predictions[i], new_features)
    
    # Sequence'i güncelle
    current_sequence <- abind::abind(new_sequence, 
                                     array(new_row, dim = c(1, 1, length(new_row))), 
                                     along = 2)
  }
  
  # Denormalize predictions
  predictions <- predictions * 
    (normalization_params[[target_col]]$max - normalization_params[[target_col]]$min) + 
    normalization_params[[target_col]]$min
  
  return(predictions)
}


# Final model oluşturma fonksiyonunu güncelleme
create_final_model <- function(grid_search_results, data, target_col, feature_cols = NULL, 
                               n_months_ahead, epochs = 100, export_path = "results") {
  
  # Grid search sonuçlarından en iyi parametreleri al
  if(is.null(grid_search_results$best_params)) {
    stop("No best parameters found in grid search results")
  }
  
  best_params <- grid_search_results$best_params
  
  # Veriyi kontrol et
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Input data is empty")
  }
  
  # feature_cols NULL ise boş vektör olarak ayarla
  if(is.null(feature_cols)) {
    feature_cols <- character(0)
  }
  
  # Feature'ları kontrol et (eğer varsa)
  if(length(feature_cols) > 0) {
    missing_cols <- setdiff(c(target_col, feature_cols), names(data))
    if(length(missing_cols) > 0) {
      stop(sprintf("Missing columns in data: %s", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Export path kontrolü
  if(!dir.exists(export_path)) {
    dir.create(export_path, recursive = TRUE)
  }
  
  # Log dosyası
  log_file <- file.path(export_path, "final_model_log.txt")
  log_progress("Starting final model training...", log_file)
  
  # Final modeli eğit
  tryCatch({
    final_results <- multivariate_lstm_forecast(
      data = data,
      target_col = target_col,
      feature_cols = feature_cols,
      n_months_ahead = n_months_ahead,
      lookback = best_params$lookback,
      epochs = epochs,
      batch_size = best_params$batch_size,
      units = best_params$neurons,
      dropout_rate = best_params$dropout_rate,
      learning_rate = best_params$learning_rate,
      seed = 1967
    )
    
    # Modeli kaydet
    model_path <- file.path(export_path, "final_model.weights.h5")
    #save_model_weights(final_results$model, model_path)
    
    # Tahminleri kaydet
    write.xlsx(final_results$forecasts, 
               file.path(export_path, "final_forecasts.xlsx"))
    
    # Performans metriklerini kaydet
    write.xlsx(final_results$performance, 
               file.path(export_path, "final_performance.xlsx"))
    
    log_progress("Final model training completed successfully.", log_file)
    
    return(final_results)
    
  }, error = function(e) {
    error_msg <- sprintf("Error in final model creation: %s", as.character(e))
    log_progress(error_msg, log_file)
    stop(error_msg)
  })
}

# multivariate_lstm_forecast fonksiyonunu da güncelleme
multivariate_lstm_forecast <- function(data, target_col, feature_cols = NULL, n_months_ahead,
                                       lookback, epochs, batch_size, units, dropout_rate,
                                       learning_rate, seed = 1967) {
  
  # Seed ayarlama
  set.seed(seed)
  tensorflow::set_random_seed(seed)
  
  # feature_cols NULL ise boş vektör olarak ayarla
  if(is.null(feature_cols)) {
    feature_cols <- character(0)
  }
  
  # Son target tarihini bul
  last_target_date <- max(data$Date[!is.na(data[[target_col]])])
  
  # Normalize edilecek sütunları belirle
  cols_to_normalize <- c(target_col)
  if(length(feature_cols) > 0) {
    cols_to_normalize <- c(cols_to_normalize, feature_cols)
  }
  
  # Veriyi normalize etme
  norm_result <- normalize_data(data, cols_to_normalize)
  normalized_data <- norm_result$normalized_data
  norm_params <- norm_result$params
  
  # Veri setini oluşturma
  dataset <- create_multivariate_dataset(normalized_data, target_col, feature_cols, lookback)
  
  # Train-test split
  train_size <- floor(0.8 * nrow(dataset$x))
  
  x_train <- dataset$x[1:train_size,,, drop = FALSE]
  y_train <- dataset$y[1:train_size,, drop = FALSE]
  
  x_test <- dataset$x[(train_size+1):nrow(dataset$x),,, drop = FALSE]
  y_test <- dataset$y[(train_size+1):nrow(dataset$y),, drop = FALSE]
  
  # Feature sayısını belirle (univariate durumda 1, multivariate durumda 1 + feature sayısı)
  n_features <- 1 + length(feature_cols)
  
  # Model oluşturma ve eğitim
  model <- create_lstm_model(lookback, n_features, units, dropout_rate, learning_rate)
  
  # Early stopping
  early_stopping <- callback_early_stopping(
    monitor = "val_loss",
    patience = 20,
    restore_best_weights = TRUE
  )
  
  # Model eğitimi
  history <- model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = 0.2,
    callbacks = list(early_stopping),
    verbose = 1
  )
  
  # Test seti tahminleri
  test_pred <- model %>% predict(x_test)
  
  # Son sequence'i al
  last_sequence <- tail(dataset$x, 1)
  
  # Feature değerlerini hazırla
  future_dates <- seq(last_target_date, by = "month", length.out = n_months_ahead + 1)[-1]
  
  # Feature değerlerini hazırla (eğer varsa)
  future_features <- NULL
  if(length(feature_cols) > 0) {
    future_features <- matrix(NA, nrow = n_months_ahead, ncol = length(feature_cols))
    
    for(i in 1:n_months_ahead) {
      future_date <- future_dates[i]
      if(future_date %in% data$Date) {
        future_row <- which(data$Date == future_date)
        future_features[i,] <- as.matrix(data[future_row, feature_cols, drop = FALSE])
      } else {
        last_known_features <- tail(data[!is.na(data[[feature_cols[1]]]), feature_cols], 1)
        future_features[i,] <- as.matrix(last_known_features)
      }
    }
  }
  
  # Tahminleri yap
  predictions <- make_future_predictions(
    model = model,
    last_known_sequences = last_sequence,
    n_ahead = n_months_ahead,
    normalization_params = norm_params,
    target_col = target_col,
    feature_forecasts = future_features
  )
  
  # Test tahminlerini denormalize etme
  test_pred_original <- test_pred * 
    (norm_params[[target_col]]$max - norm_params[[target_col]]$min) + 
    norm_params[[target_col]]$min
  
  y_test_original <- (y_test * 
                        (norm_params[[target_col]]$max - norm_params[[target_col]]$min) + 
                        norm_params[[target_col]]$min)[,1]
  
  # Performans metriklerini hesaplama
  rmse <- sqrt(mean((test_pred_original - y_test_original)^2))
  mae <- mean(abs(test_pred_original - y_test_original))
  mape <- mean(abs((y_test_original - test_pred_original) / y_test_original)) * 100
  
  # Tahmin dönemlerini oluşturma
  forecast_dates <- future_dates
  
  # Sonuçları hazırlama
  results <- list(
    forecasts = data.frame(
      Date = forecast_dates,
      Forecast = predictions,
      stringsAsFactors = FALSE
    ),
    performance = data.frame(
      Metric = c("RMSE", "MAE", "MAPE"),
      Value = c(rmse, mae, mape),
      stringsAsFactors = FALSE
    ),
    model = model,
    history = history,
    seed = seed,
    hyperparameters = list(
      lookback = lookback,
      epochs = epochs,
      batch_size = batch_size,
      units = units,
      dropout_rate = dropout_rate,
      learning_rate = learning_rate
    ),
    last_target_date = last_target_date
  )
  
  return(results)
}
