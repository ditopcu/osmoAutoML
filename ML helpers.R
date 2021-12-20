osmo_h2o <- function(train_df, model_name, fold_col, main_dir, run_time = 3600*2 ) {
  
  
  model_path <- (paste0(main_dir, model_name))
  
  dir.create(model_path)
  
  h2o_train <- as.h2o(train_df) 
  
  print("Start Auto ML")
  aml <- h2o.automl(y = "target",  # max_models = 2, #debug
                    fold_column = fold_col,
                    max_runtime_secs = run_time,
                    training_frame = h2o_train,
                    project_name = "aml_1", 
                    seed = 555)
  
  print("Saving Model")
  model_ids  <-   tibble(model_names = as.vector(aml@leaderboard$model_id)) |> 
    rowwise() |> 
    mutate(h2o_model = list(h2o.getModel(model_names))) |>
    ungroup() |> 
    mutate(save_name = paste0(model_path, "/", str_pad(row_number(), width = 6, side = "left", pad = "0"), "_",model_names,".mdl" )) 
  
  
  saveRDS(model_ids, file = paste0(model_path, "/", "h2o_models.RDS"))
  model_ids   |>
    mutate(save = map(h2o_model, ~h2o.saveModel(object = .x, model_path, force = TRUE)))
  
  print("Resetting")
  h2o.removeAll()
  
}


load_h2o_models <- function(df) {
  
  df %>% 
    rowwise() %>% 
    mutate(h2o_model = list(h2o.loadModel(model_file))) %>% 
    ungroup() %>% 
    group_by(model_name) %>% 
    mutate(model_name = paste0("model_", str_remove_all(model_name, "osmo_train"), "|", str_pad(row_number(),width = 2, side = "left",pad = "0") )) %>% 
    ungroup() %>% 
    select(model_name, model_h2o_names, h2o_model)
}


get_h2o_metrics <- function(mdl, name_add = "") {
  
  #tibble(a = 1) %>% mutate(tibble(b = 2 , c = 1))
  
  t <- tibble(
    r2  = h2o.r2(mdl),rmse = h2o.rmse(mdl),mae = h2o.mae(mdl),mse = h2o.mse(mdl)
  )
  if (name_add != "") names(t)  <- paste0(name_add, "_", names(t)) 
  t
}

get_train_predictions <- function(h2o_model_df, h2o_train_df) {
  
  
  train_predictions <-  h2o_model_df |> 
    mutate(h2o_predictions = map(h2o_model, ~h2o.predict(.x, newdata = h2o_train_df) )) |>  
    mutate(model_algorithm =  map_chr(h2o_model, function(x) x@algorithm )) |> 
    mutate(model_name = str_remove(sub("\\|.*","\\|",model_name), "\\|")) |> 
    mutate(dataset = "train") |> 
    select(model_name, dataset,  model_algorithm, h2o_predictions)
  
  train_predictions_with_data <- train_predictions %>% 
    mutate(predictions_tibble = map(h2o_predictions, function(x) {
      as_tibble(x) %>% 
        bind_cols(as.data.frame(h2o_train_df))
    }  )) |> 
    select(model_name, dataset, model_algorithm, predictions_tibble ) 
}



get_test_predictions <- function(h2o_model_df, h2o_test_df) {

    test_predictions <- h2o_model_df |> 
      mutate(h2o_predictions = map(h2o_model, ~h2o.predict(.x, newdata = h2o_test_df) )) |> 
      mutate(model_algorithm =  map_chr(h2o_model, function(x) x@algorithm )) |> 
      mutate(model_name = str_remove(sub("\\|.*","\\|",model_name), "\\|")) |> 
      mutate(dataset = "test") |> 
      select(model_name, dataset,  model_algorithm, h2o_predictions)
  
  
  
  test_predictions_with_data <- test_predictions %>% 
    mutate(predictions_tibble = map(h2o_predictions, function(x) {
      as_tibble(x) %>% 
        bind_cols(as.data.frame(h2o_test_df))
    }  )) |> 
    select(model_name, dataset, model_algorithm, predictions_tibble ) 
}




get_train_metrics <- function(h2o_model_df) {
  
  metrics_train_data <- h2o_model_df %>% 
    mutate(type = "train") %>% 
    mutate(map_df(h2o_model , ~get_h2o_metrics(.x))) %>% 
    select(-h2o_model) %>% 
    gather(stat, value, - (model_name:type)) %>% 
    spread(type, value) 
  
}

get_test_metrics <- function(h2o_model_df, h2o_test_df) {
  
  metrics_test_data <- h2o_models_loaded %>% 
    mutate(h2o_perf_test  = map(h2o_model, ~h2o.performance(.x, newdata = h2o_test_df) )) |> 
    mutate(type = "test") %>% 
    mutate(map_df(h2o_perf_test , ~get_h2o_metrics(.x))) |> 
    select(-h2o_model, -h2o_perf_test) |> 
    gather(stat, value, - (model_name:type)) %>% 
    spread(type, value)
  
}


