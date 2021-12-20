library(tidyverse)
#library(rsample) # for train / test set split
#library(caret) # for k-fold split
library(h2o)
library(fst)
library(Boruta)

library(rstatix)
library(mcr)
library(ggsci)

source("src/github/ML helpers.R")




  

# Setup functions -------------------------------------------------------


  
  h2o.init(nthreads = 1)
  set.seed(555)
  
  
  uri_raw_names <- c("target", "sys_cond_quant", "sys_dansite_quant", "sys_epitel_quant", 
                      "sys_rbc_quant", "sys_wbc_quant", "sys_albumin_semi", "sys_bilirubin_semi", 
                      "sys_glukoz_semi", "sys_keton_semi", "sys_kreatinin_semi", "sys_nitrojen_semi", 
                      "sys_ph_semi", "sys_protein_semi")
  
  
  uri_test_names <- c("target", "Conudctivity", "Specific Gravity", "Epithel", 
                       "RBC", "WBC", "Albumin", "Bilirubin", 
                       "Glucose", "Keton", "Creatinine", "Nitrite", 
                       "pH", "Protein")
  
  
  uri_names <-  uri_test_names 
  names(uri_names) <- uri_raw_names
  

  osmo_all # Dataset which includes all measurement
  

# 2.8.1 Pre-Process  ----------------------------------------------------------------

  # Merge gorups
  osmo_all <- osmo_data_analysis |> 
    mutate(sys_albumin_semi = fct_collapse(sys_albumin_semi, `0` = c("0", "10"), `80` = c("80", "100")) ) |>
    mutate(sys_kreatinin_semi = fct_collapse(sys_kreatinin_semi, `10` = c("5", "10", "20"), `50` = c("40", "50"))) |> 
    mutate(sys_ph_semi = fct_collapse(sys_ph_semi, `7.5` = c("7.5", "8", "8.5", "9")))

# 2.8.2  Feature Selection: Boruta Analysis  ---------------------------------


  osmo_boruta_df <-  osmo_all |> 
    select(-sys_osmolalite_quant, -sys_albcrea_semi) |> 
    select(target = ref_osmolalite_quant, contains("sys")) # select urine parameters +  reference measurement 
                                                            # ref_osmolalite_quant: change name as target
  
  osmo_boruta_df
  
  
  # Perform Boruta Analysis
  boruta_output <- Boruta(target ~ ., osmo_boruta_df, doTrace=3) 
  
  # Get significant Parameters
  boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)

  
  # Boruta importance table
  imp_temp <- attStats(boruta_output)
  importance_table = imp_temp[imp_temp$decision != '', c('meanImp', 'decision')] |> 
    as_tibble(rownames = "Feature") 
  
  importance_table
  
  # Boruta importance plot
  
  plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
  
  # selected features
  selected_osmo_df <- osmo_boruta_df |>  # Select only important measurements
    select(target, contains(boruta_signif))
  
  
  selected_osmo_df

  

# 2.8.3 Data Split  Train / Test Splits -----------------------------------------

  

  
  
  # Put 2/3 of the data into the training set 
  data_split <- rsample::initial_split(selected_osmo_df, prop = 200/300, strata = target)
  
  # Create data frames for the two sets:
  osmo_train <- rsample::training(data_split)
  osmo_test  <- rsample::testing(data_split)

  # k-fold Splits
  fold <- caret::createFolds(osmo_train$target, k = 10, list = FALSE)
  osmo_train$fold <- fold
  
  

# 2.8.2 (contd.) Create Feature Groups ----------------------------------------------------------

  
  # conductivity
  osmo_train_cond <- osmo_train |> 
    select(target, fold, sys_cond_quant)
  
  # conductivity + density
  osmo_train_cond_dans <- osmo_train |> 
    select(target, fold,sys_dansite_quant, sys_cond_quant)

  # conductivity +  standar parameters (dansite + protein + glucose + ph)
  osmo_train_cond_dans_prot_gluc_ph <- osmo_train |> 
    select(target, fold, sys_cond_quant, sys_dansite_quant, sys_protein_semi, sys_glukoz_semi, sys_ph_semi )
  
  
  # Feature Groups
  osmo_train_cond
  osmo_train_cond_dans
  osmo_train_cond_dans_prot_gluc_ph


# 2.8.4 H2O AUTOML  -------------------------------------------------------------
  
  model_save_location  <- "src/github/models/"
  

  
  osmo_h2o(osmo_train_cond, "osmo_cond", fold_col = "fold", main_dir = model_save_location)
  osmo_h2o(osmo_train_cond_dans, "osmo_cond_dans", fold_col = "fold", main_dir = model_save_location)
  osmo_h2o(osmo_train_cond_dans_prot_gluc_ph,"osmo_standard", fold_col = "fold", main_dir = model_save_location)

  

# 2.8.5 Train/Test sets Predictions and Performance Metrics -------------------------------------------------

  

# Load H2O models 

  
  model_save_location  <- "src/github/models/"
 
  h2o_test <- as.h2o(osmo_test)
  h2o_train <- as.h2o(osmo_train)
  
  # Read saved model file names
  model_list <- tibble(models = list.files(model_save_location))  
  
  model_list
  
  # make model df for H2O models
  h2o_model_files_raw <- model_list |> 
    mutate(path = paste0(model_save_location, models)) |> 
    rowwise() |> 
    mutate(model_h2o_names = list(list.files(path)[!str_detect(list.files(path), "RDS")])) |> 
    # mutate(path = paste0(path, "/", model_h2o_names)) |> 
    mutate(best_model0 = paste0(path, "/h2o_models.RDS")) |> 
    rowwise() |> 
    mutate(best_model0 = readRDS(best_model0)[1,1] |> pull(1)) |> 
    ungroup() 
  
  
  h2o_model_files <- h2o_model_files_raw |> 
    unnest(model_h2o_names) |> 
    group_by(models) |> 
    arrange(model_h2o_names , .by_group = TRUE) |> 
    ungroup() |> 
    mutate(model_file = paste0(path, "/", model_h2o_names )) |> 
    rename(model_name = models)
  
  # Check model count
  h2o_model_files |> 
    count(model_name)
  
  # lood saved models
  # h2o_models_loaded is used for prediction/metric/importance
  h2o_models_loaded <- h2o_model_files |> 
    load_h2o_models()
  

  
# Model Predictions 
  
  train_predictions <- get_train_predictions(h2o_models_loaded,  h2o_train ) |> 
    unnest(predictions_tibble) |> 
    select(model_name, dataset, model_algorithm, predicted = predict, target, sys_cond_quant:sys_protein_semi )


  test_predictions <- get_test_predictions(h2o_models_loaded,  h2o_test ) |> 
    unnest(predictions_tibble) |> 
    select(model_name, dataset, model_algorithm, predicted = predict, target, sys_cond_quant:sys_protein_semi )



# Model Metrics 



  train_metrics <- get_train_metrics(h2o_models_loaded)
  
  test_metrics <- get_test_metrics(h2o_models_loaded, h2o_test)

  
  all_metrics <- train_metrics %>%  
    left_join(test_metrics, by = c("model_name", "stat", "model_h2o_names"))
  
  all_metrics

  
  
# Variable Importance 
  
  
  var_imp <- h2o_models_loaded |> 
    mutate(var_imp = map(h2o_model, ~as.data.frame(h2o.varimp(.x)) ) ) |> 
    mutate(model_algorithm =  map_chr(h2o_model, function(x) x@algorithm )) |> 
    select(model_name,model_algorithm,  var_imp ) |> 
    unnest(cols = c(var_imp)) |> 
    arrange(model_name )
  
  
  
  
  

  
  



