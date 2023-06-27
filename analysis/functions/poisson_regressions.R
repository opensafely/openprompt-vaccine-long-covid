#' Run Poisson regressions for Long COVID
#' @description Run crude and age/sex-adjusted Poisson regression, tidy the output and add names of model, outcome and analysis.
#' @param cohort_data A list containing a dataset for a specified outcome and analysis.
#' @param stratifier The stratifying variable (e.g., vaccine doses, IMD)
#' @return A dataframe containing results from both models.

poisson_regressions <- function(cohort_data, stratifier) {
  print(stratifier)
  
  number_levels <- cohort_data %>%
    dplyr::select(all_of(stratifier)) %>%
    drop_na() %>%
    n_distinct()
  
  if (number_levels > 1) {
    fm1 <- formula(paste("out ~ offset(log(t/1e5)) + ", stratifier))
    fm2a <- paste("out ~ offset(log(t/1e5)) + ", stratifier, "+ age_centred + sex + variant")
    
    # need to avoid duplication in fm2 in case age_cat or sex is the primary stratifier:
    fm2b <- ifelse(str_detect(stratifier, "age"), 
                  paste("out ~ offset(log(t/1e5)) + ", stratifier, "+ sex + variant"),
                  fm2a)
    fm2 <- ifelse(str_detect(stratifier, "sex"), 
                  paste("out ~ offset(log(t/1e5)) + ", stratifier, "+ age_centred + variant"),
                  fm2b) %>% as.formula()
    
    # add variant if (and only if) a time_updated dataset is being used
    # as this variable is not available in clean_data
    
    
    # remove a lot of the memory intensive fat from the model object but keep necessary bits to predict rates
    shrink_glm_mem <- function(glm_fitted) {
      glm_fitted$model <- NULL
      glm_fitted$data <- NULL
      glm_fitted$effects <- NULL
      #glm_fitted$qr$qr <- NULL
      glm_fitted$residuals <- NULL
      glm_fitted$fitted.values <- NULL
      #glm_fitted$linear.predictors <- NULL
      glm_fitted$weights <- NULL
      glm_fitted$prior.weights <- NULL
      glm_fitted$y <- NULL
      
      return(glm_fitted)
    }
    
    convert_to_rates <- function(model){
      coef(summary(model)) %>% 
        tidyr::as_tibble(rownames = "term") %>% 
        janitor::clean_names() %>% 
        mutate(rate = exp(estimate),
               conf.low = exp(estimate - (1.96*std_error)),
               conf.high = exp(estimate + (1.96*std_error))
        )
    }
    
    # get level for the intercept term
    stratifier_intercept <- paste0(stratifier, 
                                   levels(cohort_data %>% 
                                     dplyr::select(all_of(stratifier)) %>% 
                                     pull())[1],
                                   "(baseline)")
    
    # a row of fake data to preserve the sturucture of the results dataset
    intercept_data_to_add <- bind_cols(term = stratifier_intercept, 
              estimate = NA, 
              std_error = NA, 
              z_value = NA, 
              pr_z = NA, 
              rate = 1,
              conf.low = 1, 
              conf.high = 1)
  
    poissonmodel_crude <- shrink_glm_mem(glm(fm1, data = cohort_data, family = poisson(link = "log")))
    crude_output <- convert_to_rates(poissonmodel_crude) %>% 
      bind_rows(intercept_data_to_add)
    poissonmodel_crude <- NULL
  
    poissonmodel_adj <- shrink_glm_mem(glm(fm2, data = cohort_data, family = poisson(link = "log")))
    adjusted_output <- convert_to_rates(poissonmodel_adj) %>% 
      bind_rows(intercept_data_to_add)
    poissonmodel_adj <- NULL
    
    bind_rows(
      mutate(crude_output, model = "crude"),
      mutate(adjusted_output, model = "adjusted"),
    ) %>% 
      mutate(plot_marker = stringr::str_detect(term, stratifier),
             var = stratifier,
             baseline = stratifier_intercept)
  }
}

