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
    fm1 <- formula(paste("lc_out ~ offset(log(t/1e5)) + ", stratifier))
    fm2 <- formula(paste("lc_out ~ offset(log(t/1e5)) + ", stratifier, "+ age_centred + sex"))
    
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
               conf.low = exp(estimate - std_error),
               conf.high = exp(estimate + std_error)
        )
    }
    
    poissonmodel_crude <- shrink_glm_mem(glm(fm1, data = cohort_data, family = poisson(link = "log")))
    crude_output <- convert_to_rates(poissonmodel_crude)
    poissonmodel_crude <- NULL
    
    poissonmodel_adj <- shrink_glm_mem(glm(fm2, data = cohort_data, family = poisson(link = "log")))
    adjusted_output <- convert_to_rates(poissonmodel_adj)
    poissonmodel_adj <- NULL
    
    bind_rows(
      mutate(crude_output, model = "crude"),
      mutate(adjusted_output, model = "adjusted"),
    ) %>% 
      mutate(plot_marker = stringr::str_detect(term, stratifier)) %>% 
      mutate(var = stratifier)
  }
}
