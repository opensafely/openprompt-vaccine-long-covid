#' Run GLM regressions for Long COVID
#' @description Run crude and age/sex-adjusted Poisson regression, tidy the output and add names of model, outcome and analysis.
#' @param cohort_data A list containing a dataset for a specified outcome and analysis.
#' @param stratifier The stratifying variable (e.g., vaccine doses, IMD)
#' @return A dataframe containing results from both models.

glm_regressions <- function(cohort_data, stratifier, collapse_variant = FALSE) {
  print(stratifier)
  
  number_levels <- cohort_data %>%
    dplyr::select(all_of(stratifier)) %>%
    drop_na() %>%
    n_distinct()
  
  if (number_levels > 1) {
    fm1 <- formula(paste("out ~ offset(log(t/1e5)) + ", stratifier))
    fm2a <- paste("out ~ offset(log(t/1e5)) + ", stratifier, " + age_cat + sex + region")
    
    # need to avoid duplication in fm2 in case age_cat or sex is the primary stratifier:
    fm2b <- ifelse(str_detect(stratifier, "age"), 
                  paste("out ~ offset(log(t/1e5)) + ", stratifier, "+ sex + region"),
                  fm2a)
    fm2 <- ifelse(str_detect(stratifier, "sex"), 
                  paste("out ~ offset(log(t/1e5)) + ", stratifier, "+ age_cat + region"),
                  fm2b) %>% as.formula()
    
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
    
    
    ## Create some common object for when the models are run
    stratifier_output <- NULL
    
    # get level for the intercept term
    base_level <- paste0(stratifier, 
                         levels(cohort_data %>% 
                                  dplyr::select(all_of(stratifier)) %>% 
                                  pull())[1])
    stratifier_intercept <- paste0(base_level,"(baseline)")
    
    # a row of fake data to preserve the sturucture of the results dataset
    intercept_data_to_add <- bind_cols(term = stratifier_intercept, 
                                       estimate = NA, 
                                       std_error = NA, 
                                       z_value = NA, 
                                       pr_z = NA, 
                                       rate = 1,
                                       conf.low = 1, 
                                       conf.high = 1)
    
    
    ## 2 chunks of code to either include variant period as a covariate
    ## or do separate models for each variant period
    if(collapse_variant){
      # add variant as a covariate
      fm2 <- update.formula(fm2, ~ . + variant)
      # group by covariates and sum the count of events and fup-time in each stratum
      dt_data <- data.table::setDT(cohort_data)
      
      # grab all the covariates for this stratifier (except the count and follow up time which we'll summarise)
      covars <- all.vars(fm2)[!all.vars(fm2) %in% c("out", "t")]
      
      dt_data <- dt_data[, .(out=sum(out), t=sum(t)),
                               by=covars]
      
      glm_model_crude <- shrink_glm_mem(MASS::glm.nb(fm1, data = dt_data))
      ## capture the full model output
      capture.output(
        fm1, 
        summary(glm_model_crude),
        file = here::here("output/regression_outputs/", paste0("model_out_", stratifier, ".txt")),
        append = TRUE
      )
      crude_output <- convert_to_rates(glm_model_crude) %>%
        bind_rows(intercept_data_to_add)
      glm_model_crude <- NULL
      
      glm_model_adj <- shrink_glm_mem(MASS::glm.nb(fm2, data = dt_data))
      ## capture the full model output
      capture.output(
        fm2, 
        summary(glm_model_adj),
        file = here::here("output/regression_outputs/", paste0("model_out_", stratifier, ".txt")),
        append = TRUE
      )
      adjusted_output <- convert_to_rates(glm_model_adj) %>% 
        bind_rows(intercept_data_to_add)
      glm_model_adj <- NULL
      
      ## get number of events and p-years "at risk" in each stratified level
      ## melt into long format so each observation will be repeated by each of `covars`
      dt_melt <- data.table::melt(dt_data, measure.vars = covars)
      
      ## for each of `covars` calculated the sum of `out` and `t`
      dt_summ_by_covars <- dt_melt[, lapply(.SD, sum, na.rm = TRUE), by = c("variable", "value"), .SDcols=c("out", "t")]
      
      ## merge together the variable and value column to merge onto the Model outputs
      dt_summ_by_covars$merge_var <- paste0(dt_summ_by_covars$variable, dt_summ_by_covars$value)
      dt_summ_by_covars$merge_var <- stringr::str_replace_all(dt_summ_by_covars$merge_var, base_level, stratifier_intercept)
      dt_summ_by_covars$variable <- NULL
      dt_summ_by_covars$value <- NULL
      
      stratifier_output <- bind_rows(
        stratifier_output, 
        bind_rows(
          mutate(crude_output, model = "crude"),
          mutate(adjusted_output, model = "adjusted"),
        ) %>%
          mutate(
            plot_marker = stringr::str_detect(term, stratifier),
            var = stratifier,
            baseline = stratifier_intercept,
            covid_variant = "All"
          ) %>%
          left_join(dt_summ_by_covars, by = c("term" = "merge_var")) %>%
          dplyr::select(term, out, t, everything())
      )
    }else{
      ## if doing separate models by variant period then do it in a loop
      for(variant in levels(cohort_data$variant)){
        ## subset to one covid variatnt period
        variant_data <- cohort_data[cohort_data$variant == variant, ]
        
        # grab all the covariates for this stratifier (except the count and follow up time which we'll summarise)
        covars <- all.vars(fm2)[!all.vars(fm2) %in% c("out", "t")]
        
        # group by covariates and sum the count of events and fup-time in each stratum
        dt_variant <- data.table::setDT(variant_data)
        
        dt_variant <- dt_variant[, .(out=sum(out), t=sum(t)),
                   by=covars]
        
        glm_model_crude <- shrink_glm_mem(MASS::glm.nb(fm1, data = dt_variant))
        crude_output <- convert_to_rates(glm_model_crude) %>%
          bind_rows(intercept_data_to_add)
        glm_model_crude <- NULL
      
        glm_model_adj <- shrink_glm_mem(MASS::glm.nb(fm2, data = dt_variant))
        adjusted_output <- convert_to_rates(glm_model_adj) %>% 
          bind_rows(intercept_data_to_add)
        glm_model_adj <- NULL
        
        ## get number of events and p-years "at risk" in each stratified level
        ## melt into long format so each observation will be repeated by each of `covars`
        dt_variant_melt <- data.table::melt(dt_variant, measure.vars = covars)
        
        ## for each of `covars` calculated the sum of `out` and `t`
        dt_summ_by_covars <- dt_variant_melt[, lapply(.SD, sum, na.rm = TRUE), by = c("variable", "value"), .SDcols=c("out", "t")]
        
        ## merge together the variable and value column to merge onto the Model outputs
        dt_summ_by_covars$merge_var <- paste0(dt_summ_by_covars$variable, dt_summ_by_covars$value)
        dt_summ_by_covars$merge_var <- stringr::str_replace_all(dt_summ_by_covars$merge_var, base_level, stratifier_intercept)
        dt_summ_by_covars$variable <- NULL
        dt_summ_by_covars$value <- NULL
        
        stratifier_by_variant <- bind_rows(
          mutate(crude_output, model = "crude"),
          mutate(adjusted_output, model = "adjusted"),
        ) %>% 
          mutate(plot_marker = stringr::str_detect(term, stratifier),
                 var = stratifier,
                 baseline = stratifier_intercept,
                 covid_variant = variant) %>% 
          left_join(dt_summ_by_covars, by = c("term"="merge_var")) %>% 
          dplyr::select(term, out, t, everything())
        
        stratifier_output <- bind_rows(stratifier_output,
                                       stratifier_by_variant)
      }
    }
    stratifier_output
  }
}

