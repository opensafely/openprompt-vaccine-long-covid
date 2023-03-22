#' Run Poisson regressions for Long COVID
#' @description Run crude and age/sex-adjusted Poisson regression, tidy the output and add names of model, outcome and analysis.
#' @param cohort_data A list containing a dataset for a specified outcome and analysis.
#' @param stratifier The stratifying variable (e.g., vaccine doses, IMD)
#' @return A dataframe containing results from both models.

poisson_regressions <- function(cohort_data, stratifier) {
  number_levels <- cohort_data %>%
    dplyr::select(all_of(stratifier)) %>%
    drop_na() %>%
    n_distinct()
  
  if (number_levels > 1) {
    fm1 <- formula(paste("lc_out ~ offset(log(t)) + ", stratifier))
    fm2 <-
      formula(paste("lc_out ~ offset(log(t)) + ", stratifier, "+ age_centred + sex"))
    
    # make little dataframe for prediction at all levels of the stratifier
    # and baseline levels of age and sex
    pred_data <- cohort_data %>%
      dplyr::select({
        {
          stratifier
        }
      }) %>%
      group_by_all() %>%
      slice(1) %>%
      drop_na() %>%
      # 1 year of follow up to get rates per X years
      mutate(t = 1,
             age_centred = 0,
             sex = "male")
    
    #make little function to get predictions from both models
    calculate_predictions <- function(model) {
      predictions <- predict(model,
                             newdata = pred_data,
                             type = "link",
                             se.fit = TRUE)
      model %>%
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
        filter(!stringr::str_detect(term, "sex|age")) %>%
        mutate(
          pred = predictions$fit,
          stbp = predictions$se.fit,
          #CIs
          upp = pred + (1.96 * stbp),
          low = pred - (1.96 * stbp),
          # probline
          predicted_vals = exp(pred) * 1e5,
          uci = exp(upp) * 1e5,
          lci = exp(low) * 1e5
        ) %>%
        dplyr::select(term,
                      estimate,
                      conf.low,
                      conf.high,
                      predicted_vals,
                      lci,
                      uci)
    }
    
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
    
    poissonmodel_crude <- shrink_glm_mem(glm(fm1, data = cohort_data, family = poisson(link = "log")))
    
    crude_output <- calculate_predictions(poissonmodel_crude)
    
    poissonmodel_adj <- shrink_glm_mem(glm(fm2, data = cohort_data, family = poisson(link = "log")))
    adjusted_output <- calculate_predictions(poissonmodel_adj)
    
    bind_rows(
      mutate(crude_output, model = "crude"),
      mutate(adjusted_output, model = "adjusted")
    ) %>%
      separate(term, c("var", "level"), sep = stratifier, fill = "left") %>%
      mutate(var = stratifier)
  }
  
}
