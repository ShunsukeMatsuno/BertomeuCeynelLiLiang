estimate_gamma_poly <- function(df_firm_reported){
  # This function use polynomial regression to obtain
  #  the relationship between price and reported earnings.
  # The first- and second-order derivatives are derived analytically.
  
  reg <- lm(price ~ poly(r, degree = 3, raw = TRUE), df_firm_reported)
  coef <- reg$coefficients
  
  pred <- predict(reg, df_firm_reported)
  pred_deriv <- coef[2] + 2 * coef[3] * df_firm_reported$r + 3 * coef[4] * df_firm_reported$r^2
  pred_deriv2 <- 2 * coef[3] + 6 * coef[4] * df_firm_reported$r
  
  return(list(pred = pred, pred_deriv = pred_deriv, pred_deriv2 = pred_deriv2))
}