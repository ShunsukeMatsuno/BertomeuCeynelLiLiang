compute_lkhd <- function(theta, df){
  # This function returns negative likelihood.
  
  # params
  theta_inv <- theta[1]
  m <- theta[2]
  sigma <- theta[3]
  
  # estimate gamma
  gamma <- estimate_gamma_poly(df_firm_reported)
  
  lkhd <- df_firm_reported %>% 
    mutate(pred_deriv  = gamma$pred_deriv,
           pred_deriv2 = gamma$pred_deriv2) %>% 
    mutate(lkhd = log(1 - pred_deriv2 * 1/(2 * theta_inv)) - log(sigma)
                      + log(dnorm(
                        (r - 1/(2 * theta_inv) * pred_deriv - m) / sigma,
                        0, 1))
    ) %>% 
    pull(lkhd)
  
  return(-sum(lkhd))
}