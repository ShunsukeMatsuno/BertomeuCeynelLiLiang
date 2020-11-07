plot_r_x <- function(theta_inv){
  # This function changes the cost parameter and plots the result.
  
  # parameters
  theta[1] <- theta_inv    # WARNING: theta is a global variable
  
  # optimal reporting
  df_firm_reported <- df_firm %>% 
    mutate(r = map_dbl(.x = x, .f = compute_optimal_reporting, theta = theta)) %>% 
    mutate(price = investor_response(r))
  
  # plot
  df_firm_reported_plot <- df_firm_reported %>% 
    select(firm, x, r) %>% 
    pivot_longer(-firm, names_to = "type", values_to = "freq")
  
  ggplot(df_firm_reported_plot, aes(x = freq, fill = type)) +
    geom_histogram(aes(y = ..density..),
                   color    = 'white',
                   position = 'identity', 
                   alpha    = .5,
                   bins     = 50) +
    theme_bw()
}