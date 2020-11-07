plot_obj <- function(theta, df_firm_reported, param_vec, index){
  # This function plots the objective function for each value of parameter.
  # index = 1 (theta_inv; marginal cost),
  #         2 (m; mean),
  #         3 (sigma; sd)
  
  # objective function
  theta_temp <- theta
  obj <- numeric(length(param_vec))
  
  # parallel computation 
  ## register
  library(doParallel)
  cl <- makePSOCKcluster(detectCores(logical = FALSE))
  doParallel::registerDoParallel(cl)
  
  theta_temp <- theta
  clusterExport(cl, "df_firm_reported")
  
  obj = foreach (i = seq_along(param_vec),
                 .packages = c('dplyr', 'purrr', 'BCXL'),
                 .combine = 'rbind') %dopar% {
                   theta_temp[index] <- param_vec[i]
                   BCXL::compute_lkhd(theta_temp, df_firm_reported)
                 }
  
  ## kill finished tasks
  parallel::stopCluster(cl)
  
  # To plot obj. func., make a data frame.
  df_obj <- tibble(
    param = param_vec,
    obj = obj
  )
  
  # label is determined according to `index`
  x_label <- case_when(
    index == 1 ~ 'eta',
    index == 2 ~ 'm',
    index == 3 ~ 'sigma',
  )
  
  # plot
  g <- ggplot(df_obj, aes(x = param, y = obj)) +
    geom_point(size = 2) + 
    xlab(x_label) +
    geom_vline(aes(xintercept = theta[index]), linetype = 'dotted') +
    theme_bw()
  
  plot(g)
  
}