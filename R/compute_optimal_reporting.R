compute_optimal_reporting <- function(theta, x){
  # This function solves the managers optimal manipulation problem
  
  optim_result <- optim(par = 1, 
                        fn = compute_utility, 
                        method = 'Brent',
                        lower = -1,
                        upper = 1,
                        theta = theta,
                        x = x,
                        control = list(fnscale = -1))
  
  return(optim_result$par)

}