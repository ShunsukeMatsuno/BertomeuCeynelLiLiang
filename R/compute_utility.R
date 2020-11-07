compute_utility <- function(r, x, theta){
  # This function returns utility of reporting r given cost and x.
  theta_inv <- theta[1]
  
  benefit <- investor_response(r)
  cost <- theta_inv * (r - x)^2
  
  return(benefit - cost)
}