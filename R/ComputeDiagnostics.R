# function to compute Rhat conform Vehtari et al (2019) and calculate AC manually
# requires 'dplyr', and the functions in 'ComputeAC.R', and 'ComputeRhat.R'

# load functions
source('R/ComputeRhat.R')
source('R/ComputeAC.R')

# combine the two diagnostics
convergence <- function(x, include_acf = FALSE) {
  # input: object with imputation chain(s), e.g. chain mean, choice of calculating default AC
  # output: AC values at each iteration, computed manually, if necessary also using stats:acf() 
  out <-  full_join(rhat_adapted(x), ac_adapted(x), by = "iteration") 
  if(include_acf){out <- full_join(out, ac_adapted(x, "acf"), by = "iteration")
  }
  out <- out %>% dplyr::rename(it = iteration)
  return(out)
}
