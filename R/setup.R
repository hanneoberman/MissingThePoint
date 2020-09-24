# Setup simulation
setup <- function(seed) {
  # setup environment
  library(mice)
  library(tidyverse)
  set.seed(seed)
  
  # complete data variance-covariance matrix
  varcov <<- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), ncol = 3)
  
  # patterns to ampute the data with multivariate missingness
  amp_pat <<-
    expand.grid(c(0, 1), c(0, 1), c(0, 1)) %>% #define all possible combinations of univariate and multivariate missingness
    .[c(-1, -8),]  %>% #remove the completely (un)observed cases
    setNames(c("Y", "X1", "X2")) #give correct names 
  
  # simulation parameters
  n_cases <<- 1000
  n_imp <<- 5
  m_mech <<- c("MCAR", "MAR", "MNAR")
  p_inc <<- c(0.25, 0.5, 0.75)

  # objects to store imputations in
  mids <<- chainmeans <<- chainvars <<- qhats <<- lambdas <<- NULL
  
}
