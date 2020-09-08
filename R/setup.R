# Setup simulation
setup <- function(seed) {
  # setup environment
  library(mice)
  library(tidyverse)
  set.seed(seed)
  
  # create complete data
  vcov <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), ncol = 3)
  dat <<- mvtnorm::rmvnorm(1000, sigma = vcov) %>%
    as.data.frame() %>%
    setNames(c("Y", "X1", "X2")) %>%
    mutate(Y = Y + rnorm(1000, sd = 1))
  Q <<- lm(Y ~ X1 + X2, dat) %>%
    broom::tidy() %>%
    .[["estimate"]] %>%
    .[2]
  
  # create patterns to ampute the data with multivariate missingness
  amp_pat <<-
    expand.grid(c(0, 1), c(0, 1), c(0, 1)) %>% #define all possible combinations of univariate and multivariate missingness
    .[c(-1, -8),]  %>% #remove the completely (un)observed cases
    setNames(names(ampute(dat)$patterns)) #obtain correct names of patterns
  
  # simulation parameters
  p_inc <<- c(0.05, 0.5, 0.95)
  m_mech <<- c("MCAR", "MAR", "MNAR")
  
  # objects to store imputations in
  mids <<- chainmeans <<- chainvars <<- NULL
  
}