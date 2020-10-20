# setup environment for example

setup_example <- function(seed){
  # environment
  library(mice)
  library(tidyverse)
  set.seed(seed)
  
  # parameters
  n_imp <<- 5
  n_it <<- 30
  mids <<- qhats <<- lambdas <<- NULL  
}

# other functions to load
source("~/MissingThePoint/R/ComputeDiagnostics.R")

not_all_na <- function(x) {
  !all(is.na(x))
}

preprocess_thetas <- function(theta, ext){
  out <- theta %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>%
    setNames(paste0(ext, ".", names(.))) %>%
    cbind(
      it = 1:n_it,
      m = rep(1:n_imp, each = n_it),
      .
    ) %>% 
    select_if(not_all_na) %>% 
    arrange(it)
  rownames(out) <- NULL
  return(out)
}

get_diagnostics <- function(parameters) {
  thetas <- names(parameters)[-c(1:2)]
  out <- purrr::map_dfc(thetas, function(tt) {
    matrix(parameters[[tt]], ncol = n_imp, byrow = T) %>%
      convergence() %>%
      select(r.hat.max, ac.max) %>%
      setNames(., paste0(names(.), ".", tt))
  }) %>%
    cbind(it = 1:n_it, .)
  return(out)
}

impute_once <- function(iteration, ...) {
  if(is.null(mids)){
    mids <<- mice(boys, m = n_imp, maxit = 1, printFlag = FALSE)
  }
  else {
    mids <<- mice.mids(mids, printFlag = FALSE)
  }
  mira <- mids %>% mice::lm.mids(age ~ ., data = .) 
  mipo <- mira %>%
    mice::pool() %>%
    summary(., conf.int = TRUE)
  mild <- mids %>% mice::complete("all")
  
  qhat <- 
    map_dbl(mild, ~ {
      lm(formula = age ~ ., data = .) %>% .$coefficients %>% .[2]
    })
  lambda <- mild %>%
    purrr::map_dbl(., ~ {
      dplyr::mutate(., dplyr::across(dplyr::everything(), ~as.numeric(.))) %>% 
        princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% . ^ 2 #first eigenvalue of the varcovar matrix
    })
  qhats <<- rbind(qhats, data.frame(it = iteration, m = 1:n_imp, qhat = qhat))
  lambdas <<- rbind(lambdas, data.frame(it = iteration, m = 1:n_imp, lambda = lambda))
  
  out <- data.frame(it = iteration, est = mipo[["estimate"]][2])
  return(out)
}


