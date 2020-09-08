# Impute the data
impute <-
  function(amp,
           m_mech,
           p_inc,
           it_nr,
           it_total,
           ...) {
    
    # initialize the mids object in the first iteration
    if (is.null(mids)) {
      mids <<- mice(
        amp,
        m = n_imp,
        method = "norm",
        maxit = 1,
        printFlag = FALSE
      )
    } else {
    # append to the mids object in any further iteration
      mids <<- mice.mids(
        mids, 
        maxit = 1, 
        printFlag = FALSE)
    }
    
    # analyze and pool imputations
    mipo <- mids %>%
      mice::lm.mids(Y ~ ., .) %>%
      mice::pool() %>%
      .$pooled
    
    # extract statistics
    est <- mipo$estimate[2]
    SE <- sqrt(mipo$b + (mipo$b / n_imp)) %>% .[2]
    ci_lo <- est - qt(.975, df = n_imp - 1) * SE
    ci_up <- est + qt(.975, df = n_imp - 1) * SE
    
    # add convergence parameters
    mild <- mids %>% mice::complete("all")
    qhat <-
      map_dbl(mild, ~ {
        lm(formula = Y ~ ., data = .) %>% .$coefficients %>% .[2]
      })
    lambda <- mild %>%
      purrr::map_dbl(., ~ {
        princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% . ^ 2 #first eigenvalue of the varcovar matrix
      })
    
    # save for each simulation condition
    out <- data.frame(
      mech = m_mech,
      p = p_inc,
      it = it_nr,
      est = est,
      CIW = ci_up - ci_lo,
      cov = ci_lo < Q & Q < ci_up,
      qhat = t(qhat),
      lambda = t(lambda)
    )
    
    # save in global environment
    if (it_nr == it_total) {
      chainmeans <<- c(chainmeans, list(mids$chainMean))
      chainvars <<- c(chainvars, list(mids$chainVar))
      mids <<- NULL
    }
    return(out)
  }

# # test
# impute(amp = amp, mids = mids, m_mech = "MCAR", p_inc = 0.5, it_nr = 1, n_imp = 5)
