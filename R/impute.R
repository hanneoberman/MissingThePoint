# Impute the data
impute <-
  function(amp,
           mids,
           m_mech,
           p_inc,
           it_nr,
           it_total = 50,
           n_imp = 5) {
    
    # initialize the mids object in the first iteration
    if (it_nr == 1) {
      mids <<- mice(
        amp[[m_mech]][[p_inc]],
        m = n_imp,
        method = "norm",
        maxit = 1,
        printFlag = FALSE
      )
    }
    
    # append to the mids object in any further iteration
    if (it_nr > 1) {
      mids <<- mice.mids(mids, printFlag = FALSE)
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
    # if (it_nr == it_total) {
    #   chain_means <<-
    #     data.frame(
    #       mech = rep(m_mech, it_total),
    #       p = rep(p_inc, it_total),
    #       it = 1:it_total,
    #       chain.mean.Y = mids$chainMean["Y", ,],
    #       chain.mean.X1 = mids$chainMean["X1", ,],
    #       chain.mean.X2 = mids$chainMean["X2", ,]
    #     )
    #   
    #   chain_vars <<-
    #     data.frame(
    #       mech = m_mech,
    #       p = p_inc,
    #       it = 1:it_total,
    #       chain.var.Y = mids$chainVar["Y", ,],
    #       chain.var.X1 = mids$chainVar["X1", ,],
    #       chain.var.X2 = mids$chainVar["X2", ,]
    #     )
    # }
    return(out)
  }

# # test
# impute(amp = amp, mids = mids, m_mech = "MCAR", p_inc = 0.5, it_nr = 1, n_imp = 5)
