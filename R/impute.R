# Impute the data
impute <- function(amp, m_mech, p_inc, it_nr, imp_nr = 5){
mids <- mice(amp[[m_mech]][[p_inc]], 
          m = imp_nr,
          method = "norm",
          maxit = it_nr, 
          printFlag = FALSE)

mipo <- mids %>% 
  mice::lm.mids(Y ~ ., .) %>% 
  mice::pool() %>% 
  .$pooled

est <- mipo$estimate[2]
SE <- sqrt(mipo$b + (mipo$b / mipo$m)) %>% .[2]
ci_lo <- est - qt(.975, df = imp_nr - 1) * SE 
ci_up <- est + qt(.975, df = imp_nr - 1) * SE 

pca <- mids %>% mice::complete("all") %>% 
  purrr::map_dbl(., ~ {
    princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% . ^ 2 #first eigenvalue of the varcovar matrix
  })

out <- data.frame(
  mm = m_mech,
  p = p_inc,
  it = it_nr,
  est = est,
  CIW = ci_up - ci_lo,
  cov = ci_lo < Q & Q < ci_up,
  pca = t(pca)
)

return(out)
}

# test
impute(amp = amp, m_mech = "MCAR", p_inc = 0.5, it_nr = 1, imp_nr = 5)
