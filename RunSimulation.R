# Improved simulation for thesis manuscript

# setup simulation environment
source("R/setup.R")
source("R/generate.R")
source("R/impute.R")
source("R/preprocess.R")
source("R/ComputeDiagnostics.R")
setup(seed = 1111)
it_total <- 50
n_sim <- 100

# create simulation function
simulate <- function(dat, m_mech, p_inc, amp_pat, it_total, n_imp, ...){
  generate(N=n_cases, vcov = varcov)
out <- 
  # for each missingness mechanism...
  map_dfr(m_mech, function(mm) {
  # for each proportion of incomplete cases...
    map_dfr(p_inc, function(pp) {
    # ampute the complete data
    amp <- mice::ampute(
      data = dat,
      prop = pp,
      patterns = amp_pat,
      mech = mm
    )$amp
    # for each number of iterations...
    map_dfr(1:it_total, function(it) {
  # impute the incomplete date and extract results
  impute(amp,
  m_mech = mm,
  p_inc = pp,
  it_nr = it,
  it_total,
  chainmeans,
  chainvars)})
  })
})
return(out)
}

# run simulation n_sim times
outcomes <-
  replicate(n = n_sim,
            expr = simulate(dat, m_mech, p_inc, amp_pat, it_total, chainmeans, chainvars),
            simplify = FALSE)

# preprocess theta values
mus <- preprocess(theta = chainmeans, ext = "mu.")
sigmas <- preprocess(theta = chainvars, ext = "sigma.")
qhats <- cbind(sim = rep(1:n_sim, each = n_imp*it_total*length(m_mech)*length(p_inc)), qhats)
lambdas <- cbind(sim = rep(1:n_sim, each = n_imp*it_total*length(m_mech)*length(p_inc)), lambdas)
parameters <- mus %>% 
  dplyr::full_join(sigmas) %>% 
  dplyr::full_join(qhats) %>% 
  dplyr::full_join(lambdas) 

# apply convergence diagnostics to each theta
convergence_diagnostics <- purrr::map_dfr(1:n_sim, function(ss) {
  purrr::map_dfr(m_mech, function(mm) {
    purrr::map_dfr(p_inc, function(pp) {
      purrr::map_dfc(thetas, function(tt) {
        one_theta <- parameters %>%
          filter(sim == ss, mech == mm, p == pp) %>%
          select(it, m, tt) %>%
          arrange(it)
        matrix(one_theta[[tt]], ncol = n_imp, byrow = T) %>%
          convergence() %>%
          select(r.hat.max, ac.max) %>%
          setNames(., paste0(names(.), ".", tt))
      }) %>% cbind(
        sim = ss,
        mech = mm,
        p = pp,
        it = 1:it_total,
        .
      )
    })
  })
})
# thetas %>% 
#   select(sim, mech, p, m, it, "lambda") %>% 
#   filter(mech == "MCAR", p == 0.5, sim == 1) %>%  
#   pivot_wider(names_from = m, values_from = "lambda") %>% 
#   .[,-c(1:4)] %>% 
#   convergence()

# summarize results
conv_results <- convergence_diagnostics %>% 
  select(!sim) %>% 
  aggregate(. ~ it + p + mech, data = ., function(x){mean(x, na.rm = TRUE)}) 

results <- map_df(outcomes, ~ {
  as.data.frame(.)
}) %>% aggregate(. ~ it + p + mech, data = ., function(x){mean(x, na.rm = TRUE)}) %>% 
  full_join(., conv_results)

save(parameters, file = "Data/parameters_DGM.Rdata")
save(outcomes, file = "Data/outcomes_DGM.Rdata")
save(results, file = "Data/results_DGM.Rdata")

# save(parameters, file = "Data/parameters.Rdata")
# save(outcomes, file = "Data/outcomes.Rdata")
# save(results, file = "Data/results.Rdata")
