# Improved simulation for thesis manuscript

# setup simulation environment
source("R/setup.R")
source("R/generate.R")
source("R/impute.R")
source("R/preprocess.R")
source("R/ComputeDiagnostics.R")
setup(seed = 1111)
it_total <- 50
n_sim <- 1000

# create simulation function
simulate <- function(m_mech, p_inc, amp_pat, it_total, n_imp, ...){
out <- 
  # for each missingness mechanism...
  map_dfr(m_mech, function(mm) {
  # for each proportion of incomplete cases...
    map_dfr(p_inc, function(pp) {
      generate(N=n_cases, vcov = varcov)
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
            expr = simulate(m_mech, p_inc, amp_pat, it_total, chainmeans, chainvars),
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

# get theta names for mapping
thetas <- names(parameters)[-c(1:5)]

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

# summarize results
conv_results <- convergence_diagnostics %>% 
  select(!sim) %>% 
  aggregate(. ~ it + p + mech, data = ., function(x){mean(x, na.rm = TRUE)}) 

results <- map_df(outcomes, ~ {
  as.data.frame(.)
}) %>% aggregate(. ~ it + p + mech, data = ., function(x){mean(x, na.rm = TRUE)}) %>% 
  full_join(., conv_results)

save(parameters, file = "Data/raw_parameters_25_75.Rdata")
save(outcomes, file = "Data/raw_outcomes_25_75.Rdata")
save(convergence_diagnostics, file = "Data/diagnostics_25_75.Rdata")
save(results, file = "Data/results_25_75.Rdata")
