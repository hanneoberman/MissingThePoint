# Improved simulation for thesis manuscript

# setup simulation environment
source("R/setup.R")
source("R/impute.R")
source("R/preprocess.R")
setup(seed = 1111)
it_total <- 50
n_sim <- 100
n_imp <- 5

# create simulation function
simulate <- function(dat, m_mech, p_inc, amp_pat, it_total, n_imp, ...){
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
thetas <- mus %>% 
  dplyr::full_join(sigmas) %>% 
  dplyr::full_join(qhats) %>% 
  dplyr::full_join(lambdas) 

# apply convergence diagnostics to each theta
# thetas %>% 
#   select(sim, mech, p, m, it, "lambda") %>% 
#   filter(mech == "MCAR", p == 0.5, sim == 1) %>%  
#   pivot_wider(names_from = m, values_from = "lambda") %>% 
#   .[,-c(1:4)] %>% 
#   convergence()

# summarize results
results <- map_df(outcomes, ~ {
  as.data.frame(.)
}) %>% aggregate(. ~ it + p + mech, data = ., function(x){mean(x, na.rm = TRUE)})

save(thetas, file = "Data/parameters.Rdata")
save(outcomes, file = "Data/outcomes.Rdata")
save(results, file = "Data/results.Rdata")
