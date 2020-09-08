# Improved simulation for thesis manuscript

# setup simulation environment
source("R/setup.R")
source("R/impute.R")
setup(seed = 1111)
it_total <- 4
n_sim <- 3
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
  it_total)})
  })
})
return(out)
}

# run simulation n_sim times
res <-
  replicate(n = n_sim,
            expr = simulate(dat, m_mech, p_inc, amp_pat, it_total, chainmeans, chainvars),
            simplify = FALSE)

# extract chain means and chain variances
mus <- preprocess(theta = chainmeans, ext = "mu.")
sigmas <- preprocess(theta = chainvars, ext = "sigma.")

# apply convergence diagnostics
# [some code here]

# summarize results
tab <- map_df(res, ~ {
  as.data.frame(.)
}) %>% aggregate(. ~ it + p + mech, data = ., mean)
