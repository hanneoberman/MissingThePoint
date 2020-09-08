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
# ampute the data
# amp <- map(m_mech, function(mm) {
#   map(p_inc, function(pp) {
#     mice::ampute(
#       data = dat,
#       prop = pp,
#       patterns = amp_pat,
#       mech = mm
#     )$amp
#   }) %>% set_names(as.character(p_inc))
# }) %>% set_names(as.character(m_mech))

out <- map_dfr(m_mech, function(mm) {
  map_dfr(p_inc, function(pp) {
    # ampute the data
    amp <- mice::ampute(
      data = dat,
      prop = pp,
      patterns = amp_pat,
      mech = mm
    )$amp
    
    map_dfr(1:it_total, function(it) {
  # impute and extract results
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
res <-
  replicate(n = n_sim,
            expr = simulate(dat, m_mech, p_inc, amp_pat, it_total, chainmeans, chainvars),
            simplify = FALSE)

#a <- mids$chainMean %>% as.data.frame() %>% t() %>% as.data.frame() %>% cbind(it=1:it_total, m = rep(1:5, each = 5))
