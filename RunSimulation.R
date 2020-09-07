# Improved simulation for thesis manuscript

# setup simulation environment
source("R/setup.R")
setup(seed = 1111)

# ampute the data
amp <- map(m_mech, function(mm) {
  map(p_inc, function(pp) {
  mice::ampute(
    data = dat,
    prop = pp,
    patterns = amp_pat,
    mech = "MCAR"
  )$amp
})%>% set_names(as.character(p_inc))}) %>% set_names(as.character(m_mech))

