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
  }) %>% set_names(as.character(p_inc))
}) %>% set_names(as.character(m_mech))

results <- map_dfr(m_mech, function(mm) {
  map_dfr(p_inc, function(pp) {
    map_dfr(1:50, function(it) {
  impute(amp,
  mids,
  m_mech = mm,
  p_inc = pp,
  it_nr = it)})
  })
})
