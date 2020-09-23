# Plot example of convergence diagnostics and outcomes

# setup
source("~/MissingThePoint/R/setup_example.R")
setup_example(seed=11)
ests <- purrr::map_dfr(1:n_it, function(it){impute_once(iteration = it)})
param <- preprocess_thetas(mids$chainMean, ext = "mu") %>% 
  dplyr::full_join(
    preprocess_thetas(mids$chainVar, ext = "sigma") %>% 
      dplyr::full_join(qhats) %>% 
      dplyr::full_join(lambdas)
  )
convs <- get_diagnostics(param)
example <- full_join(ests, convs)
save(example, file = "Data/example.Rdata")
save(mids, file = "Data/example_mids.Rdata")
