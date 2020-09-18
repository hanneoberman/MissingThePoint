# data generating

generate <- function(N, vcov){
dat <<- mvtnorm::rmvnorm(N, sigma = vcov) %>%
  as.data.frame() %>%
  setNames(c("Y", "X1", "X2")) %>%
  mutate(Y = Y + rnorm(N, sd = 1))
Q <<- lm(Y ~ X1 + X2, dat) %>%
  broom::tidy() %>%
  .[["estimate"]] %>%
  .[2]
r2 <<- lm(Y ~ X1 + X2, dat) %>% 
  summary() %>% 
  .[["r.squared"]]
}
