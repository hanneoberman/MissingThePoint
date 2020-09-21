# data generating

generate <- function(N, vcov){
dat <<- mvtnorm::rmvnorm(n = N, sigma = vcov) %>%
  as.data.frame() %>%
  setNames(c("Y", "X1", "X2")) #%>%
  #mutate(Y = Y + rnorm(N))
Q <<- lm(Y ~ ., dat) %>%
  broom::tidy() %>%
  .[["estimate"]] %>%
  .[2]
r2 <<- lm(Y ~ ., dat) %>% 
  summary() %>% 
  .[["r.squared"]]
}
