# rhat function

dat = thetas
theta = "mu.Y"
n_it = nrow(dat) / max(dat$m)

between_chain <-
  dat %>% 
  group_by(m) %>% 
  summarise(mean(.data[[theta]])) %>% 
  .[, 2] %>% 
  var(.)

within_chain <-
  dat %>% 
  group_by(m) %>% 
  summarise(var(.data[[theta]])) %>% 
  .[, 2] %>% 
  t() %>% 
  mean(.)

rhat <-
  sqrt((n_it * between_chain / within_chain + n_it - 1) / n_it)
