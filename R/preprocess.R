# preprocess theta values
preprocess <- function(theta, ext, ...) {
  out <- map_dfr(1:n_sim, function(i){
    map_dfr(m_mech, function(mm) {
      map_dfr(p_inc, function(pp) {
      theta[[i * which(m_mech == mm) * which(p_inc == pp)]] %>%
        as.data.frame() %>% t() %>% as.data.frame() %>%
        setNames(paste0(ext, names(.))) %>%
        cbind(
          sim = i,
          mech = mm,
          p = pp,
          it = 1:it_total,
          m = rep(1:n_imp, each = it_total),
          .
        )
    })
  })
  })
  rownames(out) <- NULL
  return(out)
}
