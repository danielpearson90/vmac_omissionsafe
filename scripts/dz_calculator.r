dz_calculator <- function(x,y) {
  
  out <- tibble(contrast = x - y) %>%
    summarise(mean_contrast = mean(contrast), sd_contrast = sd(contrast), d_z = mean_contrast/sd_contrast) %>% pull(d_z)
  return(out)
  
  
}