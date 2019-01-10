##################################################################################################################
# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##################################################################################################################
# Function that builds a linear model equation to add to a plot
lm_eqn = function(m) {
  
  foo <- summary(m)
  
  l <- list(b0 = round(foo$coefficients[1], digits = 2),
            b1 = round(foo$coefficients[2], digits = 2),
            r2 = round(foo$r.squared, digits = 3));
  
  eq <- substitute(italic(y) == b1 %.% italic(x)~"+"~b0~","~italic(r)^2~"="~r2, l)
  as.character(as.expression(eq))
}