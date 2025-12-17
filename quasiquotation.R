# Set up --------------------------------------------------------
library(rlang)
library(lobstr)
library(purrr)
#---------------------------------------------------------------#

# Quoting ------------------------------------------------------
#> Capturing an expression without evaluating it
expr(x + y)
expr(1 / 2 / 3)

f2 <- function(x) enexpr(x)
f2( a +b + c)

f3 <- function(...) enexprs(...)
f3
fn_env(f3)
env_print(f3)
f3(x = 1, y = 10 * z)

exprs(x = x ^ 2, y = y ^ 3, z = z ^ 4)
expr

f1 <- function(x, y) {
  exprs(x = x, y = y)
}
f2 <- function(x, y) {
  enexprs(x = x, y = y)
}
f1(a + b, c + d)
f2(a + b, c + d)

# Unquoting ----------------------------------------------------
x <- expr(-1)
lobstr::ast(x)
expr(f(!!x, y))

a <- sym("y")
a
b <- 1
expr(f(!!a, !!b))

mean_rm <- function(var) {
  var <- ensym(var)
  expr(mean(!!var, na.rm = TRUE))
}

mean_rm(x)
expr(!!mean_rm(x) + !!mean_rm(y))

x <- expr(x)
expr(`$`(df, !!x))
