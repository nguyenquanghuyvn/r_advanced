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

# Example --------------------------------------------
var <- expr(y)
intercept <- 10
coefs <- c(x1 = 5, x2 = -4)

coef_sym <- map(seq_along(coefs), ~ expr((!!var)[[!!.x]]))
coef_sym

linear <- function(var, val) {
  var <- ensym(var)
  coef_name <- map(seq_along(coefs[-1]), ~expr((!!var)[[!!.x]]))

  summands <- map2(val[-1], coef_name, ~ expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)
  reduce(summands, ~ expr(!!.x + !!.y))
}

linear(x, c(10, -1, 2))

## Slicing an array -------------------------------------------------------
indices <- rep(list(missing_arg()), 3)
indices

expr(x[!!!indices])

## Creating functions -----------------------------------------------------
 new_function(
  exprs(x = , y =),
  expr({x + y})
 )

power <- function(exponent) {
  new_function(
      exprs(x = ),
      expr({
        x ^ !!exponent
      }),
      caller_env()
    )
 }

power(0.5)
t <- power(0.5)
t(2)

curve2 <- function(expr, xlim = c(0, 1), n = 100) {
  expr <- enexpr(expr)
  f <- new_function(exprs(x = ), expr)

  x <- seq(xlim[1], xlim[2], length = n)
  y <- f(x)

  plot(x, y, type = "l", ylab = expr_text(expr))
}

curve2(sin(exp(4 * x)), n = 1000)
