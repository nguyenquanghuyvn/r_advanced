# Set up ---------------------------------------------
library(rlang)
library(lobstr)
#-----------------------------------------------------#

# Code is data ---------------------------------------
expr(mean(x, na.rm = TRUE))
expr(10 - 3 - 1 * 10)

capture_it <- function(x) {
  enexpr(x)
}

capture_it(1 + 2 +3)
capture_it(mean(c))

f <- expr(f(x = 1, y = 2))
f

f$z <- 5
f
f$x <- NULL
f[[2]] <- 3
f

# Code is a tree ---------------------------------------
lobstr::ast(f(a, "b"))
lobstr::ast(f1(f2(a, b), f3(1, f4(2))))

# Code can generate code -------------------------------
cv <- function(var) {
  var <- enexpr(var)
  expr(sd(!!var) / mean(!!var))
}
cv(x)
cv(x + y)

lobstr::ast(cv(x))

# Customising evaluation with functions ------------------
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )

  eval(enexpr(x),  e)
}

name <- "Hadley"
string_math("Hello " + name)

# Quosure -------------------------------------------
with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enexpr(expr), data = df)
}
df <- data.frame(x = 1:3)
a <- 10
with2(df, x + a)
