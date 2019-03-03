# microO2: import and analysis of PyroScience microoxygen electrode date
# Copyright (c) 2017, Ph. Grosjean (Philippe.Grosjean@umons.ac.be)

microO2_import <- function(file, O2 = "Ch1", T = "('C)", P = "(mbar)",
time = "Time (s)", date = "Date", date.format = "%d.%m.%Y",
daytime = "Time (HH:MM:SS)", skip = 13, ...) {
  dat <- readr::read_tsv(file, skip = skip, ...)
  # Extract the data we need, and make a tibble from it

  res <- data.frame(
    O2   = dat[[O2]],
    T    = dat[[T]],
    P    = dat[[P]],
    time = dat[[time]],
    date = as.POSIXct(as.Date(dat[[date]], format = date.format)) + dat[[daytime]]
  )

  # Return a subclassed tbl_df object
  res <- dplyr::as_data_frame(res)
  structure(res, class = c("microO2", class(res)))
}

plot.microO2 <- function(x, y, xvar = "time", yvar = "O2", range = NULL,
xlab = xvar, ylab = yvar, type = "l", ..., grid = TRUE) {
  obj <- x
  x <- obj[[xvar]]
  if (missing(y))
    y <- obj[[yvar]]
  if (!is.null(range)) {
    range <- range(range)
    in_range <- x >= range[1] & x <= range[2]
    x <- x[in_range]
    y <- y[in_range]
  }
  res <- plot(x, y, xlab = xlab, ylab = ylab, type = type , ...)
  if (isTRUE(grid)) grid()
  invisible(res)
}

predict.microO2 <- function(object, xvar = "time", yvar = "O2",
range = c(Pn1 = NA, Pn2 = NA, Rl1 = NA, Rl2 = NA, Rd1 = NA, Rd2 = NA), ...) {
  time <- object[[xvar]]
  if (is.null(time) || !is.numeric(time))
    stop("'xvar' is not found in 'object' or is not numeric")
  O2 <- object[[yvar]]
  if (is.null(O2) || !is.numeric(O2))
    stop("'yvar' is not found in 'object' or is not numeric")
  data <- dplyr::as_data_frame(data.frame(time = time, O2 = O2))
  class(data) <- c("microO2", class(data))
  if (any(is.na(range)))
    stop("you must provide actual values for 'range'")
  if (any(names(range) != c("Pn1", "Pn2", "Rl1", "Rl2", "Rd1", "Rd2")))
    stop("Names of 'range' must be 'Pn1', 'Pn2', 'Rl1', 'Rl2', 'Rd1', 'Rd2' in that order")
  if (!is.numeric(range))
    stop("'range' must be numeric")
  if (min(range) < min(time) || max(range) > max(time))
    stop("'range' is outside the actual '", xvar, "' measurments")

  # Pn
  Pn.lm <- lm(O2 ~ time, subset = time >= range[["Pn1"]] & time <= range[["Pn2"]])
  res <- c(Pn = coef(Pn.lm)[[2]])

  # Rl
  Rl.lm <- lm(O2 ~ time, subset = time >= range[["Rl1"]] & time <= range[["Rl2"]])
  res <- c(res , Rl = coef(Rl.lm)[[2]])

  # Rd
  Rd.lm <- lm(O2 ~ time, subset = time >= range[["Rd1"]] & time <= range[["Rd2"]])
  res <- c(res , Rd = coef(Rd.lm)[[2]])

  # P, P/Pn, Rl/Rd
  res <- c(P = res[["Pn"]] - res[["Rl"]], res)
  res <- c(res, P_Pn = res[["P"]]/res[["Pn"]], Rl_Rd = res[["Rl"]]/res[["Rd"]])

  # R^2 Pn, Rl, Rd
  res <- c(res, R2_Pn = summary(Pn.lm)$r.squared,
                R2_Rl = summary(Rl.lm)$r.squared,
                R2_Rd = summary(Rd.lm)$r.squared)

  # Return a predictMicroO2  object, with all required components
  structure(res, data = data, range = range,
    Pn.lm = Pn.lm, Rl.lm = Rl.lm, Rd.lm = Rd.lm,
    class = "predictMicroO2")
}

print.predictMicroO2 <- function(x, ...) {
  X <- as.numeric(x)
  names(X) <- names(x)
  print(X)
  invisible(x)
}

summary.predictMicroO2 <- function(object, ...) {
  res <- list()
  cat("- Pn:\n")
  print(res$Pn.lm <- summary(attr(object, "Pn.lm")))
  cat("\n- Rl:\n")
  print(res$Rl.lm <- summary(attr(object, "Rl.lm")))
  cat("\n- Rd:\n")
  print(res$Rd.lm <- summary(attr(object, "Rd.lm")))
  invisible(res)
}

plot.predictMicroO2 <- function(x, y, xlab = "time", ylab = "O2",
  grid = TRUE, vline = TRUE, legend.pos = "topright", ...) {
  data <- attr(x, "data")
  range <- attr(x, "range")
  res <- plot(data, range = range, xlab = xlab, ylab = ylab, grid = grid, ...)
  if (isTRUE(vline))
    abline(v = range, col = "grey", lty = 2)
  xdat <- c(range[["Pn1"]], range[["Pn2"]])
  lines(xdat, predict(attr(x, "Pn.lm"), newdata = list(time = xdat)),
    col = "darkgreen", lwd = 2)
  xdat2 <- c(range[["Rl1"]], range[["Rl2"]])
  lines(xdat2, predict(attr(x, "Rl.lm"), newdata = list(time = xdat2)),
    col = "red", lwd = 2)
  xdat3 <- c(range[["Rd1"]], range[["Rd2"]])
  lines(xdat3, predict(attr(x, "Rd.lm"), newdata = list(time = xdat3)),
    col = "darkblue", lwd = 2)
  if (!is.null(legend.pos) && legend.pos != "none")
    legend(legend.pos,
      legend = paste(c("Pn", "Rl", "Rd"), formatC(x[c("Pn", "Rl", "Rd")], digits = 3), sep = " = "),
      col = c("darkgreen", "red", "darkblue"), lwd = 2)
  invisible(res)
}

#test <- microO2_import("TestO2corail.txt")
#test
#plot(test)
#plot(test, range = c(20, 55))
## Plages d'utilisation de la courbe:
#rng <- c(Pn1 = 24, Pn2 = 38, Rl1 = 40, Rl2 = 43, Rd1 = 46, Rd2 = 50)
#test_res <- predict(test, range = rng)
#test_res
#summary(test_res)
#plot(test_res, main = "Just a test...")
