kalman_filter_method <- function(ts_train, ts_test) {
  
  #################################
  #1. Model Initialization
  ################################
  kalman_model <- SSModel(ts_train ~ SSMtrend(2, Q = list(NA, NA)) + 
                            SSMseasonal(12, Q = NA),
                          H = NA)
  kalman_model$a1[,1][1] <- ts_train[1]
  kalman_model$a1
  
  npar_Q <- 0
  if (!is.null(kalman_model$Q)) {
    npar_Q <- sum(sapply(kalman_model$Q, function(mat) sum(is.na(mat))))
  }
  npar_H <- sum(is.na(kalman_model$H))
  npar <- npar_Q + npar_H
  
  init_vals <- rep(0.1, npar)
  
  
  ################################
  #2. Model Fitting
  ################################
  fit <- fitSSM(kalman_model, inits = init_vals, method = "BFGS")
  fitted_model <- fit$model
  
  kfs <- KFS(fitted_model, smoothing = c("state", "disturbance", "mean"))
  
  smoothed_states <- kfs$alphahat
  level_smoothed <- smoothed_states[, 1]
  slope_smoothed <- smoothed_states[, 2] 
  
  

  ################################
  #3. Forecast / Prediction
  ################################
  h <- length(ts_test)
  fc <- predict(fitted_model, n.ahead = h, interval = "prediction", level = 0.95)
  fc_mean <- ts(fc[, "fit"], start = time(ts_train)[length(ts_train)] + 1/12, frequency = 12)
  fc_lwr  <- ts(fc[, "lwr"],  start = time(ts_train)[length(ts_train)] + 1/12, frequency = 12)
  fc_upr  <- ts(fc[, "upr"],  start = time(ts_train)[length(ts_train)] + 1/12, frequency = 12)
  
  
  ################################
  #4. Get residuals of the fitted model (Train RMSE)
  ################################
  resid_recursive <- residuals(kfs, type = "recursive"); resid_recursive
  rmse_train_recursive <- sqrt(mean(resid_recursive^2, na.rm = TRUE))
  cat("Train RMSE: ", rmse_train_recursive, "\n")
  
  
  
  ################################
  #5. Test RMSE
  ################################
  rmse <- sqrt(mean((as.numeric(fc_mean) - as.numeric(ts_test))^2))
  forecast::accuracy(fc_mean, ts_test)  # from forecast package
  cat("Test RMSE: ", rmse, "\n")
  
  
  ################################
  #Plot the graphs for residual
  ################################
  par(mfrow = c(2,2))
  plot(resid_recursive, main = "Recursive residuals")
  acf(resid_recursive, main = "ACF of recursive residuals")
  hist(resid_recursive, main = "Histogram residuals")
  qqnorm(resid_recursive); qqline(resid_recursive)
  
  
  ################################
  #Test whether the residuals of the model is random or not
  ################################
  Box.test(resid_recursive, lag = 24, type = "Ljung-Box")
  par(mfrow = c(1,1))
  
  
  
  ################################
  #Plot the fitted model on original series
  ################################
  # ts.plot(ts_train, level_smoothed, col = c("black", "blue"), lty = c(1,2),
  #         main = "Observed CO2 (black) and Smoothed Level (blue)")
  # legend("topleft", legend = c("Observed", "Smoothed level"), col = c("black","blue"),
         # lty = c(1,2), bty = "n")
  
  
  
  ################################
  #Plot the forecast
  ################################
  
  # plot(ts_train, xlim = c(time(ts_train)[1], time(fc_upr)[length(fc_upr)]), 
  #      ylim = range(c(ts_train, fc_lwr, fc_upr)), main = "CO2: observed + KFAS forecast", ylab = "ppm")
  # lines(fc_mean, col = "red", lty = 2, lwd = 2)
  # lines(fc_lwr, col = "red", lty = 3)
  # lines(fc_upr, col = "red", lty = 3)
  # legend("topleft", c("Observed", "Forecast mean", "95% PI"), col = c("black","red","red"),
  #        lty = c(1,2,3), bty = "n")
  
  
  
  
  
  
  
  plot(ts_data, 
       main = "Kalman Filter Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  lines(fitted(fitted_model), col = "red")
  lines(fc_mean, col = "blue")
  polygon(c(time(fc_mean), rev(time(fc_mean))),
          c(fc_lwr, rev(fc_upr)),   # 95% CI (2nd column)
          col = rgb(0, 0, 1, 0.2), border = NA)
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast", "95% CI"), 
         col = c("black", "red", "blue", rgb(0,0,1,0.2)), 
         lty = c(1,1,1, NA), 
         pch = c(NA, NA, NA, 15),
         pt.cex = 2,
         cex = 0.8)
  
  
  
}