#' Summary survival curve from aggregated survival data of a meta-analysis
#'
#' Estimation of the summary survival curve from the survival rates and the numbers of at-risk individuals extracted from studies of a meta-analysis.
#'
#' @param study A numeric vector with the numbering of the studies included in the meta-analysis. The numbering of a study is repeated for each survival probabilities extracted from this study.
#' @param time A numeric vector with the time at which the survival probabilities are collected.
#' @param n.risk A numeric vector with the number of at-risk patients in the study for each value of thr time.
#' @param surv.rate A numeric vector with the survival rates collected per study for each value of time.
#' @param confidence A text argument indicating the method to calculate the confidence interval of the summary survival probabilities: "Greenwood" or "MonteCarlo".
#' @param correctionFlag A logical variable which takes into account if user wants the continuity correaction or not (By default TRUE).
#' @param correctionVal A numeric vector for continuity correction, if you don't want to apply correction pass c(0,0).
#'
#' @return list
#' @export
#' @importFrom stats quantile
#' @author Shubhram Pandey \email{shubhram.pandey@@PAREXEL.com}
msurv <- function(study, time, n.risk, surv.rate, confidence,correctionFlag = TRUE, correctionVal = c(0.25,0.5))
{
  if (length(correctionVal) != 2) stop("Variable correctionVal should be a numeric vector of length 2")
  if (correctionFlag  & any(correctionVal <= 0)) stop("Both values of continuity correction should be greater than zero.")
  .data <- data.frame(study, time, n.risk, surv.rate)
  .data <- .data[order(study, time), ]
  study <- .data$study
  time <- .data$time
  n.risk <- .data$n.risk
  surv.rate <- .data$surv.rate
  IndiceStudies <- sort(unique(study))
  NbStudies <- length(IndiceStudies)
  IndiceTimes <- sort(unique(time))
  NbTimes <- length(IndiceTimes)
  TimeMax <- max(IndiceTimes)
  verif <- function(x) {
    ref.study <- sort(time[study == x])
    ref.data <- IndiceTimes[IndiceTimes <= max(ref.study)]
    return(1 * (sum(ref.study != ref.data) == 0))
  }
  verif.data <- data.frame(Sstudy = IndiceStudies, check = sapply(IndiceStudies,
                                                                  FUN = "verif"))
  if (sum(verif.data$check == 0) > 0) {
    return(list(verif.data = verif.data, summary.fixed = NA,
                median.fixed = NA, mean.fixed = NA, heterogeneity = NA,
                summary.random = NA, median.random = NA, mean.random = NA))
  }
  else {
    time.init <- min(time[time > 0])
    CondSurv <- c(-99, surv.rate[2:length(surv.rate)]/surv.rate[1:(length(surv.rate) -
                                                                     1)])
    CondSurv[time == time.init] <- surv.rate[time == time.init]
    if (correctionFlag) {
      ArcSineCondSurv <- asin(sqrt((n.risk * CondSurv + correctionVal[1])/(n.risk + correctionVal[2])))
      VarArcSineCondSurv <- correctionVal[1] * (n.risk + correctionVal[2]) ^ -1
    } else {
      ArcSineCondSurv <- asin(sqrt((n.risk * CondSurv)/(n.risk)))
      VarArcSineCondSurv <- 1 / n.risk
    }
    ys <- matrix(nrow = NbStudies, ncol = NbTimes)
    for (i in 1:NbStudies) ys[i, 1:sum(study == IndiceStudies[i])] <- ArcSineCondSurv[study ==
                                                                                        IndiceStudies[i]]
    for (i in 1:NbTimes) ys[is.na(ys[, i]), i] <- mean(ys[!is.na(ys[,
                                                                    i]), i])
    vars <- matrix(nrow = NbStudies, ncol = NbTimes)
    for (i in 1:NbStudies) vars[i, 1:sum(study == IndiceStudies[i])] <- VarArcSineCondSurv[study ==
                                                                                             IndiceStudies[i]]
    for (i in 1:NbTimes) vars[is.na(vars[, i]), i] <- 10^12
    mat <- matrix(0, nrow = NbTimes, ncol = NbTimes)
    maty <- matrix(0, nrow = NbTimes, ncol = 1)
    matfixedeffect <- matrix(0, nrow = NbTimes, ncol = NbTimes)
    matyfixedeffect <- matrix(0, nrow = NbTimes, ncol = 1)
    esttrun <- matrix(0, nrow = NbTimes, ncol = NbTimes)
    estmat <- matrix(0, nrow = NbTimes, ncol = NbTimes)
    count <- 1
    for (i in 1:(NbTimes - 1)) {
      for (j in (i + 1):NbTimes) {
        crossses <- (vars[, i] * vars[, j])^0.5
        mean1a <- sum(ys[, i]/crossses)/sum(1/crossses)
        mean2a <- sum(ys[, j]/crossses)/sum(1/crossses)
        Q <- sum((ys[, i] - mean1a) * (ys[, j] - mean2a)/crossses)
        bb <- sum(1/crossses) - sum(1/(crossses^2))/sum(1/crossses)
        est <- Q/bb
        estmat[i, j] <- est
      }
    }
    estmat <- estmat + t(estmat)
    for (i in 1:NbTimes) {
      cross_ses <- vars[, i]
      mean1a <- sum(ys[, i]/vars[, i])/sum(1/vars[, i])
      Q <- sum((ys[, i] - mean1a) * (ys[, i] - mean1a)/vars[,
                                                            i])
      rhos <- rep(1, NbStudies)
      check <- (1 - 1 * (vars[, i] == 10^12))
      rhos <- rhos * check
      aa <- sum(rhos) - sum(rhos/vars[, i])/sum(1/vars[,
                                                       i])
      bb <- sum(1/vars[, i]) - sum(1/(vars[, i]^2))/sum(1/vars[,
                                                               i])
      est <- (Q - aa)/bb
      estmat[i, i] <- est
    }
    eig <- eigen(estmat)
    for (i in 1:NbTimes) esttrun <- esttrun + max(0, eig$values[i]) *
      eig$vectors[, i] %*% t(eig$vectors[, i])
    for (k in 1:NbStudies) {
      within <- diag(vars[k, ])
      invwithin <- diag(1/vars[k, ])
      matfixedeffect <- matfixedeffect + invwithin
      mat1 <- within + esttrun
      mat1 <- qr.solve(mat1, diag(rep(1, length(mat1[,
                                                     1]))))
      mat <- mat + mat1
      y <- matrix(ys[k, ], nrow = NbTimes, ncol = 1)
      mat2 <- mat1 %*% y
      maty <- maty + mat2
      matyfixedeffect <- matyfixedeffect + invwithin %*%
        y
    }
    covrandomeffect <- solve(mat)
    betahatrandomeffect <- covrandomeffect %*% maty
    covfixedeffect <- solve(matfixedeffect)
    betahatfixedeffect <- covfixedeffect %*% matyfixedeffect
    HeterogeneityQ <- 0
    for (i in 1:NbStudies) {
      IndicHeterogeneity <- vars[i, ] != 10^12
      HeterogeneityQ <- HeterogeneityQ + t(ys[i, IndicHeterogeneity] -
                                             betahatfixedeffect[IndicHeterogeneity]) %*% (diag(1/vars[i,
                                                                                                      ]))[IndicHeterogeneity, IndicHeterogeneity] %*%
        (ys[i, IndicHeterogeneity] - betahatfixedeffect[IndicHeterogeneity])
    }
    HSquared <- HeterogeneityQ/(sum(as.vector(vars) != 10^12) -
                                  NbStudies)
    ISquared <- max(0, 100 * (HSquared - 1)/HSquared)
    HeterogeneityResults <- c(HeterogeneityQ, HSquared, ISquared)
    PooledSurvivalFE <- cumprod((sin(betahatfixedeffect))^2)
    PooledSurvivalRE <- cumprod((sin(betahatrandomeffect))^2)
    if (confidence == "Greenwood") {
      VarLogPooledSurvivalFE <- cumsum(4 * (cos(betahatfixedeffect)/sin(betahatfixedeffect))^2 *
                                         diag(covfixedeffect))
      PooledSurvivalICinfFE <- exp(log(PooledSurvivalFE) -
                                     1.96 * sqrt(VarLogPooledSurvivalFE))
      PooledSurvivalICsupFE <- exp(log(PooledSurvivalFE) +
                                     1.96 * sqrt(VarLogPooledSurvivalFE))
      CIPooledSurvivalFE <- cbind(PooledSurvivalICinfFE,
                                  PooledSurvivalICsupFE)
      DerivativeOfTransformation <- cos(betahatrandomeffect)/sin(betahatrandomeffect)
      VarLogPooledConditionalSurvivalProbaRE <- (DerivativeOfTransformation %*%
                                                   t(DerivativeOfTransformation)) * covrandomeffect
      MatriceTriangular <- diag(rep(1, length(betahatrandomeffect)))
      MatriceTriangular[upper.tri(MatriceTriangular)] <- 1
      VarLogPooledSurvivalRE <- 4 * (t(MatriceTriangular) %*%
                                       VarLogPooledConditionalSurvivalProbaRE %*% MatriceTriangular)
      PooledSurvivalICinfRE <- exp(log(PooledSurvivalRE) -
                                     1.96 * sqrt(diag(VarLogPooledSurvivalRE)))
      PooledSurvivalICsupRE <- exp(log(PooledSurvivalRE) +
                                     1.96 * sqrt(diag(VarLogPooledSurvivalRE)))
      CIPooledSurvivalRE <- cbind(PooledSurvivalICinfRE,
                                  PooledSurvivalICsupRE)
    }
    SimulatedArcSinCondSurvFE <- mvtnorm::rmvnorm(n = 10000, mean = betahatfixedeffect,
                                         sigma = covfixedeffect)
    SimulatedArcSinCondSurvRE <- mvtnorm::rmvnorm(n = 10000, mean = betahatrandomeffect,
                                         sigma = covrandomeffect)
    SimulatedPooledSurvFE <- apply((sin(SimulatedArcSinCondSurvFE))^2,
                                   1, cumprod)
    SimulatedPooledSurvRE <- apply((sin(SimulatedArcSinCondSurvRE))^2,
                                   1, cumprod)
    if (confidence == "MonteCarlo") {
      CIPooledSurvivalFE <- t(apply(SimulatedPooledSurvFE,
                                    1, quantile, probs = c(0.025, 0.975)))
      CIPooledSurvivalRE <- t(apply(SimulatedPooledSurvRE,
                                    1, quantile, probs = c(0.025, 0.975)))
    }
    if (PooledSurvivalFE[length(PooledSurvivalFE)] > 0.5)
      MedianTimeFE <- NA
    if (PooledSurvivalFE[length(PooledSurvivalFE)] == 0.5)
      MedianTimeFE <- IndiceTimes[length(IndiceTimes)]
    if (PooledSurvivalFE[length(PooledSurvivalFE)] < 0.5) {
      IndexMin <- max((1:length(PooledSurvivalFE))[PooledSurvivalFE >
                                                     0.5])
      IndexMax <- min((1:length(PooledSurvivalFE))[PooledSurvivalFE <
                                                     0.5])
      MedianTimeFE <- IndiceTimes[IndexMax] - (IndiceTimes[IndexMax] -
                                                 IndiceTimes[IndexMin]) * (PooledSurvivalFE[IndexMax] -
                                                                             0.5)/(PooledSurvivalFE[IndexMax] - PooledSurvivalFE[IndexMin])
    }
    MedianTimeFEbtp <- rep(NA, 10000)
    for (i in 1:10000) {
      if (SimulatedPooledSurvFE[length(PooledSurvivalFE),
                                i] < 0.5) {
        IndexMin <- max((1:length(SimulatedPooledSurvFE[,
                                                        i]))[SimulatedPooledSurvFE[, i] > 0.5])
        IndexMax <- min((1:length(SimulatedPooledSurvFE[,
                                                        i]))[SimulatedPooledSurvFE[, i] < 0.5])
        MedianTimeFEbtp[i] <- IndiceTimes[IndexMax] -
          (IndiceTimes[IndexMax] - IndiceTimes[IndexMin]) *
          (SimulatedPooledSurvFE[IndexMax, i] - 0.5)/(SimulatedPooledSurvFE[IndexMax,
                                                                            i] - SimulatedPooledSurvFE[IndexMin, i])
      }
    }
    if (sum(is.na(MedianTimeFEbtp)) == 0)
      CIMedianTimeFE <- quantile(MedianTimeFEbtp, probs = c(0.025,
                                                            0.975))
    if (sum(is.na(MedianTimeFEbtp)) > 0)
      CIMedianTimeFE <- c(NA, NA)
    if (PooledSurvivalRE[length(PooledSurvivalRE)] > 0.5)
      MedianTimeRE <- NA
    if (PooledSurvivalRE[length(PooledSurvivalRE)] == 0.5)
      MedianTimeRE <- IndiceTimes[length(IndiceTimes)]
    if (PooledSurvivalRE[length(PooledSurvivalRE)] < 0.5) {
      IndexMin <- max((1:length(PooledSurvivalRE))[PooledSurvivalRE >
                                                     0.5])
      IndexMax <- min((1:length(PooledSurvivalRE))[PooledSurvivalRE <
                                                     0.5])
      MedianTimeRE <- IndiceTimes[IndexMax] - (IndiceTimes[IndexMax] -
                                                 IndiceTimes[IndexMin]) * (PooledSurvivalRE[IndexMax] -
                                                                             0.5)/(PooledSurvivalRE[IndexMax] - PooledSurvivalRE[IndexMin])
    }
    MedianTimeREbtp <- rep(NA, 10000)
    for (i in 1:10000) {
      if (SimulatedPooledSurvRE[length(PooledSurvivalRE),
                                i] < 0.5) {
        IndexMin <- max((1:length(SimulatedPooledSurvRE[,
                                                        i]))[SimulatedPooledSurvRE[, i] > 0.5])
        IndexMax <- min((1:length(SimulatedPooledSurvRE[,
                                                        i]))[SimulatedPooledSurvRE[, i] < 0.5])
        MedianTimeREbtp[i] <- IndiceTimes[IndexMax] -
          (IndiceTimes[IndexMax] - IndiceTimes[IndexMin]) *
          (SimulatedPooledSurvRE[IndexMax, i] - 0.5)/(SimulatedPooledSurvRE[IndexMax,
                                                                            i] - SimulatedPooledSurvRE[IndexMin, i])
      }
    }
    if (sum(is.na(MedianTimeREbtp)) == 0)
      CIMedianTimeRE <- quantile(MedianTimeREbtp, probs = c(0.025,
                                                            0.975))
    if (sum(is.na(MedianTimeREbtp)) > 0)
      CIMedianTimeRE <- c(NA, NA)
    DureeIntervalle <- IndiceTimes[-1] - IndiceTimes[-length(IndiceTimes)]
    MeanTimeFE <- IndiceTimes[1] * (1 + PooledSurvivalFE[1])/2 +
      sum(DureeIntervalle * (PooledSurvivalFE[-length(PooledSurvivalFE)] +
                               PooledSurvivalFE[-1])/2)
    Temp <- (SimulatedPooledSurvFE[-length(PooledSurvivalFE),
                                   ] + SimulatedPooledSurvFE[-1, ])/2
    for (i in 1:(length(PooledSurvivalFE) - 1)) Temp[i, ] <- Temp[i,
                                                                  ] * DureeIntervalle[i]
    CIMeanTimeFE <- quantile(apply(Temp, 2, sum) + IndiceTimes[1] *
                               (SimulatedPooledSurvFE[1, ] + rep(1, 10000))/2, probs = c(0.025,
                                                                                         0.975))
    MeanTimeRE <- IndiceTimes[1] * (1 + PooledSurvivalRE[1])/2 +
      sum(DureeIntervalle * (PooledSurvivalRE[-length(PooledSurvivalRE)] +
                               PooledSurvivalRE[-1])/2)
    Temp <- (SimulatedPooledSurvRE[-length(PooledSurvivalRE),
                                   ] + SimulatedPooledSurvRE[-1, ])/2
    for (i in 1:(length(PooledSurvivalRE) - 1)) Temp[i, ] <- Temp[i,
                                                                  ] * DureeIntervalle[i]
    CIMeanTimeRE <- quantile(apply(Temp, 2, sum) + IndiceTimes[1] *
                               (SimulatedPooledSurvRE[1, ] + rep(1, 10000))/2, probs = c(0.025,
                                                                                         0.975))
    SummarySurvivalFE <- cbind(IndiceTimes, PooledSurvivalFE,
                               CIPooledSurvivalFE)
    MedianSurvivalTimeFE <- c(MedianTimeFE, CIMedianTimeFE)
    MeanSurvivalTimeFE <- c(MeanTimeFE, CIMeanTimeFE)
    SummarySurvivalRE <- cbind(IndiceTimes, PooledSurvivalRE,
                               CIPooledSurvivalRE)
    MedianSurvivalTimeRE <- c(MedianTimeRE, CIMedianTimeRE)
    MeanSurvivalTimeRE <- c(MeanTimeRE, CIMeanTimeRE)
    return(list(verif.data = verif.data, summary.fixed = SummarySurvivalFE,
                median.fixed = MedianSurvivalTimeFE, mean.fixed = MeanSurvivalTimeFE,
                heterogeneity = HeterogeneityResults, summary.random = SummarySurvivalRE,
                median.random = MedianSurvivalTimeRE, mean.random = MeanSurvivalTimeRE))
  }
}
