##' Convert a proportion and credible interval to a numerator and denominator
##'
##' Estimate the number of events and denominator that contain roughly equivalent information to an estimate and uncertainty interval for a proportion, by interpreting the estimate and interval as a Beta posterior arising from a vague Beta(0.5,0.5) prior updated with the data consisting of that number and denominator. 
##'
##' Based on fitting a Beta distribution by least squares, using the method provided by the \pkg{SHELF} package.
##'
##' Requires that the estimate and upper and lower limits are all distinct (except that \code{est=0} is allowed and handled specially for convenience, see \code{denom0}).  Vectors of estimates and limits may be supplied. 
##'
##' @param est Point estimate
##'
##' @param lower Lower 95\% credible limit 
##'
##' @param upper Upper 95\% credible limit 
##'
##' @param epsilon If any of \code{lower} are zero, then they are replaced by the minimum of \code{epsilon} and \code{est/2}.  Similarly values of 1 for \code{upper} are replaced by the maximum of \code{1-epsilon} and \code{(1+est)/2}.
##'
##' @param denom0 Denominator to use as a default when the point estimate is exactly 0 or 1 (which is not compatible with the beta distribution).  Should correspond to a guess of the population size used to produce the estimate,
##' which should be no greater than the actual population of the area, and usually less.  Should be either a scalar, or a vector of the same length as \code{est} (though note if it is a vector, then only the elements where \code{est} is 1 or 0 get used). 
##'
##'
##' @return A data frame with elements \code{num} and \code{denom} corresponding to the supplied estimate and limits. 
##' 
##' @examples 
##' est <- 3.00 / 100 
##' upper <- 3.52 / 100 
##' lower <- 2.60 / 100 
##' ci2num(est, lower, upper)
##'
##' @references Oakley (2020). SHELF: Tools to Support the Sheffield Elicitation Framework. R package version 1.7.0. \url{https://CRAN.R-project.org/package=SHELF}
##'
##' @export
ci2num <- function(est, lower, upper, epsilon=0.5, denom0=1000){
    check_proportion_ci2num(est, "est")
    check_proportion_ci2num(lower, "lower")
    check_proportion_ci2num(upper, "upper")
    check_interval_ci2num(est, lower, upper)
    r <- n <- numeric(length(est))
    zeroinds = which(est==0)
    r[zeroinds] <- 0
    n[zeroinds] <- rep(denom0, length.out=length(est))[zeroinds]

    inds <- which(est > 0)
    if (length(inds) > 0) { 
        est <- est[inds]
        lower <- lower[inds]
        upper <- upper[inds]
        lower[lower==0] <- pmin(epsilon[lower==0], est[lower==0]/2)
        upper[upper==1] <- pmax(1-epsilon[upper==1], (1+est[upper==1])/2)
        vals <- rbind(lower, est, upper)
        probs <- c(0.025, 0.5, 0.975)
        bet <- SHELF::fitdist(vals=vals, probs=probs, lower=0, upper=1)$Beta
        apost <- bet$shape1
        bpost <- bet$shape2
        aprior <- bprior <- 0.5
        r[inds] <- round(apost - aprior)
        n[inds] <- round(bpost - bprior + r[inds])
    }
    data.frame(num=r, denom=n)
}

check_proportion_ci2num <- function(x, prefix){
    badprop <- which(x < 0 | x > 1)
    if (length(badprop) > 0) {
        stop(sprintf("%s[%s]=%s should be in [0,1]", 
                     prefix, badprop[1], x[badprop[1]]))
    }
}

check_interval_ci2num <- function(x, lower, upper){
    badint <- which(lower > upper)
    if (length(badint) > 0) {
        stop(sprintf("lower[%s]=%s, should be < upper[%s]=%s",
                     badint[1], lower[badint[1]], badint[1], upper[badint[1]]))
    }    
    badint <- which((x!=0) & (x <= lower | x >= upper))
    if (length(badint) > 0) {
        stop(sprintf("est[%s]=%s should be inside the credible interval of (lower[%s]=%s, upper[%s]=%s)",
                     badint[1], x[badint[1]], 
                     badint[1], lower[badint[1]], 
                     badint[1], upper[badint[1]]))
    }
}
