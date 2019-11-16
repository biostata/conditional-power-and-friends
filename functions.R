# prior construction
Normal <- function(mu, tau, mrv = -Inf) {
    lower <- max(mu - 10*tau, mrv)
    upper <- max(mu + 10*tau, mrv)
    if (upper == lower) stop('degenerate')
    normalizing_constant <- pnorm(upper, mu, tau) - pnorm(lower, mu, tau)
    res <- list(
        mu = mu, tau = tau, lower = lower, upper = upper,
        mrv = mrv, normalizing_constant = normalizing_constant)
    class(res) <- c('Normal', class(res))
    return(res)
}

condition <- function(prior, mrv) Normal(prior$mu, prior$tau, mrv)

posterior <- function(prior, estimate, n1) {
    mu    <- 1/(1/prior$tau^2 + n1/1) * (prior$mu/prior$tau^2 + n1*estimate)
    tau   <- sqrt(1/(1/prior$tau^2 + n1/1))
    Normal(mu, tau, prior$mrv)
}

pdf <- function(prior, Delta) ifelse(Delta < prior$mrv, 0, dnorm(Delta, prior$mu, prior$tau) / prior$normalizing_constant)

power <- function(Delta, n, c) 1 - pnorm(c, mean = sqrt(n) * Delta, sd = 1)

EP <- function(prior, n, c, mrv = 0) {
    cprior <- condition(prior, mrv)
    integrate(
        function(Delta) pdf(cprior, Delta) * power(Delta, n, c), cprior$lower, cprior$upper
    )$value
}

CEP <- function(prior, estimate, n1, n, c, mrv = 0) {
    cprior <- condition(prior, mrv)
    integrate(
        function(Delta) pdf(cprior, Delta) * power(Delta, n, c), cprior$lower, cprior$upper
    )$value
}

