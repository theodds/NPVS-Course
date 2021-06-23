set.seed(1)

n = 100
p = 100

x = matrix(rnorm(n*p), n, p)
beta = c(1,0.5, 0.5, -0.4, rep(0, p-4))

y = x %*% beta + rnorm(n, sd=1)

## How many MCMC scans we want to keep
nBurn=200
nScans=1000
thin=4
nChains=2

## Hyperparameter values for residual variance
a=0.001
b=0.001

## Hyperparameter values for inclusion probability, tau
c=2
d=dim(x)[2]

## Hyperparameter values for slab variance
e=0.5
f=0.5

## Create arrays to store MCMC values
sigmaPost = matrix(NA, nChains, nScans)
gammaPost = array(NA, dim=c(nChains, nScans, p))
betaPost = array(NA, dim=c(nChains, nScans, p+1))
tauPost = matrix(NA, nChains, nScans)
sigmaBetaPost = matrix(NA, nChains, nScans)

## Starting values
sigmaPost[,1] = rgamma(nChains, 2, 2)
gammaPost[,1,] = rbinom(nChains*p, 1, p=0.01)
for (nc in 1 : nChains) {
  betaPost[nc,1,] = c(rnorm(1), rnorm(p, sd=0.2)*gammaPost[nc,1,])
}
tauPost[,1] = rbeta(nChains, 1, 1)
sigmaBetaPost[,1] = rgamma(nChains, 1, 1)

## Create a design matrix that includes column of ones
design = cbind(rep(1, n), x)

for (ni in 2 : nScans) {
  for (nc in 1 : nChains) {
    if (nc == 1 & ni %% 100 == 0) print(paste(ni, "MCMC scans have finished"))
    
    ## Update sigma squared
    aStar = a + n/2
    bStar = b + sum((y - (design %*% betaPost[nc,ni-1,]))^2)/2
    sigmaPost[nc,ni] = 1/rgamma(1,aStar,bStar)
    
    ## Update tau
    tauPost[nc,ni] = rbeta(1, c + sum(gammaPost[nc,ni-1,] == 1),
                           d + sum(gammaPost[nc,ni-1,] == 0))
    
    ## Update sigmaBeta
    sigmaBetaPost[nc,ni] = 1/rgamma(1, e + sum(gammaPost[nc,ni-1,])/2,
                                    f + sum(betaPost[nc,ni-1,-1]^2)/2)
    
    ## Update regression coefficients and variable inclusion parameters
    tempBeta = betaPost[nc,ni-1,]
    for (j in 1 : p) {
      ## Create residual that removes effect of remaining j-1 covariates
      yStar = y - design[,-(j+1)] %*% tempBeta[-(j+1)]
      
      ## Prior variance
      priorVar = sigmaBetaPost[nc,ni]
      
      ## log probability of being in group zero
      log_p0 = log(1 - tauPost[nc,ni])
      
      ## log probability of being in top group
      ## First find conditional mean and variance of beta
      postVar = 1/(t(design[,j+1]) %*% design[,j+1]  / sigmaPost[nc,ni] +
                     1/priorVar)
      postMean = postVar %*% (t(design[,j+1]) %*% yStar/sigmaPost[nc,ni])
      
      ## Now calculate the probability of inclusion
      log_p1 = log(tauPost[nc,ni]) + dnorm(0, mean=0, sd=sqrt(priorVar), log=TRUE) -
        dnorm(0, mean=postMean, sd=sqrt(postVar), log=TRUE)
      
      ## Sometimes R runs into errors if this log probability is too large so 
      ## we standardize it here
      maxlog = max(log_p0,log_p1)
      
      p0 = exp(-maxlog + log_p0)
      p1 = exp(-maxlog + log_p1)
      
      gammaPost[nc,ni,j] = sample(0:1, size=1, p=c(p0,p1))
      
      tempBeta[j+1] = 0
      if (gammaPost[nc,ni,j] == 1) tempBeta[j+1] = rnorm(1, mean=postMean, sd=sqrt(postVar))
    }
    betaPost[nc,ni,] = tempBeta
    
    ## Update intercept
    yStar = y - design[,-1] %*% betaPost[nc,ni,-1]
    betaPost[nc,ni,1] = rnorm(1, mean(yStar), sd=sqrt(sigmaPost[nc,ni]/n))
  }
}

keep = seq(nBurn + 1, nScans, by=thin)

apply(gammaPost[,keep,], 3, mean)
apply(betaPost[,keep,], 3, mean)

mean(sigmaPost[,keep])
mean(sigmaBetaPost[,keep])
mean(tauPost[,keep])
