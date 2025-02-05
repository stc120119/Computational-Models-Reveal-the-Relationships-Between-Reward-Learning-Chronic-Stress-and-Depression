
library(rstan)
library(loo)
set.seed(123)

setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')

load("behavior_data.Rdata")
nSubjects <- dim(data_data)[1]
nTrials <- dim(data_data)[2]

# 
dataList <- list(nSubjects=nSubjects,
                 nTrials=nTrials, 
                 choice=data_data[,,1], 
                 reward=data_data[,,2])

dataList$reward <- dataList$reward/10

# =============================================================================
#### Running Stan #### 
# =============================================================================
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/stanfile/')
modelFile1 <- 'hyper_rwpn.stan'   # 
modelFile2 <- 'hyper_rw.stan'   # 
modelFile3 <- 'hyper_phpn.stan'   # 
modelFile4 <- 'hyper_ph.stan'   # 
 

nIter     <- 10000
nChains   <- 6 
nWarmup   <- floor(nIter/2)
nThin     <- 1

### ==================================================model1=====================================================  
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/stanfile/')
cat("Estimating", modelFile1, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_hyper_rwpn <- stan(modelFile1, 
               data    = dataList, 
               chains  = nChains,
               iter    = nIter,
               warmup  = nWarmup,
               thin    = nThin,
               control = list(adapt_delta = 0.99),
               init    = "random",
               
)

cat("Finishing", modelFile1, "model simulation ... \n")
endTime = Sys.time(); print(endTime)

sum_hyper_rwpn <- summary(fit_hyper_rwpn)
log_rwpn <- extract_log_lik(fit_hyper_rwpn)
waic_rwpn <- waic(log_rwpn)
loo_hyper_rwpn <- loo(fit_hyper_rwpn,cores = 1)
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')
save(waic_rwpn,file = 'waic_rwpn.Rdata')
save(sum_hyper_rwpn,file = 'sum_hyper_rwpn.Rdata')
save(loo_hyper_rwpn,file = 'loo_hyper_rwpn.Rdata')

### ==================================================model2=====================================================  
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/stanfile/')
cat("Estimating", modelFile2, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_hyper_rw <- stan(modelFile2, 
                       data    = dataList, 
                       chains  = nChains,
                       iter    = nIter,
                       warmup  = nWarmup,
                       thin    = nThin,
                       control = list(adapt_delta = 0.99),
                       init    = "random",
                       
)

cat("Finishing", modelFile1, "model simulation ... \n")
endTime = Sys.time(); print(endTime)

sum_hyper_rw <- summary(fit_hyper_rw)
loo_hyper_rw <- loo(fit_hyper_rw,cores = 1)
log_rw <- extract_log_lik(fit_hyper_rw)
waic_rw <- waic(log_rw)
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')
save(waic_rw,file = 'waic_rw.Rdata')
save(sum_hyper_rw,file = 'sum_hyper_rw.Rdata')
save(loo_hyper_rw,file = 'loo_hyper_rw.Rdata')

### ==================================================model3=====================================================  
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/stanfile/')
cat("Estimating", modelFile3, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_hyper_phpn <- stan(modelFile3, 
                     data    = dataList, 
                     chains  = nChains,
                     iter    = nIter,
                     warmup  = nWarmup,
                     thin    = nThin,
                     control = list(adapt_delta = 0.99),
                     init    = "random",
                     
)

cat("Finishing", modelFile1, "model simulation ... \n")
endTime = Sys.time(); print(endTime)

sum_hyper_phpn <- summary(fit_hyper_phpn)
loo_hyper_phpn <- loo(fit_hyper_phpn,cores = 1)
log_phpn <- extract_log_lik(fit_hyper_phpn)
waic_phpn <- waic(log_phpn)
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')
save(waic_phpn,file = 'waic_phpn.Rdata')
save(sum_hyper_phpn,file = 'sum_hyper_phpn.Rdata')
save(loo_hyper_phpn,file = 'loo_hyper_phpn.Rdata')

### ==================================================model4=====================================================  
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/stanfile/')
cat("Estimating", modelFile4, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_hyper_ph <- stan(modelFile3, 
                       data    = dataList, 
                       chains  = nChains,
                       iter    = nIter,
                       warmup  = nWarmup,
                       thin    = nThin,
                       control = list(adapt_delta = 0.99),
                       init    = "random",
                       
)

cat("Finishing", modelFile1, "model simulation ... \n")
endTime = Sys.time(); print(endTime)

sum_hyper_ph <- summary(fit_hyper_ph)
loo_hyper_ph <- loo(fit_hyper_ph,cores = 1)
log_ph <- extract_log_lik(fit_hyper_ph)
waic_ph <- waic(log_ph)
setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')
save(waic_ph,file = 'waic_ph.Rdata')
save(sum_hyper_ph,file = 'sum_hyper_ph.Rdata')
save(loo_hyper_ph,file = 'loo_hyper_ph.Rdata')