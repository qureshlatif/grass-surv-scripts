library(jagsUI)
library(stringr)
library(dplyr)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_SPPI.RData")

#________ Script inputs________#
scripts.loc <- "grass-surv-scripts/"
model.file <- str_c(scripts.loc, "model_CJSRL_no_structure.jags")

# MCMC values
nc <- 3 # number of chains
nb <- 1000 # burn in
ni <- 10000 # number of iterations
nt <- 10 # thinning

save.out <- "mod_CJSRL_SPPI"
#______________________________#

# Data objects to send to JAGS
data.nams <- c("Y.alive", "Y.dead", "first", "last",
               "nBird", "nDOS")

# Stuff to save from JAGS
parameters <- c("S", "p", "psi")

# Compile data #
ymat <- data.SPPI$ymat
first <- data.SPPI$Covs$firstDay
last <- data.SPPI$Covs$lastDay
lastAlive <- data.SPPI$Covs$lastAlive
nBird <- nrow(ymat)

ymat <- ymat[,-which(apply(ymat, 2, function(x) sum(!is.na(x))) == 0)]
last <- last - min(first) + 1
lastAlive <- lastAlive - min(first) + 1
first <- first - min(first) + 1
nDOS <- ncol(ymat)

Y.alive <- (ymat == 1)*1
Y.dead <- (ymat == 2)*1
z.init <- (ymat == 1)*1
for(i in 1:length(first)) {
  z.init[i, (first[i]+1):lastAlive[i]] <- 1
  z.init[i, first[i]] <- NA
  if(any(ymat[i, ] == 2, na.rm = T)) z.init[i, which(ymat[i, ] == 2)] <- 0
}

# Function for setting initial values in JAGS
inits <- function()
  list(z = z.init, B0 = rnorm(1, 4.8, 1), p = 0.9, psi = 0.5)

# Fit model
data <- list()
for(i in 1:length(data.nams)) data[[length(data) + 1]] <- eval(as.name(data.nams[[i]]))
names(data) <- data.nams

st.time <- Sys.time()
out <- jagsUI(data, inits, parameters.to.save = parameters, model.file, n.thin=nt, n.chains=nc,
              n.burnin=nb, n.iter=ni, parallel=TRUE)
end.time <- Sys.time()
run.time <- end.time - st.time
run.time
rm(st.time,end.time)

# DSR #
str_c(round(median(out$sims.list$S), digits = 4),
  " (",
  round(quantile(out$sims.list$S, prob = 0.05, type = 8), digits = 4),
  ",",
  round(quantile(out$sims.list$S, prob = 0.95, type = 8), digits = 4),
  ")")

# PSR #
PSR <- out$sims.list$S ^ 90
str_c(round(median(PSR), digits = 4),
      " (",
      round(quantile(PSR, prob = 0.05, type = 8), digits = 4),
      ",",
      round(quantile(PSR, prob = 0.95, type = 8), digits = 4),
      ")")
