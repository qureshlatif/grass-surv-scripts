library(jagsUI)
library(stringr)
library(dplyr)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
model.file <- "grass-surv-scripts/model_CJSRLHomog_Dot.jags"

# MCMC values
nc <- 3 # number of chains
nb <- 1000 # burn in
ni <- 2000 # number of iterations
nt <- 1 #10 # thinning

save.out <- str_c("mod_CJSRLHomog_Dot_", spp)
#______________________________#

# Data objects to send to JAGS
data.nams <- c("Y.alive", "Y.dead", "first", "last",
               "nBird")

# Stuff to save from JAGS
parameters <- c("B0", "S", "p", "psi")

# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
ind <- which(data.spp$Covs$Site == "Janos" & data.spp$Covs$Season %in% c("2012-2013", "2013-2014"))
ymat <- data.spp$ymat[ind,]
first <- data.spp$Covs$firstDay[ind]
last <- data.spp$Covs$lastDay[ind]
last.alive <- data.spp$Covs$lastAlive[ind]
nBird <- nrow(ymat)

# Dump initial and final columns with no data and adjust indices accordingly #
if(!any(!is.na(ymat[,1]))) {
  chop <- 1:(which(apply(ymat, 2, function(x) any(!is.na(x))))[1] - 1)
  first <- first - length(chop)
  last <- last - length(chop)
  last.alive <- last.alive - length(chop)
}
if(!any(!is.na(ymat[,ncol(ymat)]))) {
  cols.with.data <- which(apply(ymat, 2, function(x) any(!is.na(x))))
  last.col.with.data <- cols.with.data[length(cols.with.data)]
  chop <- (last.col.with.data + 1):ncol(ymat)
  ymat <- ymat[,-chop]
}
rm(chop, last.col.with.data, cols.with.data)

Y.alive <- (ymat == 1)*1
Y.dead <- (ymat == 2)*1
z.init <- (ymat == 1)*1
for(i in 1:length(first)) {
  z.init[i, (first[i]+1):last.alive[i]] <- 1
  z.init[i, first[i]] <- NA
  if(any(ymat[i, ] == 2, na.rm = T)) z.init[i, which(ymat[i, ] == 2)] <- 0
}
z.init <- z.init[,-1] # Chop of first column with no data

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

