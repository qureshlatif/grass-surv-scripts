# Note: This version of the model requires chopping off the first column of z with no data.
model <<- nimbleCode({

# Data #


# Priors #
B0 ~ dnorm(0, 0.6666667)
psi ~ dunif(0, 1)
p ~ dunif(0, 1)
logit(S) <- B0

# Process #
for(i in 1:nBird) {
  z[i, first[i]] ~ dbern(S)
  for(j in (first[i] + 1):last[i]) {
    z[i, j] ~ dbern(S*z[i, (j - 1)])
    }
  for(j in (first[i] + 1):last[i]) {
    Y.alive[i, j] ~ dbern(p * z[i, (j - 1)])
    Y.dead[i, j] ~ dbern(psi * (1 - z[i, (j - 1)]))
    }
  }
})
