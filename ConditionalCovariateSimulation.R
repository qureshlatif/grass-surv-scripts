prp.zeros <- 0.4 # proportion of zeros in x1
n <- 1000 # sample size

x1 <- c(rep(0, prp.zeros*n), runif((1-prp.zeros)*n, 0, 1)) # primary covariate (e.g., shrub cover) with substantial number of zeros.
x2 <- runif(1000, 0, 1) # secondary covariate (e.g., shrub height)
x2[which(x1 == 0)] <- 0 # Set  secondary on primary covariate.
y <- x1 + x2 + rnorm(1000, 0, 0.3)

hist(x1)
hist(x2)
cor(x1, x2)

# Models with raw secondary covariate
mod.add <- lm(y ~ x1 + x2) # Additive model
summary(mod.add)
summary(mod.add)$r.squared

mod.intxn <- lm(y ~ x1 + x2 + I(x1*x2)) # Interaction model
summary(mod.intxn)
summary(mod.intxn)$r.squared

# Models with conditionalized secondary covariate
x2.cond <- x2
x2.cond[which(x2.cond == 0)] <- NA # Conditionalize x2
x2.cond[which(is.na(x2.cond))] <- mean(x2.cond, na.rm = T)
hist(x2.cond)
cor(x1, x2.cond)

mod.cond <- lm(y ~ x1 + x2.cond) # Model 
summary(mod.cond)
summary(mod.cond)$r.squared

mod.cond.intxn <- lm(y ~ x1 + x2.cond + I(x1*x2.cond)) # Model 
summary(mod.cond.intxn)
summary(mod.cond.intxn)$r.squared
