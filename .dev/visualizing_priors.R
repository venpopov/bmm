data <- OberauerLin_2017
data$session <- as.factor(data$session)
formula <- bmf(c ~ 0 + set_size, kappa ~ session)
model <- sdmSimple('dev_rad')
get_model_prior(formula, data, model)


x <- exp(rnorm(1000000,3,1.75))
hist(x[x<20], freq=F, breaks=100)
xd <- density(x, n = 10000, from=0, to=1000)
plot(xd, xlim=c(0,100))
xd$x[xd$y == max(xd$y)]



x <- exp(rstudent_t(1000000,5,2,0.75))
hist(x[x<20], freq=F, breaks=100)
xd <- density(x, n = 10000, from=0, to=1000)
plot(xd, xlim=c(0,100))
xd$x[xd$y == max(xd$y)]



x <- exp(rstudent_t(1000000,5,1.75,0.75))
hist(x[x<20], freq=F, breaks=100)
xd <- density(x, n = 10000, from=0, to=1000)
plot(xd, xlim=c(0,100))
xd$x[xd$y == max(xd$y)]



x <- exp(rnorm(1000000,1.75,0.75))
hist(x[x<20], freq=F, breaks=100)
xd <- density(x, n = 10000, from=0, to=1000)
plot(xd, xlim=c(0,100))
xd$x[xd$y == max(xd$y)]
