library(JuliaConnectoR)
juliaUsing("StatsBase") # TODO fix
juliaCall("mean_and_var", c(1,2,3))

# Should work
juliaCall("prod", c(1,2,3))
juliaCall("show", NULL)
juliaCall("println", "hello world")
juliaCall("println", list(as.integer(1), "bla" = 23L))
juliaCall("eval", "println(22)")

juliaUsing("BoltzmannMachines", importInternal = TRUE)

x <- barsandstripes(100L, 4L)
x
dbm <- fitdbm(x, epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
              nhiddens = c(4L,3L))
# TODO TrainLayer arguments
dbm
samples(dbm, 10L)

dbm2 <- fitdbm(x, epochs = 10L,
               pretraining = list(TrainLayer(nhidden = 4L),
                                  TrainLayer(nhidden = 3L)))
dbm2
logpartitionfunction(dbm2)
aislogimpweights(dbm2, nparticles = 10L)

particles <- initparticles(dbm2, 20L)
particles <- gibbssample(particles, dbm2, 100L)
particles

BoltzmannMachines.samples(dbm, 5L)

rbm <- fitrbm(data.matrix(iris[, 1:4]), rbmtype = GaussianBernoulliRBM)
samples(rbm, 10L)
BoltzmannMachines.Monitor()

juliaImport("BoltzmannMachines", alias = "BMs")
x <- BMs.barsandstripes(100L, 4L)
rbm2 <- BMs.fitrbm(x, epochs = 5L)
BMs.samples(rbm2, 5L)

# Should error
juliaCall("sum", c(1,2,3, "bla"))
juliaCall("thisisnotarealfunction", 100, 4)
juliaCall("RConnector.thisisnotarealfunction", "haha")
juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3))
