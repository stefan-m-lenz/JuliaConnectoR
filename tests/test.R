library(JuliaConnectoR)

# Should work
juliaCall("prod", c(1,2,3))
juliaCall("show", NULL)
juliaCall("string", list())
juliaCall("println", "hello world")
juliaCall("string", list(as.integer(1), "bla" = 23L))
juliaCall("eval", "println(22)")

# Test BoltzmannMachines package
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

# Other package
juliaImport("StatsBase")
StatsBase.mean_and_var(c(1,2,3))
StatsBase.renyientropy(rnorm(100), 1)

# more exotic data types
juliaImport("LinearAlgebra", alias = "jla")
jla.eigvals(matrix(c(1, 0, 0, -1), ncol = 2),
          matrix(c(0, 1, 1, 0), ncol = 2)) == c(1i, -1i)
jla.eigmax(matrix(c(0, 1i, -1i, 0), ncol = 2)) == 1.0
juliaCall("string", c(as.raw(0xca), as.raw(0xfe)))

# Should error
juliaCall("sum", c(1,2,3, "bla"))
juliaCall("thisisnotarealfunction", 100, 4)
juliaCall("RConnector.thisisnotarealfunction", "haha")
juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3))



