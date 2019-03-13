
juliaUsing("StatsBase") # TODO fix
juliaCall("mean_and_var", c(1,2,3))

# Should work
juliaCall("prod", c(1,2,3))
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
BoltzmannMachines.samples(dbm, 5L)
rbm <- fitrbm(barsandstripes(100L, 4L))
samples(rbm, 10L)
BoltzmannMachines.Monitor()

juliaImport("BoltzmannMachines", alias = "BMs")
rbm2 <- BMs.fitrbm(x, epochs = 5L)
BMs.samples(rbm2, 5L)

# Should error
juliaCall("sum", c(1,2,3, "bla"))
juliaCall("thisisnotarealfunction", 100, 4)
juliaCall("RConnector.thisisnotarealfunction", "haha")
juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3))
