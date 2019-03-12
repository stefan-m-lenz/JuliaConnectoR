
usingJuliaPackage("StatsBase")
juliaCall("mean_and_var", c(1,2,3))

# Should work
juliaCall("prod", c(1,2,3))
juliaCall("println", "hello world")
juliaCall("println", list(as.integer(1), "bla" = 23L))
juliaCall("eval", "println(22)")

usingJuliaPackage("BoltzmannMachines", all = TRUE)

x <- juliaCall("barsandstripes", 100L, 4L)
x
dbm <- juliaCall("BoltzmannMachines.fitdbm", x,
                epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
                nhiddens = c(4L,3L))
# TODO TrainLayer arguments

dbm
juliaCall("samples", dbm, 10L)
rbm <- juliaCall("BoltzmannMachines.fitrbm", juliaCall("barsandstripes", 100L, 4L))
juliaCall("samples", rbm, 10L)
juliaCall("BoltzmannMachines.Monitor")

# Should error
juliaCall("sum", c(1,2,3, "bla"))
juliaCall("thisisnotarealfunction", 100, 4)
juliaCall("TcpCallR.thisisnotarealfunction", "haha")
juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3))
