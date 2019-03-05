
usingJuliaPkg("StatsBase")
juliafun("mean_and_var", c(1,2,3))

# Should work
juliafun("prod", c(1,2,3))
juliafun("println", "hello world")
juliafun("println", list(as.integer(1), "bla" = 23L))
juliafun("eval", "println(22)")

usingJuliaPkg("BoltzmannMachines")

x <- juliafun("barsandstripes", 100L, 4L)
x
dbm <- juliafun("BoltzmannMachines.fitdbm", x,
                epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
                nhiddens = c(4L,3L))
# TODO TrainLayer arguments

dbm
juliafun("samples", dbm, 10L)
rbm <- juliafun("BoltzmannMachines.fitrbm", juliafun("barsandstripes", 100L, 4L))
juliafun("samples", rbm, 10L)
juliafun("BoltzmannMachines.Monitor")

# Should error
juliafun("sum", c(1,2,3, "bla"))
juliafun("thisisnotarealfunction", 100, 4)
juliafun("TcpCallR.thisisnotarealfunction", "haha")
juliafun("NotARealModule.thisisnotarealfunction", list(1,2,3))
