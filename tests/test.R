library(JuliaConnectoR)

# Should work
juliaCall("prod", c(1,2,3))
juliaCall("show", NULL)
juliaCall("string", list())
juliaCall("println", "hello world")
juliaCall("string", list(as.integer(1), "bla" = 23L))
juliaCall("eval", "println(22)")
juliaEval("using Random; Random.seed!(5)")
juliaEval("Random.seed!(5);") # no output

# Nested callback functions
juliaEval('struct TestStruct f::Function end')
juliaEval('function testNestedFun(ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
t <- juliaCall("testNestedFun",
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")}),
                    juliaCall("TestStruct", function() {print("blip")})))

juliaEval('function testNestedFunNamed(;ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
t <- juliaCall("testNestedFunNamed", ts =
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")})))


# test repeated call of nested functions
juliaEval('function testNestedFun2(ts::Vector{TestStruct}) for i = 1:2 map(t -> t.f(), ts) end end')
t <- juliaCall("testNestedFun2",
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")})))


juliaEval('function testNestedFunArgs(ts::Vector{TestStruct}, args...) map(t -> t.f(args...), ts) end')
t <- juliaCall("testNestedFunArgs",
               list(juliaCall("TestStruct", function(x) {print(x)}),
                    juliaCall("TestStruct", function(x, y) {print(x); return(c(5,6,7))})),
               c(1, 2, 3))

juliaEval('function testNestedAndUnnested(f::Function, ts::Vector{TestStruct}, args...)
            testNestedFunArgs(ts, args...)
            f(args...)
          end')
t <- juliaCall("testNestedAndUnnested",
               function(x) {print("1"); print(x)},
               list(juliaCall("TestStruct", function(x) {print("2"); print(x)}),
                    juliaCall("TestStruct", function(x, y) {print("3") ;print(x); return(c(5,6,7))})),
               list(x = 1, 2, 3)) # TODO try with function with error

# Test BoltzmannMachines package
# Install via
# juliaEval('using Pkg; Pkg.add("BoltzmannMachines")')
juliaUsing("BoltzmannMachines", importInternal = TRUE)

# a test data set
x <- barsandstripes(100L, 4L)
x

# Train DBMs with more complex parameters
dbm <- fitdbm(x, epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
              nhiddens = c(4L,3L))
dbm
dbm2 <- fitdbm(x, epochs = 10L,
               pretraining = list(TrainLayer(nhidden = 4L),
                                  TrainLayer(nhidden = 3L)))
dbm2

# Use a trained model to generate samples
samples(dbm, 10L)

# Evaluate the model
aislogimpweights(dbm2, nparticles = 10L)
logpartitionfunction(dbm2)


# monitoring, e. g. just print the progress
rbm <- fitrbm(x, epochs = 20L,
              monitoring = function(rbm, epoch) {print(epoch)})


# Now real monitoring with callback functions
# (Abusing environments for call by reference)
monitor <- new.env(parent = emptyenv())
monitor$loglik <- c()
rbm <- fitrbm(x, epochs = 100L,
              monitoring = function(rbm, epoch) {
                 monitor$loglik <- c(monitor$loglik, loglikelihood(rbm, x))
            })
monitor$loglik
plot(1:100, monitor$loglik, "l")


monitor <- new.env(parent = emptyenv())
dbm <- fitdbm(x, epochs = 50L,
              pretraining = list(TrainLayer(nhidden = 4L, epochs = 60L),
                                 TrainLayer(nhidden = 3L)),
              monitoring = function(dbm, epoch) {
                 monitor$loglik <- c(monitor$loglik, exactloglikelihood(dbm, x))
              }
)
monitor$loglik

monitor <- new.env(parent = emptyenv())



dbm <- stackrbms(x,
              trainlayers = list(TrainLayer(monitoring = function(rbm, epoch) {
                 print("mesuring loglik")
                 #monitor$loglik <- c(monitor$loglik, exactloglikelihood(dbm, x))
              },
              epochs = 50L, nhidden = 5L))
)

# monitoring function wird nicht einmal ausgeführt!!!!! Das liegt an dem Error function() {...}
# Die richtige Funktion wird einmal ausgeführt und dann wird die Antwort von Julia nie gelesen.
dbm <- fitdbm(x,
              monitoring = function(dbm, epoch) {
                 print("mesuring loglik")
                 #monitor$loglik <- c(monitor$loglik, exactloglikelihood(dbm, x))
              },
              epochs = 50L,
              pretraining = list(TrainLayer(nhidden = 4L, epochs = 60L,
                                            monitoring = function(dbm, epoch){print("blablBL")}), # TODO change input arguments
                                 TrainLayer(nhidden = 3L))
              )
monitor$loglik
plot(1:50, monitor$loglik, "l")



# TODO fix: hangs. Repeated call of nested function works ... what's wrong?
# monitoring function wird genau einmal ausgeführt!!!!!
t <- fitdbm(x, pretraining = list(TrainLayer(nhidden = 4L, epochs = 40L,
                                             monitoring = function(rbm, epoch) {
              print("Epoch");print(epoch)})))

juliaEval("function simmonitoring(ts::Vector{TrainLayer})
          ts =  BoltzmannMachines.stackrbms_preparetrainlayers(
               ts, rand(10,1000), 5, 0.001, Vector{Int}(), 1, BoltzmannMachines.NoOptimizer())
          for t in ts t.monitoring(3,4); sleep(5) end end")
juliaCall("simmonitoring", list(TrainLayer(nhidden = 4L, epochs = 40L,
                                      monitoring = function(rbm, epoch) {
                                         sleep(5);print("Epoch");print(epoch)})))

t <- fitdbm(x, pretraining = list(TrainLayer(nhidden = 4L, epochs = 40L,
                                             monitoring = function(rbm, epoch) {c(1,2,3)})))

monitor <- new.env(parent = emptyenv())
mdbm <- fitdbm(x, epochs = 50L,
               pretraining = list(
                  TrainLayer(nhidden = 4L, epochs = 40L,
                             monitoring = function(rbm, epoch) {
                                monitor$layer1 <- c(monitor$layer1,
                                                    reconstructionerror(rbm, x))
                             }),
                  TrainLayer(nhidden = 3L, epochs = 30L,
                             monitoring = function(rbm, epoch) {
                                monitor$layer2 <- c(monitor$layer2,
                                                    reconstructionerror(rbm, x))
                             })),
               monitoring = function(dbm, epoch) {
                  monitor$logproblowerbound <- c(monitor$logproblowerbound,
                                                 logproblowerbound(dbm, x))
               }
)

plot(1:100, monitor$layer1, "l")


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
BMs.samples(rbm2, 5L, conditions = juliaEval("[1 => 1.0, 2 => 0.0]"))


# Other package
# If not installed, run
# juliaEval('using Pkg; Pkg.add("StatsBase")')
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



