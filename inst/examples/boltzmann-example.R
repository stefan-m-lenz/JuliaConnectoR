library(JuliaConnectoR)

# Test BoltzmannMachines package
# If not installed, install the current version via
# juliaEval('using Pkg; Pkg.add("BoltzmannMachines"))')

# Set a random seed in Julia
juliaEval("using Random; Random.seed!(5);")

BM <- juliaImport("BoltzmannMachines")

# a test data set from the BoltzmannMachines-package, just to have some data
x <- BM$barsandstripes(100L, 4L)
x

# Train DBMs with
dbm <- BM$fitdbm(x, epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
                 nhiddens = c(4L,3L))
dbm
dbm2 <- BM$fitdbm(x, epochs = 10L,
                  pretraining = list(BM$TrainLayer(nhidden = 4L),
                                     BM$TrainLayer(nhidden = 3L)))
dbm2

# Use a trained model to generate samples
BM$samples(dbm, 10L)

# Evaluate the model: Likelihood estimation ...
BM$loglikelihood(dbm2, x)
#  ... or exact calculation (possible for such a small model)
BM$exactloglikelihood(dbm2, x)

# RBM-fitting with simple monitoring, e. g. just print the progress in R
rbm <- BM$fitrbm(x, epochs = 20L,
                 monitoring = function(rbm, epoch) {print(epoch)})


# Now real monitoring with callback functions
# (Abusing environments for call-by-reference value collection)
monitor <- new.env(parent = emptyenv())
monitor$loglik <- c()
rbm <- BM$fitrbm(x, epochs = 100L,
                 monitoring = function(rbm, epoch) {
                    monitor$loglik <- c(monitor$loglik, BM$loglikelihood(rbm, x))
                 })
plot(1:100, monitor$loglik, "l")

juliaEval("Random.seed!(5);")

# A complex dbm example with layerwise monitoring
monitor <- new.env(parent = emptyenv())
dbm <- BM$fitdbm(x, epochs = 60L,
                 learningrate = 0.05,
                 learningratepretraining = 0.01,
                 pretraining = list(
                       BM$TrainLayer(nhidden = 4L, epochs = 70L,
                              monitoring = function(rbm, epoch) {
                                 monitor$layer1 <- c(monitor$layer1,
                                                     BM$reconstructionerror(rbm, x))
                              }),
                        BM$TrainLayer(nhidden = 3L, epochs = 50L,
                              monitoring = function(rbm, epoch) {
                                 monitor$layer2 <- c(monitor$layer2,
                                                     BM$reconstructionerror(rbm, x))
                             })),
               monitoring = function(dbm, epoch) {
                  monitor$logproblowerbound <- c(monitor$logproblowerbound,
                                                 BM$exactloglikelihood(dbm, x))
               }
)
plot(1:70, monitor$layer1, "l")
plot(1:50, monitor$layer2, "l")
plot(1:60, monitor$logproblowerbound, "l")


# First approach for Gibbs-Sampling, allows access to hidden nodes
particles <- BM$initparticles(dbm2, 20L)
particles <- BM$gibbssample(particles, dbm2, 100L) # the "!" can be omitted
particles

# Second approach for Gibbs sampling: All-in-one, returning only visible nodes
BM$samples(dbm, 5L)

# Conditional Gibbs sampling
BM$samples(dbm, 5L, conditions = juliaEval("[1 => 1.0, 2 => 0.0]"))


# A Gaussian-BernoulliRBM
rbm <- BM$fitrbm(data.matrix(iris[, 1:4]), rbmtype = BM$GaussianBernoulliRBM)
BM$samples(rbm, 10L)

