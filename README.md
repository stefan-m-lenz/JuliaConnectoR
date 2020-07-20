# JuliaConnectoR <img src="JuliaConnectoR-Logo.svg" align="right" width="120" />

[![Build Status](https://travis-ci.org/stefan-m-lenz/JuliaConnectoR.svg?branch=master)](https://travis-ci.org/stefan-m-lenz/JuliaConnectoR?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/stefan-m-lenz/JuliaConnectoR/badge.svg?branch=master)](https://coveralls.io/github/stefan-m-lenz/JuliaConnectoR?branch=master)

This R-package provides a functionally oriented interface between R and Julia.
The goal is to call functions from Julia packages directly as R functions.
Julia functions imported via the `JuliaConnectoR` can accept and return R variables.
It is also possible to pass R functions as arguments in place of Julia functions, which allows callbacks from Julia to R.

From a technical perspective, R data structures are serialized with an optimized custom streaming format,
sent to a (local) Julia TCP server, and translated to Julia data structures by Julia.
The results of function calls are likewise translated back to R.
Complex Julia structures can either be used by
reference via proxy objects in R or fully translated to R data structures.

## Installation

The package can be installed in R (version &ge; 3.2) via CRAN:

    install.packages("JuliaConnectoR")

Or the development version can be installed with:

    devtools::install_github("stefan-m-lenz/JuliaConnectoR")

The package requires that [Julia (version &ge; 1.0) is installed](https://julialang.org/downloads/) and that the Julia executable is in the system search `PATH` or that the `JULIA_BINDIR` environment variable is set to the `bin` directory of the Julia installation.

## Overview

The following table lists the most important functions exported by the package:

| Function name | Description |
|---------------|-------------|
| `juliaImport` | Load a Julia package in Julia via `import` and return its functions and data types as an environment, such that the functions can be called directly in R |
| `juliaFun` | Create an R function that wraps a Julia function |
| `juliaCall` | Call any Julia function by name. Not needed for functions attached via `juliaImport` or created via `juliaFun`. |
| `juliaEval` | Evaluate a simple Julia expression (and return the result) |
| `juliaLet` | Evaluate Julia expressions with R variables in place of Julia variables employing a `let` block (and return the result) |
| `juliaGet` | Fully translate a Julia object to an R object |
| `juliaExpr` | Use a Julia expressions or refer to a Julia object via a string in R |

For a detailed description of the functions with some examples, and for more details about the translation of data structures, please see the [latest documentation](https://github.com/stefan-m-lenz/JuliaConnectoR/releases/download/v0.6.0/JuliaConnectoR.pdf).

## Examples

## Using Flux

With the `JuliaConnectoR` it is possible to use, e. g., the [Flux](https://github.com/FluxML/Flux.jl) Julia package for training a neural network on the famous `iris` data set.

You can see that the translation from Julia to R code is rather straightforward:

Below we see example code for training a neural network for classification on the famous `iris` data set in Julia and its translation in R. Both the Julia version of the script and the R version are two complete runnable examples, showing all important steps in training a neural network.

<!-- Julia-iris-data -->
<details><summary>Julia script: Data preparation</summary>

```julia
using Pkg
Pkg.add(PackageSpec(name = "RDatasets", version = "0.6"))
Pkg.add(PackageSpec(name = "Flux", version = "0.10"))

# Import packages and set a seed
import Flux
using Random
Random.seed!(1);

# Load data and split it into training and test data
function rand_split_data(x, labels)
   nsamples = size(x, 2)
   testidxs = randperm(nsamples)[1:(round(Int, nsamples*0.3))]
   trainidxs = setdiff(1:nsamples, testidxs)
   x_train = x[:, trainidxs]
   x_test = x[:, testidxs]
   labels_train = labels[trainidxs]
   labels_test = labels[testidxs]
   y = Flux.onehotbatch(labels, unique(labels))
   y_train = y[:, trainidxs]
   y_test = y[:, testidxs]
   (training = (x = x_train, y = y_train),
         test = (x = x_test, y = y_test))
end

using RDatasets
iris = dataset("datasets", "iris")
x = convert(Matrix{Float64}, (iris[:, 1:4]))'
data = rand_split_data(x, iris[:, :Species])
trainingdata = data.training
testdata = data.test
```

</details>

<!-- Julia-iris-training -->
<details><summary>Julia script: model training and evaluation</summary>

```julia
# Load necessary features
import Flux
using Statistics

# Train the Flux model
model = Flux.Chain(
      Flux.Dense(4, 4, Flux.relu),
      Flux.Dense(4, 4, Flux.relu),
      Flux.Dense(4, 3),
      Flux.softmax)

loss(model, x, y) = Flux.crossentropy(model(x), y)
loss(model, data::NamedTuple) = loss(model, data.x, data.y)

function train_network!(model, x, y; epochs, callback)
   opt = Flux.ADAM()
   loss_(x, y) = loss(model, x, y)
   for i in 1:epochs
      Flux.train!(loss_, Flux.params(model), [(x, y)], opt)
      callback(i)
   end
end

epochs = 2500
train_losses = Vector{Float32}(undef, epochs);
test_losses = Vector{Float32}(undef, epochs);
train_network!(model, trainingdata.x, trainingdata.y, epochs = epochs,
      callback = (i) -> begin
         train_losses[i] = loss(model, trainingdata)
         test_losses[i] = loss(model, testdata)
      end)

# (Could plot losses here)

# Evaluate model
accuracy(model, data) =
      mean(Flux.onecold(model(data.x)) .== Flux.onecold(data.y))
accuracy(model, trainingdata)
accuracy(model, testdata)
```

</details>

<!-- R-iris-data -->
<details><summary>Adapted R/JuliaConnectoR script: data preparation</summary>

```R
library(JuliaConnectoR)

juliaEval('using Pkg; Pkg.add(PackageSpec(name = "Flux", version = "0.10"))')

# The Julia code can simply be reused
rand_split_data <- juliaEval('
      import Flux
      using Random
      Random.seed!(1);

      function rand_split_data(x, labels)
         nsamples = size(x, 2)
         testidxs = randperm(nsamples)[1:(round(Int, nsamples*0.3))]
         trainidxs = setdiff(1:nsamples, testidxs)
         x_train = x[:, trainidxs]
         x_test = x[:, testidxs]
         labels_train = labels[trainidxs]
         labels_test = labels[testidxs]
         y = Flux.onehotbatch(labels, unique(labels))
         y_train = y[:, trainidxs]
         y_test = y[:, testidxs]
         (training = (x = x_train, y = y_train),
               test = (x = x_test, y = y_test))
      end')

x <- as.matrix(iris[, 1:4])
labels <- iris[, "Species"]
data <- rand_split_data(t(x), labels)
trainingdata <- data$training
testdata <- data$test
```

</details>

<!-- R-iris-training -->
<details open><summary>Adapter R/JuliaConnectoR script: model training and evaluation</summary>

```R
library(JuliaConnectoR)
stopifnot(packageVersion("JuliaConnectoR") >= "0.4")

# load Flux features available in R
Flux <- juliaImport("Flux")

juliaEval("using Statistics") # for Julia code only

juliaEval("import Random; Random.seed!(1);")
model <- Flux$Chain(
      Flux$Dense(4L, 4L, Flux$relu),
      Flux$Dense(4L, 4L, Flux$relu),
      Flux$Dense(4L, 3L),
      Flux$softmax)

loss <- juliaEval('loss(model, x, y) = Flux.crossentropy(model(x), y)
                  loss(model, data::NamedTuple) = loss(model, data.x, data.y)')

train_network <- juliaEval('
   function train_network!(model, x, y; epochs, callback)
      opt = Flux.ADAM()
      loss_(x, y) = loss(model, x, y)
      for i in 1:epochs
         Flux.train!(loss_, Flux.params(model), [(x, y)], opt)
         callback(i)
      end
   end')

plotLoss <- function(epoch, loss_train, loss_test, epochs) {
   if (epoch == 1) {
      ymax <- max(loss_train, loss_test)
      plot(x = c(1, 1), y = c(loss_train, loss_test),
           xlim = c(0, epochs), ylim = c(0, ymax*1.1),
           col = c("red", "blue"), xlab = "Epoch", ylab = "Loss")
      legend("topright", legend = c("Training data", "Test data"),
             col = c("red", "blue"), pch = 1)
   } else {
      points(x = c(epoch, epoch), y = c(loss_train, loss_test),
             col = c("red", "blue"))
   }
}

epochs <- 2500
train_losses <- rep(0, epochs)
test_losses <- rep(0, epochs)
train_network(model, trainingdata$x, trainingdata$y, epochs = epochs,
      callback = function(i) {
         train_losses[i] <<- loss(model, trainingdata)
         test_losses[i] <<- loss(model, testdata)
         plotLoss(i, train_losses[i], test_losses[i], epochs)
      })

accuracy <- juliaEval("accuracy(model, data) =
      mean(Flux.onecold(model(data.x)) .== Flux.onecold(data.y))")
accuracy(model, trainingdata)
accuracy(model, testdata)
```

</details>

Using the callback makes it possible to plot the training curve
during the training.
This example runs so fast that it is not really possible to watch the
training progress.
In our case it would be better to optimize the training function to accumulate the losses in Julia and plot them in the end of the training.
However, the example demonstrates the principle of how callback functions can be used to watch the progress during the training.

### Using *BoltzmannMachines* in R

The following example code shows how the `JuliaConnectoR` can be used with the Julia package [`BoltzmannMachines`](https://github.com/stefan-m-lenz/BoltzmannMachines.jl) in R.


<!-- Boltzmann-Example -->
```R
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
```

For more abstract examples, see the [tests](tests/testthat/test.R).
