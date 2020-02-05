# JuliaConnectoR <img src="JuliaConnectoR-Logo.svg" align="right" width="120" />

[![Build Status](https://travis-ci.org/stefan-m-lenz/JuliaConnectoR.svg?branch=master)](https://travis-ci.org/stefan-m-lenz/JuliaConnectoR)
[![Coverage Status](https://coveralls.io/repos/github/stefan-m-lenz/JuliaConnectoR/badge.svg?branch=master)](https://coveralls.io/github/stefan-m-lenz/JuliaConnectoR?branch=master)

This R-package provides a functionally oriented interface between R and Julia.
The goal is to call functions from Julia packages directly as R functions.
Julia functions imported via the `JuliaConnectoR` can accept and return R variables.
It is also possible to pass R functions as arguments in place of Julia functions, which allows callbacks from Julia to R.

From a technical perspective, R data structures are serialized with an optimized custom streaming format,
sent to a (local) Julia TCP server, and translated to Julia data structures by Julia.
The results of function calls are likewise translated back to R.

## Installation

The package can be installed in R via `devtools`:

    library(devtools)
    install_github("stefan-m-lenz/JuliaConnectoR")

The package requires that [Julia (Version &ge; 1.0) is installed](https://julialang.org/downloads/) and that the Julia executable is in the system search `PATH` or that the `JULIA_BINDIR` environment variable is set to the `bin` directory of the Julia installation.

## Overview

The following functions are exported by the package:

| Function name | Description |
|---------------|-------------|
| `juliaImport`/`juliaUsing` | Load a Julia package in Julia via `import` or `using` (see Julia documentation) and attach its functions and data types in the R search space, such that the functions can be called directly in R |
| `juliaFun` | Create an R function that wraps a Julia function |
| `juliaCall` | Call any Julia function by name. Not needed for functions attached via `juliaImport`/`juliaUsing` or created via `juliaFun`. |
| `juliaEval` | Evaluate a simple Julia expression (and return the result) |
| `juliaLet` | Evaluate Julia expressions with R variables in place of Julia variables employing a `let` block (and return the result) |
| `juliaExpr` | Use a Julia expressions or refer to a Julia object via a string in R |

For a detailed description of the functions with some examples, and for more details about the translation of data structures, please see the [latest documentation](https://github.com/stefan-m-lenz/JuliaConnectoR/releases/download/v0.3.1/JuliaConnectoR.pdf).

## Examples

### Using the *BoltzmannMachines* in R

The following example code shows how the `JuliaConnectoR` can be used to use the Julia package [`BoltzmannMachines`](https://github.com/stefan-m-lenz/BoltzmannMachines.jl) in R.

```{r}
library(JuliaConnectoR)

# Test BoltzmannMachines package
# If not installed, install the current version via
# juliaEval('using Pkg; Pkg.add(PackageSpec(name = "BoltzmannMachines", rev = "master"))')

# Set a random seed in Julia
juliaEval("using Random; Random.seed!(5);")

juliaUsing("BoltzmannMachines", importInternal = TRUE)

# a test data set from the BoltzmannMachines-package, just to have some data
x <- barsandstripes(100L, 4L)
x

# Train DBMs with
dbm <- fitdbm(x, epochs = 40L, learningrates = c(rep(0.05, 20), rep(0.001, 20)),
              nhiddens = c(4L,3L))
dbm
dbm2 <- fitdbm(x, epochs = 10L,
               pretraining = list(TrainLayer(nhidden = 4L),
                                  TrainLayer(nhidden = 3L)))
dbm2

# Use a trained model to generate samples
samples(dbm, 10L)

# Evaluate the model: Likelihood estimation ...
loglikelihood(dbm2, x)
#  ... or exact calculation (possible for such a small model)
exactloglikelihood(dbm2, x)

# RBM-fitting with simple monitoring, e. g. just print the progress in R
rbm <- fitrbm(x, epochs = 20L,
              monitoring = function(rbm, epoch) {print(epoch)})


# Now real monitoring with callback functions
# (Abusing environments for call-by-reference value collection)
monitor <- new.env(parent = emptyenv())
monitor$loglik <- c()
rbm <- fitrbm(x, epochs = 100L,
              monitoring = function(rbm, epoch) {
                 monitor$loglik <- c(monitor$loglik, loglikelihood(rbm, x))
            })
plot(1:100, monitor$loglik, "l")


# A complex dbm example with layerwise monitoring
monitor <- new.env(parent = emptyenv())
dbm <- fitdbm(x, epochs = 60L,
              learningrate = 0.05,
              learningratepretraining = 0.01,
              pretraining = list(
                  TrainLayer(nhidden = 4L, epochs = 70L,
                             monitoring = function(rbm, epoch) {
                                monitor$layer1 <- c(monitor$layer1,
                                                    reconstructionerror(rbm, x))
                             }),
                  TrainLayer(nhidden = 3L, epochs = 50L,
                             monitoring = function(rbm, epoch) {
                                monitor$layer2 <- c(monitor$layer2,
                                                    reconstructionerror(rbm, x))
                             })),
               monitoring = function(dbm, epoch) {
                  monitor$logproblowerbound <- c(monitor$logproblowerbound,
                                                 exactloglikelihood(dbm, x))
               }
)
plot(1:70, monitor$layer1, "l")
plot(1:50, monitor$layer2, "l")
plot(1:60, monitor$logproblowerbound, "l")


# First approach for Gibbs-Sampling, allows access to hidden nodes
particles <- initparticles(dbm2, 20L)
particles <- gibbssample(particles, dbm2, 100L) # the "!" can be omitted
particles

# Second approach for Gibbs sampling: All-in-one, returning only visible nodes
BoltzmannMachines.samples(dbm, 5L)

# Conditional Gibbs sampling
samples(dbm, 5L, conditions = juliaEval("[1 => 1.0, 2 => 0.0]"))


# A Gaussian-BernoulliRBM
rbm <- fitrbm(data.matrix(iris[, 1:4]), rbmtype = GaussianBernoulliRBM)
samples(rbm, 10L)


# Another way of getting the functions into R: Importing
juliaImport("BoltzmannMachines", alias = "BMs")
x <- BMs.barsandstripes(100L, 4L)
rbm2 <- BMs.fitrbm(x, epochs = 5L)
BMs.samples(rbm2, 5L)
```

For more abstract examples, see the [tests](tests/testthat/test.R).
