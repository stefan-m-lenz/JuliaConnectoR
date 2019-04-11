# JuliaConnectoR

This package provides a functionally oriented interface between R and Julia.
The goal is to use functions from Julia packages directly in R.
Julia functions imported via the `JuliaConnectoR` can accept and return R variables.
It is also possible to pass function arguments to enable callbacks from Julia to R.

The data structures passed to and returned from Julia are serialized to a custom streaming format, sent via TCP and translated to Julia data structures. Returned results are likewise translated back to R. R functions can be passed as arguments and will be invoked by Julia in place of Julia functions.

## Installation

The package can be installed via `devtools`:

    library(devtools)
    install_github("stefan-m-lenz/JuliaConnectoR")

The package requires that Julia (Version &ge; 0.7) is installed and that the Julia executable is in the system search `PATH` or that the `JULIA_BINDIR` environment variable is set to the `bin` directory of the Julia installation.

## List of functions

The following functions are exported by the package:

| Function name | Description |
|---------------|-------------|
| `juliaImport`/`juliaUsing` | Load a Julia package in Julia via `import` or `using` (see Julia documentation) and attach its functions and data types in the R search space, such that the functions can be called directly in R |
| `juliaCall` | Call any Julia function by name. Not needed for the functions attached via `juliaImport`/`juliaUsing` |
| `juliaEval` | Evaluate a Julia expression (and return the result) |

## Translating Julia and R data structure

Since Julia is more type-sensitive than R, it is important to know the translations of the data structures that are shown in the following table:

| R                                  | Julia |
|------------------------------------|-------|
| `vector` of length 1 of type (`typeof`)<br />&bull; `integer`<br />&bull; `double` <br />&bull; `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br />&bull; `Int` <br />&bull; `Float64` <br />&bull; `Bool` <br />&bull; `String` <br />&bull; `Complex{Float64}`<br />&bull; `UInt8` |
| `vector` of length > 1 (N = 1)  or <br /> `array` with N dimensions (`dim`) of type <br />&bull; `integer`<br />&bull; `double` <br />&bull;  `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br /><br />&bull; `Array{Int, N}` <br />&bull; `Array{Float64, N}` <br />&bull; `Array{Bool, N}`<br />&bull; `Array{String, N}` <br />&bull; `Array{Complex{Float64}, N}`<br />&bull; `Array{UInt8, N}` |
| R function (type `closure`) | Julia function that will call the given R function |
| `list` with attribute `"JLTYPE"` | Julia object of the data type specified in the attribute. The constructor is called with the elements of the list in the given order. |
| `list` without attribute `"JLTYPE"` | `Vector{T}` where `T` is the most specific supertype of the list elements after translation to Julia |
| Julia code as one-element character vector with attribute `"JLEXPR"` | Evaluation of the expression |

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
BMs.samples(dbm, 5L, conditions = juliaEval("[1 => 1.0, 2 => 0.0]"))


# A Gaussian-BernoulliRBM
rbm <- fitrbm(data.matrix(iris[, 1:4]), rbmtype = GaussianBernoulliRBM)
samples(rbm, 10L)


# Another way of getting the functions into R: Importing
juliaImport("BoltzmannMachines", alias = "BMs")
x <- BMs.barsandstripes(100L, 4L)
rbm2 <- BMs.fitrbm(x, epochs = 5L)
BMs.samples(rbm2, 5L)
```

For more abstract examples, see the [tests](tests/test.R).
