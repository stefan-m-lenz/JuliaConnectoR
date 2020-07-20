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

