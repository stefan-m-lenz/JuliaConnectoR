using Pkg
Pkg.add(PackageSpec(name = "RDatasets", version = "0.7"))
Pkg.add(PackageSpec(name = "Flux", version = "0.14"))

# Import packages and set a seed
import Flux
using Random
Random.seed!(1);

# Load data and split it into training and test data
function prepare_data(x, labels)
   nsamples = size(x, 2)
   x = Matrix{Float32}(x)
   testidxs = randperm(nsamples)[1:(round(Int, nsamples*0.3))]
   trainidxs = setdiff(1:nsamples, testidxs)
   x_train = x[:, trainidxs]
   x_test = x[:, testidxs]
   y = Flux.onehotbatch(labels, unique(labels))
   y_train = y[:, trainidxs]
   y_test = y[:, testidxs]
   (training = (x = x_train, y = y_train),
         test = (x = x_test, y = y_test))
end

using RDatasets
import Tables
iris = dataset("datasets", "iris")
x = Tables.matrix(iris[:, 1:4])'
data = prepare_data(x, iris[:, :Species])
trainingdata = data.training
testdata = data.test

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
   opt = Flux.setup(Flux.Adam(), model);
   for i in 1:epochs
      Flux.train!(loss, model, [(x, y)], opt)
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

