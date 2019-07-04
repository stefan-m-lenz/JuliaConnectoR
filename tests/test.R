library(JuliaConnectoR)
library(tools)

# Should work
juliaCall("prod", c(1,2,3))
juliaCall("show", NULL)
juliaCall("string", list())
juliaCall("println", "hello world")
juliaCall("string", list(as.integer(1), "bla" = 23L))
juliaEval("String[]")
juliaCall("eval", "println(22)")
juliaEval("using Random; Random.seed!(5)")
juliaEval("Random.seed!(5);") # no return value

# Package loading
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


juliaEval("function juliaecho(x) x end")
juliaEcho <- function(...) juliaCall("juliaecho", ...) # TODO more tests with all kinds of datatypes
juliaEcho(matrix(1:6, nrow = 2))
juliaEcho(c("bla", "blup", "blip"))


# Test Arrays with undefined references
juliaLet('(() -> begin
         x = Vector{String}(undef, 2); x[1] = "hi";
         return x;
         end)()')
juliaLet('(() -> begin
         x = Vector{Complex}(undef, 2); x[1] = im;
         return x;
         end)()')
juliaLet("Dict(zip(x, y))", x= c("bla", "blup"), y = c(1,2))


# Test Let
juliaLet("print(1)")
assertError(juliaLet("print(x)", 1))
juliaLet("juliaecho(x)", x=c(2, 3))
juliaLet("y=2*x; z = 3*u + 1; (x=y, y=z)", x=2, u=4)


# Should error
juliaCall("sum", c(1,2,3, "bla"))
juliaCall("thisisnotarealfunction", 100, 4)
juliaCall("RConnector.thisisnotarealfunction", "haha")
juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3))



