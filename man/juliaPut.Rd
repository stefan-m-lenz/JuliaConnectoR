% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{juliaPut}
\alias{juliaPut}
\title{Create a Julia proxy object from an R object}
\usage{
juliaPut(x)
}
\arguments{
\item{x}{an R object (can also be a translated Julia object)}
}
\description{
This function can be used to copy R vectors and matrices to Julia and keep
them there.
The returned proxy object can be used in place of the original vector or
matrix.
This is useful to prevent that large R vectors / matrices
are repeatedly translated when using an object in multiple calls to Julia.
}
\examples{
if (juliaSetupOk()) {

   # Transfer a large vector to Julia and use it in multiple calls
   x <- juliaPut(rnorm(100))
   # x is just a reference to a Julia vector now
   juliaEval("using Statistics")
   juliaCall("mean", x)
   juliaCall("var", x)

}

\dontshow{
JuliaConnectoR:::stopJulia()
}
}
