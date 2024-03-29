% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connecting.R
\name{startJuliaServer}
\alias{startJuliaServer}
\title{Start a Julia server that may serve multiple clients (R processes)}
\usage{
startJuliaServer(port = 11980)
}
\arguments{
\item{port}{a hint for the port that is used by the server.
If it is not available, a different port is used.
The final port is returned (invisibly).}
}
\value{
the port number (invisibly)
}
\description{
Starting a Julia server allows that different R processes may connect to the
the same Julia server and share a single session.
This can be useful for saving start-up/precompilation time when starting
additional processes or when sharing global variables between processes.
\emph{For the standard way of starting Julia, this function is not needed.
It is also not needed if child processes should use separate Julia sessions.}
}
\details{
The functions communicates the server address via setting the
\env{JULIACONNECTOR_SERVER} environment variable.
A possible value for the variable is "localhost:11980".
The \env{JULIACONNECTOR_SERVER} variable is communicated automatically via
the system environment to child processes that are started after this
function has been called.
The child processes will then connect to the same Julia server if the
variable is set.
The variable can also be set explicitly in child processes before connecting
to Julia to connect to a running server.
Unsetting the variable will result in a normal Julia start-up in the first
call to Julia, using a single-client Julia session.

For security reasons, the Julia server accepts only connections
from localhost.

For using Julia with multiple clients, it can be good to advise Julia to
use multiple threads via setting the \env{JULIA_NUM_THREADS} environment
variable before starting Julia.
}
\note{
The standard (error) output from Julia (printing and warnings)
can currently only be forwarded to one client.
This is currently the last client that has connected but this may be subject
to change.
}
\examples{
if (juliaSetupOk()) {

   Sys.setenv("JULIA_NUM_THREADS" = parallel::detectCores())
   startJuliaServer()

   library(future)
   plan(multisession) # use background R processes on the same machine

   juliaEval("global x = 1")

   # Child processes now use the same Julia session:
   f1 <- future({juliaEval("x")})
   value(f1)

   plan(sequential) # close background workers

}
\dontshow{
JuliaConnectoR:::stopJulia()
}
}
\seealso{
\link{JULIACONNECTOR_SERVER}
}
