library(devtools)
devtools::document()
if (file.exists("JuliaConnectoR.pdf"))  {
   file.remove("JuliaConnectoR.pdf")
}
system2("R", args = "CMD Rd2pdf ../JuliaConnectoR")
