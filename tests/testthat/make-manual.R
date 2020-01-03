library(devtools)
devtools::document()
if (file.exists("JuliaConnectoR.pdf"))  {
   file.remove("JuliaConnectoR.pdf")
}
system2("R.exe", args = "CMD Rd2pdf ../JuliaConnectoR")
