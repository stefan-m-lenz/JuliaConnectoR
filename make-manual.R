library(devtools)
# for (rdfile in list.files("man")) {
#    file.remove(file.path("man", rdfile))
# }
devtools::document()
if (file.exists("JuliaConnectoR.pdf"))  {
   file.remove("JuliaConnectoR.pdf")
}
system2("R", args = "CMD Rd2pdf ../JuliaConnectoR")
subdirs <- list.dirs(recursive = FALSE)
unlink(subdirs[grep(pattern = "\\.Rd2pdf.*", subdirs)], recursive = TRUE)
