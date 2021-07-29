library(devtools)
# for (rdfile in list.files("man")) {
#    file.remove(file.path("man", rdfile))
# }

makeManual <- function(x) {
   devtools::document()
   if (file.exists(paste0(x, ".pdf")))  {
      file.remove(paste0(x, ".pdf"))
   }
   try({system2("R", args = paste0("CMD Rd2pdf ../", x))})
   subdirs <- list.dirs(recursive = FALSE)
   unlink(subdirs[grep(pattern = "\\.Rd2pdf.*", subdirs)], recursive = TRUE)
}

makeManual("JuliaConnectoR")

