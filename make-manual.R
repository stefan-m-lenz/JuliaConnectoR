library(devtools)
# for (rdfile in list.files("man")) {
#    file.remove(file.path("man", rdfile))
# }
devtools::document()
if (file.exists("JuliaConnectoR.pdf"))  {
   file.remove("JuliaConnectoR.pdf")
}
try({system2("R", args = "CMD Rd2pdf ../JuliaConnectoR")})
subdirs <- list.dirs(recursive = FALSE)
unlink(subdirs[grep(pattern = "\\.Rd2pdf.*", subdirs)], recursive = TRUE)



extractCodeBlocks <- function(sourceFile, afterLines, targetFile) {
   text <- readLines(sourceFile)
   code <- unlist(lapply(afterLines, function(markLine) {
      markIdx <- match(markLine, text)
      codeBlockStart <- Find(function(i) {startsWith(text[i], "```")},
                             (markIdx + 1):length(text))
      codeBlockEnd <- Find(function(i) {startsWith(text[i], "```")},
                           (codeBlockStart + 1):length(text))
      code <- text[(codeBlockStart + 1):(codeBlockEnd - 1)]
      code[length(code)+1] <- ""
      code
   }))
   writeLines(code, targetFile)
}

makeIrisExample <- function() {
   extractCodeBlocks("README.md", c("<!-- Julia-iris-data -->",
                                    "<!-- Julia-iris-training -->"),
                     "inst/examples/iris-example.jl")

   extractCodeBlocks("README.md", c("<!-- R-iris-data -->",
                                    "<!-- R-iris-training -->"),
                     "inst/examples/iris-example.R")
}

makeIrisExample()

