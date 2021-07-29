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

extractIrisExample <- function() {
   extractCodeBlocks("README.md", c("<!-- Julia-iris-data -->",
                                    "<!-- Julia-iris-training -->"),
                     "inst/examples/iris-example/iris-example.jl")

   extractCodeBlocks("README.md", c("<!-- R-iris-data -->",
                                    "<!-- R-iris-training -->"),
                     "inst/examples/iris-example/iris-example.R")
}

extractBoltzmannExample <- function() {
   extractCodeBlocks("README.md", "<!-- Boltzmann-Example -->",
                     "inst/examples/boltzmann-example.R")
}

extractIrisExample()
extractBoltzmannExample()
