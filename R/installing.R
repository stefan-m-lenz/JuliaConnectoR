
JULIA_VERSION_JSON_URL <- "https://julialang-s3.julialang.org/bin/versions.json"

getLatestJuliaRelease <- function(juliaVersionsJson) {
   versionStrings <- names(juliaVersionsJson)
   releaseVersions <- versionStrings[grep("^\\d+\\.\\d+\\.\\d+$", versionStrings)]
   major <- regmatches(releaseVersions, regexpr("^\\d+", releaseVersions))
   major <- as.numeric(major)
   minor <- gsub("^\\d+\\.", "",
                 regmatches(releaseVersions, regexpr("^\\d+\\.\\d+", releaseVersions)))
   minor <- as.numeric(minor)
   bugfix <- gsub("^\\d+\\.\\d+\\.", "",
                  regmatches(releaseVersions, regexpr("^\\d+\\.\\d+\\.\\d+", releaseVersions)))
   bugfix <- as.numeric(bugfix)
   versionOrdering <- order(major, minor, bugfix)
   return(releaseVersions[versionOrdering[max(versionOrdering)]])
}


getJuliaSysArchName <- function(sysinfo = Sys.info()) {
   if (grepl("x86(_|-)64", sysinfo["machine"])) {
      return("x86_64")
   } else if (grepl("x86", sysinfo["machine"])) {
      return("i686")
   } else {
      # TODO : how to figure out arm64 architecture?
      message("Assuming ARM 64 architecture")
      return("aarch64")
   }
}

extractDownloadInfo <- function(juliaVersionsJson, juliaVersion, sysinfo = Sys.info()) {
   filesInfo <- juliaVersionsJson[[juliaVersion]][["files"]]

   if (sysinfo["sysname"] == "Windows") {
      filesInfo <- filesInfo[filesInfo$os == "winnt", ]
      filesInfo <- filesInfo[filesInfo$kind == "archive", ]
      if (nrow(filesInfo) == 0) {
         stop("No suitable installation option found.\n",
              "(Installation on Windows is only supported as archive, which is only available for Julia version >= 1.5)")
      }
   } else if (sysinfo["sysname"] == "Linux") {
      filesInfo <- filesInfo[filesInfo$os == "linux", ]
      # Musl is currently not supported, so filter it out.
      filesInfo <- filesInfo[!grepl("-musl$", filesInfo$triplet), ]
   } else if (sysinfo["sysname"] == "Darwin") {
      filesInfo <- filesInfo[filesInfo$os == "mac", ]
   }

   filesInfo <- filesInfo[filesInfo$arch == getJuliaSysArchName(sysinfo), ]

   if (nrow(filesInfo) != 1) {
      stop("Julia could not be installed via the JuliaConnectoR on your system.",
           "Please consider filing an issue on https://github.com/stefan-m-lenz/JuliaConnectoR")
   }
   filesInfo
}

installJuliaWindows <- function(juliaInstallFile, installDir) {
   utils::unzip(zipfile = juliaInstallFile, exdir = installDir)
}

installJuliaLinux <- function(juliaInstallFile, installDir) {

}

installJuliaMac <- function(juliaInstallFile, installDir) {
   stop("Not supported yet")
}


defaultJuliaInstallDir <- function() {
   if (version$major < 4) {
      stop("On R version < 4 an installation directory for Julia must be specified explicitly")
   } else {
      return(file.path(tools::R_user_dir("JuliaConnectoR", "data"), "julia"))
   }
}

installJulia <- function(juliaVersion = "release", installDir = defaultJuliaInstallDir()) {
   juliaVersionsJsonFileName <- tempfile()

   message("Getting information about available Julia versions...\n")
   utils::download.file(url = JULIA_VERSION_JSON_URL, destfile = juliaVersionsJsonFileName)

   juliaVersionsJsonFile <- file(juliaVersionsJsonFileName, open = "rb")
   juliaVersionsJson <- jsonlite::fromJSON(juliaVersionsJsonFile)
   close(juliaVersionsJsonFile)
   if (juliaVersion == "release") {
      juliaVersion <- getLatestJuliaRelease(juliaVersionsJson)
   } else if (is.null(juliaVersionsJson[[juliaVersion]])) {
      stop(paste0("Julia version \"", juliaVersion ,"\" cannot be identified"))
   }
   file.remove(juliaVersionsJsonFileName)

   downloadUrl <- extractDownloadInfo(juliaVersionsJson, juliaVersion)$url

   message("Downloading Julia ...\n")
   juliaInstallFile <- tempfile()
   utils::download.file(url = downloadUrl, destfile = juliaInstallFile)

   message(paste0("Installing Julia in \"", gsub("\\\\", "\\", installDir), "\" ...\n"))
   os <- Sys.info()["sysname"]

   if (os == "Windows") {
      installJuliaWindows(juliaInstallFile, installDir)
   } else if (os == "Linux") {
      installJuliaLinux(juliaInstallFile, installDir)
   } else if (os == "Darwin") {
      installJuliaMac(juliaInstallFile, installDir)
   }

   #file.remove(juliaInstallFile)

   # TODO warning if Julia is already connected
   # TODO check if folder is already present

   # TODO what if multiple Julia versions are installed?

   return(invisible(NULL))
}
