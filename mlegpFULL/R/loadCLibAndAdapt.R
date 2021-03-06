.First.lib <- function(lib, pkg) {
  #if(version$major==0 && version$minor < 62)
  #  stop("This version for R 0.62 or later")
  library.dynam("mlegpFULL", pkg, lib)

  # load package adapt, or display warning if it does not exist
  success = suppressWarnings(require(adapt, quietly=TRUE, warn.conflicts=FALSE))
        if (success) {
                suppressWarnings(detach(package:adapt))
                library(adapt)   ## will show warnings, if there are any
                cat("loading recommended package: adapt\n")
        }
        else {
                cat("warning: package adapt could not be loaded\n")
                cat("the following functions will not be available: FANOVADecomposition, plotInteractionEffects\n")
        }
}

