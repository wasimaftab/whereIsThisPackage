#' Which repository do my R packages belong to?
#'
#' This function accepts a list of R package names and groups them in two separate 
#' lists corresponding to CRAN or Bioconductor. It also list the package names  
#' that do not match with the package names in either repositories. 

#'
#' @param list.of.pkgs list of R package names
#' @return A list containing two sub lists corresponding to CRAN or Bioconductor
#' @export
whereIsThisPackage <- function(list.of.pkgs){
  pat <- paste0("^", paste0(list.of.pkgs, collapse = "$|^"), "$")
  
  ## Given a list of pkgs Find which pkgs are available
  available.pkgs <-
    intersect(list.of.pkgs, BiocManager::available(pattern = pat, include_installed = TRUE))
  
  do.not.exist <- setdiff(list.of.pkgs, available.pkgs)
  
  pat <- paste0("^", paste0(available.pkgs, collapse = "$|^"), "$")
  
  not.installed <-
    BiocManager::available(pattern = pat, include_installed = FALSE)
  
  ## Get list of all CRAN pkgs
  all.packages.CRAN <-
    as.data.frame(utils::available.packages())
  idx <- which(all.packages.CRAN$Package %in% not.installed)
  
  if (length(idx) == 0) {
    cat("These NOT installed package(s) are from Bioconductor: ")
    cat(not.installed, sep = ", ")
  } else if (length(idx) == length(not.installed)){
    cat("These NOT installed package(s) are from CRAN: ")
    cat(not.installed, sep = ", ")
  } else {
    ## some are from CRAN and some form bioconductor
    pkgs.in.cran <- all.packages.CRAN$Package[idx]
    pkgs.in.bio <- setdiff(not.installed, pkgs.in.cran)
    cat(
      "-----------------------------------------------------------------------\n"
    )
    cat("These NOT installed package(s) are from CRAN: ")
    cat(pkgs.in.cran, sep = ", ")
    cat(
      "\n#######################################################################\n"
    )
    cat("These NOT installed package(s) are from Bioconductor: ")
    cat(pkgs.in.bio, sep = ", ")
    cat(
      "\n-----------------------------------------------------------------------\n"
    )
  }
  
  if (length(do.not.exist) !=0 ){
    cat(
      "\n#######################################################################\n"
    )
    cat("These package(s) DO NOT EXIST either in Bioconductor or in CRAN: ")
    cat(do.not.exist, sep = ", ")
  }
  
  ## Return cran and bioc pkgs
  pkgs.found <- list(cran = NULL, bioc = NULL)
  if (length(pkgs.in.cran) != 0){
    pkgs.found [["cran"]] <- pkgs.in.cran
  }
  
  if (length(pkgs.in.bio) != 0){
    pkgs.found [["bioc"]] <- pkgs.in.bio
  }
  return(pkgs.found)
}
################################################################################
# pkgs.found <- whereIsThisPackage(c("limma", "Seurat", "ChAMPdata", "edgeR", "wavelets"))

################################################################################
