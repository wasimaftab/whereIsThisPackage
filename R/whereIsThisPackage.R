#' Which repository do my R packages belong to?
#'
#' This function accepts a list of R package names and groups them in two separate
#' lists corresponding to CRAN or Bioconductor. It also list the package names
#' that do not match with the package names in either repositories.

#'
#' @param list.of.pkgs list of R package names
#' @return A list containing two sub lists corresponding to CRAN or Bioconductor
#' @export
whereIsThisPackage <- function(list.of.pkgs) {
  ## remove if there are unwanted spaces in the package names
  list.of.pkgs <-
    gsub(pattern = "\\s+",
         replacement = "",
         x = list.of.pkgs)
  
  ## get a list of not installed packages
  all.installed.pkgs <- as.data.frame(installed.packages())
  list.installed.pkgs <-
    list.of.pkgs[list.of.pkgs %in% all.installed.pkgs$Package]
  not.installed.pkgs <- setdiff(list.of.pkgs, list.installed.pkgs)
  
  ## Given a list of pkgs Find which pkgs are available
  pat <-
    paste0("^", paste0(not.installed.pkgs, collapse = "$|^"), "$")
  
  available.pkgs <-
    intersect(
      not.installed.pkgs,
      BiocManager::available(pattern = pat, include_installed = TRUE)
    )
  
  pkgs.found <- list(cran = NULL,
                     bioc = NULL,
                     github = NULL)
  
  if (length(available.pkgs) != 0) {
    do.not.exist <- setdiff(not.installed.pkgs, available.pkgs)
    
    ## these pkgs are in either bioc or CRAN
    available.not.installed.pkgs <-
      setdiff(not.installed.pkgs, do.not.exist)
    
    ## Get list of all CRAN pkgs
    all.packages.CRAN <-
      as.data.frame(utils::available.packages(repos = "https://cloud.r-project.org"))
      #as.data.frame(utils::available.packages())

    
    idx <-
      which(all.packages.CRAN$Package %in% available.not.installed.pkgs)
   
    if (length(idx) == 0) {
      cat("These NOT installed package(s) are from Bioconductor: ")
      cat(available.not.installed.pkgs, sep = ", ")
      cat(
        "\n-----------------------------------------------------------------------\n"
      )
      pkgs.found [["bioc"]] <- available.not.installed.pkgs
    } else if (length(idx) == length(available.not.installed.pkgs)) {
      cat("These NOT installed package(s) are from CRAN: ")
      cat(available.not.installed.pkgs, sep = ", ")
      cat(
        "\n-----------------------------------------------------------------------\n"
      )
      pkgs.found [["cran"]] <- available.not.installed.pkgs
    } else {
      ## some are from CRAN and some form bioconductor
      pkgs.in.cran <- all.packages.CRAN$Package[idx]
      pkgs.in.bio <-
        setdiff(available.not.installed.pkgs, pkgs.in.cran)
      pkgs.found [["cran"]] <- pkgs.in.cran
      pkgs.found [["bioc"]] <- pkgs.in.bio
      cat(
        "\n-----------------------------------------------------------------------\n"
      )
      cat("These NOT installed package(s) are from CRAN: ")
      cat(pkgs.in.cran, sep = ", ")
      cat(
        "\n-----------------------------------------------------------------------\n"
      )
      cat("These NOT installed package(s) are from Bioconductor: ")
      cat(pkgs.in.bio, sep = ", ")
      cat(
        "\n-----------------------------------------------------------------------\n"
      )
    }
    
    ## Return cran and bioc pkgs
    if (length(do.not.exist) != 0) {
      cat("These package(s) DO NOT EXIST either in Bioconductor or in CRAN BUT maybe in GitHub: ")
      cat(do.not.exist, sep = ", ")
      pkgs.found [["github"]] <- do.not.exist
    }
    cat(
      "\n#######################################################################\n"
    )
  } else {
    pkgs.found[["github"]] <- not.installed.pkgs
  }
  
  return(pkgs.found)
}
################################################################################
# pkgs.found <- whereIsThisPackage(c("limma", "Seurat", "ChAMPdata", "edgeR", "wavelets"))

################################################################################
