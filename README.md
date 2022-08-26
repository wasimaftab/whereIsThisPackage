# **Which repository do my R packages belong to?**

## `whereIsThisPackage (list.of.pkgs)` function accepts a list of R package names and groups them in two separate lists corresponding to CRAN or Bioconductor. It also list the package names that do not match with the package names in either repositories. 

### <ins>Required packages</ins>:-

* BiocManager:- Install it as `install.packages("BiocManager")`
* devtools:- Install it as `install.packages("devtools")`

### <ins>Installation</ins>:-
* Install the package as `devtools::install_github("wasimaftab/whereIsThisPackage")`

### <ins>Usage</ins>:-
* Call the function by passing a list of R package names, as shown in following example
* `pkgs.found <- whereIsThisPackage(c("limma", "Seurat", "ChAMPdata", "edgeR", "wavelets"))`
