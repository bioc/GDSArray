#' GDSArraySeed or GDSArray related methods, slot getters and setters.
#' 
#' @rdname GDSArray-methods
#' @description \code{dim}, \code{dimnames}: dimension and dimnames of
#'     object contained in the GDS file.
#' @param x the \code{GDSArray} and \code{GDSArraySeed} objects.
#' @return \code{dim}: the integer vector of dimensions for
#'     \code{GDSArray} or \code{GDSArraySeed} objects.
#' @return \code{dimnames}: the unnamed list of dimension names for
#'     \code{GDSArray} and \code{GDSArraySeed} objects.
#' @examples
#' fn <- gdsExampleFileName("snpgds")
#' ga <- GDSArray(fn, "sample.annot/pop.group")
#' dim(ga)
#' dimnames(ga)
#' type(ga)
#' seed(ga)
#' dim(seed(ga))
#' gdsfile(ga)

## NOTE: There is no need to define dim() and dimnames() methods for
## GDSArraySeed/GDSArray objects. This is because the dim() and
## dimnames() primitive functions in base R return the content of
## these slots if
## present. http://bioconductor.org/packages/release/bioc/vignettes/DelayedArray/inst/doc/02-Implementing_a_backend.html#dim-and-dimnames

#' @exportMethod seed
#' @description \code{seed}: the \code{GDSArraySeed} getter for
#'     \code{GDSArray} object.
#' @return \code{seed}: the \code{GDSArraySeed} of \code{GDSArray}
#'     object.
setMethod("seed", "GDSArray", function(x) x@seed)

#' @rdname GDSArray-methods
#' @exportMethod "seed<-"
#' @description \code{seed<-}: the \code{GDSArraySeed} setter for
#'     \code{GDSArray} object.
#' @param value the new \code{GDSArraySeed} for the \code{GDSArray}
#'     object.
setReplaceMethod("seed", "GDSArray", function(x, value) {
    x@seed <- BiocGenerics:::replaceSlots(x, seed = value, check = FALSE)
})

#' @rdname GDSArray-methods
#' @description \code{gdsfile}: on-disk location of GDS file
#'     represented by this object.
#' @param object GDSArray, GDSMatrix, GDSArraySeed, GDSFile or
#'     SummarizedExperiment object.
#' @return \code{gdsfile}: the character string for the gds file path.
setGeneric("gdsfile", function(object) standardGeneric("gdsfile"),
           signature="object")

#' @rdname GDSArray-methods
#' @exportMethod gdsfile
setMethod("gdsfile", "GDSArraySeed", function(object) object@filename)

#' @rdname GDSArray-methods
setMethod("gdsfile", "GDSArray", function(object) gdsfile(seed(object)))

#' @rdname GDSArray-methods
setMethod("gdsfile", "DelayedArray", function(object) gdsfile(seed(object)))
