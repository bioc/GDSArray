persistent <- new.env()
persistent$handles <- list()

#' Acquire the GDS file connection in R in the \code{gds.class} class.
#'
#' Acquire a (possibly cached) \code{gds.class} object given it's path.
#' 
#' @param path String containing a path to a GDS file.
#' @param type String containing the GDS file type. Case
#'     insensitive. Can be "seqgds" for a GDS file with sequencing
#'     data, or "snpgds" for a GDS file with SNP data. This argument
#'     was added for the \code{VariantExperiment} package for certain
#'     functionalities. By default is NULL, which returns a regular
#'     \code{gds.class}.
#' @param other arguments to be passed to \code{openfn.gds()} inside
#'     \code{acquireGDS}.
#' @return For \code{acquireGDS}, by default returns a regular
#'     \code{gds.class} object, which are identical to that returned
#'     by \code{gdsfmt::openfn.gds(path)}. If \code{type} is not NULL,
#'     a \code{SeqVarGDSClass} that is identical to
#'     \code{SeqArray::seqOPen(path)}, or \code{SNPGDSFileClass} that
#'     is identical to \code{SNPRelate::snpgdsOpen(path)}. Both are
#'     inherited from \code{gds.class} but with additional checking
#'     and methods.
#'
#' For \code{releaseGDS}, any existing \code{gds.class} object for the
#' \code{path} is disconnected and cleared from cache, and \code{NULL}
#' is invisibly returned. This is equivalent to that returned by
#' \code{gdsfmt::closefn.gds()} except it take \code{path} as
#' input. If \code{path=NULL}, all cached connections are removed.
#'
#' @author Qian Liu
#' @details \code{acquireConn} will cache the \code{gds.class} object
#'     in the current R session to avoid repeated initialization. This
#'     improves efficiency for repeated calls. The cached
#'     \code{gds.class} object for any given \code{path} can be
#'     deleted by calling \code{releaseGDS} for the same \code{path}.
#'
#' @examples
#' fn <- gdsExampleFileName()
#' gdscon <- acquireGDS(fn)
#' acquireGDS(fn)  ## just re-uses the cache
#' acquireGDS(fn, type = "seqgds") ## construct a new GDS connection 
#' releaseGDS(fn)  ## clears the cache
#' 
#' @export
#' @rdname acquireGDS

acquireGDS <- function(path, type = NULL, ...) {
    ## Here we set up an LRU cache for the GDS connection. 
    ## This avoids the initialization time when querying lots of columns.
    nhandles <- length(persistent$handles)

    i <- which(names(persistent$handles) == path)
    if (length(i)) {
        output <- persistent$handles[[i]]
        if (i < nhandles) {
            persistent$handles <- persistent$handles[c(seq_len(i-1L),
                                                       seq(i+1L, nhandles),
                                                       i)]  ## moving to the back
        }
        fail1 <- type == "seqgds" & !is(output, "SeqVarGDSClass")
        fail2 <- type == "snpgds" & !is(output, "SNPGDSFileClass")
        if (any(fail1, fail2)) {
            releaseGDS(path)
        } else {
            return(output)
        }
    }
    ## Pulled this value from most recent cache
    limit <- 100
    if (nhandles >= limit) {
        persistent$handles <- tail(persistent$handles, limit - 1L)
    }

    ## construct a new connection if not exist
    if (!is.null(type)) type <- tolower(type)
    if (is.null(type)) {
        ex <- expression(openfn.gds(path, ...))
    } else if (type == "seqgds") {
        ex <- expression(SeqArray::seqOpen(path, ...))
    } else if (type == "snpgds") {
        ex <- expression(SNPRelate::snpgdsOpen(path, ...))
    }
    output <- tryCatch(eval(ex), error = function(e) e)
    if (is(output, "simpleError")) {
        showfile.gds(closeall = TRUE)
        output <- eval(ex)
    }
    persistent$handles[[path]] <- output
    output
} 

#' @export
#' @rdname acquireGDS
releaseGDS <- function(path, type = NULL, ...) {
    if (is.null(path)) {
        persistent$handles <- list()
    } else {
        i <- which(names(persistent$handles) == path)
        if (length(i)) {
            gdscon <- persistent$handles[[i]]
            if (is.null(type)) {
                closefn.gds(gdscon)
            } else if (type == "seqgds") {
                SeqArray::seqClose(path, ...)
            } else if (type == "snpgds") {
                SNPRelate::snpgdsClose(path, ...)
            }
            persistent$handles <- persistent$handles[-i]
        }
    }
    invisible(NULL)
}
