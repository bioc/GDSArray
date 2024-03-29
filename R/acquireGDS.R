persistent <- new.env()
persistent$handles <- list()

#' Acquire the GDS file connection in R in the \code{gds.class} class.
#'
#' Acquire a (possibly cached) \code{gds.class} object given it's path.
#' 
#' @param path String containing a path to a GDS file.
#' @return For \code{acquireGDS}, a \code{gds.class} object, which are
#'     identical to that returned by \code{gdsfmt::openfn.gds(path)}
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
#' releaseGDS(fn)  ## clears the cache
#' 
#' @export
#' @rdname acquireGDS

acquireGDS <- function(path) {
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
        return(output)
    }

    ## Pulled this value from most recent cache
    limit <- 100
    if (nhandles >= limit) {
        persistent$handles <- tail(persistent$handles, limit - 1L)
    }

    output <- tryCatch(openfn.gds(path), error = function(e) e)
    if (is(output, "simpleError")) {
        showfile.gds(closeall = TRUE)
        output <- openfn.gds(path)
    }
    persistent$handles[[path]] <- output
    output
} 

#' @export
#' @rdname acquireGDS
releaseGDS <- function(path) {
    if (is.null(path)) {
        persistent$handles <- list()
    } else {
        i <- which(names(persistent$handles) == path)
        if (length(i)) {
            gdscon <- persistent$handles[[i]]
            closefn.gds(gdscon)
            persistent$handles <- persistent$handles[-i]
        }
    }
    invisible(NULL)
}
