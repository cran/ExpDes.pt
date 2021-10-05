#' Statistics of data grouped by factors
#'
#' \code{tapply.stat} This process lies in finding statistics
#' which consist of more than one variable, grouped or crossed
#' by factors. The table must be organized by columns between
#' variables and factors.
#' @param y Data.frame variables.
#' @param x Data.frame factors.
#' @param stat Method.
#' @return
#' y Numeric
#' x Numeric
#' stat method = "mean", ...
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' (Adapted from Felipe de Mendiburu - GPL)
#' @export

tapply.stat<-function(y, x, stat = "mean")
{
    cx <- deparse(substitute(x))
    cy <- deparse(substitute(y))
    x <- data.frame(c1 = 1, x)
    y <- data.frame(v1 = 1, y)
    nx <- ncol(x)
    ny <- ncol(y)
    namex <- names(x)
    namey <- names(y)
    if (nx == 2)
        namex <- c("c1", cx)
    if (ny == 2)
        namey <- c("v1", cy)
    namexy <- c(namex, namey)
    for (i in 1:nx) {
        x[, i] <- as.character(x[, i])
    }
    z <- NULL
    for (i in 1:nx) {
        z <- paste(z, x[, i], sep = "&")
    }
    w <- NULL
    for (i in 1:ny) {
        m <- tapply(y[, i], z, stat)
        m <- as.matrix(m)
        w <- cbind(w, m)
    }
    nw <- nrow(w)
    c <- rownames(w)
    v <- rep("", nw * nx)
    dim(v) <- c(nw, nx)
    for (i in 1:nw) {
        for (j in 1:nx) {
            v[i, j] <- strsplit(c[i], "&")[[1]][j + 1]
        }
    }
    rownames(w) <- NULL
    junto <- data.frame(v[, -1], w)
    junto <- junto[, -nx]
    names(junto) <- namexy[c(-1, -(nx + 1))]
    return(junto)
}
