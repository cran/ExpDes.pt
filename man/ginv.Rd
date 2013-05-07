\name{ginv}
\alias{ginv}

\title{
Inversa generalizada
}
\description{
Calcula a inversa generalizada de Moore-Penrose de uma matriz X. 
}
\usage{
ginv(X, tol = sqrt(.Machine$double.eps))
}

\arguments{
  \item{X}{
Matriz para a qual deseja-se a inversa de Moore-Penrose.
}
  \item{tol}{
Uma tolerancia relativa para detectar valores singulares zero.
}
}
\value{
Uma inversa generalizada de Moore-Penrose para X. 
}
\references{
VENABLES, W. N.; RIPLEY, B. D. Modern Applied Statistics with S-PLUS. Third Edition. Springer. 1999. p. 100.
}

\seealso{
Veja tambem: solve, svd, eigen.
}
\examples{
## Not run: 
# The function is currently defined as
function(X, tol = sqrt(.Machine$double.eps))
{
## Generalized Inverse of a Matrix
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] \%*\% (t(s$u[, nz])/s$d[nz]) else X, dimnames = dnx[2:1])
}

## End(Not run)
}
