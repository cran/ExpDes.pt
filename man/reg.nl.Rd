\name{reg.nl}
\alias{reg.nl}

\title{Regressao Nao-linear}

\description{Ajusta modelos de regressao nao-linear na ANAVA.}

\usage{reg.nl(resp, trat)}

\arguments{
\item{resp}{Vetor numerico ou complexo contendo a variavel resposta.}

\item{trat}{Vetor numerico ou complexo contendo os tratamentos.}
}

%\details{

\value{Sao retornados os valores dos ajustes de modelos de regressao.}

\references{DRAPER, N.R.; SMITH, H. \emph{Apllied regression analysis}. 3ed. New York : John Wiley, 1998. 706p.}

\author{
Luiz Alberto Beijo

Eric Batista Ferreira
}

%\note{

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ Veja tambem: \code{\link{graficos}}}

\examples{
data(ex_nl)
attach(ex_nl)
x<-dic(trat, resp, quali=FALSE, nl=TRUE)
par(mfrow=c(1,2))
graficos(x, grau='pot')
graficos(x, grau='exp')
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
