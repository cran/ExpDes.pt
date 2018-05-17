\name{tukey}
\Rdversion{1.1}
\alias{tukey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Comparacao multipla: Tukey}
\description{Produz a comparacao multipla de tratamentos para as medias pela proposta de Tukey. Por padrao considera a significancia de 0.05.}
\usage{
tukey(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Vetor numerico ou complexo contendo a variavel resposta.
}
  \item{trt}{
Vetor numerico ou complexo contendo os tratamentos.
}
  \item{DFerror}{
Grau de liberdade do residuo.
}
  \item{SSerror}{
Soma de quadrados do residuo.
}
  \item{alpha}{
Significancia do teste de Tukey.
}
  \item{group}{
TRUE ou FALSE
}
  \item{main}{
Titulo
}

}
\details{
E necessario produzir a analise de variancia antes.
}

\references{
STEEL, R. G. D.; TORRIE, J. H.; DICKEY, D. A. Principles and procedures of statistics: a biometrical approach. 3rd Edition. 1997. 666 p.
}

\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti

(Adaptado de Felipe de Mendiburu - GPL)
}

%\note{
%%  ~~further notes~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Veja também: \code{\link{snk}}, \code{\link{duncan}}, \code{\link{lsd}}, \code{\link{lsdb}}, \code{\link{ccboot}}, \code{\link{scottknott}}, \code{\link{ccf}} 
}

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = TRUE, mcomp='tukey', sigT = 0.05)
}
%\examples{

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
