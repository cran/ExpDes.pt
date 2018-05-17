\name{ccf}
\alias{ccf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Teste de comparacoes multiplas de Calinski e Corsten}
\description{Realiza o teste de Calinski e Corsten baseado na distribuicao F}
\usage{
ccf(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Vetor numerico ou complexo contendo a variavel resposta.}
  \item{trt}{Vetor numerico ou complexo contendo os tratamentos.}
  \item{DFerror}{Grau de liberdade do residuo.}
  \item{SSerror}{Soma de quadrados do residuo.}
  \item{alpha}{Significancia do teste de Bootstrap.}
  \item{group}{TRUE ou FALSE}
  \item{main}{Titulo}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~

\value{
E retornada a comparacao das medias segundo o teste de Calinski e Corsten baseado na distribuicao F}
\references{
CALI\'{N}SKI, T.; CORSTEN, L. C. A. Clustering means in ANOVA by Simultaneous Testing. Biometrics. v. 41, p. 39-48, 1985.
}
\author{
Patricia de Siqueira Ramos

Daniel Furtado Ferreira

Eric Batista Ferreira
}

\note{
Deve ser usado para a comparacao de mais de duas medias
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\seealso{
Veja tambem: \code{\link{snk}}, \code{\link{duncan}}, \code{\link{lsd}}, \code{\link{lsdb}}, \code{\link{scottknott}}, \code{\link{tukey}}, \code{\link{ccboot}} 
}

\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, mcomp='ccf', sigT = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
