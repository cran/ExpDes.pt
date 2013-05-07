\name{ccF}
\alias{ccF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Teste de comparacoes multiplas de Calinski \& Corsten baseado na distribuicao F
}
\description{
Realiza o teste de Calinski \& Corsten baseado na distribuicao F
}
\usage{
ccF(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
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
E retornada a comparacao das medias segundo o teste de Calinski \& Corsten baseado na distribuicao F}
\references{
CALI\'{N}SKI, T.; CORSTEN, L. C. A. Clustering means in ANOVA by Simultaneous Testing. Biometrics. v. 41, p. 39-48, 1985.
}
\author{
Patricia de Siqueira Ramos

Daniel Furtado Ferreira

Eric Batista Ferreira
}
%\note{
%%  ~~further notes~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, mcomp='ccf', sigT = 0.05, sigF = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
