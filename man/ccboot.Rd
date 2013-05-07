\name{ccboot}
\alias{ccboot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparacao multipla: Bootstrap
}
\description{
Realiza o teste de Bootstrap de Ramos e Ferreira (2009) para comparacao multipla de medias.
}
\usage{
ccboot(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL, B = 1000)
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
  \item{B}{
Numero de reamostragens bootstrap.
}
}
%\details{   }
%%  ~~ If necessary, more details than the description above ~~

\value{
E retornada a comparacao das medias segundo o teste de Bootstrap.
}
\references{
RAMOS, P. S., FERREIRA, D. F. Agrupamento de medias via bootstrap de populacoes normais e nao-normais, Revista Ceres, v.56, p.140-149, 2009.
}
\author{
Patricia de Siqueira Ramos

Daniel Furtado Ferreira

Eric Batista Ferreira
}
%\note{  }
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{  }
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = TRUE, mcomp='ccboot', sigF = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
