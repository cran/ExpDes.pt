\name{lsd}
\Rdversion{1.1}
\alias{lsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Comparacao multipla: LSD}
\description{Realiza o teste t (LSD) para comparacao multipla de medias.}
\usage{
lsd(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
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
Significancia do teste de LSD.
}
  \item{group}{
TRUE ou FALSE
}
  \item{main}{
Titulo
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~

\value{
E retornada a comparacao das medias segundo o teste LSD.
}
%\references{
%% ~put references to the literature/web site here ~

\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti
}
%\note{
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\seealso{
Veja tambem: \code{\link{snk}}, \code{\link{duncan}}, \code{\link{ccboot}}, \code{\link{lsdb}}, \code{\link{scottknott}}, \code{\link{tukey}}, \code{\link{ccf}} 
}

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = TRUE, mcomp='lsd', sigT = 0.05)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
