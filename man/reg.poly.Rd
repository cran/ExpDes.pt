\name{reg.poly}
\Rdversion{1.1}
\alias{reg.poly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regressao Polinomial
}
\description{
Realiza ajustes de modelos sequenciais de regressao ate o terceiro grau.
}
\usage{
reg.poly(resp, trat, glres, SQres, gltrat, SQtrat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resp}{
Vetor numerico ou complexo contendo a variavel resposta.
}
  \item{trat}{
Vetor numerico ou complexo contendo os tratamentos.
}
  \item{glres}{
Grau de liberdade do residuo.
}
  \item{SQres}{
Soma de quadrados do residuo.
}
  \item{gltrat}{
Grau de liberdade do tratamento.
}
  \item{SQtrat}{
Soma de quadrados do tratamento.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~

\value{
Sao retornados os valores dos ajustes de modelos de regressao.
}
\references{
GOMES, F. P. Curso de Estatistica Experimental. 10a ed. Piracicaba: ESALQ/USP. 1982. 430.
}
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

%\examples{

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
