% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bartlett.r
\name{bartlett}
\alias{bartlett}
\title{Teste de homogeneidade de variancias de Bartlett}
\usage{
bartlett(trat, resp, t, r)
}
\arguments{
\item{trat}{Vetor numerico ou complexo contendo os
tratamentos.}

\item{resp}{Vetor numerico ou complexo contendo a variavel
resposta.}

\item{t}{Escalar. Numero de tratamentos.}

\item{r}{Vetor numerico ou complexo contendo o numero de
repeticoes de cada tratamento.}
}
\value{
Retorna o valor-p do teste de homogeneidade de
variancias de Bartlett para a hipotese nula de que as
variancias sao homogeneas.
}
\description{
\code{bartlett} Executa o teste de homogeneidade de
variancias de Bartlett (1937) para um delineamento
inteiramente casualizado.
}
\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = FALSE, hvar='bartlett', sigF = 0.05)
}
\references{
BARTLETT, M. S. Properties of sufficiency and
statistical tests. \emph{Proceedings of the Royal
Statistical Society - Serie A}, 60:268-282, 1937.

NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para
homogeneidade de vari?ncias em delineamentos inteiramente
casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22.
2013.
}
\seealso{
\code{\link{levene}}, \code{\link{layard}},
\code{\link{oneillmathews}} e \code{\link{samiuddin}}.
}
\author{
Eric B Ferreira, \email{eric.ferreira@unifal-mg.edu.br}

Denismar Alves Nogueira

Marcos Costa de Paula

Mateus Pimenta Siqueira Lima
}
