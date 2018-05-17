\name{bartlett}
\alias{bartlett}

\title{Teste de homogeneidade de variancias de Bartlett}
\description{Executa o teste de homogeneidade de variancias de Bartlett (1937) para um delineamento inteiramente casualizado.}
\usage{
bartlett(trat, resp, t, r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trat}{
Vetor numerico ou complexo contendo os tratamentos.
}
  \item{resp}{
Vetor numerico ou complexo contendo a variavel resposta.
}
  \item{t}{
Escalar. Numero de tratamentos.
}
  \item{r}{
Vetor numerico ou complexo contendo o numero de repeticoes de cada tratamento.
}
}

%\details{ }
%%  ~~ If necessary, more details than the description above ~~

\value{
Retorna o valor-p do teste de homogeneidade de variancias de Bartlett para a hipotese nula de que as variancias sao homogeneas
}

\references{
BARTLETT, M. S. Properties of sufficiency and statistical tests. \emph{Proceedings of the Royal Statistical Society - Serie A}, 60:268-282, 1937.

NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para homogeneidade de variancias em delineamentos inteiramente casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22. 2013.
}

\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Marcos Costa de Paula

Mateus Pimenta Siqueira Lima
}

%\note{ }
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
se also: \code{\link{levene}}, \code{\link{layard}}, \code{\link{oneillmathews}} e \code{\link{samiuddin}}
}

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = FALSE, hvar='bartlett')
}