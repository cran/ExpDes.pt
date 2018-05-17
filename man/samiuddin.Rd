\name{samiuddin}
\alias{samiuddin}

\title{Teste de homogeneidade de variancias de Samiuddin.}
\description{Executa o teste de homogeneidade de variancias de Samiuddin (1976) para um delineamento inteiramente casualizado.}

\usage{
samiuddin(trat, resp, t, r)
}

\arguments{
\item{trat}{
Vetor numerico ou complexo contendo os tratamentos.
}
\item{resp}{
Vetor numerico ou complexo contendo a variavel resposta.
}
\item{t}{
Escalar.Numero de tratamentos.
}
\item{r}{
Vetor numerico ou complexo contendo o numero de repeticoes de cada tratamento.
}
}

%\details{ }
%%  ~~ If necessary, more details than the description above ~~
  
  \value{
   Retorna o valor-p do teste de homogeneidade de variancias de Samiuddin para uma hipotese nula de que as variancias sao homogenenas.
  }

\references{
SAMIUDDIN, M. Bayesian test of homogeneity of variance. \emph{Journal of the American Statistical Association}, 71(354):515-517, Jun. 1976.

NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para homogeneidade de variâncias em delineamentos inteiramente casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22. 2013.
}
  
\author{
Denismar Alves Nogueira
    
Eric Batista Ferreira
    
Marcos Costa de Paula
    
Mateus Pimenta Siqueira Lima
}

%\note{ }
  
\seealso{
Veja também: \code{\link{bartlett}}, \code{\link{layard}}, \code{\link{levene}} e \code{\link{oneillmathews}}
}

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = FALSE, hvar='samiuddin')
}
