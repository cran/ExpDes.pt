\name{oneillmathews}
\alias{oneillmathews}

\title{Teste de homogeneidade de variancias de ONeill e Mathews.}
\description{Executa o teste de homogeneidade de variancias de ONeill e Mathews (2000) para um delineamento inteiramente casualizado.}

\usage{
oneillmathews(trat, resp, t, r)
}

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
Retorna o valor-p do teste de homogeneidade de variancias de ONeill e Mathews para uma hipotese nula de que as variancias sao homogenenas.
  }

\references{
O´NEILL, M. E.; MATHEWS, K. L. A weighted least squares approach to levene test of homogeneity of variance. \emph{Australian e New Zealand Journal Statistical}, 42(1):81-100, 2000.

NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para homogeneidade de variâncias em delineamentos inteiramente casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22. 2013.
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
Veja também: \code{\link{bartlett}}, \code{\link{layard}}, \code{\link{levene}} e \code{\link{samiuddin}}
}

\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = FALSE, hvar='oneillmathews')
}
