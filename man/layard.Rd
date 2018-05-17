\name{layard}
\alias{layard}

\title{Teste de homogeneidade de variancias de Layard }

\description{Executa o teste de homogeneidade de variancias de Layard (1973) por Jackknife para um delineamento inteiramente casualizado.}

\usage{
  layard(trat, resp, t, r)
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
    Escalar. Numeros de tratamentos
  }
  \item{r}{
   Vetor numerico ou complexo contendo o numero de repeticoes de cada tratamento.
  }
}

%\details{ }
%%  ~~ If necessary, more details than the description above ~~
  
  \value{
  Retorna o valor-p do teste de homogeneidade de variancias de Layard para uma hipotese nula de que as variancias sao homogeneas.
  }

\references{
LAYARD, M. N. J. Robust large-sample tests for homogeneity of variances. \emph{Journal of the American Statistical Association}, v.68, n.341, p.195-198, 1973.

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
    Veja tambem: \code{\link{bartlett}}, \code{\link{samiuddin}}, \code{\link{levene}}, \code{\link{oneillmathews}}
    }
    
\examples{
      data(ex1)
      attach(ex1)
      dic(trat, ig, quali = FALSE, hvar='layard', sigF = 0.05)
    }