\name{levene}
\alias{levene}

\title{Teste de homogeneidade de variancias de Levene}
\description{Executa o teste de homogeneidade de variancias de Levene (1960) para um delineamento inteiramente casualizado.}

\usage{
levene(trat, resp, t, r)
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
  Retorna o valor-p do teste de homogeneidade de variancias de Levene para uma hipotese nula de que as variancias sao homogenenas.
  }

\references{
LEVENE, H. Robust tests for equality of variances. in: Olkin, i.; ghurye, s.g.; hoeffding, w.; madow, w.g.; mann, h.b. (eds.). \emph{Contribution to Probability and Statistics. Stanford}, CA: Stanford University Press, pages 278-292, 1960.

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
    Veja também: \code{\link{bartlett}}, \code{\link{layard}}, \code{\link{oneillmathews}}, \code{\link{samiuddin}}
    }
\examples{
      data(ex1)
      attach(ex1)
      dic(trat, ig, quali = FALSE, hvar='levene')
    }