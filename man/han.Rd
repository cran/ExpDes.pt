\name{han}
\alias{han}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Teste de Homogeneidade de Variancia de Han}
\description{Realiza o teste de Han (1969) para homogeneidade de variancias em um delineamento em blocos casualizados.}
\usage{
  han(resp, trat, bloco)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resp}{
    Vetor numerico ou complexo contendo a variavel resposta.
  }
  \item{trat}{
    Vetor numerico ou complexo contendo os tratamentos.
  }
  \item{bloco}{
    Vetor numerico ou complexo contendo os blocos.
  }
}

%\details{ }
%%  ~~ If necessary, more details than the description above ~~
  
\value{
Retorna o valor-p do teste de Han de homogeneidade de variancias para a hipotese nula de que as variancias sao homogeneas\%.
}

\references{
HAN, C. P. Testing the homogeneity of variances in a two-way classification. \emph{Biometrics}, 25:153-158, Mar. 1969.

RIBEIRO, R. \emph{Proposta e comparacao do desempenho de testes para homogeneidade de variancia de modelos de classicacao one-way e two-way}. Iniciacao Cientifica. (Iniciacao Cientifica) - Universidade Federal de Alfenas. 2012.
}

\author{
Denismar Alves Nogueira
  
Eric Batista Ferreira
    
Marcos Costa de Paula
    
Mateus Pimenta Siqueira Lima

Renata Ribeiro
}

%\note{ }
%%  ~~further notes~~
  
  
  %% ~Make other sections like Warning with \section{Warning }{....} ~
  
\seealso{
Veja tambem: \code{\link{anscombetukey}} e \code{\link{oneilldbc}}
}
    
\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, mcomp = "tukey", hvar='han')
    }