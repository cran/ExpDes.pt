\name{oneilldbc}
\alias{oneilldbc}

\title{Teste de Homegeneidade de Variancia: ONeill e Mathews}
\description{Realiza o teste de ONeill e Mathews (2002) para o teste de homegenidade de variancias para um delineamento em blocos casualizados.}

\usage{
  oneilldbc(resp, trat, bloco)
}

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
    Retorna o valor-p do teste de ONeill e Mathews para a hipotese nula de que as variancias sao homogeneas\%.
  }

\references{
O´NEILL, M. E.; MATHEWS, K. L. Levene tests of homogeneity of variance for general block and treatment designs. \emph{Biometrics}, 58:216-224, Mar. 2002.

RIBEIRO, R. \emph{Proposta e comparacao do desempenho de testes para homogeneidade de variancia de modelos de classi cacao one-way e two-way}. Iniciacao Cientifica. (Iniciação Científica) - Universidade Federal de Alfenas. 2012.
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
See also: \code{\link{anscombetukey}} e \code{\link{han}}
}

\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, hvar='oneillmathews')
}
