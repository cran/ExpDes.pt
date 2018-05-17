\name{anscombetukey}
\alias{anscombetukey}

\title{Teste de Homegeneidade de Variancia: Anscombe e Tukey}

\description{Realiza o teste de Anscombe e Tukey (1963) para o teste de homegenidade de variancias em um delineamento em blocos casualizados.}

\usage{
  anscombetukey(resp, trat, bloco, glres, qmres, sqtrat, 
  sqbloco, residuos, valores.ajustados)
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
  \item{glres}{
    Escalar. Graus de liberdade do residuo.
  }
  \item{qmres}{
    Escalar. Quadrado medio do residuo.
  }
  \item{sqtrat}{
    Escalar. Soma de quadrados do tratamento.
  }
  \item{sqbloco}{
    Escalr. Soma de quadrados de boco.
  }
  \item{residuos}{
    Vetor numerico ou complexo contendo os residuos.
  }
  \item{valores.ajustados}{
    Vetor numerico ou complexo contendo os valores ajustados.
  }
}

%\details{ }
%%  ~~ If necessary, more details than the description above ~~
  
\value{
Retorna o valor-p do teste de Anscombe e Tukey de homogeneidade de variancias para uma hipotese nula de que as variancias sao homogeneas\%.
}

\references{ 
ANSCOMBE, F. J.; TUKEY, J. W. \emph{The examination and analysis of residuals.} Technometrics, 5:141-160, 1963.

RIBEIRO, R. \emph{Proposta e comparação do desempenho de testes para homogeneidade de variância de modelos de classicação one-way e two-way}. Iniciação Científica. (Iniciação Científica) - Universidade Federal de Alfenas. 2012.
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
    Veja tambem: \code{\link{han}} e \code{\link{oneilldbc}}
  }

\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, hvar='anscombetukey')
}