\name{dbc}
\alias{dbc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Delineamento em Blocos Casualizados}
\description{Analisa experimentos em Delineamento em Blocos Casualizados balanceado com um so fator, considerando o modelo fixo.}
\usage{
dbc(trat, bloco, resp, quali = TRUE, mcomp = "tukey", nl=FALSE,
hvar='oneillmathews', sigT = 0.05, sigF = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trat}{Vetor numerico ou complexo contendo os tratamentos.}
  \item{bloco}{Vetor numerico ou complexo contendo os blocos.}
  \item{resp}{Vetor numerico ou complexo contendo a variavel resposta.}
  \item{quali}{Logico. Se TRUE (default), os tratamentos sao entendido como qualitativos, se FALSE, quantitativos.}
  \item{mcomp}{Permite escolher o teste de comparacao multipla; o \emph{default} e o teste de Tukey, contudo tem-se como outras opcoes: o teste LSD
  ("lsd"), o teste LSDB ("lsdb"), o teste de Duncan ("duncan"), o teste de SNK ("snk"), o teste de Scott-Knott ("sk"), o teste de comparacoes
  multiplas bootstrap ("ccboot") e o teste de Calinski e Corsten baseado na distribuicao F ("ccf").}
  \item{nl}{Logico. Escolhe se modelos nao lineares devem ser ajustados aos fatores quantitativos. O default e FALSE.}
  \item{hvar}{Permite escolher o teste de homogeneidade de variancias; o \emph{default} e o teste de ONeill e Mathews ("oneillmathews"), contudo tem-se como outras  opcoes: o teste de Han ("han") e o teste de Ascombe e Tukey ("anscombetukey").}
  \item{sigT}{Significancia a ser adotada pelo teste de comparacao multipla de medias; o default e 5\%.}
  \item{sigF}{Significancia a ser adotada pelo teste F da ANAVA; o default e 5\%.}
}
\details{
Os argumentos sigT e mcomp so serao utilizados quando os tratamentos forem qualitativos.
}
\value{                                                                                                
Sao retornados os valores da analise de variancia do DBC em questao, o teste de normalidade de Shapiro-Wilk para os residuos do
modelo, o ajuste de modelos de regressao (caso de tratamentos quantitativos) ou os testes de comparacao de medias (caso de tratamentos
qualitativos): teste de Tukey, teste de Duncan, teste t de Student (LSD), teste t de Bonferroni, teste de Student-Newman-Keuls(SNK),
teste de Scott-Knott  e teste de comparacoes multiplas bootstrap.
}
\references{
BANZATTO, D. A.; KRONKA, S. N. Experimentacao Agricola. 4 ed. Jaboticabal: Funep. 2006. 237 p.

FERREIRA, E. B.; CAVALCANTI, P. P.; NOGUEIRA D. A. Funcao em codigo R para analisar experimentos em DBC simples, em uma so rodada. 
In: JORNADA CIENTIFICA DA UNIVERSIDADE FEDERAL DE ALFENAS-MG, 2, 2009, Alfenas. Resumos... ALfenas: Unifal-MG, 2009.
}
\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti
}

\note{
O \code{\link{graficos}} pode ser usado para construir os graficos da regressao e o \code{\link{plotres}} para analise do residuo da anava. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Para outros exemplos de experimentos em DBC, veja: \code{\link{fat2.dbc}}, \code{\link{fat3.dbc}}, \code{\link{psub2.dbc}}, \code{\link{faixas}}, \code{\link{fat2.ad.dbc}} e \code{\link{fat3.ad.dbc}}.
}

\examples{
data(ex2)
attach(ex2)
dbc(trat, provador, aparencia, quali = TRUE, mcomp="lsd", 
 hvar='oneillmathews', sigT = 0.05, sigF = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
