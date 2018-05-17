\name{dic}
\alias{dic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Delineamento Inteiramente Casualizado Simples}
\description{Analisa experimentos em Delineamento Inteiramente Casualizado balanceado com um so fator, considerando o modelo fixo.}
\usage{
dic(trat, resp, quali = TRUE, mcomp = "tukey", nl = FALSE,
hvar='bartlett', sigT = 0.05, sigF = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{trat}{Vetor numerico ou complexo contendo os tratamentos.}

\item{resp}{Vetor numerico ou complexo contendo a variavel resposta.}

\item{quali}{Logico. Se TRUE (\emph{default}), os tratamentos sao entendidos como qualitativos, se FALSE, quantitativos.}

\item{mcomp}{Permite escolher o teste de comparacao multipla; o \emph{default} e o teste de Tukey, contudo tem-se como outras opcoes: o teste LSD ('lsd'), o teste LSDB ('lsdb'), o teste de Duncan ('duncan'), o teste de SNK ('snk'), o teste de Scott-Knott ('sk'), o teste de comparacoes múltiplas \emph{bootstrap} ("ccboot") e o teste de Calinski e Corsten baseado na distribuicao F ("ccf").}

\item{nl}{Logico. Se FALSE (\emph{default}) modelos de regressao lineares são ajustados. Se TRUE, modelos de regressao não-lineares sao ajustados.}

\item{hvar}{Permite escolher o teste de homogeneidade de variancias; o \emph{default} e o teste de Bartlett, contudo tem-se como outras  opcoes: o teste de Levene ('levene'), o teste de Samiuddin ('samiuddin'), o teste de ONeill e Mathews ('oneillmathews') e o teste de Layard ('layard').}

\item{sigT}{Significancia a ser adotada pelo teste de comparacao multipla de médias; o \emph{default} e 5\%.}

\item{sigF}{Significancia a ser adotada pelo teste F da ANAVA; o \emph{default} e 5\%.}
}

\details{
Os argumentos sigT e mcomp so serao utilizados quando os tratamentos forem qualitativos.
}
\value{
Sao retornados os valores da analise de variancia do DIC em questao, o teste de normalidade de Shapiro-Wilk para os residuos do
modelo, o ajuste de modelos de regressao (caso de tratamentos quantitativos) ou os testes de comparacao de medias (caso de tratamentos
qualitativos): teste de Tukey, teste de Duncan, teste t de Student (LSD), teste t de Bonferroni, teste de Student-Newman-Keuls(SNK),
teste de Scott-Knott e teste de comparacoes multiplas bootstrap.
}
\references{
BANZATTO, D. A.; KRONKA, S. N. Experimentacao Agricola. 4 ed. Jaboticabal: Funep. 2006. 237 p.

FERREIRA, E. B.; CAVALCANTI, P. P. Funcao em codigo R para analisar experimentos em DIC simples, em uma so rodada. 
In: REUNIAO ANUAL DA REGIAO BRASILEIRA DA SOCIEDADE INTERNACIONAL DE BIOMETRIA, 54/SIMPOSIO DE ESTATISTICA APLICADA A EXPERIMENTACAO AGRONOMICA, 
13, 2009, Sao Carlos. Programas e resumos... Sao Carlos, SP: UFSCar, 2009. p. 1-5.
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
Para outros exemplos de experimentos em DIC, veja: \code{\link{fat2.dic}}, \code{\link{fat3.dic}}, \code{\link{psub2.dic}}, \code{\link{fat2.ad.dic}} e \code{\link{fat3.ad.dic}}.
}
\examples{
data(ex1)
attach(ex1)
dic(trat, ig, quali = FALSE, sigF = 0.05)
}

