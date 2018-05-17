\name{psub2.dbc}
\Rdversion{1.1}
\alias{psub2.dbc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parcelas subdivididas em DBC}
\description{Analisa experimentos em esquema de parcelas subdivididas em Delineamento em Blocos Casualizados balanceado, considerando o modelo fixo.}
\usage{
psub2.dbc(fator1, fator2, bloco, resp, quali = c(TRUE, TRUE), mcomp = "tukey", 
fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fator1}{Vetor numerico ou complexo contendo os niveis do fator 1.}
  \item{fator2}{Vetor numerico ou complexo contendo os niveis do fator 2.}
  \item{bloco}{Vetor numerico ou complexo contendo os blocos.}
  \item{resp}{Vetor numerico ou complexo contendo a variavel resposta.}
  \item{quali}{Logico, se TRUE (default) na primeira posicao, os niveis do fator 1 sao entendidos como qualitativos, se FALSE, quantitativos; da mesma
  forma, a segunda posicao e referente aos niveis do fator 2.}
  \item{mcomp}{Permite escolher o teste de comparacao multipla; o \emph{default} e o teste de Tukey, contudo tem-se como outras opcoes: o teste LSD
  ('lsd'), o teste LSDB ('lsdb'), o teste de Duncan ('duncan'), o teste de SNK ('snk'), o teste de Scott-Knott ('sk'), o teste de comparacoes
  multiplas bootstrap ('ccboot') e o teste de Calinski e Corsten baseado na distribuicao F ('ccf').}
  \item{fac.names}{Permite nomear os fatores 1 e 2.}
  \item{sigT}{Significancia a ser adotada pelo teste de comparacao multipla de medias; o default e 5\%.}
  \item{sigF}{Significancia a ser adotada pelo teste F da ANAVA; o default e 5\%.}
}
\details{
Os argumentos sigT e mcomp so serao utilizados quando os tratamentos forem qualitativos.
}
\value{
Sao retornados os valores da analise de variancia do DBC em questao, o teste de normalidade de Shapiro-Wilk para os residuos do
modelo, o ajuste de modelos de regressao (caso de tratamentos quantitativos) ou os testes de comparacao de medias (caso de tratamentos
qualitativos): teste de Tukey, teste de Duncan, teste t de Student (LSD), teste t de Bonferroni, teste de Student-Newman-Keuls (SNK),
teste de Scott-Knott e teste de comparacoes multiplas bootstrap; com o desdobramento da interacao, caso esta seja significativa.
}
\references{
BANZATTO, D. A.; KRONKA, S. N. Experimentacao Agricola. 4 ed. Jaboticabal: Funep. 2006. 237 p.
}
\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti
}

\note{
O \code{\link{graficos}} pode ser usado para construir os graficos da regressao. 
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Veja tambem: \code{\link{psub2.dic}} e \code{\link{faixas}}
}

\examples{
data(ex)
attach(ex)
psub2.dbc(trat, dose, rep, resp, quali = c(TRUE, FALSE), mcomp = "tukey", 
 fac.names = c("Tratamento", "Dose"), sigT = 0.05, sigF = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
