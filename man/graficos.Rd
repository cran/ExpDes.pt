\name{graficos}
\alias{graficos}

\title{Gráfico de modelos de regressao}

\description{Constroi graficos para os modelos de regressao ajustados na anava}

\usage{
graficos(a, grau = 1, mod = TRUE, main = " ", sub = " ",
         xlab = "Niveis (X)", ylab = "Resposta (Y)", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")
}

\arguments{
\item{a}{Objeto contendo a saída da analise do experimento, tendo esta sido feita utilizando o pacote ExpDes.pt.}
\item{grau}{Determina o modelo a ser utilizado na construcao do grafico. Para modelos polinomiais, 1 (reta) e o \emph{default}, 2 (parabola), 3 (cubica), "pot" (modelo potencia) e "exp" (modelo exponencial).}
\item{mod}{Logico. Da a opcao de imprimir o modelo ajustado e seu coeficiente de determinacao ($R^2$) no topo do grafico. O \emph{default} e TRUE.}
\item{main}{Titulo do grafico. Vazio e o \emph{default}.}
\item{sub}{Subtítulo do grafico. Vazio e o \emph{default}.}
\item{xlab}{Nome do eixo X.}
\item{ylab}{Nome do eixo Y.}
\item{pch}{Tipo de caractere que se deseja marcar os valores observados.}
\item{xlim}{Limites do eixo X.}
\item{ylim}{Limites do eixo Y.}
\item{bty}{Tipo de caixa em que o grafico estara inserido. O \emph{default} e "o".}
}

%\details{

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\references{
STEEL, R. G. D.; TORRIE, J. H. \emph{Principles and procedures in Statistics: a biometrical approach}. McGraw-Hill, New York, NY. 1980.
}

\author{Eric Batista Ferreira}

%\note{
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{reg.poly}}, \code{\link{plotres}}}

\examples{
data(ex1)
attach(ex1)
a<-dic(trat, ig, quali=FALSE)
graficos(a, grau=1)
graficos(a, grau=2)
graficos(a, grau=3)
}