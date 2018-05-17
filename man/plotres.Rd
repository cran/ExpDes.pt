\name{plotres}
\alias{plotres}

\title{Conjunto de graficos dos residuos do modelo}

\description{Graficos dos residuos da saida do modelo. Conjunto de quatro graficos sao produzidos: (1) Histograma, (2) Grafico da probabilidade normal dos residuos com bandas de confianca pela estatistica de ordem, (3) Residuos Padronizados versus Valores Ajustados e (4) box-plot (Residuos Padronizados)}

\usage{
plotres(a)
}

\arguments{
\item{a}{Objeto contendo a saida da analise do experimento, tendo esta sido feita utilizando o pacote ExpDes.pt.}
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

\author{Denismar Alves Nogueira}

\note{
Esta pode ser utilizada para contrucao dos graficos dos residuos de qualquer modelo do ExpDes.pt}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{Veja tambem: \code{\link{graficos}}}

\examples{
data(ex1)
attach(ex1)
a<-dic(trat, ig)
plotres(a)
}