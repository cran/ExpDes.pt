\name{scottknott}
\Rdversion{1.1}
\alias{scottknott}

\title{
Comparacao multipla: Scott-Knott
}
\description{
Aplica o teste Scott-Knott para a comparacao multipla de medias.
}
\usage{
scottknott(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
}

\arguments{
  \item{y}{
Vetor numerico ou complexo contendo a variavel resposta.
}
  \item{trt}{
Vetor numerico ou complexo contendo os tratamentos.
}
  \item{DFerror}{
Grau de liberdade do residuo.
}
  \item{SSerror}{
Soma de quadrados do residuo.
}
  \item{alpha}{
Significancia do teste de Scott-knott.
}
  \item{group}{
TRUE ou FALSE
}
  \item{main}{
Titulo
}
}
\details{
Nao se aplica.
}
\value{
Retorna uma tabela com as medias dos tratamentos e seu respectivo agrupamento segundo o teste de Scott-Knott.
}
\references{
RAMALHO, M. A. P.; FERREIRA, D. F.; OLIVEIRA, A. C. de. Experimentacao em Genetica e Melhoramento de Plantas. 2a ed. Lavras: UFLA. 2005. 300p.
}
\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti

(Adaptado de Laercio Junio da Silva - GPL(>=2))
}
%\note{
%%  ~~further notes~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

%\examples{

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
