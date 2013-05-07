\name{tukey}
\Rdversion{1.1}
\alias{tukey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple comparisons: Tukey
}
\description{
It makes multiple comparisons of treatments by means of Tukey. The level by alpha default is 0.05. 
}
\usage{
tukey(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Variable response 
}
  \item{trt}{
Treatments
}
  \item{DFerror}{
Degree free 
}
  \item{SSerror}{
Sum Square Error
}
  \item{alpha}{
Significant level 
}
  \item{group}{
TRUE or FALSE 
}
  \item{main}{
Title 
}
}
\details{
It is necessary first makes a analysis of variance.
}
\value{
y  Numeric 
trt  factor 
DFerror  Numeric 
MSerror  Numeric 
alpha  Numeric 
group Logic 
main  Text 
}

\references{
STEEL, R. G. D.; TORRIE, J. H.; DICKEY, D. A. Principles and procedures of statistics: a biometrical approach. 3rd Edition. 1997. 666 p.
}

\author{
Denismar Alves Nogueira

Eric Batista Ferreira

Portya Piscitelli Cavalcanti

(Adaptado de Felipe de Mendiburu - GPL)
}

%\note{
%%  ~~further notes~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
LSD.test, waller.test
}

%\examples{

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
