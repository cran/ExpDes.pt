#Teste de Anscombe&Tukey
#
anscombetukey<-function(resp, trat, bloco, glres, qmres, sqtrat, sqbloco, residuos, valores.ajustados)
{
 Trat<-length(trat)
 Bloco<-length(bloco)
 div1<-(2*glres*(qmres)^2)/glres+2
 div2<-((((Trat-2)*(Bloco-1))/Trat*Bloco)*sqtrat)+((((Trat-1)*(Bloco-2))/Trat*Bloco)*sqbloco)
 Fc17<-((sum((residuos^2)*(valores.ajustados-mean(resp))))^2)/(div1*div2)
 pvalor.hvar<-1-pf(Fc17,1,((Trat-1)*(Bloco-1)))
 output <- pvalor.hvar
 return(output)
}
