dic<-function(trat, resp, quali=TRUE, mcomp='tukey', nl=FALSE,
              hvar='bartlett', sigT=0.05, sigF=0.05) {

Trat<-factor(trat)
anava<-aov(resp~Trat)
tab<-summary(anava)

#### Encontrar o vetor com o numero de repetições ####
#i<-0
#ii<-1
#rr<-1
t<-length(levels(Trat))
#r<-matrix(0,t,1)
#for(i in 1:(length(Trat)-1)) {
#  if (Trat[i]==Trat[i+1]) {rr<-rr+1} else {r[ii]<-rr}
#  if (Trat[i]!=Trat[i+1]) rr<-1
#  if (Trat[i]!=Trat[i+1]) ii<-ii+1
#  if ((i+1)==length(Trat)){r[ii]<-rr} 
#}
r<-as.numeric(table(Trat))
#########################################

colnames(tab[[1]])<-c('GL','SQ','QM','Fc','Pr>Fc')
tab[[1]]<-rbind(tab[[1]],c(apply(tab[[1]],2,sum)))
rownames(tab[[1]])<-c('Tratamento','Residuo','Total')
cv<-round(sqrt(tab[[1]][2,3])/mean(resp)*100, 2)
tab[[1]][3,3]=' '
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(tab[[1]])
cat('------------------------------------------------------------------------\nCV =',cv,'%\n')

#Teste de normalidade
pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
cat('\n------------------------------------------------------------------------\nTeste de normalidade dos residuos \n')
cat('Valor-p: ',pvalor.shapiro, '\n')
if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
------------------------------------------------------------------------\n')}

#Teste de homogeneidade de variância
if(hvar=='bartlett') pvalor.hvar<-bartlett(trat,resp,t,r)
if(hvar=='levene') pvalor.hvar<-levene(trat,resp,t,r)
if(hvar=='samiuddin') pvalor.hvar<-samiuddin(trat,resp,t,r)
if(hvar=='oneillmathews') pvalor.hvar<-oneillmathews(trat,resp,t,r)
if(hvar=='layard') pvalor.hvar<-layard(trat,resp,t,r)

cat('\n------------------------------------------------------------------------\nTeste de homogeneidade de variancia \n')
cat('valor-p: ',pvalor.hvar, '\n')
if(pvalor.hvar<0.05){cat('ATENCAO: a 5% de significancia, as variancias nao podem ser consideradas homogeneas!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de',hvar,'a 5% de significancia, as variancias podem ser consideradas homogeneas.
------------------------------------------------------------------------\n')}


if(tab[[1]][1,5]<sigF) {

if(quali==TRUE) {

  if(mcomp=='tukey') tukey(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='duncan')duncan(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='lsd')   lsd(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='lsdb')  lsdb(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='sk')    scottknott(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='snk')   snk(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=="ccboot")ccboot(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=="ccf")   ccf(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  
                }

else if(nl==FALSE) reg<-reg.poly(resp, trat, tab[[1]][2,1],
      tab[[1]][2,2], tab[[1]][1,1], tab[[1]][1,2])
else if(nl==TRUE)  reg<-reg.nl(resp, trat)

                       }

else {
    cat('\nDe acordo com o teste F, as medias nao podem ser consideradas diferentes.\n')
    cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp,trat,mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------\n')
}

#Saida
out<-list()
out$residuos<-anava$residuals
out$gl.residual<-anava$df.residual
out$coeficientes<-anava$coefficients
out$efeitos<-anava$effects
out$valores.ajustados<-anava$fitted.values
out$medias<-tapply.stat(resp,trat,mean)
if(quali==FALSE && tab[[1]][1,5]<sigF) {out$reg<-reg}
invisible(out)
}

