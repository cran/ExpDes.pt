dbc<-function (trat, bloco, resp, quali = TRUE, mcomp = "tukey", nl=FALSE,
               hvar='oneillmathews', sigT = 0.05, sigF = 0.05){
  
Trat <- factor(trat)
Bloco <- factor(bloco)
anava <- aov(resp ~ Trat + Bloco)
tab <- summary(anava)

colnames(tab[[1]]) <- c("GL", "SQ", "QM", "Fc", "Pr>Fc")
tab[[1]] <- rbind(tab[[1]], c(apply(tab[[1]], 2, sum)))
rownames(tab[[1]]) <- c("Tratamento", "Bloco", "Residuo", "Total")
cv <- round(sqrt(tab[[1]][3, 3])/mean(resp) * 100, 2)
tab[[1]][4, 3] = " "
cat("------------------------------------------------------------------------\nQuadro da analise de variancia\n------------------------------------------------------------------------\n")
print(tab[[1]])
cat("------------------------------------------------------------------------\nCV =", 
    cv, "%\n")

#Teste de normalidade
pvalor.shapiro <- shapiro.test(anava$residuals)$p.value
cat("\n------------------------------------------------------------------------\nTeste de normalidade dos residuos \n")
cat("valor-p: ", pvalor.shapiro, "\n")
if (pvalor.shapiro < 0.05) {
  cat("ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!\n------------------------------------------------------------------------\n")
}
else {
  cat("De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.\n------------------------------------------------------------------------\n")
}
  
#Teste de homogeneidade de variância
if(hvar=='oneillmathews') pvalor.hvar<-oneilldbc(resp, Trat, Bloco)
if(hvar=='han') pvalor.hvar<-han(resp, Trat, Bloco)
if(hvar=='anscombetukey') pvalor.hvar<-anscombetukey(resp, Trat, Bloco, tab[[1]][3,1], as.numeric(tab[[1]][3,3]), tab[[1]][1,2], tab[[1]][2,2], anava$residuals, anava$fitted.values)
  
cat('\n------------------------------------------------------------------------\nTeste de homogeneidade de variancia \n')
cat('valor-p: ',pvalor.hvar, '\n')
if(pvalor.hvar<0.05){cat('ATENCAO: a 5% de significancia, as variancias nao podem ser consideradas homogeneas!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de',hvar,'a 5% de significancia, as variancias podem ser consideradas homogeneas.
------------------------------------------------------------------------\n')}
  
if (tab[[1]][1, 5] < sigF) {
  
if (quali == TRUE) {
  
  if(mcomp=='tukey') tukey(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='duncan')duncan(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='lsd')   lsd(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='lsdb')  lsdb(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='sk')    scottknott(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='snk')   snk(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=="ccboot")ccboot(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=="ccf")   ccf(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  
                  }

else if(nl==FALSE) reg<-reg.poly(resp, trat, tab[[1]][3,1],
                                 tab[[1]][3,2], tab[[1]][1,1], tab[[1]][1,2])
else if(nl==TRUE)  reg<-reg.nl(resp, trat)
                            }
else {
cat("\nDe acordo com o teste F, as medias nao podem ser consideradas diferentes.\n")
mean.table <- tapply.stat(resp, trat, mean)
colnames(mean.table) <- c("Niveis", "Medias")
print(mean.table)
cat("------------------------------------------------------------------------\n")
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

  