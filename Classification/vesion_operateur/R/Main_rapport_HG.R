library("knitr")
source("R/Packages.R")
#Function
source("R/Function.R",encoding = "UTF-8")
knitr::opts_chunk$set(error = TRUE)

#SPD jointure 
# knit2pdf("Latex/rapport_HG.Rnw", compiler = 'xelatex')
#Sans CPT 
knit2pdf("Latex/rapport_HG_Sans_CPT.Rnw", compiler = 'xelatex')
