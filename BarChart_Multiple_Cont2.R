
library(readxl)
library(readr)
library(tidyverse)
library(pdftools)
library(graphics)
library(RColorBrewer)


myC_v159 <- "A working mother can establish just as warm and\nsecure an environment as a non-working mother"
myC_v160 <- "A pre-school child is likely to suffer if\nhis or her mother is working"
myC_v161 <- "A job is alright, but what most women\nreally want is a home and children"
myC_v162 <- "Being a housewife is just as fulfilling as\nworking"
myC_v163 <- "Having a job is the best way for a woman\nto be independent"
myC_v164 <- "Both the husband and wife should contribute\nto the family income"
myC_v165 <- "In general, fathers are as well suited to\nlook after their children as women"
myC_v166 <- "Men should take as much responsibility\nas women for their household and children"
myNames <- c(myC_v165, myC_v164, myC_v163, myC_v162, myC_v161, myC_v160, myC_v159)

c1 <- c(2,40,38,12,8)
c2 <- c(4,20,35,15,26)
c3 <- c(5,10,34,40,10)
c4 <- c(5,10,34,35,16)
c5 <- c(3,40,39,13,5)
c6 <- c(3,37,42,13,5)
c7 <- c(4,30,42,16,8)
d1 <- c1/sum(c1)
d2 <- c2/sum(c2)
d3 <- c3/sum(c3)
d4 <- c4/sum(c4)
d5 <- c5/sum(c5)
d6 <- c6/sum(c6)
d7 <- c6/sum(c7)

# Hypothetical Data;

myData1 <- matrix(c(d1,d2,d3,d4,d5,d6,d7),byrow = FALSE,nrow = 5)
myData1 <- myData1*100
myData1 <- t(myData1)
myResonpse <- c("n.a./dont't know","Agree strongly","Agree","Disagree","Disagree strongly")



pdf_file <- 'BarChart_Multiple_Cont2.pdf'
cairo_pdf(filename = pdf_file,bg = 'grey98',width = 13,height = 10.5)
par(omi = c(rep(1.25,3),0.25),
    lheight = 1.15,
    family = 'Lato Light',
    las = 1)

layout(matrix(seq(1,5,by = 1),nrow = 1,ncol = 5),
       widths = c(2.5,rep(1,4)),heights = c(1,1))
DD_pos <- c(rep(45,4),35)
myC1 <- rgb(0,208,226,maxColorValue = 255)
myC2 <- rgb(109,221,225,maxColorValue = 255)
myC3 <- rgb(255,138,238,maxColorValue = 255)
myC4 <- rgb(255,0,210,maxColorValue = 255)
myColors <- c('grey',myC1,myC2,myC3,myC4)

for (i in 1:5) {
  
  if (i == 1) {
    
    par(mai = c(0.25,2.75,0.25,0.15))
    bp1 <- barplot(myData1[,i],horiz = TRUE,cex.names = 1.6,names.arg = myNames,
                   xlim = c(0,50),col = myColors[i],border = NA,axes = FALSE)
    
  }else {
    
    par(mai = c(0.25,0.1,0.25,0.15))
    bp2 <- barplot(myData1[,i],horiz = TRUE,axisnames = FALSE,xlim = c(0,50),
                   col = myColors[i],border = NA,axes = FALSE)
    
  }
  
  rect(0,0,10,8.5,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
  rect(10,0,20,8.5,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
  rect(20,0,30,8.5,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
  rect(30,0,40,8.5,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
  rect(40,0,50,8.5,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
  
  mtext(myResonpse[i],3,adj = 0,line = 0,cex = 0.95,font = 3)
  mtext(seq(10,50,by = 10),at = seq(10,50,by = 10),side = 1,line = 1,cex = 0.85)
  mtext(0,at = 0,side = 1,line = 1,cex = 0.9,family = 'Lato Bold')
  arrows(0,-0.1,0,8.6,lwd = 2.5,length = 0,xpd = TRUE,col = 'dodgerblue')
  
}

# Titling adjustments:!!!

mtext('It is often said that attitudes towards gender roles are changing.',
      side = 3,line = 3.5,adj = -0.25,cex = 2,family = 'Lato Black',
      outer = TRUE)
mtext('N = 30,000 observations.',side = 1,line = 3,adj = 0.25,cex = 1.1,
      family = 'Lato Light',font = 4,outer = TRUE)
mtext('Value in all percent.',side = 1,line = 1.5,adj = 1,cex = 1.1,
      font = 3,outer = TRUE)
mtext('Redesigned:Phan Tien Dung.\nUniversity of Tuebingen, Germany.',
      side = 1,line = 6.5,adj = 1,cex = 1.05,outer = TRUE,font = 3,
      family = 'Lato Black')

dev.off()


# Convert image form;

pdf_convert(
  
  
  pdf = pdf_file,
  format = 'png',
  pages = NULL,
  filenames = NULL,
  dpi = 72,
  antialias = TRUE,
  opw = '',
  upw = '',
  verbose = TRUE
  
  
)













