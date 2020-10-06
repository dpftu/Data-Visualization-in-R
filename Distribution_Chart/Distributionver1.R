
'Ditribution ver1'

library(readr)
library(readxl)
library(tidyverse)
library(graphics)
library(pdftools)


read_csv('women.txt',col_names = FALSE) -> myWomen
for (i in 1:ncol(myWomen)) {
  
  colnames(myWomen)[i] <- str_c('Y',i+1949,sep = '')
  
}



read_csv('men.txt',col_names = FALSE) -> myMen
for (i in 1:ncol(myMen)) {
  
  colnames(myMen)[i] <- str_c('Y',i+1949,sep = '')
  
}

right <- myWomen$Y2010
left <- myMen$Y2010

myColors_right <- c(rep(rgb(210,210,210,maxColorValue = 255),15),
                    rep(rgb(144,157,172,maxColorValue = 255),50),
                    rep(rgb(225,152,105,maxColorValue = 255),length(right)-65))

myColors_left <- myColors_right


pdf_file <- 'Distributionver1.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 9,height = 9)

par(
  
  mai = c(0.5,1,0.5,0.5),
  omi = c(rep(0.5,4)),
  family = 'Lato Light',
  las = 1
  
)


barplot(right,axes = FALSE,horiz = TRUE,axis.lty = 0,border = NA,
        col = myColors_right,xlim = c(-750,750))
barplot(-left,axes = FALSE,horiz = TRUE,axis.lty = 0,border = NA,
        col = myColors_left,xlim = c(-750,750),add = TRUE)


abline(v = 0,lwd = 28,col = par('bg'))


for (i in seq(10,90,by = 10)) {
  
  text(0,i+i*0.2,i,cex = 1.1)
  
}

mtext(abs(seq(-600,600,by = 200)),at = seq(-600,600,by = 200),side = 1,
      line = 1,cex = 0.95)


rect(-1000,15+15*0.2,1000,66+66*0.2,xpd = TRUE,
     col = rgb(210,210,210,90,maxColorValue = 255),
     border = NA)


mtext('Working Age',side = 2,line = 1.5,las = 3,adj = 0.38,cex = 1.15)
mtext('Men',side = 3,line = -5,adj = 0.25,cex = 1.25,col = 'grey',
      family = 'Lato Black')
mtext('Women',side = 3,line = -5,adj = 0.75,cex = 1.25,col = 'grey',
      family = 'Lato Black')

# Titling;

mtext('Age Structure of the population in Germany in 2010',
      side = 3,line = -1.5,adj = 0,cex = 1.75,family = 'Lato Black',
      outer = TRUE)
mtext('Values in thousand per Year of age.',side = 3,line = -3.25,
      adj = 0,cex = 1.25,font = 3,outer = TRUE)
mtext('Source: dantri.com.vn.\nRedesigned by Phan Tien Dung',side = 1,
      line = 0.75,adj = 1,cex = 0.95,font = 3,outer = TRUE)


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

rm(list = ls())
cat('\f')





















