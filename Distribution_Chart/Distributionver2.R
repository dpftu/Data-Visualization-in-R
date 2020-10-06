
'Distribution ver2'

library(tidyverse)
library(graphics)
library(readr)
library(pdftools)


read_csv('women.txt',col_names = FALSE) -> myWomen

for (i in 1:ncol(myWomen)) {
  
  colnames(myWomen)[i] <- str_c('Y',i+1949,sep = '')
  
}


read_csv('men.txt',col_names = FALSE) -> myMen


for (i in 1:ncol(myMen)) {
  
  colnames(myMen)[i] <- str_c('Y',i+1949,sep = '')
  
}




pdf_file <- 'Distributionver2.pdf'
cairo_pdf(pdf_file,bg = 'grey98',height = 9,width = 12)
par(
  
  mai = c(0.2,0.1,0.8,0.1),
  omi = c(0.75,0.2,0.85,0.2),
  mfcol = c(1,2),
  cex = 0.75,
  family = 'Lato Light',
  las = 1
  
)


myWomen$Y1970-myMen$Y1970 -> myWomenSurplus
ifelse(myWomenSurplus < 0,0,myWomenSurplus) -> myWomenSurplus


myMen$Y1970 - myWomen$Y1970 -> myMenSurplus
ifelse(myMenSurplus < 0,0,myMenSurplus) -> myMenSurplus


right <- data.frame(myWomen$Y1970-myWomenSurplus,myWomenSurplus)
left <- data.frame(myMen$Y1970-myMenSurplus,myMenSurplus)

# Create 1970 chart;

myMark_right <- right
myMark_right[!(rownames(myMark_right) %in% seq(10,90,by = 10)),] <- 0


myMark_left <- left
myMark_left[!(rownames(myMark_left) %in% seq(10,90,by = 10)),] <- 0


myColors_right_outer <- rgb(255,0,210,50,maxColorValue = 255)
myColors_right_inner <- rgb(255,0,210,120,maxColorValue = 255)
myColors_left_outer <- rgb(191,239,255,220,maxColorValue = 255)
myColors_left_inner <- rgb(191,239,255,100,maxColorValue = 255)

myColors_right <- c(myColors_right_inner,myColors_right_outer)
myColors_left <- c(myColors_left_inner,myColors_left_outer)


myB1 <- barplot(t(right),axes = FALSE,horiz = TRUE,axis.lty = 0,
                border = NA,col = myColors_right,xlim = c(-750,750))
myB2 <- barplot(-t(left),axes = FALSE,horiz = TRUE,axis.lty = 0,
                border = NA,col = myColors_left,xlim = c(-750,750),
                add = TRUE)


barplot(t(myMark_right),axes = FALSE,horiz = TRUE,axis.lty = 0,
        border = NA,col = myColors_right,xlim = c(-750,750),
        add = TRUE)
barplot(-t(myMark_left),axes = FALSE,horiz = TRUE,axis.lty = 0,
        border = NA,col = myColors_left,xlim = c(-750,750),
        add = TRUE)


abline(v = 0,lwd = 25,col = par('bg'))

mtext(abs(seq(-600,600,by = 200)),at = seq(-600,600,by = 200),
      side = 1,line = -1,cex = 0.8)

for (i in seq(10,90,by = 10)) {
  
  text(0,i+i*0.2,i,cex = 1.1)
  
}

mtext('Men',side = 3,line = 0,adj = 0.25,cex = 1.25,col = 'grey',
      family = 'Lato Black')
mtext('Women',side = 3,line = 0,adj = 0.75,cex = 1.25,col = 'grey',
      family = 'Lato Black')
mtext('Year 1970',side = 3,line = 0,adj = 0.5,cex = 1.25,font = 3,
      family = 'Lato Black')


# Year 2010 chart

myWomen$Y2010-myMen$Y2010 -> myWomenSurplus
ifelse(myWomenSurplus < 0,0,myWomenSurplus) -> myWomenSurplus


myMen$Y2010 - myWomen$Y2010 -> myMenSurplus
ifelse(myMenSurplus < 0,0,myMenSurplus) -> myMenSurplus


right <- data.frame(myWomen$Y2010-myWomenSurplus,myWomenSurplus)
left <- data.frame(myMen$Y2010-myMenSurplus,myMenSurplus)


myMark_right <- right
myMark_right[!(rownames(myMark_right) %in% seq(10,90,by = 10)),] <- 0


myMark_left <- left
myMark_left[!(rownames(myMark_left) %in% seq(10,90,by = 10)),] <- 0


myColors_right_outer <- rgb(255,0,210,50,maxColorValue = 255)
myColors_right_inner <- rgb(255,0,210,120,maxColorValue = 255)
myColors_left_outer <- rgb(191,239,255,220,maxColorValue = 255)
myColors_left_inner <- rgb(191,239,255,100,maxColorValue = 255)

myColors_right <- c(myColors_right_inner,myColors_right_outer)
myColors_left <- c(myColors_left_inner,myColors_left_outer)


myB1 <- barplot(t(right),axes = FALSE,horiz = TRUE,axis.lty = 0,
                border = NA,col = myColors_right,xlim = c(-750,750))
myB2 <- barplot(-t(left),axes = FALSE,horiz = TRUE,axis.lty = 0,
                border = NA,col = myColors_left,xlim = c(-750,750),
                add = TRUE)


barplot(t(myMark_right),axes = FALSE,horiz = TRUE,axis.lty = 0,
        border = NA,col = myColors_right,xlim = c(-750,750),
        add = TRUE)
barplot(-t(myMark_left),axes = FALSE,horiz = TRUE,axis.lty = 0,
        border = NA,col = myColors_left,xlim = c(-750,750),
        add = TRUE)


abline(v = 0,lwd = 25,col = par('bg'))

mtext(abs(seq(-600,600,by = 200)),at = seq(-600,600,by = 200),
      side = 1,line = -1,cex = 0.8)

for (i in seq(10,90,by = 10)) {
  
  text(0,i+i*0.2,i,cex = 1.1)
  
}


mtext('Men',side = 3,line = 0,adj = 0.25,cex = 1.25,col = 'grey',
      family = 'Lato Black')
mtext('Women',side = 3,line = 0,adj = 0.75,cex = 1.25,col = 'grey',
      family = 'Lato Black')
mtext('Year 2010',side = 3,line = 0,adj = 0.5,cex = 1.25,font = 3,
      family = 'Lato Black')

# Titling;

mtext('Age structure of the population in Germany',side = 3,line = 2,
      adj = 0,cex = 2.25,family = 'Lato Black',outer = TRUE)
mtext('All values in thousands per year of age',side = 3,line = -0.5,
      adj = 0,cex = 1.25,font = 3,outer = TRUE)
mtext('Source: Dantri.com.vn.\nDesigned by: Hong Van Nguyen',side = 1,
      line = 2,adj = 1,cex = 0.95,font = 3,outer = TRUE)
mtext('Note: Outer highlighed areas: surplus of women and men.',
      side = 1,line = 1.5,adj = 0,cex = 1.25,font = 3,outer = TRUE)


dev.off()





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






















