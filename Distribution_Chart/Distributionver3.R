
'Distribution ver3'

library(tidyverse)
library(graphics)
library(readr)
library(readxl)
library(stringr)
library(pdftools)



pdf_file <- 'Distributionver3.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 12,height = 8)
par(
  
  mai = c(0.2,0.1,0.8,0.1),
  omi = c(0.75,0.2,0.85,0.2),
  mfcol = c(1,2),
  cex = 0.75,
  family = 'Lato Light',
  las = 1
  
)

# Data preparation: Year 1991;


read_excel('12411-0008_1991_reformatted.xlsx',
           sheet = 1,
           col_names = c('alter',str_c('x',1:16,sep = '')),
           skip = 1) -> x1991
x1991[1:nrow(x1991)-1,] -> x1991
attach(x1991)


myF_married <- x6/1000
myF_unmarried <- (x5+x7+x8)/1000
myM_married <- x2/1000
myM_unmarried <- (x1+x3+x4)/1000


right <- data.frame(myF_married,myF_unmarried)
left <- data.frame(myM_married,myM_unmarried)


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
mtext('Year 1991',side = 3,line = 0,adj = 0.5,cex = 1.25,font = 3,
      family = 'Lato Black')


# Chart Year 2010

read_excel('12411-0008_2010_reformatted.xlsx',
           sheet = 1,
           col_names = c('alter',str_c('x',1:16,sep = '')),
           skip = 1) -> x2010
x2010[1:nrow(x1991)-1,] -> x2010
attach(x1991)


myF_married <- x6/1000
myF_unmarried <- (x5+x7+x8)/1000
myM_married <- x2/1000
myM_unmarried <- (x1+x3+x4)/1000


right <- data.frame(myF_married,myF_unmarried)
left <- data.frame(myM_married,myM_unmarried)

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

mtext('Age structure and married proportion of the population in Germany',
      side = 3,line = 2,adj = 0,cex = 2.25,family = 'Lato Black',
      outer = TRUE)
mtext('All values in thousands per year of age',side = 3,line = -0.5,
      adj = 0,cex = 1.25,font = 3,outer = TRUE)
mtext('Source: Dantri.com.vn\nDesigned by: Phan Tien Dung,Germany',
      side = 1,line = 2,adj = 1,cex = 0.95,font = 3,outer = TRUE)
mtext('Inner highlighted the areas: MARRIED',side = 1,line = 2,adj = 0,
      cex = 0.95,font = 3,outer = TRUE)

dev.off()


# Converge image;

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
































