library(readxl)
library(readr)
library(tidyverse)
library(pdftools)
library(graphics)
library(RColorBrewer)
library(mosaic)


# Creat Color 

myC1a <- rgb(251,212,150,maxColorValue = 255)
myC2a <- rgb(237,153,118,maxColorValue = 255)
myC3a <- rgb(179,213,148,maxColorValue = 255)
myC4a <- rgb(112,200,230,maxColorValue = 255)
myC1b <- rgb(243,178,40,maxColorValue = 255)
myC2b <- rgb(220,62,42,maxColorValue = 255)
myC3b <- rgb(109,182,68,maxColorValue = 255)
myC4b <- rgb(0,163,218,maxColorValue = 255)

myColors1 <- c(myC1a,myC2a,myC3a,myC4a)
myColors2 <- c(myC1b,myC2b,myC3b,myC4b)

# Creat hypothetical data


a <- c(418.7,418.7) 
b <- c(768.0,768.0) 
c <- c(436.1,436.1) 
d <- c(476.7,478.7)
(x <- as.matrix(data.frame(a,b,c,d)))

a <- c(0,148.6) 
b <- c(0,271.4) 
c <- c(0,154.7)
d <- c(0,185.8)
(y <- as.matrix(data.frame(a,b,c,d)))

# Labels

label1 <- "Humanities and social sciences"
label2 <- "Life sciences"
label3 <- "Natural sciences"
label4 <- "Engineering"
myLabels <- c(label1,label2,label3,label4)



pdf_file <- 'Column_Chart_Shares.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 11,height = 7)
par(
  
  mai = c(0.5,1,0.75,1),
  omi = c(0.75,0.5,1.25,0.5),
  cex = 0.9,
  mgp = c(3,2,0),
  family = 'Lato Light',
  las = 1
  
  )

barplot(x,col = c(rep(myC1a,2),rep(myC2a,2),rep(myC3a,2),rep(myC4a,2)),
        beside = TRUE,border = NA,axes = FALSE,names.arg = rep('',4))
barplot(2*y,col = c(myC1a,myC1b,myC2a,myC2b,myC3a,myC3b,myC4a,myC4b),
        beside = TRUE,border = NA,axes = FALSE,names.arg = myLabels,cex.names = 1.25,
        add = TRUE)

z <- 1

for (i in 1:4) {
  
  text(z+0.25,x[1,i]/2,format(round(x[1,i],1),nsmall = 1),adj = 0)
  text(z+1.25,y[2,i],format(round(y[2,i],1),nsmall = 1),adj = 0,
       col = 'white')
  text(z+0.65,x[1,i]+50,paste(format(round(100*y[2,i]/x[1,i],1),nsmall = 1),'%',sep = ' '),
       adj = 0,cex = 1.25,xpd = TRUE)
  z <- z+3
  
}

mtext('DFG grants in 2020',side = 3,line = 4,adj = 0,family = 'Lato Black',
      outer = TRUE,cex = 2)
mtext('Indiviudal grants by science sector, values in million Euro. Percent value: approval quota.',
      side = 3,line = 2,adj = 0,cex = 1.25,outer =  TRUE)
mtext('Redesigned by: Phan Tien Dung\nUniversity of Tuebingen, Germany',side = 1,
      line = 2.5,adj = 1.0,cex = 0.9,font = 3,outer = TRUE,family = 'Lato Black')

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











