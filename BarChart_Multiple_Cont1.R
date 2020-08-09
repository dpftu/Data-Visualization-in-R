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



# Hypothetical data: 

c1 <- c(2,40,38,12,8)
c2 <- c(4,20,35,15,26)
c3 <- c(5,10,34,40,10)
c4 <- c(5,10,34,35,16)
c5 <- c(3,40,39,13,5)
c6 <- c(3,37,42,13,5)
c7 <- c(4,30,42,16,8)

# Convert into percentage;

d1 <- c1/sum(c1)
d2 <- c2/sum(c2)
d3 <- c3/sum(c3)
d4 <- c4/sum(c4)
d5 <- c5/sum(c5)
d6 <- c6/sum(c6)
d7 <- c6/sum(c7)

# Data preparation 

myData1 <- matrix(c(d1,d2,d3,d4,d5,d6,d7),byrow = FALSE,nrow = 5)
myData1 <- myData1*100
myResonpse <- c("n.a./dont't know","Agree strongly","Agree","Disagree","Disagree strongly")


pdf_file <- 'BarChart_Multiple_Cont1.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 13,height = 10.5)
par(
  
  omi = c(0.25,0.75,1,0.75),
  mai = c(1.8,3.75,0.25,0),
  lheight = 1.15,
  family = 'Lato Light',
  las = 1
  
  )


# Create colors:

myC1 <- rgb(0,208,226,maxColorValue = 255)
myC2 <- rgb(109,221,225,maxColorValue = 255)
myC3 <- rgb(255,138,238,maxColorValue = 255)
myC4 <- rgb(255,0,210,maxColorValue = 255)
myColors <- c('grey',myC1,myC2,myC3,myC4)

# Barplot;
barplot(-rep(100,7),names.arg = myNames,cex.names = 1.1,horiz = TRUE,
        border = par('bg'),xlim = c(-100,70),col = myColors[1],axes = FALSE)
barplot(-(100-myData1[1,]),names.arg = myNames,cex.names = 1.1,horiz = TRUE,
        border = par('bg'),xlim = c(-100,70),col = par('bg'),axes = FALSE,add = TRUE)
barplot(-myData1[3:2,],names.arg = myNames,cex.names = 1.1,horiz = TRUE,
        border = NA,xlim = c(-100,70),col = myColors[3:2],axes = FALSE,add = TRUE)
barplot(myData1[4:5,],names.arg = myNames,cex.names = 1.1,horiz = TRUE,
        border = NA,xlim = c(-100,70),col = myColors[4:5],axes = FALSE,add = TRUE)


# Other elements 

arrows(0,-0.1,0,8.6,lwd = 2.5,length = 0,xpd = TRUE,col = 'dodgerblue')
px <- c(-98,-87,-41,15,65)
tx <- c(-105,-60,-26,8,60)
y <- rep(-1,5)

points(px,y,pch = 15,cex = 4,col = myColors,xpd = TRUE)
text(tx,y,myResonpse,adj = 1,xpd = TRUE,font = 3)
mtext(c(seq(80,0,by = -20),seq(20,60,by = 20)),side = 1,
      at = c(seq(-80,0,by = 20),seq(20,60,by = 20)),
      line = 0,cex = 0.95)

# Titling 

mtext('It is often said that attitudes towards gender roles are changing',
      side = 3,line = 2.2,adj = 0,cex = 1.8,outer = TRUE,family = 'Lato Black')
mtext('All values in percent',side = 3,line = 1,cex = 0.95,adj = 1,font = 3)
mtext('Redesigned by: Phan Tien Dung\nUniversity of Tuebingen, Germany',
      side = 1,line = 7,adj = 1,cex = 1.05,font = 3,family = 'Lato Black')

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








