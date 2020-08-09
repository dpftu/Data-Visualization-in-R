


library(readxl)
library(readr)
library(tidyverse)
library(pdftools)
library(graphics)

# Different kind of barchart multiple;
# The data was created by author.


myC_v159 <- "A working mother can establish just as warm and\nsecure an environment as a non-working mother"
myC_v160 <- "A pre-school child is likely to suffer if\nhis or her mother is working"
myC_v161 <- "A job is alright, but what most women\nreally want is a home and children"
myC_v162 <- "Being a housewife is just as fulfilling as\nworking"
myC_v163 <- "Having a job is the best way for a woman\nto be independent"
myC_v164 <- "Both the husband and wife should contribute\nto the family income"
myC_v165 <- "In general, fathers are as well suited to\nlook after their children as women"
myC_v166 <- "Men should take as much responsibility\nas women for their household and children"
myNames <- c(myC_v165, myC_v164, myC_v163, myC_v162, myC_v161, myC_v160, myC_v159)


# Create artificial data;

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
myData1
myResonpse <- c("n.a./dont't know","Agree strongly","Agree","Disagree","Disagree strongly")


pdf_file <- 'BarChart_Multiple_Cont.pdf'
cairo_pdf(filename = pdf_file,bg = 'grey98',width = 13,height = 10.5)
par(
  
  omi = c(0,0.75,1.25,0.75),
  mai = c(1.6,3.75,0.5,0),
  lheight = 1.15,
  family = 'Lato Light',
  las = 1
  
  )


myC1 <- rgb(0,208,226,maxColorValue = 255)
myC2 <- rgb(109,221,225,maxColorValue = 255)
myC3 <- rgb(255,138,238,maxColorValue = 255)
myC4 <- rgb(255,0,210,maxColorValue = 255)
myColors <- c('grey',myC1,myC2,myC3,myC4)


x <- barplot(myData1,horiz = TRUE,names.arg = myNames,cex.names = 1.1,border = NA,
             xlim = c(0,100),col = myColors,axes = FALSE)



px <- c(2,8,35,68,98)
py <- rep(9,5)
tx <- c(-2,23,43,65,95)
ty <- rep(9,5)

points(px,py,pch = 15,cex = 4,col = myColors,xpd = TRUE)
text(tx,ty,myResonpse,adj = 1,xpd = TRUE,family = 'Lato Light',font = 3)
mtext(seq(0,100,by = 20),side = 1,at = seq(0,100,by = 20),cex = 0.9)

# Add title


mtext("It is often said that the attitudes towards gender role are changing:",side = 3,
      line = 2.2,adj = 0,cex = 1.8,outer = TRUE,family = 'Lato Black')
mtext('All values in percent.',side = 1,line = 1,adj = 1,cex = 0.95,font = 3)
mtext('Redesigned by: Phan Tien Dung\nUniversity of Tuebingen, Germany',
      side = 1,line = 4.5,adj = 1,cex = 1.05,font = 3,family = 'Lato Black')
mtext("N = 10,000 observations.",side = 1,line = 2,adj = 0,cex = 1.15,family = 'Lato Black',font = 3)


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








