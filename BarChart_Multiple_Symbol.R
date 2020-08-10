


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


# Note: Need to setup font for symbol;



pdf_file <- 'BarChart_Multiple_Symbol.pdf'
cairo_pdf(filename = pdf_file,bg = 'grey98',width = 13,height = 10.5)
par(
  
  omi = c(0.65,0.65,0.85,0.85),
  mai = c(1.1,5.85,1.55,0),
  family = 'Lato Light',
  las = 1
  
  )

col_female <- rgb(255,97,0,190,maxColorValue = 255)
col_male <- rgb(68,90,111,190,maxColorValue = 255)


# Chart 

plot(1:5,type = 'n',axes = FALSE,xlab = '',ylab = '',xlim = c(0,20),ylim = c(1,6))

mySymbols <- function(n_f,n_m,y,labelling,...){
  
  par(family = "Symbol Signs")
  
  for (i in 1:n_f) {
    
    text(runif(1,0,(n_f+n_m)/10),runif(1,y,y+1),labels = "F",cex = 3.35,col = col_female)
    
  }
  
  for (i in 1:n_f) {
    
    text(runif(1,0,(n_f+n_m)/10),runif(1,y,y+1),labels = 'M',cex = 3.25,col = col_male)
    
  }
  
  par(family = 'Lato Light')
  text(-3,y+0.5,labels = labelling,xpd = TRUE,cex = 1.45,adj = 1)
  
}

mySymbols(round(336/10),round(350/10),1,labelling = myC_v161)
mySymbols(round(454/10),round(525/10),3,labelling = myC_v160)
mySymbols(round(865/10),round(720/10),5,labelling = myC_v159)
axis(side = 1,at = seq(0,20,by = 5),labels = c('0','500','1000','1500','2000'),
     col = par('bg'),col.ticks = 'grey81',lwd.ticks = 0.5,tick = -0.025) 

abline(v = seq(0,20,by = 5),lty = 'dotted')

# Titling 

mtext('It is often said that attitudes towards gender roles are changing.',
      side = 3,line = -0.5,adj = 0,cex = 2,family = 'Lato Black',outer = TRUE)
mtext('Agree strongly/ Argree',side = 3,line = -3,adj = 0,cex = 1.5,
      outer = TRUE,font = 3)
mtext('Redesigned by: Phan Tien Dung\nUnivesrity of Tuebingen,Germany.',
      side = 1,line = 0,adj = 1,cex = 1.25,outer = TRUE,font = 3,
      family = 'Lato Black')
mtext('300,00 respondents. Every figure represents 10 people.',
      side = 1,line = -2,adj = 0,cex = 1.25,outer = TRUE,font = 3)

par(family = 'Lato Light')
mtext('Women',side = 1,line = 0.5,adj = 0.02,cex = 1.5,outer = TRUE,font = 3)
mtext('Men',side = 1,line = 0.5,adj = 0.12,cex = 1.5,outer = TRUE,font = 3)

par(family = 'Symbol Signs')
mtext('F',side = 1,line = 0.5,adj = 0,cex = 1.5,outer = TRUE,font = 3,col = col_female)
mtext('M',side = 1,line = 0.5,adj = 0.1,cex = 1.5,outer = TRUE,font = 3,col = col_male)


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














