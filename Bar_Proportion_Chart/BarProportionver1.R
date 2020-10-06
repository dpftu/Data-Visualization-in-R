'Bar Proportion ver1'


library(tidyverse)
library(graphics)
library(readr)
library(readxl)
library(fBasics)
library(pdftools)



pdf_file <- 'bar_proportionver1.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 12,height = 9)
par(
  
  omi = c(0.5,0.5,1.1,0.5),
  mai = c(0,2,0,0.5),
  family = 'Lato Light',
  las = 1
  
)

read_excel('income_five_classes.xlsx',
           sheet = 1,
           col_names = TRUE,
           skip = 1) -> myData

layout(matrix(c(1,2),ncol = 1),heights = c(80,20))

par(mai = c(0,1.75,1,0))

bp1 <- barplot(as.matrix(myData),ylim = c(0,6),width = 0.5,
               axes = FALSE,horiz = TRUE,col = c('grey',seqPalette(5,'OrRd')[2:5]),
               border = NA,names.arg = gsub('.','',names(myData),fixed = TRUE),
               cex.names = 1.35)

mtext(seq(0,100,by = 20),at = seq(0,100,by = 20),side = 1,
      line = 0,cex = 1.15)
arrows(0,-0.03,0,7.3,lwd = 1.45,length = 0,xpd = TRUE,col = 'dodgerblue')
text(100-myData[5,]/2,bp1,cex = 1.1,labels = str_c(round(myData[5,],digits = 0),'%',sep = ''),
     col = 'white',family = 'Lato Light',xpd = TRUE)


# Creat other chart;

par(mai = c(0.55,1.75,0,0))
bp2 <- barplot(as.matrix(rep(20,5)),ylim = c(0,0.5),width = c(0.2),
               horiz = TRUE,col = seqPalette(5,'Greys'),border = par('bg'),
               names.arg = c('Uniform Distribution'),axes = FALSE,
               cex.names = 1.15)


# Other elements;

arrows(0,-0.01,0,0.35,lwd = 1.5,length = 0,xpd = TRUE,col = 'dodgerblue')
text(seq(10,90,by = 20),bp2,labels = str_c(rep(20,5),'%',sep = ' '),
     xpd = TRUE,col = c(rep('black',2),rep('white',3)))


title(main = 'Income Distribution over five classes in different Countries',
      line = 2.75,adj = 0,cex.main = 2.25,family = 'Lato Black',outer = TRUE)


myBreaks <- list('In Mexico the richest 10% of income recipients held over 45% of the overall income in 2000,in the USA it was',
                 '29%, in Germany 24. Compared to 1984 the share did increase.')


for (i in 1:length(myBreaks)) {
  
  mtext(myBreaks[[i]],side = 3,line = (1.8-i)*1.5,adj = 0,cex = 1.15,outer = TRUE)
  
}
mtext('Source: Dantri.com.vn\nRedesigned by: Phan Tien Dung, Germany',side = 1,
      adj = 1,cex = 0.95,font = 3,outer = TRUE)


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









