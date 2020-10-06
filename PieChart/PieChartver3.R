

'Pie Chart ver3'

library(tidyverse)
library(graphics)
library(stringr)
library(readr)
library(readxl)
library(RColorBrewer)
library(plotrix)
library(pdftools)



pdf_file <- 'PieChartver3.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 13,height = 12)
par(
  mfcol = c(2,3),
  omi = rep(c(1.5,0),2),
  mai = rep(0,4),
  cex.axis = 0.9,
  cex.lab = 1.1,
  xpd = TRUE,
  col.axis = 'green',
  col.main = 'red',
  family = 'Lato Light',
  las = 1
)

read_excel('worldenergymix.xlsx',sheet = 1) -> mydf
colnames(mydf) <- c('Region','Oil','Coal','Gas','Renew.Energy','Nuclear.Energy')

mydf %>% 
  as.data.frame() -> mydf
rownames(mydf) <- mydf$Region

mydf %>% 
  select(-Region) -> mydf

myLabellings <- colnames(mydf)

for (i in 2:nrow(mydf)) {
  
  radial.plot(rep(100/length(mydf),length(mydf)),labels = myLabellings,
              rp.type = 'p',main = '',line.col = 'grey',show.grid = TRUE,
              show.grid.labels = FALSE,radial.lim = c(0,55),poly.col = 'grey')
  radial.plot(mydf[i,],labels = '',rp.type = 'p',main = '',line.col = 'red',
              show.grid = FALSE,radial.lim = c(0,55),poly.col = 'red',add = TRUE)
  mtext(rownames(mydf)[i],line = 2,family = 'Lato Black')
  
}

mtext('World Energy Mix',side = 3,line = 4,cex = 3,family = 'Lato Black',adj = 0.025,outer = TRUE)
mtext('Shares of different energy types in total enery use',side = 3,line = 1.5,adj = 0.025,
      outer = TRUE,font = 3,cex = 1.5)
mtext('Souce: dantri.com.vn. Desiged by: Phan Tien Dung.',side = 1,cex = 1.5,font = 3,outer = TRUE,
      adj = 0.025)

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






