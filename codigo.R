# get data
setwd("~/Documentos/IES/7mo semestre/Seminario Big Data") # this folder has your HTML files 
#html <- list.files(pattern="\\.(htm|html)$") # get just .htm and .html files

# load packages
library(tm)
library(RCurl) #sudo apt-get install libcurl4-openssl-dev
library(XML)   #sudo apt-get install libxml2-dev
# get some code from github to convert HTML to text
#writeChar(con="htmlToText.R", (getURL(ssl.verifypeer = FALSE, "https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/htmlToText/htmlToText.R")))
#source("htmlToText.R")
htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE, encoding = "UTF-8")
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
} #función para poner html como texto
# convert HTML to text
#html2txt <- htmlToText("prueba2.html")
library(stringr)
html <- paste(readLines("prueba2.html", encoding = "UTF-8"), collapse="\n") ##lee el archivo .html
matched <- str_match_all(html, "<a href=\"(.*?)\"")#extrae del .html los enlaces (con encabezados)
links <- matched[[1]][, 2]#quita los encabezados

links <- links[-3]#quita enlaces inservibles
links <- links[-1]#quita enlaces inservibles
#guarda contenido de los links en un vector
html2txt <- 1:length(links)
for(i in 1:length(links)){ 
  #guarda texto de la i-ésima noticia en el i-ésimo elemento del vector
  html2txt[i] <- htmlToText(links[i]) 
}
##
df <- as.data.frame(html2txt)#pone el vector como data frame
dfCorpus <- VCorpus(VectorSource(html2txt)) #pone el data frame como corpus
#dfCorpus <- Corpus(df)
#
library(tm)   
docs <- dfCorpus
## Preprocessing      
docs <- tm_map(docs, removePunctuation)   # Quitar puntuación    
docs <- tm_map(docs, removeNumbers)      # Quita números    
docs <- tm_map(docs, tolower)   # Pone en minúsculas    
docs <- tm_map(docs, removeWords, stopwords("spanish"))   # Quita palabras vacías
library(SnowballC)   
docs <- tm_map(docs, stemDocument, language = "spanish")   # quita finales comunes en las palabras
docs <- tm_map(docs, stripWhitespace)   # quita espacios en banco
docs <- tm_map(docs, PlainTextDocument, language = "spanish")   # pone como texto plano (?)
## *This is the end of the preprocessing stage.*   

dtm <- DocumentTermMatrix(docs)   #crea matriz documento término con el corpus modificado

#dtm <- removeSparseTerms(dtm, 1-(10/length(release_corpus)))
###########################################################################################
#####codigo de ############################################################################
#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html #
###########################################################################################
### Exploración descriptiva de los datos
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   #dimensión de la matriz en filas, columnas
write.csv(m, file="DocumentTermMatrix.csv") #Guarda la matriz como .csv
### FOCUS - on just the interesting stuff...   
#  Quitar términos escasos
dtms <- removeSparseTerms(dtm, .5) # Esto hace que la matriz tenga como máximo 73% de espacio vacío
### Frecuencia de las palabras   
head(table(freq), 20)   
#la primera fila indica la frecuencia y al de abajo la cantidad de palabras que tienen esa frecuencia
tail(table(freq), 20)   #considerando las 20 frecuencias más altas
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))   
freq   #para ver las palabras que quedan dentro de la matriz Doc-Term
findFreqTerms(dtm, lowfreq=5)   # Listado de palaras que tienen almenos una frecuencia de 5
#gráfico de frecuencias de palabras con freq > 5
library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>5), aes(word, freq))  
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   
#  
## Relación entre términos
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
# se especifica una correlación con límite de 0.98
findAssocs(dtm, c("calidad" , "universidades"), corlimit=0.98) 
### Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.5) # Se prepara la doc-term-matrix con 50% de espacio vacío como máximo
freq <- colSums(as.matrix(dtm)) # Se halla frecuencia de palabras
dark2 <- brewer.pal(6, "Dark2")   #colores
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    #se grafica

### Clustering por Similaridad de Términos

### Agrupamiento Jerárquico
dtms <- removeSparseTerms(dtm, 0.6) # doc-term-matrix con 60% de espacio vacío como máximo
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # Se calcula la distancia entre las palabras
fit <- hclust(d=d, method="ward.D")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" N° de clusters que se utilizarán
rect.hclust(fit, k=5, border="red") # Dibujo de dendrograma  encerrando 5 clusters   

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.5) # doc-term-matrix con 50% de espacio vacío como máximo
d <- dist(t(dtms), method="euclidian")   #distancia entre las palabras
kfit <- kmeans(d, 2)   #especificando dos clusters
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   #se especifican parámetros gráficos

## Hallar Universidad de Valparaíso
####frecuencia global
sum(sum(str_detect(html2txt, "Universidad de Valparaíso")),
    sum(str_detect(html2txt, "universidad de Valparaíso")),
    sum(str_detect(html2txt, "universidad de valparaíso")),
    sum(str_detect(html2txt, "Universidad de valparaíso")))
####frecuencia documento
s <- 1:length(html2txt)
for (i in 1:length(html2txt)) {
  s[i] <- sum(sum(str_detect(html2txt[i], "Universidad de Valparaíso")),
              sum(str_detect(html2txt[i], "universidad de Valparaíso")),
              sum(str_detect(html2txt[i], "universidad de valparaíso")),
              sum(str_detect(html2txt[i], "Universidad de valparaíso")))
}

