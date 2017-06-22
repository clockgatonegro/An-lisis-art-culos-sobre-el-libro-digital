#Reconocer texto en español
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

#Partir el character vector en palabras
sIn <- "Los voceros del desastre llevan años señalando con el dedo el abismo hacia el que caminan sin remedio muchos de los placeres y muchas de las antiguas formas de la civilización anterior a internet y a los teléfonos inteligentes y las tabletas y demás. "
strsplit(sIn, split=" ")

#Importar el texto a R
postdigital.v <- scan("/Users/clockwork/Downloads/librodigital.txt", what="character", sep="\n")
postdigital.v

#juntar todos los parrafos en uno solo
articulos.v <- paste(postdigital.v, collapse=" ")
length(articulos.v)
#Convertir todos los articulos en minuscula
articulos.minus.v <- tolower(articulos.v)
articulos.minus.v[1]
#Separar todo en una lista de palabras
articulos.palabras.l <- strsplit(articulos.minus.v, "\\W")
articulos.palabras.l
#Denota la estructura de la lista
str(articulos.palabras.l)

# Convierte la lista a un character String
articulos.palabras.v <- unlist(articulos.palabras.l) 
class(articulos.palabras.v)

#Cuales son las posiciones que no están en blanco
no.enBlanco.v <- which(articulos.palabras.v!="")
no.enBlanco.v 

#quita los espacios en blanco y sobreescribe en articulos.palabras.v
articulos.palabras.v <- articulos.palabras.v[no.enBlanco.v]
articulos.palabras.v [4897:5678]
#cuales ubicaciones tienen la palabra digital
which(articulos.palabras.v=="digital")

#el conteo de la frecuencia de veces que aparece la palabra digital
digital.ocurrencias.v<-length(articulos.palabras.v[which(articulos.palabras.v=="digital")])

#el numero total de palabras 
palabras.totales.v<-length(articulos.palabras.v)

#cuantas veces aparece la palabra digital %
(digital.ocurrencias.v/palabras.totales.v)*100

#Número de palabras con una sola ocurrencia
length(unique(articulos.palabras.v))

#Organizar la tabla por orden de ocurrencia
articulos.frec.t<-sort(table(articulos.palabras.v), decreasing = TRUE)

#Cuantas veces aparece la palabra digital + digitales vs cuantas veces impreso + papel
articulos.palabras.digital<-articulos.frec.t["digital"]+articulos.frec.t["digitales"]
articulos.palabras.impreso<-articulos.frec.t["impresos"]+articulos.frec.t["papel"]+articulos.frec.t["impreso"]+articulos.frec.t["impresa"]+articulos.frec.t["impresas"]

#Cuantas veces está digital mas que impreso 
articulos.palabras.digital/articulos.palabras.impreso

#gráfico 
plot(articulos.frec.t[19:60], type="b",
     xlab="de la 19 a la 20", ylab="Percentage of Full Text", xaxt ="n")
axis(1,1:42, labels=names(articulos.frec.t[19:60]))

