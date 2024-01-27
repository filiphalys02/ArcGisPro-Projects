install.packages("corrplot")
install.packages("factoextra")
install.packages("readxl")
install.packages("fossil")
install.packages("psych")

### 1. Nadanie odniesienia
library(raster)

# Ponieważ utraciliśmy geoodniesienie w Fiji w obrazie etykietami obiektów
# chcemy spowrotem je nadać. Posaiamy DEM wyeksorotwany z ARCGISa, który jest identycznych rozmiarów
# i ma dane odiensienie geograficzne. Wczytujemy go.

fin_nmt = raster("TPI_10.tif") # wczytujemy obraz DEM (lub TPI) dla swojego obszaru
crs(fin_nmt) # sprawdzamy geodniesienie, jeżeli NA -> wykonaj poniżej linijke
# proj4string(fin_nmt) <- CRS("+init=epsg:3047")
fin_lbl = raster("TPI_10_fiji-killBorders-lbl.tif") # wczytujemy obraz z etykietami obiektów
fin_lbl@extent <- fin_nmt@extent # powielamy zasięg
proj4string(fin_lbl) <- CRS("+init=epsg:3047") # nadajemy odniesienie

# zapisujemy raster z nadanym goeodinesieniem. 
writeRaster(fin_lbl, filename = "fin_lbl_16bit2.tif", datatype='INT2U')

### 2. Jest do wykonania w ArcGISie

### 3. Przetwarzanie danych
library("readxl")
# gdyby były błędy, proszę otworzyć plik xls i go jeszcze raz zapisać
morp <- tabela1 #read_excel() #morph.xls 
inte <- tabela2 #read_excel() #inte.xls
dane <- merge(x = morp, y = inte, by = "Label")

### wczytujemy plik shp z punktu 2. 
formy <- shapefile("formy.shp") 
# nadpisujemy tabele atrybutów danymi z analizy obrazu
formy@data <- dane

### 4. Klasyfikacja wg. wzoru z poprzedniego projektu. 

cotozaforma <- function(r1,r2) {
  E = r1/r2
  L = r1
  Klasyfikacja <- data.frame()
  Klasa <- "" 
  for (i in 1:length(L)) {
    if (E[i] >((2*L[i]/1000)+2)) {
      if (L[i]<100) {
        Klasa <- "1.Flute"
      } else if (L[i]>=100 & L[i]<1000) {
        Klasa <- "2.Mega Flute"
      } else { 
        Klasa <- "3.Mega Scale Glacial Lineation"
      }
    } else {
      if (L[i]<800) {
        Klasa <- "4.Drumlin"
      } else if (L[i]>=800 & L[i]<1050) {
        Klasa <- "5.Mega Drumlin"
      } else {
        Klasa <- "6.Streamlined Hill"
      }
    }
    Klasyfikacja[i,1] <- Klasa
  }
  return(Klasyfikacja)
}

Klasa <- cotozaforma(r1 = dane$Ellipse.Radius1, r2 = dane$Ellipse.Radius2)
formy@data$Klasa <- Klasa$V1

### 5. Klasyfikacja bez wzoru

# Oczyśćmy dane z mniej przydatnych. 
# colnames(dane)
# str(dane)
# usuwam nie num zmienne
dane1 <- dane[,-c(1,42:51)] # tutaj kolejność może być inna - indywidualna !!!
# sprawdźmy korelacje i wybierzmy swoje zmienne na bazie których dokonamy klasyfikacji bez wzroca.
# zastanówmy się na typem zmiennej co one reprezentują. Jeżeli chcemy pójśc w kierunku wielkość, kształtu, własność
# powinniśmy skomponować addekwatny zestaw danych. Wyjaśnienie czym są policzone parametry, jest w skrypcie. 
# niektóre parametry są bardzo podobne i się dublują. 

library(corrplot)
M <- cor(dane1)
corrplot(M)

# W przykładzie zostawie
dane1 <- dane1[,c(1,3,4,16,30)]
M <- cor(dane1)
corrplot(M)

# optymalna liczba grup do wydzielenia.
library(factoextra)
fviz_nbclust(scale(dane1), kmeans, method='silhouette')

### Grupowanie
# wybrana liczba klastrów
x = 5 ### podać
  klaster <- kmeans(scale(dane1), x) ### !!!!!
library(psych)
describeBy(dane1,group = klaster$cluster)

# przypiszmy wyniki klasyfikacji do zbioru danych
formy@data$Klasa2 <- klaster$cluster

describeBy(formy@data,group = formy@data$Klasa2)

library(fossil)
# porównanie podobieństwa klasyfikacji - ze wzoru i bez
rand.index(as.numeric(as.factor(formy@data$Klasa)), klaster$cluster)

### 6. Eksport danych
shapefile(formy, 'Morphoforms.shp')