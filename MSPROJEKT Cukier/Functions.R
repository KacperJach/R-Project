#Zadanko1 es
zad1_sz <- function(dane)
{
  szereg_szcz = sort(dane)
  print(szereg_szcz)
 
  
  print("----Sr arytmetyczna-----")
  sr_arytm = mean(szereg_szcz)
  print(sr_arytm)
  
  print("----Sr harmoniczna-----")
  sr_harm = 1/mean(1/szereg_szcz)
  print(sr_harm)
  
  print("----Sr geometryczna-----")
  sr_geo = prod(szereg_szcz) ^ (1/(length(szereg_szcz)))
  print(sr_geo)
  
  print("----Moda-----")
  print(getmode(dane))
  
  print("----Kwartyl 1-----")
  kwartyl_1 = quantile(szereg_szcz, 0.25)
  print(kwartyl_1)
  
  print("----Kwartyl 3-----")
  kwartyl_3 = quantile(szereg_szcz, 0.75)
  print(kwartyl_3)
  
  print("----Mediana-----")
  mediana = median(szereg_szcz)
  print(mediana)
  
  print("----Wariancja nieobciazona-----")
  wariancja_nieobciazona = sum((szereg_szcz-sr_arytm) ^ 2) / sum(length(dane))
  print(wariancja_nieobciazona)
  
  print("----Wariancja obciazona-----")
  wariancja_obciazona = var(szereg_szcz)
  print(wariancja_obciazona)
  
  print("----Odchylenie standardowe nieobciazone-----")
  odchylenie_standard_nieobciazone = sqrt(wariancja_nieobciazona)
  print(odchylenie_standard_nieobciazone)
  
  
  print("----Odchylenie standardowe obciazone-----")
  odchylenie_standard_obciazone = sd(szereg_szcz)
  print(odchylenie_standard_obciazone)
  
  print("----Odchylenie przecietne-----")
  odchylenie_przecietne = sum(szereg_szcz - sr_arytm)/ sum(length(dane))
  print(odchylenie_przecietne)
  
  print("----Odchylenie przecietne od miediany-----")
  odchyl_przecietne_od_med = sum(abs(szereg_szcz - mediana)) / length(szereg_szcz)
  print(odchyl_przecietne_od_med)
  
  print("----Odchylenie cwiartkowe-----")
  odchyl_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchyl_cwiartkowe)#????????????????????????????????????????????
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmien = odchylenie_standard_obciazone / sr_arytm 
  print(wspolczynnik_zmien)
  
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  pozycyjny_wspolczynnik_zmien = (kwartyl_3 - kwartyl_1)/(kwartyl_3 + kwartyl_1)
  print(pozycyjny_wspolczynnik_zmien)
  
  print("----Skosnosc-----")
  skosnosc = (sum((szereg_szcz - sr_arytm) ^ 3) / length(szereg_szcz))/(odchylenie_standard_obciazone ^ 3)
  print(skosnosc) 
  
  print("----Kurtoza-----")
  kurtoza = (sum((szereg_szcz - sr_arytm) ^ 4) / length(szereg_szcz)) / (odchylenie_standard_obciazone ^ 4)
  print(kurtoza)
  
  print("----Eksces-----")
  eksces = kurtoza - 3
  print(eksces)
}

zad1_roz <-function(dane)
{
  szereg_roz = hist(dane, breaks = seq(min(dane), max(dane), length.out = 8))
  
  print("----sr arytmetyczna----")
  sr_arytmetyczna = sum(szereg_roz$counts * szereg_roz$mids) / sum(szereg_roz$counts)
  print(sr_arytmetyczna)
  
  print("----sr harmoniczna----")
  sr_harmoniczna = sum(szereg_roz$counts) / sum(szereg_roz$counts / szereg_roz$mids)
  print(sr_harmoniczna)
  
  print("----sr geometryczna----")
  sr_geometryczna = (prod(szereg_roz$mids ^ szereg_roz$counts)) ^ (1 / sum(szereg_roz$counts))
  print(sr_geometryczna)
  
  print("----Moda----")
  indeks_d = which.max(szereg_roz$counts)
  print(indeks_d)
  rozpietosc_d = szereg_roz$breaks[indeks_d+1] - szereg_roz$breaks[indeks_d]
  moda = szereg_roz$breaks[indeks_d] + ((szereg_roz$counts[indeks_d] - szereg_roz$counts[indeks_d-1])/((szereg_roz$counts[indeks_d] - szereg_roz$counts[indeks_d-1])+(szereg_roz$counts[indeks_d] - szereg_roz$counts[indeks_d+1])))*rozpietosc_d
  print(moda)
  
  print("----kwartyl_1----")
  q2 = sum(szereg_roz$counts)/2
  ceiling(q2)
  q1 = sum(szereg_roz$counts)/4
  ceiling(q1)
  q3 = (3*sum(szereg_roz$counts))/4
  ceiling(q3)
  kwartyl_1 = kwartyl(szereg_roz,1,length(dane), q1)
  
  print("----Kwartyl_3----")
  kwartyl_3 = kwartyl(szereg_roz,3,length(dane), q3)
  
  print("----Mediana----")
  mediana = kwartyl(szereg_roz,2,length(dane), q2)
  
  
  
  print("----Wariancja obciazona-----")
  wariancja = sum(((szereg_roz$mids-sr_arytmetyczna) ^ 2) * szereg_roz$counts) / sum(szereg_roz$counts)
  print(wariancja_obciazona)
  
  print("----Wariancja nieobciazona-----")
  wariancja = sum(((szereg_roz$mids-sr_arytmetyczna) ^ 2) * szereg_roz$counts) / sum(szereg_roz$counts)
  print(wariancja_nieobciazona)
  
  print("----Odchylenie standardowe obciazone-----")
  odchylenie_standard_obciazone = sqrt(wariancja_obciazona)
  print(odchylenie_standard_obciazone)
  
  print("----Odchylenie standardowe nieobciazone-----")
  odchylenie_standard_nieobciazone = sqrt(wariancja_nieobciazona)
  print(odchylenie_standard_nieobciazone) #??????????
  
   
  #???????????????????
  

  print("----Odchylenie przecietne-----")
  odchylenie_przecietne = (sum(szereg_roz$mids - sr_arytmetyczna)) / sum(szereg_roz$counts)
  print(odchylenie_przecietne)
  
  print("----Odchylenie przecietne od mediany-----")
  
  
  print("----Odchylenie cwiartkowe-----")
  odchylenie_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchylenie_cwiartkowe)
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmiennosci = (odchylenie_standard/sr_arytmetyczna)
  print(wspolczynnik_zmiennosci)
  
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  poz_wspolczynnik_zmiennosci = (odchylenie_cwiartkowe/mediana)
  print(poz_wspolczynnik_zmiennosci)
  
  print("----Skosnosc-----")
  skosnosc = (sum(((szereg_roz$mids - sr_arytmetyczna) ^ 3) * szereg_roz$counts) / sum(szereg_roz$counts))/(odchylenie_standard ^ 3) 
  print(skosnosc)
  
  print("----Kurtoza-----")
  kurtoza = (sum(((szereg_roz$mids - sr_arytmetyczna) ^ 4) * szereg_roz$counts) / sum(szereg_roz$counts))/(odchylenie_standard ^ 4)
  print(kurtoza)
  
  print("----Eksces-----")
  eksces = kurtoza - 3
  print(eksces)
  
  
}

indeks_przedzialu <-function(counts, m)
{
  for(i in 1:length(counts)){
    if (sum(counts[1:i]) >= m)
      return(i)
  }
}
kwartyl <- function(szereg_roz, nr_kwartylu, dlugosc_danych, q)
{
  indeks = indeks_przedzialu(szereg_roz$counts, q)
  rozpietosc = szereg_roz$breaks[2] - szereg_roz$breaks[1]
  kw = (szereg_roz$breaks[indeks] + (((nr_kwartylu*dlugosc_danych/4)-sum(szereg_roz$counts[1:indeks-1]))/szereg_roz$counts[indeks])*rozpietosc)
  print(kw)
}

getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
