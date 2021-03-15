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
  print(getmode(dane))#???????????????????????????????????????????????
  
  print("----Kwartyl 1-----")
  kwartyl_1 = quantile(szereg_szcz, 0.25)
  print(kwartyl_1)
  
  print("----Kwartyl 3-----")
  kwartyl_3 = quantile(szereg_szcz, 0.75)
  print(kwartyl_3)
  
  print("----Mediana-----")
  mediana = median(szereg_szcz)
  print(mediana)
  
  print("----Odchylenie standardowe-----")
  odchylenie = sd(szereg_szcz)
  print(odchylenie)
  
  print("----Wariancja-----")
  wariancja = var(szereg_szcz)
  print(wariancja)
  
  #print("----Odchylenie standardowe z gwiazdka-----")
  #odchylenie = sd(szereg_szcz)
  #print(odchylenie)
  
  #print("----Wariancja z gwiazdka-----")
  #wariancja = var(szereg_szcz)
  #print(wariancja)
  
  print("----Odchylenie przecietne-----")
  #########################################
  
  print("----Odchylenie przecietne od miediany-----")
  odchyl_przecietne_od_med = sum(abs(szereg_szcz - mediana)) / length(szereg_szcz)
  print(odchyl_przecietne_od_med)
  
  print("----Odchylenie cwiartkowe-----")
  odchyl_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchyl_cwiartkowe)#????????????????????????????????????????????
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmien = odchylenie / sr_arytm * 100
  print(wspolczynnik_zmien)
  
  #print("----Wspolczynnik asymetrii-----")
  #wspolczynnik_asymetrii =  (sum((szereg_szcz - sr_arytm) ^ 3) / length(szereg_szcz)) / (odchylenie ^ 3)
  #print(wspolczynnik_asymetrii)
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  #?????????????????
  
  print("----Wspolczynnik skosnosci-----")
  wspolczynnik_skosnosci = 3*(sr_arytm - mediana) / odchylenie
  print(wspolczynnik_skosnosci)
  
  print("----Kurtoza-----")
  kurtoza = (sum((szereg_szcz - sr_arytm) ^ 4) / length(szereg_szcz)) / (odchylenie ^ 4)
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
  
  print("----Mediana----")
  mediana = kwartyl(szereg_roz,2,length(dane), q2)
  
  print("----Kwartyl_3----")
  kwartyl_3 = kwartyl(szereg_roz,3,length(dane), q3)
  
  print("----Odchylenie cwiartkowe-----")
  odchylenie_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchylenie_cwiartkowe)
  
  print("----Wariancja-----")
  wariancja = sum(((szereg_roz$mids-sr_arytmetyczna) ^ 2) * szereg_roz$counts) / sum(szereg_roz$counts)
  print(wariancja)
  
  print("----Odchylenie standardowe-----")
  odchylenie_standard = sqrt(wariancja)
  print(odchylenie_standard)
  
  print("----Odchylenie przecietne-----")
  odchylenie_przecietne = (sum(szereg_roz$mids - sr_arytmetyczna)) / sum(szereg_roz$counts)
  print(odchylenie_przecietne)
  
  print("----Odchylenie przecietne od mediany-----")
  
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  poz_wspolczynnik_zmiennosci = (odchylenie_standard/sr_arytmetyczna) *100
  print(poz_wspolczynnik_zmiennosci)
  
  

  
 
 
  
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
