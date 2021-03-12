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
  
  print("----Mediana-----")
  mediana = median(szereg_szcz)
  print(mediana)
  
  print("----Odchylenie-----")
  odchylenie = sd(szereg_szcz)
  print(odchylenie)
  
  print("----Wariancja-----")
  wariancja = var(szereg_szcz)
  print(wariancja)
  
  print("----Odchylenie przecietne od miediany-----")
  odchyl_przecietne_od_med = sum(abs(szereg_szcz - mediana)) / length(szereg_szcz)
  print(odchyl_przecietne_od_med)
  
  print("----Odchylenie cwiartkowe-----")
  kwartyl_1 = quantile(szereg_szcz, 0.25)
  kwartyl_3 = quantile(szereg_szcz, 0.75)
  
  odchyl_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchyl_cwiartkowe)#????????????????????????????????????????????
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmien = odchylenie / sr_arytm * 100
  print(wspolczynnik_zmien)
  
  print("----Wspolczynnik asymetrii-----")
  wspolczynnik_asymetrii =  (sum((szereg_szcz - sr_arytm) ^ 3) / length(szereg_szcz)) / (odchylenie ^ 3)
  print(wspolczynnik_asymetrii)
  
  print("----Kurtoza-----")
  kurtoza = (sum((szereg_szcz - sr_arytm) ^ 4) / length(szereg_szcz)) / (odchylenie ^ 4)
  print(kurtoza)
  
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
  
  mediana = sum(szereg_roz$counts)/2
  print(mediana)
  med = szereg_roz$breaks[4] - ((2*length(dane)/4)-)
  print(length(dane))
 
  
  
}
