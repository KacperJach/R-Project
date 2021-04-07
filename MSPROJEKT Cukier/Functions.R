zad6_sz <- function(dane)
{
  szereg_szcz = sort(dane)
 
  print("----Srednia arytmetyczna-----")
  sr_arytm = mean(szereg_szcz)
  print(sr_arytm)
  
  print("----Srednia harmoniczna-----")
  sr_harm = length(szereg_szcz)/sum(1/szereg_szcz)
  print(sr_harm)
  
  print("----Srednia geometryczna-----")
  sr_geo = prod(szereg_szcz) ^ (1/(length(szereg_szcz)))
  print(sr_geo)
  
  print("----Moda-----")
  print(moda(dane))
  
  print("----Kwartyl 1-----")
  kwartyl_1 = quantile(szereg_szcz, 0.25)
  print(kwartyl_1)
  
  print("----Kwartyl 3-----")
  kwartyl_3 = quantile(szereg_szcz, 0.75)
  print(kwartyl_3)
  
  print("----Mediana-----")
  mediana = median(szereg_szcz)
  print(mediana)
  
  print("----Wariancja obciazona-----")
  wariancja_obciazona = sum((szereg_szcz-sr_arytm) ^ 2) / sum(length(dane))
  print(wariancja_obciazona)
  
  print("----Wariancja nieobciazona-----")
  wariancja_nieobciazona = var(szereg_szcz)
  print(wariancja_nieobciazona)
  
  print("----Odchylenie standardowe obciazone-----")
  odchylenie_standard_obciazone = sqrt(wariancja_obciazona)
  print(odchylenie_standard_obciazone)
  
  print("----Odchylenie standardowe nieobciazone-----")
  odchylenie_standard_nieobciazone = sd(szereg_szcz)
  print(odchylenie_standard_nieobciazone)
  
  print("----Odchylenie przecietne-----")
  odchylenie_przecietne = sum(abs(szereg_szcz - sr_arytm))/ length(szereg_szcz)
  print(odchylenie_przecietne)
  
  print("----Odchylenie przecietne od miediany-----")
  odchyl_przecietne_od_med = sum(abs(szereg_szcz - mediana)) / length(szereg_szcz)
  print(odchyl_przecietne_od_med)
  
  print("----Odchylenie cwiartkowe-----")
  odchyl_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchyl_cwiartkowe)
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmien = odchylenie_standard_nieobciazone / sr_arytm 
  print(wspolczynnik_zmien)
  
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  pozycyjny_wspolczynnik_zmien = (kwartyl_3 - kwartyl_1)/(kwartyl_3 + kwartyl_1)
  print(pozycyjny_wspolczynnik_zmien)
  
  print("----Skosnosc-----")
  skosnosc = (sum((szereg_szcz - sr_arytm) ^ 3) / length(szereg_szcz))/(odchylenie_standard_nieobciazone ^ 3)
  print(skosnosc) 
  
  print("----Kurtoza-----")
  kurtoza = (sum((szereg_szcz - sr_arytm) ^ 4) / length(szereg_szcz)) / (odchylenie_standard_nieobciazone ^ 4)
  print(kurtoza)
  
  print("----Eksces-----")
  eksces = kurtoza - 3
  print(eksces)
}

zad6_roz <-function(dane)
{
  szereg_roz = hist(dane, breaks = seq(min(dane), max(dane), length.out = round(sqrt(length(dane)),0)+1))
  
  print("----Srednia arytmetyczna----")
  sr_arytmetyczna = sum(szereg_roz$counts * szereg_roz$mids) / sum(szereg_roz$counts)
  print(sr_arytmetyczna)
  
  print("----Srednia harmoniczna----")
  sr_harmoniczna = sum(szereg_roz$counts) / sum(szereg_roz$counts / szereg_roz$mids)
  print(sr_harmoniczna)
  
  print("----Srednia geometryczna----")
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
  wariancja_obciazona = sum(((szereg_roz$mids-sr_arytmetyczna) ^ 2) * szereg_roz$counts)/ (sum(szereg_roz$counts))
  print(wariancja_obciazona)
  
  print("----Wariancja nieobciazona-----")
  wariancja_nieobciazona = sum((szereg_roz$mids-sr_arytmetyczna) ^ 2) / (sum(szereg_roz$counts)-1)
  print(wariancja_nieobciazona)
  
  print("----Odchylenie standardowe obciazone-----")
  odchylenie_standard_obciazone = sqrt(wariancja_obciazona)
  print(odchylenie_standard_obciazone)
  
  print("----Odchylenie standardowe nieobciazone-----")
  odchylenie_standard_nieobciazone = sqrt(wariancja_nieobciazona)
  print(odchylenie_standard_nieobciazone)
  
  print("----Odchylenie przecietne-----")
  odchylenie_przecietne = (sum(abs(szereg_roz$mids - sr_arytmetyczna))) / sum(szereg_roz$counts)
  print(odchylenie_przecietne)
  
  print("----Odchylenie przecietne od mediany-----")
  odchylenie_przecietne_od_mediany = sum(abs(szereg_roz$mids - mediana) * szereg_roz$counts) / sum(szereg_roz$counts)  
  print(odchylenie_przecietne_od_mediany)
  
  print("----Odchylenie cwiartkowe-----")
  odchylenie_cwiartkowe = (kwartyl_3 - kwartyl_1)/2
  print(odchylenie_cwiartkowe)
  
  print("----Wspolczynnik zmiennosci-----")
  wspolczynnik_zmiennosci = (odchylenie_standard_nieobciazone/sr_arytmetyczna)
  print(wspolczynnik_zmiennosci)
  
  print("----Pozycyjny wspolczynnik zmiennosci-----")
  poz_wspolczynnik_zmiennosci = (odchylenie_cwiartkowe/mediana)
  print(poz_wspolczynnik_zmiennosci)
  
  print("----Skosnosc-----")
  skosnosc = (sum(((szereg_roz$mids - sr_arytmetyczna) ^ 3)) / sum(szereg_roz$counts))/(odchylenie_standard_nieobciazone ^ 3) 
  print(skosnosc)
  
  print("----Kurtoza-----")
  kurtoza = (sum(((szereg_roz$mids - sr_arytmetyczna) ^ 4)) / sum(szereg_roz$counts))/(odchylenie_standard_nieobciazone ^ 4)
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

moda <- function(wektor) 
{
  uniqv <- unique(wektor)
  uniqv[which.max(tabulate(wektor))]
}

zad7 <-function(dane, wspolczynnik_ufnosci)
{
  wektor = sort(dane) 
  
  l = ks.test(wektor,"pnorm", mean(wektor), sd(wektor))
  print(l)
  
  if(l$p > 1-wspolczynnik_ufnosci) 
  { 
    print("Zawartosc cukru w procentach w dostawie burakow cukrowych ma rozklad normalny.") 
  } 
  else 
  { 
    print("Zawartosc cukru w procentach w dostawie burakow cukrowych nie ma rozkladu normalnego.") 
  }
}

zad8 <-function(wektor, istotnosc)
{
 
  cat("Poziom istotnosci testu: ", istotnosc, "\nHipoteza zerowa: przecietna == 18 \n")
  t = ((mean(wektor) - 18)*(sqrt(length(wektor) - 1)))/(sd(wektor))
  
  cat("Hipoteza alternatywna: przecietna != 18 \n")
  cat("Wartosc statystyki:",t)
  
  kwantylT = qt(1-istotnosc/2, df=length(wektor) - 1)  
  
  cat("\nPrzedzialy krytyczne: ( -oo, ",-(kwantylT),") u  (", kwantylT,",oo)\n")
  if( -kwantylT < t & t < kwantylT)                 
  {
    cat("Brak podstaw do odrzucenia hipotezy zerowej.")
  }
  else
  {
    cat("Odrzucamy hipoteze zerowa na rzecz hipotezy alternatywnej.")
  }
}

zad9 <- function(dane, odchylenie_standardowe, alfa)
{
  cat("Wartosc istotnosci testu: ", alfa, "\nWartosc odchylenia standardowego: ",odchylenie_standardowe,"\n")
  
  stat_chi_kwadrat = (var(dane)*(length(dane)-1))/(odchylenie_standardowe^2) 
  
  cat("Hipoteza alternatywna: odchylenie standardowe nie jest rowne ", odchylenie_standardowe,"\n")
  
  kwantyl_p = qchisq(1-alfa/2, length(dane)-1)
  kwantyl_l = qchisq(alfa/2, length(dane)-1)
  
  cat("Wartosc statystyki chi kwadrat:", stat_chi_kwadrat)
  cat("\nPrzedzialy krytyczne: ( -inf, ",-(kwantyl_l),") u  (",kwantyl_p,",+inf)\n")
  
  if(kwantyl_l < stat_chi_kwadrat && kwantyl_p > stat_chi_kwadrat)
  {
    cat("Na poziomie istotnosci ", alfa, "przyjmujemy hipoteze,ze odchylenie standardowe zawartosci cukru w dostawach burakow cukrowych w wojewodztwie wielkopolskim jest rowne 2% ")
  }
  else
  {
    cat("Na poziomie istotnosci ", alfa, "odrzucamy hipoteze,ze odchylenie standardowe zawartosci cukru w dostawach burakow cukrowych w wojewodztwie wielkopolskim jest rowne 2% ")
  }
  cat('\n')
}

test_fs <- function(wektor1,wektor2,alfa)
{
  cat("\n")
  cat("Test fishera-snedecora\n")
  cat("Hipoteza zerowa: wariancje sa sobie rowne\nHipoteza alternatywna: wariancja z wojewodztwa lubuskiego jest wieksza od wariancji z wojewodztwa wielkopolskiego\n")
  if(var(wektor1)>var(wektor2))
  {
    statystyka_f=var(wektor1)/var(wektor2)
    kwantyl_f=qf(1-alfa,length(wektor1)-1,length(wektor2)-1)
  }
  else 
  {
    statystyka_f=var(wektor2)/var(wektor1)
    kwantyl_f=qf(1-alfa,length(wektor2)-1,length(wektor1)-1)
  }

  cat("Wartosc statystyki fishera:",statystyka_f)
  cat("\nPrzedzial krytyczny: (", kwantyl_f,",oo)\n")
  if(statystyka_f<kwantyl_f)
  {
    cat("Nie ma podstaw do odrzucenia hipotezy zerowej, przyjmujemy ze wariancje sa w przyblizeniu rowne\n")
    return (1)
  }
  else
  {
    cat("Sa podstawy do odrzucenia hipotezy zerowej, przyjmujemy ze wariancje nie sa w przyblizeniu rowne\n")
    return (0)
  }
}

zad_10t_student <- function(wektor1,wektor2,alfa)
{
  wektor_szcz_1= sort(wektor1)
  wektor_szcz_2= sort(wektor2)
  kwantyl=qt(1-alfa,df=length(wektor_szcz_1)+length(wektor_szcz_2)-2)
  cat("Hipoteza zerowa: srednie sa rowne\nHipoteza alternatywna: Zawartosc cukru w wojewodztwie lubuskim jest mniejsza niz w wojewodztwie wielkopolskim\n")
  
  cat("Statystyka t studenta\n")
  test_t_studenta=t.test(wektor1,wektor2)
  cat(test_t_studenta)
 
  if(kwantyl<test_t_studenta$statistic)
  {
    cat("Na poziomie istotnosci ",alfa," mozna przyjac hipoteze zerowa \n")
  }
  else
  {
    cat("Na poziomie istotnosci ",alfa," mozna przyjac hipoteze alternatywna  \n")
  
  }
}
zad_10t_student_c_c <- function(wektor1,wektor2,alfa)
{
  cat("Statystyka Cochrana_Coxa\n")
  cat("Hipoteza zerowa: srednie sa rowne\nHipoteza alternatywna: Zawartość cukru w województwie lubuskim jest mniejsza niz w wojewodztwie wielkopolskim\n")
  
  statystyka = (abs(mean(wektor1)-mean(wektor2)))/(sqrt(var(wektor1)/(length(wektor1))+var(wektor2)/(length(wektor2))))
  kwantyl = ((var(wektor1)/length(wektor1))*qt(1-alfa,length(wektor1)-1)+(var(wektor2)/length(wektor2))*qt(1-alfa,length(wektor2)-1))/((var(wektor1)/length(wektor1))+(var(wektor2)/length(wektor2))) 
  
  cat("Wartosc statystyki Cochrana coxa:",statystyka)
  cat("\nWartosc graniczna przedzialu krytycznego(najwyzsza):",kwantyl, "\n")
  
  if(kwantyl < statystyka)
  {
    cat("Na poziomie istotnosci ",alfa," nie mozna przyjac hipotezy ze zawartosc cukru w wojewodztwie lubuskim jest mniejsza niz w wojewodztwie wielkopolskim \n")
  }
  else
  {
    cat("Na poziomie istotnosci ",alfa,"mozna przyjac hipoteze ze zawartosc cukru w wojewodztwie lubuskim jest mniejsza niz w wojewodztwie wielkopolskim\n")
  }
}

zad10 <- function(wektor1,wektor2,alfa)
{
  if(test_fs(wektor1,wektor2,alfa)==1) 
  {
    zad_10t_student(wektor1,wektor2,alfa)
  }
  else
  {
    zad_10t_student_c_c(wektor1,wektor2,alfa)
  }
}