
  statystyki_ceny <- function(marka = NA, model = NA, lata = NA){
    tmp = auta2012
    if(!is.na(lata)) 
      tmp <- filter(tmp, Rok.produkcji == lata)
    
    if(!is.na(model)) 
      tmp <- filter(tmp, Model == model)
    
    if(!is.na(marka)) 
      tmp <- filter(tmp, Marka == marka)
#    if(tmp == auta2012){
#      cat("Podaj model i/lub marke")
#      break
#    }  
    return(mean(tmp$Cena.w.PLN))
  }
  
  
  porownanie_ceny <- function(marka = NA, model = NA){
    sr2011 <- statystyki_ceny(marka, model, 2011)
    sr2012 <- statystyki_ceny(marka, model, 2012)
    return(sr2011/sr2012)
  }
    
  
    
    
    
    