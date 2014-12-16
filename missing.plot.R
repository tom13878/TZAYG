## Function returns variable if data is missing on plot level
## created by: Michiel

missing.plot<-function(i){
  if (any(is.na(i))) {
    pm<-"YES"}else{
      pm<-"NO"
    }
  return(pm)
}