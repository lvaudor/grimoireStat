set.seed(2018)
N=864
age=rlnorm(N,2.5,2)
espece=sample(c("chene","chene","chene","chene",
                "chataignier","chataignier","chataignier",
                "hetre","hetre",
                "sapin"),N,replace=TRUE)
hauteur=round(20*log(age)+rlnorm(N,5,0.5),1)

broceliande=bind_cols(age=age,
                      espece=espece,
                      hauteur=hauteur)

fgui=function(data){
  mu_gui=case_when(data$espece=="chene"~0.1,
                   data$espece=="chataignier"~0.4,
                   data$espece=="hetre"~0.01,
                   data$espece=="sapin"~1)
  abond_gui=rnbinom(nrow(data),mu_gui,0.5)
  abond_gui=abond_gui+round(0.001*(data$age+rnorm(nrow(data),0,10)))
  data=bind_cols(data,gui=abond_gui)
  return(data)
}

flargeur=function(data){
  coeff_largeur=case_when(data$espece=="chene"~1.2,
                          data$espece=="chataignier"~1,
                          data$espece=="hetre"~1,
                          data$espece=="sapin"~0.8)
  largeur=coeff_largeur*data$hauteur+rnorm(nrow(data),0,data$hauteur/5)
  data=bind_cols(data,largeur=largeur)
  return(data)
}

fenchantement=function(data){
  data=mutate(data,
              enchantement=case_when(espece=="chene" & age>200~0.7,
                                     espece=="chene" & age>100 & age<=200~0.5,
                                     espece=="chene" & age<100~0.25,
                                     espece!="chene"~0.1)) %>%
    mutate(enchantement=enchantement>runif(nrow(data),0,1))
  return(data)
}

fperlimpinpin=function(data){
  data=mutate(data,
              perlimpinpin=case_when(enchantement ~122,
                                     !enchantement ~106)) %>%
    mutate(perlimpinpin=rnorm(nrow(data),perlimpinpin,20)) %>%
    mutate(perlimpinpin=perlimpinpin+0.05*hauteur)
}

ffees=function(data){
  data=mutate(data,
              fees=case_when(enchantement~1,
                             !enchantement~0.2)) %>%
    mutate(fees=rnbinom(nrow(data),mu=fees,size=0.1))
  return(data)
}

flutins=function(data){
  data=mutate(data,
              lutins=age/100*gui) %>%
    mutate(lutins=rnbinom(nrow(data),mu=lutins,size=0.1))
  return(data)
}
library(magrittr)
broceliande=broceliande %>%
  mutate(groupe=espece) %>%
  group_by(groupe) %>%
  nest() %$%
  data %>%
  map(fgui) %>%
  map(flargeur) %>%
  map(fenchantement) %>%
  map(ffees) %>%
  map(flutins) %>%
  map(fperlimpinpin) %>%
  bind_rows()


ggplot(broceliande,aes(x=hauteur,y=largeur,col=espece))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(broceliande,aes(x=age,y=gui,col=espece))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(broceliande,aes(x=enchantement,y=age,fill=espece))+
  geom_boxplot()

ggplot(filter(broceliande,espece=="chene"),aes(x=enchantement,y=age,fill=espece))+
  geom_boxplot()

table(broceliande$fees)

write.table(broceliande,"datasets/broceliande.csv",sep=";", row.names=FALSE)
