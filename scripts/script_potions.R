n=67


m_formule=sample(c("latin","elfique","aucune"), n, replace=TRUE)
m_preparation=sample(c("bouillon","bouillon","maceration","distillation"), n, replace=TRUE)
i_bave_crapaud=rlnorm(n,2,1)
i_sang_hirondelle=rlnorm(n,1,1)+i_bave_crapaud
i_aile_papillon= rlnorm(n,order(i_bave_crapaud, decreasing=TRUE)/20,2)
i_ectoplasme= rlnorm(n,0.5,1)
i_graisse_troll=rlnorm(n,2,0.8)+0.1*i_bave_crapaud
i_pied_lutin=rlnorm(n,order(i_graisse_troll, decreasing=FALSE)/10,1.2)
i_givreboises=rlnorm(n,5,0.5)
i_pierre_lune=rlnorm(n,order(i_ectoplasme,decreasing=FALSE),0.7)+ i_ectoplasme
i_larmes_crocodile=rlnorm(n,order(i_graisse_troll,decreasing=FALSE)/30,1)
i_givreboises=rlnorm(n,order(i_pierre_lune,decreasing=FALSE)/10,0.5)

p_resistance=rnorm(n, 5, 4)
p_destruction=0.5*log(i_ectoplasme)+
  0.2*log(i_bave_crapaud)+
  -0.1*log(i_givreboises)
  rnorm(n,0,1)
p_invisibilite=rnorm(n,2, 4)+
  0.3*log(i_pierre_lune)
p_alteration=rnorm(n,5,2)+
  0.6*log(i_larmes_crocodile)+
  0.4*log(i_aile_papillon)+
  -0.3*log(i_graisse_troll)
p_conjuration=rnorm(n,5,5)+
  0.5*log(i_pied_lutin)
  -0.1*log(i_bave_crapaud)
  0.3*log(i_givreboises)





potions=data.frame(m_formule,
                   m_preparation,
                   i_bave_crapaud,
                   i_sang_hirondelle,
                   i_aile_papillon,
                   i_ectoplasme,
                   i_graisse_troll,
                   i_pied_lutin,
                   i_givreboises,
                   i_pierre_lune,
                   i_larmes_crocodile,
                   p_resistance,
                   p_destruction,
                   p_invisibilite,
                   p_conjuration,
                   p_alteration)

write.table(potions, "datasets/potions.csv",sep=";", row.names=FALSE)

library(FactoMineR)

mypca=PCA(select(potions,-m_formule,-m_preparation))
plot(mypca, choix="var")
mypca$eig

