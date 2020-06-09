
#Débit-------------
Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-01")&DATE<=as.Date("2020-01-02"))


k=0
for(i in 1:(nrow(Fevrier_DATA_D)-1)){
  if(Fevrier_DATA_D$Débit_CV004[i]<=1000){
    
    if(Fevrier_DATA_D$Débit_CV004[i+1]<=1000){
      k=k+1
      
    }else{
      if( k <15){
      Fevrier_DATA_D$Débit_CV004[(i-k):i]=lag(Fevrier_DATA_D$Débit_CV004,n = k+2)[i]
      k=0
      }
    }
  }else{
    k=0
  }
}



liste_date=Fevrier_DATA %>% select(DATE) %>% distinct(as.Date(DATE))
liste_date <- liste_date[[1]]

for(j in 1:54){

  print(liste_date[j])
  Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=liste_date[j]&DATE<=liste_date[j+1])
  k=0
  for(i in 1:(nrow(Fevrier_DATA_D)-1)){
    if(Fevrier_DATA_D$Débit_CV004[i]<=1000){
      
      if(Fevrier_DATA_D$Débit_CV004[i+1]<=1000){
        k=k+1
        
      }else{
        if( k <15){
          Fevrier_DATA_D$Débit_CV004[(i-k):i]=lag(Fevrier_DATA_D$Débit_CV004,n = k+2)[i]
          k=0
        }
      }
    }else{
      k=0
    }
  }
  Fevrier_DATA_D %>% 
    ggplot(aes(x=DATE,y=Dilution_HP19)) +
    geom_line(color=col_vector[3])+
    geom_point(color=col_vector[2], size=2)+
    ylim(0,1600)

  ggsave(paste(liste_date[j],".png",sep = ""),path = "image/Dilution_HP19",width =11,height = 6)

}





#Dillution 19------------------

Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-01")&DATE<=as.Date("2020-01-02"))
#Buttom
for(i in 1:(nrow(Fevrier_DATA_D)-3)){
  
  if((Fevrier_DATA_D$Dilution_HP19[i]-lead(Fevrier_DATA_D$Dilution_HP19)[i])>40){
    
    if(abs(Fevrier_DATA_D$Dilution_HP19[i+1]-Fevrier_DATA_D$Dilution_HP19[i+2])>40){
    Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
    }else if(abs(Fevrier_DATA_D$Dilution_HP19[i+5]-Fevrier_DATA_D$Dilution_HP19[i+1])>40){
      Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
    }
  }
}
#top
for(i in 1:(nrow(Fevrier_DATA_D)-3)){
  
  if((Fevrier_DATA_D$Dilution_HP19[i]-lead(Fevrier_DATA_D$Dilution_HP19)[i])< (-40)){
    
    if(abs(Fevrier_DATA_D$Dilution_HP19[i+1]-Fevrier_DATA_D$Dilution_HP19[i+2])>40){
      Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
    }else if(abs(Fevrier_DATA_D$Dilution_HP19[i+5]-Fevrier_DATA_D$Dilution_HP19[i+1])>40){
      Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
    }
  }
}
# save(Fevrier_DATA_D,file="Fevrier_DATA_D.Rda")









Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-01")&DATE<=as.Date("2020-01-02"))
Fevrier_DATA_D %>% 
  ggplot(aes(x=DATE,y=Dilution_HP19)) +
  geom_line(color=col_vector[3])+
  geom_point(color=col_vector[2], size=2)+
  ylim(0,1600)
  

  for(j in 1:54){
    
    print(liste_date[j])
    Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=liste_date[j]&DATE<=liste_date[j+1])
    k=0
    for(i in 1:(nrow(Fevrier_DATA_D)-1)){
      if(Fevrier_DATA_D$Débit_CV004[i]<=1000){
        
        if(Fevrier_DATA_D$Débit_CV004[i+1]<=1000){
          k=k+1
          
        }else{
          if( k <15){
            Fevrier_DATA_D$Débit_CV004[(i-k):i]=lag(Fevrier_DATA_D$Débit_CV004,n = k+2)[i]
            k=0
          }
        }
      }else{
        k=0
      }
    }
    Fevrier_DATA_D %>% 
      ggplot(aes(x=DATE,y=Débit_CV004)) +
      geom_line(color=col_vector[3])+
      geom_point(color=col_vector[2], size=2)+
      ylim(0,1600)
    
    ggsave(paste(liste_date[j],".png",sep = ""),path = "image/Débit_CV004",width =11,height = 6)
    
  }
  
  
  

#Débit
Fevrier_DATA_D2 =Fevrier_DATA_D


Fevrier_DATA_D =Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-01")&DATE<=as.Date("2020-01-02"))
#Buttom
for(i in 1:(nrow(Fevrier_DATA_D)-3)){

if((Fevrier_DATA_D$Débit_CV004[i]-lead(Fevrier_DATA_D$Débit_CV004)[i])>160){
  
  if(abs(Fevrier_DATA_D$Débit_CV004[i+1]-Fevrier_DATA_D$Débit_CV004[i+2])>160){
    Fevrier_DATA_D$Débit_CV004[i+1]=Fevrier_DATA_D$Débit_CV004[i]
  }else if(abs(Fevrier_DATA_D$Débit_CV004[i+5]-Fevrier_DATA_D$Débit_CV004[i+1])>40){
    Fevrier_DATA_D$Débit_CV004[i+1]=Fevrier_DATA_D$Débit_CV004[i]
  }
}
}
Fevrier_DATA_D %>% 
  ggplot(aes(x=DATE,y=Débit_CV004)) +
  geom_line(color=col_vector[3])+
  geom_point(color=col_vector[2], size=2)+
  ylim(0,1600)

#top
for(i in 1:(nrow(Fevrier_DATA_D)-3)){

if((Fevrier_DATA_D$Dilution_HP19[i]-lead(Fevrier_DATA_D$Dilution_HP19)[i])< (-40)){
  
  if(abs(Fevrier_DATA_D$Dilution_HP19[i+1]-Fevrier_DATA_D$Dilution_HP19[i+2])>40){
    Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
  }else if(abs(Fevrier_DATA_D$Dilution_HP19[i+5]-Fevrier_DATA_D$Dilution_HP19[i+1])>40){
    Fevrier_DATA_D$Dilution_HP19[i+1]=Fevrier_DATA_D$Dilution_HP19[i]
  }
}
}

Fevrier_DATA_D %>% 
ggplot(aes(x=DATE,y=Débit_CV004)) +
geom_line(color=col_vector[3])+
geom_point(color=col_vector[2], size=2)+
ylim(0,1600)


library(ggiraph)

library(plotly)


ggplotly(p)






