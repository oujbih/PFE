
library(ggplot2)
library(dplyr)
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#fines
SDP_HG %>% 
  ggplot(aes(X40µm))+
  geom_density(fill=sample(col_vector, 1))+
  geom_vline(xintercept = 20,color="red") +
  geom_vline(xintercept = 26,color="red") +
  xlab("")+ylab("fines")

#gros
SDP_HG %>% 
  ggplot(aes(X250µm))+
  geom_density(fill=sample(col_vector, 1))+
  geom_vline(xintercept = 10,color="red") +
  xlab("")+ylab("Gros")


My_data_Model_classification = SDP_HG %>% 
  mutate(fines=ifelse(X40µm<20,0,ifelse(X40µm<=26,1,0)),
         gros=ifelse(X250µm<10,1,0))

#fines
1-sum(My_data_Model_classification$fines)/nrow(SDP_HG)
#gros
1-sum(My_data_Model_classification$gros)/nrow(SDP_HG)
