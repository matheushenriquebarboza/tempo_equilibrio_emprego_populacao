library(tidyverse)
library(sf)
library(ggmap)
library(ggspatial)
library(ggrepel)

#1 importa dados####
OD_tempos <- read.csv("Insumos/OD_tempos.csv")
OD_tempos <- OD_tempos[,-1]
OD_tempos <- as.matrix(OD_tempos) #converte para formato de matriz
row.names(OD_tempos) <- colnames(OD_tempos)[1:nrow(OD_tempos)] #nomeia linhas a partir de nome das colunas

dados_bairros <- read_csv("Insumos/dados_bairros.csv")

#2 calcula indicador tempo de equilíbrio####

#2.1 registra para cada origem os destinos com menor tempo de acesso####
#inicia variaveis com tamanho certo, mas valores que nao serao utilizados
OD_tempos_ord <- OD_tempos

for (i in c(1:nrow(OD_tempos))){
  OD_tempos_ord[i,] <- rank(OD_tempos[i,], ties.method = "random")
}

#2.2 soma os empregos dos destinos, seguindo a ordem dos mais proximos, ate atingir a pia da origem, registrando tempo e posicao na lista para que isso ocorra####
#inicia variaveis com tamanho certo, mas valores que nao serao utilizados
dados_bairros$tempos <- 0
dados_bairros$pos <- 0

for (i in c( 1:nrow(OD_tempos)) ){                                      #controla origem
  for (pos in c( 1:ncol(OD_tempos)) ){                                  #controla posicao na ordem dos mais proximos
    if ( sum( (OD_tempos_ord[i,] <= pos)*dados_bairros$empregos ) >= dados_bairros$pia[i]){           
      dados_bairros$tempos[i] <- max( (OD_tempos_ord[i,] == pos) * OD_tempos[i,] , na.rm = TRUE)  #max eh usado para resolver casos com empates nos tempos
      dados_bairros$pos[i] <- pos
      break
    }
  }
}

#3 calcula indicador oportunidades acumuladas para 1h####
empregos <- dados_bairros$empregos
x <- ifelse(is.na(OD_tempos < 60*60), 0, OD_tempos < 60*60)
y <- (x %*% empregos) / sum(empregos) # %*% multiplica matrizes
dados_bairros$oport_acum <- 0
dados_bairros$oport_acum[1:160] <- y
rm(empregos); rm(x); rm(y)

#4 analises####
#4.1 importa e trata dados geográficos####
#calcula tempos a partir de exemplos####
dados_bairros$temp_exemplo <- OD_tempos["Madureira",]
dados_bairros$temp_inhauma <- OD_tempos["Inhaúma",]

#le arquivos geograficos
bairros_geo <- read_sf("Insumos/Limite_Bairro_sem_novos.shp")
rmrj_geo <- read_sf("Insumos/RMRJ_bairros.shp")
metro <- read_sf("Insumos/Trajetos_metro.shp")
trem <- read_sf("Insumos/Trajetos_trem.shp")
brt <- read_sf("Insumos/Trajetos_brt.shp")

trem_capital <- st_crop(trem , xmin=-43.8 , xmax=-43.09 , ymin=-23.08 , ymax=-22.74) #corte do layer de trens pra limitar mapas à capital

#transforma bases para projeção adequada
bairros_geo <- st_transform(bairros_geo, 4326)
rmrj_geo <- st_transform(rmrj_geo, 4326)

#join nos dados calculados para bairros
colnames(bairros_geo)[2] <- "objectID"
bairros <- bairros_geo %>% 
  left_join(dados_bairros,by="objectID")

#join nos dados calculados para rmrj
colnames(rmrj_geo)[2] <- "objectID"
rmrj_geo <- rmrj_geo[ order(rmrj_geo$NM_MUNICIP) , ]
dados_bairros$objectID[161:181] <- rmrj_geo$objectID[1:21]
rmrj <- rmrj_geo %>%
  left_join(dados_bairros,by="objectID")

#cálculo de coordenadas dos centroides
bairros$centroids <- st_transform(bairros, 29101) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry()

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

centroides <- bairros %>% 
  dplyr::select(objectID) %>%
  st_transform(4326) %>%
  st_centroid()

centroides <- centroides %>% sfc_as_cols(names = c("long", "lat"))
centroides <- centroides %>% st_set_geometry(NULL) %>% as.data.frame()
bairros <- bairros %>% left_join(centroides, by = "objectID")
remove(centroides)

rmrj$area <- as.numeric( st_area(rmrj)/1000000)

#4.1 mapa de empregos####
ggplot() +
  geom_sf(data=rmrj, aes(fill = pmin(empregos/area,5000) ) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="", fill="empregos/km2") +
  scale_fill_gradient(low="white",high="red") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=metro, color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem, color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt, color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'empregos.png', width = 730, height = 397)

#4.2 mapa de PIA####
bairros$area <-  as.numeric( st_area(bairros)/1000000)

#densidade
ggplot() +
  geom_sf(data=bairros, aes(fill = pia/area ) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="", fill="PIA/km2") +
  scale_fill_gradient(low="white",high="red") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=metro, color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem_capital, color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt, color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'pia_densidade.png', width = 730, height = 397)

#absoluto
ggplot() +
  geom_sf(data=bairros, aes(fill = pia/1000 ) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="", fill="PIA (milhares)") +
  scale_fill_gradient(low="white",high="red") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=metro, color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem_capital, color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt, color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'pia.png', width = 730, height = 397)

#4.3 grafico população acumulada x tempo de equilibrio####
dados_bairros$rank_tempos <- rank(dados_bairros$tempos, ties.method = "random")
for(i in c(1:nrow(dados_bairros))){
  dados_bairros$pop_acum_p_tempo[i] <- sum(dados_bairros$populacao * (dados_bairros$rank_tempos <= dados_bairros$rank_tempos[i]))
}

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7") # paleta preta

ggplot(dados_bairros[dados_bairros$objectID<1000,], mapping = aes( y = pop_acum_p_tempo/sum(populacao), x=tempos/60 ) )  + 
  geom_jitter(mapping = aes( color =factor( area_plan)), height = 0.06) +
  scale_colour_manual(values=cbPalette)+
  labs(x="Tempo de Equilíbrio (min)", y="População Acumulada (%)", size="População (milhares)", color='Área de Planejamento') +
  guides(alpha=FALSE) +             #remove a legenda de transparencia
  theme_classic() +                  #tema ja pronto para simplificar o grafico
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="gray" ) 
  )

#salva grafico em arquivo
dev.print(png, 'grafico_tempodeequilibrio.png', width = 730, height = 397)

#4.4 mapa tempo de equilibrio####
#destaca bairros com t=0
bairros_zerados <- bairros %>% filter(tempos==0)

#gera mapa
ggplot() +
  geom_sf( data=bairros, aes(fill = tempos/60) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="",fill="tempos (min)") +
  scale_fill_gradient(low="blue",high="white") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=bairros_zerados,  aes(fill = tempos/60) , color = "red", size=1) + #destaca no mapa os bairros com t=0
  geom_sf(data=metro,  color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem_capital,  color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt,  color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'mapa_tempodeequilibrio.png', width = 730, height = 397)

#4.5 mapa oportunidades acumuladas####
#gera mapa
ggplot() +
  geom_sf(data=bairros, aes(fill = oport_acum) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="",fill="% de empregos") +
  scale_fill_gradient(low="white",high="blue") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=metro,  color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem_capital,  color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt,  color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'oport_acumu.png', width = 730, height = 397)

#4.6 grafico ordenamento tempo de equilibrio x ordenamento oportunidades acumuladas####
bairros$vx=-rank(bairros$tempos, ties.method = "min")
bairros$vy=rank(bairros$oport_acum, ties.method = "random")

#grafico
limite_max <- 232; limite_min <- 90
ggplot(bairros)+
  geom_point(aes(x=vx, y=vy, size=populacao/1000, alpha=I(0.5)))+
  geom_text_repel(aes(x=vx, y=vy, label = ifelse( (vy > vx+limite_max) | (vy < vx+limite_min), bairr_mun,"") ))+
  geom_abline(slope = 1, intercept = 161)+
  geom_abline(slope = 1, intercept = limite_min)+
  geom_abline(slope = 1, intercept = limite_max)+
  labs(x="Ordenamento Tempo Equilíbrio", y="Ordenamento % Oportunidades Acumuladas",
       size="População (milhares)") +
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
#salva gráfico em arquivo
dev.print(png, 'discrepancias_grafico.png', width = 540, height = 300)

#4.7 mapa com bairros mais distantes da igualdade dos indicadores####
#destaca bairros muito distantes da bissetriz
bairros_discrepantes <- bairros %>%
  filter((vy > vx+limite_max) | (vy < vx+limite_min))

#gera mapa
ggplot() +
  geom_sf(data=bairros, aes(fill = tempos/60) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="",fill="tempos (h)") +
  scale_fill_gradient(low="blue", high="white") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(8,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=bairros_discrepantes,  aes(fill = tempos/60) , color = "red", size=1) + #destaca no mapa os bairros com t=0
  geom_sf(data=metro,  color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem_capital,  color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt,  color = "black", size=0.5, show.legend = "line")+
  geom_text_repel(data=bairros, aes(x = long, y = lat, label = ifelse((vy > vx+232) | (vy < vx+90), bairr_mun, '')),  #labels com linha
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5))

#salva mapa em arquivo
dev.print(png, 'discrepancias_mapa.png', width = 730, height = 397)

#4.8 mapa exemplo Inhauma####
#gera layers delimitando tempos dos indicadores
areas_umahora <- rmrj %>% filter(temp_inhauma <= 3600)
areas_umahora <- st_union(areas_umahora)

tempo_inhauma <- dados_bairros[dados_bairros$bairr_mun=="Inhaúma","tempos"]
tempo_inhauma <- as.numeric(tempo_inhauma)
areas_tempomem <- rmrj %>% filter(temp_inhauma <= tempo_inhauma)
areas_tempomem <- st_union(areas_tempomem)

#gera mapa
rmrj$temp_inhauma[is.na(rmrj$temp_inhauma)] <- 120*60
ggplot() +
  geom_sf(data=rmrj, aes(fill = pmin(temp_inhauma/60 , 120) ) , color = "gray") +
  theme_nothing(legend=TRUE) +
  labs(x="", y="",fill="tempo (min) a partir\nde Inhaúma") +
  scale_fill_gradient(low="white",high="red") +     #gradiente de cores
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(1,"cm")) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=areas_umahora, aes(alpha=I(0.1)),color = "blue", size=1) + #destaca no mapa os bairros com t=1h
  geom_sf(data=areas_tempomem, aes(alpha=I(0.1)),color = "green", size=1) + #destaca no mapa os bairros com t=M&M
  geom_sf(data=metro, color = "black", size=1, show.legend = "line") + 
  geom_sf(data=trem, color = "black", size=1, show.legend = "line", linetype="twodash") + 
  geom_sf(data=brt, color = "black", size=0.5, show.legend = "line")

#salva mapa em arquivo
dev.print(png, 'compara_indicadores_inhauma.png', width = 730, height = 397)

#4.9 MAUP####
#inicia variaveis que guardam valor acumulado de empregos acessados para inhauma e exemplo. cada linha não se associa ao bairro, mas ao próprio índice
dados_bairros$emp_inhauma_acum <- dados_bairros$temp_exemplo
dados_bairros$emp_exemplo_acum <- dados_bairros$temp_exemplo

#cálculo dos valores acumulados de emprego
for (j in c( 1:ncol(dados_bairros) ) ){
  dados_bairros$emp_exemplo_acum[j] <- sum( (rank(dados_bairros$temp_exemplo, ties.method = "min") == j) * (dados_bairros$empregos) ,na.rm = TRUE) +
    ifelse( j==1, 0, dados_bairros$emp_exemplo_acum[j-1] )
  dados_bairros$emp_inhauma_acum[j] <- sum( (rank(dados_bairros$temp_inhauma, ties.method = "min") == j) * (dados_bairros$empregos) ,na.rm = TRUE) +
    ifelse( j==1, 0, dados_bairros$emp_inhauma_acum[j-1] )
}

#grafico está em sort porque o valor acumulado de emprego não se refere ao bairro, mas ao indice
ggplot(dados_bairros)+
  geom_point( aes(x=sort(temp_exemplo/60), y=ifelse(emp_exemplo_acum/1000<12,-5,emp_exemplo_acum/1000), alpha=I(0.5), color='Madureira') )+ #ifelse retira pontos que aparecem após trecho crescente
  geom_point( aes(x=sort(temp_inhauma/60), y=ifelse(emp_inhauma_acum/1000<12,-5,emp_inhauma_acum/1000), alpha=I(0.5), color='Inhaúma') )+ #ifelse retira pontos que aparecem após trecho crescente
  scale_colour_manual(values=c("red", "blue"))+
  geom_hline(yintercept = dados_bairros$pia[dados_bairros$bairr_mun=="Madureira"]/1000 , color='blue',linetype='dashed',
             size = 1, show.legend = TRUE)+
  geom_hline(yintercept = dados_bairros$pia[dados_bairros$bairr_mun=="Inhaúma"]/1000 , color='red',linetype='dashed',
             size = 1)+
  xlim(0 , 30)+
  ylim(0 , 120)+
  theme_classic()+
  labs(x="Tempo (min)", y="Empregos acumulados (milhares)", color="Bairro")

#salva gráfico em arquivo
dev.print(png, 'descontinuidade.png', width = 500, height = 327)