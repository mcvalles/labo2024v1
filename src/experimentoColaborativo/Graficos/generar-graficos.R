# Graficas de ganancia incluidas en la presentacion

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("dplyr")
require("ggplot2")
require("GGally")
require("tidyr")
require("tidyverse")
require("scales")
require("ggrepel")

# creo environment global
envg <- env()

# Iniializo variables del environment global
envg$EXPENV <- list()
envg$EXPENV$dir = "~/buckets/b1/graficos/"
envg$EXPENV$datasets_dir = "~/buckets/b1/datasets/"

# Color selection
cols <- c("#a64d79", "#e69138", "#6aa84f", "#3d85c6", "#674ea7")

#------------------------------------------------------------------------------
graficar_experimento <- function(experimento) {
  
  #Filtrar resultados por experimento
  filtered_dataset <- subset(dataset, Experimento == experimento)
  
  #Colorear resultados por semilla
  plot_semilla = ggplot(filtered_dataset, aes(x = Envios, y = Ganancia, color = Semilla, group = interaction(Semilla, PCA))) +
                 geom_point() +
                 geom_smooth(aes(group = interaction(Semilla, PCA)), method = "loess", size= 1.5, se = FALSE) + # Use geom_smooth for curvier lines
                 geom_line(size = 1) + # Draw lines connecting the points
                 scale_color_manual(values = cols) + 
                 geom_label(aes(label = dollar_format()(Ganancia)), box.padding=0.8, size=3, show.legend = "point") +
                 labs(x = "Envíos", y = "Ganancia",title = paste0("Experimento ", experimento, ""), subtitle = "Los resultados se muestran coloreados por semilla.", color = "Semilla") +  # Change axis labels
                 theme_minimal() +
                 theme( panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       legend.text = element_text(size = 10),
                       axis.title = element_text(size = 10),
                       plot.background = element_rect(fill = "white"))
  
  #Colorear resultados por PCA
  plot_pca = ggplot(filtered_dataset, aes(x = Envios, y = Ganancia, color = PCA, group = interaction(Semilla, PCA))) +
             geom_point() +
             geom_smooth(aes(group = interaction(Semilla, PCA)), method = "loess", size= 1.5, se = FALSE) + # Use geom_smooth for curvier lines
             geom_line(size = 1) + # Draw lines connecting the points
             scale_color_manual(values = c("Sin PCA" = "#674ea7", "Con PCA" = "#a64d79")) + # Color lines based on PCA value
             geom_label(aes(label = dollar_format()(Ganancia)), box.padding=0.8, size=3, show.legend = "point") +
             labs(x = "Envíos", y = "Ganancia", title = paste0("Experimento ", experimento, ""), subtitle = "Los resultados se muestran coloreados por aplicacion de PCA.", color = NULL) +  # Change axis labels
             theme_minimal() +
             theme( panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    legend.text = element_text(size = 10),
                    axis.title = element_text(size = 10),
                    plot.background = element_rect(fill = "white"))
  
  #Visualizar resultados lado a lado
  plot_dual = ggplot(filtered_dataset, aes(x = Envios, y = Ganancia, color = PCA, group = interaction(Semilla, PCA))) +
    geom_point() +
    geom_smooth(aes(group = interaction(Semilla, PCA)), method = "loess", size= 1.5, se = FALSE) + # Use geom_smooth for curvier lines
    geom_line(size = 1) + # Draw lines connecting the points
    scale_color_manual(values = c("Sin PCA" = "#674ea7", "Con PCA" = "#a64d79")) + # Color lines based on PCA value
    geom_label(aes(label = dollar_format()(Ganancia)), box.padding=0.8, size=2, show.legend = "point" ) +
    labs(x = "Envíos", y = "Ganancia",  title = paste0("Experimento ", experimento, ""), subtitle = "Los resultados se muestran coloreados por aplicacion de PCA.", color = NULL) +  # Change axis labels
    theme_minimal() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size = 10),
           axis.title = element_text(size = 10),
           strip.text = element_blank(),
           plot.background = element_rect(fill = "white")) + 
    facet_wrap(~ PCA)
  
  # Salvar los graficos 
  ggsave(filename = paste0("experimento_", experimento, "_semillas.png"), plot = plot_semilla, path = envg$EXPENV$datasets_dir, width = 10, height = 6)
  ggsave(filename = paste0("experimento_", experimento, "_pca.png"), plot = plot_pca, path = envg$EXPENV$datasets_dir, width = 10, height = 6)
  ggsave(filename = paste0("experimento_", experimento, "_dual.png"), plot = plot_dual, path = envg$EXPENV$datasets_dir, width = 10, height = 6)
  
}
#------------------------------------------------------------------------------
graficar_promedios <- function() {
  
  #Promediar resultados de experimentos
  avg_data <- dataset %>%
    group_by(Experimento, PCA, Envios) %>%
    summarise(avg_Ganancia = mean(Ganancia, na.rm = TRUE)) %>%
    ungroup()
  
  #Colorear resultados por experimento
  promedios_exp = ggplot(avg_data, aes(x = Envios, y = avg_Ganancia, color = Experimento, group = interaction(Experimento, PCA))) +
    geom_point() +
    geom_smooth(aes(group = interaction(Experimento, PCA)), method = "loess", size= 1.5, se = FALSE) + # Use geom_smooth for curvier lines
    geom_line(size = 1) + # Draw lines connecting the points
    scale_color_manual(values = cols) + 
    geom_label(aes(label = dollar_format()(avg_Ganancia)), box.padding=0.8, size=4, show.legend = "point") +
    labs(x = "Envíos", y = "Ganancia Promedio",  title = "Ganancia promedio por experimento", subtitle = "Los resultados se muestran coloreados por experimento.", color = "Experimento") +  # Change axis labels
    theme_minimal() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size = 10),
           axis.title = element_text(size = 10),
           plot.background = element_rect(fill = "white"))
  
  #Colorear resultados por PCA
  promedios_pca = ggplot(avg_data, aes(x = Envios, y = avg_Ganancia, color = PCA, group = interaction(Experimento, PCA))) +
                  geom_point() +
                  geom_smooth(aes(group = interaction(Experimento, PCA)), method = "loess", size= 1.5, se = FALSE) + # Use geom_smooth for curvier lines
                  geom_line(size = 1) + # Draw lines connecting the points
                  scale_color_manual(values = c("Sin PCA" = "#674ea7", "Con PCA" = "#a64d79")) + # Color lines based on PCA value
                  geom_label(aes(label = dollar_format()(avg_Ganancia)), box.padding=0.8, size=4, show.legend = "point") +
                  labs(x = "Envíos", y = "Ganancia Promedio",  title = "Ganancia promedio por experimento", subtitle = "Los resultados se muestran coloreados por aplicacion de PCA.", color = NULL) +  # Change axis labels
                  theme_minimal() +
                  theme( panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.text = element_text(size = 10),
                         axis.title = element_text(size = 10),
                         plot.background = element_rect(fill = "white"))
  
  # Salvar los graficos 
  ggsave(filename = "promedio_resultados_exp.png", plot = promedios_exp, path = envg$EXPENV$datasets_dir, width = 10, height = 6)
  ggsave(filename = "promedio_resultados_pca.png", plot = promedios_pca, path = envg$EXPENV$datasets_dir, width = 10, height = 6)
  
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


#Leer dataset
dataset_dir = paste(envg$EXPENV$datasets_dir,"ganancias.csv", sep = "") 
dataset = fread(dataset_dir)
dataset = as.data.frame(dataset)

#Transformar variables categoricas
dataset$Envios = factor(dataset$Envios)
dataset$Semilla = factor(dataset$Semilla)
dataset$Experimento = factor(dataset$Experimento)
dataset$PCA = factor(dataset$PCA)

#Generar graficos para cada experimento
for (experimento in unique(dataset$Experimento)) {
  graficar_experimento(experimento)
}

graficar_promedios()


