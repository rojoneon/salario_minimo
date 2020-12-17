# Titulo:   Análisis de casos positivos y fallecimientos por COVID en la CDMX
# Datos:     
# Fecha:    14-12-2020
# Autores:  Gatitos Contra la Desigualdad


# --- Directory and packages
rm(list = ls())

library(pacman)

p_load(survival, data.table, tidyverse, reshape2, gghighlight, RColorBrewer, wesanderson, 
       ggpubr, gridExtra, magrittr,ggrepel, lubridate, zoo, scales)

# ---  Importar los datos
datitos <- read.csv("sal_min.csv", 
                    header = T, )

glimpse(datitos)
names(datitos)


# sexenios = data.frame(x1=c(1983, 1989, 1995, 2001, 2007, 2013, 2019),
#                       x2=c(1988, 1994, 2000, 2006, 2012, 2018, 2021),
                      
sexenios = data.frame(x1=c(1982, 1988, 1994, 2000, 2006, 2012, 2018),
                      x2=c(1988, 1994, 2000, 2006, 2012, 2018, 2021),
                      col=c("De la Madrid", "Salinas", "Zedillo", "Fox", "Calderón", "Peña", "AMLO"))
glimpse(sexenios)
sexenios %<>%
  mutate( col = as_factor(col))


#Función para determinar theme de las gráficas----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      #axis.line = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9,hjust = 1,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   # margin = margin(b = -0.1,
                                   #                 t = -0.1,
                                   #                 l = 2,
                                   #                 unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}





#Grafica 1----



datitos %>% 
  ggplot()+
  geom_line( aes(x=anio,
                 y=sal_min)) +
  geom_rect(data = sexenios, 
            aes(xmin = x1, xmax = x2, 
                ymin = -Inf, ymax = Inf, 
                fill = col), 
                alpha = 0.3) +
  scale_fill_viridis_d(option = "inferno") +
  scale_x_continuous(breaks = seq(1982,2021,3), 
                     limits = c(1982,2021)) +
  scale_y_continuous(breaks = seq(0,400,50), 
                     limits = c(0,400),
                     labels =scales::dollar) +
  theme_map() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         #plot.margin = unit(c(10,30,10,20), units = "point"),
         #panel.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         #plot.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         plot.title = element_text(face = "bold"),
         #plot.subtitle = element_text(face = "bold", ),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  labs ( x = " ",
         y = "Salario mínimo diario",
         title = "Gráfica 1: Evolución del salario mínimo en México",
         subtitle = "Pesos constantes de 2021",
         fill = "Sexenio",
         caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de INEGI")




#Pobreza Laboral----
pob_lab <- read.csv("pobreza laboral.csv", 
                    header = T, )
glimpse(pob_lab)

pob_lab %<>% 
  #rowwise() %>% 
  #mutate( mymean = mean(c(primer,tercer) )) %>% 
  #arrange(mymean) %>% 
  mutate(x=factor(entidad, entidad))

pob_lab %>% 
  #arrange(desc(tercer)) %>% 
  mutate(x = fct_reorder(x, tercer)) %>%
  ggplot( ) +
  geom_segment( aes(x=x, xend=x, y=primer, yend=tercer), 
                color="grey", size = 3, lineend = "round") +
  geom_point( aes(x=x, y=primer), 
              color="#3a3a39", size=3 , alpha = 0.3) +
  geom_text(aes(x=x, y=primer, label = paste0(primer,"%")),
                color="#3a3a39",
                hjust = 0, nudge_x = 0, nudge_y = -3.1, size = 3) +
  geom_point( aes(x=x, y=tercer), 
              color="#dc5356", size=3 , alpha = 0.3) +
  geom_text(aes(x=x, y=tercer, label = paste0(tercer,"%")),
            color="#dc5356",
            hjust = 0, nudge_x = 0, nudge_y = .5, size = 3) +
  coord_flip() +
  scale_y_continuous( ) +
  theme_map() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.title = element_text(face = "bold"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)
  ) +
  labs ( x = " ",
         y = "Porcentaje de población ocupada",
         title = "Gráfica 2: Cambio en pobreza laboral en México durante 2020",
         subtitle = "Población ocupada con ingresos por debajo de línea de pobreza extrema. 
         Cambios entre primer y tercer trimestre",
         caption = "Fuente: Elaborado por @gatitosvsdesig con datos de ENOE, basado en gráfica de @xzxxlmxtxx.")


