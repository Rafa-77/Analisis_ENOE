rm(list = ls())

# Lugar de trabajo
setwd("D:/alex_/Documents/UNAM/8vo Semestre/proba/Proyecto") 


# Use la ENOE, tratando de obtener una gráfica que refleje
# la situacion laboral desde un punto de vista psicologico (si desean trabajar ono,
# y si tienen trabajo o no). Tratando en primera de seleccionar solo a la poblacion
# de hombres oaxaqueños.

library(data.table)
library(foreign)
library(devtools)
library(questionr)
library(survey)
library(base)
library(ggrepel)
library(plotly)

# Bases de Datos
sdem121 <- read.dbf("enoe_n_2021_trim1_dbf/ENOEN_SDEMT121.dbf")
coe1t121 <- read.dbf ("enoe_n_2021_trim1_dbf/ENOEN_COE1T121.dbf")

sdem121 <- read.dbf("ENOEN_SDEMT121.dbf")
coe1t121 <- read.dbf ("ENOEN_COE1T121.dbf")
# Variables:
# SDEMT121 - ENT == 20 : Oaxaca
# SDEMT121 - SEX == 1 : Hombres

# SDEMT121 - POS_OCU : 
### 0	No aplica	
### 1	Trabajadores subordinados y remunerados
### 2	Empleadores	
### 3	Trabajadores por cuenta propia
### 4	Trabajadores sin pago

# COE1T121 - P2F : 
### 1	Sí tiene necesidad de trabajar	
### 2	Sólo tiene deseos de trabajar	
### 3	No tiene necesidad ni deseos de trabajar	
### 9	No sabe


# Para trabajar con la ENOE primero se define una llave.
llave<-c("CD_A","ENT","CON","V_SEL","N_HOG","N_REN")

# Se le añade la variable que nos interesa P2F
w <- c(llave,"P2F")

# Y se unen las bases de datos que contienen las variables a utilizar.
base1 <- merge(sdem121, 
               coe1t121[w], 
               by=llave, 
               all.x=TRUE)

# De la nueva base escogemos a los HOMBRES que sean de OAXACA.
HomOax <-base1[which(base1$ENT == '20' & base1$SEX == "1"),]


# Etiquetamos las variables.
HomOax$POS_OCU <- factor(HomOax$POS_OCU,
                         levels = c(0, 1, 2,3,4),
                         labels = c("No aplica",
                                    "Trabajadores subordinados y remunerados", 
                                    "Empleadores", 
                                    "Trabajadores por cuenta propia", 
                                    "Trabajadores sin pago"))

HomOax$P2F <- factor(HomOax$P2F,
                     levels = c(1, 2,3,9),
                     labels = c("Con necesidad de trabajar",
                                "Con deseos de trabajar", 
                                "Sin necesidad ni deseos de trabajar", 
                                "No sabe")) 

# Utilizamos un factor de ponderacion para que los datos muestrales obtengan propiedades
# que permitan su analisis.
PonMatrizHom <- wtd.table(HomOax$P2F, HomOax$POS_OCU, weights=HomOax$FAC_TRI)

# Obtenemos el DataFrame
df_2da <- as.data.frame.matrix(PonMatrizHom) 


###############################################################
# Gráficas
###############################################################

# Grafica de Trabajadores subordinados y remunerados
orden_trab_sub <- factor(row.names(df_2da), 
                         levels = rev(c("No sabe", 
                                        "Con deseos de trabajar",
                                        "Con necesidad de trabajar", 
                                        "Sin necesidad ni deseos de trabajar")))

fig1_2da <- plot_ly(df_2da, 
                type='pie', 
                labels = orden_trab_sub, 
                values = df_2da[,2], 
                textposition = 'inside') %>% 
  layout(uniformtext = list(minsize = 12, mode = 'hide'),
         title = colnames(df_2da)[2])

fig1_2da



# Grafica de Trabajadores por cuenta propia
orden_trab_cuent <- factor(row.names(df_2da), 
                           levels = rev(c("No sabe", 
                                          "Con deseos de trabajar",
                                          "Con necesidad de trabajar", 
                                          "Sin necesidad ni deseos de trabajar")))

fig2_2da <- plot_ly(df, 
                type='pie', 
                labels = orden_trab_cuent, 
                values = df_2da[,4], 
                textposition = 'inside') %>% 
  layout(uniformtext = list(minsize=12, mode='hide'),
         title = colnames(df_2da)[4])

fig2_2da
