#############################################################
## Este es el script para calcular notas en ESOD180, puede ##
##    ser utilizado, distribuido y modificado libremente   ##
#############################################################
# Cargar paquetes requeridos
library(tidyverse)
library(here)
# Importa todas las evaluaciones
here()
df <- read.csv('evaluacion-seminario.csv',sep=';')
str(df)
# Ordena los datos
grades <- df %>% 
  # Reemplaza las categorías por números (puntaje)
  # Note for DH of the future: create a function to make this code less gross.
  mutate(Item...Diagnostico.general..2.puntos..=recode(Item...Diagnostico.general..2.puntos..,
                                  'Logrado' = 3*2,
                                  'Logrado parcialmente' = 2*2,
                                  'No logrado' = 1
  )) %>% 
  mutate(Item..Diagnostico.odontologico..2.puntos..=recode(Item..Diagnostico.odontologico..2.puntos..,
                                                           'Logrado' = 3*2,
                                                           'Logrado parcialmente' = 2*2,
                                                           'No logrado' = 1
  )) %>% 
  mutate(Item..Metodologia..2.puntos..=recode(Item..Metodologia..2.puntos..,
                                              'Logrado' = 3*2,
                                              'Logrado parcialmente' = 2*2,
                                              'No logrado' = 1
  )) %>% 
  mutate(Item..Recursos.humanos..1.punto..=recode(Item..Recursos.humanos..1.punto..,
                                                  'Logrado' = 3,
                                                  'Logrado parcialmente' = 2,
                                                  'No logrado' = 1
  )) %>% 
  mutate(Item..Recursos.financieros..1.punto..=recode(Item..Recursos.financieros..1.punto..,
                                                      'Logrado' = 3,
                                                      'Logrado parcialmente' = 2,
                                                      'No logrado' = 1
  )) %>% 
  mutate(Item..Evaluacion.del.programa..2.puntos..=recode(Item..Evaluacion.del.programa..2.puntos..,
                                                          'Logrado' = 3*2,
                                                          'Logrado parcialmente' = 2*2,
                                                          'No logrado' = 1
  )) %>% 
  # Agrega una nueva columna (score) con el puntaje total de cada evaluador
  mutate(score = rowSums(.[3:8])) %>% 
  # Agrega una nueva columna (grades) con la nota de cada evaluador
  mutate(grade = (score * 7)/30) %>% 
  # Muestra la nota para cada estudiante
  group_by(Grupo) %>% 
  summarize(Nota=mean(grade))
# Guarda el archivo
write.csv2(grades,'notas.csv')
