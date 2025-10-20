### Alertas PADF

#### Labels

ciudad_labels <- c(
  "Guayaquil" = 1, "Esmeraldas" = 2, "Machala" = 3, "Manta" = 4, 
  "Portoviejo" = 5, "Santo Domingo" = 6, "Babahoyo" = 7, "Quevedo" = 8, 
  "Durán" = 9, "Salinas-La Libertad" = 10
)

data$nombre_ciudad_str <- factor(data$ciudad, levels = as.vector(rep(1:10, each = 1)) , labels = names(ciudad_labels))

data <- data %>%
  relocate(nombre_ciudad_str, .after = ciudad)


subcircuito_labels <- c(
  "Universidad 2" = 1, "Centro 1" = 2, "Puerta Negra" = 3, "Recreo 8" = 4, 
  "Recreo 4" = 5, "La Herradura 1" = 6, "Divino Niño 2" = 7, "Panorama 1" = 8, 
  "Centro 2" = 9, "Divino Niño 1" = 10, "Valle Hermoso 1" = 11, "La Tolita 1" = 12, 
  "Mata De Cacao 1" = 13, "Malvinas Norte 1" = 14, "Malvinas Sur 1" = 15, 
  "Trinitaria Norte 1" = 16, "Guasmo 1" = 17, "Floresta 1" = 18, "Centenario 1" = 19, 
  "Fortin 1" = 20, "Fortin 2" = 21, "Nueva Prosperina 2" = 22, "Virgen Del Carmen 2" = 23, 
  "5 De Junio 2" = 24, "Libertad Centro 3" = 25, "Virgen Del Carmen 1" = 26, 
  "Enriquez Gallo 1" = 27, "Libertad Centro 4" = 28, "La Union" = 29, 
  "Luz De America 1" = 30, "Rayito De Luz 2" = 31, "9 De Mayo 2" = 32, 
  "Nuevo Pilo 1" = 33, "La Ferroviaria 1" = 34, "Parque Lineal 1" = 35, 
  "Puerto Bolivar 1" = 36, "San Mateo 2" = 37, "Esteros 1" = 38, 
  "Maria Auxiliadora 2" = 39, "Maria Auxiliadora 3" = 40, "Miraflores 2" = 41, 
  "Cuba 4" = 42, "Universidad 1" = 43, "San Mateo 1" = 44, "Floron 4" = 45, 
  "Floron 2" = 46, "San Pablo 1" = 47, "San Pablo 2" = 48, "Portoviejo 3" = 49, 
  "Galo Plaza 1" = 50, "Galo Plaza 3" = 51, "7 De Octubre 1" = 52, 
  "7 De Octubre 2" = 53, "Control 1" = 54, "Valdramina 1" = 55, 
  "Guayacan 1" = 56, "Santa Rosa 3" = 57, "Chila 1" = 58, "Centro 4" = 59, 
  "Nuevo Israel 1" = 60, "Julio Moreno 1" = 61, "Cristo Vive 1" = 62, 
  "Tabiazo 1" = 63, "Valle Hermoso 1" = 64
)

data$nombre_subcircuito_str <- factor(data$subcircuito, levels = as.vector(rep(1:64, each = 1)) , labels = names(subcircuito_labels))

data <- data %>%
  relocate(nombre_subcircuito_str, .after = subcircuito)


data$nombre_encuestador_str <- data$username


# Ajustes


data <- data %>%
  mutate(empleo = if_else(id_encuestado == "993908060","6",empleo))

data <- data %>%
  mutate(
    # Primero, parsea la fecha y hora si no lo están,
    starttime = mdy_hms(starttime, tz = "UTC", locale = "C"),
    endtime = mdy_hms(endtime, tz = "UTC", locale = "C"),
    SubmissionDate = mdy_hms(SubmissionDate, tz = "UTC", locale = "C"),
    starttime = with_tz(starttime, tzone = "America/Lima"),
    endtime = with_tz(endtime, tzone = "America/Lima"),
    SubmissionDate = with_tz(SubmissionDate, tzone = "America/Lima"),
    endtime_fixed = as.character(endtime))

# Último estado (Tracking)

tracking <- data %>% 
  group_by(id_encuestado)%>%
  mutate(contactos = row_number())%>%
  filter(endtime == max(endtime) & username != "anonymousUser")%>%
  mutate(
    estado = case_when(
      acepta_llamada == 1 ~ "Encuesta exitosa",
      no_acepta == 1 ~ "No contesta",
      no_acepta == 2 ~ "Número equivocado",
      no_acepta == 3 ~ "Pidió reagendamiento",
      no_acepta == 4 ~ "No desea participar",
    )
  )%>%
  select(endtime,username,id_encuestado,nombre_encuestado,estado,contactos,evento_padf,nombre_ciudad_str)

# Consolidar rechazos

rechazos <- data%>%mutate(rechazo=if_else(no_acepta == "4" | consent == "2",1,0,missing = 0))%>%
  pull(rechazo)
equivocado <- data%>%mutate(equivocado=if_else(no_acepta == "2",1,0,missing = 0))%>%
  pull(equivocado)

data$rechazos <- sum(rechazos)
data$equivocado <- sum(equivocado)

# Filtrar pilotos y datos incorrectos

data <- data %>%
filter(!KEY %in% c("uuid:505ab3b3-96d0-447e-967a-6a8676e50642"))


data <- data %>%
  filter(username != "anonymousUser")

# Remmplazar nombres 

data <- data %>%
  mutate(nombre = if_else(KEY == "uuid:aefbfdda-8aff-4c11-9630-1245b6d1f74b",
                          "Nelly Aracely Toscano Rivera", nombre))

# Levantar alertas

data <- data

alertas <- data %>% filter(consent ==  1)%>%mutate(duration_minutes = round(as.numeric(duration)/60,2),
                                                num_cel = id_encuestado)


#### Alertas de tiempos #####

# Flag duración

alertas <- alertas %>%
  mutate(
    flag_duration_mas = if_else((duration_minutes - median(duration_minutes)) / sd(duration_minutes) > 1 & 
                                  consent == 1, 1, 0,missing = 0),
    flag_duration_menos = if_else((((duration_minutes - median(duration_minutes)) / sd(duration_minutes) < -1) | duration_minutes <=10) & 
                                    consent == 1, 1, 0, missing = 0))

alertas <- alertas %>%
  mutate(flag_duration_mas = if_else( duration_minutes >= 1000,0,flag_duration_mas))

#### Validación de saltos ####

alertas<-alertas %>%
  mutate(
    s_num_menores = if_else(!is.na(num_menores) & consent == 1 & menores != 1, 1, 0),
    s_tipo_delito = if_else(!is.na(tipo_delito) & consent == 1 & delito != 1, 1, 0),
    s_lugar_delito = if_else(!is.na(lugar_delito) & consent == 1 & delito != 1, 1, 0),
    s_otro_lugar_delito = if_else(!is.na(otro_lugar_delito) & consent == 1 & lugar_delito != 66, 1, 0),
    s_modalidad_delito = if_else(!is.na(modalidad_delito) & consent == 1 & delito != 1, 1, 0),
    s_frecuencia_delito = if_else(!is.na(frecuencia_delito) & consent == 1 & delito!= 1, 1, 0),
    s_mayorfrecuencia_delito = if_else(!is.na(mayorfrecuencia_delito) & consent == 1 & delito != 1, 1, 0),
    s_denuncia_delito = if_else(!is.na(denuncia_delito) & consent == 1 & delito != 1, 1, 0),
    s_nodenuncia_delito = if_else(!is.na(nodenuncia_delito) & consent == 1 & delito != 1 & denuncia_delito != 2, 1, 0),
    s_menor_delito = if_else(!is.na(menor_delito) & consent == 1 & menores != 1, 1, 0),
    s_menortipo_delito = if_else(!is.na(menortipo_delito) & consent == 1 & menores != 1, 1, 0),
    s_menortipo_delito_otro = if_else(!is.na(menortipo_delito_otro) & consent == 1 & menortipo_delito != 66, 1, 0),
    s_otro_lugar_desconfianza = if_else(!is.na(otro_lugar_desconfianza) & consent == 1 & desconfianza_66 != 1, 1, 0),
    s_norespeto = if_else(!is.na(norespeto) & consent == 1 & respeto == 1, 1, 0),
    s_otro_lugar_norespeto = if_else(!is.na(otro_lugar_norespeto) & consent == 1 & norespeto_66 != 1, 1, 0),
    s_otro_actividades = if_else(!is.na(otro_actividades) & consent == 1 & actividades_66 != 1, 1, 0),
    s_relacion_ninos3 = if_else(!is.na(relacion_ninos3) & consent == 1 & relacion_ninos != 1 & relacion_ninos != 2, 1, 0),
    s_otro_relacion_ninos3 = if_else(!is.na(otro_relacion_ninos3) & consent == 1 & relacion_ninos3_66 != 1, 1, 0),
    s_oportunidad = if_else(!is.na(oportunidad) & consent == 1 & frecuencia_pne != 1 & frecuencia_pne != 2 & frecuencia_pne != 3, 1, 0),
    s_testigosi = if_else(!is.na(testigosi) & consent == 1 & testigo != 1, 1, 0),
    s_testigo_frecuencia = if_else(!is.na(testigo_frecuencia) & consent == 1 & testigo != 1, 1, 0),
    s_testigo_muerte = if_else(!is.na(testigo_muerte) & consent == 1 & testigo != 1 & testigo_frecuencia != 1, 1, 0),
    s_preocupacion = if_else(!is.na(preocupacion) & consent == 1 & testigo != 1, 1, 0),
    s_otro_preocupacion = if_else(!is.na(otro_preocupacion) & consent == 1 & preocupacion != 66, 1, 0),
    s_otro_espacios = if_else(!is.na(otro_espacios) & consent == 1 & espacios_66 != 1, 1, 0),
    s_otro_temor = if_else(!is.na(otro_temor) & consent == 1 & temor_66 != 1, 1, 0),
    s_autorizar_menores = if_else(!is.na(autorizar_menores) & consent == 1 & menores != 1, 1, 0),
    s_otro_autorizar_menores = if_else(!is.na(otro_autorizar_menores) & consent == 1 & autorizar_menores_66 != 1, 1, 0),
    s_seguridad_tipo = if_else(!is.na(seguridad_tipo) & consent == 1 & seguridad_extra != 1, 1, 0),
    s_otro_seguridad_tipo = if_else(!is.na(otro_seguridad_tipo) & consent == 1 & seguridad_tipo_66 != 1, 1, 0),
    s_otro_enterar = if_else(!is.na(otro_enterar) & consent == 1 & enterar_66 != 1, 1, 0),
    s_otro_problemas = if_else(!is.na(otro_problemas) & consent == 1 & problemas_66 != 1, 1, 0),
    s_mujeres = if_else(!is.na(mujeres) & consent == 1 & reclutamiento != 1, 1, 0),
    s_otro_mujeres = if_else(!is.na(otro_mujeres) & consent == 1 & mujeres != 66, 1, 0),
    s_hombres = if_else(!is.na(hombres) & consent == 1 & reclutamiento != 1, 1, 0),
    s_otro_hombres = if_else(!is.na(otro_hombres) & consent == 1 & hombres != 66, 1, 0),
    s_reclutamiento_menores = if_else(!is.na(reclutamiento_menores) & consent == 1 & menores != 1, 1, 0),
    s_edad2 = if_else(!is.na(edad2) & consent == 1 & consumo != 1, 1, 0),
    s_sustancias = if_else(!is.na(sustancias) & consent == 1 & consumo != 1, 1, 0),
    s_otro_sustancias = if_else(!is.na(otro_sustancias) & consent == 1 & sustancias_66 != 1, 1, 0),
    s_comunidad = if_else(!is.na(comunidad) & consent == 1 & consumo != 1, 1, 0),
    s_razones = if_else(!is.na(razones) & consent == 1 & nivel != 1, 1, 0),
    s_exposicion = if_else(!is.na(exposicion) & consent == 1 & riesgo != 1, 1, 0),
    s_seguridad_sino = if_else(!is.na(seguridad_sino) & consent == 1 & actividades_sino != 1, 1, 0),
    s_padf2 = if_else(!is.na(padf2) & consent == 1 & padf != 1, 1, 0))


variables_salto <- c(
  "s_num_menores",
  "s_tipo_delito",
  "s_lugar_delito",
  "s_otro_lugar_delito",
  "s_modalidad_delito",
  "s_frecuencia_delito",
  "s_mayorfrecuencia_delito",
  "s_denuncia_delito",
  "s_nodenuncia_delito",
  "s_menor_delito",
  "s_menortipo_delito",
  "s_menortipo_delito_otro",
  "s_otro_lugar_desconfianza",
  "s_norespeto",
  "s_otro_lugar_norespeto",
  "s_otro_actividades",
  "s_relacion_ninos3",
  "s_otro_relacion_ninos3",
  "s_oportunidad",
  "s_testigosi",
  "s_testigo_frecuencia",
  "s_testigo_muerte",
  "s_preocupacion",
  "s_otro_preocupacion",
  "s_otro_espacios",
  "s_otro_temor",
  "s_autorizar_menores",
  "s_otro_autorizar_menores",
  "s_seguridad_tipo",
  "s_otro_seguridad_tipo",
  "s_otro_enterar",
  "s_otro_problemas",
  "s_mujeres",
  "s_otro_mujeres",
  "s_hombres",
  "s_otro_hombres",
  "s_reclutamiento_menores",
  "s_edad2",
  "s_sustancias",
  "s_otro_sustancias",
  "s_comunidad",
  "s_razones",
  "s_exposicion",
  "s_seguridad_sino",
  "s_padf2"
)

alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[,variables_salto], na.rm = T))

#### Validación de preguntas obligatorias (missings) #####  

library(dplyr)

alertas <- alertas %>%
  mutate(
    # Sección 1: Caracterización sociodemográfica
    m_nombre = if_else(is.na(nombre) & consent == 1, 1, 0),
    m_num_cel = if_else(is.na(num_cel) & consent == 1, 1, 0),
    m_ciudad = if_else(is.na(ciudad) & consent == 1, 1, 0),
    m_educacion = if_else(is.na(educacion) & consent == 1, 1, 0),
    m_genero = if_else(is.na(genero) & consent == 1, 1, 0),
    m_empleo = if_else(is.na(empleo) & consent == 1, 1, 0),
    m_ingresos = if_else(is.na(ingresos) & consent == 1, 1, 0),
    m_edad = if_else(is.na(edad) & consent == 1, 1, 0),
    m_num_perso = if_else(is.na(num_perso) & consent == 1, 1, 0),
    m_menores =  if_else(is.na(menores) & consent == 1, 1, 0),
    m_num_menores = if_else(is.na(num_menores) & consent == 1 & menores == 1, 1, 0),
    
    # Sección 2: Victimización: Frecuencia y tipo de delitos
    m_delito = if_else(is.na(delito) & consent == 1, 1, 0),
    m_tipo_delito = if_else(is.na(tipo_delito) & consent == 1 & delito == 1, 1, 0),
    m_lugar_delito = if_else(is.na(lugar_delito) & consent == 1 & delito == 1, 1, 0),
    m_otro_lugar_delito = if_else(is.na(otro_lugar_delito) & consent == 1 & delito == 1 & lugar_delito == 99, 1, 0),
    m_modalidad_delito = if_else(is.na(modalidad_delito) & consent == 1 & delito == 1, 1, 0),
    m_frecuencia_delito = if_else(is.na(frecuencia_delito) & consent == 1 & delito == 1, 1, 0),
    m_mayorfrecuencia_delito = if_else(is.na(mayorfrecuencia_delito) & consent == 1 & delito == 1, 1, 0),
    m_denuncia_delito = if_else(is.na(denuncia_delito) & consent == 1 & delito == 1, 1, 0),
    m_nodenuncia_delito = if_else(is.na(nodenuncia_delito) & consent == 1 & delito == 1 & denuncia_delito == 2, 1, 0),
    m_menor_delito = if_else(is.na(menor_delito) & consent == 1 & menores == 1, 1, 0),
    m_menortipo_delito = if_else(is.na(menortipo_delito) & consent == 1 & menor_delito == 1, 1, 0),
    
    # Sección 3: Institucionalidad
    m_confianza_policia = if_else(is.na(confianza_policia) & consent == 1, 1, 0),
    m_confianza_ffaa = if_else(is.na(confianza_ffaa) & consent == 1, 1, 0),
    m_confianza_minint = if_else(is.na(confianza_minint) & consent == 1, 1, 0),
    m_confianza_dinapen = if_else(is.na(confianza_dinapen) & consent == 1, 1, 0),
    m_confianza_ecu911 = if_else(is.na(confianza_ecu911) & consent == 1, 1, 0),
    m_confianza_sisrehab = if_else(is.na(confianza_sisrehab) & consent == 1, 1, 0),
    m_confianza_2_policia = if_else(is.na(confianza_2_policia) & consent == 1, 1, 0),
    m_confianza_2_ffaa = if_else(is.na(confianza_2_ffaa) & consent == 1, 1, 0),
    m_confianza_2_minint = if_else(is.na(confianza_2_minint) & consent == 1, 1, 0),
    m_confianza_2_dinapen = if_else(is.na(confianza_2_dinapen) & consent == 1, 1, 0),
    m_confianza_2_ecu911 = if_else(is.na(confianza_2_ecu911) & consent == 1, 1, 0),
    m_confianza_2_sisrehab = if_else(is.na(confianza_2_sisrehab) & consent == 1, 1, 0),
    m_desconfianza = if_else(is.na(desconfianza) & consent == 1, 1, 0),
    m_otro_lugar_desconfianza = if_else(is.na(otro_lugar_desconfianza) & consent == 1 & desconfianza == 66, 1, 0),
    m_respeto = if_else(is.na(respeto) & consent == 1, 1, 0),
    m_otro_lugar_norespeto = if_else(is.na(otro_lugar_norespeto) & consent == 1 & norespeto == 66, 1, 0),
    m_convivencia = if_else(is.na(convivencia) & consent == 1, 1, 0),
    m_mejora = if_else(is.na(mejora) & consent == 1, 1, 0),
    m_actividades = if_else(is.na(actividades) & consent == 1, 1, 0),
    m_otro_actividades = if_else(is.na(otro_actividades) & consent == 1 & actividades == 66, 1, 0),
    m_actividades2 = if_else(is.na(actividades2) & consent == 1, 1, 0),
    m_actividades3 = if_else(is.na(actividades3) & consent == 1, 1, 0),
    m_relacion = if_else(is.na(relacion) & consent == 1, 1, 0),
    m_relacion_ninos = if_else(is.na(relacion_ninos) & consent == 1, 1, 0),
    m_relacion_ninos2 = if_else(is.na(relacion_ninos2) & consent == 1, 1, 0),
    m_otro_relacion_ninos3 = if_else(is.na(otro_relacion_ninos3) & consent == 1 & relacion_ninos3 == 66, 1, 0),
    m_frecuencia_pne = if_else(is.na(frecuencia_pne) & consent == 1, 1, 0),
    m_accionar = if_else(is.na(accionar) & consent == 1, 1, 0),
    m_accionar2 = if_else(is.na(accionar2) & consent == 1, 1, 0),
    m_relacion_ninos3 = if_else(is.na(relacion_ninos3) & consent == 1 & (relacion_ninos == 1 | relacion_ninos == 2), 1, 0),
    m_oportunidad = if_else(is.na(oportunidad) & consent == 1 & (frecuencia_pne == 1 | frecuencia_pne == 2 | frecuencia_pne == 3) , 1, 0),
    m_norespeto = if_else(is.na(norespeto) & consent == 1 & respeto != 1, 1, 0),
    m_operativos = if_else(is.na(operativos) & consent == 1, 1, 0),
    
    # Sección 4: Percepción de seguridad
    m_testigo = if_else(is.na(testigo) & consent == 1, 1, 0),
    m_testigosi = if_else(is.na(testigosi) & consent == 1 & testigo == 1, 1, 0),
    m_testigo_frecuencia = if_else(is.na(testigo_frecuencia) & consent == 1 & testigo == 1, 1, 0),
    m_testigo_muerte = if_else(is.na(testigo_muerte) & consent == 1 & testigo == 1 & testigo_frecuencia == 1, 1, 0),
    m_preocupacion = if_else(is.na(preocupacion) & consent == 1 & testigo == 1, 1, 0),
    m_otro_preocupacion = if_else(is.na(otro_preocupacion) & consent == 1 & preocupacion == 66, 1, 0),
    m_espacios = if_else(is.na(espacios) & consent == 1, 1, 0),
    m_otro_espacios = if_else(is.na(otro_espacios) & consent == 1 & espacios == 66, 1, 0),
    m_parques = if_else(is.na(parques) & consent == 1, 1, 0),
    m_iglesia = if_else(is.na(iglesia) & consent == 1, 1, 0),
    m_calles = if_else(is.na(calles) & consent == 1, 1, 0),
    m_centros = if_else(is.na(centros) & consent == 1, 1, 0),
    m_espectaculos = if_else(is.na(espectaculos) & consent == 1, 1, 0),
    m_playa = if_else(is.na(playa) & consent == 1, 1, 0),
    m_centro = if_else(is.na(centro) & consent == 1, 1, 0),
    m_medidas = if_else(is.na(medidas) & consent == 1, 1, 0),
    m_actor = if_else(is.na(actor) & consent == 1, 1, 0),
    m_seguridad = if_else(is.na(seguridad) & consent == 1, 1, 0),
    m_temor = if_else(is.na(temor) & consent == 1, 1, 0),
    m_otro_temor = if_else(is.na(otro_temor) & consent == 1 & temor == 99, 1, 0),
    m_autorizar_menores = if_else(is.na(autorizar_menores) & consent == 1 & menores == 1, 1, 0),
    m_otro_autorizar_menores = if_else(is.na(otro_autorizar_menores) & consent == 1 & autorizar_menores == 66, 1, 0),
    m_seguridad_extra = if_else(is.na(seguridad_extra) & consent == 1, 1, 0),
    m_seguridad_tipo = if_else(is.na(seguridad_tipo) & consent == 1 & seguridad_extra == 1, 1, 0),
    m_otro_seguridad_tipo = if_else(is.na(otro_seguridad_tipo) & consent == 1 & seguridad_tipo == 66, 1, 0),
    m_medios = if_else(is.na(medios) & consent == 1, 1, 0),
    m_enterar = if_else(is.na(enterar) & consent == 1, 1, 0),
    m_otro_enterar = if_else(is.na(otro_enterar) & consent == 1 & enterar == 66, 1, 0),
    
    # Sección 5: Reclutamiento forzoso
    m_confianza = if_else(is.na(confianza) & consent == 1, 1, 0),
    m_problemas = if_else(is.na(problemas) & consent == 1, 1, 0),
    m_otro_problemas = if_else(is.na(otro_problemas) & consent == 1 & problemas == 66, 1, 0),
    m_testigo2 = if_else(is.na(testigo2) & consent == 1, 1, 0),
    m_reclutamiento = if_else(is.na(reclutamiento) & consent == 1, 1, 0),
    m_mujeres = if_else(is.na(mujeres) & consent == 1 & reclutamiento == 1, 1, 0),
    m_otro_mujeres = if_else(is.na(otro_mujeres) & consent == 1 & mujeres == 66, 1, 0),
    m_hombres = if_else(is.na(hombres) & consent == 1 & reclutamiento == 1, 1, 0),
    m_otro_hombres = if_else(is.na(otro_hombres) & consent == 1 & hombres == 66, 1, 0),
    m_reclutamiento_menores = if_else(is.na(reclutamiento_menores) & consent == 1 & menores == 1, 1, 0),
    
    # Sección 6: Continuación de Reclutamiento forzoso y Percepción de Seguridad
    m_consumo = if_else(is.na(consumo) & consent == 1, 1, 0),
    m_edad2 = if_else(is.na(edad2) & consent == 1 & consumo == 1, 1, 0),
    m_sustancias = if_else(is.na(sustancias) & consent == 1 & consumo == 1, 1, 0),
    m_otro_sustancias = if_else(is.na(otro_sustancias) & consent == 1 & sustancias == 66, 1, 0),
    m_comunidad = if_else(is.na(comunidad) & consent == 1 & consumo == 1, 1, 0),
    m_nivel = if_else(is.na(nivel) & consent == 1, 1, 0),
    m_razones = if_else(is.na(razones) & consent == 1 & nivel == 1, 1, 0),
    m_riesgo = if_else(is.na(riesgo) & consent == 1, 1, 0),
    m_exposicion = if_else(is.na(exposicion) & consent == 1 & riesgo == 1, 1, 0),
    m_actividades_sino = if_else(is.na(actividades_sino) & consent == 1, 1, 0),
    m_seguridad_sino = if_else(is.na(seguridad_sino) & consent == 1 & actividades_sino == 1, 1, 0),
    m_padf = if_else(is.na(padf) & consent == 1, 1, 0),
    m_padf2 = if_else(is.na(padf2) & consent == 1 & padf == 1 & actividades_sino == 1 & seguridad_sino == 1, 1, 0)
  )

variables_missing <- c(
  "m_nombre",
  "m_num_cel",
  "m_ciudad",
  "m_educacion",
  "m_genero",
  "m_empleo",
  "m_ingresos",
  "m_edad",
  "m_num_perso",
  "m_menores",
  "m_num_menores",
  "m_delito",
  "m_tipo_delito",
  "m_lugar_delito",
  "m_otro_lugar_delito",
  "m_modalidad_delito",
  "m_frecuencia_delito",
  "m_mayorfrecuencia_delito",
  "m_denuncia_delito",
  "m_nodenuncia_delito",
  "m_menor_delito",
  "m_menortipo_delito",
  "m_confianza_policia",
  "m_confianza_ffaa",
  "m_confianza_minint",
  "m_confianza_dinapen",
  "m_confianza_ecu911",
  "m_confianza_sisrehab",
  "m_confianza_2_policia",
  "m_confianza_2_ffaa",
  "m_confianza_2_minint",
  "m_confianza_2_dinapen",
  "m_confianza_2_ecu911",
  "m_confianza_2_sisrehab",
  "m_desconfianza",
  "m_otro_lugar_desconfianza",
  "m_respeto",
  "m_norespeto",
  "m_otro_lugar_norespeto",
  "m_convivencia",
  "m_mejora",
  "m_actividades",
  "m_otro_actividades",
  "m_actividades2",
  "m_actividades3",
  "m_relacion",
  "m_relacion_ninos",
  "m_relacion_ninos2",
  "m_relacion_ninos3",
  "m_otro_relacion_ninos3",
  "m_frecuencia_pne",
  "m_accionar",
  "m_accionar2",
  "m_oportunidad",
  "m_operativos",
  "m_testigo",
  "m_testigosi",
  "m_testigo_frecuencia",
  "m_testigo_muerte",
  "m_preocupacion",
  "m_otro_preocupacion",
  "m_espacios",
  "m_otro_espacios",
  "m_parques",
  "m_iglesia",
  "m_calles",
  "m_centros",
  "m_espectaculos",
  "m_playa",
  "m_centro",
  "m_medidas",
  "m_actor",
  "m_seguridad",
  "m_temor",
  "m_otro_temor",
  "m_autorizar_menores",
  "m_otro_autorizar_menores",
  "m_seguridad_extra",
  "m_seguridad_tipo",
  "m_otro_seguridad_tipo",
  "m_medios",
  "m_enterar",
  "m_otro_enterar",
  "m_confianza",
  "m_problemas",
  "m_otro_problemas",
  "m_testigo2",
  "m_reclutamiento",
  "m_mujeres",
  "m_otro_mujeres",
  "m_hombres",
  "m_otro_hombres",
  "m_reclutamiento_menores",
  "m_consumo",
  "m_edad2",
  "m_sustancias",
  "m_otro_sustancias",
  "m_comunidad",
  "m_nivel",
  "m_razones",
  "m_riesgo",
  "m_exposicion",
  "m_actividades_sino",
  "m_seguridad_sino",
  "m_padf",
  "m_padf2"
)

alertas <- alertas %>%
  mutate(
    total_missing = rowSums(across(all_of(variables_missing)), na.rm = TRUE)
  )


# Transformación de variables con valor 88 y 99 para "No sabe / No responde"
alertas <- alertas %>%
  mutate(
    # Variables con valor 88 (No sabe)
    nn_menores = if_else(menores == 88, 1, 0),
    nn_delito = if_else(delito == 88, 1, 0),
    nn_tipo_delito = if_else(tipo_delito == 88, 1, 0),
    nn_modalidad_delito = if_else(modalidad_delito == 88, 1, 0),
    nn_menor_delito = if_else(menor_delito == 88, 1, 0),
    nn_menortipo_delito = if_else(menortipo_delito == 88, 1, 0),
    nn_mejora = if_else(mejora == 88, 1, 0),
    nn_actividades_88 = if_else(actividades_88 == 1, 1, 0), # Opción múltiple
    nn_actividades2 = if_else(actividades2 == 88, 1, 0),
    nn_actividades3 = if_else(actividades3 == 88, 1, 0),
    nn_relacion_ninos2 = if_else(relacion_ninos2 == 88, 1, 0),
    nn_frecuencia_pne = if_else(frecuencia_pne == 88, 1, 0),
    nn_accionar2 = if_else(accionar2 == 88, 1, 0),
    nn_oportunidad = if_else(oportunidad == 88, 1, 0),
    nn_operativos = if_else(operativos == 88, 1, 0),
    nn_testigo = if_else(testigo == 88, 1, 0),
    nn_testigosi_88 = if_else(testigosi_88 == 1, 1, 0), # Opción múltiple
    nn_testigo_frecuencia = if_else(testigo_frecuencia == 88, 1, 0),
    nn_seguridad = if_else(seguridad == 88, 1, 0),
    nn_seguridad_extra = if_else(seguridad_extra == 88, 1, 0),
    nn_edad2_88 = if_else(edad2_88 == 1, 1, 0), # Opción múltiple
    nn_sustancias = if_else(sustancias == 88, 1, 0),
    nn_nivel = if_else(nivel == 88, 1, 0),
    nn_riesgo = if_else(riesgo == 88, 1, 0),
    nn_exposicion = if_else(exposicion == 88, 1, 0),
    nn_testigo2 = if_else(testigo2 == 88 | testigo2 == 99, 1, 0),  # Condición con OR para 88 y 99
    nn_reclutamiento = if_else(reclutamiento == 88 | reclutamiento == 99, 1, 0), # Condición con OR para 88 y 99
    nn_reclutamiento_menores = if_else(reclutamiento_menores == 88 | reclutamiento_menores == 99, 1, 0), # Condición con OR para 88 y 99
    nn_consumo = if_else(consumo == 88 | consumo == 99, 1, 0), # Condición con OR para 88 y 99
    nn_razones = if_else(razones_88 == 1 | razones_99 == 1, 1, 0), # Condición con OR para 88 y 99 y opción múltiple
    
    # Variables con valor 99 (No responde)
    nn_educacion = if_else(educacion == 99, 1, 0),
    nn_genero = if_else(genero == 99, 1, 0),
    nn_mujeres_99 = if_else(mujeres_99 == 1, 1, 0), # Opción múltiple
    nn_hombres_99 = if_else(hombres_99 == 1, 1, 0), # Opción múltiple
    nn_actividades_sino = if_else(actividades_sino == 99, 1, 0),
    nn_seguridad_sino = if_else(seguridad_sino == 99, 1, 0),
    nn_padf = if_else(padf == 99, 1, 0),
    nn_padf2 = if_else(padf2 == 99, 1, 0))

# Vector con los nombres de las variables nuevas
variables_nn <- c(
  "nn_menores",
  "nn_delito",
  "nn_tipo_delito",
  "nn_modalidad_delito",
  "nn_menor_delito",
  "nn_menortipo_delito",
  "nn_mejora",
  "nn_actividades_88",
  "nn_actividades2",
  "nn_actividades3",
  "nn_relacion_ninos2",
  "nn_frecuencia_pne",
  "nn_accionar2",
  "nn_oportunidad",
  "nn_operativos",
  "nn_testigo",
  "nn_testigosi_88",
  "nn_testigo_frecuencia",
  "nn_seguridad",
  "nn_seguridad_extra",
  "nn_edad2_88",
  "nn_sustancias",
  "nn_nivel",
  "nn_riesgo",
  "nn_exposicion",
  "nn_testigo2",
  "nn_reclutamiento",
  "nn_reclutamiento_menores",
  "nn_consumo",
  "nn_razones",
  "nn_educacion",
  "nn_genero",
  "nn_mujeres_99",
  "nn_hombres_99",
  "nn_actividades_sino",
  "nn_seguridad_sino",
  "nn_padf",
  "nn_padf2")


alertas <- alertas %>%
  mutate(
    total_nsnr = rowSums(alertas[,variables_nn], na.rm = T), #Crear score de NS/NR
    flag_nsnr = if_else(((total_nsnr - median(total_nsnr, na.rm = TRUE))/sd(total_nsnr, na.rm = TRUE))>2,1,0,missing = 0)) # Crear FLAG para NSNR



# Alerta de valores numéricos extremos ####

# Filtrar los valores extremos y calcular la mediana y desviación estándar

alertas <- alertas %>%
  mutate(
    num_perso = as.numeric(num_perso),
    num_menores = as.numeric(num_menores),
    # Cálculo de valores extremos para i2_1_cuantos_casos
    ex_num_perso = if_else(
      abs((num_perso - median(num_perso, na.rm = T)) / sd(num_perso, na.rm = T)) > 2, 1, 0,
      missing = 0
    ),
    ex_num_menores = if_else(
      menores == 1 & (abs((num_menores - median(num_menores, na.rm = T)) / sd(num_menores, na.rm = T)) > 2), 1, 0,
      missing = 0
    ))


## DUPLICADOS ----

caract_especi <- c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
                   "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
                   "ñ" = "n", "Ñ" = "N")

alertas <- alertas %>% 
  mutate(
    nombre =  str_squish(str_replace_all(toupper(nombre), caract_especi)))

alertas <- alertas %>%
  mutate(
    dup_nombre = if_else(duplicated(nombre) & consent == 1, 1, 0),
    dup_celular = if_else(duplicated(num_cel) & consent == 1,1,0))

alertas <- alertas %>%
  mutate(
    dup_nombre = if_else((duplicated(nombre) | duplicated(nombre, fromLast = TRUE)) & consent == 1, 1, 0),
    dup_celular = if_else((duplicated(num_cel) | duplicated(num_cel, fromLast = TRUE)) & consent == 1, 1, 0)
  )


# Encuestas rechazadas y duplicados, valores faltante y atípicos

alertas <- alertas %>%
  mutate(
    flag_rejected = if_else(consent == 2, 1, 0),
    flag_saltos = if_else(total_saltos > 0,1,0),
    flag_duplicated = if_else(dup_nombre == 1 | dup_celular == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else(ex_num_perso == 1 | ex_num_menores == 1,1,0))


### Crear alertas LOOKER


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duration_mas == 0 & flag_duration_menos == 0 & flag_nsnr == 0 & flag_duplicated == 0 &  
                            flag_missing == 0 &  flag_saltos == 0 & flag_rejected == 0 & flag_extreme_values == 0,1,0),
         Alertas = if_else(flag_duration_mas == 1 | flag_duration_menos == 1 | flag_nsnr == 1 | flag_duplicated == 1 |   
                             flag_missing == 1 | flag_saltos == 1 | flag_extreme_values == 1,1,0),
         Rechazos = if_else(flag_rejected == 1,1,0),
         tiempos_anomalos_mas = if_else(flag_duration_mas == 1,"Sí","No"),
         tiempos_anomalos_menos = if_else(flag_duration_menos == 1,"Sí","No"),
         exceso_nsnr = if_else(flag_nsnr == 1,"Sí","No"),
         id_repetido = if_else(flag_duplicated == 1, "Sí","No"),
         valores_faltantes = if_else(flag_missing == 1,"Sí","No"),
         saltos_irregulares = if_else(flag_saltos == 1,"Sí","No"),
         valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No")) 

# Indicadores

library(dplyr)
library(tibble)

# Metas manuales por encuestador
metas <- tribble(
  ~username,          ~meta_tratamiento,
  "hector.pino",              58,
  "gabriela.lopez",           30,
  "jordan.macias",            18,
  "jessica.perez",            29,
  "patricia.perez",           19,
  "melanie.leon",             24,
  "jean.olaya",               50,
  "made.moyano",              14,
  "abi.guanoluisa",            3
)

# Copia segura de columnas mínimas
cols_min <- c("username","id_encuestado","consent","no_acepta","evento_padf")
faltan <- setdiff(cols_min, names(data))
if (length(faltan) > 0) stop("Faltan columnas en `data`: ", paste(faltan, collapse = ", "))

data_src <- data %>%
  select(all_of(cols_min)) %>%
  mutate(
    consent_i   = suppressWarnings(as.integer(consent)),     # 1 si completa
    no_acepta_i = suppressWarnings(as.integer(no_acepta)),   # 1,2,3,4 o NA
    trat_flag   = suppressWarnings(as.integer(evento_padf))  # 1=tratamiento, 0=control
  )

# Un registro por caso de tratamiento:
# - se asigna al primer encuestador que lo tocó
# - se toma el último estado no_acepta visto
casos_trat <- data_src %>%
  filter(trat_flag == 1L, username != "anonymousUser") %>%
  group_by(id_encuestado) %>%
  summarise(
    username_asignado = dplyr::first(username),
    completado        = as.integer(any(consent_i == 1, na.rm = TRUE)),
    ultimo_no_acepta  = dplyr::last(no_acepta_i[!is.na(no_acepta_i)], default = NA_integer_),
    .groups = "drop"
  )

# Agregados por encuestador y métricas finales
progreso_trat_enc <- casos_trat %>%
  group_by(username_asignado) %>%
  summarise(
    llamados    = dplyr::n(),                                   # casos únicos tocados
    completadas = sum(completado, na.rm = TRUE),                # completas
    no_contesta = sum(ultimo_no_acepta == 1, na.rm = TRUE),
    equivocado  = sum(ultimo_no_acepta == 2, na.rm = TRUE),
    reagendar   = sum(ultimo_no_acepta == 3, na.rm = TRUE),
    no_desea    = sum(ultimo_no_acepta == 4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(username = username_asignado) %>%
  right_join(metas, by = "username") %>%                        # mantener todos los de metas
  mutate(
    llamados                 = coalesce(llamados, 0L),
    completadas              = coalesce(completadas, 0L),
    no_contesta              = coalesce(no_contesta, 0L),
    equivocado               = coalesce(equivocado,  0L),
    reagendar                = coalesce(reagendar,   0L),
    no_desea                 = coalesce(no_desea,    0L),
    meta_tratamiento         = coalesce(meta_tratamiento, 0L),
    
    # Meta efectiva: meta original menos "no desea"
    meta_efectiva            = pmax(meta_tratamiento - no_desea, 0L),
    
    # Avances y pendientes
    avance_llamados          = if_else(meta_tratamiento > 0, llamados    / meta_tratamiento, NA_real_),
    avance_completadas       = if_else(meta_efectiva      > 0, completadas / meta_efectiva,   NA_real_),
    pendientes_por_llamar    = pmax(meta_tratamiento - llamados,     0L),
    pendientes_por_completar = pmax(meta_efectiva      - completadas, 0L)
  ) %>%
  select(
    username,
    meta_tratamiento, meta_efectiva,
    llamados, completadas,
    avance_llamados, avance_completadas,
    pendientes_por_llamar, pendientes_por_completar,
    no_contesta, equivocado, reagendar, no_desea
  ) %>%
  arrange(username)
