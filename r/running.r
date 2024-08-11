library(tidyverse)
library(janitor)
library(lubridate)

# Cargo las sesiones de entrenamiento
act <- read_csv("../running.csv")
dim(act)

# Veo las columnas
colnames(act)

# Limpio los nombres.
colnames(act) <- colnames(clean_names(act))

# Veo que tipos de actividades hay registradas.
act |>
  group_by(tipo_de_actividad) |>
  summarize(n = n())

# Me quedo solo con las de running.
run <- filter(act, tipo_de_actividad == "Carrera")
run

# Me quedo con columnas de interes.
run <- run |>
  select(2,4:9,12,14:15,38:41)
colnames(run)

# Veamos los tipos de datos almacenados en cada columna
glimpse(run)

# Algunos cambios a realizar son
# A number: distancia, calorias, altura_minima, altura_maxima, ascenso_total, descenso_total
# A time: tiempo, tiempo_en_movimiento, tiempo_transcurrido.

parse_number(run$distancia)
parse_number(run$calorias)
parse_number(run$altura_minima)
parse_number(run$altura_maxima)
parse_number(run$ascenso_total)

#La siguiente arroja error.
parse_number(run$descenso_total)

# Reviso. Dice que recibio --. En ese caso, lo cambio a mano.
run |>
  filter(descenso_total == "--")

run[run$descenso_total=="--","descenso_total"] <- "0"

#Ahora si, reemplazo los tipos de datos de esas columnas.
to_number <- c("distancia","calorias","altura_minima","altura_maxima","ascenso_total","descenso_total")

run |>
  mutate(across(all_of(to_number), parse_number))

run <- run |>
  mutate(across(all_of(to_number), parse_number))

# Me queda transformar los ritmos medios de str a double. Primero reemplazo el ":" por ".", despues parseo.

run$ritmo_medio <- parse_number(gsub(":",".",run$ritmo_medio))

glimpse(run)

# Agrego el anio, el mes y el dia para cada activdad. Los extraigo de la fecha. Esto seria:

run |>
  mutate(anio = year(fecha), mes = month(fecha), dia = day(fecha)) |>
  select(anio,mes,dia)

# Creo las columnas y le asigno los valores.

weekdays(run$fecha)

# Factor ordenado por mes
month(run$fecha, label=TRUE) 


run[,c("anio","mes","dia","dia_semana")] <- run |>
  mutate(anio = year(fecha), mes = month(fecha, label=TRUE), dia = day(fecha), dia_semana = weekdays(run$fecha)) |>
  select(anio,mes,dia, dia_semana)

# Tambien agrego la hora.

run |>
  mutate(hora = format(fecha, "%H:%M:%S")) |>
  select(hora)


run[,"hora"] <- parse_time(format(run$fecha, "%H:%M:%S"))


# Paso variables categoricas a factors.

run$anio <- factor(run$anio, levels = c("2023","2024"))

day_english <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

run$dia_semana <- factor(run$dia_semana, levels=day_english, ordered = TRUE)


# ------- VIS --------


# Cantidad de entrenamientos segun anio
count_per_year <- run |>
  group_by(anio) |>
  summarize(cantidad = n())

diferencia <- count_per_year$cantidad[1] - count_per_year$cantidad[2]

# Bar char

ggplot(count_per_year, aes(x=anio, y = cantidad, fill = anio)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = cantidad), vjust= 4, size=5) +
  labs(title = "Entrenamientos por anio", subtitle = "02/2023-07/2024",x="Anio",y="Cantidad", fill = "Anio")

# Pie chart

count_per_year <- count_per_year |>
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

pie <- ggplot(count_per_year, aes(x="",y=cantidad,fill = anio)) + geom_bar(stat="identity",width = 1)
pie + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5) +
  labs(title="Entrenamientos por anio", subtitle= sprintf("Total: %d",sum(count_per_year$cantidad)) , x="", y="", fill="Anio") +
  theme_void()

# Cantidad de entrenos por mes y anio

count_per_month_year <- run |>
  group_by(mes,anio) |>
  summarize(cantidad = n())

ggplot(count_per_month_year, aes(x=mes,y=cantidad, fill=anio)) + geom_bar(stat="identity") +
  geom_text(aes(label = cantidad),position= position_stack(vjust= 0.5), size=4) +
  labs(title="Cantidad de entrenamientos por mes y anio", x='Mes',y='Cantidad',fill="Anio") +
  theme(axis.text.x = element_text(angle=45))


round(diferencia / 3 ) # Cantidad de semanas para alcanzar los entrenos, si entreno 3 veces x semana
round(diferencia / 3 ) / 4 # Osea, 4 meses.


# Cantidad de kilometros por anio

km_per_year <- run |>
  group_by(anio) |>
  summarize(kilometros = sum(distancia))

ggplot(km_per_year, aes(x= anio, y = kilometros, fill = anio)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(kilometros)), vjust = 4, size = 5 ) +
  labs(title = "Total kilometros por anio", x = "Anio", y = "Distancia", fill = "Anio")

# Cantidad de kilometros por mes y anio

km_per_year_month <- run |>
  group_by(anio,mes) |>
  summarize(kilometros = sum(distancia))

# En un grafico

ggplot(km_per_year_month, aes(x=mes, y = kilometros, fill = anio)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(kilometros)),position= position_stack(vjust= 0.5), size=3) +
  labs(title = "Kilometros por mes y anio",fill="Anio",x="Mes",y="Kilometros") + theme(axis.text.x = element_text(angle = 45))

# Por separado

ggplot(km_per_year_month, aes(x=mes, y = kilometros, fill = anio)) + geom_bar(stat = "identity") +
  facet_wrap(~anio, nrow = 2) +
  geom_text(aes(label = round(kilometros)),position= position_stack(vjust= 0.5), size=3) +
  labs(title = "Kilometros por mes y anio",fill="Anio",x="Mes",y="Kilometros") + 
  theme(axis.text.x = element_text(angle = 45))

###### AGOSTO 2023 ------

# Veamos Agosto de 2023. Hay 17 entrenamientos.


agosto_2023 <- run |>
  filter(mes=="Aug", anio==2023)

# cantidad de entrenos agosto 2023
count_agosto2023 <- agosto_2023 |>
  summarize(cantidad = n())
# Cantidad km agosto 2023
km_agosto2023 <- agosto_2023 |>
  summarize(kilometros = sum(distancia))
# Tiempo entrenando agosto 2023, en horas
tiempo_agosto2023 <- agosto_2023 |>
  summarize(tiempo_total = as.numeric(sum(tiempo) / 3600) )
# Promedio tiempo entrenamiento en agosto 2023, en horas
promedio_tiempo_agosto2023 <- agosto_2023 |>
  summarize(tiempo_total = as.numeric(mean(tiempo) / 3600) )
# Promedio distancia entrenamiento en agosto 2023
km_agosto2023 <- agosto_2023 |>
  summarize(kilometros = mean(distancia))

agosto_2023
# Boxplot de distancia.

ggplot(agosto_2023, aes(x=0, y=distancia)) + geom_boxplot() + geom_jitter(width = 0, size=2, alpha=0.5) +
  labs(title='Boxplot distancia Agosto 2023', x='Agosto 2023', y='Distancia')

ggplot(agosto_2023, aes(x=dia_semana, y=distancia, fill=dia_semana)) + geom_boxplot() + geom_jitter(width = 0) +
  labs(title='Boxplot distancia por dia Agosto 2023',x='Dia',y='Distancia',fill='Dia de la semana', caption = 'Los viernes no se entrena') +
  theme(axis.text.x = element_text(angle = 45))

# Semana con mas kilometros

run |>
  filter(fecha>=as.Date("07/08/23", "%d/%m/%y"), fecha<=as.Date("13/08/23", "%d/%m/%y")) |>
  select(distancia, dia_semana)


# Kilometros en esa semana
run |>
  filter(fecha>=as.Date("07/08/23", "%d/%m/%y"), fecha<=as.Date("13/08/23", "%d/%m/%y")) |>
  summarize(total_km = sum(distancia))


# Cantidad de entrenamientos segun dia de la semana
# Por dia

count_per_week_day <- run |>
  group_by(dia_semana) |>
  summarize(cantidad = n())

ggplot(count_per_week_day, aes(x=dia_semana, y = cantidad, fill = dia_semana)) + geom_bar(stat="identity") +
  geom_text(aes(label = cantidad), vjust=2) +
  labs(title = "Entrenamientos segun dia", subtitle = "Periodo [2023-2024]", x = "Dia", y = "Cantidad", fill = "Dia") +
  theme(axis.text.x = element_text(angle = 45))

# Por dia y anio

count_per_week_day_year <- run |>
  group_by(dia_semana, anio) |>
  summarize(cantidad = n())

# Stackeado

ggplot(count_per_week_day_year, aes(x=dia_semana, y = cantidad, fill = anio)) + geom_bar(stat="identity") +
  geom_text(aes(label = cantidad),position= position_stack(vjust= 0.5), size=4) + 
  labs(title = "Entrenamientos segun dia y anio", x = "Dia", y = "Cantidad", fill = "Dia") +
  theme(axis.text.x = element_text(angle = 45))

# No stackeado

ggplot(count_per_week_day_year, aes(x=dia_semana, y = cantidad, fill = anio)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label = cantidad),position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(title = "Entrenamientos segun dia y anio", x = "Dia", y = "Cantidad", fill = "Dia") +
  theme(axis.text.x = element_text(angle = 45))





# Extraigo los "outliers". Duraciones mayores a hora y media o menores a 40 minutos.
# Le doy margen de 5 minutos.
min_time <- as.difftime("00:35:00", format="%H:%M:%S")
max_time <- as.difftime("01:35:00", format="%H:%M:%S")

outliers_time <- run |>
  filter(tiempo > max_time | tiempo <min_time)

# Veo algunos de los sabados. Si el domingo hay carrera, los sabados suele hacerse una
# "activacion" de unos minutos.
outliers_time |>
  filter(dia_semana=="sábado") |>
  select(titulo, tiempo, fecha, dia_semana, distancia)
# Se ven 
# Veamos si efectivamente los dias siguientes hubo carrera.
fechas <- outliers_time |>
  filter(dia_semana=="sábado", tiempo <= min_time) |>
  select(fecha)

fechas <- as.Date(fechas$fecha + days(1))
fechas

run |>
  filter(as.Date(fecha) %in% fechas) |>
  select(titulo,distancia, fecha)

# La que falta se suspendio por lluvia jaja.

# Busquemos tambien entrenamientos largos de domingo, pueden ser carreras como se vio recien.
outliers_time |>
  filter(dia_semana=="domingo") |>
  select(titulo, tiempo, fecha, dia_semana, distancia)

# La maraton, la media maraton y algun entrenamiento de dos horas.

# Veamos finalmente durante la semana.

outliers_time |>
  filter(dia_semana!="domingo",dia_semana!="sábado" ) |>
  select(titulo, tiempo, fecha, dia_semana, distancia, ritmo_medio)

# Varios son previos a entrenar con mi profesor, usaba los entrenos que me sugeria el reloj.



# ----------- VIS TIEMPO ------

# Time series con fecha y tiempo.

ggplot(run, aes(x=fecha,y=tiempo, colour=anio)) + geom_line() + geom_point(size=1)

# Ploteo multiples series, una para cada dia de la semana.

ggplot(run, aes(x=fecha,y=tiempo)) + geom_line(aes(color=dia_semana)) + geom_point(size=0.6)

# Con plotly. Graficos interactivos.

library('plotly')

run$tiempo_minutos <- round(period_to_seconds(hms(run$tiempo)) / 60,digits=2)

fig <- plot_ly(run, x = ~fecha, y = ~tiempo_minutos, type = 'scatter', mode = 'lines+markers') |>
  layout(title = "Serie de tiempo duracion de entrenamientos",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Duracion (minutos)"))
fig

# Multiple, una para cada dia de la semana.

fig <- plot_ly(run, x = ~fecha, y = ~tiempo_minutos, color = ~dia_semana ,type = 'scatter', mode = 'lines+markers') |>
  layout(title = "Serie de tiempo duracion de entrenamientos",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Duracion (minutos)"))
# Aca se puede ver por parejas (Mon-Tues-Thurs-Sat y Wed-Fri-Sun) que los fin de semana se corre mas. 
fig


# ------ CORRELACIONES ----
# Veamos si mas tiempo implica mas distancia.

ggplot(run, aes(x=tiempo, y = distancia)) + geom_point(alpha=0.5) +
  labs(title="Tiempo en movimiento vs Distancia cubierta", x="Tiempo (hs)",y="Distancia (km)")



# Veamos si mas tiempo implica mas calorias.
ggplot(run, aes(x=tiempo, y = calorias)) + geom_point(alpha=0.5) + geom_smooth(method=lm) +
  labs(title="Tiempo en movimiento vs calorias gastadas", x="Tiempo (hs)",y="Calorias")