library(readxl)
library(tidyverse)

options(scipen = 999)

# Colocar todos los archivos en una sola carpeta y guardar los nombres
# en el objeto archivos
carpeta<-"/Volumes/GoogleDrive/Mi unidad/1.Maestria II Ciclo 2021/Curso de Analisis De Casos/Caso4/Data/BCCR"
archivos <- dir(carpeta)
#archivos<- "Curvas Semanales (Histórico 2017).xlsx"

# Inicializar una lista donde se va guardar cada tabla de las curvas
df_arch<- list()
s <- 0
# Para cada archivo se van a recorrer cada hoja para extraer la tabla de la 
# curva

for(i in 1:length(archivos)){
  
  path <- paste0(carpeta,"/",archivos[i])
  
  # Extraer los nombres de las hojas de excel
  sheets <- excel_sheets(path)
  df <- NA
  sem<-NA
 
  
  for(j in 1:length(sheets)){
    # Se lee el archivo de excel para la hoja j y se estandariza el nombre de
    # la columna
    df <- read_excel(path,sheets[j])%>%
      janitor::clean_names()
    
    # Se extrae el rango de fechas a la que hace referencia la curva
    sem <- as.character(
      df%>%
        filter(!is.na(x2))%>%
        select(2)%>%
        top_n(1)%>%
        mutate(
          x2=str_replace(x2," al ", "~"),
          x2=str_replace_all(x2,"\\**", ""),
          x2=trimws(str_replace_all(toupper(x2),"[A-Z]+",""))
          
        )
    )
    
    # Se limpian los datos para extraer de la tabla los datos
    df <- df%>%
      filter(!is.na(x2))%>%
      filter(substr(x3,1,6)!="Plazon en años")%>%
      select(-1)
    
    # Se excluyen todas las columnas que son nulas y que no tiene datos 
    # de la tabla
    df<- df[ , colSums(is.na(df))==0]
    
    # La primera fila de la tabla anterior contiene los nombres de la columna
    # Se realiza el cambio para colocarle el nombre a las columnas
    colnames(df)<- df[1,]
    
    # Se vuelven a estandarizar los nombres de las columnas y se transforma
    # las columnas a valores numericos
    
    df<-df[-1,]%>%
      janitor::clean_names()%>%
      mutate_if(is.character,as.numeric)
    
    # Se guarda en la lista la tabla de cada curva, se agrega el archivo y
    # la hoja de la que viene. Adicionalmente, cada tabla se guarda en el objeto
    # lista con el nombre de la semana
    
    df_arch[[sem]] <- df%>%
      mutate(archivo=archivos[i],
             Hoja=sheets[j])
    
    # A modo de log se imprime el nombre del archivo, la hoja y la semana
    print(archivos[i])
    print(sheets[j])
    print(sem)
  }
  
}

CurvSobLonger <- bind_rows(df_arch, .id = "Semana") %>%
  mutate(SemanaArchivo = Semana) %>%
  separate(Semana,
           c("SemanaInicio", "SemanaFin"),
           sep = "~",
           convert = T) %>%
  mutate(
    SemanaInicio = ifelse(
      nchar(SemanaInicio) >= 10,
      format(as.Date(SemanaInicio, format = '%d/%m/%Y'), "%Y-%m-%d"),
      format(as.Date(SemanaInicio, format = '%d/%m/%y'), "20%y-%m-%d")
    ),
    SemanaFin =
      ifelse(
        nchar(SemanaFin) >= 10,
        format(as.Date(SemanaFin, format = '%d/%m/%Y'), "%Y-%m-%d"),
        format(as.Date(SemanaFin, format = '%d/%m/%y'), "20%y-%m-%d")
      )
  )%>%
  mutate(Anio=trimws(str_replace_all(toupper(archivo),"[A-Z]+","")),
         Anio=trimws(str_replace_all(Anio,"[[:punct:]]+","")),
         Anio=trimws(str_replace_all(Anio,"\\́",""))
  )


CurvSobWider<-CurvSobLonger%>%
  select(archivo,Hoja,Anio,SemanaInicio,SemanaFin,
    plazo_en_dias,tasa_percent)%>%
  pivot_wider(names_from = plazo_en_dias, values_from =tasa_percent)


unique(CurvSobWider$SemanaFin)
unique(CurvSobWider$SemanaInicio)

rm(list=ls()[!ls() %in% c("CurvSobLonger","CurvSobWider","df_arch")])

save.image("/Volumes/GoogleDrive/Mi unidad/1.Maestria II Ciclo 2021/Curso de Analisis De Casos/Caso4/Data/DatosCurvaSoberana.RData")