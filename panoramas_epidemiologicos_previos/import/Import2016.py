import tabula
import pandas as pd
import os
from os import listdir
import re
from glob import glob

#Directorio 2017
dir_2016 = "panoramas_epidemiologicos_previos/2016"

#Estados para verificar el sistema
estados = {"CHIAPAS","VERACRUZ","NUEVO LEÓN","GUERRERO","MICHOACÁN","TAMAULIPAS",
           "JALISCO","MORELOS","PUEBLA","NAYARIT","TABASCO","QUINTANA ROO","SINALOA",
           "YUCATÁN","SONORA","MÉXICO","COLIMA","SAN LUIS POTOSÍ","OAXACA","HIDALGO",
           "COAHUILA","CAMPECHE","BAJA CALIFORNIA","BAJA CALIFORNIA SUR","CHIHUAHUA",
           "DISTRITO FEDERAL","DURANGO","GUANAJUATO","QUERÉTARO","TLAXCALA","ZACATECAS",
           "AGUASCALIENTES"}

#Directorio de 2017
fnames    = [os.path.join(dir_2016, f) for f in listdir(dir_2016)]
fnames    = [name for name in fnames if name.find(".pdf") != -1]

for name in fnames:

    print("Leyendo " +  name)

    epiweek = re.sub(r'.*dengue_sem_|_[0-9]+.pdf', '', name)
    year    = re.sub(r'.*dengue_sem_[0-9]+_|.pdf', '', name)

    if float(epiweek) == 31 or float(epiweek) == 32:
        table = tabula.read_pdf(name, pages=8, area=(100, 7.48, 491, 700), pandas_options={'header': None})
    else:
        table  = tabula.read_pdf(name, pages=7, area=(100, 7.48, 491, 700), pandas_options={'header': None})

    df     = table[0]
    df     = df[df[0].isin(estados)]

    if epiweek == "44":
        durango = df[df[0] == "DURANGO"].iloc[0].dropna().reset_index(drop = True)
        df      = df.drop(df.columns[11:15], axis=1)
        df.iloc[df[0] == "DURANGO",:] = durango
        df.loc[df[0] == "DURANGO", 4] = 841

    df.columns = ["Estado","Probables (año pasado)", "Probables (año actual)",
                  "Confirmados Dengue No Grave (año pasado)",
                  "Confirmados Dengue No Grave (año actual)",
                  "Dengue con Signos de Alarma y Dengue Grave (año pasado)",
                  "Dengue con Signos de Alarma y Dengue Grave (año actual)",
                  "Total Confirmados (año pasado)", "Total Confirmados (año actual)",
                  "Defunciones (año pasado)", "Defunciones (año actual)"]

    df = df.assign(Semana_Epidemiologica = epiweek)
    df = df.assign(Anio = year)

    if df.shape[0] != 32:
        print("No sirvió " + name)
        #Pano_dengue_sem_52_2018
    else:
        if name != fnames[0]:
            df_all = pd.concat([df_all.reset_index(drop=True), df], axis=0)
        else:
            df_all = df


#Save as csv
df_all.to_csv(os.path.join("panoramas_epidemiologicos_previos/processed", "2016.csv"),  index=False)
