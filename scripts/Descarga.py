import requests
import os
from datetime import date
from daterange import *

download_folder = '/Users/rod/DengueMX/datos-abiertos'

if not os.path.exists(download_folder):
    os.makedirs(download_folder)

#Fecha inicial de datos
start_date = date(2020, 12, 04) #December 2020. The one from 03/12/2020 has an error thus ignored
end_date   = date.today()

#Ejemplos de fechas
#https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/etv/historicos/2020/datos_abiertos_dengue_031220.zip
#https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/etv/historicos/2021/datos_abiertos_dengue_301221.zip
#https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/etv/historicos/2022/datos_abiertos_dengue_050522.zip


base_url   = 'https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/etv/historicos'

#Descarga por fecha
for fecha in daterange(start_date, end_date):

    #Formateo de los datos
    fecha_format = fecha.strftime('%d%m%y')
    dname        = "datos_abiertos_dengue_%s.zip" % fecha_format

    #Checar si ya lo tenemos en carpeta
    if not os.path.isfile(os.path.join(download_folder, dname)):

        url = base_url + "/" + str(fecha.year) + "/" + dname

        print('Descargando ' + fecha.strftime("%d/%m/%Y") + " de "  + url)

        # Descarga del archivo
        try:
            req = requests.get(url)

            if req.status_code == 404:
                print("Datos no encontrados para " + fecha.strftime("%d/%m/%Y"))

            else:
                # Split URL to get the file name
                filename = os.path.join(download_folder, url.split('/')[-1])

                # Writing the file to the local file system
                with open(filename, 'wb') as output_file:
                    output_file.write(req.content)
                print('Descarga completada de ' + fecha.strftime("%d/%m/%Y"))

        except:
            print('No encontrado el archivo para ' + fecha.strftime("%d/%m/%Y"))

    else:
        print("El archivo de " + fecha.strftime("%d/%m/%Y") + " ya está descargado")


print('¡Terminé!')
