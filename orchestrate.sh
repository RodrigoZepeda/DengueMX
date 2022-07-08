. ~/.keychain/`/bin/hostname`-sh
cd /media/rodrigo/covid/DengueMX
date=$(date '+%Y-%m-%d')

#Descarga de el más reciente archivo de dengue
/home/rodrigo/miniconda3/envs/Dengue/bin/python3 /media/rodrigo/covid/DengueMX/scripts/Descarga.py

#Generamos la info con los panoramas previos
for fname in $(ls /media/rodrigo/covid/DengueMX/panoramas_epidemiologicos_previos/import)
do
    echo $fname
    /home/rodrigo/miniconda3/envs/Dengue/bin/python3 panoramas_epidemiologicos_previos/import/$fname
done

#Formateo de la base de dengue
/usr/bin/R < /media/rodrigo/covid/DengueMX/scripts/format_dataset.R --no-save

#Descarga del más reciente archivo de clima
/usr/bin/R < /media/rodrigo/covid/DengueMX/datos-clima/descarga_clima_1985_today.R --no-save

#Correr el modelo
/usr/bin/R < /media/rodrigo/covid/DengueMX/scripts/model_bayes.R --no-save

#Subir a Github
/usr/bin/git -C /media/rodrigo/covid/DengueMX add .
/usr/bin/git -C /media/rodrigo/covid/DengueMX commit -m "Actualización ${date}"
/usr/bin/git -C /media/rodrigo/covid/DengueMX push origin lognormal