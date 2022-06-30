. ~/.keychain/`/bin/hostname`-sh
cd /home/rodrigo/DengueMX
date=$(date '+%Y-%m-%d')

#Descarga de el más reciente archivo de dengue
/home/rodrigo/miniconda3/envs/Dengue/bin/python3 /home/rodrigo/DengueMX/scripts/Descarga.py

#Formateo de la base de dengue
/usr/bin/R < /home/rodrigo/DengueMX/scripts/format_dataset.R --no-save

#Descarga del más reciente archivo de clima
/usr/bin/R < /home/rodrigo/DengueMX/datos-clima/descarga_clima_1985_today.R --no-save

#Correr el modelo
/usr/bin/R < /home/rodrigo/DengueMX/scripts/model_bayes.R --no-save

#Subir a Github
/usr/bin/git -C /home/rodrigo/DengueMX add .
/usr/bin/git -C /home/rodrigo/DengueMX commit -m "Actualización ${date}"
/usr/bin/git -C /home/rodrigo/DengueMX push origin lognormal