#ORCHESTRATE
#-----------------------------
#File for downloading 
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
#. ~/.keychain/`/bin/hostname`-sh
#FROM https://stackoverflow.com/questions/55966634/unable-to-run-git-commands-with-crontab
eval `ssh-agent -s` && ssh-add ~/.ssh/github && ssh-add -l
cd /home/rod/DengueMX
date=$(date '+%Y-%m-%d')

#Descarga de el más reciente archivo de dengue
/home/rod/miniconda3/envs/DengueMX/bin/python3 /home/rod/DengueMX/scripts/Descarga.py

#Generamos la info con los panoramas previos
for fname in $(ls /home/rod/DengueMX/panoramas_epidemiologicos_previos/import)
do
    echo $fname
    /home/rod/miniconda3/envs/Dengue/bin/python3 panoramas_epidemiologicos_previos/import/$fname
done

#Formateo de la base de dengue
/usr/bin/R < /home/rod/DengueMX/scripts/format_dataset.R --no-save

#Descarga del más reciente archivo de clima
/usr/bin/R < /home/rod/DengueMX/datos-clima/descarga_clima_1985_today.R --no-save

#Correr el modelo
/usr/bin/R < /home/rod/DengueMX/scripts/model_bayes.R --no-save

#Subir a Github
/usr/bin/git -C /home/rod/DengueMX add .
/usr/bin/git -C /home/rod/DengueMX commit -m "Actualización ${date}"
/usr/bin/git -C /home/rod/DengueMX push origin lognormal