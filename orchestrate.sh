#ORCHESTRATE
#-----------------------------
#File for downloading 
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
#. ~/.keychain/`/bin/hostname`-sh
#FROM https://stackoverflow.com/questions/55966634/unable-to-run-git-commands-with-crontab
eval `ssh-agent -s` && ssh-add ~/.ssh/github && ssh-add -l
cd /Users/rodrigozepedatello/Documents/DengueMX
date=$(date '+%Y-%m-%d')

#Descarga de el más reciente archivo de dengue
/usr/local/Caskroom/miniconda/base/envs/DengueMX/bin/python3 /Users/rodrigozepedatello/Documents/DengueMX/scripts/Descarga.py

#Generamos la info con los panoramas previos
for fname in $(ls /Users/rodrigozepedatello/Documents/DengueMX/panoramas_epidemiologicos_previos/import)
do
    echo $fname
    /usr/local/Caskroom/miniconda/base/envs/DengueMX/bin/python3 panoramas_epidemiologicos_previos/import/$fname
done

#Formateo de la base de dengue
/usr/local/bin/R < /Users/rodrigozepedatello/Documents/DengueMX/scripts/format_dataset.R --no-save

#Descarga del más reciente archivo de clima
/usr/local/Caskroom/miniconda/base/envs/DengueMX/bin/python3 /Users/rodrigozepedatello/Documents/DengueMX/scripts/download_climate.py

#Correr el modelo
/usr/local/bin/R < /Users/rodrigozepedatello/Documents/DengueMX/scripts/model_bayes.R --no-save

#Subir a Github
/usr/bin/git -C /Users/rodrigozepedatello/Documents/DengueMX add .
/usr/bin/git -C /Users/rodrigozepedatello/Documents/DengueMX commit -m "Actualización ${date}"
/usr/bin/git -C /Users/rodrigozepedatello/Documents/DengueMX push origin lognormal