. ~/.keychain/`/bin/hostname`-sh
cd /home/rodrigo/DengueMX
date=$(date '+%Y-%m-%d')
/home/rodrigo/miniconda3/envs/DengueMX/bin/python3 /home/rodrigo/DengueMX/scripts/Descarga.py
/usr/bin/R < /home/rodrigo/DengueMX/scripts/format_dataset.R --no-save
/usr/bin/R < /home/rodrigo/DengueMX/run_model.R --no-save
/usr/bin/git -C /home/rodrigo/DengueMX add .
/usr/bin/git -C /home/rodrigo/DengueMX commit -m "Actualización ${date}"
/usr/bin/git -C /home/rodrigo/DengueMX push origin master