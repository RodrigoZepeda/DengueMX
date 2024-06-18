import os
from tabula import read_pdf
import pandas as pd
from datetime import datetime, timedelta
import requests
import warnings

# The initial year in CONAGUA is 1985
init_year = 1985

# Directory
save_dir = os.path.join("datos-clima","pdf_reports")

# The maximal year is current year - 35 days
current_date = datetime.now()
date_35_days_ago = current_date - timedelta(days=35)
year_35_days_ago = date_35_days_ago.year
end_year = year_35_days_ago

# Get weather variables
wvar = ["PREC","TMIN","TMAX","TMED"]

# All tables
all_data_frames = []

for year in range(init_year, end_year + 1):
    for var in wvar:

        # Show what we are printing
        print(f"Processing year: {year}, variable: {var}")

        #Construct the url for download
        url = f"https://smn.conagua.gob.mx/tools/DATA/Climatología/Pronóstico%20climático/Temperatura%20y%20Lluvia/{var}/{year}.pdf"

        # Use os.path.join to construct file paths
        fname = os.path.join(save_dir, f"{var}_{yr}.pdf")
        savename = os.path.join(save_dir, f"{var}_{yr}.csv")


        #Check if I've already got the data or if its updatable
        if not os.path.isfile(fname) or year == end_year:

            print(f"Downloading year: {year}, variable: {var}")

            #Get the file from CONAGUA (their certificate is not working)
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                response = requests.get(url, verify=False)

            # Save the content to a file
            with open(fname, 'wb') as file:
                file.write(response.content)

        # Read the file in tabula
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            tb = read_pdf(fname, lattice = "True", pages = "1", area = ["52","33","520","730"])
            tb = pd.DataFrame(tb[0])

        tb["year"] = year
        tb["var"] = var

        # Append the data frame to the list
        all_data_frames.append(tb)

# Concatenate all data frames into a single data frame
preclima = pd.concat(all_data_frames, ignore_index=True)

# Convert column names to uppercase
preclima.columns = preclima.columns.str.upper()

# Modify ENTIDAD to uppercase ESTADO
preclima['ENTIDAD'] = preclima['ESTADO'].str.upper()

# Rename YEAR to ANIO
preclima = preclima.rename(columns={'YEAR': 'ANIO'})

# Define a function to map variable names
def map_variable(var):
    if var == "PREC":
        return "Precipitacion"
    elif var == "TMIN":
        return "Temperatura_Minima"
    elif var == "TMAX":
        return "Temperatura_Maxima"
    elif var == "TMED":
        return "Temperatura_Promedio"
    else:
        return var

# Apply the function to create VARIABLE column
preclima['VARIABLE'] = preclima['VAR'].apply(map_variable)

# Pivot the table to long format
preclima = preclima.melt(id_vars=['ENTIDAD', 'ANIO', 'VARIABLE'],
                          value_vars=['ENE', 'FEB', 'MAR', 'ABR', 'MAY', 'JUN',
                                      'JUL', 'AGO', 'SEP', 'OCT', 'NOV', 'DIC'],
                          var_name='MES', value_name='VALOR')

# Map MES to MES_NUM
month_map = {
    'ENE': '01', 'FEB': '02', 'MAR': '03', 'ABR': '04', 'MAY': '05', 'JUN': '06',
    'JUL': '07', 'AGO': '08', 'SEP': '09', 'OCT': '10', 'NOV': '11', 'DIC': '12'
}
preclima['MES_NUM'] = preclima['MES'].map(month_map)

# Create FECHA_PROXY column
preclima['FECHA_PROXY'] = preclima.apply(lambda row: datetime(int(row['ANIO']), int(row['MES_NUM']), 15), axis=1)

# Select final columns
preclima = preclima[['ENTIDAD', 'ANIO', 'VARIABLE', 'MES', 'VALOR', 'MES_NUM', 'FECHA_PROXY']]

# Save the combined data frame to a CSV file if needed
save_dir = os.path.join("datos-clima","processed")
preclima.to_csv(os.path.join(save_dir, "Clima_info.csv"), index=False)




