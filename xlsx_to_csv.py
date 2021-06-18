# This Script requires an argument True or False if you would like to delete the already existing Excel files
# =============================================================================
# Author  : Nicola Brandenberg
# Created Date: June 2021
# =============================================================================
"""This script transforms all xlsx-Files in a specific folder to csv-Files. While doing so, it checks of the second
row is part of the date, if not it will delete this row. It also changes the date for example 13-Mar-2021 to
13.03.2021. With an argument at the start of the script (True/False), you can decide whether or not you want to delete
the xlsx-Files. """


# Imports
import sys
import pandas as pd
import glob
from pathlib import Path

data_Path = "data/S&P500/"
arg = sys.argv[1]  # Argument from the script call


def csv_with_pandas(del_files):
    # List with all filenames in directory
    filenames_list = glob.glob(data_Path + "*.xlsx")

    # Counter for new created CSV-Files
    file_counter = 0
    counter_del_obj = 0

    # Go through each file in file list
    for f in filenames_list:
        # Read and store content of an excel file
        read_file = pd.read_excel(f, engine='openpyxl')  # engine="openpyxl", because there was an error while
        # running the script from CMD. Fixes a problem where pandas only can read the old xls format Excel-Files

        # Delete second row
        if read_file.iloc[0, 1] == "Close":
            read_file = read_file.drop([0], axis=0)

        # print(read_file.get("Date"))

        # Convert Date in a Date data type
        read_file["Date"] = pd.to_datetime(read_file["Date"])

        # print(read_file.get("Date"))

        # Check if file already exists and replace "xlsx" with "csv"
        new_file = Path(f.replace("xlsx", "csv"))
        if not new_file.is_file():
            # Write the dataframe object into csv file
            read_file.to_csv(new_file, index=False, header=True, sep=";")
            # Increment File-Counter by 1
            file_counter += 1
            if del_files == "True":
                # Delete the xlsx-Files
                excel_file = Path(f)
                excel_file.unlink()
                # Increment Counter for deleted Objectives
                counter_del_obj += 1
        else:
            print(str(new_file) + " already exists")

    # Information message
    print(str(file_counter) + " new CSV-Files have been created.")
    print(str(counter_del_obj) + " Excel-files have been deleted.")


# Call function csv_with_pd()
try:
    # Check if system argument was True or False
    if arg == "True" or arg == "False":
        csv_with_pandas(arg)
    else:
        # Throwing an IndexError exception, because system argument wasn't True or False
        raise IndexError
except IndexError:
    print("Please enter a valid argument (True=Delete xlsx-Files, False=Don't delete xlsx-Files)")
except Exception as e:
    print("Error while converting Excel-Files into CSV-files:")
    print(e)  # print Error message
