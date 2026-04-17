import datetime
import pandas
import os
from os import path

#Set start and end year
year_start=2013
year_end=2013
years=range(year_start,year_end+1) 

#Input CMEMS User and Password (case sensitive)
USER = ""
PASSWORD = ""

#Set Lat/lon bounds
min_lon = str(-78)
max_lon = str(-62)
min_lat = str(34)
max_lat = str(47)
for y in range(len(years)):

    out_dir = "D:/GLORYS/Data/"+str(years[y])+"/"
    if(not os.path.exists(out_dir)):
        os.makedirs(out_dir)
        
        
    dt = datetime.datetime(years[y],1,1)
    end = datetime.datetime(years[y],12,31)
    step = datetime.timedelta(days = 1)
    
    all_days = []
    
    while dt <= end:
        all_days.append(dt.strftime('%Y-%m-%d'))
        dt += step
        
    for d in range(len(all_days)):
        t1 = all_days[d]+" 00:00:00"
        t2 = all_days[d]+" 23:59:59"
        #Change to desired filename prefix
        new_name = "GLORYS_REANALYSIS_"+all_days[d]+".nc"
        #Change to appropriate output path
        
        
        if(path.exists(out_dir+new_name)):
           print(new_name+" EXISTS")
           continue
           
        #print(new_name)
        #If additional variables are desired: need to add "--variable var.name"
        
        command = "python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_MULTIYEAR_PHY_001_030-TDS --product-id cmems_mod_glo_phy_my_0.083_P1D-m --longitude-min "+min_lon+" --longitude-max "+max_lon+" --latitude-min "+min_lat+" --latitude-max "+max_lat+" --date-min "+t1+" --date-max "+t2+" --depth-min 0.493 --depth-max 5727.918 --variable so --variable thetao --variable uo --variable vo --variable zos --out-dir "+out_dir+" --out-name "+new_name+" --user "+USER+" --pwd "+PASSWORD

        os.system(command)