# query for South Atlantic 2020-March 2026, bottom temp, mixed layer depth, and salinity
# taken from https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/download
# select variables, box around area, and time then click automate for this command line script
copernicusmarine subset --dataset-id cmems_mod_glo_phy_my_0.083deg_P1D-m --variable bottomT --variable mlotst --variable so --start-datetime 2020-01-01T00:00:00 --end-datetime 2026-03-24T00:00:00 --minimum-longitude -81.93758229561611 --maximum-longitude -73.58601015136274 --minimum-latitude 23.92043656389978 --maximum-latitude 36.62098406437724 --minimum-depth 0.49402499198913574 --maximum-depth 5727.9169921875

# try again bigger extent
copernicusmarine subset --dataset-id cmems_mod_glo_phy_my_0.083deg_P1D-m --variable bottomT --variable mlotst --variable siconc --variable sithick --variable so --variable thetao --variable uo --variable usi --variable vo --variable vsi --variable zos --start-datetime 2026-03-24T00:00:00 --end-datetime 2026-03-24T00:00:00 --minimum-longitude -82.8115114150069 --maximum-longitude -70.62329125150937 --minimum-latitude 23.43562594555034 --maximum-latitude 36.08727653351544 --minimum-depth 0.49402499198913574 --maximum-depth 0.49402499198913574
