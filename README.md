# Authour: Angus Kennedy

# This repository contains all relevant scripts to my honours thesis project, 'Comparing CO2 and CH4 Fluxes in Prevailing Saltmarsh Zones in the Southern Gulf of Saint Lawrence,' completed at StFX University in Antigonish, Nova Scotia.

# There were only two primary scripts used in this project, a CR Basic script, used to program a Campbell Scientific CR1000 datalogger, and an R script, to process soil gas flux measurements.

# The CR Basic script included some wiring instructions for the Campbell Scientific CR1000 data logger, but was primarily simply the instructions for the scanning of sensors and recording of sensor outputs.

# The R script used in this project was intended to take one measurement .csv file and one benchmarking .csv file, correct the CO2 and CH4 concentrations in the measurement .csv file using the benchmarking .csv file, plot the CO2 and CH4 concentrations each against the file's time, determine a suitable timeframe based on field notes and the graphed CO2 and CH4 concentrations over time, calculate the mean rates of CO2 and CH4 concentration change, standardize the mean rates of CO2 and CH4 concentration change to the soil gas chamber's volume and surface area of its base, and output those standardized mean rates of CO2 and CH4 concentration change.
