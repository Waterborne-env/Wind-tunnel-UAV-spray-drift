# Wind-tunnel-UAV-spray-drift
Authors:  Farah Abi-Akar and Brenna Kent   
Last edited:  2022-07-08

## Disclaimer
The R scripts and data uploaded to this repository were used to generate the results to be published in:

Grant S, Perine J, Abi-Akar F, Lane T, Kent B, Mohler C, Scott C, and Ritter A.  A wind-tunnel assessment of parameters that may impact spray drift during UAV pesticide application.  Submitted for publication 2022.

These files are provided “as-is” with the sole purpose to allow the reproduction of the published results without any warranties of performance or fitness for any other purpose.

## R files 
File “UAVtunnel_Main.R” generates the majority of figures and tables in the publication, as well as the nozzle characterization ANOVA and the spray drift regression.  Different sections within the file are labeled by the figures, tables, and analyses they run.  To run this file, upload the input files found in the “data” folder in this repository to a folder on your computer, change the two folder locations in the first section, ensure the listed packages are installed, and run the script.

File “UAVtunnel_Direction.R” generates the figures showing wind direction and speed in the tunnel. The sections that generate the figures are specifically labeled. As with the other R file, upload the input files found in the “data” folder in this repository, change the two folder locations in the first section, ensure the listed packages are installed, and run the script.
