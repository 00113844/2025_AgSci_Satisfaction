# Script to fix dependencies and generate manifest.json

# 1. Install missing dependencies identified by rsconnect
missing_packages <- c("haven", "rvest")
new_packages <- missing_packages[!(missing_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. Generate the manifest.json file
if (!require("rsconnect")) install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".")

message("manifest.json has been generated successfully!")
