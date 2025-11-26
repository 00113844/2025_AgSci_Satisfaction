# Script to fix xml2 version conflict and generate manifest

# 1. Force install xml2 binary to update to >= 1.4.0
# We use type="binary" to avoid compilation errors on Windows
install.packages("xml2", type = "binary")

# 2. Install the missing tidyverse dependencies
install.packages(c("haven", "rvest"), type = "binary")

# 3. Generate the manifest
if (!require("rsconnect")) install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".")

message("manifest.json has been generated successfully!")
