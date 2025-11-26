# Script to deploy the Shiny App to Posit Connect Cloud or shinyapps.io

# 1. Install the rsconnect package
if (!require("rsconnect")) install.packages("rsconnect")
library(rsconnect)

# 2. Authenticate
# You need to link this R session to your Posit account.
# - Go to https://connect.posit.cloud/ (or https://www.shinyapps.io/)
# - Log in.
# - Click on your name/profile (top right) -> "Tokens" -> "Show".
# - Copy the command that looks like: rsconnect::setAccountInfo(name='...', token='...', secret='...')
# - Paste it in the Console below and press Enter.

# 3. Deploy
# Run the following command to publish your app.
# It will upload 'Data_Analysis.R' and your CSV data file.

rsconnect::deployApp(
  appDir = ".", 
  appFiles = c(
    "Data_Analysis.R", 
    "Student Satisfaction Survey for Agricultural Sciences Program - Form responses.csv"
  ),
  appName = "Ag_Student_Satisfaction_Survey", # Choose a unique name for your app
  forceUpdate = TRUE
)
