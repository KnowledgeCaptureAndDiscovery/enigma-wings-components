library(rsconnect)

rsconnect::setAccountInfo(name='${SHINY_NAME}', token='${SHINY_TOKEN}', secret='${SHINY_SECRET}')
rsconnect::deployApp()
