FROM rocker/shiny:4.1.0
RUN apt-get update && apt-get install -y libssl-dev
RUN install2.r readxl metafor gridExtra ggplot2 plotly rsconnect listviewer dyplr shinydashboard shinythemes
