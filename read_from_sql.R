# Read data from SQL Server
library(RODBC)

## *** means sensitive data
conn <- odbcConnect("***", uid="***", pwd="***")
data <- sqlQuery(conn, "select * from ***")
