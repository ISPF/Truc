
library(RODBC,quietly =T, warn.conflicts = F)
library(data.table)

# Extraction de la série CVS à partir du cube
my_conn  <- odbcConnect("EFT")
query  <- "select 
cast(cast(\"[Date].[Mois].[Mois].[MEMBER_CAPTION]\" as varchar) as Date) as date, 
cast(\"[Visiteurs].[Region].[Region].[MEMBER_CAPTION]\" as varchar)  as Region,
cast(\"[Visiteurs].[Pays Diff].[Pays Diff].[MEMBER_CAPTION]\" as varchar)  as PaysDiff,
cast(cast(\"[Measures].[Nombre de touristes]\" as varchar) as int) as Touristes
from openquery([SQL_CUBES_TOURISME], 'select non empty [Measures].[Nombre de touristes] on 0,
non empty ([Visiteurs].[Region].Children,[Visiteurs].[Pays Diff].Children,[Date].[Mois].children) on 1 
from EFT2015')"
dataSerie <- sqlQuery(my_conn, query)
close(my_conn)

fwrite(dataSerie,"S02E02/SerieTourisme.csv")
