library(RPostgres)

##########standard WRDS SQL query for an entire dataset
res <- dbSendQuery(wrds, "SELECT * FROM dataset")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

###########WRDS SQL query to get list of libraries
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

######WRDS SQL query to identify datasets within a specific library
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='library'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

######WRDS SQL query to identify column headers within a specific dataset
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='library'
                   and table_name='dataset'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

######WRDS SQL query to select specific variables within a specific dataset
res <- dbSendQuery(wrds, "select cusip,permno,date,bidlo,askhi
                   from crsp.dsf")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

######## filter dataset by variable values
res <- dbSendQuery(wrds, "select cusip,permno,date,bidlo,askhi
                   from crsp.dsf
                   where askhi > 2500
                   and bidlo < 2000")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

######## filter by date
res <- dbSendQuery(wrds, "select cusip,permno,date,bidlo,askhi
                   from crsp.dsf
                   where date between '2013-01-07'
                   and '2013-01-08'")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

######## filter by multiple criteria
res <- dbSendQuery(wrds, "select cusip,permno,date,bidlo,askhi
                   from crsp.dsf
                   where date between '1960-01-01'
                   and '1980-01-01'
                   and askhi > 2500
                   and bidlo < 2000")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

########### join and query two Compustat datasets
res <- dbSendQuery(wrds, "select a.gvkey, a.datadate, a.tic,
                   a.conm, a.at, a.lt, b.prccm, b.cshoq
                   from comp.funda a join comp.secm b
                   on a.gvkey = b.gvkey
                   and a.datadate = b.datadate
                   where a.tic = 'IBM'
                   and a.datafmt = 'STD'
                   and a.consol = 'C'
                   and a.indfmt = 'INDL'")
data <- dbFetch(res, n = -1)
dbClearResult(res)
data