library(AzureAuth) ## to connect with credentials
library(AzureStor) ## to access the stored data

## install azure cli: https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-windows?tabs=azure-cli
## sign in to Azure using your AAD un/pw in the terminal/commandline: az login 
## get token in terminal/commandline: az account get-access-token --resource https://datacenterdc2datalake.blob.core.windows.net/

### define your token 
token = "" # your token
ad_endp_tok2 <- storage_endpoint("https://datacenterdc2datalake.blob.core.windows.net/", token=token) ## again, the rsource/URL

## Connections to some most-used containers.
cont_proj <- storage_container(ad_endp_tok2, "project") 
df <- storage_read_csv(cont_proj, "who_lives/2022/WhoLives_longer.csv") %>% select(-1)

