library(AzureAuth) ## to connect with credentials
library(AzureStor) ## to access the stored data

## install azure cli: https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-windows?tabs=azure-cli
## sign in to Azure using your AAD un/pw in the terminal/commandline: az login 
## get token in terminal/commandline: az account get-access-token --resource https://datacenterdc2datalake.blob.core.windows.net/

### define your token 
token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6IjJaUXBKM1VwYmpBWVhZR2FYRUpsOGxWMFRPSSIsImtpZCI6IjJaUXBKM1VwYmpBWVhZR2FYRUpsOGxWMFRPSSJ9.eyJhdWQiOiJodHRwczovL2RhdGFjZW50ZXJkYzJkYXRhbGFrZS5ibG9iLmNvcmUud2luZG93cy5uZXQvIiwiaXNzIjoiaHR0cHM6Ly9zdHMud2luZG93cy5uZXQvNzQ5YzFmYmEtMmMyOS00YTBmLTg3ZGEtODc3NjQ3YmJiZDg2LyIsImlhdCI6MTY2Mzg3NjUxNywibmJmIjoxNjYzODc2NTE3LCJleHAiOjE2NjM4ODE1MDYsImFjciI6IjEiLCJhaW8iOiJBVFFBeS84VEFBQUFKemRXMkZJRTNUdmdhR08rRFZMajkxdGxEY3FkZXdWKzJrU01VbW1mQXUrMW52NitwTWllOHRQR1RvbUpuNHlmIiwiYW1yIjpbInB3ZCJdLCJhcHBpZCI6IjA0YjA3Nzk1LThkZGItNDYxYS1iYmVlLTAyZjllMWJmN2I0NiIsImFwcGlkYWNyIjoiMCIsImZhbWlseV9uYW1lIjoiVG9tbGluIiwiZ2l2ZW5fbmFtZSI6IkhhbGVpZ2giLCJncm91cHMiOlsiNzcyYzQ3MWMtZTVkMy00ZWI0LWE2YTEtZGY5ZGVmOTYxZmVjIiwiYWQwOTQ4YWMtYWY2OC00NDk1LThjY2QtNDE0YzNjY2ZmMzczIl0sImlwYWRkciI6IjE3NC42Ny4xMjkuMTkiLCJuYW1lIjoiSGFsZWlnaCBUb21saW4iLCJvaWQiOiIxODk4MTI1My00MTY0LTQ5MjAtYTFiOS0xOWIwNjcxODA1ZjEiLCJwdWlkIjoiMTAwMzIwMDIxRUUwMzY0NyIsInJoIjoiMC5BU2tBdWgtY2RDa3NEMHFIMm9kMlI3dTlob0dtQnVUVTg2aENrTGJDc0NsSmV2RXBBSDguIiwic2NwIjoidXNlcl9pbXBlcnNvbmF0aW9uIiwic3ViIjoiejlNcXBEeWdRMW9yYUVnR3R0WVE3enl2NHMxVTlqY3B0UHc1LUJKMWZPZyIsInRpZCI6Ijc0OWMxZmJhLTJjMjktNGEwZi04N2RhLTg3NzY0N2JiYmQ4NiIsInVuaXF1ZV9uYW1lIjoiSGFsZWlnaHRAZGF0YWNlbnRlcnJlc2VhcmNoLm9yZyIsInVwbiI6IkhhbGVpZ2h0QGRhdGFjZW50ZXJyZXNlYXJjaC5vcmciLCJ1dGkiOiJkbXlJR1ltRktVQ1NVeV9MdXZNaUFBIiwidmVyIjoiMS4wIn0.Ig-kVqxoOZXrk-GDgS7HWwrBy6r956iCdsdJ06EkVDar3Q6PGPXd2kDBZajSjmwNnFAYZ1kijtGsDU74kFje7C-mII_sOMTjpcj6LhrdMdmCaQ8xu7LXS4q6fHTULngVz2e9hDVbWukgNAthhigNN3-5WwpkHl11a4uHINRg-NnRV62YHEsrYoAuSnAfAgRow-3W7SIoUUoN5QZTVgi9wHryKgE1YNT0J3d2LyJ3iNwpbMzpwu1ggccSLkCoBB3yLyZQpu3lyEYwQZBgA6RE5ZmY0fwEF5Z8EFssuGfhxOebY3g2aHvc9K0NGBeTAZpu0gMb7c3kY82qikIDO3zqAA"
# delete token before pushing to github

ad_endp_tok2 <- storage_endpoint("https://datacenterdc2datalake.blob.core.windows.net/", token=token) ## again, the rsource/URL

## Connections to some most-used containers.
cont_proj <- storage_container(ad_endp_tok2, "project") 
df <- storage_read_csv(cont_proj, "who_lives/2022/WhoLives_longer.csv") %>% select(-1) #make sure this is updated within datalake - querying_WhoLives_factconsolidated

for_csv <- df %>% select(api_path, vintage, geo_id, geo_name, state_fips, county_fips) # go back and grab tract_fips and msa_fips
# idea: have this for_csv and then for each analyzed df we can do analyzed_df %>% left_join(for_csv, by = c("geo_name" = "place)) or something. 
# Then we can always re-order it for the warehouse 