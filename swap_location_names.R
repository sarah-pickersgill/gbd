## Matthew Coates
## Function to take country names and swap between GBD, UN WPP, World Bank, etc.

## arguments:
## data = name of input dataset
## var = variable that contains country name
## out = type of output, options are: GBD, UN WPP, World Bank

swap_locnames <- function(data=NA,var="location_name",out="GBD",version=NA) {
  
  data[,location_name:=data[[paste0(var)]]]
  
  if (out == "GBD") {
    
    data[location_name=="Republic of Korea",location_name:="South Korea"]
    data[location_name=="Dem. People's Rep. Korea",location_name:="North Korea"]
    data[location_name=="Brunei Darussalam",location_name:="Brunei"]
    data[location_name=="Cabo Verde",location_name:="Cape Verde"]
    data[location_name=="Congo Republic",location_name:="Congo"]
    data[location_name=="Gambia",location_name:="The Gambia"]
    data[location_name=="Gambia, The",location_name:="The Gambia"]
    data[location_name=="Eswatini",location_name:="Swaziland"]
    data[grepl("Iran",location_name),location_name:="Iran"]
    data[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
    data[grepl("Lao",location_name),location_name:="Laos"]
    data[grepl("Tanzania",location_name),location_name:="Tanzania"]
    data[grepl("United States of America",location_name),location_name:="United States"]
    data[grepl("Syria",location_name),location_name:="Syria"]
    data[grepl("Viet Nam",location_name),location_name:="Vietnam"]
    data[grepl("Bahamas",location_name),location_name:="The Bahamas"]
    data[grepl("Grenadines",location_name),location_name:="Saint Vincent and the Grenadines"]
    data[grepl("Democratic Rep of Congo",location_name),location_name:="Democratic Republic of the Congo"]
    data[grepl("DR Congo",location_name),location_name:="Democratic Republic of the Congo"]
    data[grepl("Republic of Congo",location_name),location_name:="Congo"]
    data[grepl("US Virgin Islands",location_name),location_name:="Virgin Islands, U.S."]
    data[grepl("o To",location_name) & grepl("ncipe",location_name),location_name:="Sao Tome and Principe"]
    data[grepl("St Lucia",location_name),location_name:="Saint Lucia"]
    data[grepl("Micronesia",location_name),location_name:="Federated States of Micronesia"]
    data[grepl("Timor",location_name),location_name:="Timor-Leste"]
    data[grepl("State of Palestine",location_name),location_name:="Palestine"]
    
  }
  
  ## there are one or two slight changes in GBD names
  if (out == "GBD" & version == 2019) {
    data[grepl("Swaziland",location_name),location_name:="Eswatini"]
    
  }
  
  return(data)
  
}



