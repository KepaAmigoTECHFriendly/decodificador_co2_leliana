#' @title Decodificación de datos CO2 del sensor Watteco VAQAO
#'
#' @description Decodifica los datos del sensor Watteco VAQAO. Como input, recibe la trama de datos en hexadecimal sin espacios.
#'
#' @param dato_hex
#'
#' @return json
#'
#' @examples  decode_co2_vaqao(dato_hex)
#'
#' @import rvest
#' stringr
#' jsonlite
#' dplyr
#'
#' @export

decode_co2_vaqao <- function(dato_hex){

  dato_hex <- as.character(dato_hex)

  url <- paste("http://support.nke.fr/Lora/?trameBatch=",dato_hex,"&timestamp=&BatchAttributes=3+1%2C10%2C7%2CT+2%2C100%2C6%2CH+3%2C10%2C6%2CCO2+4%2C10%2C6%2CCOV&+submit=Submit2",sep = "")

  html_inicial <- read_html(url)
  datos <-  html_inicial %>% html_nodes(xpath = "//textarea[contains(@wrap,'hard')]") %>% html_text() %>% str_trim() %>% unlist()
  datos <- fromJSON(datos)
  datos_sensor <- as.data.frame(datos$dataset)
  datos_sensor$ts <- format(as.numeric(as.POSIXct(gsub("T"," ", datos_sensor$data_absolute_timestamp)))*1000,scientific = FALSE)  # Paso a ts
  datos_sensor$data$value <- as.numeric(datos_sensor$data$value)
  datos_sensor$data$value[datos_sensor$data$label_name == "H"] <- datos_sensor$data$value[datos_sensor$data$label_name == "H"]/100
  datos_sensor$data$value[datos_sensor$data$label_name == "T"] <- datos_sensor$data$value[datos_sensor$data$label_name == "T"]/100

  json <- toJSON(datos_sensor)
  json <- json %>%
    gsub("\\[","",.) %>%
    gsub("\\]","",.)

  return(json)
}
