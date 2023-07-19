logical_condition <- function(input_condition){
  switch(input_condition,
         "igual a" = "==",
         "maior que" = ">",
         "menor que" = "<",
         "maior ou igual a" = ">=",
         "menor ou igual a" = "<=",
         "diferente de" = "!="
  )
}
