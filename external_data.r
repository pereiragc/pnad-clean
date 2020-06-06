##  state labels

state.names <- c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara",
                 "Distrito Federal","Espirito Santo", "Goias", "Maranhao",
                 "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Para",
                 "Paraiba", "Parana", "Pernambuco", "Piaui", "Rio de Janeiro",
                 "Rio Grande do Norte", "Rio Grande do Sul", "Rondonia",
                 "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe",
                 "Tocantins")
state.codes <- c("12", "27", "16", "13", "29", "23", "53", "32", "52", "21",
                 "51", "50", "31", "15", "25", "41", "26", "22", "33", "24",
                 "43", "11", "14", "42", "35", "28", "17")
uf_translate <- data.table(UF=state.codes,
                           uf_name=factor(state.names, ordered = TRUE))
