# Crear un diccionario de frutas y sus colores
frutas = {"alejandro": {"edad":25,"altura":1.78}, "banana": "amarillo", "cereza": "rojo"}

# Crear un subdiccionario de información adicional para una fruta específica
info_fruta = {"vitaminas": ["A", "C"], "calorias": 52}

# Agregar el subdiccionario al diccionario existente "frutas"
frutas["naranja"] = info_fruta

print (frutas.items())
