# Crear un diccionario de estudiantes y sus calificaciones
calificaciones = {'Juan': 90, 'María': 85, 'Carlos': 95, 'Laura': 92}

# Acceder a la calificación de un estudiante
calificacion_juan = calificaciones['Juan']
print("La calificación de Juan es:", calificacion_juan)

# Modificar la calificación de un estudiante
calificaciones['María'] = 88

# Agregar un nuevo estudiante y su calificación
calificaciones['Pedro'] = 91

# Eliminar a un estudiante del diccionario
del calificaciones['Carlos']

# Comprobar si un estudiante existe en el diccionario
if 'Laura' in calificaciones:
    print("Laura está en el diccionario de calificaciones.")

# Obtener todas las calificaciones
print("Calificaciones:", calificaciones)

# Iterar sobre las claves y valores del diccionario
for estudiante, calificacion in calificaciones.items():
    print(estudiante, ":", calificacion)