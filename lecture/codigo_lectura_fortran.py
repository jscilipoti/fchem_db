from lectura_xml_class import Reader
import xml.etree.ElementTree as ET
import json

# Crear una función para convertir un elemento XML en un diccionario
def element_to_dict(element):
    data = {}
    for child in element:
        if len(child) > 0:
            data[child.tag] = element_to_dict(child)
        else:
            data[child.tag] = child.attrib.get('value')
    return data

# Leer el archivo XML
tree = ET.parse("lecture/ChemSep8.32.xml")
root = tree.getroot()

file = open("properties_lecture.txt", "w")

for compound in root.findall('compound'):
    for property in compound:
        cadena = property.attrib['name'].replace(' ', '_')
        cadena = cadena.replace("(","_")
        cadena = cadena.replace(")","")
        cadena = cadena.replace("*","")
        cadena = cadena.replace("-","_")
        file.write(f"call json%get('{property.tag}.value', self%{cadena}%value, found)\n")
    break
file.close()

file = open("properties_declaration.txt", "w")

for compound in root.findall('compound'):
    for property in compound:
        cadena = property.attrib['name'].replace(' ', '_')
        cadena = cadena.replace("(","_")
        cadena = cadena.replace(")","")       
        cadena = cadena.replace("*","")
        cadena = cadena.replace("-","_")         
        file.write(f"real(pr) :: {cadena}\n")
    break
file.close()

# real(pr) :: molecular_weight
# call json%get('MolecularWeight.value', self%molecular_weight, found)        
