
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

# Convertir cada objeto <compound> en un diccionario y guardarlo en un archivo JSON
for i, compound in enumerate(root.findall('compound')):
    compound_dict = element_to_dict(compound)
    #creo el diccionario
    properties = {}
    for property in compound:
        properties[property.tag] = property.attrib
        for parameters in property:
            properties[property.tag][parameters.tag] = parameters.attrib
            print({parameters.tag:parameters.attrib})
    #print(properties)
    # Guardar el diccionario como JSON en un archivo
    compound_object = Reader(i)

    file_name = compound_object["CompoundID"].value
    file_name = "lecture/db_json/"+file_name+".json"   
    with open(file_name, 'w') as file:
        json.dump(properties, file, indent=4)
   # exit()    
print("FIN")
