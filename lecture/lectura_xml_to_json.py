"""import xmltodict
import json

# Leer el archivo XML
with open("lecture/dos_comp.xml", 'r') as file:
    xml_string = file.read()

# Convertir el XML a un diccionario
xml_string = '<compound>'+xml_string+'</compound>'
data_dict = xmltodict.parse(xml_string)

# Obtener la lista de objetos <compound>
compounds = data_dict['compound']

# Verificar si hay múltiples objetos <compound>
if isinstance(compounds, list):
    # Si hay múltiples objetos <compound>, procesar cada uno por separado
    for i, compound in enumerate(compounds):
        compound_dict = {'compound': compound}

        # Convertir el diccionario a JSON
        json_data = json.dumps(compound_dict, indent=4)

        # Guardar el JSON en un archivo separado para cada objeto <compound>
        filename = f'lecture/db_json/output_{i+1}.json'
        with open(filename, 'w') as file:
            file.write(json_data)

else:
    # Si solo hay un objeto <compound>, procesarlo y guardarlo en un archivo JSON
    compound_dict = {'compound': compounds}

    # Convertir el diccionario a JSON
    json_data = json.dumps(compound_dict, indent=4)

    # Guardar el JSON en un archivo
    filename = 'lecture/db_json/output.json'
    with open(filename, 'w') as file:
        file.write(json_data)
        """
from lectura_xml_class import Reader
import xml.etree.ElementTree as ET
import json

# Leer el archivo XML
tree = ET.parse("lecture/ChemSep8.32.xml")
root = tree.getroot()

# Crear una función para convertir un elemento XML en un diccionario
def element_to_dict(element):
    data = {}
    for child in element:
        if len(child) > 0:
            data[child.tag] = element_to_dict(child)
        else:
            data[child.tag] = child.attrib.get('value')
    return data

# Convertir cada objeto <compound> en un diccionario y guardarlo en un archivo JSON
for i, compound in enumerate(root.findall('compound')):
    compound_dict = element_to_dict(compound)

    # Guardar el diccionario como JSON en un archivo
    compound_object = Reader(i)

    file_name = compound_object["CompoundID"].value
    file_name = "lecture/db_json/"+file_name+".json"   
    with open(file_name, 'w') as file:
        json.dump(compound_dict, file, indent=4)