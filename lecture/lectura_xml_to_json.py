
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
"""
# Convertir cada objeto <compound> en un diccionario y guardarlo en un archivo JSON
for i, compound in enumerate(root.findall('compound')):
    compound_dict = element_to_dict(compound)
    #creo el diccionario
    properties = {}
    for property in compound:
        properties[property.tag] = property.attrib
        for parameters in property:
            for key,value in parameters.attrib.items():
                diccionario =  {key:value}
                properties[property.tag].update({parameters.tag:diccionario})
            #print({parameters.tag:parameters.attrib})
"""
# Convertir el XML en un diccionario
def parse_element(element):
    result = {}
    if len(element.attrib) > 0:
        result.update(element.attrib)
    if len(element) > 0:
        for child in element:
            if child.tag in result:
                if not isinstance(result[child.tag], list):
                    result[child.tag] = [result[child.tag]]
                result[child.tag].append(parse_element(child))
            else:
                result[child.tag] = parse_element(child)
    elif element.text:
        value = element.text.strip()
        try:
            result['value'] = int(value)
        except ValueError:
            try:
                result['value'] = float(value)
            except ValueError:
                result['value'] = value
    return result


for i, compound in enumerate(root.findall('compound')):
    properties = parse_element(compound)

    #recorro el diccionario y cambio los string por float
    for key1,subdict1 in properties.items():
        for key2,subdict2 in subdict1.items():
            try:
                valor = float(subdict2)
                subdict1[key2] = valor
            except (ValueError,TypeError):
                pass
            try:           
                for key3,subdict3 in subdict2.items():
                    try:
                        valor = float(subdict3)
                        subdict2[key3] = valor
                    except (ValueError,TypeError):
                        pass                      
            except AttributeError:
                pass




    compound_object = Reader(i)

    file_name = compound_object["CompoundID"].value
    file_name = "lecture/db_json/"+file_name+".json"   
# Escribir el diccionario en un archivo JSON
    with open(file_name, 'w') as archivo_json:
        json.dump(properties, archivo_json, indent=4)
      


"""
    #recorro el diccionario y cambio los string por float
    for key1,subdict1 in properties.items():
        for key2,subdict2 in subdict1.items():
            try:
                valor = float(subdict2)
                subdict1[key2] = valor
            except (ValueError,TypeError):
                pass
            try:           
                for key3,subdict3 in subdict2.items():
                    try:
                        valor = float(subdict3)
                        subdict2[key3] = valor
                    except (ValueError,TypeError):
                        pass                      
            except AttributeError:
                pass
"""
        

    # Guardar el diccionario como JSON en un archivo

print("FIN")
