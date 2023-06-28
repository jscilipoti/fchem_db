import xml.etree.ElementTree as ET
from lectura_xml_class import Reader

root = ET.parse("lecture/ChemSep8.32.xml")
comp = root.getroot()

compound_conter = 0
for compund in comp:
    compound_object = Reader(compound_conter)

    file_name = compound_object["CompoundID"].value
    file_name = "lecture/db/"+file_name+".txt"
    file = open(file_name, "w")    

    for property in compund:
        #property.tag #Etiqueta, nombre de la propiedad
        #imprimo el nombre de la namelist
        file.write("&"+property.tag+"\n")

        attributes = property.attrib #Guardo el diccionario en attributes
        #recorro el diccionario e imprimo el contenido de la namelist
        for attribute, value in attributes.items():
            if attribute == "value":
                file.write(f"  {attribute} = {value}\n") 
            else:
                file.write(f"  {attribute} = \"{value}\"\n")
  
        for parameters in property:
            attributes = parameters.attrib
            for attribute, value in attributes.items():
                if attribute == "units": continue
                file.write(f"  {parameters.tag} = {value}\n") 
        file.write("/"+"\n")  
    file.close()
    compound_conter += 1
print("FIN")


        

                
