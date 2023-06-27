import xml.etree.ElementTree as ET

class Reader:
    def __init__(self, c_index) -> None:
        root = ET.parse("lecture/ChemSep8.32.xml")
        comp = root.getroot() #Se guarda el árbol completo

        self.c_index = c_index
        self.compound = comp[self.c_index]

        self.dict = {}
        for property in self.compound:
            self.dict.update({property.tag : property})

    def __getitem__(self, key: str):
        # index = DICCIONARIO[key]
        # return self.compound[index]
        return Propiedad(self.dict[key])
    
class Propiedad:
    def __init__(self, compound_property) -> None:
        self.compound_property = compound_property
        self.name = self.compound_property.attrib.get("name")
        self.value = self.compound_property.attrib.get("value")
        self.dict = {}

        for property in self.compound_property:
            self.dict.update({property.tag: property.attrib})

    def __getitem__(self, key:str):
        return self.dict.get(key)["value"]

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
            file.write(f"  {attribute} = {value}\n") 
  
        for parameters in property:
            attributes = parameters.attrib
            for attribute, value in attributes.items():
                if attribute == "units": continue
                file.write(f"  {parameters.tag} = {value}\n") 
        file.write("/"+"\n")  
    file.close()
    compound_conter += 1
print("FIN")


        

                
