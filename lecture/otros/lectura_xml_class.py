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
"""   
i=0
while True:
    substance = Reader(i)
    if substance["CompoundID"].value == "Propane":
       break
    i+=1
print ("i= ",i)
"""
for i in range(3):
  substance = Reader(i)
  file_name = substance["CompoundID"].value
  file_name = "lecture/db/"+file_name+".txt"
  file = open(file_name, "w")

  file.write("&"+str(substance.compound.attrib.values)+"\n")
  file.write("  "+substance["CompoundID"].name+"\n")
  file.write("/"+"\n")

  file.close()

"""

substance = Reader(66)
print (substance["UnifacVLE"]["group"])
print (substance["CompoundID"].value)
print
#coso["eqno"]
    
"""

import os

directorio_actual = os.getcwd()
print("Directorio actual:", directorio_actual)