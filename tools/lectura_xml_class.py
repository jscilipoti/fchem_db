import xml.etree.ElementTree as ET

class Reader:
    def __init__(self, c_index) -> None:
        root = ET.parse("tools/ChemSep8.32.xml")
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
