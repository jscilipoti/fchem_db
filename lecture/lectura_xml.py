import xml.etree.ElementTree as ET

root = ET.parse("lecture/ChemSep8.32.xml")
root_node = root.getroot()

print (len(root_node))
for child in root_node:
    for i in child:
        #tag = i.tag
        print (i.attrib)
