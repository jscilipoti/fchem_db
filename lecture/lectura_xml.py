import xml.etree.ElementTree as ET

root = ET.parse("lecture/ChemSep8.32.xml")
root_node = root.getroot()

import ipdb
ipdb.set_trace()

print (len(root_node))
for child in root_node:
    print (child.tag)
    for i in child:
        print(i.tag)
        print (i.attrib)
        for j in i.attrib:
            print (f"key: {j}")
            print(f"valor: {i.attrib[j]}")
        

                
