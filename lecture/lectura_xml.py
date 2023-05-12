import xml.etree.ElementTree as ET

root = ET.parse("lecture/ChemSep8.32.xml")
comp = root.getroot()

#import ipdb
#ipdb.set_trace()

print (len(comp))
for child in comp:
    print (child.tag)
    for i in child:
        print(i.tag)
        print (i.attrib)
        for j in i.attrib:
            print (f"key: {j}")
            print(f"valor: {i.attrib[j]}")
        if len(i)>0:
            for k in i:
                print(k.tag)
                print (k.attrib)

        

                
