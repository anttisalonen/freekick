#!/usr/bin/python

import lxml
from lxml import etree

def main():
    root = etree.Element("root")
    child1 = etree.SubElement(root, "child1")
    child2 = etree.SubElement(root, "child2")
    child3 = etree.SubElement(root, "child3")

    child4 = etree.Element("child4", interesting="totally")
    child1.append(child4)

    xmlstring = etree.tostring(root, pretty_print=True)

    f=open('./test.xml', 'w')
    f.write(xmlstring)
    f.close()

if __name__ == '__main__':
    main()
