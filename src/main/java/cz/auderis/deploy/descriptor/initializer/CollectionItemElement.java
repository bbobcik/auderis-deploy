package cz.auderis.deploy.descriptor.initializer;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "item")
@XmlType
public class CollectionItemElement extends AbstractInitializerContentHolder {
	private static final long serialVersionUID = 20150728L;

}
