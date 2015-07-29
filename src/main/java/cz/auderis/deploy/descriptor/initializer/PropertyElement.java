package cz.auderis.deploy.descriptor.initializer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "property")
@XmlType
public class PropertyElement extends AbstractInitializerContentHolder {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "name", required = true)
	protected String name;

}
