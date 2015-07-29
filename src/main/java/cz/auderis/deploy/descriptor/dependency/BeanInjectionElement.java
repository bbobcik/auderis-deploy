package cz.auderis.deploy.descriptor.dependency;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "inject")
@XmlType()
public class BeanInjectionElement extends AbstractInjectionElement {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "bean")
	protected String beanName;

}
