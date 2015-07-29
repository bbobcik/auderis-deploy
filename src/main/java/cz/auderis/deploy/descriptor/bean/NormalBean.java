package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.initializer.ConstructorElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "bean")
@XmlType
public class NormalBean extends AbstractBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "class", required = true)
	protected String beanClass;

	@XmlElement(required = false)
	protected ConstructorElement constructor;

	@XmlElementRef(type = PropertyElement.class, required = false)
	protected List<PropertyElement> properties;

	public NormalBean() {
		super();
		this.beanClass = Void.class.getName();
		this.properties = new ArrayList<PropertyElement>(1);
	}

	@Override
	public BeanType getBeanType() {
		return BeanType.NORMAL;
	}

	@Override
	public String getBeanClassName() {
		return beanClass;
	}

	public ConstructorElement getConstructor() {
		return constructor;
	}

	public List<PropertyElement> getProperties() {
		return properties;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!super.equals(obj)) {
			return false;
		}
		assert obj instanceof NormalBean;
		assert null != beanClass;
		final NormalBean other = (NormalBean) obj;
		if (!beanClass.equals(other.getBeanClassName())) {
			return false;
		}
		return true;
	}

}
