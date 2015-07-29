package cz.auderis.deploy.descriptor.initializer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "argument")
@XmlType
public class ConstructorArgumentElement implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "property", required = true)
	protected String propertyName;

	@XmlAttribute(name = "class", required = false)
	protected String argumentClass;

	protected transient PropertyElement referencedProperty;

	public ConstructorArgumentElement() {
		super();
	}

	public String getPropertyName() {
		return propertyName;
	}

	public String getArgumentClassName() {
		return argumentClass;
	}

	public PropertyElement getReferencedProperty() {
		return referencedProperty;
	}

	public void setReferencedProperty(PropertyElement referencedProperty) {
		this.referencedProperty = referencedProperty;
	}

}
