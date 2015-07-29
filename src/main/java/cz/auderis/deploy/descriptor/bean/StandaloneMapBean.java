package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.initializer.MapEntryElement;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@XmlRootElement(name = "map")
@XmlType()
public class StandaloneMapBean extends StandaloneCollectionBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "keyClass")
	protected String keyClassName;

	@XmlAttribute(name = "valueClass")
	protected String valueClassName;

	@XmlElementRef(required = false)
	protected List<MapEntryElement> entries;

	public StandaloneMapBean() {
		this.keyClassName = String.class.getName();
		this.valueClassName = Object.class.getName();
		this.entries = new ArrayList<MapEntryElement>(1);
	}

	@Override
	public BeanType getBeanType() {
		return BeanType.MAP;
	}

	@Override
	public String getBeanClassName() {
		return Map.class.getName();
	}

	public String getKeyClassName() {
		return keyClassName;
	}

	public String getValueClassName() {
		return valueClassName;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!(obj instanceof StandaloneMapBean) || !super.equals(obj)) {
			return false;
		}
		final StandaloneMapBean other = (StandaloneMapBean) obj;
		assert null != keyClassName;
		assert null != valueClassName;
		if (!keyClassName.equals(other.getKeyClassName())) {
			return false;
		} else if (!valueClassName.equals(other.getValueClassName())) {
			return false;
		}
		return true;
	}

}
