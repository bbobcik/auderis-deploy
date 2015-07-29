package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.initializer.CollectionItemElement;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

@XmlRootElement(name = "set")
@XmlType()
public class StandaloneSetBean extends StandaloneCollectionBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "itemClass")
	protected String itemClass;

	@XmlElementRef(required = false)
	protected List<CollectionItemElement> items;

	public StandaloneSetBean() {
		this.itemClass = Object.class.getName();
		this.items = new ArrayList<CollectionItemElement>(1);
	}

	@Override
	public BeanType getBeanType() {
		return BeanType.SET;
	}

	@Override
	public String getBeanClassName() {
		return Set.class.getName();
	}

	public String getItemClassName() {
		return itemClass;
	}

	public List<CollectionItemElement> getItems() {
		return items;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!super.equals(obj)) {
			return false;
		}
		assert obj instanceof StandaloneSetBean;
		assert null != itemClass;
		final StandaloneSetBean other = (StandaloneSetBean) obj;
		if (!itemClass.equals(other.getItemClassName())) {
			return false;
		}
		return true;
	}

}
