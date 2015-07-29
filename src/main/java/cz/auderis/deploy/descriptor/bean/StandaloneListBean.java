package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.initializer.CollectionItemElement;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "list")
@XmlType()
public class StandaloneListBean extends StandaloneCollectionBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "itemClass")
	protected String itemClass;

	@XmlElementRef(required = false)
	protected List<CollectionItemElement> items;

	public StandaloneListBean() {
		this.itemClass = Object.class.getName();
		this.items = new ArrayList<CollectionItemElement>(1);
	}

	@Override
	public BeanType getBeanType() {
		return BeanType.LIST;
	}

	@Override
	public String getBeanClassName() {
		return List.class.getName();
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
		assert obj instanceof StandaloneListBean;
		assert null != itemClass;
		final StandaloneListBean other = (StandaloneListBean) obj;
		if (!itemClass.equals(other.getItemClassName())) {
			return false;
		}
		return true;
	}

}
