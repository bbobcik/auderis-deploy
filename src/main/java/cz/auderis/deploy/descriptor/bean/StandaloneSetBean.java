/*
 * Copyright 2015 Boleslav Bobcik - Auderis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.initializer.CollectionItemElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

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
		super(BeanType.SET);
		this.itemClass = Object.class.getName();
		this.items = new ArrayList<CollectionItemElement>(1);
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
	public void updateDefinition(AbstractBean updatingAbstractBean) {
		super.updateDefinition(updatingAbstractBean);
		//
		assert updatingAbstractBean instanceof StandaloneSetBean;
		final StandaloneSetBean updatingBean = (StandaloneSetBean) updatingAbstractBean;
		// Replace item class
		final String updatingItemClass = updatingBean.getItemClassName();
		if ((null != updatingItemClass) && !updatingItemClass.trim().isEmpty()) {
			this.itemClass = updatingItemClass;
		}
		// Replace set items
		this.items.clear();
		this.items.addAll(updatingBean.getItems());
	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			if (visitor instanceof DeploymentStructureVisitor) {
				final DeploymentStructureVisitor structVisitor = (DeploymentStructureVisitor) visitor;
				final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
				if (parentFirst) {
					visitor.visitSetBean(this);
				}
				for (final CollectionItemElement item : items) {
					item.accept(structVisitor, context);
				}
				if (!parentFirst) {
					visitor.visitSetBean(this);
				}
			} else {
				visitor.visitSetBean(this);
			}
		} finally {
			context.popContextPart();
		}
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
