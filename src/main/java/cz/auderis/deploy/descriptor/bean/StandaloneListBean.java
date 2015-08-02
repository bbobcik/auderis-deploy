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

@XmlRootElement(name = "list")
@XmlType()
public class StandaloneListBean extends StandaloneCollectionBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "itemClass", required = false)
	protected String itemClass;

	@XmlElementRef(required = false)
	protected List<CollectionItemElement> items;

	public StandaloneListBean() {
		super(BeanType.LIST);
		this.itemClass = Object.class.getName();
		this.items = new ArrayList<CollectionItemElement>(1);
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
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			if (visitor instanceof DeploymentStructureVisitor) {
				final DeploymentStructureVisitor structVisitor = (DeploymentStructureVisitor) visitor;
				final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
				if (parentFirst) {
					visitor.visitListBean(this);
				}
				for (final CollectionItemElement item : items) {
					item.accept(structVisitor, context);
				}
				if (!parentFirst) {
					visitor.visitListBean(this);
				}
			} else {
				visitor.visitListBean(this);
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
		assert obj instanceof StandaloneListBean;
		assert null != itemClass;
		final StandaloneListBean other = (StandaloneListBean) obj;
		if (!itemClass.equals(other.getItemClassName())) {
			return false;
		}
		return true;
	}

}
