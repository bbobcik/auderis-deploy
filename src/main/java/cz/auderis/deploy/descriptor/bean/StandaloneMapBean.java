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

import cz.auderis.deploy.descriptor.initializer.MapEntryElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

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
		super(BeanType.MAP);
		this.keyClassName = String.class.getName();
		this.valueClassName = Object.class.getName();
		this.entries = new ArrayList<MapEntryElement>(1);
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

	public List<MapEntryElement> getEntries() {
		return entries;
	}

	@Override
	public void updateDefinition(AbstractBean updatingAbstractBean) {
		super.updateDefinition(updatingAbstractBean);
		//
		assert updatingAbstractBean instanceof StandaloneMapBean;
		final StandaloneMapBean updatingBean = (StandaloneMapBean) updatingAbstractBean;
		// Replace key and value class as needed
		final String updatingKeyClass = updatingBean.getKeyClassName();
		if ((null != updatingKeyClass) && !updatingKeyClass.trim().isEmpty()) {
			this.keyClassName = updatingKeyClass;
		}
		final String updatingValueClass = updatingBean.getValueClassName();
		if ((null != updatingValueClass) && !updatingValueClass.trim().isEmpty()) {
			this.valueClassName = updatingValueClass;
		}
		// Replace map entries
		this.entries.clear();
		this.entries.addAll(updatingBean.getEntries());
	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			if (visitor instanceof DeploymentStructureVisitor) {
				final DeploymentStructureVisitor structVisitor = (DeploymentStructureVisitor) visitor;
				final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
				if (parentFirst) {
					visitor.visitMapBean(this);
				}
				for (final MapEntryElement entry : entries) {
					entry.accept(structVisitor, context);
				}
				if (!parentFirst) {
					visitor.visitMapBean(this);
				}
			} else {
				visitor.visitMapBean(this);
			}

		} finally {
			context.popContextPart();
		}
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
