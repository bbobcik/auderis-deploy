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

import cz.auderis.deploy.descriptor.dependency.ExtraDependency;
import cz.auderis.deploy.descriptor.initializer.ConstructorElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

@XmlRootElement(name = "bean")
@XmlType
public class NormalBean extends AbstractBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "class", required = true)
	protected String beanClass;

	@XmlElementRef(required = false)
	protected ConstructorElement constructor;

	@XmlElementRef(type = PropertyElement.class, required = false)
	protected List<PropertyElement> properties;

	@XmlElementRef(type = ExtraDependency.class, required = false)
	protected List<ExtraDependency> extraDependencies;

	public NormalBean() {
		super(BeanType.NORMAL);
		this.beanClass = Void.class.getName();
		this.properties = new ArrayList<PropertyElement>(1);
		this.extraDependencies = new ArrayList<ExtraDependency>(1);
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
	public void updateDefinition(AbstractBean updatingAbstractBean) {
		super.updateDefinition(updatingAbstractBean);
		//
		assert updatingAbstractBean instanceof NormalBean;
		final NormalBean updatingBean = (NormalBean) updatingAbstractBean;
		// Replace bean class
		this.beanClass = updatingBean.getBeanClassName();
		// Replace constructor element if defined
		final ConstructorElement newConstructor = updatingBean.getConstructor();
		if (null != newConstructor) {
			this.constructor = newConstructor;
		}
		// Update list of properties
		final List<PropertyElement> updatingProperties = updatingBean.getProperties();
		final Map<String, PropertyElement> updateMap = new HashMap<String, PropertyElement>(updatingProperties.size());
		for (final PropertyElement property : updatingProperties) {
			updateMap.put(property.getName(), property);
		}
		final ListIterator<PropertyElement> propertyIterator = properties.listIterator();
		while (propertyIterator.hasNext()) {
			final PropertyElement property = propertyIterator.next();
			final String name = property.getName();
			final PropertyElement update = updateMap.get(name);
			if (null != update) {
				propertyIterator.remove();
				propertyIterator.add(update);
				updateMap.remove(name);
			}
		}
		properties.addAll(updateMap.values());
		// Replace dependencies
		this.dependencies.clear();
		this.dependencies.addAll(updatingBean.getDependencies());
	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			if (visitor instanceof DeploymentStructureVisitor) {
				final DeploymentStructureVisitor structVisitor = (DeploymentStructureVisitor) visitor;
				final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
				if (parentFirst) {
					visitor.visitNormalBean(this);
				}
				for (final PropertyElement property : properties) {
					property.accept(structVisitor, context);
				}
				if (null != constructor) {
					constructor.accept(structVisitor, context);
				}
				for (final ExtraDependency extraDependency : extraDependencies) {
					extraDependency.accept(structVisitor, context);
				}
				if (!parentFirst) {
					visitor.visitNormalBean(this);
				}
			} else {
				visitor.visitNormalBean(this);
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
		assert obj instanceof NormalBean;
		assert null != beanClass;
		final NormalBean other = (NormalBean) obj;
		if (!beanClass.equals(other.getBeanClassName())) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder str = new StringBuilder(64);
		str.append(name);
		str.append("{cls=").append(beanClass);
		str.append(", ");
		appendCommonBeanInfo(str);
		str.append('}');
		return str.toString();
	}

}
