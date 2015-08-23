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

}
