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

package cz.auderis.deploy.descriptor.initializer;

import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "argument")
@XmlType
public class ConstructorArgumentElement implements VisitableStructuralNode, Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "property", required = true)
	protected String propertyName;

	@XmlAttribute(name = "class", required = false)
	protected String argumentClass;

	protected PropertyElement referencedProperty;

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

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitBeanConstructorArgument(this);
		} finally {
			context.popContextPart();
		}
	}

}
