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

package cz.auderis.deploy.descriptor.dependency;

import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "inject")
@XmlType()
public class BeanInjectionElement extends AbstractInjectionElement {
	private static final long serialVersionUID = 20150728L;

	// Support auto-wiring by omitting bean name
	@XmlAttribute(name = "bean", required = false)
	protected String beanName;

	@XmlTransient
	protected MaterializationMode materializationMode;

	public BeanInjectionElement() {
		super(InjectionType.BEAN);
	}

	public String getBeanName() {
		if (isAutowireInjection()) {
			return null;
		}
		return beanName;
	}

	public MaterializationMode getMaterializationMode() {
		return materializationMode;
	}

	public boolean isAutowireInjection() {
		return (null == beanName) || beanName.trim().isEmpty();
	}

	@XmlAttribute(name = "materialize")
	protected final String getMaterializationModeCode() {
		return materializationMode.getCanonicalName();
	}

	protected final void setMaterializationModeCode(String matModeCode) {
		final MaterializationMode mode = parseEnumByName(matModeCode, MaterializationMode.class);
		if (null != mode) {
			this.materializationMode = mode;
		} else if (isBlank(matModeCode) && !isStrictParsingEnabled()) {
			this.materializationMode = MaterializationMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid bean materialization mode '" + matModeCode + "'");
		}
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitBeanInjection(this);
		} finally {
			context.popContextPart();
		}
	}

	@Override
	public int hashCode() {
		return 11*super.hashCode() + beanName.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!super.equals(obj)) {
			return false;
		}
		final BeanInjectionElement other = (BeanInjectionElement) obj;
		final String otherName = other.getBeanName();
		if ((null == beanName) && (null != otherName)) {
			return false;
		} else if ((null != beanName) && !beanName.equals(otherName)) {
			return false;
		} else if (materializationMode != other.getMaterializationMode()) {
			return false;
		}
		return true;
	}

}
