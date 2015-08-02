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

package cz.auderis.deploy.descriptor.producer;

import cz.auderis.deploy.descriptor.DeploymentEntry;
import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "factory")
@XmlType
public class FactoryDefinition extends DeploymentEntry {
	private static final long serialVersionUID = -6863617493100045759L;

	@XmlID
	@XmlAttribute(name = "name", required = true)
	protected String name;

	@XmlAttribute(name = "class", required = true)
	protected String factoryClass;

	@XmlAttribute(name = "method", required = true)
	protected String factoryMethod;

	@XmlTransient
	protected FactoryProductionMode productionMode;


	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitFactoryDefinition(this);
		} finally {
			context.popContextPart();
		}
	}

	@XmlAttribute(name = "mode")
	protected final String getProductionModeCode() {
		return productionMode.getCanonicalName();
	}

	protected final void setProductionModeCode(String prodModeCode) {
		final FactoryProductionMode mode = parseEnumByName(prodModeCode, FactoryProductionMode.class);
		if (null != mode) {
			this.productionMode = mode;
		} else if (isBlank(prodModeCode) && !isStrictParsingEnabled()) {
			this.productionMode = FactoryProductionMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid factory production mode '" + prodModeCode + "'");
		}
	}

}
