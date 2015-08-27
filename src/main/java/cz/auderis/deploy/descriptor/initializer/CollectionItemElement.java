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

import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "item")
@XmlType
public class CollectionItemElement extends AbstractInitializerContentHolder implements VisitableStructuralNode, InjectionPlace {
	private static final long serialVersionUID = 20150728L;

	@XmlTransient
	protected CollectionItemDependencyMode itemDependencyMode;

	public CollectionItemElement() {
		super();
		itemDependencyMode = CollectionItemDependencyMode.getDefaultMode();
	}

	public CollectionItemDependencyMode getDependencyMode() {
		return itemDependencyMode;
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
			if (parentFirst) {
				visitor.visitCollectionItem(this);
			}
			acceptVisitorForContents(visitor, context);
			if (!parentFirst) {
				visitor.visitCollectionItem(this);
			}
		} finally {
			context.popContextPart();
		}
	}

	@XmlAttribute(name = "dependencyMode")
	protected final String getItemDependencyModeCode() {
		return itemDependencyMode.getCanonicalName();
	}

	protected final void setItemDependencyModeCode(String modeCode) {
		final CollectionItemDependencyMode mode = parseEnumByName(modeCode, CollectionItemDependencyMode.class);
		if (null != mode) {
			this.itemDependencyMode = mode;
		} else if (isBlank(modeCode) && !isStrictParsingEnabled()) {
			this.itemDependencyMode = CollectionItemDependencyMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid item dependency mode '" + modeCode + "'");
		}
	}

}
