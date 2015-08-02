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

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;
import java.io.Serializable;
import java.util.List;

public abstract class AbstractInitializerContentHolder implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlMixed
	@XmlElementRefs({
			@XmlElementRef(type = BeanInjectionElement.class),
			@XmlElementRef(type = PropertyInjectionElement.class)
	})
	protected List<Object> contents;

	public List<Object> getContents() {
		return contents;
	}


	protected void acceptVisitorForContents(DeploymentStructureVisitor visitor, VisitorContext context) {
		if (null == context) {
			return;
		}
		for (final Object item : contents) {
			if (item instanceof String) {
				visitor.visitStringValue((String) item);
			} else if (item instanceof VisitableStructuralNode) {
				((VisitableStructuralNode) item).accept(visitor, context);
			} else {
				visitor.visitUnknownValue(item);
			}
		}
	}

}
