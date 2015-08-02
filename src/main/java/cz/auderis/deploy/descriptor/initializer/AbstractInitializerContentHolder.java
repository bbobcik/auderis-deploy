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

import cz.auderis.deploy.descriptor.dependency.AbstractInjectionElement;
import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;
import java.io.Serializable;
import java.util.Collections;
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
		normalizeContents();
		return contents;
	}


	protected void normalizeContents() {
		if (null == contents) {
			contents = Collections.emptyList();
			return;
		} else if (contents.size() <= 1) {
			return;
		}
		int textSize = 0;
		for (final Object item : contents) {
			if (item instanceof AbstractInjectionElement) {
				contents = Collections.singletonList(item);
				return;
			}
			assert item instanceof String;
			textSize += ((String) item).length();
		}
		final StringBuilder str = new StringBuilder(textSize);
		for (final Object item : contents) {
			str.append(item);
		}
		final String joinedText = str.toString();
		contents = Collections.singletonList((Object) joinedText);
	}

	protected void acceptVisitorForContents(DeploymentStructureVisitor visitor, VisitorContext context) {
		normalizeContents();
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
