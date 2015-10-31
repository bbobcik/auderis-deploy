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
import cz.auderis.deploy.descriptor.dependency.ResolutionStatus;
import cz.auderis.deploy.descriptor.dependency.ResolvableElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static cz.auderis.deploy.descriptor.initializer.InitializerContentType.TEXT;
import static cz.auderis.deploy.descriptor.initializer.InitializerContentType.determineTypeFromMixedContents;

public abstract class AbstractInitializerContentHolder implements Serializable, ResolvableElement {
	private static final long serialVersionUID = 20150728L;

	@XmlMixed
	@XmlElementRefs({
			@XmlElementRef(type = BeanInjectionElement.class),
			@XmlElementRef(type = PropertyInjectionElement.class)
	})
	protected List<Object> contents;

	private InitializerContentType contentType;
	private boolean normalized;

	protected AbstractInitializerContentHolder() {
		contents = new ArrayList<Object>(1);
	}

	public List<Object> getContents() {
		normalizeContents();
		return contents;
	}

	public InitializerContentType getContentType() {
		normalizeContents();
		assert null != contentType : "Content type was not yet determined";
		return contentType;
	}

	public void setContentType(InitializerContentType contentType) {
		if (null == contentType) {
			throw new NullPointerException();
		}
		this.contentType = contentType;
	}

	@Override
	public ResolutionStatus getResolutionStatus() {
		normalizeContents();
		ResolutionStatus status = ResolutionStatus.RESOLVED;
		for (final Object part : contents) {
			if (part instanceof ResolvableElement) {
				final ResolvableElement element = (ResolvableElement) part;
				if (!element.getResolutionStatus().isResolved()) {
					status = ResolutionStatus.UNRESOLVED;
					break;
				}
			}
		}
		return status;
	}

	@Override
	public void setResolutionStatus(ResolutionStatus newStatus) {
		throw new AssertionError("cannot change status of compound resolvable element");
	}

	@Override
	public boolean isCompoundElement() {
		return true;
	}

	public void normalizeContents() {
		if (normalized) {
			return;
		} else if (null == contents) {
			contents = Collections.emptyList();
			contentType = TEXT;
			normalized = true;
			return;
		}
		if (null == contentType) {
			contentType = determineTypeFromMixedContents(contents);
		}
		switch (contentType) {
			case TEXT: {
				final StringBuilder str = new StringBuilder(64);
				for (final Object contentPart : contents) {
					str.append(contentPart.toString());
				}
				contents = Collections.singletonList((Object) str.toString());
				break;
			}

			case BEAN:
				// Fall through to BEAN_PROPERTY
			case BEAN_PROPERTY: {
				List<Object> replacementList = null;
				PART_SCAN:
				for (final Object contentPart : contents) {
					if ((null != contentPart) && !(contentPart instanceof String)) {
						replacementList = Collections.singletonList(contentPart);
						break PART_SCAN;
					}
				}
				assert null != replacementList;
				contents = replacementList;
				break;
			}

			default:
				throw new AssertionError();
		}
		normalized = true;
	}

	protected void acceptVisitorForContents(DeploymentStructureVisitor visitor, VisitorContext context) {
		// Context manipulation should be done in concrete class implementation
		if (null == contents) {
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
