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

import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;

import java.io.Serializable;

public abstract class AbstractInjectionElement implements VisitableStructuralNode, Serializable {
	private static final long serialVersionUID = 20150728L;

	protected final InjectionType type;

	protected AbstractInjectionElement(InjectionType type) {
		assert null != type;
		this.type = type;
	}

	public InjectionType getType() {
		return type;
	}

	@Override
	public int hashCode() {
		return type.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if ((null == obj) || !getClass().isAssignableFrom(obj.getClass())) {
			// This works as an "instanceof" condition without hard-coding the constraint class, allowing
			// usage from subclasses
			return false;
		}
		assert obj instanceof AbstractInjectionElement;
		final AbstractInjectionElement other = (AbstractInjectionElement) obj;
		if (type != other.getType()) {
			return false;
		}
		return true;
	}

}
