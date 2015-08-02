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

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@XmlType(name = "constructor")
public class ConstructorElement implements VisitableStructuralNode, Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlElementRef
	protected List<ConstructorArgumentElement> arguments;

	public ConstructorElement() {
		super();
		this.arguments = new ArrayList<ConstructorArgumentElement>(1);
	}

	public List<ConstructorArgumentElement> getArguments() {
		return arguments;
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
			if (parentFirst) {
				visitor.visitBeanConstructor(this);
			}
			if (null != arguments) {
				for (final ConstructorArgumentElement argument : arguments) {
					argument.accept(visitor, context);
				}
			}
			if (!parentFirst) {
				visitor.visitBeanConstructor(this);
			}
		} finally {
			context.popContextPart();
		}
	}

}
