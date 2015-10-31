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


import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "dependsOn")
@XmlType(name = "extraDependencyType")
public class ExtraDependency implements VisitableStructuralNode, ResolvableElement, Serializable{
	private static final long serialVersionUID = 20150821L;

	@XmlAttribute(name = "bean", required = true)
	protected String beanName;

	protected ResolutionStatus resolutionStatus;

	public ExtraDependency() {
		this.resolutionStatus = ResolutionStatus.UNRESOLVED;
	}

	public String getBeanName() {
		return beanName;
	}

	@Override
	public ResolutionStatus getResolutionStatus() {
		return resolutionStatus;
	}

	@Override
	public void setResolutionStatus(ResolutionStatus newStatus) {
		if (null == newStatus) {
			throw new NullPointerException();
		}
		this.resolutionStatus = newStatus;
	}

	@Override
	public boolean isCompoundElement() {
		return false;
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitExtraDependency(this);
		} finally {
			context.popContextPart();
		}
	}

}
