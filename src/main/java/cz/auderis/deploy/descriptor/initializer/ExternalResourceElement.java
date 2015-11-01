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

import cz.auderis.deploy.descriptor.dependency.ResolutionStatus;
import cz.auderis.deploy.descriptor.dependency.ResolvableElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import java.io.Serializable;

@XmlRootElement(name = "resource")
@XmlType
public class ExternalResourceElement implements VisitableStructuralNode, ResolvableElement, Serializable {
	private static final long serialVersionUID = 20151101L;

	@XmlValue
	protected String resourceSpecification;

	// Workaround for null pointer exception thrown by JAXB reference implementation.
	// See issue https://java.net/jira/browse/JAXB-943 as well as
	// http://stackoverflow.com/questions/14490548/using-jaxb-xmlvalue-with-xmlelementref
	@XmlAttribute(name = "dummy-attr", required = false)
	private String dummyAttr;

	protected ResolutionStatus resolutionStatus;

	public ExternalResourceElement() {
		this.resolutionStatus = ResolutionStatus.UNRESOLVED;
	}

	public String getResourceSpecification() {
		return resourceSpecification;
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
			visitor.visitResourceSpecification(this);
		} finally {
			context.popContextPart();
		}
	}

}
