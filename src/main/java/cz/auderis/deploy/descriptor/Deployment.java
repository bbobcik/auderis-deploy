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

package cz.auderis.deploy.descriptor;

import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "deployment")
@XmlType
public class Deployment implements VisitableNode, Serializable {
	private static final long serialVersionUID = 20150728L;

	public static final String SCHEMA_URI = "http://auderis.cz/ns/deployment/1.0";
	public static final String SCHEMA_LOCATION = "/META-INF/auderis-deployment.xsd";

	@XmlElementRef(type = DeploymentEntry.class)
	private List<DeploymentEntry> entries;

	private String id;

	public Deployment() {
		this.entries = new ArrayList<DeploymentEntry>(8);
	}

	public List<DeploymentEntry> getEntries() {
		return entries;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
			if (parentFirst) {
				visitor.visitDeployment(this);
			}
			for (final DeploymentEntry entry : entries) {
				entry.accept(visitor, context);
			}
			if (!parentFirst) {
				visitor.visitDeployment(this);
			}
		} finally {
			context.popContextPart();
		}
	}

}
