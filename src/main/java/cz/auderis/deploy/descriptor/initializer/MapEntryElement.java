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

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "entry")
@XmlType
public class MapEntryElement implements VisitableStructuralNode, Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlElement(name = "key", required = true)
	protected MapKeyElement key;

	@XmlElement(name = "value", required = true)
	protected MapValueElement value;

	public MapKeyElement getKeyElement() {
		return key;
	}

	public MapValueElement getValueElement() {
		return value;
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitMapEntry(this);
		} finally {
			context.popContextPart();
		}
	}

}
